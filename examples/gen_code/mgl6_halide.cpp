#include "Halide.h"
#include <cstdio>
#include <ctime>

using namespace Halide;
using namespace std;

inline void timespec_diff(const struct timespec &a, const struct timespec &b, struct timespec &result)
{
    result.tv_sec = a.tv_sec - b.tv_sec;
    result.tv_nsec = a.tv_nsec - b.tv_nsec;
    if (result.tv_nsec < 0)
    {
        --result.tv_sec;
        result.tv_nsec += 1000000000L;
    }
}
inline double toSec(const struct timespec &t) { return t.tv_sec + t.tv_nsec / 1e9L; }

Target find_gpu_target()
{
    // Start with a target suitable for the machine you're running this on.
    Target target = get_host_target();

    std::vector<Target::Feature> features_to_try;
    features_to_try.push_back(Target::CUDA);
    // features_to_try.push_back(Target::OpenCL);

    for (Target::Feature f : features_to_try)
    {
        Target new_target = target.with_feature(f);
        if (host_supports_target_device(new_target))
        {
            return new_target;
        }
    }

    printf("Requested GPU(s) are not supported. (Do you have the proper hardware and/or driver installed?)\n");
    return target;
}


template <int dim>
Func init_nonzero(const std::string&, bool) { return Func(Expr(0.0));}

template <int dim>
Func set_zero(const std::string&, bool) { return Func(Expr(0.0));}

template<>
inline Func init_nonzero<1>(const std::string& funcName, bool vectorize)
{
    Var d;
    Func nz(funcName);
    nz(d) = (((d+1 ) + Expr(1e-5)) / Expr(100.0)) * cast<double>(10);
   
    return nz;
}

template<>
inline Func init_nonzero<2>(const std::string& funcName, bool vectorize)
{
    Var d1, d2;
    Func nz(funcName);
    nz(d1, d2) = ((((d1+1 + (d2+1)*10)) + Expr(1e-5)) / Expr(100.0)) * cast<double>(10);

    return nz;
}

template<>
inline Func init_nonzero<3>(const std::string& funcName, bool vectorize)
{
    Var d1, d2, d3;
    Func nz(funcName);
    nz(d1, d2, d3) = ((((d1+1 + (d2+1)*10 + (d3+1)*100)) + Expr(1e-5)) / Expr(100.0)) * cast<double>(10);

    return nz;
}

template <>
inline Func set_zero<1>(const std::string& funcName, bool vectorize)
{
    Var d;
    Func set_zero(funcName);
    set_zero(d) = Expr(0.0);

    return set_zero;
}

template <>
inline Func set_zero<2>(const std::string& funcName, bool vectorize)
{
    Var d1, d2;
    Func set_zero(funcName);
    set_zero(d1, d2) = Expr(0.0);

    return set_zero;
}

template <>
inline Func set_zero<3>(const std::string& funcName, bool vectorize)
{
    Var d1, d2, d3;
    Func set_zero(funcName);
    set_zero(d1, d2, d3) = Expr(0.0);

    return set_zero;
}

Func targetFunction(const std::string &funcName,
                    Buffer<double, 3> r, 
                    Buffer<double, 3> u, 
                    Buffer<double, 3> v, 
                    Buffer<double, 1> a,
                    const int i1s, const int i1e,
                    const int i2s, const int i2e, 
                    const int i3s, const int i3e) {

    Var d1("i1"), d2("i2"), d3("i3");
    Func res(funcName);
    auto u_b = BoundaryConditions::constant_exterior(u, Expr(1.0));
    Func u_c("u_conv"), u1("u1"), u2("u2"), r_upd("r_upd");
    u_c(d1, d2, d3) = u_b(d1, d2, d3);

    u1(d1, d2, d3) = u_c(d1, d2-1, d3  ) + 
                     u_c(d1, d2+1, d3  ) + 
                     u_c(d1, d2  , d3-1) +
                     u_c(d1, d2  , d3+1);

    u2(d1, d2, d3) = u_c(d1, d2-1, d3-1) + 
                     u_c(d1, d2+1, d3-1) + 
                     u_c(d1, d2-1, d3+1) +
                     u_c(d1, d2+1, d3+1);
    
    r_upd(d1, d2, d3) = v(d1, d2, d3) -
                       Expr(a(0)) * u_c(d1, d2, d3) - 
                       Expr(a(2)) * (u2(d1, d2, d3) + u1(d1-1, d2, d3) + u1(d1+1, d2, d3)) -
                       Expr(a(3)) * (u2(d1-1, d2, d3) + u2(d1+1, d2, d3));
    Func r_fn("r");
    auto uoi = i1s <= d1 and d1 <= i1e &&
               i2s <= d2 and d2 <= i2e &&
               i3s <= d3 and d3 <= i3e;
    r_fn(d1, d2, d3) = select(uoi, r_upd(d1,d2,d3), r(d1,d2,d3));
    // u_fn(d1, d2, d3) = u_upd(d1,d2,d3);

    return r_fn;
}

extern "C" {
    void mg_loop6_(
        double* r, double* u, double *v, double *a, double *u1, double *u2,
        const int *n1, const int *n2, const int *n3
    );
}

#ifndef _3D_1
#define _3D_1 512
#define _3D_2 512
#define _3D_3 512
#endif

int main(int argc, char** argv)
{
    // Check for memory constraints and adjust size if needed
    const int n1 = _3D_1;
    const int n2 = _3D_2;
    const int n3 = _3D_3;

    // const int n1 = 5;
    // const int n2 = 5;
    // const int n3 = 5;
    
    printf("Processing data size: %dx%dx%d (%.2f GB total memory needed)\n", 
           n1, n2, n3, (3.0 * n1 * n2 * n3 * sizeof(double)) / (1024.0*1024.0*1024.0));
    
    // Loop bounds from the original code
    const int i1s = 1, i1e = n1 - 2; // i1: 2 to 254 for u computation
    const int i2s = 1, i2e = n2 - 2; // i2: 2 to 254
    const int i3s = 1, i3e = n3 - 2; // i3: 2 to 254
    
    const int d1_range = n1; // i1: 2 to 254 (253 elements)
    const int d2_range = n2; // i2: 2 to 254 (253 elements)  
    const int d3_range = n3; // i3: 2 to 254 (253 elements)
    
    // Array dimensions with padding for stencil access
    const int c_range = 4; // c(0:3)

    // --------------------------- Preparation --------------------------
    Var d1("i1"), d2("i2"), d3("i3"), d1o, d2o, d3o, d1i, d2i, d3i;

    bool with_gpu = true;
    Target gpu_target = find_gpu_target();
    if (!gpu_target.has_gpu_feature())
    {
        printf("Don't have a gpu!\n");
        with_gpu = false;
    }
    Target cpu_target = get_host_target();
    
    // Enable GPU execution now that CPU version works

    // Input data generation functions
    auto i3_cpu = init_nonzero<3>("init_3d", d1_range >= 8);
    auto i1_cpu = init_nonzero<1>("init_1d", false);
    auto z3_cpu = set_zero<3>("zero3", d1_range >= 8);
    i3_cpu.compile_jit(cpu_target);
    i1_cpu.compile_jit(cpu_target);
    z3_cpu.compile_jit(cpu_target);

    Func i3_gpu(i3_cpu), i1_gpu(i1_cpu), z3_gpu(z3_cpu);
    if ( with_gpu ) 
    {
        i3_gpu.compile_jit(gpu_target);
        i1_gpu.compile_jit(gpu_target);
        z3_gpu.compile_jit(gpu_target);
    }

    // Create input buffers
    Buffer<double, 3> r({d1_range, d2_range, d3_range}, "r");
    Buffer<double, 3> u({d1_range, d2_range, d3_range}, "u");
    Buffer<double, 3> v({d1_range, d2_range, d3_range}, "v");
    Buffer<double, 1> u1(d1_range, "u1");
    Buffer<double, 1> u2(d1_range, "u2");
    Buffer<double, 1> c(4, "c");
    Buffer<double> u_g(u), r_g(r), v_g(v), c_g(c);

    // Fortran output
    Buffer<double, 3> r_base({d1_range, d2_range, d3_range}, "r_raw");
    // Lifted CPU output
    Buffer<double, 3> r_cpu({d1_range, d2_range, d3_range}, "r_cpu");
    // Lifted GPU output
    Buffer<double, 3> r_gpu({d1_range, d2_range, d3_range}, "r_gpu");

    // --------------------------- mg_loop6 kernel --------------------------
    printf("mg_loop6 kernel start!\n");
    // gpu_target.set_feature(Halide::Target::Debug);

    i1_cpu.realize(c);
    auto res_cpu = targetFunction("mg_loop6_cpu",
                                  r, u, v, c,
                                  i1s, i1e,
                                  i2s, i2e,
                                  i3s, i3e);
    
    // CPU optimizations
    if(d1_range >= 8) res_cpu.vectorize(d1, 8);
    try {
    res_cpu.parallel(d3).parallel(d2).compile_jit(cpu_target);
    } catch (CompileError &e) {
        std::cerr << e.what();
    }
    z3_cpu.realize(r);
    i3_cpu.realize(u);
    i3_cpu.realize(v);
    i1_cpu.realize(u1);
    i1_cpu.realize(u2);

    z3_cpu.realize(r_base);
    mg_loop6_(
        r_base.get()->begin(),
        u.get()->begin(),
        v.get()->begin(),
        c.get()->begin(),
        u1.get()->begin(),
        u2.get()->begin(),
        &n1, &n2, &n3
    );

    try {
        z3_cpu.realize(r_cpu);
        res_cpu.realize(r_cpu);
    } catch ( RuntimeError &e ) {
        std::cerr << e.what();
        return 1;
    }

    double correctness_1 = 0.0;
    for (int j = 0; j < n3; j++) {
        for (int k = 0; k < n2; k++) {
            for (int i = 0; i < n1; i++) {
                int a = i;
                int b = k;
                int c = j;
                auto A = r_base(a,b,c);
                auto B = r_cpu(i,k,j);
                double diff1 = abs(A - B);
                correctness_1 += diff1;
            }
        }
    }
    printf("The number for correctness_1 is %d\n",int(correctness_1/(n1*n2*n3)));

    int times = 5;
    double cost_time = 0;
    struct timespec t1, t2, elapsed;
    clock_gettime(CLOCK_REALTIME, &t1);
    for (int i = 0; i < times; i++)
    {
        clock_gettime(CLOCK_REALTIME, &t1);
        mg_loop6_(
            r_base.get()->begin(),
            u.get()->begin(),
            v.get()->begin(),
            c.get()->begin(),
            u1.get()->begin(),
            u2.get()->begin(),
            &n1, &n2, &n3
        );
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("mg_loop6 legacy code average cost time: %lfms\n\n",cost_time/times*1000);

    cost_time = 0;
    clock_gettime(CLOCK_REALTIME, &t1);
    for (int i = 0; i < times; i++)
    {
        clock_gettime(CLOCK_REALTIME, &t1);
        res_cpu.realize(r_cpu);
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("mg_loop6 lifted cpu average cost time: %lfms\n\n",cost_time/times*1000);



    if ( with_gpu )
    {
        i1_cpu.realize(c_g);
        auto res_gpu = targetFunction("mg_loop6_cpu",
                                    r_g, u_g, v_g, c_g,
                                    i1s, i1e,
                                    i2s, i2e,
                                    i3s, i3e);

        // GPU optimizations
        res_gpu.gpu_tile(d1,d2,d3,d1o,d2o,d3o,d1i,d2i,d3i,8,8,4)
                .compile_jit(gpu_target);
    
        // IO initialization
        z3_gpu.realize(r_g);
        i3_cpu.realize(u_g);
        i3_cpu.realize(v_g);
            
        // GPU warm up
        try {
            z3_gpu.realize(r_gpu);
            res_gpu.realize(r_gpu);
        } catch ( RuntimeError &e ) {
            std::cerr << e.what();
            return 2;
        }

        cost_time = 0;
        clock_gettime(CLOCK_REALTIME, &t1);
        for (int i = 0; i < times; i++)
        {
            res_gpu.realize(r_gpu);
        }
        r_gpu.copy_to_host();
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
        printf("lifted gpu average cost time: %lfms\n\n",cost_time/times*1000);
        
    }

    return 0;
} 