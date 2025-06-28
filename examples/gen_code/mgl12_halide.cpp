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
    if (vectorize) nz.vectorize(d, 8);
    return nz;
}

template<>
inline Func init_nonzero<2>(const std::string& funcName, bool vectorize)
{
    Var d1, d2;
    Func nz(funcName);
    nz(d1, d2) = ((((d1+1 + (d2+1)*10)) + Expr(1e-5)) / Expr(100.0)) * cast<double>(10);
    nz.parallel(d2);
    if (vectorize) nz.vectorize(d1, 8);
    return nz;
}

template<>
inline Func init_nonzero<3>(const std::string& funcName, bool vectorize)
{
    Var d1, d2, d3;
    Func nz(funcName);
    nz(d1, d2, d3) = ((((d1+1 + (d2+1)*10 + (d3+1)*100)) + Expr(1e-5)) / Expr(100.0)) * cast<double>(10);
    nz.parallel(d2);
    nz.parallel(d3);
    if (vectorize) nz.vectorize(d1, 8);
    return nz;
}

template <>
inline Func set_zero<1>(const std::string& funcName, bool vectorize)
{
    Var d;
    Func set_zero(funcName);
    set_zero(d) = Expr(0.0);
    set_zero.parallel(d);
    if (vectorize) set_zero.vectorize(d, 8);
    return set_zero;
}

template <>
inline Func set_zero<2>(const std::string& funcName, bool vectorize)
{
    Var d1, d2;
    Func set_zero(funcName);
    set_zero(d1, d2) = Expr(0.0);
    set_zero.parallel(d1);
    set_zero.parallel(d2);
    if (vectorize) set_zero.vectorize(d1, 8);
    return set_zero;
}

template <>
inline Func set_zero<3>(const std::string& funcName, bool vectorize)
{
    Var d1, d2, d3;
    Func set_zero(funcName);
    set_zero(d1, d2, d3) = Expr(0.0);
    set_zero.parallel(d1);
    set_zero.parallel(d2);
    set_zero.parallel(d3);
    if (vectorize) set_zero.vectorize(d1, 8);
    return set_zero;
}

Func targetFunction(const std::string &funcName,
                    Buffer<double, 3> u, 
                    const int i1s, const int i1e,
                    const int i2s, const int i2e, 
                    const int i3s, const int i3e) {

    Var d1("i1"), d2("i2"), d3("i3");
    Func res(funcName);
    

    auto roi1 = i2s == d2 &&
               i1s <= d1 and d1 <= i1e &&
               i3s <= d3 and d3 <= i3e;
    auto roi2 = i2e == d2 &&
                i1s <= d1 and d1 <= i1e &&
                i3s <= d3 and d3 <= i3e;
    res(d1, d2, d3) = select(roi1, u(d1, i2e-1, d3),
                             select(roi2, u(d1, 1, d3),
                                    u(d1, d2, d3)));

    return res;
}

extern "C" {
    void mg_loop12_(
        double* u,
        const int *n1, const int *n2, const int *n3
    );
}

int main(int argc, char** argv)
{
    // Check for memory constraints and adjust size if needed
    const int n1 = 512;
    const int n2 = 512;
    const int n3 = 512;

    // const int n1 = 5;
    // const int n2 = 5;
    // const int n3 = 5;
    
    printf("Processing data size: %dx%dx%d (%.2f GB total memory needed)\n", 
           n1, n2, n3, (3.0 * n1 * n2 * n3 * sizeof(double)) / (1024.0*1024.0*1024.0));
    
    // Loop bounds from the original code
    const int i1s = 1, i1e = n1 - 2; // i1: 2 to 254 for u computation
    const int i2s = 0, i2e = n2 - 1; // i2: 2 to 254
    const int i3s = 1, i3e = n3 - 2; // i3: 2 to 254
    
    // Array dimensions with padding for stencil access
    const int u_total_d1 = n1;
    const int u_total_d2 = n2;
    const int u_total_d3 = n3;

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
    auto i3_cpu = init_nonzero<3>("init_3d", n1 >= 8);
    auto z3_cpu = set_zero<3>("zero3", n1 >= 8);
    i3_cpu.compile_jit(cpu_target);
    z3_cpu.compile_jit(cpu_target);

    Func i3_gpu(i3_cpu), z3_gpu(z3_cpu);
    if ( with_gpu ) 
    {
        i3_gpu.compile_jit(gpu_target);
        z3_gpu.compile_jit(gpu_target);
    }

    // Create input buffers
    Buffer<double, 3> u({u_total_d1, u_total_d2, u_total_d3}, "u");
    Buffer<double> u_g(u);

    // Fortran output
    Buffer<double, 3> u_base({u_total_d1, u_total_d2, u_total_d3}, "u_raw");
    // Lifted CPU output
    Buffer<double, 3> u_cpu({u_total_d1, u_total_d2, u_total_d3}, "u_cpu");
    // Lifted GPU output
    Buffer<double, 3> u_gpu({u_total_d1, u_total_d2, u_total_d3}, "u_gpu");

    // --------------------------- mg_loop12 kernel --------------------------
    printf("mg_loop12 kernel start!\n");
    // gpu_target.set_feature(Halide::Target::Debug);

    auto res_cpu = targetFunction("mg_loop12_cpu",
                                  u,
                                  i1s, i1e,
                                  i2s, i2e,
                                  i3s, i3e);
    
    // CPU optimizations
    res_cpu.parallel(d3)
           .vectorize(d1, 8)
           .compile_jit(cpu_target);

    i3_cpu.realize(u);

    i3_cpu.realize(u_base);
    mg_loop12_(
        u_base.get()->begin(),
        &n1, &n2, &n3
    );

    try {
        i3_cpu.realize(u_cpu);
        res_cpu.realize(u_cpu);
    } catch ( RuntimeError &e ) {
        std::cerr << e.what();
        return 1;
    }

    double correctness_1 = 0.0;
    for (int j = 1; j < n1-1; j++) {
        for (int k = 1; k < n2-1; k++) {
            for (int i = 0; i < n3; i++) {
                int a = i;
                int b = k;
                int c = j;
                auto A = u_base(a,b,c);
                auto B = u_cpu(i,k,j);
                double diff1 = abs(A - B);
                correctness_1 += diff1;
            }
        }
    }
    printf("The number for correctness_1 is %d\n",int(correctness_1));

    int times = 5;
    double cost_time = 0;
    struct timespec t1, t2, elapsed;
    clock_gettime(CLOCK_REALTIME, &t1);
    for (int i = 0; i < times; i++)
    {
        clock_gettime(CLOCK_REALTIME, &t1);
        i3_cpu.realize(u_base);
        mg_loop12_(
            u_base.get()->begin(),
            &n1, &n2, &n3
        );
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("mg_loop12 legacy code average cost time: %lfms\n\n",cost_time/times*1000);

    cost_time = 0;
    clock_gettime(CLOCK_REALTIME, &t1);
    for (int i = 0; i < times; i++)
    {
        clock_gettime(CLOCK_REALTIME, &t1);
        res_cpu.realize(u_cpu);
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("mg_loop12 lifted cpu average cost time: %lfms\n\n",cost_time/times*1000);



    if ( with_gpu )
    {
        auto res_gpu = targetFunction("mg_loop12_gpu",
                                    u_g,
                                    i1s, i1e,
                                    i2s, i2e,
                                    i3s, i3e);

        // GPU optimizations
        res_gpu.gpu_tile(d1,d2,d3,d1o,d2o,d3o,d1i,d2i,d3i,8,8,4)
                .compile_jit(gpu_target);
    
        // IO initialization
        i3_gpu.realize(u_g);
        // GPU warm up
        try {
            i3_gpu.realize(u_gpu);
            res_gpu.realize(u_gpu);
        } catch ( RuntimeError &e ) {
            std::cerr << e.what();
            return 2;
        }

        cost_time = 0;
        clock_gettime(CLOCK_REALTIME, &t1);
        for (int i = 0; i < times; i++)
        {
            res_gpu.realize(u_gpu);
        }
        u_gpu.copy_to_host();
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
        printf("lifted gpu average cost time: %lfms\n\n",cost_time/times*1000);
    }

    return 0;
} 