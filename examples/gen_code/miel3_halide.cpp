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
Func init_nonzero(const std::string&, bool) { return Func(Expr(0.f));}

template <int dim>
Func set_zero(const std::string&, bool) { return Func(Expr(0.f));}

template<>
inline Func init_nonzero<1>(const std::string& funcName, bool vectorize)
{
    Var d;
    Func nz(funcName);
    nz(d) = cast<float>((((d ) + Expr(1e-5)) / Expr(10000000.0)));
    return nz;
}

template<>
inline Func init_nonzero<2>(const std::string& funcName, bool vectorize)
{
    Var d1, d2;
    Func nz(funcName);
    nz(d1, d2) = cast<float>(((((d1 + d2*10)) + Expr(1e-5)) / Expr(10000000.0)));
    return nz;
}

template<>
inline Func init_nonzero<3>(const std::string& funcName, bool vectorize)
{
    Var d1, d2, d3;
    Func nz(funcName);
    nz(d1, d2, d3) = cast<float>(((((d1 + d2*10 + d3*10)) + Expr(1e-5)) / Expr(10000000.0)));
    return nz;
}

template <>
inline Func set_zero<1>(const std::string& funcName, bool vectorize)
{
    Var d;
    Func set_zero(funcName);
    set_zero(d) = Expr(0.f);
    return set_zero;
}

template <>
inline Func set_zero<2>(const std::string& funcName, bool vectorize)
{
    Var d1, d2;
    Func set_zero(funcName);
    set_zero(d1, d2) = Expr(0.0f);
    return set_zero;
}

template <>
inline Func set_zero<3>(const std::string& funcName, bool vectorize)
{
    Var d1, d2, d3;
    Func set_zero(funcName);
    set_zero(d1, d2, d3) = Expr(0.0f);
    return set_zero;
}



Func targetFunction(Buffer<float, 3> ww,
                    Buffer<float, 3> u,
                    Buffer<float, 3> v,
                    Buffer<float, 2> mut,
                    Buffer<float, 1> rdnw,
                    Buffer<float, 1> c1f,
                    Buffer<float, 1> c2f) {

    Var i("i"), k("k"), j("j");
    Func wwE("wwE"), wwI("wwI");
    
    Expr dt(0.5f);
    Expr rdx(0.5f);
    Expr rdy(0.5f);
    Expr alpha_max(1.1f);
    Expr alpha_min(0.9f);
    Expr Ceps(0.9f);
    
    Expr cmnx_ratio = alpha_min/alpha_max;
    Expr cutoff = Expr(2.0f) - cmnx_ratio;
    Expr r4cmx = Expr(1.0f)/(Expr(4.0f) - Expr(4.0f)*cmnx_ratio);
    
    auto cx = Expr(0.25f)*rdx*(u(i+1,k,j) + u(i,k,j) + u(i+1,max(k-1,0),j) + u(i,max(k-1,0),j));
    auto cy = Expr(0.25f)*rdy*(v(i,k,j+1) + v(i,k,j) + v(i,max(k-1,0),j+1) + v(i,max(k-1,0),j));
    
    auto cw_max = max(alpha_max - dt*Ceps*sqrt(cx*cx + cy*cy), Expr(0.0f));
    
    auto cw_max_gt_zero = cw_max > Expr(0.0f);

    auto cr = ww(i,k,j) * dt * rdnw(k) / (c1f(k)*mut(i,j) + c2f(k));
    auto cw_max2 = cw_max * cw_max;
    auto cw_min = cw_max * cmnx_ratio;
    auto cw = abs(cr);
    
    auto cff = select(cw < cw_min, cw_max2,
                     cw < cutoff*cw_min, cw_max2 + r4cmx*(cw-cw_min)*(cw-cw_min),
                     cw_max*cw);
    
    auto wfrac = cw_max2 / cff;
    wfrac = max(min(wfrac, Expr(1.0f)), Expr(0.0f));
        
    wwE(i,k,j) = select(cw_max_gt_zero, ww(i,k,j) * wfrac, Expr(0.0f));
    wwI(i,k,j) = select(cw_max_gt_zero, ww(i,k,j) * (Expr(1.0f) - wfrac), ww(i,k,j));
    
    return wwE;
}

extern "C" {
    void ww_split_(
        float* wwE, float* wwI,
        const float* u, const float* v, const float* ww,
        const float* mut, const float* rdnw,
        const float* c1f, const float* c2f,
        const int* its, const int* ite,
        const int* jts, const int* jte,
        const int* kts, const int* kte
    );
}

int main(int argc, char** argv)
{
    const int its = 0;
    const int ite = 1024;
    const int jts = 0;
    const int jte = 1024;
    const int kts = 0;
    const int kte = 1024;

    const int d1_range = ite - its; // i: its to ite
    const int d2_range = kte - kts;  // k: kts+1 to kte-1
    const int d3_range = jte - jts; // j: jts to jte

    const int ww_d1_range = d1_range + 1;
    const int ww_d2_range = d2_range + 3;
    const int ww_d3_range = d3_range + 1;

    const int u_d1_range = d1_range + 3; // its:ite+1
    const int u_d2_range = d2_range + 4; // kts-1:kte 
    const int u_d3_range = d3_range + 1;

    const int v_d1_range = d1_range + 1;
    const int v_d2_range = d2_range + 4; // kts-1:kte 
    const int v_d3_range = d3_range + 3; // jts:jte+1

    const int rdnw_d2_range = d2_range + 4; // kts:kte
    const int c1f_d2_range = d2_range + 3;
    const int c2f_d2_range = d2_range + 3;

    const int out_d1_range = d1_range + 1;
    const int out_d2_range = d2_range + 3;
    const int out_d3_range = d3_range + 1;

    // --------------------------- Preparation --------------------------
    printf("Prepare the random data\n");
    bool with_gpu = true;
    Var d1("i"), d2("k"), d3("j"), d1o, d2o, d3o, d1i, d2i, d3i;

    Target gpu_target = find_gpu_target();
    if (!gpu_target.has_gpu_feature())
    {
        printf("Don't have a gpu!\n");
        with_gpu = false;
    }
    Target cpu_target = get_host_target();

    // Input data generation functions
    auto i3_cpu = init_nonzero<3>("init", d1_range + 1 >= 8);
    auto i2_cpu = init_nonzero<2>("init", d1_range + 1 >= 8);
    auto i1_cpu = init_nonzero<1>("init", d1_range + 1 >= 8);
    auto z3_cpu = set_zero<3>("zero", d1_range + 1>= 8);
    auto i3_gpu = i3_cpu;
    auto i2_gpu = i2_cpu;
    auto i1_gpu = i1_cpu;
    auto z3_gpu = z3_cpu;

    i3_cpu.compile_jit(cpu_target);
    i2_cpu.compile_jit(cpu_target);
    i1_cpu.compile_jit(cpu_target);
    z3_cpu.compile_jit(cpu_target);
    if ( with_gpu )
    {
        i3_gpu.compile_jit(gpu_target);
        i2_gpu.compile_jit(gpu_target);
        i1_gpu.compile_jit(gpu_target);
        z3_gpu.compile_jit(gpu_target);
    }
    
    // Create input buffers
    Buffer<float, 1> rdnw(rdnw_d2_range, "rdnw"), rdnw_g(rdnw);
    Buffer<float, 1> c1f(c1f_d2_range, "c1f"), c1f_g(c1f),
                     c2f(c2f_d2_range, "c2f"), c2f_g(c2f);

    Buffer<float, 2> mut({ww_d1_range, ww_d3_range}, "mut"), mut_g(mut);
    
    Buffer<float, 3> ww({ww_d1_range, ww_d2_range, ww_d3_range}, "ww"), ww_g(ww),
                     u({u_d1_range, u_d2_range, u_d3_range}, "u"), u_g(u),
                     v({v_d1_range, v_d2_range, v_d3_range}, "v"), v_g(v);

    // Fortran output
    Buffer<float, 3> out_base({out_d1_range, out_d2_range, out_d3_range}, "out_base"); 

    // Halide CPU output
    Buffer<float, 3> out_cpu({out_d1_range, out_d2_range, out_d3_range}, "out_cpu"); 

    // Halide GPU output
    Buffer<float, 3> out_gpu({out_d1_range, out_d2_range, out_d3_range}, "out_gpu"); 

    // --------------------------- ww_split kernel --------------------------
    printf("miel3 kernel start!\n");

    // Coordinate transformations for proper indexing
 
    auto res_cpu = targetFunction(ww, u, v, mut, rdnw, c1f, c2f);

    // CPU optimizations
    res_cpu.parallel(d3);
    if (d1_range >= 8) res_cpu.vectorize(d1,8);
    res_cpu.compile_jit(cpu_target);

    try {
    i3_cpu.realize(ww);
    i3_cpu.realize(u);
    i3_cpu.realize(v);
    i2_cpu.realize(mut);
    i1_cpu.realize(rdnw);
    i1_cpu.realize(c1f);
    i1_cpu.realize(c2f);
    } catch (RuntimeError &e)
    {
        std::cerr << e.what();
    }

    z3_cpu.realize(out_base);

    ww_split_(
        out_base.get()->begin(),
        out_base.get()->begin() + out_d1_range*out_d2_range*out_d3_range,
        u.get()->begin(),
        v.get()->begin(),
        ww.get()->begin(),
        mut.get()->begin(),
        rdnw.get()->begin(),
        c1f.get()->begin(),
        c2f.get()->begin(),
        &its, &ite,
        &jts, &jte,
        &kts, &kte
    );

    // CPU execution
    try {
        z3_cpu.realize(out_cpu);
        res_cpu.realize(out_cpu);
    } catch ( RuntimeError &e ) {
        std::cerr << e.what();
        return 1;
    }

    double correctness_1 = 0.0;
    for (int j = 0; j < out_d3_range; j++) {
        for (int k = 0; k < out_d2_range; k++) {
            for (int i = 0; i < out_d1_range; i++) {
                int a = i;
                int b = k;
                int c = j;
                auto A = out_base(a,b,c);
                auto B = out_cpu(i,k,j);
                double diff1 = abs(A - B);
                if ( diff1 >= 1e-1 )
                {
                    printf("output1 = Fort[%d, %d, %d]: %lf | Hali[%d, %d, %d]: %lf\n", 
                            a, b, c, A, i, k, j, B);
                }
                correctness_1 += diff1;
            }
        }
    }
    printf("The number for correctness_1 is %d\n",int(correctness_1/(out_d1_range*out_d2_range*out_d3_range)));

    int times = 20;
    double cost_time = 0;
    struct timespec t1, t2, elapsed;
    clock_gettime(CLOCK_REALTIME, &t1);
    for (int i = 0; i < times; i++)
    {
        clock_gettime(CLOCK_REALTIME, &t1);
        ww_split_(
            out_base.get()->begin(),
            out_base.get()->begin() + out_d1_range*out_d2_range*out_d3_range,
            u.get()->begin(),
            v.get()->begin(),
            ww.get()->begin(),
            mut.get()->begin(),
            rdnw.get()->begin(),
            c1f.get()->begin(),
            c2f.get()->begin(),
            &its, &ite,
            &jts, &jte,
            &kts, &kte
        );
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("legacy code average cost time: %lfms\n\n",cost_time/times*1000);

    cost_time = 0;
    clock_gettime(CLOCK_REALTIME, &t1);
    for (int i = 0; i < times; i++)
    {
        clock_gettime(CLOCK_REALTIME, &t1);
        res_cpu.realize(out_cpu);
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("lifted cpu average cost time: %lfms\n\n",cost_time/times*1000);

    if ( with_gpu )
    {
        auto res_gpu = targetFunction(ww_g, u_g, v_g, mut_g, rdnw_g, c1f_g, c2f_g);

        // GPU optimizations
        res_gpu.gpu_tile(d1,d2,d3,d1o,d2o,d3o,d1i,d2i,d3i,8,8,4)
                .compile_jit(gpu_target);
    
        // IO initialization
        i3_gpu.realize(ww_g);
        i3_gpu.realize(u_g);
        i3_gpu.realize(v_g);
        i2_gpu.realize(mut_g);
        i1_gpu.realize(rdnw_g);
        i1_gpu.realize(c1f_g);
        i1_gpu.realize(c2f_g);
        
        // GPU warm up
        try {
            z3_gpu.realize(out_gpu);
            res_gpu.realize(out_gpu);
        } catch ( RuntimeError &e ) {
            std::cerr << e.what();
            return 2;
        }

        cost_time = 0;
        clock_gettime(CLOCK_REALTIME, &t1);
        for (int i = 0; i < times; i++)
        {
            res_gpu.realize(out_gpu);
        }
        out_gpu.copy_to_host();
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
        printf("lifted gpu average cost time: %lfms\n\n",cost_time/times*1000);
    }
    return 0;
}
