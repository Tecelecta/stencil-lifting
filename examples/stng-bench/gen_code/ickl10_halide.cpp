#include <ctime>
#include <Halide.h>
#include <cstdio>
#include <cstdlib>
#include <cmath>

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
    Target target = get_host_target();

    std::vector<Target::Feature> features_to_try;
    features_to_try.push_back(Target::CUDA);

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
    nz(d) = (((d % 13) + Expr(0.1)) / Expr(13.0)) * cast<double>(10);

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

inline Func targetFunction(const std::string& funcName,
                           Buffer<double, 1>& celly,
                           const Buffer<double, 1>& vertexx,
                           const Expr x_min, const Expr x_max)
{
    Var k("k");
    Func target(funcName);
    
    // Fortran: celly(k) = 0.5 * (vertexx(k) + vertexx(k + 1))
    target(k) = select(k < (cast<int>(x_max) - cast<int>(x_min) + 5), 
                       Expr(0.5) * (vertexx(k) + vertexx(k + 1)), 
                       Expr(0.0));
    return target;
}

extern "C" {
    void initialise_chunk_kernel_loop10_(int *k,
                                        double *celly,
                                        double *vertexx,
                                        const int* x_max,
                                        const int* x_min);
}

#ifndef _1D_1
#define _1D_1 1.5e7
#endif

int main(int argc, char** argv)
{
    const int x_max = _1D_1;
    const int x_min = 0;
    const int x_range = x_max - x_min;

    // --------------------------- Preparation --------------------------
    bool with_gpu = true;
    
    Var k("k");
    Var bk, tk;

    Target gpu_target = find_gpu_target();
    if (!gpu_target.has_gpu_feature())
    {
        printf("Don't have a gpu!\n");
        with_gpu = false;
    }
    Target cpu_target = get_host_target();

    Func init1_cpu = init_nonzero<1>("init", x_range + 6 >= 8);
    Func zero_cpu = set_zero<1>("zero", x_range + 5 >= 8);
    Func init1_gpu = init1_cpu;
    Func zero_gpu = zero_cpu;
    init1_cpu.compile_jit(cpu_target);
    zero_cpu.compile_jit(cpu_target);
    if (with_gpu)
    {
        init1_gpu.compile_jit(gpu_target);
        zero_gpu.compile_jit(gpu_target);
    }

    // celly: (x_min-2):(x_max+2) -> x_range + 5
    // vertexx: (x_min-2):(x_max+3) -> x_range + 6
    Buffer<double,1> celly_base(x_range + 5, "celly");
    Buffer<double,1> vertexx(x_range + 6, "vertexx");
    Buffer<double,1> celly_cpu(x_range + 5, "celly");
    Buffer<double,1> celly_gpu(x_range + 5, "celly");
    Buffer<double,1> vertexx_g(vertexx);

    // --------------------------- initialise_chunk_kernel_loop10 kernel --------------------------
    int k_dummy = 0;

    // building baseline stencil
    Func cpu_fn = targetFunction("initialise_chunk_kernel_loop10_cpu",
                                celly_cpu, vertexx,
                                Expr(x_min), Expr(x_max));
    if (x_range + 5 >= 8) cpu_fn.vectorize(k, 8);
    cpu_fn.compile_jit(cpu_target);

    // IO initialization
    init1_cpu.realize(vertexx);
    zero_cpu.realize(celly_base);
    zero_cpu.realize(celly_cpu);

    // Calling baseline Fortran
    double *celly_ = celly_base.get()->begin();
    double *vertexx_ = vertexx.get()->begin();
    initialise_chunk_kernel_loop10_(&k_dummy, celly_, vertexx_, &x_max, &x_min);
    
    // Calling halide cpu
    try {
        cpu_fn.realize(celly_cpu);
    } catch (RuntimeError &e) {
        std::cerr << e.what();
        return 1;
    }

    double correctness_1 = 0.0;
    for (int a = 0; a < x_range + 5; a++) {
        double diff = abs(celly_base(a) - celly_cpu(a));
        if (diff >= 1e-3)
        {
            printf("output1 [%d] = A: %lf | B: %lf\n", 
                    a, celly_base(a), celly_cpu(a));
        }
        correctness_1 += diff;
    }

    int times = 100;
    double cost_time = 0;
    struct timespec t1, t2, elapsed;
    
    // Baseline Fortran timing
    cost_time = 0;
    for (int i = 0; i < times; i++)
    {
        clock_gettime(CLOCK_REALTIME, &t1);
        zero_cpu.realize(celly_base);
        zero_cpu.realize(celly_cpu);
        initialise_chunk_kernel_loop10_(&k_dummy, celly_, vertexx_, &x_max, &x_min);
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("legacy code: %lfms\n", cost_time/times*10000);

    // Halide CPU timing
    cost_time = 0;
    for (int i = 0; i < times; i++)
    {
        clock_gettime(CLOCK_REALTIME, &t1);
        init1_cpu.realize(vertexx);
        cpu_fn.realize(celly_cpu);
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("lifted cpu: %lfms\n", cost_time/times*1000);

    if (with_gpu)
    {
        // building gpu func
        Func gpu_fn = targetFunction("initialise_chunk_kernel_loop10_gpu",
                                    celly_gpu, vertexx_g,
                                    Expr(x_min), Expr(x_max));
        gpu_fn.gpu_tile(k, bk, tk, 256).compile_jit(gpu_target);
        
        // GPU IO initialization
        vertexx_g.copy_from(vertexx);
        
        // warmup
        try {
            zero_gpu.realize(celly_gpu);
            gpu_fn.realize(celly_gpu);
        } catch (RuntimeError &e) {
            std::cerr << e.what();
            return -1;
        }

        cost_time = 0;
        clock_gettime(CLOCK_REALTIME, &t1);
        for (int i = 0; i < times; i++)
        {
            gpu_fn.realize(celly_gpu);
        }
        celly_gpu.copy_to_host();
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time = toSec(elapsed);
        printf("lifted gpu: %lfms\n\n", cost_time/times*1000);
    }

    return 0;
} 