#include <ctime>
#include <Halide.h>

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

template<>
inline Func init_nonzero<2>(const std::string& funcName, bool vectorize)
{
    Var d1, d2;
    Func nz(funcName);
    nz(d1, d2) = ((((d1 + d2) % 13) + Expr(0.1)) / Expr(13.0)) * cast<double>(10);

    return nz;
}

template<>
inline Func init_nonzero<3>(const std::string& funcName, bool vectorize)
{
    Var d1, d2, d3;
    Func nz(funcName);
    nz(d1, d2, d3) = ((((d1 + d2 + d3) % 13) + Expr(0.1)) / Expr(13.0)) * cast<double>(10);
    

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

inline Func targetFunction(const std::string& funcName,
                           Buffer<double, 1>& vertexdy,
                           const Expr dy,
                           const Expr y_min, const Expr y_max)
{
    Var k("k");
    Func target(funcName);
    
    // Fortran: vertexdy(k) = dy
    target(k) = dy;
    return target;
}

extern "C" {
    void initialise_chunk_kernel_loop9_(
        const int *k,
        const double *dy,
        double *vertexdy,
        const int* y_max, const int* y_min);
}

#ifndef _1D_1
#define _1D_1 1.5e7
#endif

int main(int argc, char** argv)
{
    const int y_max = _1D_1;
    const int y_min = 0;

    const int y_range = y_max - y_min;

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

    Func init1_cpu = init_nonzero<1>("init1", y_range + 6 >= 8);
    Func zero1_cpu = set_zero<1>("zero1", y_range + 6 >= 8);
    Func init1_gpu = init1_cpu;
    Func zero1_gpu = zero1_cpu;
    init1_cpu.compile_jit(cpu_target);
    zero1_cpu.compile_jit(cpu_target);
    if (with_gpu)
    {
        init1_gpu.compile_jit(gpu_target);
        zero1_gpu.compile_jit(gpu_target);
    }

    // Array dimensions:
    // vertexdy: (y_min-2):(y_max+3) -> y_range + 6
    Buffer<double, 1> vertexdy_base(y_range + 6, "vertexdy"), vertexdy_g(vertexdy_base);
    Buffer<double, 1> vertexdy_cpu(y_range + 6, "vertexdy");
    Buffer<double, 1> vertexdy_gpu(y_range + 6, "vertexdy");

    // --------------------------- initialise_chunk_kernel_loop9 kernel --------------------------
    
    double dy = 0.25;
    
    // building cpu func
    Func cpu_fn = targetFunction("initialise_chunk_kernel_loop9_cpu", 
                                vertexdy_cpu, Expr(dy),
                                Expr(y_min), Expr(y_max));
    // cpu_fn.parallel(k);
    if (y_range + 6 >= 8) cpu_fn.vectorize(k, 8);
    cpu_fn.compile_jit(cpu_target);

    // IO initialization
    init1_cpu.realize(vertexdy_base);

    // Calling baseline
    int dummy_k = 0;
    double *vertexdy_ = vertexdy_base.get()->begin();
    initialise_chunk_kernel_loop9_(
        &dummy_k,
        &dy,
        vertexdy_,
        &y_max, &y_min);
    
    // Calling halide cpu
    try {
        zero1_cpu.realize(vertexdy_cpu);
        cpu_fn.realize(vertexdy_cpu);
    } catch (RuntimeError &e) {
        std::cerr << e.what();
        return 1;
    }

    double correctness_1 = 0.0;
    int errors = 0;
    for (int k_idx = 0; k_idx < y_range + 6; k_idx++) {
        // Fortran loop: k from y_min-2 to y_max+3
        // This corresponds to k_idx from 0 to y_range+5
        
        double diff = abs(vertexdy_base(k_idx) - vertexdy_cpu(k_idx));
        if (diff >= 1e-10)
        {
            if (errors < 10) {
                printf("output [%d] = Fortran: %lf | Halide: %lf | diff: %e\n", 
                       k_idx, vertexdy_base(k_idx), vertexdy_cpu(k_idx), diff);
            }
            errors++;
        }
        correctness_1 += diff;
    }
    
    if (errors > 0) {
        printf("Total errors: %d, sum of differences: %e\n", errors, correctness_1);
    }

    int times = 100;
    double cost_time = 0;
    struct timespec t1, t2, elapsed;
    clock_gettime(CLOCK_REALTIME, &t1);
    for (int i = 0; i < times; i++)
    {
        clock_gettime(CLOCK_REALTIME, &t1);
        init1_cpu.realize(vertexdy_base);
        initialise_chunk_kernel_loop9_(
            &dummy_k,
            &dy,
            vertexdy_,
            &y_max, &y_min);
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("legacy code: %lfms\n", cost_time/times*10000);

    cost_time = 0;
    clock_gettime(CLOCK_REALTIME, &t1);
    for (int i = 0; i < times; i++)
    {
        clock_gettime(CLOCK_REALTIME, &t1);
        init1_cpu.realize(vertexdy_base);
        cpu_fn.realize(vertexdy_cpu);
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("lifted cpu: %lfms\n", cost_time/times*1000);

    if (with_gpu)
    {
        // building gpu func
        Func gpu_fn = targetFunction("initialise_chunk_kernel_loop9_gpu", 
                                    vertexdy_gpu, Expr(dy),
                                    Expr(y_min), Expr(y_max));
        gpu_fn.gpu_tile(k, bk, tk, 256)
              .compile_jit(gpu_target);
        
        // GPU IO initialization
        vertexdy_g.copy_from(vertexdy_base);
        
        // warmup
        try {
            zero1_gpu.realize(vertexdy_gpu);
            gpu_fn.realize(vertexdy_gpu);
        } catch (RuntimeError &e) {
            std::cerr << e.what();
            return -1;
        }

        cost_time = 0;
        clock_gettime(CLOCK_REALTIME, &t1);
        for (int i = 0; i < times; i++)
        {
            gpu_fn.realize(vertexdy_gpu);
        }
        vertexdy_gpu.copy_to_host();
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time = toSec(elapsed);
        printf("lifted gpu: %lfms\n\n", cost_time/times*1000);
    }

    return 0;
} 