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
    if (vectorize) nz.vectorize(d, 8);
    return nz;
}

template <>
inline Func set_zero<1>(const std::string& funcName, bool vectorize)
{
    Var d;
    Func set_zero(funcName);
    set_zero(d) = Expr(0.0);
    if (vectorize) set_zero.vectorize(d, 8);
    return set_zero;
}

inline Func targetFunction(const std::string& funcName,
                           Buffer<double, 1>& celldy,
                           const Expr dy,
                           const Expr y_min, const Expr y_max)
{
    Var k("k");
    Func target(funcName);
    
    // Fortran: celldy(k) = dy
    // Direct implementation - set each element to the constant value dy
    target(k) = dy;
    return target;
}

extern "C" {
    void initialise_chunk_kernel_loop13_(int *k,
                                        double *celldy,
                                        double *dy,
                                        const int* y_max,
                                        const int* y_min);
}

int main(int argc, char** argv)
{
    const int y_max = 1.5e7;
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

    Func init1_cpu = init_nonzero<1>("init", y_range + 5 >= 8);
    Func zero_cpu = set_zero<1>("zero", y_range + 5 >= 8);
    Func init1_gpu = init1_cpu;
    Func zero_gpu = zero_cpu;
    init1_cpu.compile_jit(cpu_target);
    zero_cpu.compile_jit(cpu_target);
    if (with_gpu)
    {
        init1_gpu.compile_jit(gpu_target);
        zero_gpu.compile_jit(gpu_target);
    }

    // celldy: (y_min-2):(y_max+2) -> y_range + 5
    Buffer<double,1> celldy_base(y_range + 5, "celldy");
    Buffer<double,1> celldy_cpu(y_range + 5, "celldy");
    Buffer<double,1> celldy_gpu(y_range + 5, "celldy");

    // --------------------------- initialise_chunk_kernel_loop13 kernel --------------------------
    double dy = 2.5;
    int k_dummy = 0;

    // building baseline stencil
    Func cpu_fn = targetFunction("initialise_chunk_kernel_loop13_cpu",
                                celldy_cpu, Expr(dy),
                                Expr(y_min), Expr(y_max));
    if (y_range + 5 >= 8) cpu_fn.vectorize(k, 8);
    cpu_fn.compile_jit(cpu_target);

    // IO initialization
    zero_cpu.realize(celldy_base);
    zero_cpu.realize(celldy_cpu);

    // Calling baseline Fortran
    double *celldy_ = celldy_base.get()->begin();
    initialise_chunk_kernel_loop13_(&k_dummy, celldy_, &dy, &y_max, &y_min);
    
    // Calling halide cpu
    try {
        cpu_fn.realize(celldy_cpu);
    } catch (RuntimeError &e) {
        std::cerr << e.what();
        return 1;
    }

    double correctness_1 = 0.0;
    for (int a = 0; a < y_range + 5; a++) {
        double diff = abs(celldy_base(a) - celldy_cpu(a));
        if (diff >= 1e-3)
        {
            printf("output1 [%d] = A: %lf | B: %lf\n", 
                    a, celldy_base(a), celldy_cpu(a));
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
        zero_cpu.realize(celldy_base);
        cpu_fn.realize(celldy_cpu);
        initialise_chunk_kernel_loop13_(&k_dummy, celldy_, &dy, &y_max, &y_min);
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
        zero_cpu.realize(celldy_base);
        cpu_fn.realize(celldy_cpu);
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("lifted cpu: %lfms\n", cost_time/times*1000);

    if (with_gpu)
    {
        // building gpu func
        Func gpu_fn = targetFunction("initialise_chunk_kernel_loop13_gpu",
                                    celldy_gpu, Expr(dy),
                                    Expr(y_min), Expr(y_max));
        gpu_fn.gpu_tile(k, bk, tk, 256).compile_jit(gpu_target);
        
        // warmup
        try {
            zero_gpu.realize(celldy_gpu);
            gpu_fn.realize(celldy_gpu);
        } catch (RuntimeError &e) {
            std::cerr << e.what();
            return -1;
        }

        cost_time = 0;
        clock_gettime(CLOCK_REALTIME, &t1);
        for (int i = 0; i < times; i++)
        {
            gpu_fn.realize(celldy_gpu);
        }
        celldy_gpu.copy_to_host();
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time = toSec(elapsed);
        printf("lifted gpu: %lfms\n\n", cost_time/times*1000);
    }

    return 0;
} 