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

template<>
inline Func init_nonzero<2>(const std::string& funcName, bool vectorize)
{
    Var d1, d2;
    Func nz(funcName);
    nz(d1, d2) = ((((d1 + d2) % 13) + Expr(0.1)) / Expr(13.0)) * cast<double>(10);

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

inline Func targetFunction(const std::string& funcName,
                           Buffer<double, 2>& xarea,
                           const Buffer<double, 1>& celldy,
                           const Expr x_min, const Expr x_max,
                           const Expr y_min, const Expr y_max)
{
    Var j("j"), k("k");
    Func target(funcName);
    
    auto roi = j <= 20004 && k <= 20004;
    
    target(j, k) = select(roi, celldy(k), Expr(0.0));
    return target;
}

extern "C" {
    void initialise_chunk_kernel_loop15_(int *j, int *k,
                                        double *celldy,
                                        const int* x_max, const int* x_min,
                                        double *xarea,
                                        const int* y_max, const int* y_min);
}

#ifndef _2D_1
#define _2D_1 2e4
#define _2D_2 2e4
#endif

int main(int argc, char** argv)
{
    const int x_max = _2D_1;
    const int x_min = 0;
    const int y_max = _2D_2;
    const int y_min = 0;

    const int x_range = x_max - x_min;
    const int y_range = y_max - y_min;

    // --------------------------- Preparation --------------------------
    bool with_gpu = true;
    
    Var j("j"), k("k");
    Var bj, bk, tj, tk;

    Target gpu_target = find_gpu_target();
    if (!gpu_target.has_gpu_feature())
    {
        printf("Don't have a gpu!\n");
        with_gpu = false;
    }
    Target cpu_target = get_host_target();

    Func init1_cpu = init_nonzero<1>("init1", y_range + 5 >= 8);
    Func init2_cpu = init_nonzero<2>("init2", x_range + 6 >= 8);
    Func zero1_cpu = set_zero<1>("zero1", y_range + 5 >= 8);
    Func zero2_cpu = set_zero<2>("zero2", x_range + 6 >= 8);
    Func init1_gpu = init1_cpu;
    Func init2_gpu = init2_cpu;
    Func zero1_gpu = zero1_cpu;
    Func zero2_gpu = zero2_cpu;
    
    init1_cpu.compile_jit(cpu_target);
    init2_cpu.compile_jit(cpu_target);
    zero1_cpu.compile_jit(cpu_target);
    zero2_cpu.compile_jit(cpu_target);
    
    if (with_gpu)
    {
        init1_gpu.compile_jit(gpu_target);
        init2_gpu.compile_jit(gpu_target);
        zero1_gpu.compile_jit(gpu_target);
        zero2_gpu.compile_jit(gpu_target);
    }

    // celldy: (y_min-2):(y_max+2) -> y_range + 5
    // xarea: (x_min-2):(x_max+3), (y_min-2):(y_max+2) -> (x_range + 6) × (y_range + 5)
    Buffer<double,1> celldy(y_range + 5, "celldy");
    Buffer<double,1> celldy_g(celldy);
    Buffer<double,2> xarea_base({x_range + 6, y_range + 5}, "xarea");
    Buffer<double,2> xarea_cpu({x_range + 6, y_range + 5}, "xarea");
    Buffer<double,2> xarea_gpu({x_range + 6, y_range + 5}, "xarea");

    // --------------------------- initialise_chunk_kernel_loop15 kernel --------------------------
    int j_dummy = 0;
    int k_dummy = 0;

    // building baseline stencil
    Func cpu_fn = targetFunction("initialise_chunk_kernel_loop15_cpu",
                                xarea_cpu, celldy,
                                Expr(x_min), Expr(x_max),
                                Expr(y_min), Expr(y_max));
    cpu_fn.parallel(k);
    if (x_range + 6 >= 8) cpu_fn.vectorize(j, 8);
    cpu_fn.compile_jit(cpu_target);

    // IO initialization
    init1_cpu.realize(celldy);
    zero2_cpu.realize(xarea_base);
    zero2_cpu.realize(xarea_cpu);

    // Calling baseline Fortran
    double *celldy_ = celldy.get()->begin();
    double *xarea_ = xarea_base.get()->begin();
    initialise_chunk_kernel_loop15_(&j_dummy, &k_dummy, celldy_, &x_max, &x_min, xarea_, &y_max, &y_min);
    
    // Calling halide cpu
    try {
        cpu_fn.realize(xarea_cpu);
    } catch (RuntimeError &e) {
        std::cerr << e.what();
        return 1;
    }

    double correctness_1 = 0.0;
    for (int b = 0; b < y_range + 5; b++) {
        for (int a = 0; a < x_range + 6; a++) {
            double diff = abs(xarea_base(a, b) - xarea_cpu(a, b));
            if (diff >= 1e-3)
            {
                printf("output1 [%d, %d] = A: %lf | B: %lf\n", 
                        a, b, xarea_base(a, b), xarea_cpu(a, b));
            }
            correctness_1 += diff;
        }
    }

    int times = 5;
    double cost_time = 0;
    struct timespec t1, t2, elapsed;
    
    // Baseline Fortran timing
    cost_time = 0;
    for (int i = 0; i < times; i++)
    {
        clock_gettime(CLOCK_REALTIME, &t1);
        zero2_cpu.realize(xarea_base);
        initialise_chunk_kernel_loop15_(&j_dummy, &k_dummy, celldy_, &x_max, &x_min, xarea_, &y_max, &y_min);
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("legacy code: %lfms\n", cost_time/times*1000);

    // Halide CPU timing
    cost_time = 0;
    for (int i = 0; i < times; i++)
    {
        clock_gettime(CLOCK_REALTIME, &t1);
        cpu_fn.realize(xarea_cpu);
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("lifted cpu: %lfms\n", cost_time/times*1000);

    if (with_gpu)
    {
        // building gpu func
        Func gpu_fn = targetFunction("initialise_chunk_kernel_loop15_gpu",
                                    xarea_gpu, celldy_g,
                                    Expr(x_min), Expr(x_max),
                                    Expr(y_min), Expr(y_max));
        gpu_fn.gpu_tile(j, k, bj, bk, tj, tk, 8, 8)
              .compile_jit(gpu_target);
        
        // GPU IO initialization
        celldy_g.copy_from(celldy);
        
        // warmup
        try {
            zero2_gpu.realize(xarea_gpu);
            gpu_fn.realize(xarea_gpu);
        } catch (RuntimeError &e) {
            std::cerr << e.what();
            return -1;
        }

        cost_time = 0;
        clock_gettime(CLOCK_REALTIME, &t1);
        for (int i = 0; i < times; i++)
        {
            gpu_fn.realize(xarea_gpu);
        }
        xarea_gpu.copy_to_host();
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time = toSec(elapsed);
        printf("lifted gpu: %lfms\n\n", cost_time/times*1000);
    }

    return 0;
} 