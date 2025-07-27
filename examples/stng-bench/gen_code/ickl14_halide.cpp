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
inline Func init_nonzero<2>(const std::string& funcName, bool vectorize)
{
    Var d1, d2;
    Func nz(funcName);
    nz(d1, d2) = ((((d1 + d2) % 13) + Expr(0.1)) / Expr(13.0)) * cast<double>(10);
    nz.parallel(d2);
    if (vectorize) nz.vectorize(d1, 8);
    return nz;
}

template <>
inline Func set_zero<2>(const std::string& funcName, bool vectorize)
{
    Var d1, d2;
    Func set_zero(funcName);
    set_zero(d1, d2) = Expr(0.0);
    set_zero.parallel(d2);
    if (vectorize) set_zero.vectorize(d1, 8);
    return set_zero;
}

inline Func targetFunction(const std::string& funcName,
                           Buffer<double, 2>& volume,
                           const Expr dx, const Expr dy,
                           const Expr x_min, const Expr x_max,
                           const Expr y_min, const Expr y_max)
{
    Var j("j"), k("k");
    Func target(funcName);
    
    // Fortran: volume(j,k) = dx * dy
    // Direct implementation - set each element to the constant value dx * dy
    target(j, k) = dx * dy;
    return target;
}

extern "C" {
    void initialise_chunk_kernel_loop14_(int *j, int *k,
                                        double *dx, double *dy,
                                        double *volume,
                                        const int* x_max, const int* x_min,
                                        const int* y_max, const int* y_min);
}

int main(int argc, char** argv)
{
    const int x_max = 2e4;
    const int x_min = 0;
    const int y_max = 2e4;
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

    Func init2_cpu = init_nonzero<2>("init", x_range + 5 >= 8);
    Func zero_cpu = set_zero<2>("zero", x_range + 5 >= 8);
    Func init2_gpu = init2_cpu;
    Func zero_gpu = zero_cpu;
    init2_cpu.compile_jit(cpu_target);
    zero_cpu.compile_jit(cpu_target);
    if (with_gpu)
    {
        init2_gpu.compile_jit(gpu_target);
        zero_gpu.compile_jit(gpu_target);
    }

    // volume: (x_min-2):(x_max+2), (y_min-2):(y_max+2) -> x_range + 5, y_range + 5
    Buffer<double,2> volume_base({x_range + 5, y_range + 5}, "volume");
    Buffer<double,2> volume_cpu({x_range + 5, y_range + 5}, "volume");
    Buffer<double,2> volume_gpu({x_range + 5, y_range + 5}, "volume");

    // --------------------------- initialise_chunk_kernel_loop14 kernel --------------------------
    double dx = 1.5;
    double dy = 2.5;
    int j_dummy = 0;
    int k_dummy = 0;

    // building baseline stencil
    Func cpu_fn = targetFunction("initialise_chunk_kernel_loop14_cpu",
                                volume_cpu, Expr(dx), Expr(dy),
                                Expr(x_min), Expr(x_max),
                                Expr(y_min), Expr(y_max));
    cpu_fn.parallel(k);
    if (x_range + 5 >= 8) cpu_fn.vectorize(j, 8);
    cpu_fn.compile_jit(cpu_target);

    // IO initialization
    zero_cpu.realize(volume_base);
    zero_cpu.realize(volume_cpu);

    // Calling baseline Fortran
    double *volume_ = volume_base.get()->begin();
    initialise_chunk_kernel_loop14_(&j_dummy, &k_dummy, &dx, &dy, volume_, &x_max, &x_min, &y_max, &y_min);
    
    // Calling halide cpu
    try {
        cpu_fn.realize(volume_cpu);
    } catch (RuntimeError &e) {
        std::cerr << e.what();
        return 1;
    }

    double correctness_1 = 0.0;
    for (int b = 0; b < y_range + 5; b++) {
        for (int a = 0; a < x_range + 5; a++) {
            double diff = abs(volume_base(a, b) - volume_cpu(a, b));
            if (diff >= 1e-3)
            {
                printf("output1 [%d, %d] = A: %lf | B: %lf\n", 
                        a, b, volume_base(a, b), volume_cpu(a, b));
            }
            correctness_1 += diff;
        }
    }

    int times = 10;
    double cost_time = 0;
    struct timespec t1, t2, elapsed;
    
    // Baseline Fortran timing
    cost_time = 0;
    for (int i = 0; i < times; i++)
    {
        clock_gettime(CLOCK_REALTIME, &t1);
        cpu_fn.realize(volume_cpu);
        initialise_chunk_kernel_loop14_(&j_dummy, &k_dummy, &dx, &dy, volume_, &x_max, &x_min, &y_max, &y_min);
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
        cpu_fn.realize(volume_cpu);
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("lifted cpu: %lfms\n", cost_time/times*1000);

    if (with_gpu)
    {
        // building gpu func
        Func gpu_fn = targetFunction("initialise_chunk_kernel_loop14_gpu",
                                    volume_gpu, Expr(dx), Expr(dy),
                                    Expr(x_min), Expr(x_max),
                                    Expr(y_min), Expr(y_max));
        gpu_fn.gpu_tile(j, k, bj, bk, tj, tk, 8, 8)
              .compile_jit(gpu_target);
        
        // warmup
        try {
            zero_gpu.realize(volume_gpu);
            gpu_fn.realize(volume_gpu);
        } catch (RuntimeError &e) {
            std::cerr << e.what();
            return -1;
        }

        cost_time = 0;
        clock_gettime(CLOCK_REALTIME, &t1);
        for (int i = 0; i < times; i++)
        {
            gpu_fn.realize(volume_gpu);
        }
        volume_gpu.copy_to_host();
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time = toSec(elapsed);
        printf("lifted gpu: %lfms\n\n", cost_time/times*1000);
    }

    return 0;
} 