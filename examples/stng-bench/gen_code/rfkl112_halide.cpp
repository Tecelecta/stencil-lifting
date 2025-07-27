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
    if (vectorize) nz.vectorize(d, 8);
    return nz;
}

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

template<>
inline Func init_nonzero<3>(const std::string& funcName, bool vectorize)
{
    Var d1, d2, d3;
    Func nz(funcName);
    nz(d1, d2, d3) = ((((d1 + d2 + d3) % 13) + Expr(0.1)) / Expr(13.0)) * cast<double>(10);
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
    if (vectorize) set_zero.vectorize(d, 8);
    return set_zero;
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

template <>
inline Func set_zero<3>(const std::string& funcName, bool vectorize)
{
    Var d1, d2, d3;
    Func set_zero(funcName);
    set_zero(d1, d2, d3) = Expr(0.0);

    return set_zero;
}

inline Func targetFunction(const std::string& funcName,
                           Buffer<double, 2>& yvel0,
                           const Buffer<double, 2>& yvel1,
                           const Expr x_min, const Expr x_max,
                           const Expr y_min, const Expr y_max)
{
    Var j("j"), k("k");
    Func target(funcName);
    auto roi = x_min <= j && j <= (x_max + 1) && 
               y_min <= k && k <= (y_max + 1);
    
    // Fortran: yvel0(j,k) = yvel1(j,k)
    // Convert Fortran indices to C++ buffer indices
    // Fortran j,k range: [x_min, x_max+1], [y_min, y_max+1]
    // C++ j,k range: [0, x_max-x_min+1], [0, y_max-y_min+1]
    // Array buffer indices: j-x_min+2, k-y_min+2
    target(j, k) = select(roi, 
        yvel1(j - x_min + 2, k - y_min + 2),
        yvel0(j, k));
    return target;
}

extern "C" {
    void reset_field_kernel_loop112_(
        const int *j, const int *k,
        const int* x_max, const int* x_min,
        const int* y_max, const int* y_min,
        double *yvel0,
        double *yvel1);
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

    Func init2_cpu = init_nonzero<2>("init2", x_range + 6 >= 8);
    Func zero2_cpu = set_zero<2>("zero2", x_range + 6 >= 8);
    Func init2_gpu = init2_cpu;
    Func zero2_gpu = zero2_cpu;
    init2_cpu.compile_jit(cpu_target);
    zero2_cpu.compile_jit(cpu_target);
    if (with_gpu)
    {
        init2_gpu.compile_jit(gpu_target);
        zero2_gpu.compile_jit(gpu_target);
    }

    // Array dimensions:
    // yvel0, yvel1: (x_min-2):(x_max+3), (y_min-2):(y_max+3) -> (x_range + 6) × (y_range + 6)
    Buffer<double, 2> yvel1({x_range + 6, y_range + 6}, "yvel1"), yvel1_g(yvel1);
    Buffer<double, 2> yvel0_base({x_range + 6, y_range + 6}, "yvel0");
    Buffer<double, 2> yvel0_cpu({x_range + 2, y_range + 2}, "yvel0");
    Buffer<double, 2> yvel0_gpu({x_range + 2, y_range + 2}, "yvel0");

    // --------------------------- reset_field_kernel_loop112 kernel --------------------------
    
    // building cpu func
    Func cpu_fn = targetFunction("reset_field_kernel_loop112_cpu", 
                                yvel0_cpu, yvel1,
                                Expr(x_min), Expr(x_max), 
                                Expr(y_min), Expr(y_max));
    cpu_fn.parallel(k);
    if (x_range + 2 >= 8) cpu_fn.vectorize(j, 8);
    cpu_fn.compile_jit(cpu_target);

    // IO initialization
    init2_cpu.realize(yvel1);
    init2_cpu.realize(yvel0_base);

    // Calling baseline
    int dummy_j = 0, dummy_k = 0;
    double *yvel0_ = yvel0_base.get()->begin();
    double *yvel1_ = yvel1.get()->begin();
    reset_field_kernel_loop112_(
        &dummy_j, &dummy_k,
        &x_max, &x_min,
        &y_max, &y_min,
        yvel0_,
        yvel1_);
    
    // Calling halide cpu
    try {
        zero2_cpu.realize(yvel0_cpu);
        cpu_fn.realize(yvel0_cpu);
    } catch (RuntimeError &e) {
        std::cerr << e.what();
        return 1;
    }

    double correctness_1 = 0.0;
    int errors = 0;
    for (int k_idx = 0; k_idx < y_range + 2; k_idx++) {
        for (int j_idx = 0; j_idx < x_range + 2; j_idx++) {
            // Fortran loop: j from x_min to x_max+1, k from y_min to y_max+1
            // C++ loop: j_idx from 0 to x_range+1, k_idx from 0 to y_range+1
            // yvel0_base index for Fortran yvel0(j_idx+x_min, k_idx+y_min) is (j_idx+x_min-x_min+2, k_idx+y_min-y_min+2) = (j_idx+2, k_idx+2)
            
            double diff = abs(yvel0_base(j_idx + 2, k_idx + 2) - yvel0_cpu(j_idx, k_idx));
            if (diff >= 1e-10)
            {
                if (errors < 10) {
                    printf("output [%d, %d] = Fortran: %lf | Halide: %lf | diff: %e\n", 
                           j_idx, k_idx, yvel0_base(j_idx + 2, k_idx + 2), yvel0_cpu(j_idx, k_idx), diff);
                }
                errors++;
            }
            correctness_1 += diff;
        }
    }
    
    if (errors > 0) {
        printf("Total errors: %d, sum of differences: %e\n", errors, correctness_1);
    }

    int times = 10;
    double cost_time = 0;
    struct timespec t1, t2, elapsed;
    clock_gettime(CLOCK_REALTIME, &t1);
    for (int i = 0; i < times; i++)
    {
        clock_gettime(CLOCK_REALTIME, &t1);
        reset_field_kernel_loop112_(
            &dummy_j, &dummy_k,
            &x_max, &x_min,
            &y_max, &y_min,
            yvel0_,
            yvel1_);
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("legacy code: %lfms\n", cost_time/times*1000);

    cost_time = 0;
    clock_gettime(CLOCK_REALTIME, &t1);
    for (int i = 0; i < times; i++)
    {
        clock_gettime(CLOCK_REALTIME, &t1);
        cpu_fn.realize(yvel0_cpu);
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("lifted cpu: %lfms\n", cost_time/times*1000);

    if (with_gpu)
    {
        // building gpu func
        Func gpu_fn = targetFunction("reset_field_kernel_loop112_gpu", 
                                    yvel0_gpu, yvel1_g,
                                    Expr(x_min), Expr(x_max), 
                                    Expr(y_min), Expr(y_max));
        gpu_fn.gpu_tile(j, k, bj, bk, tj, tk, 8, 8)
              .compile_jit(gpu_target);
        
        // GPU IO initialization
        yvel1_g.copy_from(yvel1);
        
        // warmup
        try {
            zero2_gpu.realize(yvel0_gpu);
            gpu_fn.realize(yvel0_gpu);
        } catch (RuntimeError &e) {
            std::cerr << e.what();
            return -1;
        }

        cost_time = 0;
        clock_gettime(CLOCK_REALTIME, &t1);
        for (int i = 0; i < times; i++)
        {
            gpu_fn.realize(yvel0_gpu);
        }
        yvel0_gpu.copy_to_host();
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time = toSec(elapsed);
        printf("lifted gpu: %lfms\n\n", cost_time/times*1000);
    }

    return 0;
} 