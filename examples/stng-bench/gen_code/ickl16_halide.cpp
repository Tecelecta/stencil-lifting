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
                           Buffer<double, 2>& yarea,
                           const Buffer<double, 1>& celldx,
                           const Expr x_min, const Expr x_max,
                           const Expr y_min, const Expr y_max)
{
    Var j("j"), k("k");
    Func target(funcName);
    
    target(j, k) = celldx(j);
    return target;
}

extern "C" {
    void initialise_chunk_kernel_loop16_(
        const int *j, const int *k,
        double *celldx, 
        const int* x_max, const int* x_min,
        const int* y_max, const int* y_min,
        double *yarea);
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

    Func init1_cpu = init_nonzero<1>("init1", x_range + 5 >= 8);
    Func init2_cpu = init_nonzero<2>("init2", x_range + 5 >= 8);
    Func zero2_cpu = set_zero<2>("zero2", x_range + 5 >= 8);
    Func init1_gpu = init1_cpu;
    Func init2_gpu = init2_cpu;
    Func zero2_gpu = zero2_cpu;
    init1_cpu.compile_jit(cpu_target);
    init2_cpu.compile_jit(cpu_target);
    zero2_cpu.compile_jit(cpu_target);
    if (with_gpu)
    {
        init1_gpu.compile_jit(gpu_target);
        init2_gpu.compile_jit(gpu_target);
        zero2_gpu.compile_jit(gpu_target);
    }

    // Array dimensions:
    // celldx: (x_min-2):(x_max+2) -> x_range + 5
    // yarea: (x_min-2):(x_max+2),(y_min-2):(y_max+3) -> (x_range + 5) × (y_range + 6)
    Buffer<double, 1> celldx(x_range + 5, "celldx"), celldx_g(celldx);
    Buffer<double, 2> yarea_base({x_range + 5, y_range + 6}, "yarea");
    Buffer<double, 2> yarea_cpu({x_range + 5, y_range + 5}, "yarea");
    Buffer<double, 2> yarea_gpu({x_range + 5, y_range + 5}, "yarea");

    // --------------------------- initialise_chunk_kernel_loop16 kernel --------------------------
    
    // building cpu func
    Func cpu_fn = targetFunction("initialise_chunk_kernel_loop16_cpu", 
                                yarea_cpu, celldx,
                                Expr(x_min), Expr(x_max), 
                                Expr(y_min), Expr(y_max));
    cpu_fn.parallel(k);
    if (x_range + 5 >= 8) cpu_fn.vectorize(j, 8);
    cpu_fn.compile_jit(cpu_target);

    // IO initialization
    init1_cpu.realize(celldx);
    init2_cpu.realize(yarea_base);

    // Calling baseline
    int dummy_j = 0, dummy_k = 0;
    double *celldx_ = celldx.get()->begin();
    double *yarea_ = yarea_base.get()->begin();
    initialise_chunk_kernel_loop16_(
        &dummy_j, &dummy_k,
        celldx_, 
        &x_max, &x_min, &y_max, &y_min,
        yarea_);
    
    // Calling halide cpu
    try {
        zero2_cpu.realize(yarea_cpu);
        cpu_fn.realize(yarea_cpu);
    } catch (RuntimeError &e) {
        std::cerr << e.what();
        return 1;
    }

    double correctness_1 = 0.0;
    int errors = 0;
    for (int k_idx = 0; k_idx < y_range + 5; k_idx++) {
        for (int j_idx = 0; j_idx < x_range + 5; j_idx++) {
            // Fortran loop: k from y_min-2 to y_max+2, j from x_min-2 to x_max+2
            // yarea_cpu(j_idx, k_idx) corresponds to Fortran yarea(j_idx-2, k_idx-2)
            // yarea_base index for Fortran yarea(j_idx-2, k_idx-2) is (j_idx-2+2, k_idx-2+2) = (j_idx, k_idx)
            
            double diff = abs(yarea_base(j_idx, k_idx) - yarea_cpu(j_idx, k_idx));
            if (diff >= 1e-10)
            {
                if (errors < 10) {
                    printf("output [%d, %d] = Fortran: %lf | Halide: %lf | diff: %e\n", 
                           j_idx, k_idx, yarea_base(j_idx, k_idx), yarea_cpu(j_idx, k_idx), diff);
                }
                errors++;
            }
            correctness_1 += diff;
        }
    }
    
    if (errors > 0) {
        printf("Total errors: %d, sum of differences: %e\n", errors, correctness_1);
    } 

    int times = 5;
    double cost_time = 0;
    struct timespec t1, t2, elapsed;
    clock_gettime(CLOCK_REALTIME, &t1);
    for (int i = 0; i < times; i++)
    {
        clock_gettime(CLOCK_REALTIME, &t1);
        init2_cpu.realize(yarea_base);
        initialise_chunk_kernel_loop16_(
            &dummy_j, &dummy_k,
            celldx_, 
            &x_max, &x_min, &y_max, &y_min,
            yarea_);
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
        cpu_fn.realize(yarea_cpu);
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("lifted cpu: %lfms\n", cost_time/times*1000);

    if (with_gpu)
    {
        // building gpu func
        Func gpu_fn = targetFunction("initialise_chunk_kernel_loop16_gpu", 
                                    yarea_gpu, celldx_g,
                                    Expr(x_min), Expr(x_max), 
                                    Expr(y_min), Expr(y_max));
        gpu_fn.gpu_tile(j, k, bj, bk, tj, tk, 8, 8)
              .compile_jit(gpu_target);
        
        // GPU IO initialization
        celldx_g.copy_from(celldx);
        
        // warmup
        try {
            zero2_gpu.realize(yarea_gpu);
            gpu_fn.realize(yarea_gpu);
        } catch (RuntimeError &e) {
            std::cerr << e.what();
            return -1;
        }

        cost_time = 0;
        clock_gettime(CLOCK_REALTIME, &t1);
        for (int i = 0; i < times; i++)
        {
            gpu_fn.realize(yarea_gpu);
        }
        yarea_gpu.copy_to_host();
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time = toSec(elapsed);
        printf("lifted gpu: %lfms\n\n", cost_time/times*1000);
    }

    return 0;
} 