#include <cstdio>
#include <ctime>
// include the generated header 
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
    // Start with a target suitable for the machine you're running this on.
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

// Divergence stencil: combine 3 input arrays with different directional stencils
inline Func targetFunction(const std::string& funcName,
                          Buffer<double, 3>& aout,
                          const Buffer<double, 3>& ain1,
                          const Buffer<double, 3>& ain2,
                          const Buffer<double, 3>& ain3)
{
    Var i("i"), j("j"), k("k");
    Func target(funcName);
    
    // Divergence stencil: mixed directional stencil computation
    // Fortran: do k=2,257; do j=2,257; do i=2,257 (1-based indexing)
    // Halide: k=1,256; j=1,256; i=1,256 (0-based indexing, subtract 1 from Fortran indices)
    target(i, j, k) = select(
        1 <= i && i <= 256 && 
        1 <= j && j <= 256 && 
        1 <= k && k <= 256,
        ain1(i, j, k) + 
        (Expr(1.0/6.0)) * (ain1(clamp(i+1, 0, 257), j, k) + ain1(clamp(i-1, 0, 257), j, k)) +
        (Expr(2.0/6.0)) * (ain2(i, clamp(j+1, 0, 257), k) + ain2(i, clamp(j-1, 0, 257), k)) +
        (Expr(3.0/6.0)) * (ain3(i, j, clamp(k+1, 0, 257)) + ain3(i, j, clamp(k-1, 0, 257))),
        Expr(0.0));
    
    return target;
}

extern "C" {
    void stencil_(double *ain1, 
                  double *ain2, 
                  double *ain3,
                  double *aout);
}

int main(int argc, char** argv)
{
    // printf("Divergence Stencil Caller Start!\n");
    
    // Array dimensions: 258x258x258 (same as Fortran)
    const int SIZE = 258;
    
    // --------------------------- Preparation --------------------------
    // printf("Prepare the random data\n");
    bool with_gpu = true;
    
    Var i("i"), j("j"), k("k");
    Var bi, bj, bk, ti, tj, tk;

    Target gpu_target = find_gpu_target();
    if (!gpu_target.has_gpu_feature())
    {
        printf("Don't have a gpu!\n");
        with_gpu = false;
    }
    Target cpu_target = get_host_target();

    Func init3_cpu = init_nonzero<3>("init", SIZE >= 8);
    Func zero_cpu = set_zero<3>("zero", SIZE >= 8);
    Func init3_gpu = init3_cpu;
    Func zero_gpu = zero_cpu;
    
    init3_cpu.compile_jit(cpu_target);
    zero_cpu.compile_jit(cpu_target);
    if (with_gpu)
    {
        init3_gpu.compile_jit(gpu_target);
        zero_gpu.compile_jit(gpu_target);
    }

    // Create buffers with same dimensions as Fortran arrays
    Buffer<double,3> ain1({SIZE, SIZE, SIZE}, "ain1"), ain1_g(ain1);
    Buffer<double,3> ain2({SIZE, SIZE, SIZE}, "ain2"), ain2_g(ain2);
    Buffer<double,3> ain3({SIZE, SIZE, SIZE}, "ain3"), ain3_g(ain3);
    
    // baseline output (Fortran)
    Buffer<double,3> out_base({SIZE, SIZE, SIZE}, "aout_base");
    // halide cpu output
    Buffer<double,3> out_cpu({SIZE, SIZE, SIZE}, "aout_cpu");
    // halide gpu output
    Buffer<double,3> out_gpu({SIZE, SIZE, SIZE}, "aout_gpu");

    // --------------------------- divergence stencil kernel --------------------------
    // printf("divergence stencil kernel start!\n");

    // building halide cpu stencil
    Func cpu_fn = targetFunction("stencil_cpu", 
                                out_cpu, ain1, ain2, ain3);
    
    // Simple but effective scheduling strategy
    cpu_fn.parallel(k);  // Parallelize outer loop
    if (SIZE >= 8) cpu_fn.vectorize(i, 8);  // Vectorize inner loop
    
    cpu_fn.compile_jit(cpu_target);

    // IO initialization
    init3_cpu.realize(ain1);
    init3_cpu.realize(ain2);
    init3_cpu.realize(ain3);
    zero_cpu.realize(out_base);

    // Calling baseline Fortran function
    double *ain1_ = ain1.get()->begin();
    double *ain2_ = ain2.get()->begin();
    double *ain3_ = ain3.get()->begin();
    double *aout_base_ = out_base.get()->begin();
    stencil_(ain1_, ain2_, ain3_, aout_base_);
    
    // Calling halide cpu
    try {
        zero_cpu.realize(out_cpu);
        cpu_fn.realize(out_cpu);
    } catch (RuntimeError &e) {
        std::cerr << e.what();
        return 1;
    }

    // check result
    double correctness_1 = 0.0;
    int error_count = 0;
    for (int c = 0; c < SIZE; c++) {
        for (int b = 0; b < SIZE; b++) {
            for (int a = 0; a < SIZE; a++) {
                double diff = abs(out_base(a,b,c) - out_cpu(a,b,c));
                // if ( diff >= 1e-12 )
                // {
                //     if (error_count < 10) { // Only print first 10 errors
                //         printf("output [%d, %d, %d] = Fortran: %lf | Halide: %lf | Diff: %e\n", 
                //                 a, b, c, out_base(a,b,c), out_cpu(a,b,c), diff);
                //     }
                //     error_count++;
                // }
                correctness_1 += diff;
            }
        }
    }

    // printf("Total correctness error: %e, Error count: %d\n", correctness_1, error_count);

    int times = 5;
    double cost_time = 0;
    struct timespec t1, t2, elapsed;
    
    // Benchmark Fortran
    for (int i = 0; i < times; i++)
    {
        clock_gettime(CLOCK_REALTIME, &t1);
        stencil_(ain1_, ain2_, ain3_, aout_base_);
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("legacy code: %lfms\n", cost_time/times*1000);

    // Benchmark Halide CPU
    cost_time = 0;
    for (int i = 0; i < times; i++)
    {
        clock_gettime(CLOCK_REALTIME, &t1);
        cpu_fn.realize(out_cpu);
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("lifted cpu: %lfms\n", cost_time/times*1000);

    if (with_gpu)
    {
        // building gpu func
        Func gpu_fn = targetFunction("stencil_gpu", 
                                    out_gpu, ain1_g, ain2_g, ain3_g);
        gpu_fn.gpu_tile(i, j, k, bi, bj, bk, ti, tj, tk, 8, 8, 8)
              .compile_jit(gpu_target);
        
        // GPU IO initialization
        init3_gpu.realize(ain1_g);
        init3_gpu.realize(ain2_g);
        init3_gpu.realize(ain3_g);
        
        // warmup
        try {
            zero_gpu.realize(out_gpu);
            gpu_fn.realize(out_gpu);
        } catch ( RuntimeError &e ) {
            std::cerr << e.what();
            return -1;
        }

        cost_time = 0;
        clock_gettime(CLOCK_REALTIME, &t1);
        for (int i = 0; i < times; i++)
        {
            gpu_fn.realize(out_gpu);
        }
        out_gpu.copy_to_host();
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time = toSec(elapsed);
        printf("lifted gpu: %lfms\n\n", cost_time/times*1000);
    }

    return 0;
} 