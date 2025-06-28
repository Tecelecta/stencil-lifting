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

// Gradient stencil: compute gradients in X, Y, Z directions
inline std::tuple<Func, Func, Func> targetFunctions(const std::string& baseName,
                                                     Buffer<double, 3>& aout1,
                                                     Buffer<double, 3>& aout2,
                                                     Buffer<double, 3>& aout3,
                                                     const Buffer<double, 3>& ain)
{
    Var i("i"), j("j"), k("k");
    Func grad_x(baseName + "_x");
    Func grad_y(baseName + "_y");
    Func grad_z(baseName + "_z");
    
    // Gradient X direction: aout1
    grad_x(i, j, k) = select(
        1 <= i && i <= 256 && 
        1 <= j && j <= 256 && 
        1 <= k && k <= 256,
        ain(i, j, k) + 
        (Expr(1.0/6.0)) * (ain(clamp(i+1, 0, 257), j, k) + ain(clamp(i-1, 0, 257), j, k)),
        Expr(0.0));
    
    // Gradient Y direction: aout2
    grad_y(i, j, k) = select(
        1 <= i && i <= 256 && 
        1 <= j && j <= 256 && 
        1 <= k && k <= 256,
        ain(i, j, k) + 
        (Expr(2.0/6.0)) * (ain(i, clamp(j+1, 0, 257), k) + ain(i, clamp(j-1, 0, 257), k)),
        Expr(0.0));
    
    // Gradient Z direction: aout3
    grad_z(i, j, k) = select(
        1 <= i && i <= 256 && 
        1 <= j && j <= 256 && 
        1 <= k && k <= 256,
        ain(i, j, k) + 
        (Expr(3.0/6.0)) * (ain(i, j, clamp(k+1, 0, 257)) + ain(i, j, clamp(k-1, 0, 257))),
        Expr(0.0));
    
    return std::make_tuple(grad_x, grad_y, grad_z);
}

extern "C" {
    void stencil_(double *ain, 
                  double *aout1,
                  double *aout2, 
                  double *aout3);
}

int main(int argc, char** argv)
{
    // printf("Gradient Stencil Caller Start!\n");
    
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
    Buffer<double,3> ain({SIZE, SIZE, SIZE}, "ain"), ain_g(ain);
    
    // baseline outputs (Fortran)
    Buffer<double,3> out1_base({SIZE, SIZE, SIZE}, "aout1_base");
    Buffer<double,3> out2_base({SIZE, SIZE, SIZE}, "aout2_base");
    Buffer<double,3> out3_base({SIZE, SIZE, SIZE}, "aout3_base");
    
    // halide cpu outputs
    Buffer<double,3> out1_cpu({SIZE, SIZE, SIZE}, "aout1_cpu");
    Buffer<double,3> out2_cpu({SIZE, SIZE, SIZE}, "aout2_cpu");
    Buffer<double,3> out3_cpu({SIZE, SIZE, SIZE}, "aout3_cpu");
    
    // halide gpu outputs
    Buffer<double,3> out1_gpu({SIZE, SIZE, SIZE}, "aout1_gpu");
    Buffer<double,3> out2_gpu({SIZE, SIZE, SIZE}, "aout2_gpu");
    Buffer<double,3> out3_gpu({SIZE, SIZE, SIZE}, "aout3_gpu");

    // --------------------------- gradient stencil kernel --------------------------
    // printf("gradient stencil kernel start!\n");

    // building halide cpu stencil
    auto [grad_x_cpu, grad_y_cpu, grad_z_cpu] = targetFunctions("stencil_cpu", 
                                                                out1_cpu, out2_cpu, out3_cpu, ain);
    
    // Simple but effective scheduling strategy for all gradients
    grad_x_cpu.parallel(k);
    grad_y_cpu.parallel(k);
    grad_z_cpu.parallel(k);
    if (SIZE >= 8) {
        grad_x_cpu.vectorize(i, 8);
        grad_y_cpu.vectorize(i, 8);
        grad_z_cpu.vectorize(i, 8);
    }
    
    grad_x_cpu.compile_jit(cpu_target);
    grad_y_cpu.compile_jit(cpu_target);
    grad_z_cpu.compile_jit(cpu_target);

    // IO initialization
    init3_cpu.realize(ain);
    zero_cpu.realize(out1_base);
    zero_cpu.realize(out2_base);
    zero_cpu.realize(out3_base);

    // Calling baseline Fortran function
    double *ain_ = ain.get()->begin();
    double *aout1_base_ = out1_base.get()->begin();
    double *aout2_base_ = out2_base.get()->begin();
    double *aout3_base_ = out3_base.get()->begin();
    stencil_(ain_, aout1_base_, aout2_base_, aout3_base_);
    
    // Calling halide cpu
    try {
        zero_cpu.realize(out1_cpu);
        zero_cpu.realize(out2_cpu);
        zero_cpu.realize(out3_cpu);
        grad_x_cpu.realize(out1_cpu);
        grad_y_cpu.realize(out2_cpu);
        grad_z_cpu.realize(out3_cpu);
    } catch (RuntimeError &e) {
        std::cerr << e.what();
        return 1;
    }

    // check result for all 3 outputs
    double correctness_1 = 0.0, correctness_2 = 0.0, correctness_3 = 0.0;
    int error_count = 0;
    
    for (int c = 0; c < SIZE; c++) {
        for (int b = 0; b < SIZE; b++) {
            for (int a = 0; a < SIZE; a++) {
                double diff1 = abs(out1_base(a,b,c) - out1_cpu(a,b,c));
                double diff2 = abs(out2_base(a,b,c) - out2_cpu(a,b,c));
                double diff3 = abs(out3_base(a,b,c) - out3_cpu(a,b,c));
                
                // if (diff1 >= 1e-12 || diff2 >= 1e-12 || diff3 >= 1e-12)
                // {
                //     if (error_count < 10) { // Only print first 10 errors
                //         printf("output [%d, %d, %d] = Fortran: (%lf, %lf, %lf) | Halide: (%lf, %lf, %lf)\n", 
                //                 a, b, c, 
                //                 out1_base(a,b,c), out2_base(a,b,c), out3_base(a,b,c),
                //                 out1_cpu(a,b,c), out2_cpu(a,b,c), out3_cpu(a,b,c));
                //     }
                //     error_count++;
                // }
                correctness_1 += diff1;
                correctness_2 += diff2;
                correctness_3 += diff3;
            }
        }
    }

    // printf("Total correctness error: X=%e, Y=%e, Z=%e, Error count: %d\n", 
    //        correctness_1, correctness_2, correctness_3, error_count);

    int times = 5;
    double cost_time = 0;
    struct timespec t1, t2, elapsed;
    
    // Benchmark Fortran
    for (int i = 0; i < times; i++)
    {
        clock_gettime(CLOCK_REALTIME, &t1);
        stencil_(ain_, aout1_base_, aout2_base_, aout3_base_);
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
        grad_x_cpu.realize(out1_cpu);
        grad_y_cpu.realize(out2_cpu);
        grad_z_cpu.realize(out3_cpu);
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("lifted cpu: %lfms\n", cost_time/times*1000);

    if (with_gpu)
    {
        // building gpu funcs
        auto [grad_x_gpu, grad_y_gpu, grad_z_gpu] = targetFunctions("stencil_gpu", 
                                                                    out1_gpu, out2_gpu, out3_gpu, ain_g);
        
        grad_x_gpu.gpu_tile(i, j, k, bi, bj, bk, ti, tj, tk, 8, 8, 8).compile_jit(gpu_target);
        grad_y_gpu.gpu_tile(i, j, k, bi, bj, bk, ti, tj, tk, 8, 8, 8).compile_jit(gpu_target);
        grad_z_gpu.gpu_tile(i, j, k, bi, bj, bk, ti, tj, tk, 8, 8, 8).compile_jit(gpu_target);
        
        // GPU IO initialization
        ain_g.copy_from(ain);
        
        // warmup
        try {
            zero_gpu.realize(out1_gpu);
            zero_gpu.realize(out2_gpu);
            zero_gpu.realize(out3_gpu);
            grad_x_gpu.realize(out1_gpu);
            grad_y_gpu.realize(out2_gpu);
            grad_z_gpu.realize(out3_gpu);
        } catch ( RuntimeError &e ) {
            std::cerr << e.what();
            return -1;
        }

        cost_time = 0;
        clock_gettime(CLOCK_REALTIME, &t1);
        for (int i = 0; i < times; i++)
        {
            grad_x_gpu.realize(out1_gpu);
            grad_y_gpu.realize(out2_gpu);
            grad_z_gpu.realize(out3_gpu);
        }
        out1_gpu.copy_to_host();
        out2_gpu.copy_to_host();
        out3_gpu.copy_to_host();
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time = toSec(elapsed);
        printf("lifted gpu: %lfms\n\n", cost_time/times*1000);
    }

    return 0;
} 