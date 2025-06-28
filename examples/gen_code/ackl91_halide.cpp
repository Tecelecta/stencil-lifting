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

inline Func targetFunction(const std::string& funcName,
                           Buffer<double, 2>& post_vol,
                           Buffer<double, 2>& pre_vol,
                           const Buffer<double, 2>& vol_flux_x,
                           const Buffer<double, 2>& vol_flux_y,
                           const Buffer<double, 2>& volume,
                           const Expr x_max, const Expr x_min,
                           const Expr y_max, const Expr y_min)
{
    Var j("j"), k("k");
    Func target(funcName);
    
    // j, k are the output coordinates in the range [0, x_range+5) x [0, y_range+5)
    // We need to map these to the actual loop coordinates used in Fortran
    // Fortran loop: j from x_min-2 to x_max+2, k from y_min-2 to y_max+2
    // So: actual_j = j + (x_min-2), actual_k = k + (y_min-2)
    // But for buffer access, we need to offset by +2 since buffers start from effective index 0
    
    Expr actual_j = j + (x_min - 2);  // This gives us the original Fortran j coordinate
    Expr actual_k = k + (y_min - 2);  // This gives us the original Fortran k coordinate
    
    // For buffer access, we need to map back to 0-based indexing
    // vol_flux_x buffer: maps Fortran indices [x_min-2:x_max+3, y_min-2:y_max+2] to [0:x_range+6, 0:y_range+5]
    // volume buffer: maps Fortran indices [x_min-2:x_max+2, y_min-2:y_max+2] to [0:x_range+5, 0:y_range+5] 
    
    Expr vol_j = actual_j - (x_min - 2);  // Maps to [0, x_range+5)
    Expr vol_k = actual_k - (y_min - 2);  // Maps to [0, y_range+5)
    
    Expr flux_x_j = actual_j - (x_min - 2);     // Maps to [0, x_range+6)
    Expr flux_x_k = actual_k - (y_min - 2);     // Maps to [0, y_range+5)
    
    Expr flux_y_j = actual_j - (x_min - 2);     // Maps to [0, x_range+5)
    Expr flux_y_k = actual_k - (y_min - 2);     // Maps to [0, y_range+6)
    
    // Check bounds to ensure we're within the valid computation region
    auto roi = actual_j >= (x_min - 2) && actual_j <= (x_max + 2) && 
               actual_k >= (y_min - 2) && actual_k <= (y_max + 2);
    
    // Original Fortran computation:
    // pre_vol(j,k) = volume(j,k) + (vol_flux_x(j+1,k) - vol_flux_x(j,k) + vol_flux_y(j,k+1) - vol_flux_y(j,k))
    // post_vol(j,k) = pre_vol(j,k) - (vol_flux_x(j+1,k) - vol_flux_x(j,k))
    
    Expr pre_vol_calc = volume(vol_j, vol_k) + 
                       (vol_flux_x(flux_x_j + 1, flux_x_k) - vol_flux_x(flux_x_j, flux_x_k) + 
                        vol_flux_y(flux_y_j, flux_y_k + 1) - vol_flux_y(flux_y_j, flux_y_k));
    
    Expr post_vol_calc = pre_vol_calc - (vol_flux_x(flux_x_j + 1, flux_x_k) - vol_flux_x(flux_x_j, flux_x_k));
    
    target(j, k) = select(roi, post_vol_calc, Expr(0.0));
    return target;
}

extern "C" {
    void advec_cell_kernel_loop91_(double *post_vol,
                                    double *pre_vol,
                                    double *vol_flux_x,
                                    double *vol_flux_y,
                                    double *volume,
                                    const int* x_max, const int* x_min,
                                    const int* y_max, const int* y_min);
}

int main(int argc, char** argv)
{
    // printf("Caller Start!\n");
    const int x_max = 2e4;
    const int x_min = 0;
    const int y_max = 2e4;
    const int y_min = 0;

    // const int x_max = 8;
    // const int x_min = 0;
    // const int y_max = 8;
    // const int y_min = 0;

    const int x_range = x_max - x_min;
    const int y_range = y_max - y_min;

    // --------------------------- Preparation --------------------------
    // printf("Prepare the random data\n");
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

    Func init2_cpu = init_nonzero<2>("init", x_range + 6 >= 8);
    Func zero_cpu = set_zero<2>("zero", x_range + 6 >= 8);
    Func init2_gpu = init2_cpu;
    Func zero_gpu = zero_cpu;
    init2_cpu.compile_jit(cpu_target);
    zero_cpu.compile_jit(cpu_target);
    if (with_gpu)
    {
        init2_gpu.compile_jit(gpu_target);
        zero_gpu.compile_jit(gpu_target);
    }

    // Input buffers with appropriate dimensions
    Buffer<double,2> vol_flux_x({x_range+6, y_range+5}, "vol_flux_x"), vol_flux_x_g(vol_flux_x);
    Buffer<double,2> vol_flux_y({x_range+5, y_range+6}, "vol_flux_y"), vol_flux_y_g(vol_flux_y);
    Buffer<double,2> volume({x_range+5, y_range+5}, "volume"), volume_g(volume);

    // Output buffers - Fortran computes 133x133 points into 134x134 arrays
    Buffer<double,2> post_vol_base({x_range+6, y_range+6}, "post_vol");
    Buffer<double,2> pre_vol_base({x_range+6, y_range+6}, "pre_vol");
    // Halide output should match the actual computation domain (133x133)
    Buffer<double,2> post_vol_cpu({x_range+5, y_range+5}, "post_vol_cpu");
    Buffer<double,2> post_vol_gpu({x_range+5, y_range+5}, "post_vol_gpu");

    // --------------------------- advec_cell_kernel_loop91 kernel --------------------------
    // printf("advec_cell_kernel_loop91 kernel start!\n");

    // building baseline stencil
    Func cpu_fn = targetFunction("advec_cell_kernel_loop91_cpu", 
                                post_vol_cpu, pre_vol_base, vol_flux_x, vol_flux_y, volume,
                                Expr(x_max), Expr(x_min), 
                                Expr(y_max), Expr(y_min));
    cpu_fn.parallel(k);
    if (x_range + 6 >= 8) cpu_fn.vectorize(j, 8);
    cpu_fn.compile_jit(cpu_target);

    // IO initialization
    init2_cpu.realize(vol_flux_x);
    init2_cpu.realize(vol_flux_y);
    init2_cpu.realize(volume);
    zero_cpu.realize(post_vol_base);
    zero_cpu.realize(pre_vol_base);

    // Calling baseline Fortran
    double *post_vol_ = post_vol_base.get()->begin();
    double *pre_vol_ = pre_vol_base.get()->begin();
    double *vol_flux_x_ = vol_flux_x.get()->begin();
    double *vol_flux_y_ = vol_flux_y.get()->begin();
    double *volume_ = volume.get()->begin();
    
    advec_cell_kernel_loop91_(post_vol_, 
                               pre_vol_,
                               vol_flux_x_, 
                               vol_flux_y_, 
                               volume_,
                               &x_max, &x_min, &y_max, &y_min);
    
    // Calling halide cpu
    try {
        zero_cpu.realize(post_vol_cpu);
        cpu_fn.realize(post_vol_cpu);
    } catch (RuntimeError &e) {
        std::cerr << e.what();
        return 1;
    }

    // check result - map indices correctly
    // Fortran writes to post_vol_base with j_adj,k_adj in [0,132]
    // Halide writes to post_vol_cpu with j,k in [0,132] 
    double correctness_1 = 0.0;
    for (int k_adj = 0; k_adj < y_range+5; k_adj++) {
        for (int j_adj = 0; j_adj < x_range+5; j_adj++) {
            // Fortran stores at post_vol_base using 1D index: j_adj + k_adj * (x_range+6)
            // But post_vol_base is accessed as 2D: post_vol_base(j_adj, k_adj)
            double diff = abs(post_vol_base(j_adj, k_adj) - post_vol_cpu(j_adj, k_adj));
            if ( diff >= 1e-3 )
            {
                printf("output [%d, %d] = Fortran: %lf | Halide: %lf\n", 
                        j_adj, k_adj, post_vol_base(j_adj, k_adj), post_vol_cpu(j_adj, k_adj));
            }
            correctness_1 += diff;
        }
    }

    // printf("The correctness difference is %lf\n", correctness_1);

    int times = 5;
    double cost_time = 0;
    struct timespec t1, t2, elapsed;
    
    // Benchmark Fortran version
    for (int i = 0; i < times; i++)
    {
        clock_gettime(CLOCK_REALTIME, &t1);
        advec_cell_kernel_loop91_(post_vol_, 
                                   pre_vol_,
                                   vol_flux_x_, 
                                   vol_flux_y_, 
                                   volume_,
                                   &x_max, &x_min, &y_max, &y_min);
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("legacy code: %lfms\n", cost_time/times*1000);

    // Benchmark Halide CPU version
    cost_time = 0;
    for (int i = 0; i < times; i++)
    {
        clock_gettime(CLOCK_REALTIME, &t1);
        cpu_fn.realize(post_vol_cpu);
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("lifted cpu: %lfms\n", cost_time/times*1000);

    if (with_gpu)
    {
        // building gpu func
        Func gpu_fn = targetFunction("advec_cell_kernel_loop91_gpu", 
                                    post_vol_gpu, pre_vol_base, vol_flux_x_g, vol_flux_y_g, volume_g,
                                    Expr(x_max), Expr(x_min), 
                                    Expr(y_max), Expr(y_min));
        gpu_fn.gpu_tile(j, k, bj, bk, tj, tk, 8, 8)
              .compile_jit(gpu_target);
        
        // GPU IO initialization
        init2_gpu.realize(vol_flux_x_g);
        init2_gpu.realize(vol_flux_y_g);
        init2_gpu.realize(volume_g);
        
        // warmup
        try {
            zero_gpu.realize(post_vol_gpu);
            gpu_fn.realize(post_vol_gpu);
        } catch ( RuntimeError &e ) {
            std::cerr << e.what();
            return -1;
        }

        // Benchmark GPU version
        cost_time = 0;
        clock_gettime(CLOCK_REALTIME, &t1);
        for (int i = 0; i < times; i++)
        {
            gpu_fn.realize(post_vol_gpu);
        }
        post_vol_gpu.copy_to_host();
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time = toSec(elapsed);
        printf("lifted gpu: %lfms\n\n", cost_time/times*1000);
    }

    return 0;
}