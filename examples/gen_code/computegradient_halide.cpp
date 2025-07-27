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

// Compute gradient with limiter: compute gradients in X and Y directions with slope limiting
inline std::tuple<Func, Func> targetFunctions(const std::string& baseName,
                                               Buffer<double, 3>& dux,
                                               Buffer<double, 3>& duy,
                                               const Buffer<double, 2>& xo,
                                               const Buffer<double, 2>& yo,
                                               const Buffer<double, 3>& uo,
                                               const Expr ifirst0, const Expr ilast0,
                                               const Expr ifirst1, const Expr ilast1,
                                               const Expr gcw0, const Expr gcw1)
{
    Var j("j"), k("k"), l("l");
    Func grad_x(baseName + "_x");
    Func grad_y(baseName + "_y");
    
    // Define the region of interest based on Fortran loop bounds
    // Add safety bounds to prevent out-of-bounds access
    auto roi = ifirst0 <= j && j <= ilast0 && 
               ifirst1 <= k && k <= ilast1 && 
               0 <= l && l <= 3 &&  // Halide uses 0-based indexing for l
               j >= 1 && j < (int)uo.dim(0).extent() - 1 &&  // Ensure j-1 and j+1 are valid
               k >= 1 && k < (int)uo.dim(1).extent() - 1;    // Ensure k-1 and k+1 are valid
    
    // X-direction gradient with limiter - use clamp for safe access
    // DUL = (UO(J,K,L) - UO(J-1,K,L)) / (XO(J,K) - XO(J-1,K))
    // DUR = (UO(J+1,K,L) - UO(J,K,L)) / (XO(J+1,K) - XO(J,K))
    Expr j_clamp = clamp(j, 1, (int)uo.dim(0).extent() - 2);
    Expr k_clamp = clamp(k, 1, (int)uo.dim(1).extent() - 2);
    Expr dul_x = (uo(j_clamp, k_clamp, l) - uo(j_clamp-1, k_clamp, l)) / 
                 (xo(j_clamp, k_clamp) - xo(j_clamp-1, k_clamp));
    Expr dur_x = (uo(j_clamp+1, k_clamp, l) - uo(j_clamp, k_clamp, l)) / 
                 (xo(j_clamp+1, k_clamp) - xo(j_clamp, k_clamp));
    
    // Gradient limiter formula
    // DUX = 0.5*(SIGN(1.0,DUL) + SIGN(1.0,DUR)) * 2.0*ABS(DUL*DUR) / (ABS(DUL) + ABS(DUR) + 1.0E-10)
    Expr sign_dul_x = select(dul_x > 0, Expr(1.0), select(dul_x < 0, Expr(-1.0), Expr(0.0)));
    Expr sign_dur_x = select(dur_x > 0, Expr(1.0), select(dur_x < 0, Expr(-1.0), Expr(0.0)));
    Expr gradient_x = Expr(0.5) * (sign_dul_x + sign_dur_x) * 
                      Expr(2.0) * abs(dul_x * dur_x) / (abs(dul_x) + abs(dur_x) + Expr(1.0e-10));
    
    grad_x(j, k, l) = select(roi, gradient_x, Expr(0.0));
    
    // Y-direction gradient with limiter - use clamp for safe access
    // DUL = (UO(J,K,L) - UO(J,K-1,L)) / (YO(J,K) - YO(J,K-1))
    // DUR = (UO(J,K+1,L) - UO(J,K,L)) / (YO(J,K+1) - YO(J,K))
    Expr dul_y = (uo(j_clamp, k_clamp, l) - uo(j_clamp, k_clamp-1, l)) / 
                 (yo(j_clamp, k_clamp) - yo(j_clamp, k_clamp-1));
    Expr dur_y = (uo(j_clamp, k_clamp+1, l) - uo(j_clamp, k_clamp, l)) / 
                 (yo(j_clamp, k_clamp+1) - yo(j_clamp, k_clamp));
    
    // Gradient limiter formula
    Expr sign_dul_y = select(dul_y > 0, Expr(1.0), select(dul_y < 0, Expr(-1.0), Expr(0.0)));
    Expr sign_dur_y = select(dur_y > 0, Expr(1.0), select(dur_y < 0, Expr(-1.0), Expr(0.0)));
    Expr gradient_y = Expr(0.5) * (sign_dul_y + sign_dur_y) * 
                      Expr(2.0) * abs(dul_y * dur_y) / (abs(dul_y) + abs(dur_y) + Expr(1.0e-10));
    
    grad_y(j, k, l) = select(roi, gradient_y, Expr(0.0));
    
    return std::make_tuple(grad_x, grad_y);
}

extern "C" {
    void computegradient_(int *ifirst0, int *ilast0, int *ifirst1, int *ilast1,
                         int *gcw0, int *gcw1,
                         double *xo, double *yo, double *uo, 
                         double *dux, double *duy);
}

int main(int argc, char** argv)
{
    // printf("ComputeGradient Caller Start!\n");
    
    // Array dimensions based on typical CFD grid
    const int NI = 5000;  // Grid size in i-direction
    const int NJ = 5000;  // Grid size in j-direction
    const int GCW0 = 2; // Ghost cell width in i-direction
    const int GCW1 = 2; // Ghost cell width in j-direction
    const int NCOMP = 4; // Number of components
    
    // Loop bounds (Fortran 1-based, but we'll use 0-based for Halide)
    const int ifirst0 = GCW0;
    const int ilast0 = NI + GCW0 - 1;
    const int ifirst1 = GCW1;
    const int ilast1 = NJ + GCW1 - 1;
    
    // Array dimensions
    const int XI_SIZE = NI + 2*GCW0 + 1;  // +1 for node-centered coordinates
    const int YJ_SIZE = NJ + 2*GCW1 + 1;
    const int UI_SIZE = NI + 2*GCW0;
    const int UJ_SIZE = NJ + 2*GCW1;
    
    // --------------------------- Preparation --------------------------
    // printf("Prepare the random data\n");
    bool with_gpu = true;
    
    Var j("j"), k("k"), l("l");
    Var bj, bk, bl, tj, tk, tl;

    Target gpu_target = find_gpu_target();
    if (!gpu_target.has_gpu_feature())
    {
        printf("Don't have a gpu!\n");
        with_gpu = false;
    }
    Target cpu_target = get_host_target();

    Func init2_cpu = init_nonzero<2>("init2", XI_SIZE >= 8);
    Func init3_cpu = init_nonzero<3>("init3", UI_SIZE >= 8);
    Func zero3_cpu = set_zero<3>("zero3", UI_SIZE >= 8);
    Func init2_gpu = init2_cpu;
    Func init3_gpu = init3_cpu;
    Func zero3_gpu = zero3_cpu;
    
    init2_cpu.compile_jit(cpu_target);
    init3_cpu.compile_jit(cpu_target);
    zero3_cpu.compile_jit(cpu_target);
    if (with_gpu)
    {
        init2_gpu.compile_jit(gpu_target);
        init3_gpu.compile_jit(gpu_target);
        zero3_gpu.compile_jit(gpu_target);
    }

    // Create buffers with same dimensions as Fortran arrays
    Buffer<double,2> xo({XI_SIZE, YJ_SIZE}, "xo"), xo_g(xo);
    Buffer<double,2> yo({XI_SIZE, YJ_SIZE}, "yo"), yo_g(yo);
    Buffer<double,3> uo({UI_SIZE, UJ_SIZE, NCOMP}, "uo"), uo_g(uo);
    
    // baseline outputs (Fortran)
    Buffer<double,3> dux_base({UI_SIZE, UJ_SIZE, NCOMP}, "dux_base");
    Buffer<double,3> duy_base({UI_SIZE, UJ_SIZE, NCOMP}, "duy_base");
    
    // halide cpu outputs
    Buffer<double,3> dux_cpu({UI_SIZE, UJ_SIZE, NCOMP}, "dux_cpu");
    Buffer<double,3> duy_cpu({UI_SIZE, UJ_SIZE, NCOMP}, "duy_cpu");
    
    // halide gpu outputs
    Buffer<double,3> dux_gpu({UI_SIZE, UJ_SIZE, NCOMP}, "dux_gpu");
    Buffer<double,3> duy_gpu({UI_SIZE, UJ_SIZE, NCOMP}, "duy_gpu");

    // --------------------------- computegradient kernel --------------------------
    // printf("computegradient kernel start!\n");

    // building halide cpu stencil
    auto [grad_x_cpu, grad_y_cpu] = targetFunctions("computegradient_cpu", 
                                                    dux_cpu, duy_cpu, xo, yo, uo,
                                                    Expr(ifirst0), Expr(ilast0),
                                                    Expr(ifirst1), Expr(ilast1),
                                                    Expr(GCW0), Expr(GCW1));
    
    // Simple but effective scheduling strategy for both gradients
    grad_x_cpu.parallel(k);
    grad_y_cpu.parallel(k);
    if (UI_SIZE >= 8) {
        grad_x_cpu.vectorize(j, 8);
        grad_y_cpu.vectorize(j, 8);
    }
    
    grad_x_cpu.compile_jit(cpu_target);
    grad_y_cpu.compile_jit(cpu_target);

    // IO initialization
    init2_cpu.realize(xo);
    init2_cpu.realize(yo);
    init3_cpu.realize(uo);
    zero3_cpu.realize(dux_base);
    zero3_cpu.realize(duy_base);

    // Convert loop bounds to Fortran format (add 1 for 1-based indexing)
    int f_ifirst0 = ifirst0 + 1;
    int f_ilast0 = ilast0 + 1;
    int f_ifirst1 = ifirst1 + 1;
    int f_ilast1 = ilast1 + 1;
    int f_gcw0 = GCW0;
    int f_gcw1 = GCW1;

    // Calling baseline Fortran function
    double *xo_ = xo.get()->begin();
    double *yo_ = yo.get()->begin();
    double *uo_ = uo.get()->begin();
    double *dux_base_ = dux_base.get()->begin();
    double *duy_base_ = duy_base.get()->begin();
    
    computegradient_(&f_ifirst0, &f_ilast0, &f_ifirst1, &f_ilast1,
                     &f_gcw0, &f_gcw1,
                     xo_, yo_, uo_, dux_base_, duy_base_);
    
    // Calling halide cpu
    try {
        zero3_cpu.realize(dux_cpu);
        zero3_cpu.realize(duy_cpu);
        grad_x_cpu.realize(dux_cpu);
        grad_y_cpu.realize(duy_cpu);
    } catch (RuntimeError &e) {
        std::cerr << e.what();
        return 1;
    }

    // check result for both outputs
    double correctness_x = 0.0, correctness_y = 0.0;
    int error_count = 0;
    
    for (int l = 0; l < NCOMP; l++) {
        for (int k = 0; k < UJ_SIZE; k++) {
            for (int j = 0; j < UI_SIZE; j++) {
                double diff_x = abs(dux_base(j,k,l) - dux_cpu(j,k,l));
                double diff_y = abs(duy_base(j,k,l) - duy_cpu(j,k,l));
                
                // if (diff_x >= 1e-12 || diff_y >= 1e-12)
                // {
                //     if (error_count < 10) { // Only print first 10 errors
                //         printf("output [%d, %d, %d] = Fortran: (dux=%lf, duy=%lf) | Halide: (dux=%lf, duy=%lf)\n", 
                //                 j, k, l, 
                //                 dux_base(j,k,l), duy_base(j,k,l),
                //                 dux_cpu(j,k,l), duy_cpu(j,k,l));
                //     }
                //     error_count++;
                // }
                correctness_x += diff_x;
                correctness_y += diff_y;
            }
        }
    }

    // printf("Total correctness error: dux=%e, duy=%e, Error count: %d\n", 
    //        correctness_x, correctness_y, error_count);

    int times = 5;
    double cost_time = 0;
    struct timespec t1, t2, elapsed;
    
    // Benchmark Fortran
    for (int i = 0; i < times; i++)
    {
        clock_gettime(CLOCK_REALTIME, &t1);
        computegradient_(&f_ifirst0, &f_ilast0, &f_ifirst1, &f_ilast1,
                         &f_gcw0, &f_gcw1,
                         xo_, yo_, uo_, dux_base_, duy_base_);
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
        grad_x_cpu.realize(dux_cpu);
        grad_y_cpu.realize(duy_cpu);
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("lifted cpu: %lfms\n", cost_time/times*1000);

    if (with_gpu)
    {
        // building gpu funcs
        auto [grad_x_gpu, grad_y_gpu] = targetFunctions("computegradient_gpu", 
                                                        dux_gpu, duy_gpu, xo_g, yo_g, uo_g,
                                                        Expr(ifirst0), Expr(ilast0),
                                                        Expr(ifirst1), Expr(ilast1),
                                                        Expr(GCW0), Expr(GCW1));
        
        grad_x_gpu.gpu_tile(j, k, l, bj, bk, bl, tj, tk, tl, 8, 8, 4).compile_jit(gpu_target);
        grad_y_gpu.gpu_tile(j, k, l, bj, bk, bl, tj, tk, tl, 8, 8, 4).compile_jit(gpu_target);
        
        // GPU IO initialization
        xo_g.copy_from(xo);
        yo_g.copy_from(yo);
        uo_g.copy_from(uo);
        
        // warmup
        try {
            zero3_gpu.realize(dux_gpu);
            zero3_gpu.realize(duy_gpu);
            grad_x_gpu.realize(dux_gpu);
            grad_y_gpu.realize(duy_gpu);
        } catch ( RuntimeError &e ) {
            std::cerr << e.what();
            return -1;
        }

        cost_time = 0;
        clock_gettime(CLOCK_REALTIME, &t1);
        for (int i = 0; i < times; i++)
        {
            grad_x_gpu.realize(dux_gpu);
            grad_y_gpu.realize(duy_gpu);
        }
        dux_gpu.copy_to_host();
        duy_gpu.copy_to_host();
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time = toSec(elapsed);
        printf("lifted gpu: %lfms\n\n", cost_time/times*1000);
    }

    return 0;
} 