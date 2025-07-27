#include "Halide.h"
#include <cstdio>
#include <ctime>
#include <algorithm>

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
    nz(d) = (((d ) + Expr(1e-5)) / Expr(100.0)) * cast<double>(10);
    return nz;
}

template<>
inline Func init_nonzero<2>(const std::string& funcName, bool vectorize)
{
    Var d1, d2;
    Func nz(funcName);
    nz(d1, d2) = ((((d1 + d2*10)) + Expr(1e-5)) / Expr(100.0)) * cast<double>(10);
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

void calc_dt_kernel_halide(Func& dt_min_val_func,
                          Buffer<double, 1> celldx,
                          Buffer<double, 1> celldy,
                          Buffer<double, 2> xarea,
                          Buffer<double, 2> yarea,
                          Buffer<double, 2> volume,
                          Buffer<double, 2> density0,
                          Buffer<double, 2> viscosity_a,
                          Buffer<double, 2> soundspeed,
                          Buffer<double, 2> xvel0,
                          Buffer<double, 2> yvel0,
                          int x_min, int x_max, int y_min, int y_max) {
    
    Var j("j"), k("k");
    
    // Constants from Fortran code
    Expr g_small = Expr(1.0);
    Expr g_big = Expr(1.0);
    Expr dtc_safe = Expr(0.5);
    Expr dtu_safe = Expr(0.5);
    Expr dtv_safe = Expr(0.5);
    Expr dtdiv_safe = Expr(0.5);
    
    // Offset for buffer access (x_min-2, y_min-2)
    Expr j_offset = Expr(2);  // x_min-2 offset
    Expr k_offset = Expr(2);  // y_min-2 offset
    
    auto j_buf = j + j_offset;
    auto k_buf = k + k_offset;
    
    // Calculate dsx and dsy
    auto dsx = celldx(j_buf);
    auto dsy = celldy(k_buf);
    
    // Calculate cc (sound speed + viscosity term)
    auto cc_base = soundspeed(j_buf, k_buf) * soundspeed(j_buf, k_buf);
    cc_base = cc_base + Expr(2.0) * viscosity_a(j_buf, k_buf) / density0(j_buf, k_buf);
    auto cc = max(sqrt(cc_base), g_small);
    
    // Calculate dtct
    auto dtct = dtc_safe * min(dsx, dsy) / cc;
    
    // Calculate divergence and related terms
    auto dv1_x = (xvel0(j_buf, k_buf) + xvel0(j_buf, k_buf+1)) * xarea(j_buf, k_buf);
    auto dv2_x = (xvel0(j_buf+1, k_buf) + xvel0(j_buf+1, k_buf+1)) * xarea(j_buf+1, k_buf);
    
    auto div_x = dv2_x - dv1_x;
    
    auto dtut = dtu_safe * Expr(2.0) * volume(j_buf, k_buf) / 
                max(max(abs(dv1_x), abs(dv2_x)), g_small * volume(j_buf, k_buf));
    
    auto dv1_y = (yvel0(j_buf, k_buf) + yvel0(j_buf+1, k_buf)) * yarea(j_buf, k_buf);
    auto dv2_y = (yvel0(j_buf, k_buf+1) + yvel0(j_buf+1, k_buf+1)) * yarea(j_buf, k_buf+1);
    
    auto div_y = dv2_y - dv1_y;
    
    auto dtvt = dtv_safe * Expr(2.0) * volume(j_buf, k_buf) / 
                max(max(abs(dv1_y), abs(dv2_y)), g_small * volume(j_buf, k_buf));
    
    // Total divergence
    auto div = (div_x + div_y) / (Expr(2.0) * volume(j_buf, k_buf));
    
    // Calculate dtdivt
    auto dtdivt = select(div < -g_small, 
                        dtdiv_safe * (-Expr(1.0) / div),
                        g_big);
    
    // Final result: minimum of all dt values
    dt_min_val_func(j, k) = min(min(min(g_big, dtct), min(dtut, dtvt)), dtdivt);
}

extern "C" {
    void calc_dt_kernel_(
        int* x_min, int* x_max, int* y_min, int* y_max,
        double* xarea, double* yarea, double* celldx, double* celldy,
        double* volume, double* density0, double* viscosity_a, double* soundspeed,
        double* xvel0, double* yvel0, double* dt_min_val
    );
}

int main(int argc, char** argv)
{
    const int x_min = 0;
    const int x_max = 2e4;
    const int y_min = 0;
    const int y_max = 2e4;
    
    // Calculate buffer sizes based on Fortran array dimensions
    const int celldx_size = (x_max + 2) - (x_min - 2) + 1;
    const int celldy_size = (y_max + 2) - (y_min - 2) + 1;
    const int xarea_x_size = (x_max + 3) - (x_min - 2) + 1;
    const int xarea_y_size = (y_max + 2) - (y_min - 2) + 1;
    const int yarea_x_size = (x_max + 2) - (x_min - 2) + 1;
    const int yarea_y_size = (y_max + 3) - (y_min - 2) + 1;
    const int common_x_size = (x_max + 2) - (x_min - 2) + 1;
    const int common_y_size = (y_max + 2) - (y_min - 2) + 1;
    const int vel_x_size = (x_max + 3) - (x_min - 2) + 1;
    const int vel_y_size = (y_max + 3) - (y_min - 2) + 1;
    
    bool with_gpu = true;
    Var d1("j"), d2("k"), d1o, d2o, d1i, d2i;
    
    Target gpu_target = find_gpu_target();
    if (!gpu_target.has_gpu_feature())
    {
        printf("Don't have a gpu!\n");
        with_gpu = false;
    }
    Target cpu_target = get_host_target();
    
    auto i1_cpu = init_nonzero<1>("init1d", common_x_size >= 8);
    auto i2_cpu = init_nonzero<2>("init2d", common_x_size >= 8);
    auto z2_cpu = set_zero<2>("zero2d", common_x_size >= 8);
    auto i1_gpu = i1_cpu;
    auto i2_gpu = i2_cpu;
    auto z2_gpu = z2_cpu;
    
    i1_cpu.compile_jit(cpu_target);
    i2_cpu.compile_jit(cpu_target);
    z2_cpu.compile_jit(cpu_target);
    if (with_gpu) {
        i1_gpu.compile_jit(gpu_target);
        i2_gpu.compile_jit(gpu_target);
        z2_gpu.compile_jit(gpu_target);
    }
    
    // Create buffers for all input arrays
    Buffer<double, 1> celldx({celldx_size}, "celldx"), celldx_g(celldx);
    Buffer<double, 1> celldy({celldy_size}, "celldy"), celldy_g(celldy);
    Buffer<double, 2> xarea({xarea_x_size, xarea_y_size}, "xarea"), xarea_g(xarea);
    Buffer<double, 2> yarea({yarea_x_size, yarea_y_size}, "yarea"), yarea_g(yarea);
    Buffer<double, 2> volume({common_x_size, common_y_size}, "volume"), volume_g(volume);
    Buffer<double, 2> density0({common_x_size, common_y_size}, "density0"), density0_g(density0);
    Buffer<double, 2> viscosity_a({common_x_size, common_y_size}, "viscosity_a"), viscosity_a_g(viscosity_a);
    Buffer<double, 2> soundspeed({common_x_size, common_y_size}, "soundspeed"), soundspeed_g(soundspeed);
    Buffer<double, 2> xvel0({vel_x_size, vel_y_size}, "xvel0"), xvel0_g(xvel0);
    Buffer<double, 2> yvel0({vel_x_size, vel_y_size}, "yvel0"), yvel0_g(yvel0);
    
    // Output array: dt_min_val(x_min:x_max, y_min:y_max)
    const int output_j_size = x_max - x_min + 1;
    const int output_k_size = y_max - y_min + 1;
    
    Buffer<double, 2> dt_min_val_base({output_j_size, output_k_size}, "dt_min_val_base");
    Buffer<double, 2> dt_min_val_cpu({output_j_size, output_k_size}, "dt_min_val_cpu");
    Buffer<double, 2> dt_min_val_gpu({output_j_size, output_k_size}, "dt_min_val_gpu");
    
    printf("kernel start!\n");
    
    Func dt_min_val_func_cpu("dt_min_val_cpu");
    
    calc_dt_kernel_halide(dt_min_val_func_cpu,
                         celldx, celldy, xarea, yarea, volume,
                         density0, viscosity_a, soundspeed, xvel0, yvel0,
                         x_min, x_max, y_min, y_max);
    
    dt_min_val_func_cpu.parallel(d2);
    if (common_x_size >= 8) dt_min_val_func_cpu.vectorize(d1, 8);
    dt_min_val_func_cpu.compile_jit(cpu_target);
    
    // Initialize input data
    i1_cpu.realize(celldx);
    i1_cpu.realize(celldy);
    i2_cpu.realize(xarea);
    i2_cpu.realize(yarea);
    i2_cpu.realize(volume);
    i2_cpu.realize(density0);
    i2_cpu.realize(viscosity_a);
    i2_cpu.realize(soundspeed);
    i2_cpu.realize(xvel0);
    i2_cpu.realize(yvel0);
    
    z2_cpu.realize(dt_min_val_base);
    
    // Call original Fortran kernel for comparison
    int x_min_i = x_min, x_max_i = x_max, y_min_i = y_min, y_max_i = y_max;
    calc_dt_kernel_(&x_min_i, &x_max_i, &y_min_i, &y_max_i,
                   xarea.get()->begin(), yarea.get()->begin(),
                   celldx.get()->begin(), celldy.get()->begin(),
                   volume.get()->begin(), density0.get()->begin(),
                   viscosity_a.get()->begin(), soundspeed.get()->begin(),
                   xvel0.get()->begin(), yvel0.get()->begin(),
                   dt_min_val_base.get()->begin());
    
    try {
        z2_cpu.realize(dt_min_val_cpu);
        dt_min_val_func_cpu.realize(dt_min_val_cpu);
    } catch (RuntimeError &e) {
        std::cerr << e.what();
        return 1;
    }
    
    // Correctness check
    double correctness = 0.0;
    int count = 0;
    
    for (int k = y_min; k <= y_max; k++) {
        for (int j = x_min; j <= x_max; j++) {
            int hal_idx_j = j - x_min;
            int hal_idx_k = k - y_min;
            
            auto diff = abs(dt_min_val_base(hal_idx_j, hal_idx_k) - dt_min_val_cpu(hal_idx_j, hal_idx_k));
            
            if (diff >= 1e-10) {
                printf("dt_min_val difference [%d, %d]: Fort=%lf | Hali=%lf\n", 
                       j, k, dt_min_val_base(hal_idx_j, hal_idx_k), dt_min_val_cpu(hal_idx_j, hal_idx_k));
            }
            
            correctness += diff;
            count++;
        }
    }
    
    printf("dt_min_val average error: %e\n", correctness / count);
    
    // Performance measurement
    int times = 20;
    double cost_time = 0;
    struct timespec t1, t2, elapsed;
    
    // Measure original Fortran kernel performance
    cost_time = 0;
    for (int i = 0; i < times; i++) {
        clock_gettime(CLOCK_REALTIME, &t1);
        calc_dt_kernel_(&x_min_i, &x_max_i, &y_min_i, &y_max_i,
                       xarea.get()->begin(), yarea.get()->begin(),
                       celldx.get()->begin(), celldy.get()->begin(),
                       volume.get()->begin(), density0.get()->begin(),
                       viscosity_a.get()->begin(), soundspeed.get()->begin(),
                       xvel0.get()->begin(), yvel0.get()->begin(),
                       dt_min_val_base.get()->begin());
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("legacy code average cost time: %lfms\n\n", cost_time/times*1000);
    
    // Measure Halide CPU kernel performance
    cost_time = 0;
    for (int i = 0; i < times; i++) {
        clock_gettime(CLOCK_REALTIME, &t1);
        dt_min_val_func_cpu.realize(dt_min_val_cpu);
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("lifted cpu average cost time: %lfms\n\n", cost_time/times*1000);
    
    // GPU version if available
    if (with_gpu) {
        Func dt_min_val_func_gpu("dt_min_val_gpu");
        
        calc_dt_kernel_halide(dt_min_val_func_gpu,
                             celldx_g, celldy_g, xarea_g, yarea_g, volume_g,
                             density0_g, viscosity_a_g, soundspeed_g, xvel0_g, yvel0_g,
                             x_min, x_max, y_min, y_max);
        
        dt_min_val_func_gpu.gpu_tile(d1, d2, d1o, d2o, d1i, d2i, 16, 16)
                           .compile_jit(gpu_target);
        
        // Initialize GPU data
        i1_gpu.realize(celldx_g);
        i1_gpu.realize(celldy_g);
        i2_gpu.realize(xarea_g);
        i2_gpu.realize(yarea_g);
        i2_gpu.realize(volume_g);
        i2_gpu.realize(density0_g);
        i2_gpu.realize(viscosity_a_g);
        i2_gpu.realize(soundspeed_g);
        i2_gpu.realize(xvel0_g);
        i2_gpu.realize(yvel0_g);
        
        try {
            z2_gpu.realize(dt_min_val_gpu);
            dt_min_val_func_gpu.realize(dt_min_val_gpu);
        } catch (RuntimeError &e) {
            std::cerr << e.what();
            return 2;
        }
        
        // Measure GPU performance
        cost_time = 0;
        clock_gettime(CLOCK_REALTIME, &t1);
        for (int i = 0; i < times; i++) {
            dt_min_val_func_gpu.realize(dt_min_val_gpu);
        }
        dt_min_val_gpu.copy_to_host();
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
        printf("lifted gpu average cost time: %lfms\n\n", cost_time/times*1000);
    }
    
    return 0;
} 