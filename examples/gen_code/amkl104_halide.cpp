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

void advec_mom_kernel_loop104_halide(Func& mom_flux_func,
                                    Buffer<double, 2> node_flux,
                                    Buffer<double, 2> node_mass_pre,
                                    Buffer<double, 1> celldx,
                                    Buffer<double, 2> vel1,
                                    int x_min, int x_max, int y_min, int y_max) {
    
    Var j("j"), k("k");
    
    Expr j_offset = Expr(1);  // j=0 -> Fortran j=-1 -> array index 1
    Expr k_offset = Expr(2);  // k=0 -> Fortran k=0 -> array index 2
    
    auto j_buf = j + j_offset;
    auto k_buf = k + k_offset;
    
    auto node_flux_negative = node_flux(j_buf, k_buf) < Expr(0.0);
    
    auto upwind_buf = select(node_flux_negative, 
                            min(j_buf+2, Expr(x_max+3+j_offset-1)), 
                            max(j_buf-1, Expr(x_min-2+j_offset)));
    auto donor_buf = select(node_flux_negative, j_buf+1, j_buf);
    auto downwind_buf = select(node_flux_negative, j_buf, j_buf+1);
    auto dif_buf = select(node_flux_negative, j_buf+1, 
                         max(j_buf-1, Expr(x_min-2+j_offset)));
    
    auto sigma = abs(node_flux(j_buf, k_buf)) / node_mass_pre(donor_buf, k_buf);
    auto width = celldx(j_buf);
    
    auto vdiffuw = vel1(donor_buf, k_buf) - vel1(upwind_buf, k_buf);
    auto vdiffdw = vel1(downwind_buf, k_buf) - vel1(donor_buf, k_buf);
    
    auto limiter_condition = vdiffuw * vdiffdw > Expr(0.0);
    
    auto auw = abs(vdiffuw);
    auto adw = abs(vdiffdw);
    auto wind = select(vdiffdw <= Expr(0.0), Expr(-1.0), Expr(1.0));
    
    auto limiter = select(limiter_condition,
        wind * min(width * ((Expr(2.0) - sigma) * adw / width + (Expr(1.0) + sigma) * auw / celldx(dif_buf)) / Expr(6.0),
                   min(auw, adw)),
        Expr(0.0));
    
    auto advec_vel_s = vel1(donor_buf, k_buf) + (Expr(1.0) - sigma) * limiter;
    

    auto is_boundary = j == Expr(0);
    
    mom_flux_func(j, k) = select(is_boundary, 
                                Expr(0.0), 
                                advec_vel_s * node_flux(j_buf, k_buf));
}

extern "C" {
    void advec_mom_kernel_loop104_(
        double* mom_flux, double* node_flux, double* node_mass_pre,
        int* x_max, int* x_min, int* y_max, int* y_min,
        double* celldx, double* vel1
    );
}

int main(int argc, char** argv)
{
    const int x_min = 0;
    const int x_max = 2e4;
    const int y_min = 0;
    const int y_max = 2e4;
    
    const int array_x_size = (x_max + 5) - (x_min - 2) + 1;
    const int array_y_size = (y_max + 3) - (y_min - 2) + 1;
    const int celldx_size = array_x_size;
    
    bool with_gpu = true;
    Var d1("j"), d2("k"), d1o, d2o, d1i, d2i;
    
    Target gpu_target = find_gpu_target();
    if (!gpu_target.has_gpu_feature())
    {
        printf("Don't have a gpu!\n");
        with_gpu = false;
    }
    Target cpu_target = get_host_target();
    
    auto i1_cpu = init_nonzero<1>("init1d", array_x_size >= 8);
    auto i2_cpu = init_nonzero<2>("init2d", array_x_size >= 8);
    auto z2_cpu = set_zero<2>("zero2d", array_x_size >= 8);
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
    
    Buffer<double, 2> node_flux({array_x_size, array_y_size}, "node_flux"), node_flux_g(node_flux);
    Buffer<double, 2> node_mass_pre({array_x_size, array_y_size}, "node_mass_pre"), node_mass_pre_g(node_mass_pre);
    Buffer<double, 1> celldx({celldx_size}, "celldx"), celldx_g(celldx);
    Buffer<double, 2> vel1({array_x_size, array_y_size}, "vel1"), vel1_g(vel1);

    Buffer<double, 2> mom_flux_base({array_x_size, array_y_size}, "mom_flux_base");
    
    // j: x_min-1 to x_max+1 (x_max - x_min + 3), k: y_min to y_max+1 (y_max - y_min + 2)
    const int output_j_size = x_max - x_min + 3;
    const int output_k_size = y_max - y_min + 2;
    
    Buffer<double, 2> mom_flux_cpu({output_j_size, output_k_size}, "mom_flux_cpu");
    Buffer<double, 2> mom_flux_gpu({output_j_size, output_k_size}, "mom_flux_gpu");
    
    printf("kernel start!\n");
    
    Func mom_flux_func_cpu("mom_flux_cpu");
    
    advec_mom_kernel_loop104_halide(mom_flux_func_cpu,
                                   node_flux, node_mass_pre, celldx, vel1,
                                   x_min, x_max, y_min, y_max);
    
    mom_flux_func_cpu.parallel(d2);
    if (array_x_size >= 8) mom_flux_func_cpu.vectorize(d1, 8);
    mom_flux_func_cpu.compile_jit(cpu_target);
    
    i2_cpu.realize(node_flux);
    i2_cpu.realize(node_mass_pre);
    i1_cpu.realize(celldx);
    i2_cpu.realize(vel1);
    
    z2_cpu.realize(mom_flux_base);
    
    std::vector<double> node_flux_f(array_x_size * array_y_size);
    std::vector<double> node_mass_pre_f(array_x_size * array_y_size);
    std::vector<double> vel1_f(array_x_size * array_y_size);
    std::vector<double> mom_flux_f(array_x_size * array_y_size, 0.0);
    
    for (int k = 0; k < array_y_size; k++) {
        for (int j = 0; j < array_x_size; j++) {
            int fort_idx = k + j * array_y_size;
            
            node_flux_f[fort_idx] = node_flux(j, k);
            node_mass_pre_f[fort_idx] = node_mass_pre(j, k);
            vel1_f[fort_idx] = vel1(j, k);
        }
    }
    
    int x_min_i = x_min, x_max_i = x_max, y_min_i = y_min, y_max_i = y_max;
    advec_mom_kernel_loop104_(mom_flux_f.data(),
                             node_flux_f.data(),
                             node_mass_pre_f.data(),
                             &x_max_i, &x_min_i, &y_max_i, &y_min_i,
                             celldx.get()->begin(),
                             vel1_f.data());
    
    for (int k = 0; k < array_y_size; k++) {
        for (int j = 0; j < array_x_size; j++) {
            int fort_idx = k + j * array_y_size;
            mom_flux_base(j, k) = mom_flux_f[fort_idx];
        }
    }
    
    try {
        z2_cpu.realize(mom_flux_cpu);
        mom_flux_func_cpu.realize(mom_flux_cpu);
    } catch (RuntimeError &e) {
        std::cerr << e.what();
        return 1;
    }
    
    double correctness = 0.0;
    int count = 0;
    int significant_errors = 0;
    
    for (int k = y_min; k <= y_max+1; k++) {
        for (int j = x_min-1; j <= x_max+1; j++) {
            int fort_idx_j = j - (x_min - 2);
            int fort_idx_k = k - (y_min - 2);
            
            int hal_idx_j = j - (x_min - 1);
            int hal_idx_k = k - y_min;
            
            auto diff = abs(mom_flux_base(fort_idx_j, fort_idx_k) - mom_flux_cpu(hal_idx_j, hal_idx_k));
            
            if (diff >= 1e-6) {
                significant_errors++;
                if (significant_errors <= 5) {
                    printf("Significant error [%d, %d]: Fort=%lf | Hali=%lf (diff=%e)\n", 
                           j, k, mom_flux_base(fort_idx_j, fort_idx_k), mom_flux_cpu(hal_idx_j, hal_idx_k), diff);
                }
            }
            
            correctness += diff;
            count++;
        }
    }
    
    printf("mom_flux average error: %e\n", correctness / count);
    
    int times = 20;
    double cost_time = 0;
    struct timespec t1, t2, elapsed;
    
    cost_time = 0;
    for (int i = 0; i < times; i++) {
        clock_gettime(CLOCK_REALTIME, &t1);
        advec_mom_kernel_loop104_(mom_flux_base.get()->begin(),
                                 node_flux.get()->begin(),
                                 node_mass_pre.get()->begin(),
                                 &x_max_i, &x_min_i, &y_max_i, &y_min_i,
                                 celldx.get()->begin(),
                                 vel1.get()->begin());
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("legacy code average cost time: %lfms\n\n", cost_time/times*1000);
    
    cost_time = 0;
    for (int i = 0; i < times; i++) {
        clock_gettime(CLOCK_REALTIME, &t1);
        mom_flux_func_cpu.realize(mom_flux_cpu);
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("lifted cpu average cost time: %lfms\n\n", cost_time/times*1000);
    
    if (with_gpu) {
        Func mom_flux_func_gpu("mom_flux_gpu");
        
        advec_mom_kernel_loop104_halide(mom_flux_func_gpu,
                                       node_flux_g, node_mass_pre_g, celldx_g, vel1_g,
                                       x_min, x_max, y_min, y_max);
        
        mom_flux_func_gpu.gpu_tile(d1, d2, d1o, d2o, d1i, d2i, 16, 16)
                         .compile_jit(gpu_target);
            
        i2_gpu.realize(node_flux_g);
        i2_gpu.realize(node_mass_pre_g);
        i1_gpu.realize(celldx_g);
        i2_gpu.realize(vel1_g);
        
        try {
            z2_gpu.realize(mom_flux_gpu);
            mom_flux_func_gpu.realize(mom_flux_gpu);
        } catch (RuntimeError &e) {
            std::cerr << e.what();
            return 2;
        }
        
        cost_time = 0;
        clock_gettime(CLOCK_REALTIME, &t1);
        for (int i = 0; i < times; i++) {
            mom_flux_func_gpu.realize(mom_flux_gpu);
        }
        mom_flux_gpu.copy_to_host();
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
        printf("lifted gpu average cost time: %lfms\n\n", cost_time/times*1000);
    }
    
    return 0;
} 