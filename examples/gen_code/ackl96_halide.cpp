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

void advec_cell_kernel_halide(Func& mass_flux_y_func, Func& ener_flux_func,
                             Buffer<double, 1> vertexdy,
                             Buffer<double, 2> density1,
                             Buffer<double, 2> energy1,
                             Buffer<double, 2> vol_flux_y,
                             Buffer<double, 2> pre_vol,
                             int x_min, int x_max, int y_min, int y_max) {
    
    Var j("j"), k("k");
    
    Expr j_offset = Expr(2);  // x_min-2 offset, since x_min=0, offset is 2
    Expr k_offset = Expr(2);  // y_min-2 offset, since y_min=0, offset is 2
    
    Expr one_by_six = Expr(1.0/6.0);
    
    auto j_buf = j + j_offset;
    auto k_buf = k + k_offset;
    
    auto vol_flux_positive = vol_flux_y(j_buf, k_buf) > Expr(0.0);
    
    auto upwind_buf = select(vol_flux_positive, k_buf-2, min(k_buf+1, y_max+2+k_offset));
    auto donor_buf = select(vol_flux_positive, k_buf-1, k_buf);
    auto downwind_buf = select(vol_flux_positive, k_buf, k_buf-1);
    auto dif_buf = select(vol_flux_positive, k_buf-1, min(k_buf+1, y_max+2+k_offset));
    
    auto sigmat = abs(vol_flux_y(j_buf, k_buf)) / pre_vol(j_buf, donor_buf);
    auto sigma3 = (Expr(1.0) + sigmat) * (vertexdy(k_buf) / vertexdy(dif_buf));
    auto sigma4 = Expr(2.0) - sigmat;
    
    auto sigma = sigmat;
    auto sigmav = sigmat;
    
    auto diffuw_density = density1(j_buf, donor_buf) - density1(j_buf, upwind_buf);
    auto diffdw_density = density1(j_buf, downwind_buf) - density1(j_buf, donor_buf);
    auto wind_density = select(diffdw_density <= Expr(0.0), Expr(-1.0), Expr(1.0));
    
    auto limiter_condition_density = diffuw_density * diffdw_density > Expr(0.0);
    auto limiter_density = select(limiter_condition_density,
        (Expr(1.0) - sigmav) * wind_density * 
        min(abs(diffuw_density), 
            min(abs(diffdw_density),
                one_by_six * (sigma3 * abs(diffuw_density) + sigma4 * abs(diffdw_density)))),
        Expr(0.0));
    
    auto mass_flux_result = vol_flux_y(j_buf, k_buf) * (density1(j_buf, donor_buf) + limiter_density);
    mass_flux_y_func(j, k) = mass_flux_result;
    
    auto sigmam = abs(mass_flux_result) / (density1(j_buf, donor_buf) * pre_vol(j_buf, donor_buf));
    
    auto diffuw_energy = energy1(j_buf, donor_buf) - energy1(j_buf, upwind_buf);
    auto diffdw_energy = energy1(j_buf, downwind_buf) - energy1(j_buf, donor_buf);
    auto wind_energy = select(diffdw_energy <= Expr(0.0), Expr(-1.0), Expr(1.0));
    
    auto limiter_condition_energy = diffuw_energy * diffdw_energy > Expr(0.0);
    auto limiter_energy = select(limiter_condition_energy,
        (Expr(1.0) - sigmam) * wind_energy * 
        min(abs(diffuw_energy), 
            min(abs(diffdw_energy),
                one_by_six * (sigma3 * abs(diffuw_energy) + sigma4 * abs(diffdw_energy)))),
        Expr(0.0));
    
    ener_flux_func(j, k) = mass_flux_result * (energy1(j_buf, donor_buf) + limiter_energy);
}

extern "C" {
    void advec_cell_kernel_(
        int* x_min, int* x_max, int* y_min, int* y_max,
        double* vertexdy, double* density1, double* energy1,
        double* mass_flux_y, double* vol_flux_y, double* pre_vol, double* ener_flux
    );
}

int main(int argc, char** argv)
{
    const int x_min = 0;
    const int x_max = 2e4;
    const int y_min = 0;
    const int y_max = 2e4;
    
    const int vertexdy_size = (y_max + 3) - (y_min - 2) + 1;
    const int density_x_size = (x_max + 2) - (x_min - 2) + 1;
    const int density_y_size = (y_max + 2) - (y_min - 2) + 1;
    const int flux_x_size = (x_max + 2) - (x_min - 2) + 1;
    const int flux_y_size = (y_max + 3) - (y_min - 2) + 1;
    const int prevol_x_size = (x_max + 3) - (x_min - 2) + 1;
    const int prevol_y_size = (y_max + 3) - (y_min - 2) + 1;
    
    bool with_gpu = true;
    Var d1("j"), d2("k"), d1o, d2o, d1i, d2i;
    
    Target gpu_target = find_gpu_target();
    if (!gpu_target.has_gpu_feature())
    {
        printf("Don't have a gpu!\n");
        with_gpu = false;
    }
    Target cpu_target = get_host_target();
    
    auto i1_cpu = init_nonzero<1>("init1d", density_x_size >= 8);
    auto i2_cpu = init_nonzero<2>("init2d", density_x_size >= 8);
    auto z2_cpu = set_zero<2>("zero2d", density_x_size >= 8);
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
    
    Buffer<double, 1> vertexdy({vertexdy_size}, "vertexdy"), vertexdy_g(vertexdy);
    Buffer<double, 2> density1({density_x_size, density_y_size}, "density1"), density1_g(density1);
    Buffer<double, 2> energy1({density_x_size, density_y_size}, "energy1"), energy1_g(energy1);
    Buffer<double, 2> vol_flux_y({flux_x_size, flux_y_size}, "vol_flux_y"), vol_flux_y_g(vol_flux_y);
    Buffer<double, 2> pre_vol({prevol_x_size, prevol_y_size}, "pre_vol"), pre_vol_g(pre_vol);

    Buffer<double, 2> mass_flux_y_base({flux_x_size, flux_y_size}, "mass_flux_y_base");
    Buffer<double, 2> ener_flux_base({prevol_x_size, prevol_y_size}, "ener_flux_base");
    
    // j: x_min to x_max (x_max - x_min + 1), k: y_min to y_max+2 (y_max - y_min + 3)
    const int output_j_size = x_max - x_min + 1;
    const int output_k_size = y_max - y_min + 3;
    
    Buffer<double, 2> mass_flux_y_cpu({output_j_size, output_k_size}, "mass_flux_y_cpu");
    Buffer<double, 2> ener_flux_cpu({output_j_size, output_k_size}, "ener_flux_cpu");
    
    Buffer<double, 2> mass_flux_y_gpu({output_j_size, output_k_size}, "mass_flux_y_gpu");
    Buffer<double, 2> ener_flux_gpu({output_j_size, output_k_size}, "ener_flux_gpu");
    
    printf("kernel start!\n");
    
    Func mass_flux_y_func_cpu("mass_flux_y_cpu");
    Func ener_flux_func_cpu("ener_flux_cpu");
    
    advec_cell_kernel_halide(mass_flux_y_func_cpu, ener_flux_func_cpu,
                            vertexdy, density1, energy1, vol_flux_y, pre_vol,
                            x_min, x_max, y_min, y_max);
    
    mass_flux_y_func_cpu.parallel(d2);
    if (density_x_size >= 8) mass_flux_y_func_cpu.vectorize(d1, 8);
    mass_flux_y_func_cpu.compile_jit(cpu_target);
    
    ener_flux_func_cpu.parallel(d2);
    if (density_x_size >= 8) ener_flux_func_cpu.vectorize(d1, 8);
    ener_flux_func_cpu.compile_jit(cpu_target);
    
    i1_cpu.realize(vertexdy);
    i2_cpu.realize(density1);
    i2_cpu.realize(energy1);
    i2_cpu.realize(vol_flux_y);
    i2_cpu.realize(pre_vol);
    
    z2_cpu.realize(mass_flux_y_base);
    z2_cpu.realize(ener_flux_base);
    
    int x_min_i = x_min, x_max_i = x_max, y_min_i = y_min, y_max_i = y_max;
    advec_cell_kernel_(&x_min_i, &x_max_i, &y_min_i, &y_max_i,
                      vertexdy.get()->begin(),
                      density1.get()->begin(),
                      energy1.get()->begin(),
                      mass_flux_y_base.get()->begin(),
                      vol_flux_y.get()->begin(),
                      pre_vol.get()->begin(),
                      ener_flux_base.get()->begin());
    
    try {
        z2_cpu.realize(mass_flux_y_cpu);
        z2_cpu.realize(ener_flux_cpu);
        mass_flux_y_func_cpu.realize(mass_flux_y_cpu);
        ener_flux_func_cpu.realize(ener_flux_cpu);
    } catch (RuntimeError &e) {
        std::cerr << e.what();
        return 1;
    }
    
    double correctness_mass = 0.0;
    double correctness_ener = 0.0;
    int count = 0;
    
    for (int k = y_min; k <= y_max+2; k++) {
        for (int j = x_min; j <= x_max; j++) {
            int fort_idx_j = j - (x_min - 2);
            int fort_idx_k = k - (y_min - 2);
            
            int hal_idx_j = j - x_min;
            int hal_idx_k = k - y_min;
            
            auto mass_diff = abs(mass_flux_y_base(fort_idx_j, fort_idx_k) - mass_flux_y_cpu(hal_idx_j, hal_idx_k));
            auto ener_diff = abs(ener_flux_base(fort_idx_j, fort_idx_k) - ener_flux_cpu(hal_idx_j, hal_idx_k));
            
            if (mass_diff >= 1e-10) {
                // printf("mass_flux difference [%d, %d]: Fort=%lf | Hali=%lf\n", 
                //        j, k, mass_flux_y_base(fort_idx_j, fort_idx_k), mass_flux_y_cpu(hal_idx_j, hal_idx_k));
            }
            if (ener_diff >= 1e-10) {
                // printf("ener_flux difference [%d, %d]: Fort=%lf | Hali=%lf\n", 
                //        j, k, ener_flux_base(fort_idx_j, fort_idx_k), ener_flux_cpu(hal_idx_j, hal_idx_k));
            }
            
            correctness_mass += mass_diff;
            correctness_ener += ener_diff;
            count++;
        }
    }
    
    // printf("mass_flux average error: %e\n", correctness_mass / count);
    // printf("ener_flux average error: %e\n", correctness_ener / count);
    
    int times = 20;
    double cost_time = 0;
    struct timespec t1, t2, elapsed;
    
    cost_time = 0;
    for (int i = 0; i < times; i++) {
        clock_gettime(CLOCK_REALTIME, &t1);
        advec_cell_kernel_(&x_min_i, &x_max_i, &y_min_i, &y_max_i,
                          vertexdy.get()->begin(),
                          density1.get()->begin(),
                          energy1.get()->begin(),
                          mass_flux_y_base.get()->begin(),
                          vol_flux_y.get()->begin(),
                          pre_vol.get()->begin(),
                          ener_flux_base.get()->begin());
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("legacy code average cost time: %lfms\n\n", cost_time/times*1000);
    
    cost_time = 0;
    for (int i = 0; i < times; i++) {
        clock_gettime(CLOCK_REALTIME, &t1);
        mass_flux_y_func_cpu.realize(mass_flux_y_cpu);
        ener_flux_func_cpu.realize(ener_flux_cpu);
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("lifted cpu average cost time: %lfms\n\n", cost_time/times*1000);
    
    if (with_gpu) {
        Func mass_flux_y_func_gpu("mass_flux_y_gpu");
        Func ener_flux_func_gpu("ener_flux_gpu");
        
        advec_cell_kernel_halide(mass_flux_y_func_gpu, ener_flux_func_gpu,
                                vertexdy_g, density1_g, energy1_g, vol_flux_y_g, pre_vol_g,
                                x_min, x_max, y_min, y_max);
        
        mass_flux_y_func_gpu.gpu_tile(d1, d2, d1o, d2o, d1i, d2i, 16, 16)
                            .compile_jit(gpu_target);
        ener_flux_func_gpu.gpu_tile(d1, d2, d1o, d2o, d1i, d2i, 16, 16)
                          .compile_jit(gpu_target);
            
        i1_gpu.realize(vertexdy_g);
        i2_gpu.realize(density1_g);
        i2_gpu.realize(energy1_g);
        i2_gpu.realize(vol_flux_y_g);
        i2_gpu.realize(pre_vol_g);
        
        try {
            z2_gpu.realize(mass_flux_y_gpu);
            z2_gpu.realize(ener_flux_gpu);
            mass_flux_y_func_gpu.realize(mass_flux_y_gpu);
            ener_flux_func_gpu.realize(ener_flux_gpu);
        } catch (RuntimeError &e) {
            std::cerr << e.what();
            return 2;
        }
        
        cost_time = 0;
        clock_gettime(CLOCK_REALTIME, &t1);
        for (int i = 0; i < times; i++) {
            mass_flux_y_func_gpu.realize(mass_flux_y_gpu);
            ener_flux_func_gpu.realize(ener_flux_gpu);
        }
        mass_flux_y_gpu.copy_to_host();
        ener_flux_gpu.copy_to_host();
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
        printf("lifted gpu average cost time: %lfms\n\n", cost_time/times*1000);
    }
    
    return 0;
} 