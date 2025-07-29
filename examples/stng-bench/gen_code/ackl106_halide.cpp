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
    Func nz(funcName);
    Var x("x");
    nz(x) = Expr(1.0);
    return nz;
}

template<>
inline Func init_nonzero<2>(const std::string& funcName, bool vectorize)
{
    Func nz(funcName);
    Var x("x"), y("y");
    nz(x, y) = Expr(1.0);
    return nz;
}

template <>
inline Func set_zero<2>(const std::string& funcName, bool vectorize)
{
    Func set_zero(funcName);
    Var x("x"), y("y");
    set_zero(x, y) = Expr(0.0);
    return set_zero;
}

extern "C" {
    void advec_cell_kernel_loop106_(const int* j, const int* k,
                                  double *advec_vol,
                                  double *density1,
                                  double *ener_flux, 
                                  double *energy1,
                                  double *mass_flux_x,
                                  double *mass_flux_y,
                                  double *post_ener,
                                  double *post_mass,
                                  double *pre_mass,
                                  double *pre_vol,
                                  double *vol_flux_x,
                                  double *vol_flux_y,
                                  const int* x_max, const int* x_min,
                                  const int* y_max, const int* y_min);
}

#ifndef _2D_1
#define _2D_1 1e4
#define _2D_2 1e4
#endif

int main(int argc, char** argv)
{
    // printf("Caller Start!\n");
    const int x_max = _2D_1;  
    const int x_min = 0;
    const int y_max = _2D_2;  
    const int y_min = 0;

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

    // Input buffers - match Fortran array dimensions
    Buffer<double,2> density1({x_range+5, y_range+5}, "density1"), density1_g(density1);
    Buffer<double,2> ener_flux({x_range+6, y_range+6}, "ener_flux"), ener_flux_g(ener_flux);
    Buffer<double,2> energy1({x_range+5, y_range+5}, "energy1"), energy1_g(energy1);
    Buffer<double,2> mass_flux_x({x_range+6, y_range+5}, "mass_flux_x"), mass_flux_x_g(mass_flux_x);
    Buffer<double,2> mass_flux_y({x_range+5, y_range+6}, "mass_flux_y"), mass_flux_y_g(mass_flux_y);
    Buffer<double,2> vol_flux_x({x_range+6, y_range+5}, "vol_flux_x"), vol_flux_x_g(vol_flux_x);
    Buffer<double,2> vol_flux_y({x_range+5, y_range+6}, "vol_flux_y"), vol_flux_y_g(vol_flux_y);
    Buffer<double,2> pre_vol({x_range+6, y_range+6}, "pre_vol"), pre_vol_g(pre_vol);

    // Output buffers - match Fortran output arrays
    Buffer<double,2> advec_vol_base({x_range+6, y_range+6}, "advec_vol_base");
    Buffer<double,2> density1_base({x_range+5, y_range+5}, "density1_base");
    Buffer<double,2> ener_flux_base({x_range+6, y_range+6}, "ener_flux_base");
    Buffer<double,2> energy1_base({x_range+5, y_range+5}, "energy1_base");
    Buffer<double,2> post_ener_base({x_range+6, y_range+6}, "post_ener_base");
    Buffer<double,2> post_mass_base({x_range+6, y_range+6}, "post_mass_base");
    Buffer<double,2> pre_mass_base({x_range+6, y_range+6}, "pre_mass_base");
    Buffer<double,2> pre_vol_base({x_range+6, y_range+6}, "pre_vol_base");

    // Halide output buffers
    Buffer<double,2> density1_cpu({x_range+5, y_range+5}, "density1_cpu");
    Buffer<double,2> energy1_cpu({x_range+5, y_range+5}, "energy1_cpu");
    
    Buffer<double,2> density1_gpu({x_range+5, y_range+5}, "density1_gpu");
    Buffer<double,2> energy1_gpu({x_range+5, y_range+5}, "energy1_gpu");

    // --------------------------- advec_cell_kernel_loop106 kernel --------------------------
    Func cpu_fn_density1("cpu_fn_density1");
    cpu_fn_density1(j, k) = select(
        j >= 2 && j <= (x_range + 2) && k >= 2 && k <= (y_range + 2),
        (density1(j, k) * pre_vol(j, k) + mass_flux_y(j, k) - mass_flux_y(j, k + 1)) / 
        (pre_vol(j, k) + vol_flux_y(j, k) - vol_flux_y(j, k + 1)),
        density1(j, k)
    );
    cpu_fn_density1.parallel(k);
    if (x_range + 1 >= 8) cpu_fn_density1.vectorize(j, 8);
    cpu_fn_density1.compile_jit(cpu_target);

    Func cpu_fn_energy1("cpu_fn_energy1");
    cpu_fn_energy1(j, k) = select(
        j >= 2 && j <= (x_range + 2) && k >= 2 && k <= (y_range + 2),
        (energy1(j, k) * density1(j, k) * pre_vol(j, k) + 
         ener_flux(j, k) - ener_flux(j, k + 1)) / 
        (density1(j, k) * pre_vol(j, k) + mass_flux_y(j, k) - mass_flux_y(j, k + 1)),
        energy1(j, k)
    );
    cpu_fn_energy1.parallel(k);
    if (x_range + 1 >= 8) cpu_fn_energy1.vectorize(j, 8);
    cpu_fn_energy1.compile_jit(cpu_target);

    srand(42);
    for (int y = 0; y < density1.height(); y++) {
        for (int x = 0; x < density1.width(); x++) {
            density1(x, y) = ((((x + y) % 13) + 0.1) / 13.0) * 2.0 + 1.0;
            energy1(x, y) = ((((x + y) % 17) + 0.2) / 17.0) * 3.0 + 1.0;
        }
    }
    
    for (int y = 0; y < pre_vol.height(); y++) {
        for (int x = 0; x < pre_vol.width(); x++) {
            pre_vol(x, y) = ((((x + y) % 19) + 0.3) / 19.0) * 2.0 + 1.0;
        }
    }
    
    for (int y = 0; y < mass_flux_y.height(); y++) {
        for (int x = 0; x < mass_flux_y.width(); x++) {
            mass_flux_y(x, y) = ((((x + y) % 11) + 0.4) / 11.0) * 0.5;
        }
    }
    
    for (int y = 0; y < vol_flux_y.height(); y++) {
        for (int x = 0; x < vol_flux_y.width(); x++) {
            vol_flux_y(x, y) = ((((x + y) % 23) + 0.5) / 23.0) * 0.3;
        }
    }
    
    for (int y = 0; y < ener_flux.height(); y++) {
        for (int x = 0; x < ener_flux.width(); x++) {
            ener_flux(x, y) = ((((x + y) % 7) + 0.6) / 7.0) * 1.5;
        }
    }

    for (int y = 0; y < mass_flux_x.height(); y++) {
        for (int x = 0; x < mass_flux_x.width(); x++) {
            mass_flux_x(x, y) = ((((x + y) % 29) + 0.7) / 29.0) * 0.8;
        }
    }

    for (int y = 0; y < vol_flux_x.height(); y++) {
        for (int x = 0; x < vol_flux_x.width(); x++) {
            vol_flux_x(x, y) = ((((x + y) % 31) + 0.8) / 31.0) * 0.4;
        }
    }

    // Initialize output arrays
    zero_cpu.realize(advec_vol_base);
    zero_cpu.realize(post_ener_base);
    zero_cpu.realize(post_mass_base);
    zero_cpu.realize(pre_mass_base);
    
    // Copy input data for Fortran call
    density1_base.copy_from(density1);
    energy1_base.copy_from(energy1);
    pre_vol_base.copy_from(pre_vol);

    // Calling baseline Fortran
    double *advec_vol_ = advec_vol_base.get()->begin();
    double *density1_ = density1_base.get()->begin();
    double *ener_flux_ = ener_flux.get()->begin();
    double *energy1_ = energy1_base.get()->begin();
    double *mass_flux_x_ = mass_flux_x.get()->begin();
    double *mass_flux_y_ = mass_flux_y.get()->begin();
    double *post_ener_ = post_ener_base.get()->begin();
    double *post_mass_ = post_mass_base.get()->begin();
    double *pre_mass_ = pre_mass_base.get()->begin();
    double *pre_vol_ = pre_vol_base.get()->begin();
    double *vol_flux_x_ = vol_flux_x.get()->begin();
    double *vol_flux_y_ = vol_flux_y.get()->begin();
    
    int dummy_j = 0, dummy_k = 0;
    advec_cell_kernel_loop106_(&dummy_j, &dummy_k,
                              advec_vol_, density1_, ener_flux_, energy1_,
                              mass_flux_x_, mass_flux_y_, post_ener_, post_mass_,
                              pre_mass_, pre_vol_, vol_flux_x_, vol_flux_y_,
                              &x_max, &x_min, &y_max, &y_min);
    
    // Calling halide cpu
    try {
        // Initialize output buffers
        zero_cpu.realize(density1_cpu);
        zero_cpu.realize(energy1_cpu);
        
        // Execute the computation
        cpu_fn_density1.realize(density1_cpu);
        cpu_fn_energy1.realize(energy1_cpu);
    } catch (RuntimeError &e) {
        std::cerr << e.what();
        return 1;
    }

    // Verify correctness for density1 and energy1
    double correctness_density1 = 0.0;
    double correctness_energy1 = 0.0;
    int errors_density1 = 0;
    int errors_energy1 = 0;
    
    // Compare results in the computation range
    for (int k_idx = 0; k_idx < y_range + 1; k_idx++) { 
        for (int j_idx = 0; j_idx < x_range + 1; j_idx++) {
            double diff_density1 = abs(density1_base(j_idx + 2, k_idx + 2) - density1_cpu(j_idx + 2, k_idx + 2));
            double diff_energy1 = abs(energy1_base(j_idx + 2, k_idx + 2) - energy1_cpu(j_idx + 2, k_idx + 2));
            
            if (diff_density1 >= 1e-10)
            {
                if (errors_density1 < 10) {
                    printf("density1 [%d, %d] = Fortran: %lf | Halide: %lf | diff: %e\n", 
                            j_idx, k_idx, density1_base(j_idx + 2, k_idx + 2), density1_cpu(j_idx + 2, k_idx + 2), diff_density1);
                }
                errors_density1++;
            }
            
            if (diff_energy1 >= 1e-10)
            {
                if (errors_energy1 < 10) {
                    printf("energy1 [%d, %d] = Fortran: %lf | Halide: %lf | diff: %e\n", 
                            j_idx, k_idx, energy1_base(j_idx + 2, k_idx + 2), energy1_cpu(j_idx + 2, k_idx + 2), diff_energy1);
                }
                errors_energy1++;
            }
            
            correctness_density1 += diff_density1;
            correctness_energy1 += diff_energy1;
        }
    }

    // printf("Total errors for density1: %d, sum of differences: %e\n", errors_density1, correctness_density1);
    // printf("Total errors for energy1: %d, sum of differences: %e\n", errors_energy1, correctness_energy1);

    int times = 5;
    double cost_time = 0;
    struct timespec t1, t2, elapsed;
    
    // Benchmark Fortran version
    for (int i = 0; i < times; i++)
    {
        clock_gettime(CLOCK_REALTIME, &t1);
        advec_cell_kernel_loop106_(&dummy_j, &dummy_k,
                                  advec_vol_, density1_, ener_flux_, energy1_,
                                  mass_flux_x_, mass_flux_y_, post_ener_, post_mass_,
                                  pre_mass_, pre_vol_, vol_flux_x_, vol_flux_y_,
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
        cpu_fn_density1.realize(density1_cpu);
        cpu_fn_energy1.realize(energy1_cpu);
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("lifted cpu: %lfms\n", cost_time/times*1000);

    if (with_gpu)
    {
        // Building GPU functions
        Func gpu_fn_density1("gpu_fn_density1");
        gpu_fn_density1(j, k) = select(
            j >= 2 && j <= (x_range + 2) && k >= 2 && k <= (y_range + 2),
            (density1_g(j, k) * pre_vol_g(j, k) + mass_flux_y_g(j, k) - mass_flux_y_g(j, k + 1)) / 
            (pre_vol_g(j, k) + vol_flux_y_g(j, k) - vol_flux_y_g(j, k + 1)),
            density1_g(j, k) 
        );
        gpu_fn_density1.gpu_tile(j, k, bj, bk, tj, tk, 8, 8).compile_jit(gpu_target);

        Func gpu_fn_energy1("gpu_fn_energy1");
        gpu_fn_energy1(j, k) = select(
            j >= 2 && j <= (x_range + 2) && k >= 2 && k <= (y_range + 2),
            (energy1_g(j, k) * density1_g(j, k) * pre_vol_g(j, k) + 
             ener_flux_g(j, k) - ener_flux_g(j, k + 1)) / 
            (density1_g(j, k) * pre_vol_g(j, k) + mass_flux_y_g(j, k) - mass_flux_y_g(j, k + 1)),
            energy1_g(j, k)
        );
        gpu_fn_energy1.gpu_tile(j, k, bj, bk, tj, tk, 8, 8).compile_jit(gpu_target);
        
        // GPU IO initialization
        density1_g.copy_from(density1);
        energy1_g.copy_from(energy1);
        pre_vol_g.copy_from(pre_vol);
        mass_flux_y_g.copy_from(mass_flux_y);
        vol_flux_y_g.copy_from(vol_flux_y);
        ener_flux_g.copy_from(ener_flux);
        
        // warmup
        try {
            zero_gpu.realize(density1_gpu);
            zero_gpu.realize(energy1_gpu);
            
            gpu_fn_density1.realize(density1_gpu);
            gpu_fn_energy1.realize(energy1_gpu);
        } catch ( RuntimeError &e ) {
            std::cerr << e.what();
            return -1;
        }

        // Benchmark GPU version
        cost_time = 0;
        clock_gettime(CLOCK_REALTIME, &t1);
        for (int i = 0; i < times; i++)
        {
            gpu_fn_density1.realize(density1_gpu);
            gpu_fn_energy1.realize(energy1_gpu);
        }
        density1_gpu.copy_to_host();
        energy1_gpu.copy_to_host();
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time = toSec(elapsed);
        printf("lifted gpu: %lfms\n\n", cost_time/times*1000);
    }

    return 0;
} 