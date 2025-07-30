#include <cstdio>
#include <ctime>
#include <cstdlib>
#include <cmath>
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

extern "C" {
    void advec_mom_kernel_loop107_(double *mom_flux,
                                   double *node_mass_post,
                                   double *node_mass_pre,
                                   double *vel1,
                                   const int* x_max, const int* x_min,
                                   const int* y_max, const int* y_min);
}

#ifndef _2D_1
#define _2D_1 2e4
#define _2D_2 2e4
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

    Func init2_cpu = init_nonzero<2>("init", x_range + 2 >= 8);
    Func zero_cpu = set_zero<2>("zero", x_range + 2 >= 8);
    Func init2_gpu = init2_cpu;
    Func zero_gpu = zero_cpu;
    init2_cpu.compile_jit(cpu_target);
    zero_cpu.compile_jit(cpu_target);
    if (with_gpu)
    {
        init2_gpu.compile_jit(gpu_target);
        zero_gpu.compile_jit(gpu_target);
    }

    // Buffer dimensions based on Fortran array sizes
    Buffer<double,2> mom_flux({x_range + 6, y_range + 6}, "mom_flux"), mom_flux_g(mom_flux);
    Buffer<double,2> node_mass_post({x_range + 6, y_range + 6}, "node_mass_post"), node_mass_post_g(node_mass_post);
    Buffer<double,2> node_mass_pre({x_range + 6, y_range + 6}, "node_mass_pre"), node_mass_pre_g(node_mass_pre);
    
    Buffer<double,2> vel1_base({x_range + 2, y_range + 2}, "vel1");
    Buffer<double,2> vel1_cpu({x_range + 2, y_range + 2}, "vel1");
    Buffer<double,2> vel1_gpu({x_range + 2, y_range + 2}, "vel1");

    // --------------------------- advec_mom_kernel_loop107 kernel --------------------------
    // printf("advec_mom_kernel_loop107 kernel start!\n");
    Func cpu_fn("cpu_fn");
    // Fortran: vel1(j,k) = (vel1(j,k) * node_mass_pre(j,k) + mom_flux(j,k - 1) - mom_flux(j,k)) / node_mass_post(j,k)
    // Index mapping: j maps to j+2, k maps to k+2, k-1 maps to k+1
    cpu_fn(j, k) = (vel1_cpu(j, k) * node_mass_pre(j + 2, k + 2) + 
                    mom_flux(j + 2, k + 1) - mom_flux(j + 2, k + 2)) / 
                   node_mass_post(j + 2, k + 2);
    cpu_fn.parallel(k);
    if (x_range + 2 >= 8) cpu_fn.vectorize(j, 8);
    cpu_fn.compile_jit(cpu_target);

    srand(42);  
    // Initialize input arrays with random data
    for (int y = 0; y < mom_flux.height(); y++) {
        for (int x = 0; x < mom_flux.width(); x++) {
            mom_flux(x, y) = ((((x + y) % 13) + 0.1) / 13.0) * 10.0;
            node_mass_post(x, y) = ((((x + y + 1) % 13) + 0.1) / 13.0) * 10.0 + 1.0; 
            node_mass_pre(x, y) = ((((x + y + 2) % 13) + 0.1) / 13.0) * 10.0;
        }
    }

    // Initialize vel1 arrays
    init2_cpu.realize(vel1_base);
    vel1_cpu.copy_from(vel1_base);

    // Calling baseline Fortran
    double *mom_flux_ = mom_flux.get()->begin();
    double *node_mass_post_ = node_mass_post.get()->begin();
    double *node_mass_pre_ = node_mass_pre.get()->begin();
    double *vel1_ = vel1_base.get()->begin();
    
    advec_mom_kernel_loop107_(mom_flux_, node_mass_post_, node_mass_pre_, vel1_, 
                              &x_max, &x_min, &y_max, &y_min);
    
    // Calling halide cpu
    try {
        cpu_fn.realize(vel1_cpu);
    } catch (RuntimeError &e) {
        std::cerr << e.what();
        return 1;
    }

    double correctness = 0.0;
    int errors = 0;
    
    for (int k_idx = 0; k_idx < y_range + 2; k_idx++) {
        for (int j_idx = 0; j_idx < x_range + 2; j_idx++) {
            int fortran_j = j_idx + x_min;
            int fortran_k = k_idx + y_min;
            int cpp_j = j_idx;
            int cpp_k = k_idx;
            
            double diff = abs(vel1_base(j_idx, k_idx) - vel1_cpu(j_idx, k_idx));
            
            if (diff >= 1e-10)
            {
                if (errors < 10) {
                    // printf("vel1 [%d, %d] = Fortran: %lf | Halide: %lf | diff: %e\n", 
                    //         j_idx, k_idx, vel1_base(j_idx, k_idx), vel1_cpu(j_idx, k_idx), diff);
                }
                errors++;
            }
            correctness += diff;
        }
    }

    // printf("Total errors for vel1: %d, sum of differences: %e\n", errors, correctness);

    int times = 20;
    double cost_time = 0;
    struct timespec t1, t2, elapsed;

    for (int i = 0; i < times; i++)
    {
        clock_gettime(CLOCK_REALTIME, &t1);
        advec_mom_kernel_loop107_(mom_flux_, node_mass_post_, node_mass_pre_, vel1_, 
                                  &x_max, &x_min, &y_max, &y_min);
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("legacy code: %lfms\n", cost_time/times*1000);

    cost_time = 0;
    for (int i = 0; i < times; i++)
    {
        clock_gettime(CLOCK_REALTIME, &t1);
        cpu_fn.realize(vel1_cpu);
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("lifted cpu: %lfms\n", cost_time/times*1000);

    if (with_gpu)
    {
        // building gpu func
        Func gpu_fn("gpu_fn");
        gpu_fn(j, k) = (vel1_gpu(j, k) * node_mass_pre_g(j + 2, k + 2) + 
                        mom_flux_g(j + 2, k + 1) - mom_flux_g(j + 2, k + 2)) / 
                       node_mass_post_g(j + 2, k + 2);
        gpu_fn.gpu_tile(j, k, bj, bk, tj, tk, 8, 8)
              .compile_jit(gpu_target);
        
        // GPU IO initialization
        mom_flux_g.copy_from(mom_flux);
        node_mass_post_g.copy_from(node_mass_post);
        node_mass_pre_g.copy_from(node_mass_pre);
        vel1_gpu.copy_from(vel1_base);
        
        // warmup
        try {
            gpu_fn.realize(vel1_gpu);
        } catch ( RuntimeError &e ) {
            std::cerr << e.what();
            return -1;
        }

        cost_time = 0;
        clock_gettime(CLOCK_REALTIME, &t1);
        for (int i = 0; i < times; i++)
        {
            gpu_fn.realize(vel1_gpu);
        }
        vel1_gpu.copy_to_host();
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time = toSec(elapsed);
        printf("lifted gpu: %lfms\n\n", cost_time/times*1000);
    }

    return 0;
}
