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

extern "C" {
    void advec_mom_kernel_loop102_(double *density1,
                                   double *node_mass_post,
                                   double *post_vol,
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

    Func init2_cpu = init_nonzero<2>("init", x_range + 5 >= 8);
    Func zero_cpu = set_zero<2>("zero", x_range + 5 >= 8);
    Func init2_gpu = init2_cpu;
    Func zero_gpu = zero_cpu;
    init2_cpu.compile_jit(cpu_target);
    zero_cpu.compile_jit(cpu_target);
    if (with_gpu)
    {
        init2_gpu.compile_jit(gpu_target);
        zero_gpu.compile_jit(gpu_target);
    }

    
    Buffer<double,2> density1({x_range + 5, y_range + 5}, "density1"), density1_g(density1);
    Buffer<double,2> post_vol({x_range + 6, y_range + 6}, "post_vol"), post_vol_g(post_vol);

    
    Buffer<double,2> node_mass_post_base({x_range + 6, y_range + 6}, "node_mass_post");
    
    Buffer<double,2> node_mass_post_cpu({x_range + 4, y_range + 2}, "node_mass_post");
    
    Buffer<double,2> node_mass_post_gpu({x_range + 4, y_range + 2}, "node_mass_post");

    // --------------------------- advec_mom_kernel_loop102 kernel --------------------------
    // printf("advec_mom_kernel_loop102 kernel start!\n");

    Func cpu_fn("cpu_fn");
    cpu_fn(j, k) = Expr(0.25) * (density1(j + 1, k + 1) * post_vol(j + 1, k + 1) + 
                                  density1(j + 1, k + 2) * post_vol(j + 1, k + 2) + 
                                  density1(j, k + 1) * post_vol(j, k + 1) + 
                                  density1(j, k + 2) * post_vol(j, k + 2));
    cpu_fn.parallel(k);
    if (x_range + 4 >= 8) cpu_fn.vectorize(j, 8);
    cpu_fn.compile_jit(cpu_target);

    srand(42);   
    for (int y = 0; y < density1.height(); y++) {
        for (int x = 0; x < density1.width(); x++) {
            density1(x, y) = ((((x + y) % 13) + 0.1) / 13.0) * 10.0;
        }
    }
    
    for (int y = 0; y < post_vol.height(); y++) {
        for (int x = 0; x < post_vol.width(); x++) {
            post_vol(x, y) = ((((x + y) % 13) + 0.2) / 13.0) * 10.0;
        }
    }

    zero_cpu.realize(node_mass_post_base);

    // Calling baseline Fortran
    double *density1_ = density1.get()->begin();
    double *node_mass_post_ = node_mass_post_base.get()->begin();
    double *post_vol_ = post_vol.get()->begin();
    
    advec_mom_kernel_loop102_(density1_, node_mass_post_, post_vol_, 
                              &x_max, &x_min, &y_max, &y_min);
    
    // Calling halide cpu
    try {
        zero_cpu.realize(node_mass_post_cpu);
        cpu_fn.realize(node_mass_post_cpu);
    } catch (RuntimeError &e) {
        std::cerr << e.what();
        return 1;
    }

  
    double correctness = 0.0;
    int errors = 0;
    
    for (int k_idx = 0; k_idx < y_range + 2; k_idx++) {
        for (int j_idx = 0; j_idx < x_range + 4; j_idx++) {
            int fortran_j = j_idx + x_min - 1;
            int fortran_k = k_idx + y_min;
            int cpp_j = j_idx + 1;
            int cpp_k = k_idx + 2;
            
            double diff = abs(node_mass_post_base(cpp_j, cpp_k) - node_mass_post_cpu(j_idx, k_idx));
            
            // if (diff >= 1e-10)
            // {
            //     if (errors < 10) {
            //         printf("node_mass_post [%d, %d] = Fortran: %lf | Halide: %lf | diff: %e\n", 
            //                 j_idx, k_idx, node_mass_post_base(cpp_j, cpp_k), node_mass_post_cpu(j_idx, k_idx), diff);
            //     }
            //     errors++;
            // }
            correctness += diff;
        }
    }

    // printf("Total errors for node_mass_post: %d, sum of differences: %e\n", errors, correctness);

    int times = 5;
    double cost_time = 0;
    struct timespec t1, t2, elapsed;
    
    for (int i = 0; i < times; i++)
    {
        clock_gettime(CLOCK_REALTIME, &t1);
        advec_mom_kernel_loop102_(density1_, node_mass_post_, post_vol_, 
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
        cpu_fn.realize(node_mass_post_cpu);
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("lifted cpu: %lfms\n", cost_time/times*1000);

    if (with_gpu)
    {
        // building gpu func
        Func gpu_fn("gpu_fn");
        gpu_fn(j, k) = Expr(0.25) * (density1_g(j + 1, k + 1) * post_vol_g(j + 1, k + 1) + 
                                      density1_g(j + 1, k + 2) * post_vol_g(j + 1, k + 2) + 
                                      density1_g(j, k + 1) * post_vol_g(j, k + 1) + 
                                      density1_g(j, k + 2) * post_vol_g(j, k + 2));
        gpu_fn.gpu_tile(j, k, bj, bk, tj, tk, 8, 8)
              .compile_jit(gpu_target);
        
        // GPU IO initialization
        density1_g.copy_from(density1);
        post_vol_g.copy_from(post_vol);
        
        // warmup
        try {
            zero_gpu.realize(node_mass_post_gpu);
            gpu_fn.realize(node_mass_post_gpu);
        } catch ( RuntimeError &e ) {
            std::cerr << e.what();
            return -1;
        }

        cost_time = 0;
        clock_gettime(CLOCK_REALTIME, &t1);
        for (int i = 0; i < times; i++)
        {
            gpu_fn.realize(node_mass_post_gpu);
        }
        node_mass_post_gpu.copy_to_host();
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time = toSec(elapsed);
        printf("lifted gpu: %lfms\n\n", cost_time/times*1000);
    }

    return 0;
} 