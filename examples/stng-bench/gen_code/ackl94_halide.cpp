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

inline Func targetFunctionPre(const std::string& funcName,
                              Buffer<double, 2>& pre_vol,
                              const Buffer<double, 2>& volume,
                              const Buffer<double, 2>& vol_flux_x,
                              const Buffer<double, 2>& vol_flux_y,
                              const Expr x_min, const Expr x_max,
                              const Expr y_min, const Expr y_max)
{
    Var j("j"), k("k");
    Func target(funcName);
    auto roi = (x_min - 2) <= j && j <= (x_max + 2) && 
               (y_min - 2) <= k && k <= (y_max + 2);
    // Fortran: pre_vol(j,k) = volume(j,k) + (vol_flux_y(j,k + 1) - vol_flux_y(j,k) + vol_flux_x(j + 1,k) - vol_flux_x(j,k))
    target(j, k) = select(roi, 
        volume(j - x_min + 2, k - y_min + 2) + 
        (vol_flux_y(j - x_min + 2, k - y_min + 2 + 1) - vol_flux_y(j - x_min + 2, k - y_min + 2) + 
         vol_flux_x(j - x_min + 2 + 1, k - y_min + 2) - vol_flux_x(j - x_min + 2, k - y_min + 2)),
        Expr(0.0));
    return target;
}

inline Func targetFunctionPost(const std::string& funcName,
                               Buffer<double, 2>& post_vol,
                               const Buffer<double, 2>& pre_vol_halide,
                               const Buffer<double, 2>& vol_flux_y,
                               const Expr x_min, const Expr x_max,
                               const Expr y_min, const Expr y_max)
{
    Var j("j"), k("k");
    Func target(funcName);
    auto roi = (x_min - 2) <= j && j <= (x_max + 2) && 
               (y_min - 2) <= k && k <= (y_max + 2);
    // Fortran: post_vol(j,k) = pre_vol(j,k) - (vol_flux_y(j,k + 1) - vol_flux_y(j,k))
    target(j, k) = select(roi, 
        pre_vol_halide(j - x_min + 2, k - y_min + 2) - 
        (vol_flux_y(j - x_min + 2, k - y_min + 2 + 1) - vol_flux_y(j - x_min + 2, k - y_min + 2)),
        Expr(0.0));
    return target;
}

extern "C" {
    void advec_cell_kernel_loop94_(double *post_vol,
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

    // post_vol, pre_vol: (x_min-2):(x_max+3), (y_min-2):(y_max+3) -> x_range+6, y_range+6
    // vol_flux_x: (x_min-2):(x_max+3), (y_min-2):(y_max+2) -> x_range+6, y_range+5  
    // vol_flux_y: (x_min-2):(x_max+2), (y_min-2):(y_max+3) -> x_range+5, y_range+6
    // volume: (x_min-2):(x_max+2), (y_min-2):(y_max+2) -> x_range+5, y_range+5
    
    Buffer<double,2> vol_flux_x({x_range + 6, y_range + 5}, "vol_flux_x"), vol_flux_x_g(vol_flux_x);
    Buffer<double,2> vol_flux_y({x_range + 5, y_range + 6}, "vol_flux_y"), vol_flux_y_g(vol_flux_y);
    Buffer<double,2> volume({x_range + 5, y_range + 5}, "volume"), volume_g(volume);

    Buffer<double,2> post_vol_base({x_range + 6, y_range + 6}, "post_vol");
    Buffer<double,2> pre_vol_base({x_range + 6, y_range + 6}, "pre_vol");
    
    Buffer<double,2> post_vol_cpu({x_range + 5, y_range + 5}, "post_vol");
    Buffer<double,2> pre_vol_cpu({x_range + 5, y_range + 5}, "pre_vol");
    
    Buffer<double,2> post_vol_gpu({x_range + 5, y_range + 5}, "post_vol");
    Buffer<double,2> pre_vol_gpu({x_range + 5, y_range + 5}, "pre_vol");

    // --------------------------- advec_cell_kernel_loop94 kernel --------------------------
    // printf("advec_cell_kernel_loop94 kernel start!\n");

    Func cpu_fn_pre("cpu_fn_pre");
    cpu_fn_pre(j, k) = volume(j, k) + 
                       (vol_flux_y(j, k + 1) - vol_flux_y(j, k) + 
                        vol_flux_x(j + 1, k) - vol_flux_x(j, k));
    cpu_fn_pre.parallel(k);
    if (x_range + 5 >= 8) cpu_fn_pre.vectorize(j, 8);
    cpu_fn_pre.compile_jit(cpu_target);

    Func cpu_fn_post("cpu_fn_post");
    cpu_fn_post(j, k) = pre_vol_cpu(j, k) - 
                        (vol_flux_y(j, k + 1) - vol_flux_y(j, k));
    cpu_fn_post.parallel(k);
    if (x_range + 5 >= 8) cpu_fn_post.vectorize(j, 8);
    cpu_fn_post.compile_jit(cpu_target);

    srand(42);
    for (int y = 0; y < vol_flux_x.height(); y++) {
        for (int x = 0; x < vol_flux_x.width(); x++) {
            vol_flux_x(x, y) = ((((x + y) % 13) + 0.1) / 13.0) * 10.0;
        }
    }
    
    for (int y = 0; y < vol_flux_y.height(); y++) {
        for (int x = 0; x < vol_flux_y.width(); x++) {
            vol_flux_y(x, y) = ((((x + y) % 13) + 0.2) / 13.0) * 10.0;
        }
    }
    
    for (int y = 0; y < volume.height(); y++) {
        for (int x = 0; x < volume.width(); x++) {
            volume(x, y) = ((((x + y) % 13) + 0.3) / 13.0) * 10.0;
        }
    }

    zero_cpu.realize(post_vol_base);
    zero_cpu.realize(pre_vol_base);

    // Calling baseline Fortran
    double *post_vol_ = post_vol_base.get()->begin();
    double *pre_vol_ = pre_vol_base.get()->begin();
    double *vol_flux_x_ = vol_flux_x.get()->begin();
    double *vol_flux_y_ = vol_flux_y.get()->begin();
    double *volume_ = volume.get()->begin();
    
    advec_cell_kernel_loop94_(post_vol_, pre_vol_, 
                              vol_flux_x_, vol_flux_y_, volume_, 
                              &x_max, &x_min, &y_max, &y_min);
    
    // Calling halide cpu
    try {
        zero_cpu.realize(pre_vol_cpu);
        zero_cpu.realize(post_vol_cpu);
        cpu_fn_pre.realize(pre_vol_cpu);
        cpu_fn_post.realize(post_vol_cpu);
    } catch (RuntimeError &e) {
        std::cerr << e.what();
        return 1;
    }

    double correctness_pre = 0.0;
    double correctness_post = 0.0;
    int errors_pre = 0;
    int errors_post = 0;
    
    for (int k_idx = 0; k_idx < y_range + 5; k_idx++) { 
        for (int j_idx = 0; j_idx < x_range + 5; j_idx++) {
            int fortran_j = j_idx - 2;
            int fortran_k = k_idx - 2;
            int cpp_j = fortran_j + 2; 
            int cpp_k = fortran_k + 2; 
            
            double diff_pre = abs(pre_vol_base(cpp_j, cpp_k) - pre_vol_cpu(j_idx, k_idx));
            double diff_post = abs(post_vol_base(cpp_j, cpp_k) - post_vol_cpu(j_idx, k_idx));
            
            if (diff_pre >= 1e-10)
            {
                if (errors_pre < 10) {
                    printf("pre_vol [%d, %d] = Fortran: %lf | Halide: %lf | diff: %e\n", 
                            j_idx, k_idx, pre_vol_base(cpp_j, cpp_k), pre_vol_cpu(j_idx, k_idx), diff_pre);
                }
                errors_pre++;
            }
            if (diff_post >= 1e-10)
            {
                if (errors_post < 10) {
                    printf("post_vol [%d, %d] = Fortran: %lf | Halide: %lf | diff: %e\n", 
                            j_idx, k_idx, post_vol_base(cpp_j, cpp_k), post_vol_cpu(j_idx, k_idx), diff_post);
                }
                errors_post++;
            }
            correctness_pre += diff_pre;
            correctness_post += diff_post;
        }
    }

    // printf("Total errors for pre_vol: %d, sum of differences: %e\n", errors_pre, correctness_pre);
    // printf("Total errors for post_vol: %d, sum of differences: %e\n", errors_post, correctness_post);
    
    // printf("pre_vol check point:\n");
    // printf("%lf\n", pre_vol_cpu(0, 0));
    // printf("%lf\n", pre_vol_cpu(100, 200));
    // printf("%lf\n", pre_vol_cpu(500, 800));
    
    // printf("post_vol check point:\n");
    // printf("%lf\n", post_vol_cpu(0, 0));
    // printf("%lf\n", post_vol_cpu(100, 200));
    // printf("%lf\n", post_vol_cpu(500, 800));

    int times = 5;
    double cost_time = 0;
    struct timespec t1, t2, elapsed;
    clock_gettime(CLOCK_REALTIME, &t1);
    for (int i = 0; i < times; i++)
    {
        clock_gettime(CLOCK_REALTIME, &t1);
        advec_cell_kernel_loop94_(post_vol_, pre_vol_, 
                                  vol_flux_x_, vol_flux_y_, volume_, 
                                  &x_max, &x_min, &y_max, &y_min);
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("legacy code: %lfms\n", cost_time/times*1000);

    cost_time = 0;
    clock_gettime(CLOCK_REALTIME, &t1);
    for (int i = 0; i < times; i++)
    {
        clock_gettime(CLOCK_REALTIME, &t1);
        cpu_fn_pre.realize(pre_vol_cpu);
        cpu_fn_post.realize(post_vol_cpu);
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("lifted cpu: %lfms\n", cost_time/times*1000);

    if (with_gpu)
    {
        // building gpu func
        Func gpu_fn_pre("gpu_fn_pre");
        gpu_fn_pre(j, k) = volume_g(j, k) + 
                           (vol_flux_y_g(j, k + 1) - vol_flux_y_g(j, k) + 
                            vol_flux_x_g(j + 1, k) - vol_flux_x_g(j, k));
        gpu_fn_pre.gpu_tile(j, k, bj, bk, tj, tk, 8, 8)
                  .compile_jit(gpu_target);

        Func gpu_fn_post("gpu_fn_post");
        gpu_fn_post(j, k) = pre_vol_gpu(j, k) - 
                            (vol_flux_y_g(j, k + 1) - vol_flux_y_g(j, k));
        gpu_fn_post.gpu_tile(j, k, bj, bk, tj, tk, 8, 8)
                   .compile_jit(gpu_target);
        
        // GPU IO initialization
        vol_flux_x_g.copy_from(vol_flux_x);
        vol_flux_y_g.copy_from(vol_flux_y);
        volume_g.copy_from(volume);
        
        // warmup
        try {
            zero_gpu.realize(pre_vol_gpu);
            zero_gpu.realize(post_vol_gpu);
            gpu_fn_pre.realize(pre_vol_gpu);
            gpu_fn_post.realize(post_vol_gpu);
        } catch ( RuntimeError &e ) {
            std::cerr << e.what();
            return -1;
        }

        cost_time = 0;
        clock_gettime(CLOCK_REALTIME, &t1);
        for (int i = 0; i < times; i++)
        {
            gpu_fn_pre.realize(pre_vol_gpu);
            gpu_fn_post.realize(post_vol_gpu);
        }
        pre_vol_gpu.copy_to_host();
        post_vol_gpu.copy_to_host();
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time = toSec(elapsed);
        printf("lifted gpu: %lfms\n\n", cost_time/times*1000);
    }

    return 0;
}