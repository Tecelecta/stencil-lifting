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

void advec_mom_kernel_loop106_halide(Func& node_mass_post_func, Func& node_mass_pre_func,
                                    Buffer<double, 2> density1,
                                    Buffer<double, 2> post_vol,
                                    Buffer<double, 2> node_flux,
                                    int x_min, int x_max, int y_min, int y_max) {
    
    Var j("j"), k("k");
    
    Expr j_offset = Expr(2);  // x_min-2 offset, since x_min=0, offset is 2
    Expr k_offset = Expr(1); 
    
    auto j_buf = j + j_offset;
    auto k_buf = k + k_offset;
    
    auto j_buf_minus1 = max(j_buf-1, Expr(0)); 
    auto k_buf_minus1 = max(k_buf-1, Expr(0)); 
    
    auto node_mass_post_result = Expr(0.25) * (
        density1(j_buf, k_buf_minus1) * post_vol(j_buf, k_buf_minus1) +
        density1(j_buf, k_buf) * post_vol(j_buf, k_buf) +
        density1(j_buf_minus1, k_buf_minus1) * post_vol(j_buf_minus1, k_buf_minus1) +
        density1(j_buf_minus1, k_buf) * post_vol(j_buf_minus1, k_buf)
    );
    
    auto is_lower_boundary = k == Expr(0);
    auto is_upper_boundary = k == Expr(y_max - y_min + 3); 
    auto is_boundary = is_lower_boundary || is_upper_boundary;
    
    auto post_with_boundary = select(is_boundary, Expr(0.0), node_mass_post_result);
    node_mass_post_func(j, k) = post_with_boundary;
    
    auto flux_minus = select(k_buf == Expr(1), Expr(1.0), node_flux(j_buf, k_buf-1)); 
    auto flux_current = node_flux(j_buf, k_buf);
    auto node_mass_pre_result = post_with_boundary - flux_minus + flux_current;
    node_mass_pre_func(j, k) = select(is_boundary, Expr(0.0), node_mass_pre_result);
}

extern "C" {
    void advec_mom_kernel_loop106_(
        double* node_mass_post, double* node_mass_pre, double* density1,
        double* post_vol, double* node_flux,
        int* x_max, int* x_min, int* y_max, int* y_min
    );
}

int main(int argc, char** argv)
{
    const int x_min = 0;
    const int x_max = 2e4;
    const int y_min = 0;
    const int y_max = 2e4;
    
    // amkl106的数组维度：所有数组都是 (x_min-2:x_max+3, y_min-2:y_max+3)
    const int array_x_size = (x_max + 3) - (x_min - 2) + 1;
    const int array_y_size = (y_max + 3) - (y_min - 2) + 1;
    
    bool with_gpu = true;
    Var d1("j"), d2("k"), d1o, d2o, d1i, d2i;
    
    Target gpu_target = find_gpu_target();
    if (!gpu_target.has_gpu_feature())
    {
        printf("Don't have a gpu!\n");
        with_gpu = false;
    }
    Target cpu_target = get_host_target();
    
    auto i2_cpu = init_nonzero<2>("init2d", array_x_size >= 8);
    auto z2_cpu = set_zero<2>("zero2d", array_x_size >= 8);
    auto i2_gpu = i2_cpu;
    auto z2_gpu = z2_cpu;
    
    i2_cpu.compile_jit(cpu_target);
    z2_cpu.compile_jit(cpu_target);
    if (with_gpu) {
        i2_gpu.compile_jit(gpu_target);
        z2_gpu.compile_jit(gpu_target);
    }
    
    Buffer<double, 2> density1({array_x_size, array_y_size}, "density1"), density1_g(density1);
    Buffer<double, 2> post_vol({array_x_size, array_y_size}, "post_vol"), post_vol_g(post_vol);
    Buffer<double, 2> node_flux({array_x_size, array_y_size}, "node_flux"), node_flux_g(node_flux);

    Buffer<double, 2> node_mass_post_base({array_x_size, array_y_size}, "node_mass_post_base");
    Buffer<double, 2> node_mass_pre_base({array_x_size, array_y_size}, "node_mass_pre_base");
    
    // j: x_min to x_max+1 (x_max - x_min + 2), k: y_min-1 to y_max+2 (y_max - y_min + 4)
    const int output_j_size = x_max - x_min + 2;
    const int output_k_size = y_max - y_min + 4;
    
    Buffer<double, 2> node_mass_post_cpu({output_j_size, output_k_size}, "node_mass_post_cpu");
    Buffer<double, 2> node_mass_pre_cpu({output_j_size, output_k_size}, "node_mass_pre_cpu");
    
    Buffer<double, 2> node_mass_post_gpu({output_j_size, output_k_size}, "node_mass_post_gpu");
    Buffer<double, 2> node_mass_pre_gpu({output_j_size, output_k_size}, "node_mass_pre_gpu");
    
    printf("kernel start!\n");
    
    Func node_mass_post_func_cpu("node_mass_post_cpu");
    Func node_mass_pre_func_cpu("node_mass_pre_cpu");
    
    advec_mom_kernel_loop106_halide(node_mass_post_func_cpu, node_mass_pre_func_cpu,
                                   density1, post_vol, node_flux,
                                   x_min, x_max, y_min, y_max);
    
    node_mass_post_func_cpu.parallel(d2);
    if (array_x_size >= 8) node_mass_post_func_cpu.vectorize(d1, 8);
    node_mass_post_func_cpu.compile_jit(cpu_target);
    
    node_mass_pre_func_cpu.parallel(d2);
    if (array_x_size >= 8) node_mass_pre_func_cpu.vectorize(d1, 8);
    node_mass_pre_func_cpu.compile_jit(cpu_target);
    
    i2_cpu.realize(density1);
    i2_cpu.realize(post_vol);
    i2_cpu.realize(node_flux);
    
    z2_cpu.realize(node_mass_post_base);
    z2_cpu.realize(node_mass_pre_base);
    
    std::vector<double> density1_f(array_x_size * array_y_size);
    std::vector<double> post_vol_f(array_x_size * array_y_size);
    std::vector<double> node_flux_f(array_x_size * array_y_size);
    std::vector<double> node_mass_post_f(array_x_size * array_y_size, 0.0);
    std::vector<double> node_mass_pre_f(array_x_size * array_y_size, 0.0);
    
    for (int k = 0; k < array_y_size; k++) {
        for (int j = 0; j < array_x_size; j++) {
            int fort_idx = k + j * array_y_size;
            
            density1_f[fort_idx] = density1(j, k);
            post_vol_f[fort_idx] = post_vol(j, k);
            node_flux_f[fort_idx] = node_flux(j, k);
        }
    }
    
    int x_min_i = x_min, x_max_i = x_max, y_min_i = y_min, y_max_i = y_max;
    advec_mom_kernel_loop106_(node_mass_post_f.data(),
                             node_mass_pre_f.data(),
                             density1_f.data(),
                             post_vol_f.data(),
                             node_flux_f.data(),
                             &x_max_i, &x_min_i, &y_max_i, &y_min_i);
    
    for (int k = 0; k < array_y_size; k++) {
        for (int j = 0; j < array_x_size; j++) {
            int fort_idx = k + j * array_y_size;
            node_mass_post_base(j, k) = node_mass_post_f[fort_idx];
            node_mass_pre_base(j, k) = node_mass_pre_f[fort_idx];
        }
    }
    
    try {
        z2_cpu.realize(node_mass_post_cpu);
        z2_cpu.realize(node_mass_pre_cpu);
        node_mass_post_func_cpu.realize(node_mass_post_cpu);
        node_mass_pre_func_cpu.realize(node_mass_pre_cpu);
    } catch (RuntimeError &e) {
        std::cerr << e.what();
        return 1;
    }
    
    double correctness_post = 0.0;
    double correctness_pre = 0.0;
    int count = 0;
    int significant_errors_post = 0;
    int significant_errors_pre = 0;
    
    for (int k = y_min-1; k <= y_max+2; k++) {
        for (int j = x_min; j <= x_max+1; j++) {
            int fort_idx_j = j - (x_min - 2);
            int fort_idx_k = k - (y_min - 2);
            
            int hal_idx_j = j - x_min;
            int hal_idx_k = k - (y_min - 1);
            
            auto post_diff = abs(node_mass_post_base(fort_idx_j, fort_idx_k) - node_mass_post_cpu(hal_idx_j, hal_idx_k));
            auto pre_diff = abs(node_mass_pre_base(fort_idx_j, fort_idx_k) - node_mass_pre_cpu(hal_idx_j, hal_idx_k));
            
            if (post_diff >= 1e-6) {
                significant_errors_post++;
                if (significant_errors_post <= 3) {
                    printf("node_mass_post error [%d, %d]: Fort=%lf | Hali=%lf (diff=%e)\n", 
                           j, k, node_mass_post_base(fort_idx_j, fort_idx_k), node_mass_post_cpu(hal_idx_j, hal_idx_k), post_diff);
                }
            }
            if (pre_diff >= 1e-6) {
                significant_errors_pre++;
                if (significant_errors_pre <= 3) {
                    // printf("node_mass_pre error [%d, %d]: Fort=%lf | Hali=%lf (diff=%e)\n", 
                    //        j, k, node_mass_pre_base(fort_idx_j, fort_idx_k), node_mass_pre_cpu(hal_idx_j, hal_idx_k), pre_diff);
                }
            }
            
            correctness_post += post_diff;
            correctness_pre += pre_diff;
            count++;
        }
    }
    
    
    int times = 20;
    double cost_time = 0;
    struct timespec t1, t2, elapsed;
    
    cost_time = 0;
    for (int i = 0; i < times; i++) {
        clock_gettime(CLOCK_REALTIME, &t1);
        advec_mom_kernel_loop106_(node_mass_post_f.data(),
                                 node_mass_pre_f.data(),
                                 density1_f.data(),
                                 post_vol_f.data(),
                                 node_flux_f.data(),
                                 &x_max_i, &x_min_i, &y_max_i, &y_min_i);
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("legacy code average cost time: %lfms\n\n", cost_time/times*1000);
    
    cost_time = 0;
    for (int i = 0; i < times; i++) {
        clock_gettime(CLOCK_REALTIME, &t1);
        node_mass_post_func_cpu.realize(node_mass_post_cpu);
        node_mass_pre_func_cpu.realize(node_mass_pre_cpu);
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("lifted cpu average cost time: %lfms\n\n", cost_time/times*1000);
    
    if (with_gpu) {
        Func node_mass_post_func_gpu("node_mass_post_gpu");
        Func node_mass_pre_func_gpu("node_mass_pre_gpu");
        
        advec_mom_kernel_loop106_halide(node_mass_post_func_gpu, node_mass_pre_func_gpu,
                                       density1_g, post_vol_g, node_flux_g,
                                       x_min, x_max, y_min, y_max);
        
        node_mass_post_func_gpu.gpu_tile(d1, d2, d1o, d2o, d1i, d2i, 16, 16)
                               .compile_jit(gpu_target);
        node_mass_pre_func_gpu.gpu_tile(d1, d2, d1o, d2o, d1i, d2i, 16, 16)
                              .compile_jit(gpu_target);
            
        i2_gpu.realize(density1_g);
        i2_gpu.realize(post_vol_g);
        i2_gpu.realize(node_flux_g);
        
        try {
            z2_gpu.realize(node_mass_post_gpu);
            z2_gpu.realize(node_mass_pre_gpu);
            node_mass_post_func_gpu.realize(node_mass_post_gpu);
            node_mass_pre_func_gpu.realize(node_mass_pre_gpu);
        } catch (RuntimeError &e) {
            std::cerr << e.what();
            return 2;
        }
        
        cost_time = 0;
        clock_gettime(CLOCK_REALTIME, &t1);
        for (int i = 0; i < times; i++) {
            node_mass_post_func_gpu.realize(node_mass_post_gpu);
            node_mass_pre_func_gpu.realize(node_mass_pre_gpu);
        }
        node_mass_post_gpu.copy_to_host();
        node_mass_pre_gpu.copy_to_host();
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
        printf("lifted gpu average cost time: %lfms\n\n", cost_time/times*1000);
    }
    
    return 0;
} 