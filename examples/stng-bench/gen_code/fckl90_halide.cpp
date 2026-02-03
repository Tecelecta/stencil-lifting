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

void flux_calc_kernel_loop90_halide(Func& vol_flux_y_func,
                                   Buffer<double, 2> yarea,
                                   Buffer<double, 2> yvel0,
                                   Buffer<double, 2> yvel1,
                                   double dt,
                                   int x_min, int x_max, int y_min, int y_max) {
    Var j("j"), k("k");
    Expr j_offset = Expr(2); // x_min-2
    Expr k_offset = Expr(2); // y_min-2
    auto j_buf = j + j_offset;
    auto k_buf = k + k_offset;
    
    // Fortran: vol_flux_y(j,k) = 0.25 * dt * yarea(j,k) * (yvel0(j,k) + yvel0(j + 1,k) + yvel1(j,k) + yvel1(j + 1,k))
    vol_flux_y_func(j, k) = Expr(0.25) * Expr(dt) * yarea(j_buf, k_buf) * 
                            (yvel0(j_buf, k_buf) + yvel0(j_buf + 1, k_buf) + 
                             yvel1(j_buf, k_buf) + yvel1(j_buf + 1, k_buf));
}

extern "C" {
    void flux_calc_kernel_loop90_(int* j, int* k, double* dt, double* vol_flux_y,
                                   int* x_max, int* x_min, int* y_max, int* y_min,
                                   double* yarea, double* yvel0, double* yvel1);
}


#ifndef _2D_1
#define _2D_1 2e4
#define _2D_2 2e4
#endif

int main(int argc, char** argv)
{
    const int x_min = 0;
    const int x_max = _2D_1;
    const int y_min = 0;
    const int y_max = _2D_2;
    const double dt = 0.1;

    // Calculate array sizes based on Fortran dimensions
    const int vol_flux_y_x_size = (x_max + 2) - (x_min - 2) + 1;  // (x_min-2):(x_max+2)
    const int vol_flux_y_y_size = (y_max + 3) - (y_min - 2) + 1;  // (y_min-2):(y_max+3)
    const int yvel_x_size = (x_max + 3) - (x_min - 2) + 1;  // (x_min-2):(x_max+3)
    const int yvel_y_size = (y_max + 3) - (y_min - 2) + 1;  // (y_min-2):(y_max+3)
    
    // Calculate output sizes for Halide functions
    const int output_j_size = x_max - x_min + 1;  // x_min:x_max
    const int output_k_size = (y_max + 1) - y_min + 1;  // y_min:y_max+1

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

    Func init2_cpu = init_nonzero<2>("init", vol_flux_y_x_size >= 8);
    Func zero_cpu = set_zero<2>("zero", vol_flux_y_x_size >= 8);
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
    // vol_flux_y: (x_min - 2):(x_max + 2), (y_min - 2):(y_max + 3)
    Buffer<double,2> vol_flux_y({vol_flux_y_x_size, vol_flux_y_y_size}, "vol_flux_y"), vol_flux_y_g(vol_flux_y);
    
    // yarea: (x_min - 2):(x_max + 2), (y_min - 2):(y_max + 3)
    Buffer<double,2> yarea({vol_flux_y_x_size, vol_flux_y_y_size}, "yarea"), yarea_g(yarea);
    
    // yvel0, yvel1: (x_min - 2):(x_max + 3), (y_min - 2):(y_max + 3)
    Buffer<double,2> yvel0({yvel_x_size, yvel_y_size}, "yvel0"), yvel0_g(yvel0);
    Buffer<double,2> yvel1({yvel_x_size, yvel_y_size}, "yvel1"), yvel1_g(yvel1);

    // Separate buffers for Halide output
    Buffer<double,2> vol_flux_y_cpu({output_j_size, output_k_size}, "vol_flux_y_cpu");
    Buffer<double,2> vol_flux_y_gpu({output_j_size, output_k_size}, "vol_flux_y_gpu");

    // Define Halide functions
    Func vol_flux_y_func_cpu("vol_flux_y_func_cpu");
    
    flux_calc_kernel_loop90_halide(vol_flux_y_func_cpu, yarea, yvel0, yvel1, dt,
                                   x_min, x_max, y_min, y_max);

    vol_flux_y_func_cpu.parallel(k);
    if (output_j_size >= 8) vol_flux_y_func_cpu.vectorize(j, 8);
    vol_flux_y_func_cpu.compile_jit(cpu_target);

    // Initialize input arrays
    init2_cpu.realize(yarea);
    init2_cpu.realize(yvel0);
    init2_cpu.realize(yvel1);
    
    // Create separate buffer for Fortran output
    Buffer<double,2> vol_flux_y_base({vol_flux_y_x_size, vol_flux_y_y_size}, "vol_flux_y_base");
    zero_cpu.realize(vol_flux_y_base);

    // Call Fortran baseline
    int j_val = 0, k_val = 0;
    double *vol_flux_y_ = vol_flux_y_base.get()->begin();
    double *yarea_ = yarea.get()->begin();
    double *yvel0_ = yvel0.get()->begin();
    double *yvel1_ = yvel1.get()->begin();
    
    flux_calc_kernel_loop90_(&j_val, &k_val, const_cast<double*>(&dt), vol_flux_y_, 
                              const_cast<int*>(&x_max), const_cast<int*>(&x_min), 
                              const_cast<int*>(&y_max), const_cast<int*>(&y_min), 
                              yarea_, yvel0_, yvel1_);

    // Call Halide CPU
    try {
        zero_cpu.realize(vol_flux_y_cpu);
        vol_flux_y_func_cpu.realize(vol_flux_y_cpu);
    } catch (RuntimeError &e) {
        std::cerr << e.what();
        return 1;
    }

    // Verification
    double correctness = 0.0;
    int errors = 0;
    
    for (int k_idx = 0; k_idx < output_k_size; k_idx++) {
        for (int j_idx = 0; j_idx < output_j_size; j_idx++) {
            int fortran_array_j = j_idx + 2;
            int fortran_array_k = k_idx + 2;
            
            double diff = abs(vol_flux_y_base(fortran_array_j, fortran_array_k) - vol_flux_y_cpu(j_idx, k_idx));
            
            if (diff >= 1e-10)
            {
                if (errors < 10) {
                    printf("vol_flux_y [%d, %d] = Fortran: %lf | Halide: %lf | diff: %e\n", 
                            fortran_array_j, fortran_array_k, vol_flux_y_base(fortran_array_j, fortran_array_k), 
                            vol_flux_y_cpu(j_idx, k_idx), diff);
                }
                errors++;
            }
            correctness += diff;
        }
    }

    // Performance benchmarking
    int times = 5;
    double cost_time = 0;
    struct timespec t1, t2, elapsed;

    // Legacy Fortran timing
    for (int i = 0; i < times; i++)
    {
        zero_cpu.realize(vol_flux_y_base);
        
        clock_gettime(CLOCK_REALTIME, &t1);
        flux_calc_kernel_loop90_(&j_val, &k_val, const_cast<double*>(&dt), vol_flux_y_, 
                                  const_cast<int*>(&x_max), const_cast<int*>(&x_min), 
                                  const_cast<int*>(&y_max), const_cast<int*>(&y_min), 
                                  yarea_, yvel0_, yvel1_);
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("legacy code: %lfms\n", cost_time/times*1000);

    // Halide CPU timing
    cost_time = 0;
    for (int i = 0; i < times; i++)
    {
        zero_cpu.realize(vol_flux_y_cpu);
        
        clock_gettime(CLOCK_REALTIME, &t1);
        vol_flux_y_func_cpu.realize(vol_flux_y_cpu);
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("lifted cpu: %lfms\n", cost_time/times*1000);

    // GPU implementation
    if (with_gpu)
    {
        Func vol_flux_y_func_gpu("vol_flux_y_func_gpu");
        
        flux_calc_kernel_loop90_halide(vol_flux_y_func_gpu, yarea_g, yvel0_g, yvel1_g, dt,
                                       x_min, x_max, y_min, y_max);
        
        vol_flux_y_func_gpu.gpu_tile(j, k, bj, bk, tj, tk, 8, 8)
                           .compile_jit(gpu_target);
        
        // GPU IO initialization
        vol_flux_y_g.copy_from(vol_flux_y);
        yarea_g.copy_from(yarea);
        yvel0_g.copy_from(yvel0);
        yvel1_g.copy_from(yvel1);
        
        zero_gpu.realize(vol_flux_y_g);
        
        try {
            vol_flux_y_func_gpu.realize(vol_flux_y_gpu);
        } catch ( RuntimeError &e ) {
            std::cerr << e.what();
            return -1;
        }

        cost_time = 0;
        times = 100;
        clock_gettime(CLOCK_REALTIME, &t1);
        for (int i = 0; i < times; i++)
        {
            vol_flux_y_func_gpu.realize(vol_flux_y_gpu);
        }
        vol_flux_y_gpu.copy_to_host();
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time = toSec(elapsed);
        printf("lifted gpu: %lfms\n\n", cost_time/times*1000);
    }

    return 0;
} 