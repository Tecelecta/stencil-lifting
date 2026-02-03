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
    void flux_calc_kernel_loop89_(int* j, int* k, double* dt, double* vol_flux_x,
                                   const int* x_max, const int* x_min, double* xarea,
                                   double* xvel0, double* xvel1, const int* y_max, const int* y_min);
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
    const double dt = 0.1;

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
    // vol_flux_x: (x_min - 2):(x_max + 3), (y_min - 2):(y_max + 2)
    Buffer<double,2> vol_flux_x({x_range + 6, y_range + 5}, "vol_flux_x"), vol_flux_x_g(vol_flux_x);
    
    // xarea: (x_min - 2):(x_max + 3), (y_min - 2):(y_max + 2)
    Buffer<double,2> xarea({x_range + 6, y_range + 5}, "xarea"), xarea_g(xarea);
    
    // xvel0, xvel1: (x_min - 2):(x_max + 3), (y_min - 2):(y_max + 3)
    Buffer<double,2> xvel0({x_range + 6, y_range + 6}, "xvel0"), xvel0_g(xvel0);
    Buffer<double,2> xvel1({x_range + 6, y_range + 6}, "xvel1"), xvel1_g(xvel1);

    // --------------------------- flux_calc_kernel_loop89 kernel --------------------------
    // printf("flux_calc_kernel_loop89 kernel start!\n");
    Func cpu_fn("cpu_fn");
    cpu_fn(j, k) = Expr(0.25) * Expr(dt) * xarea(j, k) * 
                   (xvel0(j, k) + xvel0(j, k + 1) + 
                    xvel1(j, k) + xvel1(j, k + 1));
    cpu_fn.parallel(k);
    if (x_range + 2 >= 8) cpu_fn.vectorize(j, 8);
    cpu_fn.compile_jit(cpu_target);

    srand(42);  
    // Initialize input arrays with random data
    init2_cpu.realize(vol_flux_x);
    init2_cpu.realize(xarea);
    // for (int y = 0; y < vol_flux_x.height(); y++) {
    //     for (int x = 0; x < vol_flux_x.width(); x++) {
    //         vol_flux_x(x, y) = 0.0;
    //         xarea(x, y) = ((((x + y) % 13) + 0.1) / 13.0) * 10.0;
    //     }
    // }
    
    init2_cpu.realize(xvel0);
    init2_cpu.realize(xvel1);
    // for (int y = 0; y < xvel0.height(); y++) {
    //     for (int x = 0; x < xvel0.width(); x++) {
    //         xvel0(x, y) = ((((x + y) % 13) + 0.1) / 13.0) * 10.0;
    //         xvel1(x, y) = ((((x + y + 1) % 13) + 0.1) / 13.0) * 10.0;
    //     }
    // }

    // Calling baseline Fortran
    int j_val = 0, k_val = 0;
    double *vol_flux_x_ = vol_flux_x.get()->begin();
    double *xarea_ = xarea.get()->begin();
    double *xvel0_ = xvel0.get()->begin();
    double *xvel1_ = xvel1.get()->begin();
    
    flux_calc_kernel_loop89_(&j_val, &k_val, const_cast<double*>(&dt), vol_flux_x_, 
                              &x_max, &x_min, xarea_, xvel0_, xvel1_, &y_max, &y_min);
    
    // Calling halide cpu
    try {
        cpu_fn.realize(vol_flux_x);
    } catch (RuntimeError &e) {
        std::cerr << e.what();
        return 1;
    }

    double correctness = 0.0;
    int errors = 0;
    
    Buffer<double,2> vol_flux_x_halide({x_range + 6, y_range + 5}, "vol_flux_x_halide");

    // for (int y = 0; y < vol_flux_x_halide.height(); y++) {
    //     for (int x = 0; x < vol_flux_x_halide.width(); x++) {
    //         vol_flux_x_halide(x, y) = 0.0;
    //     }
    // }
    
    try {
        zero_cpu.realize(vol_flux_x_halide);
        cpu_fn.realize(vol_flux_x_halide);
    } catch (RuntimeError &e) {
        std::cerr << e.what();
        return 1;
    }
    
    for (int k_idx = 0; k_idx < y_range + 1; k_idx++) {
        for (int j_idx = 0; j_idx < x_range + 2; j_idx++) {
            int fortran_j = j_idx + x_min;
            int fortran_k = k_idx + y_min;
            int cpp_j = j_idx;
            int cpp_k = k_idx;
            
            int fortran_array_j = j_idx + 2;
            int fortran_array_k = k_idx + 2;
            
            double diff = abs(vol_flux_x(fortran_array_j, fortran_array_k) - vol_flux_x_halide(fortran_array_j, fortran_array_k));
            
            if (diff >= 1e-10)
            {
                if (errors < 10) {
                    printf("vol_flux_x [%d, %d] = Fortran: %lf | Halide: %lf | diff: %e\n", 
                            fortran_array_j, fortran_array_k, vol_flux_x(fortran_array_j, fortran_array_k), 
                            vol_flux_x_halide(fortran_array_j, fortran_array_k), diff);
                }
                errors++;
            }
            correctness += diff;
        }
    }


    int times = 20;
    double cost_time = 0;
    struct timespec t1, t2, elapsed;

    for (int i = 0; i < times; i++)
    {
        // for (int y = 0; y < vol_flux_x.height(); y++) {
        //     for (int x = 0; x < vol_flux_x.width(); x++) {
        //         vol_flux_x(x, y) = 0.0;
        //     }
        // }
        
        clock_gettime(CLOCK_REALTIME, &t1);
        flux_calc_kernel_loop89_(&j_val, &k_val, const_cast<double*>(&dt), vol_flux_x_, 
                                  &x_max, &x_min, xarea_, xvel0_, xvel1_, &y_max, &y_min);
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("legacy code: %lfms\n", cost_time/times*1000);

    cost_time = 0;
    for (int i = 0; i < times; i++)
    {
        // for (int y = 0; y < vol_flux_x_halide.height(); y++) {
        //     for (int x = 0; x < vol_flux_x_halide.width(); x++) {
        //         vol_flux_x_halide(x, y) = 0.0;
        //     }
        // }
        
        clock_gettime(CLOCK_REALTIME, &t1);
        cpu_fn.realize(vol_flux_x_halide);
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("lifted cpu: %lfms\n", cost_time/times*1000);

    if (with_gpu)
    {
        Func gpu_fn("gpu_fn");
        gpu_fn(j, k) = Expr(0.25) * Expr(dt) * xarea_g(j, k) * 
                       (xvel0_g(j, k) + xvel0_g(j, k + 1) + 
                        xvel1_g(j, k) + xvel1_g(j, k + 1));
        gpu_fn.gpu_tile(j, k, bj, bk, tj, tk, 8, 8)
              .compile_jit(gpu_target);
        
        // GPU IO initialization
        vol_flux_x_g.copy_from(vol_flux_x);
        xarea_g.copy_from(xarea);
        xvel0_g.copy_from(xvel0);
        xvel1_g.copy_from(xvel1);
        
        // for (int y = 0; y < vol_flux_x_g.height(); y++) {
        //     for (int x = 0; x < vol_flux_x_g.width(); x++) {
        //         vol_flux_x_g(x, y) = 0.0;
        //     }
        // }
        zero_gpu.realize(vol_flux_x_g);
        
        try {
            gpu_fn.realize(vol_flux_x_g);
        } catch ( RuntimeError &e ) {
            std::cerr << e.what();
            return -1;
        }

        cost_time = 0;
        times = 100;
        clock_gettime(CLOCK_REALTIME, &t1);
        for (int i = 0; i < times; i++)
        {
            gpu_fn.realize(vol_flux_x_g);
        }
        vol_flux_x_g.copy_to_host();
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time = toSec(elapsed);
        printf("lifted gpu: %lfms\n\n", cost_time/times*1000);
    }

    return 0;
} 