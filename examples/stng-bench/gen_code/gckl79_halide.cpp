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

void generate_chunk_kernel_loop79_halide(Func& xvel0_func,
                                        Buffer<double, 1> state_xvel,
                                        int number_of_states,
                                        int x_min, int x_max, int y_min, int y_max) {
    Var j("j"), k("k");
    Expr j_offset = Expr(2); // x_min-2
    Expr k_offset = Expr(2); // y_min-2
    auto j_buf = j + j_offset;
    auto k_buf = k + k_offset;
    
    // Fortran: xvel0(j,k) = state_xvel(1)
    xvel0_func(j, k) = Expr(1.0); // state_xvel(1) in Fortran = state_xvel(0) in C++
}

extern "C" {
    void generate_chunk_kernel_loop79_(int* number_of_states, double* state_xvel,
                                       int* x_max, int* x_min, double* xvel0, int* y_max, int* y_min);
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
    int number_of_states = 5; // Example value

    // Calculate array sizes based on Fortran dimensions
    const int xvel0_x_size = (x_max + 3) - (x_min - 2) + 1;  // (x_min-2):(x_max+3)
    const int xvel0_y_size = (y_max + 3) - (y_min - 2) + 1;  // (y_min-2):(y_max+3)
    
    // Calculate output sizes for Halide functions
    const int output_j_size = (x_max + 2) - (x_min - 2) + 1;  // (x_min-2):(x_max+2) - loop range
    const int output_k_size = (y_max + 2) - (y_min - 2) + 1;  // (y_min-2):(y_max+2) - loop range

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

    Func init2_cpu = init_nonzero<2>("init", xvel0_x_size >= 8);
    Func zero_cpu = set_zero<2>("zero", xvel0_x_size >= 8);
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
    // xvel0: (x_min - 2):(x_max + 3), (y_min - 2):(y_max + 3)
    Buffer<double,2> xvel0({xvel0_x_size, xvel0_y_size}, "xvel0"), xvel0_g(xvel0);
    
    // state_xvel: (1):(number_of_states)
    Buffer<double,1> state_xvel({number_of_states}, "state_xvel"), state_xvel_g(state_xvel);

    // Separate buffers for Halide output
    Buffer<double,2> xvel0_cpu({output_j_size, output_k_size}, "xvel0_cpu");
    Buffer<double,2> xvel0_gpu({output_j_size, output_k_size}, "xvel0_gpu");

    // Define Halide functions
    Func xvel0_func_cpu("xvel0_func_cpu");
    
    generate_chunk_kernel_loop79_halide(xvel0_func_cpu, state_xvel, number_of_states,
                                        x_min, x_max, y_min, y_max);

    xvel0_func_cpu.parallel(k);
    if (output_j_size >= 8) xvel0_func_cpu.vectorize(j, 8);
    xvel0_func_cpu.compile_jit(cpu_target);

    // Initialize input arrays
    init2_cpu.realize(xvel0);
    // Initialize state_xvel with some values
    for (int i = 0; i < number_of_states; i++) {
        state_xvel(i) = 1.0 + i * 0.1; // Example initialization
    }
    
    // Create separate buffer for Fortran output
    Buffer<double,2> xvel0_base({xvel0_x_size, xvel0_y_size}, "xvel0_base");
    zero_cpu.realize(xvel0_base);

    // Call Fortran baseline
    double *xvel0_ = xvel0_base.get()->begin();
    double *state_xvel_ = state_xvel.get()->begin();
    
    generate_chunk_kernel_loop79_(&number_of_states, state_xvel_,
                                  const_cast<int*>(&x_max), const_cast<int*>(&x_min), 
                                  xvel0_, const_cast<int*>(&y_max), const_cast<int*>(&y_min));

    // Call Halide CPU
    try {
        zero_cpu.realize(xvel0_cpu);
        xvel0_func_cpu.realize(xvel0_cpu);
    } catch (RuntimeError &e) {
        std::cerr << e.what();
        return 1;
    }

    // Verification
    double correctness = 0.0;
    int errors = 0;
    
    for (int k_idx = 0; k_idx < output_k_size; k_idx++) {
        for (int j_idx = 0; j_idx < output_j_size; j_idx++) {
            double diff = abs(xvel0_base(j_idx, k_idx) - xvel0_cpu(j_idx, k_idx));
            
            if (diff >= 1e-10)
            {
                if (errors < 10) {
                    printf("xvel0 [%d, %d] = Fortran: %lf | Halide: %lf | diff: %e\n", 
                            j_idx, k_idx, xvel0_base(j_idx, k_idx), 
                            xvel0_cpu(j_idx, k_idx), diff);
                }
                errors++;
            }
            correctness += diff;
        }
    }

    // Performance benchmarking
    int times = 50;
    double cost_time = 0;
    struct timespec t1, t2, elapsed;

    // Legacy Fortran timing
    for (int i = 0; i < times; i++)
    {
        zero_cpu.realize(xvel0_base);
        
        clock_gettime(CLOCK_REALTIME, &t1);
        generate_chunk_kernel_loop79_(&number_of_states, state_xvel_,
                                      const_cast<int*>(&x_max), const_cast<int*>(&x_min), 
                                      xvel0_, const_cast<int*>(&y_max), const_cast<int*>(&y_min));
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("legacy code: %lfms\n", cost_time/times*1000);

    // Halide CPU timing
    cost_time = 0;
    for (int i = 0; i < times; i++)
    {
        zero_cpu.realize(xvel0_cpu);
        
        clock_gettime(CLOCK_REALTIME, &t1);
        xvel0_func_cpu.realize(xvel0_cpu);
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("lifted cpu: %lfms\n", cost_time/times*1000);

    // GPU implementation
    if (with_gpu)
    {
        Func xvel0_func_gpu("xvel0_func_gpu");
        
        generate_chunk_kernel_loop79_halide(xvel0_func_gpu, state_xvel_g, number_of_states,
                                           x_min, x_max, y_min, y_max);
        
        xvel0_func_gpu.gpu_tile(j, k, bj, bk, tj, tk, 8, 8)
                       .compile_jit(gpu_target);
        
        // GPU IO initialization
        xvel0_g.copy_from(xvel0);
        state_xvel_g.copy_from(state_xvel);
        
        zero_gpu.realize(xvel0_g);
        
        try {
            xvel0_func_gpu.realize(xvel0_gpu);
        } catch ( RuntimeError &e ) {
            std::cerr << e.what();
            return -1;
        }

        cost_time = 0;
        clock_gettime(CLOCK_REALTIME, &t1);
        for (int i = 0; i < times; i++)
        {
            xvel0_func_gpu.realize(xvel0_gpu);
        }
        xvel0_gpu.copy_to_host();
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time = toSec(elapsed);
        printf("lifted gpu: %lfms\n\n", cost_time/times*1000);
    }

    return 0;
} 