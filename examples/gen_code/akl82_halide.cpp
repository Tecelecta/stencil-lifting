// g++ accelerate_kernel_loop82_halide.cpp -o accelerate_kernel_loop82_halide -I ~/software/Halide-14.0.0-x86-64-linux/include/ -lHalide -ldl -lpthread -O3 -std=c++17
#include <cstdio>
#include <ctime>
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

template<>
inline Func init_nonzero<3>(const std::string& funcName, bool vectorize)
{
    Var d1, d2, d3;
    Func nz(funcName);
    nz(d1, d2, d3) = ((((d1 + d2 + d3) % 13) + Expr(0.1)) / Expr(13.0)) * cast<double>(10);
    nz.parallel(d2);
    nz.parallel(d3);
    if (vectorize) nz.vectorize(d1, 8);
    return nz;
}

template <>
inline Func set_zero<1>(const std::string& funcName, bool vectorize)
{
    Var d;
    Func set_zero(funcName);
    set_zero(d) = Expr(0.0);
    set_zero.parallel(d);
    if (vectorize) set_zero.vectorize(d, 8);
    return set_zero;
}

template <>
inline Func set_zero<2>(const std::string& funcName, bool vectorize)
{
    Var d1, d2;
    Func set_zero(funcName);
    set_zero(d1, d2) = Expr(0.0);
    set_zero.parallel(d1);
    set_zero.parallel(d2);
    if (vectorize) set_zero.vectorize(d1, 8);
    return set_zero;
}

template <>
inline Func set_zero<3>(const std::string& funcName, bool vectorize)
{
    Var d1, d2, d3;
    Func set_zero(funcName);
    set_zero(d1, d2, d3) = Expr(0.0);
    set_zero.parallel(d1);
    set_zero.parallel(d2);
    set_zero.parallel(d3);
    if (vectorize) set_zero.vectorize(d1, 8);
    return set_zero;
}

inline Func targetFunction(const std::string& funcName,
                           Buffer<double, 2>& stepbymass,
                           const Buffer<double, 2>& density0,
                           const Buffer<double, 2>& volume,
                           const Expr dt,
                           const Expr x_min, const Expr x_max,
                           const Expr y_min, const Expr y_max)
{
    Var j("j"), k("k");
    Func target(funcName);
    auto roi = x_min <= j && j <= x_max+1 && 
               y_min <= k && k <= y_max+1;
    
    // Access with offset +2 to match Fortran array indexing pattern
    Expr nodal_mass = (density0(j-1+2, k-1+2) * volume(j-1+2, k-1+2) +
                       density0(j+2, k-1+2) * volume(j+2, k-1+2) +
                       density0(j+2, k+2) * volume(j+2, k+2) +
                       density0(j-1+2, k+2) * volume(j-1+2, k+2)) * Expr(0.25);
    
    target(j, k) = select(roi, 
        Expr(0.5) * dt / nodal_mass,
        stepbymass(j, k));
    return target;
}

extern "C" {
    void accelerate_kernel_loop82_(double *density0, 
                                   double *dt,
                                   double *nodal_mass,
                                   double *stepbymass, 
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
    // const int x_max = 2;
    // const int x_min = 0;
    // const int y_max = 2;
    // const int y_min = 0;

    const int x_range = x_max-x_min;
    const int y_range = y_max-y_min;

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

    Buffer<double,2> density0({x_range+5, y_range+5}, "density0"), density0_g(density0);
    Buffer<double,2> volume({x_range+5, y_range+5}, "volume"), volume_g(volume);

    // baseline output (matching Fortran array size)
    Buffer<double,2> out_base({x_range+5, y_range+6}, "stepbymass");
    // halide cpu output  
    Buffer<double,2> out_cpu({x_range+2, y_range+2}, "stepbymass");
    // halide gpu output
    Buffer<double,2> out_gpu({x_range+2, y_range+2}, "stepbymass");

    // --------------------------- accelerate_kernel_loop82 kernel --------------------------
    // printf("accelerate_kernel_loop82 kernel start!\n");
    double dt = 0.94;
    double nodal_mass = 0.0;

    // building baseline stencil
    Func cpu_fn = targetFunction("accelerate_kernel_loop82_cpu", 
                                out_cpu, density0, volume, 
                                Expr(dt),
                                Expr(x_min), Expr(x_max), 
                                Expr(y_min), Expr(y_max));
    cpu_fn.parallel(k);
    if (x_range + 5 >= 8) cpu_fn.vectorize(j,8);
    cpu_fn.compile_jit(cpu_target);

    // IO initialization
    init2_cpu.realize(density0);
    init2_cpu.realize(volume);
    zero_cpu.realize(out_base);

    // Calling baseline
    double *density0_ = density0.get()->begin();
    double *volume_ = volume.get()->begin();
    double *stepbymass_ = out_base.get()->begin();
    accelerate_kernel_loop82_(density0_, 
                              &dt, 
                              &nodal_mass,
                              stepbymass_, 
                              volume_,
                              &x_max, &x_min, &y_max, &y_min);
    
    // Calling halide cpu
    try {
        zero_cpu.realize(out_cpu);
        cpu_fn.realize(out_cpu);
    } catch (RuntimeError &e) {
        std::cerr << e.what();
        return 1;
    }

    // check result - following mdel3 pattern with index offset
    double correctness_1 = 0.0;
    for (int b = 0; b < y_range+2; b++) {
        for (int a = 0; a < x_range+2; a++) {
            // Index mapping: Fortran uses (x_min to x_max+1, y_min to y_max+1)
            // which maps to out_base indices (2 to x_range+3, 2 to y_range+3)
            double diff = abs(out_base(a+2, b+2) - out_cpu(a, b));
            if ( diff >= 1e-3 )
            {
                // printf("output1 [%d, %d] = A: %lf | B: %lf\n", 
                //         a, b, out_base(a+2, b+2), out_cpu(a, b));
            }
            correctness_1 += diff;
        }
    }

    // printf("The number for correctness_1 is %d\n",int(correctness_1));

    int times = 5;
    double cost_time = 0;
    struct timespec t1, t2, elapsed;
    clock_gettime(CLOCK_REALTIME, &t1);
    for (int i = 0; i < times; i++)
    {
        clock_gettime(CLOCK_REALTIME, &t1);
        accelerate_kernel_loop82_(density0_, 
                                  &dt, 
                                  &nodal_mass,
                                  stepbymass_, 
                                  volume_,
                                  &x_max, &x_min, &y_max, &y_min);
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("legacy code: %lfms\n",cost_time/times*1000);

    cost_time = 0;
    clock_gettime(CLOCK_REALTIME, &t1);
    for (int i = 0; i < times; i++)
    {
        clock_gettime(CLOCK_REALTIME, &t1);
        cpu_fn.realize(out_cpu);
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("lifted cpu: %lfms\n",cost_time/times*1000);


    if (with_gpu)
    {
        // building gpu func
        Func gpu_fn = targetFunction("accelerate_kernel_loop82_gpu", 
                                    out_cpu, density0_g, volume_g, 
                                    Expr(dt),
                                    Expr(x_min), Expr(x_max), 
                                    Expr(y_min), Expr(y_max));
        // gpu_target.set_feature(Halide::Target::Debug);
        gpu_fn.gpu_tile(j, k, bj, bk, tj, tk, 8, 8)
              .compile_jit(gpu_target);
        
        // GPU IO initialization
        init2_gpu.realize(density0_g);
        init2_gpu.realize(volume_g);
        
        // warmup
        try {
            zero_gpu.realize(out_gpu);
            gpu_fn.realize(out_gpu);
        } catch ( RuntimeError &e ) {
            std::cerr << e.what();
            return -1;
        }

        cost_time = 0;
        clock_gettime(CLOCK_REALTIME, &t1);
        for (int i = 0; i < times; i++)
        {
            gpu_fn.realize(out_gpu);
        }
        out_gpu.copy_to_host();
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time = toSec(elapsed);
        printf("lifted gpu: %lfms\n\n",cost_time/times*1000);
    }

    return 0;
}