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

template<>
inline Func init_nonzero<3>(const std::string& funcName, bool vectorize)
{
    Var d1, d2, d3;
    Func nz(funcName);
    nz(d1, d2, d3) = ((((d1 + d2 + d3) % 13) + Expr(0.1)) / Expr(13.0)) * cast<double>(10);
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

template <>
inline Func set_zero<3>(const std::string& funcName, bool vectorize)
{
    Var d1, d2, d3;
    Func set_zero(funcName);
    set_zero(d1, d2, d3) = Expr(0.0);
    set_zero.parallel(d3);
    if (vectorize) set_zero.vectorize(d1, 8);
    return set_zero;
}

extern "C" {
    void mg_loop15_(const int* i1, const int* i2, const int* i3,
                    const int* n1, const int* n2, const int* n3,
                    double *z);
}

int main(int argc, char** argv)
{
    // printf("Caller Start!\n");
    const int n1 = 1024;
    const int n2 = 1024;
    const int n3 = 1024;

    // --------------------------- Preparation --------------------------
    // printf("Prepare the random data\n");
    bool with_gpu = true;
    
    Var i1("i1"), i2("i2"), i3("i3");
    Var bi1, bi2, bi3, ti1, ti2, ti3;

    Target gpu_target = find_gpu_target();
    if (!gpu_target.has_gpu_feature())
    {
        printf("Don't have a gpu!\n");
        with_gpu = false;
    }
    Target cpu_target = get_host_target();

    Func init3_cpu = init_nonzero<3>("init", n1 >= 8);
    Func zero3_cpu = set_zero<3>("zero", n1 >= 8);
    Func init3_gpu = init3_cpu;
    Func zero3_gpu = zero3_cpu;
    init3_cpu.compile_jit(cpu_target);
    zero3_cpu.compile_jit(cpu_target);
    if (with_gpu)
    {
        init3_gpu.compile_jit(gpu_target);
        zero3_gpu.compile_jit(gpu_target);
    }

    // z: (n1, n2, n3)
    Buffer<double,3> z_base({n1, n2, n3}, "z_base");
    Buffer<double,3> z_cpu({n1, n2, n3}, "z_cpu");
    Buffer<double,3> z_gpu({n1, n2, n3}, "z_gpu");

    // --------------------------- mg_loop15 kernel --------------------------
    // printf("mg_loop15 kernel start!\n");

    Func cpu_fn("cpu_fn");
    cpu_fn(i1, i2, i3) = Expr(0.0);
    cpu_fn.parallel(i3);
    if (n1 >= 8) cpu_fn.vectorize(i1, 8);
    cpu_fn.compile_jit(cpu_target);

    init3_cpu.realize(z_base);

    // Calling baseline Fortran
    double *z_ = z_base.get()->begin();
    int unused_i1 = 0, unused_i2 = 0, unused_i3 = 0;
    mg_loop15_(&unused_i1, &unused_i2, &unused_i3, &n1, &n2, &n3, z_);
    
    // Calling halide cpu
    try {
        cpu_fn.realize(z_cpu);
    } catch (RuntimeError &e) {
        std::cerr << e.what();
        return 1;
    }

    double correctness = 0.0;
    int errors = 0;
    
    // Check correctness
    for (int i3_idx = 0; i3_idx < n3; i3_idx++) { 
        for (int i2_idx = 0; i2_idx < n2; i2_idx++) {
            for (int i1_idx = 0; i1_idx < n1; i1_idx++) {
                double diff = abs(z_base(i1_idx, i2_idx, i3_idx) - z_cpu(i1_idx, i2_idx, i3_idx));
                if (diff >= 1e-10)
                {
                    if (errors < 10) {
                        printf("z [%d, %d, %d] = Fortran: %lf | Halide: %lf | diff: %e\n", 
                                i1_idx, i2_idx, i3_idx, z_base(i1_idx, i2_idx, i3_idx), z_cpu(i1_idx, i2_idx, i3_idx), diff);
                    }
                    errors++;
                }
                correctness += diff;
            }
        }
    }

    // printf("Total errors: %d, sum of differences: %e\n", errors, correctness);
    
    // printf("z check point:\n");
    // printf("%lf\n", z_cpu(0, 0, 0));
    // printf("%lf\n", z_cpu(100, 100, 100));
    // printf("%lf\n", z_cpu(n1-1, n2-1, n3-1));

    int times = 100;
    double cost_time = 0;
    struct timespec t1, t2, elapsed;
    clock_gettime(CLOCK_REALTIME, &t1);
    for (int i = 0; i < times; i++)
    {
        clock_gettime(CLOCK_REALTIME, &t1);
        init3_cpu.realize(z_base);
        mg_loop15_(&unused_i1, &unused_i2, &unused_i3, &n1, &n2, &n3, z_);
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
        cpu_fn.realize(z_cpu);
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("lifted cpu: %lfms\n", cost_time/times*1000);

    if (with_gpu)
    {
        // building gpu func
        Func gpu_fn("gpu_fn");
        gpu_fn(i1, i2, i3) = Expr(0.0);
        gpu_fn.gpu_tile(i1, i2, i3, bi1, bi2, bi3, ti1, ti2, ti3, 8, 8, 8)
              .compile_jit(gpu_target);
        
        // warmup
        try {
            gpu_fn.realize(z_gpu);
        } catch ( RuntimeError &e ) {
            std::cerr << e.what();
            return -1;
        }

        cost_time = 0;
        clock_gettime(CLOCK_REALTIME, &t1);
        for (int i = 0; i < times; i++)
        {
            gpu_fn.realize(z_gpu);
        }
        z_gpu.copy_to_host();
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time = toSec(elapsed);
        printf("lifted gpu: %lfms\n\n", cost_time/times*1000);
    }

    return 0;
} 