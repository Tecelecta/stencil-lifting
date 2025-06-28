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
                           Buffer<double, 2>& wn,
                           const Buffer<double, 2>& wo,
                           const Expr ifirst0, const Expr ilast0,
                           const Expr ifirst1, const Expr ilast1,
                           const Expr gcw0, const Expr gcw1)
{
    Var j("j"), k("k");
    Func target(funcName);

    auto roi = ifirst0 <= j + ifirst0 && j + ifirst0 <= ilast0 && 
               ifirst1 <= k + ifirst1 && k + ifirst1 <= ilast1;
    
    auto j_fortran = j + ifirst0;
    auto k_fortran = k + ifirst1;
    auto j_wo = j_fortran - (ifirst0 - gcw0);
    auto k_wo = k_fortran - (ifirst1 - gcw1);
    
    target(j, k) = select(roi,
        wo(j_wo, k_wo) / Expr(4.0) +
        (wo(j_wo + 1, k_wo) + wo(j_wo - 1, k_wo) +
         wo(j_wo, k_wo + 1) + wo(j_wo, k_wo - 1)) / Expr(8.0) +
        (wo(j_wo + 1, k_wo + 1) + wo(j_wo + 1, k_wo - 1) +
         wo(j_wo - 1, k_wo + 1) + wo(j_wo - 1, k_wo - 1)) / Expr(16.0),
        wn(j, k));
    
    return target;
}

extern "C" {
    void wcnewiteration_(const int* ifirst0, const int* ilast0,
                         const int* ifirst1, const int* ilast1,
                         const int* gcw0, const int* gcw1,
                         double *wo,
                         double *wn);
}

int main(int argc, char** argv)
{
    // printf("wcnewiteration Caller Start!\n");
    
    const int ifirst0 = 2;
    const int ilast0 = 2e4 + 2;
    const int ifirst1 = 2; 
    const int ilast1 = 2e4 + 2;
    const int gcw0 = 2;
    const int gcw1 = 2;

    const int j_range = ilast0 - ifirst0 + 1;
    const int k_range = ilast1 - ifirst1 + 1;
    const int wo_j_range = (ilast0 + gcw0) - (ifirst0 - gcw0) + 1;
    const int wo_k_range = (ilast1 + gcw1) - (ifirst1 - gcw1) + 1;

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

    Func init2_cpu = init_nonzero<2>("init", j_range >= 8);
    Func zero_cpu = set_zero<2>("zero", j_range >= 8);
    Func init2_gpu = init2_cpu;
    Func zero_gpu = zero_cpu;
    init2_cpu.compile_jit(cpu_target);
    zero_cpu.compile_jit(cpu_target);
    if (with_gpu)
    {
        init2_gpu.compile_jit(gpu_target);
        zero_gpu.compile_jit(gpu_target);
    }

    Buffer<double,2> wo({wo_j_range, wo_k_range}, "wo"), wo_g(wo);
    Buffer<double,2> wn_base({j_range, k_range}, "wn_base");
    Buffer<double,2> wn_cpu({j_range, k_range}, "wn_cpu");
    Buffer<double,2> wn_gpu({j_range, k_range}, "wn_gpu");  

    // --------------------------- wcnewiteration kernel --------------------------
    // printf("wcnewiteration kernel start!\n");

    // building baseline stencil
    Func cpu_fn = targetFunction("wcnewiteration_cpu", 
                                wn_cpu, wo, 
                                Expr(ifirst0), Expr(ilast0), 
                                Expr(ifirst1), Expr(ilast1), 
                                Expr(gcw0), Expr(gcw1));
    cpu_fn.parallel(k);
    if (j_range >= 8) cpu_fn.vectorize(j, 8);
    cpu_fn.compile_jit(cpu_target);

    // IO initialization
    init2_cpu.realize(wo);
    zero_cpu.realize(wn_base);

    // Calling baseline Fortran function
    double *wo_ = wo.get()->begin();
    double *wn_base_ = wn_base.get()->begin();
    wcnewiteration_(&ifirst0, &ilast0, &ifirst1, &ilast1, 
                    &gcw0, &gcw1, wo_, wn_base_);
    
    // Calling halide cpu
    try {
        zero_cpu.realize(wn_cpu);
        cpu_fn.realize(wn_cpu);
    } catch (RuntimeError &e) {
        std::cerr << e.what();
        return 1;
    }

    // check result
    double correctness_1 = 0.0;
    int error_count = 0;
    for (int b = 0; b < k_range; b++) {
        for (int a = 0; a < j_range; a++) {
            double diff = abs(wn_base(a, b) - wn_cpu(a, b));
            if (diff >= 1e-3)
            {
                // if (error_count < 10) 
                // {
                //     printf("output [%d, %d] = Fortran: %lf | Halide: %lf | Diff: %lf\n", 
                //             a, b, wn_base(a, b), wn_cpu(a, b), diff);
                // }
                error_count++;
            }
            correctness_1 += diff;
        }
    }

    // printf("Total errors: %d, Total difference: %lf\n", error_count, correctness_1);
    // printf("Correctness check: %s\n", (correctness_1 < 1e-6) ? "PASSED" : "FAILED");

    // Performance testing
    int times = 5;
    double cost_time = 0;
    struct timespec t1, t2, elapsed;
    
    // Fortran timing
    for (int i = 0; i < times; i++)
    {
        clock_gettime(CLOCK_REALTIME, &t1);
        wcnewiteration_(&ifirst0, &ilast0, &ifirst1, &ilast1, 
                        &gcw0, &gcw1, wo_, wn_base_);
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("legacy code: %lfms\n", cost_time/times*1000);

    // Halide CPU timing
    cost_time = 0;
    for (int i = 0; i < times; i++)
    {
        clock_gettime(CLOCK_REALTIME, &t1);
        cpu_fn.realize(wn_cpu);
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("lifted cpu: %lfms\n", cost_time/times*1000);

    if (with_gpu)
    {
        // building gpu func
        Func gpu_fn = targetFunction("wcnewiteration_gpu", 
                                    wn_gpu, wo_g, 
                                    Expr(ifirst0), Expr(ilast0), 
                                    Expr(ifirst1), Expr(ilast1), 
                                    Expr(gcw0), Expr(gcw1));
        gpu_fn.gpu_tile(j, k, bj, bk, tj, tk, 8, 8)
              .compile_jit(gpu_target);
        
        // GPU IO initialization
        init2_gpu.realize(wo_g);
        
        // warmup
        try {
            zero_gpu.realize(wn_gpu);
            gpu_fn.realize(wn_gpu);
        } catch (RuntimeError &e) {
            std::cerr << e.what();
            return -1;
        }

        // GPU timing
        cost_time = 0;
        clock_gettime(CLOCK_REALTIME, &t1);
        for (int i = 0; i < times; i++)
        {
            gpu_fn.realize(wn_gpu);
        }
        wn_gpu.copy_to_host();
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time = toSec(elapsed);
        printf("lifted gpu: %lfms\n\n", cost_time/times*1000);
    }

    return 0;
} 