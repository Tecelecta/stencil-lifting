#include <cstdio>
#include <ctime>
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

inline Expr FMonitor(const Expr& x, const Expr& dx, const Expr& dy, 
                     const Expr& alpha, const Expr& beta)
{
    return sqrt(Expr(1.0) + alpha * x * x + beta * (dx * dx + dy * dy));
}

inline Func targetFunction(const std::string& funcName,
                           Buffer<double, 2>& wo,
                           const Buffer<double, 2>& xo,
                           const Buffer<double, 2>& yo,
                           const Buffer<double, 3>& uo,
                           const Expr ifirst0, const Expr ilast0,
                           const Expr ifirst1, const Expr ilast1,
                           const Expr gcw0, const Expr gcw1,
                           const Expr alpha, const Expr beta,
                           const Expr dxi, const Expr det)
{
    Var j("j"), k("k");
    Func target(funcName);

    auto roi = ifirst0 <= j + ifirst0 && j + ifirst0 <= ilast0 && 
               ifirst1 <= k + ifirst1 && k + ifirst1 <= ilast1;

    auto j_fortran = j + ifirst0;
    auto k_fortran = k + ifirst1;

    auto j_xy = j_fortran - (ifirst0 - gcw0);
    auto k_xy = k_fortran - (ifirst1 - gcw1);

    auto j_u = j_fortran - (ifirst0 - gcw0);
    auto k_u = k_fortran - (ifirst1 - gcw1);

    auto xxi = Expr(0.5) * (xo(j_xy + 1, k_xy) - xo(j_xy - 1, k_xy));
    auto yxi = Expr(0.5) * (yo(j_xy + 1, k_xy) - yo(j_xy - 1, k_xy));
    auto xet = Expr(0.5) * (xo(j_xy, k_xy + 1) - xo(j_xy, k_xy - 1));
    auto yet = Expr(0.5) * (yo(j_xy, k_xy + 1) - yo(j_xy, k_xy - 1));
    
    auto area = xxi * yet - xet * yxi;
    auto xix = yet / area;
    auto xiy = -xet / area;
    auto etx = -yxi / area;
    auto ety = xxi / area;

    auto uu = uo(j_u, k_u, 0);
    auto uxi = (uo(j_u + 1, k_u, 0) - uo(j_u - 1, k_u, 0)) / (Expr(2.0) * dxi);
    auto uet = (uo(j_u, k_u + 1, 0) - uo(j_u, k_u - 1, 0)) / (Expr(2.0) * det);
    
    auto ux = uxi * xix + uet * etx;  
    auto uy = uxi * xiy + uet * ety;

    target(j, k) = select(roi,
        FMonitor(uu, uxi, uet, alpha, beta),
        wo(j, k));
    
    return target;
}

extern "C" {
    void computewcnew_(const int* ifirst0, const int* ilast0,
                       const int* ifirst1, const int* ilast1,
                       const int* gcw0, const int* gcw1,
                       const double* alpha, const double* beta,
                       const double* dxi, const double* det,
                       double* xo, double* yo, double* uo, double* wo);
}

int main(int argc, char** argv)
{
    // printf("computewcnew Caller Start!\n");
    
    const int ifirst0 = 2;
    const int ilast0 = 2e4 + 2;
    const int ifirst1 = 2; 
    const int ilast1 = 2e4 + 2;
    const int gcw0 = 2;
    const int gcw1 = 2;
    
    const double alpha = 1.0;
    const double beta = 1.0;
    const double dxi = 0.01;
    const double det = 0.01;

    const int j_range = ilast0 - ifirst0 + 1;
    const int k_range = ilast1 - ifirst1 + 1;
    const int xy_j_range = (ilast0 + 1 + gcw0) - (ifirst0 - gcw0) + 1;
    const int xy_k_range = (ilast1 + 1 + gcw1) - (ifirst1 - gcw1) + 1;
    const int u_j_range = (ilast0 + gcw0) - (ifirst0 - gcw0) + 1;
    const int u_k_range = (ilast1 + gcw1) - (ifirst1 - gcw1) + 1;

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

    Func init2_cpu = init_nonzero<2>("init2", j_range >= 8);
    Func init3_cpu = init_nonzero<3>("init3", j_range >= 8);
    Func zero_cpu = set_zero<2>("zero", j_range >= 8);
    Func init2_gpu = init2_cpu;
    Func init3_gpu = init3_cpu;
    Func zero_gpu = zero_cpu;
    
    init2_cpu.compile_jit(cpu_target);
    init3_cpu.compile_jit(cpu_target);
    zero_cpu.compile_jit(cpu_target);
    if (with_gpu)
    {
        init2_gpu.compile_jit(gpu_target);
        init3_gpu.compile_jit(gpu_target);
        zero_gpu.compile_jit(gpu_target);
    }

    Buffer<double,2> xo({xy_j_range, xy_k_range}, "xo"), xo_g(xo);
    Buffer<double,2> yo({xy_j_range, xy_k_range}, "yo"), yo_g(yo);
    Buffer<double,3> uo({u_j_range, u_k_range, 4}, "uo"), uo_g(uo);
    Buffer<double,2> wo_base({j_range, k_range}, "wo_base");
    Buffer<double,2> wo_cpu({j_range, k_range}, "wo_cpu");
    Buffer<double,2> wo_gpu({j_range, k_range}, "wo_gpu");  

    // --------------------------- computewcnew kernel --------------------------
    // printf("computewcnew kernel start!\n");

    // building CPU stencil
    Func cpu_fn = targetFunction("computewcnew_cpu", 
                                wo_cpu, xo, yo, uo, 
                                Expr(ifirst0), Expr(ilast0), 
                                Expr(ifirst1), Expr(ilast1), 
                                Expr(gcw0), Expr(gcw1),
                                Expr(alpha), Expr(beta),
                                Expr(dxi), Expr(det));
    cpu_fn.parallel(k);
    if (j_range >= 8) cpu_fn.vectorize(j, 8);
    cpu_fn.compile_jit(cpu_target);

    // IO initialization
    init2_cpu.realize(xo);
    init2_cpu.realize(yo); 
    init3_cpu.realize(uo);
    zero_cpu.realize(wo_base);

    // Calling baseline Fortran function
    double *xo_ = xo.get()->begin();
    double *yo_ = yo.get()->begin();
    double *uo_ = uo.get()->begin();
    double *wo_base_ = wo_base.get()->begin();
    computewcnew_(&ifirst0, &ilast0, &ifirst1, &ilast1, 
                  &gcw0, &gcw1, &alpha, &beta, &dxi, &det,
                  xo_, yo_, uo_, wo_base_);
    
    // Calling halide cpu
    try {
        zero_cpu.realize(wo_cpu);
        cpu_fn.realize(wo_cpu);
    } catch (RuntimeError &e) {
        std::cerr << e.what();
        return 1;
    }

    // check result
    double correctness_1 = 0.0;
    int error_count = 0;
    for (int b = 0; b < k_range; b++) {
        for (int a = 0; a < j_range; a++) {
            double diff = abs(wo_base(a, b) - wo_cpu(a, b));
            if (diff >= 1e-3)
            {
                // if (error_count < 10) 
                // {
                //     printf("output [%d, %d] = Fortran: %lf | Halide: %lf | Diff: %lf\n", 
                //             a, b, wo_base(a, b), wo_cpu(a, b), diff);
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
        computewcnew_(&ifirst0, &ilast0, &ifirst1, &ilast1, 
                      &gcw0, &gcw1, &alpha, &beta, &dxi, &det,
                      xo_, yo_, uo_, wo_base_);
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
        cpu_fn.realize(wo_cpu);
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("lifted cpu: %lfms\n", cost_time/times*1000);

    if (with_gpu)
    {
        // building gpu func
        Func gpu_fn = targetFunction("computewcnew_gpu", 
                                    wo_gpu, xo_g, yo_g, uo_g, 
                                    Expr(ifirst0), Expr(ilast0), 
                                    Expr(ifirst1), Expr(ilast1), 
                                    Expr(gcw0), Expr(gcw1),
                                    Expr(alpha), Expr(beta),
                                    Expr(dxi), Expr(det));
        gpu_fn.gpu_tile(j, k, bj, bk, tj, tk, 8, 8)
              .compile_jit(gpu_target);
        
        // GPU IO initialization
        init2_gpu.realize(xo_g);
        init2_gpu.realize(yo_g);
        init3_gpu.realize(uo_g);
        
        // warmup
        try {
            zero_gpu.realize(wo_gpu);
            gpu_fn.realize(wo_gpu);
        } catch (RuntimeError &e) {
            std::cerr << e.what();
            return -1;
        }

        // GPU timing
        cost_time = 0;
        clock_gettime(CLOCK_REALTIME, &t1);
        for (int i = 0; i < times; i++)
        {
            gpu_fn.realize(wo_gpu);
        }
        wo_gpu.copy_to_host();
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time = toSec(elapsed);
        printf("lifted gpu: %lfms\n\n", cost_time/times*1000);
    }

    return 0;
} 