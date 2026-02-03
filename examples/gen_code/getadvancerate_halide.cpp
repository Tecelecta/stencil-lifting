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

template<>
inline Func init_nonzero<3>(const std::string& funcName, bool vectorize)
{
    Var d1, d2, d3;
    Func nz(funcName);
    nz(d1, d2, d3) = ((((d1 + d2 + d3) % 13) + Expr(0.1)) / Expr(13.0)) * cast<double>(10);

    return nz;
}

template<>
inline Func init_nonzero<4>(const std::string& funcName, bool vectorize)
{
    Var d1, d2, d3, d4;
    Func nz(funcName);
    nz(d1, d2, d3, d4) = ((((d1 + d2 + d3 + d4) % 13) + Expr(0.1)) / Expr(13.0)) * cast<double>(10);

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

template <>
inline Func set_zero<3>(const std::string& funcName, bool vectorize)
{
    Var d1, d2, d3;
    Func set_zero(funcName);
    set_zero(d1, d2, d3) = Expr(0.0);

    return set_zero;
}

template <>
inline Func set_zero<4>(const std::string& funcName, bool vectorize)
{
    Var d1, d2, d3, d4;
    Func set_zero(funcName);
    set_zero(d1, d2, d3, d4) = Expr(0.0);

    return set_zero;
}

inline Func targetFunction(const std::string& funcName,
                           Buffer<double, 4>& advancerate,
                           const Expr dx0, const Expr dx1, const Expr dx2,
                           const Buffer<double, 4>& source,
                           const Buffer<double, 4>& flux0,
                           const Buffer<double, 4>& flux1,
                           const Buffer<double, 4>& flux2,
                           const Expr ifirst0, const Expr ilast0,
                           const Expr ifirst1, const Expr ilast1,
                           const Expr ifirst2, const Expr ilast2)
{
    Var ic0("ic0"), ic1("ic1"), ic2("ic2"), k("k");
    Func target(funcName);

    auto div0 = -(flux0(ic0 + 1, ic1, ic2, k) - flux0(ic0, ic1, ic2, k)) / dx0;
    
    auto div1 = -(flux1(ic1 + 1, ic2, ic0, k) - flux1(ic1, ic2, ic0, k)) / dx1;
    
    auto div2 = -(flux2(ic2 + 1, ic0, ic1, k) - flux2(ic2, ic0, ic1, k)) / dx2;
    
    target(ic0, ic1, ic2, k) = div0 + div1 + div2 + source(ic0, ic1, ic2, k);
    
    return target;
}

extern "C" {
    void getadvancerate_(const int* ifirst0, const int* ilast0,
                         const int* ifirst1, const int* ilast1,
                         const int* ifirst2, const int* ilast2,
                         double* dx,
                         double* source, double* flux0, double* flux1, double* flux2,
                         double* advancerate);
}

#ifndef _3D_1
#define _3D_1 400
#define _3D_2 400
#define _3D_3 400
#endif

int main(int argc, char** argv)
{
    // printf("getadvancerate Caller Start!\n");
    
    const int ifirst0 = 2;
    const int ilast0 = _3D_1 + 2;
    const int ifirst1 = 2; 
    const int ilast1 = _3D_2 + 2;
    const int ifirst2 = 2;
    const int ilast2 = _3D_3 + 2;

    const int ic0_range = ilast0 - ifirst0 + 1;
    const int ic1_range = ilast1 - ifirst1 + 1;
    const int ic2_range = ilast2 - ifirst2 + 1;
    
    const int flux0_ic0_range = (ilast0 + 1) - ifirst0 + 1;
    const int flux0_ic1_range = ilast1 - ifirst1 + 1;
    const int flux0_ic2_range = ilast2 - ifirst2 + 1;
    
    const int flux1_ic1_range = (ilast1 + 1) - ifirst1 + 1;
    const int flux1_ic2_range = ilast2 - ifirst2 + 1;
    const int flux1_ic0_range = ilast0 - ifirst0 + 1;
    
    const int flux2_ic2_range = (ilast2 + 1) - ifirst2 + 1;
    const int flux2_ic0_range = ilast0 - ifirst0 + 1;
    const int flux2_ic1_range = ilast1 - ifirst1 + 1;

    // --------------------------- Preparation --------------------------
    // printf("Prepare the random data\n");
    bool with_gpu = true;
    
    Var ic0("ic0"), ic1("ic1"), ic2("ic2"), k("k");
    Var bic0, bic1, bic2, tic0, tic1, tic2;

    Target gpu_target = find_gpu_target();
    if (!gpu_target.has_gpu_feature())
    {
        printf("Don't have a gpu!\n");
        with_gpu = false;
    }
    Target cpu_target = get_host_target();

    Func init1_cpu = init_nonzero<1>("init1", false);
    Func init4_cpu = init_nonzero<4>("init4", ic0_range >= 8);
    Func zero4_cpu = set_zero<4>("zero4", ic0_range >= 8);
    Func init1_gpu = init1_cpu;
    Func init4_gpu = init4_cpu;
    Func zero4_gpu = zero4_cpu;
    
    init1_cpu.compile_jit(cpu_target);
    init4_cpu.compile_jit(cpu_target);
    zero4_cpu.compile_jit(cpu_target);
    if (with_gpu)
    {
        init1_gpu.compile_jit(gpu_target);
        init4_gpu.compile_jit(gpu_target);
        zero4_gpu.compile_jit(gpu_target);
    }

    Buffer<double,1> dx(3, "dx"), dx_g(dx);
    Buffer<double,4> flux0({flux0_ic0_range, flux0_ic1_range, flux0_ic2_range, 5}, "flux0"), flux0_g(flux0);
    Buffer<double,4> flux1({flux1_ic1_range, flux1_ic2_range, flux1_ic0_range, 5}, "flux1"), flux1_g(flux1);
    Buffer<double,4> flux2({flux2_ic2_range, flux2_ic0_range, flux2_ic1_range, 5}, "flux2"), flux2_g(flux2);
    Buffer<double,4> source({ic0_range, ic1_range, ic2_range, 5}, "source"), source_g(source);
    Buffer<double,4> advancerate_base({ic0_range, ic1_range, ic2_range, 5}, "advancerate_base");
    Buffer<double,4> advancerate_cpu({ic0_range, ic1_range, ic2_range, 5}, "advancerate_cpu");
    Buffer<double,4> advancerate_gpu({ic0_range, ic1_range, ic2_range, 5}, "advancerate_gpu");  

    // --------------------------- getadvancerate kernel --------------------------
    // printf("getadvancerate kernel start!\n");

    // IO initialization
    init1_cpu.realize(dx);
    init4_cpu.realize(flux0);
    init4_cpu.realize(flux1);
    init4_cpu.realize(flux2);
    init4_cpu.realize(source);
    zero4_cpu.realize(advancerate_base);

    // Extract dx values for use in Halide functions
    double dx0_val = dx(0);
    double dx1_val = dx(1);
    double dx2_val = dx(2);

    // building CPU stencil
    Func cpu_fn = targetFunction("getadvancerate_cpu", 
                                advancerate_cpu, Expr(dx0_val), Expr(dx1_val), Expr(dx2_val),
                                source, flux0, flux1, flux2,
                                Expr(ifirst0), Expr(ilast0), 
                                Expr(ifirst1), Expr(ilast1), 
                                Expr(ifirst2), Expr(ilast2));
    
    Var ic0_o, ic0_i, ic1_o, ic1_i, ic2_o, ic2_i;
    
    if (ic0_range >= 64 && ic1_range >= 64) {
        cpu_fn.tile(ic0, ic1, ic0_o, ic1_o, ic0_i, ic1_i, 32, 32);
        
        cpu_fn.parallel(ic1_o);
        
        if (ic0_range >= 16) {
            cpu_fn.vectorize(ic0_i, 16);
        } else if (ic0_range >= 8) {
            cpu_fn.vectorize(ic0_i, 8);
        }
    } else {
        if (ic2_range >= ic1_range) {
            cpu_fn.parallel(ic2);
        } else {
            cpu_fn.parallel(ic1);
        }
        
        if (ic0_range >= 16) {
            cpu_fn.vectorize(ic0, 16);
        } else if (ic0_range >= 8) {
            cpu_fn.vectorize(ic0, 8);
        }
    }
    
    cpu_fn.compile_jit(cpu_target);

    // Calling baseline Fortran function
    double *dx_ = dx.get()->begin();
    double *flux0_ = flux0.get()->begin();
    double *flux1_ = flux1.get()->begin();
    double *flux2_ = flux2.get()->begin();
    double *source_ = source.get()->begin();
    double *advancerate_base_ = advancerate_base.get()->begin();
    getadvancerate_(&ifirst0, &ilast0, &ifirst1, &ilast1, &ifirst2, &ilast2,
                    dx_, source_, flux0_, flux1_, flux2_, advancerate_base_);
    
    // Calling halide cpu
    try {
        zero4_cpu.realize(advancerate_cpu);
        cpu_fn.realize(advancerate_cpu);
    } catch (RuntimeError &e) {
        std::cerr << e.what();
        return 1;
    }

    // check result
    double correctness_1 = 0.0;
    int error_count = 0;
    for (int c = 0; c < 5; c++) {
        for (int b = 0; b < ic2_range; b++) {
            for (int a = 0; a < ic1_range; a++) {
                for (int i = 0; i < ic0_range; i++) {
                    double diff = abs(advancerate_base(i, a, b, c) - advancerate_cpu(i, a, b, c));
                    if (diff >= 1e-3)
                    {
                        // if (error_count < 10) 
                        // {
                        //     printf("output [%d, %d, %d, %d] = Fortran: %lf | Halide: %lf | Diff: %lf\n", 
                        //             i, a, b, c, advancerate_base(i, a, b, c), advancerate_cpu(i, a, b, c), diff);
                        // }
                        error_count++;
                    }
                    correctness_1 += diff;
                }
            }
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
        getadvancerate_(&ifirst0, &ilast0, &ifirst1, &ilast1, &ifirst2, &ilast2,
                        dx_, source_, flux0_, flux1_, flux2_, advancerate_base_);
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
        cpu_fn.realize(advancerate_cpu);
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("lifted cpu: %lfms\n", cost_time/times*1000);

    if (with_gpu)
    {
        // building gpu func
        Func gpu_fn = targetFunction("getadvancerate_gpu", 
                                    advancerate_gpu, Expr(dx0_val), Expr(dx1_val), Expr(dx2_val),
                                    source_g, flux0_g, flux1_g, flux2_g,
                                    Expr(ifirst0), Expr(ilast0), 
                                    Expr(ifirst1), Expr(ilast1), 
                                    Expr(ifirst2), Expr(ilast2));
        gpu_fn.gpu_tile(ic0, ic1, bic0, bic1, tic0, tic1, 8, 8)
              .compile_jit(gpu_target);
        
        // GPU IO initialization
        init1_gpu.realize(dx_g);
        init4_gpu.realize(flux0_g);
        init4_gpu.realize(flux1_g);
        init4_gpu.realize(flux2_g);
        init4_gpu.realize(source_g);
        
        // warmup
        try {
            zero4_gpu.realize(advancerate_gpu);
            gpu_fn.realize(advancerate_gpu);
        } catch (RuntimeError &e) {
            std::cerr << e.what();
            return -1;
        }

        // GPU timing
        cost_time = 0;
        clock_gettime(CLOCK_REALTIME, &t1);
        for (int i = 0; i < times; i++)
        {
            gpu_fn.realize(advancerate_gpu);
        }
        advancerate_gpu.copy_to_host();
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time = toSec(elapsed);
        printf("lifted gpu: %lfms\n\n", cost_time/times*1000);
    }

    return 0;
} 