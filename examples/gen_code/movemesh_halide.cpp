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

inline Func targetFunction_xn(const std::string& funcName,
                             Buffer<double, 2>& xn,
                             const Buffer<double, 2>& xo,
                             const Buffer<double, 2>& wo,
                             const Expr ifirst0, const Expr ilast0,
                             const Expr ifirst1, const Expr ilast1,
                             const Expr gcw0, const Expr gcw1)
{
    Var j("j"), k("k");
    Func target(funcName);
    
    auto xo_offset_j = gcw0;
    auto xo_offset_k = gcw1;
    auto wo_offset_j = gcw0;
    auto wo_offset_k = gcw1;
    
    auto roi = ifirst0 <= j && j <= ilast0+1 && 
               ifirst1 <= k && k <= ilast1+1;

    auto cxp = Expr(0.5) * (wo(j + wo_offset_j, k + wo_offset_k) + wo(j + wo_offset_j, k-1 + wo_offset_k));
    auto cxm = Expr(0.5) * (wo(j-1 + wo_offset_j, k + wo_offset_k) + wo(j-1 + wo_offset_j, k-1 + wo_offset_k));
    auto cyp = Expr(0.5) * (wo(j + wo_offset_j, k + wo_offset_k) + wo(j-1 + wo_offset_j, k + wo_offset_k));
    auto cym = Expr(0.5) * (wo(j + wo_offset_j, k-1 + wo_offset_k) + wo(j-1 + wo_offset_j, k-1 + wo_offset_k));
    
    auto weight_sum = cxp + cxm + cyp + cym;
    
    target(j, k) = select(roi,
        (cxp * xo(j+1 + xo_offset_j, k + xo_offset_k) + cxm * xo(j-1 + xo_offset_j, k + xo_offset_k) +
         cyp * xo(j + xo_offset_j, k+1 + xo_offset_k) + cym * xo(j + xo_offset_j, k-1 + xo_offset_k)) / weight_sum,
        xn(j, k));
    
    return target;
}

inline Func targetFunction_yn(const std::string& funcName,
                             Buffer<double, 2>& yn,
                             const Buffer<double, 2>& yo,
                             const Buffer<double, 2>& wo,
                             const Expr ifirst0, const Expr ilast0,
                             const Expr ifirst1, const Expr ilast1,
                             const Expr gcw0, const Expr gcw1)
{
    Var j("j"), k("k");
    Func target(funcName);
    
    auto yo_offset_j = gcw0;
    auto yo_offset_k = gcw1;
    auto wo_offset_j = gcw0;
    auto wo_offset_k = gcw1;
    
    auto roi = ifirst0 <= j && j <= ilast0+1 && 
               ifirst1 <= k && k <= ilast1+1;
    
    auto cxp = Expr(0.5) * (wo(j + wo_offset_j, k + wo_offset_k) + wo(j + wo_offset_j, k-1 + wo_offset_k));
    auto cxm = Expr(0.5) * (wo(j-1 + wo_offset_j, k + wo_offset_k) + wo(j-1 + wo_offset_j, k-1 + wo_offset_k));
    auto cyp = Expr(0.5) * (wo(j + wo_offset_j, k + wo_offset_k) + wo(j-1 + wo_offset_j, k + wo_offset_k));
    auto cym = Expr(0.5) * (wo(j + wo_offset_j, k-1 + wo_offset_k) + wo(j-1 + wo_offset_j, k-1 + wo_offset_k));
    
    auto weight_sum = cxp + cxm + cyp + cym;
    
    target(j, k) = select(roi,
        (cxp * yo(j+1 + yo_offset_j, k + yo_offset_k) + cxm * yo(j-1 + yo_offset_j, k + yo_offset_k) +
         cyp * yo(j + yo_offset_j, k+1 + yo_offset_k) + cym * yo(j + yo_offset_j, k-1 + yo_offset_k)) / weight_sum,
        yn(j, k));
    
    return target;
}

extern "C" {
    void movemesh_(const int* ifirst0, const int* ilast0,
                   const int* ifirst1, const int* ilast1,
                   const int* gcw0, const int* gcw1,
                   double* xo, double* yo, double* wo,
                   double* xn, double* yn);
}


int main(int argc, char** argv)
{
    // printf("MoveMesh Caller Start!\n");

    const int ifirst0 = 0;
    const int ilast0 = 2e4;   
    const int ifirst1 = 0; 
    const int ilast1 = 2e4;  
    const int gcw0 = 2;
    const int gcw1 = 2;

    const int j_range_in = ilast0 - ifirst0 + 1;
    const int k_range_in = ilast1 - ifirst1 + 1;
    const int j_range_out = ilast0 - ifirst0 + 2;
    const int k_range_out = ilast1 - ifirst1 + 2;

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

    Func init2_cpu = init_nonzero<2>("init", j_range_in + 2*gcw0 >= 8);
    Func zero2_cpu = set_zero<2>("zero", j_range_out >= 8);
    Func init2_gpu = init2_cpu;
    Func zero2_gpu = zero2_cpu;
    init2_cpu.compile_jit(cpu_target);
    zero2_cpu.compile_jit(cpu_target);
    if (with_gpu)
    {
        init2_gpu.compile_jit(gpu_target);
        zero2_gpu.compile_jit(gpu_target);
    }

    Buffer<double,2> xo({j_range_in + 2*gcw0 + 1, k_range_in + 2*gcw1 + 1}, "xo"), xo_g(xo);
    Buffer<double,2> yo({j_range_in + 2*gcw0 + 1, k_range_in + 2*gcw1 + 1}, "yo"), yo_g(yo);
    Buffer<double,2> wo({j_range_in + 2*gcw0, k_range_in + 2*gcw1}, "wo"), wo_g(wo);

    // baseline output
    Buffer<double,2> xn_base({j_range_out, k_range_out}, "xn");
    Buffer<double,2> yn_base({j_range_out, k_range_out}, "yn");
    // halide cpu output
    Buffer<double,2> xn_cpu({j_range_out, k_range_out}, "xn");
    Buffer<double,2> yn_cpu({j_range_out, k_range_out}, "yn");
    // halide gpu output
    Buffer<double,2> xn_gpu({j_range_out, k_range_out}, "xn");
    Buffer<double,2> yn_gpu({j_range_out, k_range_out}, "yn");

    // --------------------------- movemesh kernel --------------------------
    // printf("movemesh kernel start!\n");

    // building CPU stencil
    Func cpu_fn_xn = targetFunction_xn("movemesh_xn_cpu", 
                                      xn_cpu, xo, wo,
                                      Expr(ifirst0), Expr(ilast0), 
                                      Expr(ifirst1), Expr(ilast1), 
                                      Expr(gcw0), Expr(gcw1));
    Func cpu_fn_yn = targetFunction_yn("movemesh_yn_cpu", 
                                      yn_cpu, yo, wo,
                                      Expr(ifirst0), Expr(ilast0), 
                                      Expr(ifirst1), Expr(ilast1), 
                                      Expr(gcw0), Expr(gcw1));
    
    cpu_fn_xn.parallel(k);
    cpu_fn_yn.parallel(k);
    if (j_range_out >= 8) {
        cpu_fn_xn.vectorize(j, 8);
        cpu_fn_yn.vectorize(j, 8);
    }
    cpu_fn_xn.compile_jit(cpu_target);
    cpu_fn_yn.compile_jit(cpu_target);

    // IO initialization
    init2_cpu.realize(xo);
    init2_cpu.realize(yo);
    init2_cpu.realize(wo);
    zero2_cpu.realize(xn_base);
    zero2_cpu.realize(yn_base);

    // Calling baseline
    double *xo_ = xo.get()->begin();
    double *yo_ = yo.get()->begin();
    double *wo_ = wo.get()->begin();
    double *xn_ = xn_base.get()->begin();
    double *yn_ = yn_base.get()->begin();
    
    movemesh_(&ifirst0, &ilast0, &ifirst1, &ilast1,
              &gcw0, &gcw1,
              xo_, yo_, wo_, xn_, yn_);
    
    // Calling halide cpu
    try {
        zero2_cpu.realize(xn_cpu);
        zero2_cpu.realize(yn_cpu);
        cpu_fn_xn.realize(xn_cpu);
        cpu_fn_yn.realize(yn_cpu);
    } catch (RuntimeError &e) {
        std::cerr << e.what();
        return 1;
    }

    // check result for xn
    double correctness_xn = 0.0;
    for (int k = 0; k < k_range_out; k++) {
        for (int j = 0; j < j_range_out; j++) {
            double diff = abs(xn_base(j, k) - xn_cpu(j, k));
            // if (diff >= 1e-3) {
            //     printf("xn [%d, %d] = Fortran: %lf | Halide: %lf\n", 
            //             j, k, xn_base(j, k), xn_cpu(j, k));
            // }
            correctness_xn += diff;
        }
    }

    // check result for yn  
    double correctness_yn = 0.0;
    for (int k = 0; k < k_range_out; k++) {
        for (int j = 0; j < j_range_out; j++) {
            double diff = abs(yn_base(j, k) - yn_cpu(j, k));
            // if (diff >= 1e-3) {
            //     printf("yn [%d, %d] = Fortran: %lf | Halide: %lf\n", 
            //             j, k, yn_base(j, k), yn_cpu(j, k));
            // }
            correctness_yn += diff;
        }
    }

    // printf("The number for correctness_xn is %d\n", int(correctness_xn));
    // printf("The number for correctness_yn is %d\n", int(correctness_yn));

    int times = 5;
    double cost_time = 0;
    struct timespec t1, t2, elapsed;
    
    // Fortran timing
    for (int i = 0; i < times; i++)
    {
        clock_gettime(CLOCK_REALTIME, &t1);
        movemesh_(&ifirst0, &ilast0, &ifirst1, &ilast1,
                  &gcw0, &gcw1,
                  xo_, yo_, wo_, xn_, yn_);
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
        cpu_fn_xn.realize(xn_cpu);
        cpu_fn_yn.realize(yn_cpu);
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("lifted cpu: %lfms\n", cost_time/times*1000);

    if (with_gpu)
    {
        // building gpu func
        Func gpu_fn_xn = targetFunction_xn("movemesh_xn_gpu", 
                                          xn_gpu, xo_g, wo_g,
                                          Expr(ifirst0), Expr(ilast0), 
                                          Expr(ifirst1), Expr(ilast1), 
                                          Expr(gcw0), Expr(gcw1));
        Func gpu_fn_yn = targetFunction_yn("movemesh_yn_gpu", 
                                          yn_gpu, yo_g, wo_g,
                                          Expr(ifirst0), Expr(ilast0), 
                                          Expr(ifirst1), Expr(ilast1), 
                                          Expr(gcw0), Expr(gcw1));
        
        gpu_fn_xn.gpu_tile(j, k, bj, bk, tj, tk, 16, 16).compile_jit(gpu_target);
        gpu_fn_yn.gpu_tile(j, k, bj, bk, tj, tk, 16, 16).compile_jit(gpu_target);
        
        // GPU IO initialization
        init2_gpu.realize(xo_g);
        init2_gpu.realize(yo_g);
        init2_gpu.realize(wo_g);
        
        // warmup
        try {
            zero2_gpu.realize(xn_gpu);
            zero2_gpu.realize(yn_gpu);
            gpu_fn_xn.realize(xn_gpu);
            gpu_fn_yn.realize(yn_gpu);
        } catch ( RuntimeError &e ) {
            std::cerr << e.what();
            return -1;
        }

        cost_time = 0;
        clock_gettime(CLOCK_REALTIME, &t1);
        for (int i = 0; i < times; i++)
        {
            gpu_fn_xn.realize(xn_gpu);
            gpu_fn_yn.realize(yn_gpu);
        }
        xn_gpu.copy_to_host();
        yn_gpu.copy_to_host();
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time = toSec(elapsed);
        printf("lifted gpu: %lfms\n\n", cost_time/times*1000);
        
        // check GPU result consistency with Fortran
        double correctness_gpu_xn = 0.0;
        for (int k = 0; k < k_range_out; k++) {
            for (int j = 0; j < j_range_out; j++) {
                double diff = abs(xn_base(j, k) - xn_gpu(j, k));
                // if (diff >= 1e-3) {
                //     printf("GPU xn [%d, %d] = Fortran: %lf | GPU: %lf\n", 
                //             j, k, xn_base(j, k), xn_gpu(j, k));
                // }
                correctness_gpu_xn += diff;
            }
        }

        // check GPU result for yn  
        double correctness_gpu_yn = 0.0;
        for (int k = 0; k < k_range_out; k++) {
            for (int j = 0; j < j_range_out; j++) {
                double diff = abs(yn_base(j, k) - yn_gpu(j, k));
                // if (diff >= 1e-3) {
                //     printf("GPU yn [%d, %d] = Fortran: %lf | GPU: %lf\n", 
                //             j, k, yn_base(j, k), yn_gpu(j, k));
                // }
                correctness_gpu_yn += diff;
            }
        }

        // printf("The number for GPU correctness_xn is %d\n", int(correctness_gpu_xn));
        // printf("The number for GPU correctness_yn is %d\n", int(correctness_gpu_yn));
    }

    return 0;
} 