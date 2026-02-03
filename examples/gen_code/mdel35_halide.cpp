#include "Halide.h"
#include <cstdio>
#include <ctime>

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

template<>
inline Func init_nonzero<3>(const std::string& funcName, bool vectorize)
{
    Var d1, d2, d3;
    Func nz(funcName);
    nz(d1, d2, d3) = ((((d1 + d2*10 + d3*100)) + Expr(1e-5)) / Expr(100.0)) * cast<double>(10);

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

Func targetFunction(const std::string& funcName,
                    Buffer<double, 3> hat,
                    Buffer<double, 1> fnm,
                    Buffer<double, 1> fnp,
                    const double cf1,
                    const double cf2,
                    const double cf3,
                    const double cft1,
                    const double cft2,
                    const int jts, const int jte,
                    const int kts, const int kte,
                    const int its, const int ite) {

    Var d1("i"), d2("k"), d3("j");
    Func res(funcName);

    Func hat_c("hat_convert");
    auto hat_b = BoundaryConditions::constant_exterior(hat, 0);
    hat_c(d1,d2,d3) = hat_b(d1+3,d2,d3+3);
    
    Func fnm_c("fnm_convert");
    fnm_c(d2) = fnm(d2);
    
    Func fnp_c("fnp_convert");
    fnp_c(d2) = fnp(d2);

    // ------- result1_func (main hatavg calculation for k=kts+1 to ktf) --------
    // Original: hatavg(i,k,j) = 0.5 * (fnm(k) * (hat(i-1,k,j) + hat(i,k,j)) + fnp(k) * (hat(i-1,k-1,j) + hat(i,k-1,j)))
    Func roi1_func("roi1_func");
    roi1_func(d1,d2,d3) = Expr(0.5) * 
        (fnm_c(d2) * (hat_c(d1-1,d2,d3) + hat_c(d1,d2,d3)) +
         fnp_c(d2) * (hat_c(d1-1,d2-1,d3) + hat_c(d1,d2-1,d3)));

    // ------- result2_func (boundary condition at k=1) --------
    // Original: hatavg(i,1,j) = 0.5 * (cf1*(hat(i-1,1,j)+hat(i,1,j)) + cf2*(hat(i-1,2,j)+hat(i,2,j)) + cf3*(hat(i-1,3,j)+hat(i,3,j)))
    Func roi2_func("roi2_func");
    roi2_func(d1,d2,d3) = Expr(0.5) * 
        (Expr(cf1) * (hat_c(d1-1,d2,d3) + hat_c(d1,d2,d3)) +
         Expr(cf2) * (hat_c(d1-1,d2+1,d3) + hat_c(d1,d2+1,d3)) +
         Expr(cf3) * (hat_c(d1-1,d2+2,d3) + hat_c(d1,d2+2,d3)));
    
    // ------- result3_func (boundary condition at k=kte) --------
    // Original: hatavg(i,kte,j) = 0.5 * (cft1*(hat(i,ktes1,j)+hat(i-1,ktes1,j)) + cft2*(hat(i,ktes2,j)+hat(i-1,ktes2,j)))
    Func roi3_func("roi3_func");
    roi3_func(d1,d2,d3) = Expr(0.5) * 
        (Expr(cft1) * (hat_c(d1,d2,d3) + hat_c(d1-1,d2,d3)) +
         Expr(cft2) * (hat_c(d1,d2-1,d3) + hat_c(d1-1,d2-1,d3)));
    
    auto roi2 = 0 == d2;
    auto roi3 = d2 == kte-kts;
    try {
        res(d1, d2, d3) = select(roi2, roi2_func(d1, d2, d3),
                                select(roi3, roi3_func(d1,d2,d3),
                                        roi1_func(d1,d2,d3)));
    } catch ( CompileError &e ) {
        std::cerr << e.what();
    }
    return res;
}

extern "C" {
    void cal_helicity_(
        double* hatavg, double* hat, double* fnm, double* fnp, 
        const double* cf1, const double* cf2, const double* cf3, 
        const double* cft1, const double* cft2,
        const int* jts, const int* jte, 
        const int* kts, const int* kte, 
        const int* its, const int* ite
    );
}

#ifndef _3D_1
#define _3D_1 1024
#define _3D_2 1024
#define _3D_3 1024
#endif

int main(int argc, char** argv)
{
    const int its = 0;              //changed to 1024
    const int ite = _3D_1;
    const int jts = 0;
    const int jte = _3D_2;
    const int kts = 1;
    const int kte = 1+_3D_3;

    // const int its = 0;
    // const int ite = 5;
    // const int jts = 0;
    // const int jte = 5;
    // const int kts = 1;
    // const int kte = 1+5;

    const int d1_range = ite - its; // i: its to ite
    const int d2_range = kte - kts;  // k: kts+1 to ktf
    const int d3_range = jte - jts; // j: j_start to j_end
    
    // Extended ranges for arrays with padding
    const int hat_d1_range = d1_range + 6; // its-3:ite+2
    const int hat_d2_range = d2_range + 1; // kts:kte
    const int hat_d3_range = d3_range + 6; // jts-3:jte+2
    
    const int fn_range = d2_range + 1;     // kts:kte

    const int out_d1_range = d1_range + 1;
    const int out_d2_range = d2_range + 1;
    const int out_d3_range = d3_range + 1;

    const int roi_d1_range = out_d1_range;     // i-1 not aligned
    const int roi_d2_range = out_d2_range; // k-1 aligned
    const int roi_d3_range = out_d3_range;

    // --------------------------- Preparation --------------------------
    printf("Prepare the random data\n");
    bool with_gpu = true;
    Var d1("i"), d2("k"), d3("j"), d1o, d2o, d3o, d1i, d2i, d3i;

    Target gpu_target = find_gpu_target();
    if (!gpu_target.has_gpu_feature())
    {
        printf("Don't have a gpu!\n");
        with_gpu = false;
    }
    Target cpu_target = get_host_target();

    // Input data generation functions
    auto init3_cpu = init_nonzero<3>("init", d1_range + 1 >= 8);
    auto init1_cpu = init_nonzero<1>("init", d1_range + 1 >= 8);
    auto zero_cpu = set_zero<3>("zero", d1_range + 1>= 8);
    auto init3_gpu = init3_cpu;
    auto init1_gpu = init1_cpu;
    auto zero_gpu = zero_cpu;

    init3_cpu.compile_jit(cpu_target);
    init1_cpu.compile_jit(cpu_target);
    zero_cpu.compile_jit(cpu_target);
    if ( with_gpu )
    {
        init3_gpu.compile_jit(gpu_target);
        init1_gpu.compile_jit(gpu_target);
        zero_gpu.compile_jit(gpu_target);
    }
    
    // Create input buffers
    Buffer<double, 3> hat({hat_d1_range, hat_d2_range, hat_d3_range}, "hat");
    Buffer<double, 1> fnm(fn_range, "fnm");
    Buffer<double, 1> fnp(fn_range, "fnp");

    Buffer<double, 3> hat_g(hat);
    Buffer<double, 1> fnm_g(fnm), fnp_g(fnp);

    // Fortran output
    Buffer<double, 3> out_base({out_d1_range, out_d2_range, out_d3_range}, "hatavg_raw"); // hat

    // Halide CPU output
    Buffer<double, 3> out_cpu({roi_d1_range, roi_d2_range, roi_d3_range}, "hatavg"); // hatavg

    // Halide GPU roiput
    Buffer<double, 3> out_gpu({roi_d1_range, roi_d2_range, roi_d3_range}, "hatavg"); // hatavg

    // --------------------------- horizontal_diffusion_u_2 kernel --------------------------
    printf("mdel35 kernel start!\n");

    // Coordinate transformations for proper indexing

    const double cf1 = .125, cf2 = .375, cf3 = .575, cft1 = .25, cft2 = .75; 
    auto res_cpu = targetFunction("cal_helicity_cpu",
                   hat, fnm, fnp, cf1, cf2, cf3, cft1, cft2,
                   jts, jte, kts, kte, its, ite);

    // CPU optimizations
    res_cpu.parallel(d3);
    if (d1_range >= 8) res_cpu.vectorize(d1,8);
    res_cpu.compile_jit(cpu_target);

    init3_cpu.realize(hat);
    init1_cpu.realize(fnp);
    init1_cpu.realize(fnm);

    zero_cpu.realize(out_base);

    cal_helicity_(
        out_base.get()->begin(),
        hat.get()->begin(),
        fnm.get()->begin(),
        fnp.get()->begin(),
        &cf1, &cf2, &cf3, &cft1, &cft2,
        &jts, &jte,
        &kts, &kte,
        &its, &ite
    );

    // CPU execution
    try {
        zero_cpu.realize(out_cpu);
        res_cpu.realize(out_cpu);
    } catch ( RuntimeError &e ) {
        std::cerr << e.what();
        return 1;
    }

    double correctness_1 = 0.0;
    for (int j = 0; j < roi_d3_range; j++) {
        for (int k = 0; k < roi_d2_range; k++) {
            for (int i = 0; i < roi_d1_range; i++) {
                int a = i;
                int b = k;
                int c = j;
                auto A = out_base(a,b,c);
                auto B = out_cpu(i,k,j);
                double diff1 = abs(A - B);
                correctness_1 += diff1;
            }
        }
    }
    printf("The number for correctness_1 is %d\n",int(correctness_1));

    int times = 5;
    double cost_time = 0;
    struct timespec t1, t2, elapsed;
    clock_gettime(CLOCK_REALTIME, &t1);
    for (int i = 0; i < times; i++)
    {
        clock_gettime(CLOCK_REALTIME, &t1);
        cal_helicity_(
            out_base.get()->begin(),
            hat.get()->begin(),
            fnm.get()->begin(),
            fnp.get()->begin(),
            &cf1, &cf2, &cf3, &cft1, &cft2,
            &jts, &jte,
            &kts, &kte,
            &its, &ite
        );
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("cal_helicity legacy code average cost time: %lfms\n\n",cost_time/times*1000);

    cost_time = 0;
    clock_gettime(CLOCK_REALTIME, &t1);
    for (int i = 0; i < times; i++)
    {
        clock_gettime(CLOCK_REALTIME, &t1);
        res_cpu.realize(out_cpu);
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("cal_helicity lifted cpu average cost time: %lfms\n\n",cost_time/times*1000);



    if ( with_gpu )
    {
        auto res_gpu = targetFunction("cal_helicity_gpu",
                   hat_g, fnm_g, fnp_g, cf1, cf2, cf3, cft1, cft2,
                   jts, jte, kts, kte, its, ite);

        // GPU optimizations
        res_gpu.gpu_tile(d1,d2,d3,d1o,d2o,d3o,d1i,d2i,d3i,8,8,4)
                .compile_jit(gpu_target);
    
        // IO initialization
        init3_gpu.realize(hat_g);
        init1_gpu.realize(fnp_g);
        init1_gpu.realize(fnm_g);
        
        // GPU warm up
        try {
            zero_gpu.realize(out_gpu);
            res_gpu.realize(out_gpu);
        } catch ( RuntimeError &e ) {
            std::cerr << e.what();
            return 2;
        }

        cost_time = 0;
        clock_gettime(CLOCK_REALTIME, &t1);
        for (int i = 0; i < times; i++)
        {
            res_gpu.realize(out_gpu);
        }
        out_gpu.copy_to_host();
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
        printf("cal_helicity lifted gpu average cost time: %lfms\n\n",cost_time/times*1000);
        
    }
    return 0;
} 