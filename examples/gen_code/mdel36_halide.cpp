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
                    Buffer<double, 3> ph,
                    Buffer<double, 3> phb,
                    Buffer<double, 3> wavg,
                    Buffer<double, 3> rvort,
                    Buffer<double, 2> ht) {

    Var i("i"), k("k"), j("j");
    Func res(funcName);
    Func ph_r("ph_remap"), phb_r("phb_remap"), ht_r("ht_remap");
    Expr g(9.81);

    ph_r(i,k,j) = ph(i+1, k, j+1);
    phb_r(i,k,j) = phb(i+1, k, j+1);
    ht_r(i,j) = ht(i+1, j+1);

    auto zl = Expr(.25) * (
        ((ph_r(i  , k  , j  ) + phb_r(i  , k  ,j  )) / g - ht_r(i  , j  )) + 
        ((ph_r(i-1, k  , j  ) + phb_r(i-1, k  ,j  )) / g - ht_r(i-1, j  )) + 
        ((ph_r(i  , k  , j-1) + phb_r(i  , k  ,j-1)) / g - ht_r(i  , j-1)) + 
        ((ph_r(i-1, k  , j-1) + phb_r(i-1, k  ,j-1)) / g - ht_r(i-1, j-1)));

    auto zu = Expr(.25) * (
        ((ph_r(i  , k+1, j  ) + phb_r(i  , k+1,j  )) / g - ht_r(i  , j  )) + 
        ((ph_r(i-1, k+1, j  ) + phb_r(i-1, k+1,j  )) / g - ht_r(i-1, j  )) + 
        ((ph_r(i  , k+1, j-1) + phb_r(i  , k+1,j-1)) / g - ht_r(i  , j-1)) + 
        ((ph_r(i-1, k+1, j-1) + phb_r(i-1, k+1,j-1)) / g - ht_r(i-1, j-1)));

    auto br_val = ( ( wavg(i,k  ,j) * rvort(i,k  ,j) + 
                      wavg(i,k+1,j) * rvort(i,k+1,j) ) * Expr(.5) ) 
                  * ( zu - zl );

    res(i,k,j) = select(
        zl >= Expr(2.) && zu <= Expr(5000.),
        select(
            wavg(i,k,j) > 0 && wavg(i,k+1,j) > 0,
            br_val,
            Expr(0.)
        ),
        Expr(0.)
    );

    return res;
}

extern "C" {
    void cal_helicity_(
        double* uh, 
        const double* ph, const double* phb, const double* ht, 
        const double *wavg, const double* rvort,
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
    const int kts = 0;
    const int kte = _3D_3;

    // const int its = 0;
    // const int ite = 5;
    // const int jts = 0;
    // const int jte = 5;
    // const int kts = 0;
    // const int kte = 5;

    const int d1_range = ite - its; // i: its to ite
    const int d2_range = kte - kts;  // k: kts+1 to ktf
    const int d3_range = jte - jts; // j: j_start to j_end
    
    // Extended ranges for arrays with padding
    const int fn_range = d2_range + 1;     // kts:kte

    const int ph_d1_range = d1_range + 2;
    const int ph_d2_range = d2_range + 2;
    const int ph_d3_range = d3_range + 2;

    const int wr_d1_range = d1_range + 1;
    const int wr_d2_range = d2_range + 2;
    const int wr_d3_range = d3_range + 1;

    const int out_d1_range = d1_range + 1; 
    const int out_d2_range = d2_range + 1; 
    const int out_d3_range = d3_range + 1;

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
    auto i3_cpu = init_nonzero<3>("init", d1_range + 1 >= 8);
    auto i2_cpu = init_nonzero<2>("init", d1_range + 1 >= 8);
    auto z3_cpu = set_zero<3>("zero", d1_range + 1>= 8);
    auto i3_gpu = i3_cpu;
    auto i2_gpu = i2_cpu;
    auto z3_gpu = z3_cpu;

    i3_cpu.compile_jit(cpu_target);
    i2_cpu.compile_jit(cpu_target);
    z3_cpu.compile_jit(cpu_target);
    if ( with_gpu )
    {
        i3_gpu.compile_jit(gpu_target);
        i2_gpu.compile_jit(gpu_target);
        z3_gpu.compile_jit(gpu_target);
    }
    
    // Create input buffers
    Buffer<double, 2> ht({ph_d1_range, ph_d3_range}, "ht");
    
    Buffer<double, 3> ph({ph_d1_range, ph_d2_range, ph_d3_range}, "ph"),
                      phb({ph_d1_range, ph_d2_range, ph_d3_range}, "phb"),
                      wavg({wr_d1_range, wr_d2_range, wr_d3_range}, "wavg"),
                      rvort({wr_d1_range, wr_d2_range, wr_d3_range}, "rvort");

    Buffer<double, 2> ht_g(ht);
    Buffer<double, 3> ph_g(ph), phb_g(phb), wavg_g(wavg), rvort_g(rvort);

    // Fortran output
    Buffer<double, 3> uh_base({out_d1_range, out_d2_range, out_d3_range}, "uh_base"); 

    // Halide CPU output
    Buffer<double, 3> uh_cpu({out_d1_range, out_d2_range, out_d3_range}, "uh_cpu"); 

    // Halide GPU output
    Buffer<double, 3> uh_gpu({out_d1_range, out_d2_range, out_d3_range}, "uh_gpu"); 

    // --------------------------- horizontal_diffusion_u_2 kernel --------------------------
    printf("mdel36 kernel start!\n");

    // Coordinate transformations for proper indexing
 
    auto res_cpu = targetFunction("cal_helicity",
                                  ph, phb, wavg, rvort, ht);

    // CPU optimizations
    res_cpu.parallel(d3);
    if (d1_range >= 8) res_cpu.vectorize(d1,8);
    res_cpu.compile_jit(cpu_target);

    i3_cpu.realize(ph);
    i3_cpu.realize(phb);
    i3_cpu.realize(wavg);
    i3_cpu.realize(rvort);
    i2_cpu.realize(ht);

    z3_cpu.realize(uh_base);

    cal_helicity_(
        uh_base.get()->begin(),
        ph.get()->begin(),
        phb.get()->begin(),
        ht.get()->begin(),
        wavg.get()->begin(),
        rvort.get()->begin(),
        &its, &ite,
        &kts, &kte,
        &jts, &jte
    );

    // CPU execution
    try {
        z3_cpu.realize(uh_cpu);
        res_cpu.realize(uh_cpu);
    } catch ( RuntimeError &e ) {
        std::cerr << e.what();
        return 1;
    }

    double correctness_1 = 0.0;
    for (int j = 0; j < out_d3_range; j++) {
        for (int k = 0; k < out_d2_range; k++) {
            for (int i = 0; i < out_d1_range; i++) {
                int a = i;
                int b = k;
                int c = j;
                auto A = uh_base(a,b,c);
                auto B = uh_cpu(i,k,j);
                double diff1 = abs(A - B);
                if ( diff1 >= 1e-3 )
                {
                    printf("output1 = Fort[%d, %d, %d]: %lf | Hali[%d, %d, %d]: %lf\n", 
                            a, b, c, A, i, k, j, B);
                }
                correctness_1 += diff1;
            }
        }
    }
    printf("The number for correctness_1 is %d\n",int(correctness_1/(out_d1_range*out_d2_range*out_d3_range)));

    int times = 5;
    double cost_time = 0;
    struct timespec t1, t2, elapsed;
    clock_gettime(CLOCK_REALTIME, &t1);
    for (int i = 0; i < times; i++)
    {
        clock_gettime(CLOCK_REALTIME, &t1);
        cal_helicity_(
            uh_base.get()->begin(),
            ph.get()->begin(),
            phb.get()->begin(),
            ht.get()->begin(),
            wavg.get()->begin(),
            rvort.get()->begin(),
            &its, &ite,
            &kts, &kte,
            &jts, &jte
        );
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("legacy code average cost time: %lfms\n\n",cost_time/times*1000);

    cost_time = 0;
    clock_gettime(CLOCK_REALTIME, &t1);
    for (int i = 0; i < times; i++)
    {
        clock_gettime(CLOCK_REALTIME, &t1);
        res_cpu.realize(uh_cpu);
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("lifted cpu average cost time: %lfms\n\n",cost_time/times*1000);

    if ( with_gpu )
    {
        auto res_gpu = targetFunction("cal_helicity",
                                    ph_g, phb_g, wavg_g, rvort_g, ht_g);

        // GPU optimizations
        res_gpu.gpu_tile(d1,d2,d3,d1o,d2o,d3o,d1i,d2i,d3i,8,8,4)
                .compile_jit(gpu_target);
    
        // IO initialization
        i3_gpu.realize(ph_g);
        i3_gpu.realize(phb_g);
        i3_gpu.realize(wavg_g);
        i3_gpu.realize(rvort_g);
        i2_gpu.realize(ht_g);
        
        // GPU warm up
        try {
            z3_gpu.realize(uh_gpu);
            res_gpu.realize(uh_gpu);
        } catch ( RuntimeError &e ) {
            std::cerr << e.what();
            return 2;
        }

        cost_time = 0;
        clock_gettime(CLOCK_REALTIME, &t1);
        for (int i = 0; i < times; i++)
        {
            res_gpu.realize(uh_gpu);
        }
        uh_gpu.copy_to_host();
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
        printf("lifted gpu average cost time: %lfms\n\n",cost_time/times*1000);
    }
    return 0;
} 