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

Func targetFunction(Buffer<double, 3> tke,
                    Buffer<double, 3> BN2,
                    Buffer<double, 3> rdzw,
                    Buffer<double, 2> msftx,
                    Buffer<double, 2> msfty) {

    Var i("i"), k("k"), j("j");

    Func res("calc_l_scale");
    Expr dx(.8), dy(.8);

    auto deltas = pow( dx/msftx(i,j)*dy/msfty(i,j)/rdzw(i,k,j), Expr(1./3) );
    auto tmp = sqrt( max( tke(i,k,j), Expr(1.e-6) ) );
    auto brval_t = Expr(.76) * tmp / sqrt( BN2(i,k,j) );
    brval_t = min(brval_t, deltas);
    brval_t = max(brval_t, Expr(.001)*deltas);

    res(i,k,j) = select(
        BN2(i,k,j) > Expr(1.e-6),
        brval_t,
        deltas
    );
    return res;
}

extern "C" {
    void calc_l_scale_(
        double* l_scale, 
        double* tke, double* BN2, double* rdzw, double* msftx, double* msfty, 
        const int* jts, const int* jte, 
        const int* kts, const int* kte, 
        const int* its, const int* ite
    );
}

int main(int argc, char** argv)
{
    const int its = 0;              //changed to 1024
    const int ite = 1024;
    const int jts = 0;
    const int jte = 1024;
    const int kts = 0;
    const int kte = 1024;

    // const int its = 0;
    // const int ite = 5;
    // const int jts = 0;
    // const int jte = 5;
    // const int kts = 0;
    // const int kte = 5;

    const int d1_range = ite - its; // i: its to ite
    const int d2_range = kte - kts;  // k: kts+1 to ktf
    const int d3_range = jte - jts; // j: j_start to j_end
    
    const int in_d1_range = d1_range + 1;    // Extended for i-1 access
    const int in_d2_range = d2_range + 1;   // Extended for k+1 access
    const int in_d3_range = d3_range + 1;    // Extended for j+1 access

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
    Buffer<double, 3> tke({in_d1_range, in_d2_range, in_d3_range}, "tke"), tke_g(tke);
    Buffer<double, 3> bn2({in_d1_range, in_d2_range, in_d3_range}, "bn2"), bn2_g(bn2);
    Buffer<double, 3> rdzw({in_d1_range, in_d2_range, in_d3_range}, "rdzw"), rdzw_g(rdzw);
    Buffer<double, 2> msftx({in_d1_range, in_d3_range}, "msftx"), msftx_g(msftx);
    Buffer<double, 2> msfty({in_d1_range, in_d3_range}, "msfty"), msfty_g(msfty);

    // Fortran output
    Buffer<double, 3> lscale_base({out_d1_range, out_d2_range, out_d3_range}, "lscale_raw"); // lscale

    // Halide CPU output
    Buffer<double, 3> lscale_cpu({out_d1_range, out_d2_range, out_d3_range}, "lscale"); // lscale

    // Halide GPU roiput
    Buffer<double, 3> lscale_gpu({out_d1_range, out_d2_range, out_d3_range}, "lscale"); // lscale

    // --------------------------- calc_l_scale kernel --------------------------
    printf("calc_l_scale kernel start!\n");

    // Coordinate transformations for proper indexing
    auto res_cpu = targetFunction(tke, bn2, rdzw, msftx, msfty);

    // CPU optimizations
    res_cpu.parallel(d3);
    if (d1_range >= 8) res_cpu.vectorize(d1,8);
    res_cpu.compile_jit(cpu_target);

    i3_cpu.realize(rdzw);
    i3_cpu.realize(tke);
    i3_cpu.realize(tke);
    i2_cpu.realize(msftx);
    i2_cpu.realize(msfty);

    z3_cpu.realize(lscale_base);

    calc_l_scale_(
        lscale_base.get()->begin(), 
        tke.get()->begin(),
        bn2.get()->begin(),
        rdzw.get()->begin(),
        msftx.get()->begin(), 
        msfty.get()->begin(), 
        &its, &ite, &jts, &jte, &kts, &kte
    );

    // CPU execution
    try {
        z3_cpu.realize(lscale_cpu);
        res_cpu.realize(lscale_cpu);
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
                auto A = lscale_base(a,b,c);
                auto B = lscale_cpu(i,k,j);
                double diff1 = abs( A - B );
                if ( diff1 >= 1e-3 )
                {
                    printf("output1 = Fort[%d, %d, %d]: %lf | Hali[%d, %d, %d]: %lf\n", 
                            a, b, c, A, i, k, j, B);
                }
                correctness_1 += diff1;
            }
        }
    }
    printf("The number for correctness_1 is %d\n",int(correctness_1/out_d3_range/out_d2_range/out_d1_range));


    int times = 5;
    double cost_time = 0;
    struct timespec t1, t2, elapsed;
    clock_gettime(CLOCK_REALTIME, &t1);
    for (int i = 0; i < times; i++)
    {
        clock_gettime(CLOCK_REALTIME, &t1);
        calc_l_scale_(
            lscale_base.get()->begin(), 
            tke.get()->begin(),
            bn2.get()->begin(),
            rdzw.get()->begin(),
            msftx.get()->begin(), 
            msfty.get()->begin(), 
            &its, &ite, &jts, &jte, &kts, &kte
        );
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("calc_l_scale legacy code average cost time: %lfms\n\n",cost_time/times*1000);

    cost_time = 0;
    clock_gettime(CLOCK_REALTIME, &t1);
    for (int i = 0; i < times; i++)
    {
        clock_gettime(CLOCK_REALTIME, &t1);
        res_cpu.realize(lscale_cpu);
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("calc_l_scale lifted cpu average cost time: %lfms\n\n",cost_time/times*1000);



    if ( with_gpu )
    {
        auto res_gpu = targetFunction(tke_g, bn2_g, rdzw_g, msftx_g, msfty_g);

        // GPU optimizations
        res_gpu.gpu_tile(d1,d2,d3,d1o,d2o,d3o,d1i,d2i,d3i,8,8,4)
                .compile_jit(gpu_target);

        // IO initialization
        i3_cpu.realize(rdzw_g);
        i3_cpu.realize(tke_g);
        i3_cpu.realize(tke_g);
        i2_cpu.realize(msftx_g);
        i2_cpu.realize(msfty_g);

        // GPU warm up
        try {
            z3_gpu.realize(lscale_gpu);
            res_gpu.realize(lscale_gpu);
        } catch ( RuntimeError &e ) {
            std::cerr << e.what();
            return 2;
        }

        cost_time = 0;
        clock_gettime(CLOCK_REALTIME, &t1);
        for (int i = 0; i < times; i++)
        {
            res_gpu.realize(lscale_gpu);
        }
        lscale_gpu.copy_to_host();
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
        printf("calc_l_scale lifted gpu average cost time: %lfms\n\n",cost_time/times*1000);
        
    }
    return 0;
} 