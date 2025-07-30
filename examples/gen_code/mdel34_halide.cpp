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
                    Buffer<double, 2> hpbl,
                    Buffer<double, 2> kpbl,
                    Buffer<double, 2> msftx,
                    Buffer<double, 2> msfty,
                    Buffer<double, 2> sflux,
                    Buffer<double, 2> enlfrac2,
                    Buffer<double, 3> zq,
                    Buffer<double, 3> entfacmf) {

    Var i("i"), k("k"), j("j");
    Func res(funcName);

    auto pthnl = [](Expr d, Expr h) {
        auto doh = d / h;
        auto num = Expr(1.0)*pow(doh, Expr(2.0)) + Expr(0.936)*pow(doh, Expr(0.875)) + Expr(-1.110);
        auto den = Expr(1.0)*pow(doh, Expr(2.0)) + Expr(0.312)*pow(doh, Expr(0.875)) + Expr(0.329);
        auto pthnl_1 = select( h != Expr(0.),
            Expr(0.243) * num / den + (Expr(1. - 0.243)),
            Expr(1.)
        );
        auto pthnl_2 = max(pthnl_1, Expr(0.));
        auto pthnl_3 = min(pthnl_2, Expr(1.));
        return select(d <= Expr(100.),
            Expr(0.),
            pthnl_3
        );
    };

    Expr dx(.8), dy(.8), mltop(1.), sfcfracn1(.075), 
         zfmin(.075), a11(.8), a12(.8), nlfrac(1.);
    Func deltaoh("deltaoh"), zfacmf("zfacmf"), hfxpbl("hfxpbl");
    deltaoh(i, j) = sflux(i, j) / hpbl(i, j);
    auto delxy = sqrt(dx / msftx(i, j) * dy / msfty(i, j));
    auto mlfrac = mltop - deltaoh(i, j);
    auto ezfrac = mltop + deltaoh(i, j);
    // zfacmf(i, 0, j) = ;
    auto sfcfracn = max(sfcfracn1, min(max((zq(i, 1, j)/hpbl(i, j)), zfmin), Expr(1.)));

    auto sflux0 = (a11+a12*sfcfracn)*sflux(i,j);
    auto snlflux0 = nlfrac*sflux0;
    auto amf1 = snlflux0/sfcfracn;
    auto amf2 = -snlflux0/(mlfrac-sfcfracn);
    auto bmf2 = -mlfrac*amf2;
    auto amf3 = snlflux0*enlfrac2(i,j)/deltaoh(i,j);
    auto bmf3 = -amf3*mlfrac;
    hfxpbl(i,j) = amf3 + bmf3;
    auto pth1 = pthnl(delxy,hpbl(i,j));
    hfxpbl(i,j) = hfxpbl(i,j)*pth1;

    zfacmf(i,k,j) = max((zq(i,k+1,j)/hpbl(i,j)),zfmin);
    res(i,k,j) = select( k < kpbl(i,j),
        (select(zfacmf(i,k,j) <= sfcfracn,
            amf1*zfacmf(i,k,j),
            select(zfacmf(i,k,j) <= mlfrac,
                amf2*zfacmf(i,k,j)+bmf2,
                Expr(0.)
            )
        ) + hfxpbl(i,j) * exp(-entfacmf(i,k,j))) * pth1,
        Expr(0.)
    );

    return res;
}

extern "C" {
    void nonlocal_flux_(
        double* nlflux, double* hfxpbl, double* deltaoh, double* zfacmf, 
        const double* hpbl, const double* kpbl, const double* msftx, const double *msfty,
        const double* sflux, const double* enlfrac2, const double *zq, const double *entfacmf,
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

    const int in_d1_range = d1_range + 1;
    const int in_d2_range = d2_range + 1;
    const int in_d3_range = d3_range + 1;

    const int zq_d2_range = d2_range + 2;

    const int out_d1_range = in_d1_range; // i-1 not aligned
    const int out_d2_range = in_d2_range; // k-1 aligned
    const int out_d3_range = in_d3_range;

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
    auto z2_cpu = set_zero<2>("zero", d1_range + 1>= 8);
    auto i3_gpu = i3_cpu;
    auto i2_gpu = i2_cpu;
    auto z3_gpu = z3_cpu;
    auto z2_gpu = z2_cpu;

    i3_cpu.compile_jit(cpu_target);
    i2_cpu.compile_jit(cpu_target);
    z3_cpu.compile_jit(cpu_target);
    z2_cpu.compile_jit(cpu_target);
    if ( with_gpu )
    {
        i3_gpu.compile_jit(gpu_target);
        i2_gpu.compile_jit(gpu_target);
        z3_gpu.compile_jit(gpu_target);
        z2_gpu.compile_jit(gpu_target);
    }
    
    // Create input buffers
    Buffer<double, 2> hpbl({in_d1_range, in_d2_range}, "hpbl"),
                      kpbl({in_d1_range, in_d2_range}, "kpbl"),
                      msftx({in_d1_range, in_d2_range}, "msftx"),
                      msfty({in_d1_range, in_d2_range}, "msfty"),
                      sflux({in_d1_range, in_d2_range}, "sflux"),
                      enlfrac2({in_d1_range, in_d2_range}, "enlfrac2");
    
    Buffer<double, 3> zq({in_d1_range, zq_d2_range, in_d3_range}, "zq"),
                      entfacmf({in_d1_range, in_d2_range, in_d3_range}, "entfacmf");

    Buffer<double, 2> hpbl_g(hpbl), kpbl_g(kpbl), msftx_g(msftx), msfty_g(msfty), 
                      sflux_g(sflux), enlfrac2_g(enlfrac2);

    Buffer<double, 3> zq_g(zq), entfacmf_g(entfacmf);

    // Fortran output
    Buffer<double, 3> nlflux_base({out_d1_range, out_d2_range, out_d3_range}, "nlflux_base"); 
    Buffer<double, 2> hfxpbl_base({out_d1_range, out_d2_range}, "hfxpbl_base"); 
    Buffer<double, 2> deltaoh_base({out_d1_range, out_d2_range}, "deltaoh_base"); 
    Buffer<double, 3> zfacmf_base({out_d1_range, out_d2_range, out_d3_range}, "zfacmf_base"); 

    // Halide CPU output
    Buffer<double, 3> nlflux_cpu({out_d1_range, out_d2_range, out_d3_range}, "nlflux_cpu"); 

    // Halide GPU output
    Buffer<double, 3> nlflux_gpu({out_d1_range, out_d2_range, out_d3_range}, "nlflux_gpu"); 

    // --------------------------- horizontal_diffusion_u_2 kernel --------------------------
    printf("mdel34 kernel start!\n");

    // Coordinate transformations for proper indexing
 
    auto res_cpu = targetFunction("nonlocal_flux",
                                  hpbl, kpbl, msftx, msfty,
                                  sflux, enlfrac2, zq, entfacmf);

    // CPU optimizations
    res_cpu.parallel(d3);
    if (d1_range >= 8) res_cpu.vectorize(d1,8);
    res_cpu.compile_jit(cpu_target);

    i2_cpu.realize(hpbl);
    i2_cpu.realize(kpbl);
    i2_cpu.realize(msftx);
    i2_cpu.realize(msfty);
    i2_cpu.realize(sflux);
    i2_cpu.realize(enlfrac2);
    i3_cpu.realize(zq);
    i3_cpu.realize(entfacmf);

    z3_cpu.realize(nlflux_base);
    z3_cpu.realize(zfacmf_base);
    z2_cpu.realize(hfxpbl_base);
    z2_cpu.realize(deltaoh_base);

    nonlocal_flux_(
        nlflux_base.get()->begin(),
        hfxpbl_base.get()->begin(),
        deltaoh_base.get()->begin(),
        zfacmf_base.get()->begin(),
        hpbl.get()->begin(),
        kpbl.get()->begin(),
        msftx.get()->begin(), 
        msfty.get()->begin(),
        sflux.get()->begin(),
        enlfrac2.get()->begin(),
        zq.get()->begin(),
        entfacmf.get()->begin(),
        &jts, &jte,
        &kts, &kte,
        &its, &ite
    );

    // CPU execution
    try {
        z3_cpu.realize(nlflux_cpu);
        res_cpu.realize(nlflux_cpu);
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
                auto A = nlflux_base(a,b,c);
                auto B = nlflux_cpu(i,k,j);
                double diff1 = abs(A - B);
                if ( diff1 >= 1e-6 )
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
        nonlocal_flux_(
            nlflux_base.get()->begin(),
            hfxpbl_base.get()->begin(),
            deltaoh_base.get()->begin(),
            zfacmf_base.get()->begin(),
            hpbl.get()->begin(),
            kpbl.get()->begin(),
            msftx.get()->begin(), 
            msfty.get()->begin(),
            sflux.get()->begin(),
            enlfrac2.get()->begin(),
            zq.get()->begin(),
            entfacmf.get()->begin(),
            &jts, &jte,
            &kts, &kte,
            &its, &ite
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
        res_cpu.realize(nlflux_cpu);
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("lifted cpu average cost time: %lfms\n\n",cost_time/times*1000);



    if ( with_gpu )
    {
        auto res_gpu = targetFunction("nonlocal_flux",
                                  hpbl_g, kpbl_g, msftx_g, msfty_g,
                                  sflux_g, enlfrac2_g, zq_g, entfacmf_g);

        // GPU optimizations
        res_gpu.gpu_tile(d1,d2,d3,d1o,d2o,d3o,d1i,d2i,d3i,8,8,4)
                .compile_jit(gpu_target);
    
        // IO initialization
        i2_gpu.realize(hpbl_g);
        i2_gpu.realize(kpbl_g);
        i2_gpu.realize(msftx_g);
        i2_gpu.realize(msfty_g);
        i2_gpu.realize(sflux_g);
        i2_gpu.realize(enlfrac2_g);
        i3_gpu.realize(zq_g);
        i3_gpu.realize(entfacmf_g);
        
        // GPU warm up
        try {
            z3_gpu.realize(nlflux_gpu);
            res_gpu.realize(nlflux_gpu);
        } catch ( RuntimeError &e ) {
            std::cerr << e.what();
            return 2;
        }

        cost_time = 0;
        clock_gettime(CLOCK_REALTIME, &t1);
        for (int i = 0; i < times; i++)
        {
            res_gpu.realize(nlflux_gpu);
        }
        nlflux_gpu.copy_to_host();
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
        printf("lifted gpu average cost time: %lfms\n\n",cost_time/times*1000);
        
    }
    return 0;
} 