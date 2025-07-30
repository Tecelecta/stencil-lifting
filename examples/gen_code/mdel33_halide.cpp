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

void targetFunction(Func& res1, Func& res2, Func& res3, Func& res4,
                    Buffer<double, 3> titau1,
                    Buffer<double, 3> titau2,
                    Buffer<double, 3> zx,
                    Buffer<double, 3> zy,
                    Buffer<double, 1> fnm,
                    Buffer<double, 1> fnp,
                    const int jts, const int jte,
                    const int kts, const int kte,
                    const int its, const int ite) {

    Var d1("i"), d2("k"), d3("j");

    Func titau1_conv("titau1_convert");
    titau1_conv(d1,d2,d3) = titau1(d1+2,d2+(kts+1),d3+1);
    
    Func titau2_conv("titau2_convert");
    titau2_conv(d1,d2,d3) = titau2(d1+2,d2+(kts+1),d3+1);
    
    Func zx_conv("zx_convert");
    // zx_conv(d1,d2,d3) = zx(d1+2,d2+(kts+1),d3);
    zx_conv(d1,d2,d3) = zx(d1+1,d2+(kts+1),d3);
    
    Func zy_conv("zy_convert");
    zy_conv(d1,d2,d3) = zy(d1+2,d2+(kts+1),d3);
    
    Func fnm_conv("fnm_convert");
    fnm_conv(d2) = fnm(d2+(kts+1));
    
    Func fnp_conv("fnp_convert");
    fnp_conv(d2) = fnp(d2+(kts+1));

    // ------- result1_func (titau1avg calculation) --------
    // Original: titau1avg(i,k,j)=0.5*(fnm(k)*(titau1(i-1,k,j)+titau1(i,k,j))+fnp(k)*(titau1(i-1,k-1,j)+titau1(i,k-1,j)))
    res1(d1,d2,d3) = Expr(0.5) * 
        (fnm_conv(d2) * (titau1_conv(d1-1,d2,d3) + titau1_conv(d1,d2,d3)) +
         fnp_conv(d2) * (titau1_conv(d1-1,d2-1,d3) + titau1_conv(d1,d2-1,d3)));
  
    // ------- result2_func (titau2avg calculation) --------
    // Original: titau2avg(i,k,j)=0.5*(fnm(k)*(titau2(i,k,j+1)+titau2(i,k,j))+fnp(k)*(titau2(i,k-1,j+1)+titau2(i,k-1,j)))
    res2(d1,d2,d3) = Expr(0.5) * 
        (fnm_conv(d2) * (titau2_conv(d1,d2,d3+1) + titau2_conv(d1,d2,d3)) +
         fnp_conv(d2) * (titau2_conv(d1,d2-1,d3+1) + titau2_conv(d1,d2-1,d3)));

    // ------- result3_func (zx_at_u calculation) --------
    // Original: zx_at_u(i,k,j) = 0.5 * (zx(i,k,j) + zx(i,k+1,j))
    res3(d1,d2,d3) = Expr(0.5) * (zx_conv(d1,d2,d3) + zx_conv(d1,d2+1,d3));

    // ------- result4_func (zy_at_u calculation) --------
    // Original: zy_at_u(i,k,j) = 0.125 * (zy(i-1,k,j) + zy(i,k,j) + zy(i-1,k,j+1) + zy(i,k,j+1) + 
    //                                      zy(i-1,k+1,j) + zy(i,k+1,j) + zy(i-1,k+1,j+1) + zy(i,k+1,j+1))
    res4(d1,d2,d3) = Expr(0.125) * 
        (zy_conv(d1-1,d2,d3) + zy_conv(d1,d2,d3) + zy_conv(d1-1,d2,d3+1) + zy_conv(d1,d2,d3+1) +
         zy_conv(d1-1,d2+1,d3) + zy_conv(d1,d2+1,d3) + zy_conv(d1-1,d2+1,d3+1) + zy_conv(d1,d2+1,d3+1));
}

extern "C" {
    void horizontal_diffusion_u_2_(
        double* titau1avg, double* titau2avg, double* zx_at_u, double* zy_at_u,
        double* tittitau1, double* titau2, double* fnm, double* fnp, 
        double* zx, double* zy,
        const int* jts, const int* jte, 
        const int* kts, const int* kte, 
        const int* its, const int* ite
    );
}

#ifndef _3D_1
#define _3D_1 512
#define _3D_2 512
#define _3D_3 512
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
    const int titau_d1_range = d1_range + 3; // its-1:ite+1
    const int titau_d2_range = d2_range + 1; // kts:kte
    const int titau_d3_range = d3_range + 3; // jts-1:jte+1
    
    const int zx_d1_range = d1_range + 1;    // Extended for boundary access
    const int zx_d2_range = d2_range + 2;   // Extended for k+1 access
    const int zx_d3_range = d3_range + 1;

    const int zy_d1_range = d1_range + 2;    // Extended for i-1 access
    const int zy_d2_range = d2_range + 2;   // Extended for k+1 access
    const int zy_d3_range = d3_range + 2;    // Extended for j+1 access
    
    const int fnm_range = d2_range + 1;     // kts:kte
    const int fnp_range = d2_range + 1;     // kts:kte

    const int out_d1_range = d1_range + 1;
    const int out_d2_range = d2_range + 1;
    const int out_d3_range = d3_range + 1;

    const int roi_d1_range = out_d1_range - 1;
    const int roi_d2_range = out_d2_range - 1;
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
    Buffer<double, 3> titau1({titau_d1_range, titau_d2_range, titau_d3_range}, "titau1");
    Buffer<double, 3> titau2({titau_d1_range, titau_d2_range, titau_d3_range}, "titau2");
    Buffer<double, 3> zx({zx_d1_range, zx_d2_range, zx_d3_range}, "zx");
    Buffer<double, 3> zy({zy_d1_range, zy_d2_range, zy_d3_range}, "zy");
    Buffer<double, 1> fnm(fnm_range, "fnm");
    Buffer<double, 1> fnp(fnp_range, "fnp");

    Buffer<double, 3> titau1_g(titau1), titau2_g(titau2), zx_g(zx), zy_g(zy);
    Buffer<double, 1> fnm_g(fnm), fnp_g(fnp);

    // Fortran output
    Buffer<double, 3> titau1avg_base({out_d1_range, out_d2_range, out_d3_range}, "titau1avg_raw"); // titau1avg
    Buffer<double, 3> titau2avg_base({out_d1_range, out_d2_range, out_d3_range}, "titau2avg_raw"); // titau2avg
    Buffer<double, 3> zx_at_u_base({out_d1_range, out_d2_range, out_d3_range}, "zx_at_u_raw"); // zx_at_u
    Buffer<double, 3> zy_at_u_base({out_d1_range, out_d2_range, out_d3_range}, "zy_at_u_raw"); // zy_at_u

    // Halide CPU output
    Buffer<double, 3> titau1avg_cpu({roi_d1_range, roi_d2_range, roi_d3_range}, "titau1avg"); // titau1avg
    Buffer<double, 3> titau2avg_cpu({roi_d1_range, roi_d2_range, roi_d3_range}, "titau2avg"); // titau2avg
    Buffer<double, 3> zx_at_u_cpu({roi_d1_range, roi_d2_range, roi_d3_range}, "zx_at_u"); // zx_at_u
    Buffer<double, 3> zy_at_u_cpu({roi_d1_range, roi_d2_range, roi_d3_range}, "zy_at_u"); // zy_at_u

    // Halide GPU roiput
    Buffer<double, 3> titau1avg_gpu({roi_d1_range, roi_d2_range, roi_d3_range}, "titau1avg"); // titau1avg
    Buffer<double, 3> titau2avg_gpu({roi_d1_range, roi_d2_range, roi_d3_range}, "titau2avg"); // titau2avg
    Buffer<double, 3> zx_at_u_gpu({roi_d1_range, roi_d2_range, roi_d3_range}, "zx_at_u"); // zx_at_u
    Buffer<double, 3> zy_at_u_gpu({roi_d1_range, roi_d2_range, roi_d3_range}, "zy_at_u"); // zy_at_u

    // --------------------------- horizontal_diffusion_u_2 kernel --------------------------
    printf("horizontal_diffusion_u_2 kernel start!\n");

    // Coordinate transformations for proper indexing
    Func res1_cpu("o1"), res2_cpu("o2"), res3_cpu("o3"), res4_cpu("o4");

    targetFunction(res1_cpu, res2_cpu, res3_cpu, res4_cpu,
                   titau1, titau2, zx, zy, fnm, fnp,
                   jts, jte, kts, kte, its, ite);

    // CPU optimizations
    res1_cpu.parallel(d3);
    if (d1_range >= 8) res1_cpu.vectorize(d1,8);
    res1_cpu.compile_jit(cpu_target);

    res2_cpu.parallel(d3);
    if (d1_range >= 8) res2_cpu.vectorize(d1,8);
    res2_cpu.compile_jit(cpu_target);

    res3_cpu.parallel(d3);
    if (d1_range >= 8) res3_cpu.vectorize(d1,8);
    res3_cpu.compile_jit(cpu_target);

    res4_cpu.parallel(d3);
    if (d1_range >= 8) res4_cpu.vectorize(d1,8);
    res4_cpu.compile_jit(cpu_target);


    init3_cpu.realize(titau1);
    init3_cpu.realize(titau2);
    init3_cpu.realize(zx);
    init3_cpu.realize(zy);
    init1_cpu.realize(fnp);
    init1_cpu.realize(fnm);

    zero_cpu.realize(titau1avg_base);
    zero_cpu.realize(titau2avg_base);
    zero_cpu.realize(zx_at_u_base);
    zero_cpu.realize(zy_at_u_base);

    horizontal_diffusion_u_2_(
        titau1avg_base.get()->begin(),
        titau2avg_base.get()->begin(),
        zx_at_u_base.get()->begin(),
        zy_at_u_base.get()->begin(),
        titau1.get()->begin(),
        titau2.get()->begin(),
        fnm.get()->begin(),
        fnp.get()->begin(),
        zx.get()->begin(),
        zy.get()->begin(),
        &jts, &jte,
        &kts, &kte,
        &its, &ite
    );

    // CPU execution
    try {
        zero_cpu.realize(titau1avg_cpu);
        zero_cpu.realize(titau2avg_cpu);
        zero_cpu.realize(zx_at_u_cpu);
        zero_cpu.realize(zy_at_u_cpu);

        res1_cpu.realize(titau1avg_cpu);
        res2_cpu.realize(titau2avg_cpu);
        res3_cpu.realize(zx_at_u_cpu);
        res4_cpu.realize(zy_at_u_cpu);
    } catch ( RuntimeError &e ) {
        std::cerr << e.what();
        return 1;
    }

    double correctness_1 = 0.0, correctness_2 = 0.0, correctness_3 = 0.0, correctness_4 = 0.0;
    for (int j = 0; j < roi_d3_range; j++) {
        for (int k = 0; k < roi_d2_range; k++) {
            for (int i = 0; i < roi_d1_range; i++) {
                int a = i+1;
                int b = k+1;
                int c = j;
                double diff1 = abs(titau1avg_base(a,b,c) - titau1avg_cpu(i,k,j));
                double diff2 = abs(titau2avg_base(a,b,c) - titau2avg_cpu(i,k,j));
                double diff3 = abs(zx_at_u_base(a,b,c) - zx_at_u_cpu(i,k,j));
                double diff4 = abs(zy_at_u_base(a,b,c) - zy_at_u_cpu(i,k,j));

                correctness_1 += diff1;
                correctness_2 += diff2;
                correctness_3 += diff3;
                correctness_4 += diff4;
            }
        }
    }
    printf("The number for correctness_1 is %d\n",int(correctness_1));
    printf("The number for correctness_2 is %d\n",int(correctness_2));
    printf("The number for correctness_3 is %d\n",int(correctness_3));
    printf("The number for correctness_4 is %d\n",int(correctness_4));

    int times = 5;
    double cost_time = 0;
    struct timespec t1, t2, elapsed;
    clock_gettime(CLOCK_REALTIME, &t1);
    for (int i = 0; i < times; i++)
    {
        clock_gettime(CLOCK_REALTIME, &t1);
        horizontal_diffusion_u_2_(
            titau1avg_base.get()->begin(),
            titau2avg_base.get()->begin(),
            zx_at_u_base.get()->begin(),
            zy_at_u_base.get()->begin(),
            titau1.get()->begin(),
            titau2.get()->begin(),
            fnm.get()->begin(),
            fnp.get()->begin(),
            zx.get()->begin(),
            zy.get()->begin(),
            &jts, &jte,
            &kts, &kte,
            &its, &ite
        );
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("horizontal_diffusion_u_2 legacy code average cost time: %lfms\n\n",cost_time/times*1000);

    cost_time = 0;
    clock_gettime(CLOCK_REALTIME, &t1);
    for (int i = 0; i < times; i++)
    {
        clock_gettime(CLOCK_REALTIME, &t1);
        res1_cpu.realize(titau1avg_cpu);
        res2_cpu.realize(titau2avg_cpu);
        res3_cpu.realize(zx_at_u_cpu);
        res4_cpu.realize(zy_at_u_cpu);
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("horizontal_diffusion_u_2 lifted cpu average cost time: %lfms\n\n",cost_time/times*1000);



    if ( with_gpu )
    {
        Func res1_gpu("o1"), res2_gpu("o2"), res3_gpu("o3"), res4_gpu("o4");
        targetFunction(res1_gpu, res2_gpu, res3_gpu, res4_gpu,
                titau1_g, titau2_g, zx_g, zy_g, fnm_g, fnp_g,
                jts, jte, kts, kte, its, ite);

        // GPU optimizations
        res1_gpu.gpu_tile(d1,d2,d3,d1o,d2o,d3o,d1i,d2i,d3i,8,8,4)
                .compile_jit(gpu_target);

        res2_gpu.gpu_tile(d1,d2,d3,d1o,d2o,d3o,d1i,d2i,d3i,8,8,4)
                .compile_jit(gpu_target);

        res3_gpu.gpu_tile(d1,d2,d3,d1o,d2o,d3o,d1i,d2i,d3i,8,8,4)
                .compile_jit(gpu_target);

        res4_gpu.gpu_tile(d1,d2,d3,d1o,d2o,d3o,d1i,d2i,d3i,8,8,4)
                .compile_jit(gpu_target);
    
        // IO initialization
        init3_gpu.realize(titau1_g);
        init3_gpu.realize(titau2_g);
        init3_gpu.realize(zx_g);
        init3_gpu.realize(zy_g);
        init1_gpu.realize(fnp_g);
        init1_gpu.realize(fnm_g);
        
        // GPU warm up
        try {
            zero_gpu.realize(titau1avg_gpu);
            zero_gpu.realize(titau2avg_gpu);
            zero_gpu.realize(zx_at_u_gpu);
            zero_gpu.realize(zy_at_u_gpu);
            
            res1_gpu.realize(titau1avg_gpu);
            res2_gpu.realize(titau2avg_gpu);
            res3_gpu.realize(zx_at_u_gpu);
            res4_gpu.realize(zy_at_u_gpu);
        } catch ( RuntimeError &e ) {
            std::cerr << e.what();
            return 2;
        }

        cost_time = 0;
        clock_gettime(CLOCK_REALTIME, &t1);
        for (int i = 0; i < times; i++)
        {
            res1_gpu.realize(titau1avg_gpu);
            res2_gpu.realize(titau2avg_gpu);
            res3_gpu.realize(zx_at_u_gpu);
            res4_gpu.realize(zy_at_u_gpu);
        }
        zy_at_u_gpu.copy_to_host();
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
        printf("horizontal_diffusion_u_2 lifted gpu average cost time: %lfms\n\n",cost_time/times*1000);
        
    }
    return 0;
} 