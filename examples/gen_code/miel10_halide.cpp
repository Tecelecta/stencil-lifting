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
Func init_nonzero(const std::string&, bool) { return Func(Expr(0.f));}

template <int dim>
Func set_zero(const std::string&, bool) { return Func(Expr(0.f));}

template<>
inline Func init_nonzero<1>(const std::string& funcName, bool vectorize)
{
    Var d;
    Func nz(funcName);
    nz(d) = cast<float>((((d ) + Expr(1e-5)) / Expr(100.0)) * 10);
   
    return nz;
}

template<>
inline Func init_nonzero<2>(const std::string& funcName, bool vectorize)
{
    Var d1, d2;
    Func nz(funcName);
    nz(d1, d2) = cast<float>(((((d1 + d2*10)) + Expr(1e-5)) / Expr(100.0)) * 10);

    return nz;
}

template<>
inline Func init_nonzero<3>(const std::string& funcName, bool vectorize)
{
    Var d1, d2, d3;
    Func nz(funcName);
    nz(d1, d2, d3) = cast<float>(((((d1 + d2*10 + d3*100)) + Expr(1e-5)) / Expr(100.0)) * 10);

    return nz;
}

template <>
inline Func set_zero<1>(const std::string& funcName, bool vectorize)
{
    Var d;
    Func set_zero(funcName);
    set_zero(d) = Expr(0.f);

    return set_zero;
}

template <>
inline Func set_zero<2>(const std::string& funcName, bool vectorize)
{
    Var d1, d2;
    Func set_zero(funcName);
    set_zero(d1, d2) = Expr(0.0f);

    return set_zero;
}

template <>
inline Func set_zero<3>(const std::string& funcName, bool vectorize)
{
    Var d1, d2, d3;
    Func set_zero(funcName);
    set_zero(d1, d2, d3) = Expr(0.0f);

    return set_zero;
}



Func targetFunction(Buffer<float, 3> pho,
                    Buffer<float, 3> tendency,
                    Buffer<float, 3> phb,
                    Buffer<float, 3> wwi,
                    Buffer<float, 1> c1,
                    Buffer<float, 1> c2, 
                    Buffer<float, 2> msfty,
                    Buffer<float, 2> mut,
                    Buffer<float, 1> rdzw) {

    Var i("i"), k("k"), j("j");
    Func res("advect_ph_implicit");
    Func pho_r("pho_remap"), phb_r("phb_remap"), rdzw_r("rdzw_remap");
    Expr dt_rk(.0375f);

    pho_r(i,k,j) = pho(i, k+1, j);
    phb_r(i,k,j) = phb(i, k+1, j);
    rdzw_r(k) = rdzw(k+1);

    auto wic = Expr(.5f) * wwi(i,k,j)*(rdzw_r(k-1)+rdzw_r(k)) * msfty(i,j) / (c1(k)*mut(i,j)+c2(k));
    auto at_ik = -dt_rk*max(wic,Expr(0.f));
    auto ct_ik = dt_rk*min(wic,Expr(0.f));
    auto btmp_i = -at_ik - ct_ik;

    res(i,k,j) = tendency(i,k,j) * dt_rk * msfty(i,j) / (c1(k)*mut(i,j)+c2(k)) 
               - at_ik*pho_r(i,k-1,j) - btmp_i*pho_r(i,k,j) - ct_ik * pho_r(i,k+1,j)
               - at_ik*phb_r(i,k-1,j) - btmp_i*phb_r(i,k,j) - ct_ik * phb_r(i,k+1,j);

    return res;
}

extern "C" {
    void advect_ph_implicit_(
        float* rt, 
        const float* pho, const float* tendency, const float* phb, 
        const float *wwi, const float* c1, const float* c2,
        const float *mut, const float *msfty, const float* rdzw,
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
    
    // Extended ranges for arrays with padding
    const int fn_range = d2_range + 1;     // kts:kte

    const int ph_d1_range = d1_range + 1;
    const int ph_d2_range = d2_range + 3;
    const int ph_d3_range = d3_range + 1;

    const int rdzw_d2_range = d2_range + 2;

    const int in_d1_range = d1_range + 1; 
    const int in_d2_range = d2_range + 1; 
    const int in_d3_range = d3_range + 1;

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
    auto i1_cpu = init_nonzero<1>("init", d1_range + 1 >= 8);
    auto z3_cpu = set_zero<3>("zero", d1_range + 1>= 8);
    auto i3_gpu = i3_cpu;
    auto i2_gpu = i2_cpu;
    auto i1_gpu = i1_cpu;
    auto z3_gpu = z3_cpu;

    i3_cpu.compile_jit(cpu_target);
    i2_cpu.compile_jit(cpu_target);
    i1_cpu.compile_jit(cpu_target);
    z3_cpu.compile_jit(cpu_target);
    if ( with_gpu )
    {
        i3_gpu.compile_jit(gpu_target);
        i2_gpu.compile_jit(gpu_target);
        i1_gpu.compile_jit(gpu_target);
        z3_gpu.compile_jit(gpu_target);
    }
    
    // Create input buffers
    Buffer<float, 1> rdzw(rdzw_d2_range, "rdzw"), rdzw_g(rdzw);
    Buffer<float, 1> c1(in_d2_range, "c1"), c1_g(c1),
                     c2(in_d2_range, "c2"), c2_g(c2);

    Buffer<float, 2> msfty({in_d1_range, in_d3_range}, "msfty"), msfty_g(msfty),
                     mut({in_d1_range, in_d3_range}, "mut"), mut_g(mut);
    
    Buffer<float, 3> pho({ph_d1_range, ph_d2_range, ph_d3_range}, "pho"), pho_g(pho),
                     phb({ph_d1_range, ph_d2_range, ph_d3_range}, "phb"), phb_g(phb),
                     tendency({in_d1_range, in_d2_range, in_d3_range}, "tendency"), tendency_g(tendency),
                     wwi({in_d1_range, in_d2_range, in_d3_range}, "wwi"), wwi_g(wwi);

    // Fortran output
    Buffer<float, 3> out_base({out_d1_range, out_d2_range, out_d3_range}, "out_base"); 

    // Halide CPU output
    Buffer<float, 3> out_cpu({out_d1_range, out_d2_range, out_d3_range}, "out_cpu"); 

    // Halide GPU output
    Buffer<float, 3> out_gpu({out_d1_range, out_d2_range, out_d3_range}, "out_gpu"); 

    // --------------------------- horizontal_diffusion_u_2 kernel --------------------------
    printf("miel10 kernel start!\n");

    // Coordinate transformations for proper indexing
 
    auto res_cpu = targetFunction(pho, tendency, phb, wwi, c1, c2, msfty, mut, rdzw);

    // CPU optimizations
    res_cpu.parallel(d3);
    if (d1_range >= 8) res_cpu.vectorize(d1,8);
    res_cpu.compile_jit(cpu_target);

    try {
    i3_cpu.realize(pho);
    i3_cpu.realize(phb);
    i3_cpu.realize(tendency);
    i3_cpu.realize(wwi);
    i2_cpu.realize(msfty);
    i2_cpu.realize(mut);
    i1_cpu.realize(rdzw);
    i1_cpu.realize(c1);
    i1_cpu.realize(c2);
    } catch (RuntimeError &e)
    {
        std::cerr << e.what();
    }

    z3_cpu.realize(out_base);

    advect_ph_implicit_(
        out_base.get()->begin(),
        pho.get()->begin(),
        tendency.get()->begin(),
        phb.get()->begin(),
        wwi.get()->begin(),
        c1.get()->begin(),
        c2.get()->begin(),
        mut.get()->begin(),
        msfty.get()->begin(),
        rdzw.get()->begin(),
        &its, &ite,
        &kts, &kte,
        &jts, &jte
    );

    // CPU execution
    try {
        z3_cpu.realize(out_cpu);
        res_cpu.realize(out_cpu);
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
                auto A = out_base(a,b,c);
                auto B = out_cpu(i,k,j);
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
        advect_ph_implicit_(
            out_base.get()->begin(),
            pho.get()->begin(),
            tendency.get()->begin(),
            phb.get()->begin(),
            wwi.get()->begin(),
            c1.get()->begin(),
            c2.get()->begin(),
            mut.get()->begin(),
            msfty.get()->begin(),
            rdzw.get()->begin(),
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
        res_cpu.realize(out_cpu);
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("lifted cpu average cost time: %lfms\n\n",cost_time/times*1000);

    if ( with_gpu )
    {
        auto res_gpu = targetFunction(pho_g,
                                      tendency_g, 
                                      phb_g, 
                                      wwi_g, 
                                      c1_g, 
                                      c2_g, 
                                      msfty_g, 
                                      mut_g, 
                                      rdzw_g);

        // GPU optimizations
        res_gpu.gpu_tile(d1,d2,d3,d1o,d2o,d3o,d1i,d2i,d3i,8,8,4)
                .compile_jit(gpu_target);
    
        // IO initialization
        i3_gpu.realize(pho_g);
        i3_gpu.realize(phb_g);
        i3_gpu.realize(tendency_g);
        i3_gpu.realize(wwi_g);
        i2_gpu.realize(msfty_g);
        i2_gpu.realize(mut_g);
        i1_gpu.realize(rdzw_g);
        i1_gpu.realize(c1_g);
        i1_gpu.realize(c2_g);
        
        // GPU warm up
        try {
            z3_gpu.realize(out_gpu);
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
        printf("lifted gpu average cost time: %lfms\n\n",cost_time/times*1000);
    }
    return 0;
} 