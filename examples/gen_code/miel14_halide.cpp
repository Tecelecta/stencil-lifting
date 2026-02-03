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
Func init_nonzero(const std::string&, bool) { return Func(Expr(0.f));}

template <int dim>
Func set_zero(const std::string&, bool) { return Func(Expr(0.f));}

template<>
inline Func init_nonzero<1>(const std::string& funcName, bool vectorize)
{
    Var d;
    Func nz(funcName);
    nz(d) = cast<float>((((d ) + Expr(1e-5)) / Expr(10000000.0)));
    return nz;
}

template<>
inline Func init_nonzero<2>(const std::string& funcName, bool vectorize)
{
    Var d1, d2;
    Func nz(funcName);
    nz(d1, d2) = cast<float>(((((d1 + d2*10)) + Expr(1e-5)) / Expr(10000000.0)));
    return nz;
}

template<>
inline Func init_nonzero<3>(const std::string& funcName, bool vectorize)
{
    Var d1, d2, d3;
    Func nz(funcName);
    nz(d1, d2, d3) = cast<float>(((((d1 + d2*10 + d3*10)) + Expr(1e-5)) / Expr(10000000.0)));
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

// miel14 kernel
Func targetFunction(Buffer<float, 3> w_old,
                   Buffer<float, 3> tendency,
                   Buffer<float, 3> rom,
                   Buffer<float, 2> mut_old,
                   Buffer<float, 2> mut_new,
                   Buffer<float, 1> c1,
                   Buffer<float, 1> c2,
                   Buffer<float, 2> msfty,
                   Buffer<float, 1> rdzu) {
    Var i("i"), k("k"), j("j");
    Func rt("advect_v_implicit");
    Expr dt_rk(.375f);

    Func rom_r("rom_remap"), w_old_r("wold_remap");
    rom_r(i, k, j) = rom(i, k+1, j);
    w_old_r(i, k, j) = w_old(i, k+1, j);

    auto wiL = 0.5f * (rom_r(i, k-1, j) + rom_r(i, k, j)) * rdzu(k) * msfty(i, j) / (c1(k)*mut_new(i, j) + c2(k));
    auto wiR = 0.5f * (rom_r(i, k+1, j) + rom_r(i, k, j)) * rdzu(k) * msfty(i, j) / (c1(k)*mut_new(i, j) + c2(k));
    auto at = -dt_rk * max(wiL, Expr(0.f));
    auto ct =  dt_rk * min(wiR, Expr(0.f));
    auto btmp = dt_rk * (max(wiR, Expr(0.f)) - min(wiL, Expr(0.f)));
    auto bt = 1.0f + btmp;
    rt(i,k,j) = dt_rk * tendency(i,k,j) * msfty(i,j)
                - (c1(k)*mut_old(i,j)+c2(k))*(at*w_old_r(i,k-1,j) + btmp*w_old_r(i,k,j) + ct*w_old_r(i,k+1,j));
    return rt;
}

extern "C" {
    void advect_v_implicit_(
        float* rt,
        const float* w_old, const float* tendency, const float* rom,
        const float* c1, const float* c2,
        const float* mut_old, const float* mut_new,
        const float* msfty, const float* rdzu,
        const int* its, const int* ite,
        const int* jts, const int* jte,
        const int* kts, const int* kte
    );
}


#ifndef _3D_1
#define _3D_1 1024
#define _3D_2 1024
#define _3D_3 1024
#endif

int main(int argc, char** argv)
{
    const int its = 0;
    const int ite = _3D_1;
    const int jts = 0;
    const int jte = _3D_2;
    const int kts = 0;
    const int kte = _3D_3;

    const int d1_range = ite - its;
    const int d2_range = kte - kts;
    const int d3_range = jte - jts;

    const int in_d1_range = d1_range + 1;
    const int in_d2_range = d2_range + 1;
    const int in_d3_range = d3_range + 1;

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
    Buffer<float, 1> rdzu(in_d2_range, "rdzu"), rdzu_g(rdzu);
    Buffer<float, 1> c1(in_d2_range, "c1"), c1_g(c1),
                     c2(in_d2_range, "c2"), c2_g(c2);

    Buffer<float, 2> msfty({in_d1_range, in_d3_range}, "msfty"), msfty_g(msfty),
                     mut_old({in_d1_range, in_d3_range}, "mut_old"), mut_old_g(mut_old),
                     mut_new({in_d1_range, in_d3_range}, "mut_new"), mut_new_g(mut_new);
    
    Buffer<float, 3> w_old({in_d1_range, in_d2_range+2, in_d3_range}, "w_old"), w_old_g(w_old),
                     tendency({in_d1_range, in_d2_range, in_d3_range}, "tendency"), tendency_g(tendency),
                     rom({in_d1_range, in_d2_range+2, in_d3_range}, "rom"), rom_g(rom);

    // Fortran output
    Buffer<float, 3> out_base({in_d1_range, in_d2_range, in_d3_range}, "out_base"); 

    // Halide CPU output
    Buffer<float, 3> out_cpu({in_d1_range, in_d2_range, in_d3_range}, "out_cpu"); 

    // Halide GPU output
    Buffer<float, 3> out_gpu({in_d1_range, in_d2_range, in_d3_range}, "out_gpu"); 

    // --------------------------- miel14 kernel --------------------------
    printf("miel14 kernel start!\n");

    auto res_cpu = targetFunction(w_old, tendency, rom, mut_old, mut_new, c1, c2, msfty, rdzu);

    // CPU optimizations
    res_cpu.parallel(d3);
    if (d1_range >= 8) res_cpu.vectorize(d1,8);
    res_cpu.compile_jit(cpu_target);

    try {
    i3_cpu.realize(w_old);
    i3_cpu.realize(tendency);
    i3_cpu.realize(rom);
    i2_cpu.realize(msfty);
    i2_cpu.realize(mut_old);
    i2_cpu.realize(mut_new);
    i1_cpu.realize(rdzu);
    i1_cpu.realize(c1);
    i1_cpu.realize(c2);
    } catch (RuntimeError &e)
    {
        std::cerr << e.what();
    }

    z3_cpu.realize(out_base);

    advect_v_implicit_(
        out_base.get()->begin(),
        w_old.get()->begin(),
        tendency.get()->begin(),
        rom.get()->begin(),
        c1.get()->begin(),
        c2.get()->begin(),
        mut_old.get()->begin(),
        mut_new.get()->begin(),
        msfty.get()->begin(),
        rdzu.get()->begin(),
        &its, &ite,
        &jts, &jte,
        &its, &ite
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
    for (int j = 0; j < in_d3_range; j++) {
        for (int k = 0; k < in_d2_range; k++) {
            for (int i = 0; i < in_d1_range; i++) {
                int a = i;
                int b = k;
                int c = j;
                auto A = out_base(a,b,c);
                auto B = out_cpu(i,k,j);
                double diff1 = abs(A - B);
                if ( diff1 >= 1e-1 )
                {
                    printf("output1 = Fort[%d, %d, %d]: %lf | Hali[%d, %d, %d]: %lf\n", 
                            a, b, c, A, i, k, j, B);
                }
                correctness_1 += diff1;
            }
        }
    }
    printf("The number for correctness_1 is %d\n",int(correctness_1/(in_d1_range*in_d2_range*in_d3_range)));

    int times = 20;
    double cost_time = 0;
    struct timespec t1, t2, elapsed;
    clock_gettime(CLOCK_REALTIME, &t1);
    for (int i = 0; i < times; i++)
    {
        clock_gettime(CLOCK_REALTIME, &t1);
        advect_v_implicit_(
            out_base.get()->begin(),
            w_old.get()->begin(),
            tendency.get()->begin(),
            rom.get()->begin(),
            c1.get()->begin(),
            c2.get()->begin(),
            mut_old.get()->begin(),
            mut_new.get()->begin(),
            msfty.get()->begin(),
            rdzu.get()->begin(),
            &its, &ite,
            &jts, &jte,
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
        res_cpu.realize(out_cpu);
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("lifted cpu average cost time: %lfms\n\n",cost_time/times*1000);

    if ( with_gpu )
    {
        auto res_gpu = targetFunction(w_old_g, tendency_g, rom_g, mut_old_g, mut_new_g, c1_g, c2_g, msfty_g, rdzu_g);
        res_gpu.gpu_tile(d1,d2,d3,d1o,d2o,d3o,d1i,d2i,d3i,8,8,4)
                .compile_jit(gpu_target);
        i3_gpu.realize(w_old_g);
        i3_gpu.realize(tendency_g);
        i3_gpu.realize(rom_g);
        i2_gpu.realize(msfty_g);
        i2_gpu.realize(mut_old_g);
        i2_gpu.realize(mut_new_g);
        i1_gpu.realize(rdzu_g);
        i1_gpu.realize(c1_g);
        i1_gpu.realize(c2_g);
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