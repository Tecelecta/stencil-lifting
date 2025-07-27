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



void targetFunction(Func& bdy_fn,
                    Func& bdy_tend_fn,
                    Expr theta_to_thetam,
                    Buffer<float, 2> mub,
                    Buffer<float, 3> mu_bdy_ys,
                    Buffer<float, 3> mu_bdy_tend_ys,
                    Buffer<float, 3> moist_bdy_ys,
                    Buffer<float, 3> moist_bdy_tend_ys,
                    Buffer<float, 3> t_bdy_ys, 
                    Buffer<float, 3> t_bdy_tend_ys) {

    Var i("i"), k("k"), j("j");
    Expr T0(.5f);
    Expr R_v(.25f);
    Expr R_d(.25f);
    Expr dt_interval(1120.f);

    auto mu_old_bdy_ys    = mu_bdy_ys(i,0,j) + mub(i,j);
    auto t_old_bdy_ys     = t_bdy_ys(i,k,j) / mu_old_bdy_ys;
    auto moist_old_bdy_ys = moist_bdy_ys(i,k,j) / mu_old_bdy_ys;
    auto mu_new_bdy_ys    = mu_old_bdy_ys + mu_bdy_tend_ys(i,0,j) * dt_interval;
    auto t_new_bdy_ys     = (t_bdy_ys(i,k,j) + t_bdy_tend_ys(i,k,j) * dt_interval ) / mu_new_bdy_ys;
    auto moist_new_bdy_ys = (moist_bdy_ys(i,k,j) + moist_bdy_tend_ys(i,k,j)*dt_interval) / mu_new_bdy_ys;
    
    auto bdy_tval = ( ( ( t_old_bdy_ys + T0 ) * ( Expr(1.f) + (R_v/R_d) * moist_old_bdy_ys ) ) - T0 ) * mu_old_bdy_ys;
    auto bdy_tend_tval = ( ( mu_new_bdy_ys * ( ( t_new_bdy_ys + T0 ) * ( Expr(1.f) + (R_v/R_d) * moist_new_bdy_ys ) - T0 ) ) - 
                           ( mu_old_bdy_ys * ( ( t_old_bdy_ys + T0 ) * ( Expr(1.f) + (R_v/R_d) * moist_old_bdy_ys ) - T0 ) ) ) / dt_interval;
    auto bdy_fval = ( ( ( t_old_bdy_ys + T0 ) / ( Expr(1.f) + (R_v/R_d) * moist_old_bdy_ys ) ) - T0 ) * mu_old_bdy_ys;
    auto bdy_tend_fval = ( ( mu_new_bdy_ys * ( ( t_new_bdy_ys + T0 ) / ( Expr(1.f) + (R_v/R_d) * moist_new_bdy_ys ) - T0 ) ) - 
                           ( mu_old_bdy_ys * ( ( t_old_bdy_ys + T0 ) / ( Expr(1.f) + (R_v/R_d) * moist_old_bdy_ys ) - T0 ) ) ) / dt_interval;
    
    bdy_fn(i,k,j) = select(theta_to_thetam, bdy_tval, bdy_fval);
    bdy_tend_fn(i,k,j) = select(theta_to_thetam, bdy_tend_tval, bdy_tend_fval);
}

extern "C" {
    void theta_and_thetam_lbc_only_(
        float* new_t_bdy_ys, float * new_t_bdy_tend_ys, bool* theta_to_thetam,
        const float* mub, const float* mu_bdy_ys, const float* mu_bdy_tend_ys, 
        const float *moist_bdy_ys, const float* moist_bdy_tend_ys, 
        const float* t_bdy_ys, const float *t_bdy_tend_ys, 
        const int* its, const int* ite,
        const int* jts, const int* jte, 
        const int* kts, const int* kte 
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
    
    // Create input buffer
    Buffer<float, 2> mub({in_d1_range, in_d3_range}, "mub"), mub_g(mub);

    Buffer<float, 3> mu_bdy_ys({in_d1_range, 1, in_d3_range}, "mu_bdy_ys"), mu_bdy_ys_g(mu_bdy_ys),
                     mu_bdy_tend_ys({in_d1_range, 1, in_d3_range}, "mu_bdy_tend_ys"), mu_bdy_tend_ys_g(mu_bdy_tend_ys),
                     moist_bdy_ys({in_d1_range, in_d2_range, in_d3_range}, "moist_bdy_ys"), moist_bdy_ys_g(moist_bdy_ys),
                     moist_bdy_tend_ys({in_d1_range, in_d2_range, in_d3_range}, "moist_bdy_tend_ys"), moist_bdy_tend_ys_g(moist_bdy_tend_ys),
                     t_bdy_ys({in_d1_range, in_d2_range, in_d3_range}, "t_bdy_ys"), t_bdy_ys_g(t_bdy_ys),
                     t_bdy_tend_ys({in_d1_range, in_d2_range, in_d3_range}, "t_bdy_tend_ys"), t_bdy_tend_ys_g(t_bdy_tend_ys);

    // Fortran output
    Buffer<float, 3> out1_base({out_d1_range, out_d2_range, out_d3_range}, "out1_base"); 
    Buffer<float, 3> out2_base({out_d1_range, out_d2_range, out_d3_range}, "out2_base"); 

    // Halide CPU output
    Buffer<float, 3> out1_cpu({out_d1_range, out_d2_range, out_d3_range}, "out1_cpu"); 
    Buffer<float, 3> out2_cpu({out_d1_range, out_d2_range, out_d3_range}, "out2_cpu"); 

    // Halide GPU output
    Buffer<float, 3> out1_gpu({out_d1_range, out_d2_range, out_d3_range}, "out1_gpu"); 
    Buffer<float, 3> out2_gpu({out_d1_range, out_d2_range, out_d3_range}, "out2_gpu"); 

    // --------------------------- horizontal_diffusion_u_2 kernel --------------------------
    printf("mbel10 kernel start!\n");

    // Coordinate transformations for proper indexing
 
    Func bdy_fn_cpu("new_t_bdy_ys"), bdy_tend_fn_cpu("new_t_bdy_tend_ys");
    bool theta_to_thetam = true;
    targetFunction(bdy_fn_cpu, bdy_tend_fn_cpu, 
                   Expr(theta_to_thetam),
                   mub,
                   mu_bdy_ys,
                   mu_bdy_tend_ys,
                   moist_bdy_ys,
                   moist_bdy_tend_ys,
                   t_bdy_ys,
                   t_bdy_tend_ys);

    // CPU optimizations
    bdy_fn_cpu.parallel(d3);
    if (d1_range >= 8) bdy_fn_cpu.vectorize(d1,8);
    bdy_fn_cpu.compile_jit(cpu_target);

    bdy_tend_fn_cpu.parallel(d3);
    if (d1_range >= 8) bdy_tend_fn_cpu.vectorize(d1,8);
    bdy_tend_fn_cpu.compile_jit(cpu_target);

    try {
    i3_cpu.realize(mu_bdy_ys);
    i3_cpu.realize(mu_bdy_tend_ys);
    i3_cpu.realize(moist_bdy_ys);
    i3_cpu.realize(moist_bdy_tend_ys);
    i3_cpu.realize(t_bdy_ys);
    i3_cpu.realize(t_bdy_tend_ys);
    i2_cpu.realize(mub);
    } catch (RuntimeError &e)
    {
        std::cerr << e.what();
    }

    z3_cpu.realize(out1_base);
    z3_cpu.realize(out2_base);

    theta_and_thetam_lbc_only_(
        out1_base.get()->begin(),
        out2_base.get()->begin(),
        &theta_to_thetam,
        mub.get()->begin(),
        mu_bdy_ys.get()->begin(),
        mu_bdy_tend_ys.get()->begin(),
        moist_bdy_ys.get()->begin(),
        moist_bdy_tend_ys.get()->begin(),
        t_bdy_ys.get()->begin(),
        t_bdy_tend_ys.get()->begin(),
        &its, &ite,
        &kts, &kte,
        &jts, &jte
    );

    // CPU execution
    try {
        z3_cpu.realize(out1_cpu);
        z3_cpu.realize(out2_cpu);
        bdy_fn_cpu.realize(out1_cpu);
        bdy_tend_fn_cpu.realize(out2_cpu);
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
                auto A = out2_base(a,b,c);
                auto B = out2_cpu(i,k,j);
                double diff1 = abs(A - B);
                if ( diff1 >= 1e-2 )
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
        theta_and_thetam_lbc_only_(
            out1_base.get()->begin(),
            out2_base.get()->begin(),
            &theta_to_thetam,
            mub.get()->begin(),
            mu_bdy_ys.get()->begin(),
            mu_bdy_tend_ys.get()->begin(),
            moist_bdy_ys.get()->begin(),
            moist_bdy_tend_ys.get()->begin(),
            t_bdy_ys.get()->begin(),
            t_bdy_tend_ys.get()->begin(),
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
        bdy_tend_fn_cpu.realize(out2_cpu);
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("lifted cpu average cost time: %lfms\n\n",cost_time/times*1000);

    if ( with_gpu )
    {
        Func bdy_fn_gpu("new_t_bdy_ys"), bdy_tend_fn_gpu("new_t_bdy_tend_ys");
        targetFunction(bdy_fn_gpu, bdy_tend_fn_gpu, 
                    Expr(theta_to_thetam),
                    mub_g,
                    mu_bdy_ys_g,
                    mu_bdy_tend_ys_g,
                    moist_bdy_ys_g,
                    moist_bdy_tend_ys_g,
                    t_bdy_ys_g,
                    t_bdy_tend_ys_g);

        // GPU optimizations
        bdy_fn_gpu.gpu_tile(d1,d2,d3,d1o,d2o,d3o,d1i,d2i,d3i,8,8,4)
                .compile_jit(gpu_target);
        bdy_tend_fn_gpu.gpu_tile(d1,d2,d3,d1o,d2o,d3o,d1i,d2i,d3i,8,8,4)
                .compile_jit(gpu_target);
    
        // IO initialization
        i3_gpu.realize(mu_bdy_ys_g);
        i3_gpu.realize(mu_bdy_tend_ys_g);
        i3_gpu.realize(moist_bdy_ys_g);
        i3_gpu.realize(moist_bdy_tend_ys_g);
        i3_gpu.realize(t_bdy_ys_g);
        i3_gpu.realize(t_bdy_tend_ys_g);
        i2_gpu.realize(mub_g);
        
        // GPU warm up
        try {
            z3_gpu.realize(out1_gpu);
            z3_gpu.realize(out2_gpu);
            bdy_fn_gpu.realize(out1_gpu);
            bdy_tend_fn_gpu.realize(out2_gpu);
        } catch ( RuntimeError &e ) {
            std::cerr << e.what();
            return 2;
        }

        cost_time = 0;
        clock_gettime(CLOCK_REALTIME, &t1);
        for (int i = 0; i < times; i++)
        {
            bdy_tend_fn_gpu.realize(out2_gpu);
        }
        out2_gpu.copy_to_host();
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
        printf("lifted gpu average cost time: %lfms\n\n",cost_time/times*1000);
    }
    return 0;
} 