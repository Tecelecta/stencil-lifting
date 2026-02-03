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

Func targetFunction(Buffer<double, 3> rdzw,
                    Buffer<double, 3> rdz,
                    Buffer<double, 3> p8w,
                    Buffer<double, 3> t8w,
                    Buffer<double, 3> theta,
                    const int kts, const int kte) {

    Var i("i"), k("k"), j("j");

    Func res("meso_length_scale");
    Func theta_ = BoundaryConditions::constant_exterior(theta, Expr(0.));
    Func rdz_ = BoundaryConditions::constant_exterior(rdz, Expr(0.));
    Func rdzw_ = BoundaryConditions::constant_exterior(rdzw, Expr(0.));
    Expr p1000mb(100000.), r_d(287.04), cp(1004.6);

    auto tmpdz_i = Expr(1.) / rdz_(i,k+1,j) + Expr(1.) / rdz_(i,k,j);
    auto brval_i = (theta_(i, k+1, j) - theta_(i, k-1, j)) / tmpdz_i;

    auto tmpdz_b = Expr(1.) / rdzw_(i,k+1,j) + Expr(1.) / rdzw_(i,k,j);
    auto thetasfc = t8w(i,kts,j) / pow( p8w(i,k,j)/p1000mb, r_d/cp );
    auto brval_b = ( theta_(i,k+1,j) - thetasfc ) / tmpdz_b;
    
    auto tmpdz_e = Expr(1.) / rdz_(i,k,j) + Expr(.5) / rdzw_(i,k,j);
    auto thetatop = t8w(i,kte,j) / pow( p8w(i,kte,j)/p1000mb, r_d/cp );
    auto brval_e = ( thetatop - theta_(i,k-1,j) ) / tmpdz_e;

    res(i,k,j) = select(
        k == kts,
        brval_b,
        select( 
            k==kte,
            brval_e,
            brval_i
        )
    );

    return res;
}

extern "C" {
    void meso_length_scale_(
        double* dthrdn, 
        double* rdzw, double* rdz, double* p8w, double* t8w, double* theta, 
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
    auto z3_cpu = set_zero<3>("zero", d1_range + 1>= 8);
    auto i3_gpu = i3_cpu;
    auto z3_gpu = z3_cpu;

    i3_cpu.compile_jit(cpu_target);
    z3_cpu.compile_jit(cpu_target);
    if ( with_gpu )
    {
        i3_gpu.compile_jit(gpu_target);
        z3_gpu.compile_jit(gpu_target);
    }
    
    // Create input buffers
    Buffer<double, 3> rdzw({in_d1_range, in_d2_range, in_d3_range}, "rdzw"), rdzw_g(rdzw);
    Buffer<double, 3> rdz({in_d1_range, in_d2_range, in_d3_range}, "rdz"), rdz_g(rdz);
    Buffer<double, 3> p8w({in_d1_range, in_d2_range, in_d3_range}, "p8w"), p8w_g(p8w);
    Buffer<double, 3> t8w({in_d1_range, in_d2_range, in_d3_range}, "t8w"), t8w_g(t8w);
    Buffer<double, 3> theta({in_d1_range, in_d2_range, in_d3_range}, "theta"), theta_g(theta);

    // Fortran output
    Buffer<double, 3> dthrdn_base({out_d1_range, out_d2_range, out_d3_range}, "dthrdn_raw"); // dthrdn

    // Halide CPU output
    Buffer<double, 3> dthrdn_cpu({out_d1_range, out_d2_range, out_d3_range}, "dthrdn"); // dthrdn

    // Halide GPU roiput
    Buffer<double, 3> dthrdn_gpu({out_d1_range, out_d2_range, out_d3_range}, "dthrdn"); // dthrdn

    // --------------------------- horizontal_diffusion_u_2 kernel --------------------------
    printf("horizontal_diffusion_u_2 kernel start!\n");

    // Coordinate transformations for proper indexing
    auto res_cpu = targetFunction(rdzw, rdz, p8w, t8w, theta, kts, kte);

    // CPU optimizations
    res_cpu.parallel(d3);
    if (d1_range >= 8) res_cpu.vectorize(d1,8);
    res_cpu.compile_jit(cpu_target);

    i3_cpu.realize(rdzw);
    i3_cpu.realize(rdz);
    i3_cpu.realize(p8w);
    i3_cpu.realize(t8w);
    i3_cpu.realize(theta);

    z3_cpu.realize(dthrdn_base);

    meso_length_scale_(
        dthrdn_base.get()->begin(), 
        rdzw.get()->begin(),
        rdz.get()->begin(),
        p8w.get()->begin(),
        t8w.get()->begin(), 
        theta.get()->begin(), 
        &its, &ite, &jts, &jte, &kts, &kte
    );

    // CPU execution
    try {
        z3_cpu.realize(dthrdn_cpu);
        res_cpu.realize(dthrdn_cpu);
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
                auto A = dthrdn_base(a,b,c);
                auto B = dthrdn_cpu(i,k,j);
                double diff1 = abs( A - B );
                if ( diff1 >= 1e-2 )
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
        meso_length_scale_(
            dthrdn_base.get()->begin(), 
            rdzw.get()->begin(),
            rdz.get()->begin(),
            p8w.get()->begin(),
            t8w.get()->begin(), 
            theta.get()->begin(), 
            &its, &ite, &jts, &jte, &kts, &kte
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
        res_cpu.realize(dthrdn_cpu);
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("horizontal_diffusion_u_2 lifted cpu average cost time: %lfms\n\n",cost_time/times*1000);



    if ( with_gpu )
    {
        auto res_gpu = targetFunction(rdzw_g, rdz_g, p8w_g, t8w_g, theta_g, kts, kte);

        // GPU optimizations
        res_gpu.gpu_tile(d1,d2,d3,d1o,d2o,d3o,d1i,d2i,d3i,8,8,4)
                .compile_jit(gpu_target);

        // IO initialization
        i3_gpu.realize(rdzw_g);
        i3_gpu.realize(rdz_g);
        i3_gpu.realize(p8w_g);
        i3_gpu.realize(t8w_g);
        i3_gpu.realize(theta_g);

        // GPU warm up
        try {
            z3_gpu.realize(dthrdn_gpu);
            res_gpu.realize(dthrdn_gpu);
        } catch ( RuntimeError &e ) {
            std::cerr << e.what();
            return 2;
        }

        cost_time = 0;
        clock_gettime(CLOCK_REALTIME, &t1);
        for (int i = 0; i < times; i++)
        {
            res_gpu.realize(dthrdn_gpu);
        }
        dthrdn_gpu.copy_to_host();
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
        printf("horizontal_diffusion_u_2 lifted gpu average cost time: %lfms\n\n",cost_time/times*1000);
        
    }
    return 0;
} 