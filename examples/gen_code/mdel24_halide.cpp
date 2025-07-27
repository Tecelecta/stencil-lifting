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

void targetFunction(Func& xkmh, Func& xkhh,
                    Buffer<double, 3> rdzw,
                    Buffer<double, 3> def2,
                    Buffer<double, 3> zx,
                    Buffer<double, 3> zy,
                    Buffer<double, 2> msftx,
                    Buffer<double, 2> msfty) {

    Var i("i"), k("k"), j("j");

    Expr c_s(.25), pr(.125), dx(.8), dy(.8);
    auto mlen_h = sqrt(dx / msftx(i,j) * dy / msfty(i,j));
    auto tmp = sqrt(def2(i,k,j));
    auto xkmh_tmp = c_s*c_s*mlen_h*mlen_h*tmp;
    xkmh_tmp = min(xkmh_tmp, Expr(10.)*mlen_h );

    auto dxm = dx/msftx(i,j);
    auto dym = dy/msfty(i,j);
    auto tmpzx = (Expr(0.25)*( abs(zx(i,k,j))+ abs(zx(i+1,k,j  )) + abs(zx(i,k+1,j))+ abs(zx(i+1,k+1,j  )))*rdzw(i,k,j)*dxm);
    auto tmpzy = (Expr(0.25)*( abs(zy(i,k,j))+ abs(zy(i  ,k,j+1)) + abs(zy(i,k+1,j))+ abs(zy(i  ,k+1,j+1)))*rdzw(i,k,j)*dym);
    auto alpha = max(sqrt(tmpzx*tmpzx+tmpzy*tmpzy),Expr(1.));
    auto def_limit = max(Expr(10.)/mlen_h, Expr(1.e-3));

    auto brval_t = xkmh_tmp/(alpha*alpha);
    auto brval_f = xkmh_tmp/(alpha);

    auto xkmh_out = select(
        tmp > def_limit,
        brval_t,
        brval_f
    );
    xkmh(i,k,j) = xkmh_out;
    xkhh(i,k,j) = xkmh_out / pr;
}

extern "C" {
    void tke_km_(
        double* xkmh, double* xkhh,
        double* rdzw, double* def2,
        double* zx, double* zy,
        double* msftx, double* msfty,
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
    
    const int in_d1_range = d1_range + 1;
    const int in_d2_range = d2_range + 1;
    const int in_d3_range = d3_range + 1;

    const int zxy_d1_range = d1_range + 2;    // Extended for i-1 access
    const int zxy_d2_range = d2_range + 2;   // Extended for k+1 access
    const int zxy_d3_range = d3_range + 2;    // Extended for j+1 access

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
    Buffer<double, 3> rdzw({in_d1_range, in_d2_range, in_d3_range}, "rdzw"), rdzw_g(rdzw);
    Buffer<double, 3> def2({in_d1_range, in_d2_range, in_d3_range}, "def2"), def2_g(def2);
    Buffer<double, 3> zx({zxy_d1_range, zxy_d2_range, zxy_d3_range}, "zx"), zx_g(zx);
    Buffer<double, 3> zy({zxy_d1_range, zxy_d2_range, zxy_d3_range}, "zy"), zy_g(zy);
    Buffer<double, 2> msftx({in_d1_range, in_d3_range}, "msftx"), msftx_g(msftx);
    Buffer<double, 2> msfty({in_d1_range, in_d3_range}, "msfty"), msfty_g(msfty);

    // Fortran output
    Buffer<double, 3> xkmh_base({out_d1_range, out_d2_range, out_d3_range}, "xkmh_raw"); // xkmh
    Buffer<double, 3> xkhh_base({out_d1_range, out_d2_range, out_d3_range}, "xkhh_raw"); // xkhh

    // Halide CPU output
    Buffer<double, 3> xkmh_cpu({out_d1_range, out_d2_range, out_d3_range}, "xkmh"); // xkmh
    Buffer<double, 3> xkhh_cpu({out_d1_range, out_d2_range, out_d3_range}, "xkhh"); // xkhh

    // Halide GPU roiput
    Buffer<double, 3> xkmh_gpu({out_d1_range, out_d2_range, out_d3_range}, "xkmh"); // xkmh
    Buffer<double, 3> xkhh_gpu({out_d1_range, out_d2_range, out_d3_range}, "xkhh"); // xkhh

    // --------------------------- kernel --------------------------
    printf("kernel start!\n");

    // Coordinate transformations for proper indexing
    Func fn1_cpu("tke_km_xkmh");
    Func fn2_cpu("tke_km_xkhh");
    targetFunction(fn1_cpu, fn2_cpu, 
                    rdzw,
                    def2,
                    zx,
                    zy,
                    msftx,
                    msfty);

    // CPU optimizations
    fn1_cpu.parallel(d3);
    if (d1_range >= 8) fn1_cpu.vectorize(d1,8);
    fn1_cpu.compile_jit(cpu_target);

    fn2_cpu.parallel(d3);
    if (d1_range >= 8) fn2_cpu.vectorize(d1,8);
    fn2_cpu.compile_jit(cpu_target);
    
    i3_cpu.realize(rdzw);
    i3_cpu.realize(def2);
    i3_cpu.realize(zx);
    i3_cpu.realize(zx);
    i2_cpu.realize(msftx);
    i2_cpu.realize(msfty);

    z3_cpu.realize(xkmh_base);
    z3_cpu.realize(xkhh_base);

    tke_km_(
        xkmh_base.get()->begin(),
        xkhh_base.get()->begin(),
        rdzw.get()->begin(),
        def2.get()->begin(),
        zx.get()->begin(),
        zy.get()->begin(),
        msftx.get()->begin(),
        msfty.get()->begin(),
        &its, &ite, &jts, &jte, &kts, &kte
    );

    // CPU execution
    try {
        z3_cpu.realize(xkmh_cpu);
        z3_cpu.realize(xkhh_cpu);
        fn1_cpu.realize(xkmh_cpu);
        fn2_cpu.realize(xkhh_cpu);
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
                auto A = xkmh_base(a,b,c);
                auto B = xkmh_cpu(i,k,j);
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
        tke_km_(
            xkmh_base.get()->begin(),
            xkhh_base.get()->begin(),
            rdzw.get()->begin(),
            def2.get()->begin(),
            zx.get()->begin(),
            zy.get()->begin(),
            msftx.get()->begin(),
            msfty.get()->begin(),
            &its, &ite, &jts, &jte, &kts, &kte
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
        fn1_cpu.realize(xkmh_cpu);
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("lifted cpu average cost time: %lfms\n\n",cost_time/times*1000);



    if ( with_gpu )
    {
        Func fn1_gpu("tke_km_xkmh");
        Func fn2_gpu("tke_km_xkhh");
        targetFunction(fn1_gpu, fn2_gpu, 
                        rdzw_g,
                        def2_g,
                        zx_g,
                        zy_g,
                        msftx_g,
                        msfty_g);

        // GPU optimizations
        fn1_gpu.gpu_tile(d1,d2,d3,d1o,d2o,d3o,d1i,d2i,d3i,8,8,4)
                .compile_jit(gpu_target);
        fn2_gpu.gpu_tile(d1,d2,d3,d1o,d2o,d3o,d1i,d2i,d3i,8,8,4)
                .compile_jit(gpu_target);

        // IO initialization
        i3_gpu.realize(rdzw_g);
        i3_gpu.realize(def2_g);
        i3_gpu.realize(zx_g);
        i3_gpu.realize(zx_g);
        i2_gpu.realize(msftx_g);
        i2_gpu.realize(msfty_g);

        // GPU warm up
        try {
            z3_gpu.realize(xkmh_gpu);
            z3_gpu.realize(xkhh_gpu);
            fn1_gpu.realize(xkmh_gpu);
            fn2_gpu.realize(xkhh_gpu);
        } catch ( RuntimeError &e ) {
            std::cerr << e.what();
            return 2;
        }

        cost_time = 0;
        clock_gettime(CLOCK_REALTIME, &t1);
        for (int i = 0; i < times; i++)
        {
            fn1_gpu.realize(xkmh_gpu);
        }
        xkmh_gpu.copy_to_host();
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
        printf("lifted gpu average cost time: %lfms\n\n",cost_time/times*1000);
    }
    return 0;
} 