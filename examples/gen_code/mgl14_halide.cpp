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
    nz(d) = (((d+1 ) + Expr(1e-5)) / Expr(100.0)) * cast<double>(10);
    if (vectorize) nz.vectorize(d, 8);
    return nz;
}

template<>
inline Func init_nonzero<2>(const std::string& funcName, bool vectorize)
{
    Var d1, d2;
    Func nz(funcName);
    nz(d1, d2) = ((((d1+1 + (d2+1)*10)) + Expr(1e-5)) / Expr(100.0)) * cast<double>(10);
    nz.parallel(d2);
    if (vectorize) nz.vectorize(d1, 8);
    return nz;
}

template<>
inline Func init_nonzero<3>(const std::string& funcName, bool vectorize)
{
    Var d1, d2, d3;
    Func nz(funcName);
    nz(d1, d2, d3) = ((((d1+1 + (d2+1)*10 + (d3+1)*100)) + Expr(1e-5)) / Expr(100.0)) * cast<double>(10);
    nz.parallel(d2);
    nz.parallel(d3);
    if (vectorize) nz.vectorize(d1, 8);
    return nz;
}

template <>
inline Func set_zero<1>(const std::string& funcName, bool vectorize)
{
    Var d;
    Func set_zero(funcName);
    set_zero(d) = Expr(0.0);
    set_zero.parallel(d);
    if (vectorize) set_zero.vectorize(d, 8);
    return set_zero;
}

template <>
inline Func set_zero<2>(const std::string& funcName, bool vectorize)
{
    Var d1, d2;
    Func set_zero(funcName);
    set_zero(d1, d2) = Expr(0.0);
    set_zero.parallel(d1);
    set_zero.parallel(d2);
    if (vectorize) set_zero.vectorize(d1, 8);
    return set_zero;
}

template <>
inline Func set_zero<3>(const std::string& funcName, bool vectorize)
{
    Var d1("mm"), d2, d3;
    Func set_zero(funcName);
    set_zero(d1, d2, d3) = Expr(0.0);
    set_zero.parallel(d1);
    set_zero.parallel(d2);
    set_zero.parallel(d3);
    if (vectorize) set_zero.vectorize(d1, 8);
    return set_zero;
}

Func targetFunction(const std::string &funcName,
                    Buffer<double, 2> ten) {

    Var d1("i1"), d2("i2");
    Func res(funcName);
    res(d1, d2) = cast<double>(1 - d2);

    return res;
}

extern "C" {
    void mg_loop14_(
        double* j1, double *j2, double *j3, double *ten,
        const int *mm
    );
}

int main(int argc, char** argv)
{
    // Check for memory constraints and adjust size if needed
    const int mm = 1024*1024*10;

    // const int n1 = 5;
    // const int n2 = 5;
    // const int n3 = 5;
    
    // --------------------------- Preparation --------------------------
    Var d1("i1"), d1o, d1i;

    bool with_gpu = true;
    Target gpu_target = find_gpu_target();
    if (!gpu_target.has_gpu_feature())
    {
        printf("Don't have a gpu!\n");
        with_gpu = false;
    }
    Target cpu_target = get_host_target();
    
    // Enable GPU execution now that CPU version works

    // Input data generation functions
    auto z2_cpu = set_zero<2>("zero2", mm >= 8);
    Func z2_gpu(z2_cpu);
    if ( with_gpu ) 
    {
        z2_gpu.compile_jit(gpu_target);
    }

    Buffer<double, 2> j1({mm, 2}, "j1");
    Buffer<double, 2> j2({mm, 2}, "j2");
    Buffer<double, 2> j3({mm, 2}, "j3");

    // Fortran output
    Buffer<double, 2> out_base({mm, 2}, "out_raw");
    // Lifted CPU output
    Buffer<double, 2> out_cpu({mm, 2}, "out_cpu");
    // Lifted GPU output
    Buffer<double, 2> out_gpu({mm, 2}, "out_gpu");

    // --------------------------- mg_loop14 kernel --------------------------
    printf("mg_loop14 kernel start!\n");
    // gpu_target.set_feature(Halide::Target::Debug);

    auto res_cpu = targetFunction("mg_loop14_cpu",
                                  out_cpu);
    
    // CPU optimizations
    res_cpu.vectorize(d1, 8)
           .compile_jit(cpu_target);


    z2_cpu.realize(out_base);
    z2_cpu.realize(j1);
    z2_cpu.realize(j2);
    z2_cpu.realize(j3);

    mg_loop14_(
        j1.get()->begin(),
        j2.get()->begin(),
        j3.get()->begin(),
        out_base.get()->begin(),
        &mm
    );

    try {
        z2_cpu.realize(out_cpu);
        res_cpu.realize(out_cpu);
    } catch ( RuntimeError &e ) {
        std::cerr << e.what();
        return 1;
    }

    double correctness_1 = 0.0;
    for (int k = 0; k < 1; k++) {
        for (int i = 0; i < mm; i++) {
            int a = i;
            int b = k;
            auto A = out_base(a,b);
            auto B = out_cpu(i,k);
            double diff1 = abs(A - B);
            correctness_1 += diff1;
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
        // z2_cpu.realize(out_base);
        mg_loop14_(
            j1.get()->begin(),
            j2.get()->begin(),
            j3.get()->begin(),
            out_base.get()->begin(),
            &mm
        );
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("mg_loop14 legacy code average cost time: %lfms\n\n",cost_time/times*1000);

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
    printf("mg_loop14 lifted cpu average cost time: %lfms\n\n",cost_time/times*1000);



    if ( with_gpu )
    {
        auto res_gpu = targetFunction("mg_loop14_gpu",
                                      out_gpu);
        // GPU optimizations
        res_gpu.gpu_tile(d1,d1o,d1i,8)
                .compile_jit(gpu_target);
    
        // IO initialization
        z2_gpu.realize(out_gpu);
        // GPU warm up
        try {
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
        res_gpu.copy_to_host();
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
        printf("lifted gpu average cost time: %lfms\n\n",cost_time/times*1000);
    }

    return 0;
} 