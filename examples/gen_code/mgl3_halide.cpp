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
    if (vectorize) nz.vectorize(d, 8);
    return nz;
}

template<>
inline Func init_nonzero<2>(const std::string& funcName, bool vectorize)
{
    Var d1, d2;
    Func nz(funcName);
    nz(d1, d2) = ((((d1 + d2*10)) + Expr(1e-5)) / Expr(100.0)) * cast<double>(10);
    nz.parallel(d2);
    if (vectorize) nz.vectorize(d1, 8);
    return nz;
}

template<>
inline Func init_nonzero<3>(const std::string& funcName, bool vectorize)
{
    Var d1, d2, d3;
    Func nz(funcName);
    nz(d1, d2, d3) = ((((d1 + d2*10 + d3*100)) + Expr(1e-5)) / Expr(100.0)) * cast<double>(10);
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
    Var d1, d2, d3;
    Func set_zero(funcName);
    set_zero(d1, d2, d3) = Expr(0.0);
    set_zero.parallel(d1);
    set_zero.parallel(d2);
    set_zero.parallel(d3);
    if (vectorize) set_zero.vectorize(d1, 8);
    return set_zero;
}

void targetFunction(Func &mi_fn, Func &m1_fn, Func &m2_fn, Func &m3_fn,
                    Buffer<double, 2> mi, Buffer<double, 2> ng,
                    const int lt) {

    Var d1("ax"), d2("k");
    // mi(ax,k) = 2 + ng(ax,k) for ax=1,3 and k=0,1
    mi_fn(d1,d2) = 2 + ng(d1, d2);
    m1_fn(d2) = mi_fn(0,d2);
    m2_fn(d2) = mi_fn(1,d2);
    m3_fn(d2) = mi_fn(2,d2);
}

extern "C" {
    void mg_loop3_(
        double* m1, double* m2, double *m3, double *mi,
        double *ng, const int *lt
    );
}

int main(int argc, char** argv)
{
    const int lt = 6e7;              //changed to 6e5

    // const int lt = 5;

    const int d1_range = lt;

    const int roi_d1_range = d1_range;

    // --------------------------- Preparation --------------------------
    bool with_gpu = true;
    Var d1("ax"), d2("k"), d1o, d2o, d1i, d2i;

    Target gpu_target = find_gpu_target();
    if (!gpu_target.has_gpu_feature())
    {
        printf("Don't have a gpu!\n");
        with_gpu = false;
    }
    Target cpu_target = get_host_target();

    // Input data generation functions
    auto init2_cpu = init_nonzero<2>("init", false);
    auto z1_cpu = set_zero<1>("zero", d1_range>= 8);
    auto z2_cpu = set_zero<2>("zero", false);
    auto init2_gpu = init2_cpu;
    auto z1_gpu = z1_cpu;
    auto z2_gpu = z2_cpu;

    init2_cpu.compile_jit(cpu_target);
    z1_cpu.compile_jit(cpu_target);
    z2_cpu.compile_jit(cpu_target);
    if ( with_gpu )
    {
        init2_gpu.compile_jit(gpu_target);
        z1_gpu.compile_jit(gpu_target);
        z2_gpu.compile_jit(gpu_target);
    }
    
    // Create input buffers
    Buffer<double, 2> ng({3, d1_range}, "ng");
    Buffer<double, 2> ng_g(ng);

    // Fortran output
    Buffer<double, 1> m1_base(d1_range, "m1_raw"); 
    Buffer<double, 1> m2_base(d1_range, "m2_raw"); 
    Buffer<double, 1> m3_base(d1_range, "m3_raw"); 
    // Buffer<double, 2> mi_base({3, d1_range}, "mi_raw"); 

    // Halide CPU output
    Buffer<double, 1> m1_cpu(d1_range, "m1"); 
    Buffer<double, 1> m2_cpu(d1_range, "m2"); 
    Buffer<double, 1> m3_cpu(d1_range, "m3"); 
    Buffer<double, 2> mi_cpu({3, d1_range}, "mi"); 
    // Halide GPU roiput
    Buffer<double, 1> m1_gpu(d1_range, "m1"); 
    Buffer<double, 1> m2_gpu(d1_range, "m2"); 
    Buffer<double, 1> m3_gpu(d1_range, "m3"); 
    Buffer<double, 2> mi_gpu({3, d1_range}, "mi"); 

    // --------------------------- horizontal_diffusion_u_2 kernel --------------------------
    printf("mgl3 kernel start!\n");

    // Coordinate transformations for proper indexing
    Func mi_fn("mi_fn"), m1_fn("m1_fn"), m2_fn("m2_fn"), m3_fn("m3_fn");
    targetFunction(mi_fn, m1_fn, m2_fn, m3_fn, mi_cpu, ng, lt);

    // CPU optimizations
    // mi_fn.parallel(d2)
    //      .vectorize(d1,3)
    //      .compile_jit(cpu_target);

    m1_fn.parallel(d2).vectorize(d2, 8).compile_jit(cpu_target);
    m2_fn.parallel(d2).vectorize(d2, 8).compile_jit(cpu_target);
    m3_fn.parallel(d2).vectorize(d2, 8).compile_jit(cpu_target);

    init2_cpu.realize(ng);
    
    z2_cpu.realize(mi_cpu);
    z1_cpu.realize(m1_base);
    z1_cpu.realize(m2_base);
    z1_cpu.realize(m3_base);

    mg_loop3_(
        m1_base.get()->begin(),
        m2_base.get()->begin(),
        m3_base.get()->begin(),
        mi_cpu.get()->begin(),
        ng.get()->begin(),
        &lt
    );

    // CPU execution
    try {
        // z2_cpu.realize(mi_cpu);
        z1_cpu.realize(m1_cpu);
        z1_cpu.realize(m2_cpu);
        z1_cpu.realize(m3_cpu);

        // mi_fn.realize(mi_cpu);
        m1_fn.realize(m1_cpu);
        m2_fn.realize(m2_cpu);
        m3_fn.realize(m3_cpu);
    } catch ( RuntimeError &e ) {
        std::cerr << e.what();
        return 1;
    }

    double correctness_1 = 0.0;
    double correctness_2 = 0.0;
    double correctness_3 = 0.0;
    double correctness_4 = 0.0;
    for (int i = 0; i < roi_d1_range; i++) {
        int a = i;
        auto A = m1_base(a);
        auto B = m1_cpu(i);
        double diff1 = abs(A - B);
        correctness_1 += diff1;

        A = m2_base(a);
        B = m2_cpu(i);
        double diff2 = abs(A - B);
        correctness_2 += diff2;

        A = m3_base(a);
        B = m3_cpu(i);
        double diff3 = abs(A - B);
        correctness_3 += diff3;
    }
    printf("The number for correctness_1 is %d\n",int(correctness_1));
    printf("The number for correctness_2 is %d\n",int(correctness_2));
    printf("The number for correctness_3 is %d\n",int(correctness_3));

    int times = 5;
    double cost_time = 0;
    struct timespec t1, t2, elapsed;
    clock_gettime(CLOCK_REALTIME, &t1);
    for (int i = 0; i < times; i++)
    {
        clock_gettime(CLOCK_REALTIME, &t1);
        mg_loop3_(
            m1_base.get()->begin(),
            m2_base.get()->begin(),
            m3_base.get()->begin(),
            mi_cpu.get()->begin(),
            ng.get()->begin(),
            &lt
        );
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("mg_loop3 lifted cpu average cost time: %lfms\n\n",cost_time/times*1000);

    cost_time = 0;
    clock_gettime(CLOCK_REALTIME, &t1);
    for (int i = 0; i < times; i++)
    {
        clock_gettime(CLOCK_REALTIME, &t1);
        // mi_fn.realize(mi_cpu);
        m1_fn.realize(m1_cpu);
        // m2_fn.realize(m2_cpu);
        // m3_fn.realize(m3_cpu);
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("mg_loop3 lifted cpu average cost time: %lfms\n\n",cost_time/times*1000);



    if ( with_gpu )
    {
        targetFunction(mi_fn, m1_fn, m2_fn, m3_fn, mi_gpu, ng_g, lt);

        // GPU optimizations
        // mi_fn.gpu_tile(d1,d2,d1o,d2o,d1i,d2i,8,8)
        //     .compile_jit(gpu_target);
        
        m1_fn.gpu_tile(d2,d2o,d2i,8)
            .compile_jit(gpu_target);
        m2_fn.gpu_tile(d2,d2o,d2i,8)
            .compile_jit(gpu_target);
        m3_fn.gpu_tile(d2,d2o,d2i,8)
            .compile_jit(gpu_target);
    
        // IO initialization
        init2_gpu.realize(ng);

        z1_gpu.realize(m1_gpu);
        z1_gpu.realize(m2_gpu);
        z1_gpu.realize(m3_gpu);
        z2_gpu.realize(mi_gpu);
        
        // GPU warm up
        try {
            // mi_fn.realize(mi_gpu);
            m1_fn.realize(m1_gpu);
            m2_fn.realize(m2_gpu);
            m3_fn.realize(m3_gpu);
        } catch ( RuntimeError &e ) {
            std::cerr << e.what();
            return 2;
        }

        cost_time = 0;
        clock_gettime(CLOCK_REALTIME, &t1);
        for (int i = 0; i < times; i++)
        {
            // mi_fn.realize(mi_gpu);
            // m1_fn.realize(m1_gpu);
            // m2_fn.realize(m2_gpu);
            m3_fn.realize(m3_gpu);
        }
        m3_gpu.copy_to_host();
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
        printf("lifted gpu average cost time: %lfms\n\n",cost_time/times*1000);
        
    }
    return 0;
} 