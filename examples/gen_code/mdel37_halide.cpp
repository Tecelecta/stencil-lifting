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

Func targetFunction(const std::string& funcName,
                    Buffer<double, 3> w,
                    const int jts, const int jte,
                    const int kts, const int kte,
                    const int its, const int ite) {

    Var d1("i"), d2("k"), d3("j");
    Func res(funcName);

    Func w_c("w_c");
    // auto w_b = BoundaryConditions::constant_exterior(w, 0);
    w_c(d1,d2,d3) = w(d1+1,d2,d3+1); 

    // ------- result1_func (wavg calculation) --------
    // Original: wavg(i,k,j) = 0.125 * (w(i,k,j) + w(i-1,k,j) + w(i,k,j-1) + w(i-1,k,j-1) + 
    //                                  w(i,k+1,j) + w(i-1,k+1,j) + w(i,k+1,j-1) + w(i-1,k+1,j-1))
    res(d1,d2,d3) = Expr(0.125) * 
        (w_c(d1,d2  ,d3  ) + w_c(d1-1,d2  ,d3  ) + 
         w_c(d1,d2  ,d3-1) + w_c(d1-1,d2  ,d3-1) +
         w_c(d1,d2+1,d3  ) + w_c(d1-1,d2+1,d3  ) + 
         w_c(d1,d2+1,d3-1) + w_c(d1-1,d2+1,d3-1));

    return res;
}

extern "C" {
    void cal_helicity_(
        double* wavg, double* w,
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
    const int kts = 1;
    const int kte = 1+1024;

    // const int its = 0;
    // const int ite = 5;
    // const int jts = 0;
    // const int jte = 5;
    // const int kts = 0;
    // const int kte = 0+5;

    const int d1_range = ite - its; // i: its to ite
    const int d2_range = kte - kts;  // k: kts+1 to ktf
    const int d3_range = jte - jts; // j: j_start to j_end
    
    // Extended ranges for arrays with padding
    const int w_d1_range = d1_range + 2; // its-1:ite
    const int w_d2_range = d2_range + 2; // kts:kte+1
    const int w_d3_range = d3_range + 2; // jts-1:jte

    const int out_d1_range = d1_range + 1;
    const int out_d2_range = d2_range + 1;
    const int out_d3_range = d3_range + 1;

    const int roi_d1_range = out_d1_range; // i-1 not aligned
    const int roi_d2_range = out_d2_range; // k-1 aligned
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
    auto zero_cpu = set_zero<3>("zero", d1_range + 1>= 8);
    auto init3_gpu = init3_cpu;
    auto zero_gpu = zero_cpu;

    init3_cpu.compile_jit(cpu_target);
    zero_cpu.compile_jit(cpu_target);
    if ( with_gpu )
    {
        init3_gpu.compile_jit(gpu_target);
        zero_gpu.compile_jit(gpu_target);
    }
    
    // Create input buffers
    Buffer<double, 3> w({w_d1_range, w_d2_range, w_d3_range}, "w");
    Buffer<double, 3> w_g(w);

    // Fortran output
    Buffer<double, 3> out_base({out_d1_range, out_d2_range, out_d3_range}, "wavg_raw"); // w

    // Halide CPU output
    Buffer<double, 3> out_cpu({roi_d1_range, roi_d2_range, roi_d3_range}, "wavg"); // wavg

    // Halide GPU roiput
    Buffer<double, 3> out_gpu({roi_d1_range, roi_d2_range, roi_d3_range}, "wavg"); // wavg

    // --------------------------- horizontal_diffusion_u_2 kernel --------------------------
    printf("mdel37 kernel start!\n");

    // Coordinate transformations for proper indexing

    const double cf1 = .125, cf2 = .375, cf3 = .575, cft1 = .25, cft2 = .75; 
    auto res_cpu = targetFunction("cal_helicity_cpu",
                   w,
                   jts, jte, kts, kte, its, ite);

    // CPU optimizations
    res_cpu.parallel(d3);
    if (d1_range >= 8) res_cpu.vectorize(d1,8);
    res_cpu.compile_jit(cpu_target);


    init3_cpu.realize(w);

    zero_cpu.realize(out_base);

    cal_helicity_(
        out_base.get()->begin(),
        w.get()->begin(),
        &jts, &jte,
        &kts, &kte,
        &its, &ite
    );

    // CPU execution
    try {
        zero_cpu.realize(out_cpu);
        res_cpu.realize(out_cpu);
    } catch ( RuntimeError &e ) {
        std::cerr << e.what();
        return 1;
    }

    double correctness_1 = 0.0;
    for (int j = 0; j < roi_d3_range; j++) {
        for (int k = 0; k < roi_d2_range; k++) {
            for (int i = 0; i < roi_d1_range; i++) {
                int a = i;
                int b = k;
                int c = j;
                auto A = out_base(a,b,c);
                auto B = out_cpu(i,k,j);
                double diff1 = abs(A - B);
                correctness_1 += diff1;
            }
        }
    }
    printf("The number for correctness_1 is %d\n",int(correctness_1/(roi_d3_range*roi_d2_range*roi_d1_range)));

    int times = 5;
    double cost_time = 0;
    struct timespec t1, t2, elapsed;
    clock_gettime(CLOCK_REALTIME, &t1);
    for (int i = 0; i < times; i++)
    {
        clock_gettime(CLOCK_REALTIME, &t1);
        cal_helicity_(
            out_base.get()->begin(),
            w.get()->begin(),
            &jts, &jte,
            &kts, &kte,
            &its, &ite
        );
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("cal_helicity legacy code average cost time: %lfms\n\n",cost_time/times*1000);

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
    printf("cal_helicity lifted cpu average cost time: %lfms\n\n",cost_time/times*1000);



    if ( with_gpu )
    {
        auto res_gpu = targetFunction("cal_helicity_gpu",
                   w_g, 
                   jts, jte, kts, kte, its, ite);

        // GPU optimizations
        res_gpu.gpu_tile(d1,d2,d3,d1o,d2o,d3o,d1i,d2i,d3i,8,8,4)
                .compile_jit(gpu_target);
    
        // IO initialization
        init3_gpu.realize(w_g);
        
        // GPU warm up
        try {
            zero_gpu.realize(out_gpu);
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