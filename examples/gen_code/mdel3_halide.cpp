#include <cstdio>
#include <ctime>
// include the generated header 
#include <Halide.h>


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
    nz(d) = (((d % 13) + Expr(0.1)) / Expr(13.0)) * cast<double>(10);
    if (vectorize) nz.vectorize(d, 8);
    return nz;
}

template<>
inline Func init_nonzero<2>(const std::string& funcName, bool vectorize)
{
    Var d1, d2;
    Func nz(funcName);
    nz(d1, d2) = ((((d1 + d2) % 13) + Expr(0.1)) / Expr(13.0)) * cast<double>(10);
    nz.parallel(d2);
    if (vectorize) nz.vectorize(d1, 8);
    return nz;
}

template<>
inline Func init_nonzero<3>(const std::string& funcName, bool vectorize)
{
    Var d1, d2, d3;
    Func nz(funcName);
    nz(d1, d2, d3) = ((((d1 + d2 + d3) % 13) + Expr(0.1)) / Expr(13.0)) * cast<double>(10);
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

inline Func targetFunction(const std::string& funcName,
                           Buffer<double, 3>& hatavg,
                           const Buffer<double, 3>& hat,
                           const Buffer<double, 1>& fnm,
                           const Buffer<double, 1>& fnp,
                           const Expr jts, const Expr jte,
                           const Expr kts, const Expr kte,
                           const Expr its, const Expr ite)
{
    Var i("i"), k("k"), j("j");
    Func target(funcName);
    auto roi = jts <= j && j <= jte && 
               kts+1 <= k+1 && k+1 <= kte && 
               its <= i && i <= ite;
    target(i, k, j) = select(roi, 
        Expr(0.5) * ( fnm(k+1) * ( hat(i, k+1, j) + hat(i+1, k+1, j)) + 
                      fnp(k+1) * ( hat(i, k,   j) + hat(i+1, k,   j)) ),
        hatavg(i, k, j));
    return target;
}

extern "C" {
    void cal_deform_and_div_(double *hatavg, 
                             double *hat, 
                             double *fnm,
                             double *fnp,
                             const int* jts, const int* jte,
                             const int* kts, const int* kte,
                             const int* its, const int* ite );
}


int main(int argc, char** argv)
{
    const int jte = 1e4+2;
    const int jts = 2;
    const int kte = 1e4;
    const int kts = 0;
    const int ite = 1e4+2;
    const int its = 2;

    // const int jte = 8+2;
    // const int jts = 2;
    // const int kte = 8;
    // const int kts = 0;
    // const int ite = 8+2;
    // const int its = 2;


    const int j_range = jte-jts;
    const int k_range = kte-kts;
    const int i_range = ite-its;

    // --------------------------- Preparation --------------------------
    printf("Prepare the random data\n");
    bool with_gpu = true;
    
    Var j("j"), k("k"), i("i");
    Var bj, bk, bi, tj, tk, ti;

    Target gpu_target = find_gpu_target();
    if (!gpu_target.has_gpu_feature())
    {
        printf("Don't have a gpu!\n");
        with_gpu = false;
    }
    Target cpu_target = get_host_target();

    Func init3_cpu = init_nonzero<3>("init", i_range + 5 >= 8);
    Func init1_cpu = init_nonzero<1>("init", i_range + 1 >= 8);
    Func zero_cpu = set_zero<3>("zero", i_range + 5 >= 8);
    Func init3_gpu = init3_cpu;
    Func init1_gpu = init1_cpu;
    Func zero_gpu = zero_cpu;
    init3_cpu.compile_jit(cpu_target);
    init1_cpu.compile_jit(cpu_target);
    zero_cpu.compile_jit(cpu_target);
    if (with_gpu)
    {
        init3_gpu.compile_jit(gpu_target);
        init1_gpu.compile_jit(gpu_target);
        zero_gpu.compile_jit(gpu_target);
    }

    Buffer<double,3> hat({i_range+5, k_range+1, j_range+5}, "hat"), hat_g(hat);
    Buffer<double,1> fnm(k_range+1, "fnm"), fnp(k_range+1, "fnp"), fnm_g(fnm), fnp_g(fnp);

    // baseline input
    Buffer<double,3> out_base({i_range+5, k_range+1, j_range+5}, "hatavg");
    // halide cpu output
    Buffer<double,3> out_cpu({i_range+4, k_range, j_range+5}, "hatavg");
    // halide gpu output
    Buffer<double,3> out_gpu({i_range+4, k_range, j_range+5}, "hatavg");

    // --------------------------- advec_cell_kernel_loop91 kernel --------------------------
    printf("mdel3 kernel start!\n");

    // building baseline stencil
    Func cpu_fn = targetFunction("cal_deform_and_div_cpu", 
                                out_cpu, hat, fnm, fnp, 
                                Expr(jts), Expr(jte), 
                                Expr(kts), Expr(kte), 
                                Expr(its), Expr(ite));
    cpu_fn.parallel(j);
    if (i_range + 5 >= 8) cpu_fn.vectorize(i,8);
    cpu_fn.compile_jit(cpu_target);

    // IO initialization
    init3_cpu.realize(hat);
    init1_cpu.realize(fnm);
    init1_cpu.realize(fnp);
    zero_cpu.realize(out_base);

    // Calling baseline
    double *hat_ = hat.get()->begin();
    double *fnm_ = fnm.get()->begin();
    double *fnp_ = fnp.get()->begin();
    double *hatavg_ = out_base.get()->begin();
    cal_deform_and_div_(hatavg_, 
                        hat_, 
                        fnm_, 
                        fnp_, 
                        &jts, &jte, &kts, &kte, &its, &ite);
    
    // Calling halide cpu
    try {
        zero_cpu.realize(out_cpu);
        cpu_fn.realize(out_cpu);
    } catch (RuntimeError &e) {
        std::cerr << e.what();
        return 1;
    }

    // check result
    double correctness_1 = 0.0;
    for (int c = 0; c < j_range+5; c++) {
        for (int b = 0; b < k_range; b++) {
            for (int a = 0; a < i_range+4; a++) {
                double diff = abs(out_base(a,b+1,c) - out_cpu(a,b,c));
                if ( diff >= 1e-3 )
                {
                    printf("output1 [%d, %d, %d] = A: %lf | B: %lf\n", 
                            a, b, c, out_base(a,b+1,c), out_cpu(a,b,c));
                }
                correctness_1 += diff;
            }
        }
    }

    printf("The number for correctness_1 is %d\n",int(correctness_1));
    printf("output1 check point:\n");

    int times = 5;
    double cost_time = 0;
    struct timespec t1, t2, elapsed;
    clock_gettime(CLOCK_REALTIME, &t1);
    for (int i = 0; i < times; i++)
    {
        clock_gettime(CLOCK_REALTIME, &t1);
        cal_deform_and_div_(hatavg_, 
                            hat_, 
                            fnm_, 
                            fnp_, 
                            &jts, &jte, &kts, &kte, &its, &ite);
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("advec_cell_kernel_loop91 legacy code average cost time: %lfms\n\n",cost_time/times*1000);

    cost_time = 0;
    clock_gettime(CLOCK_REALTIME, &t1);
    for (int i = 0; i < times; i++)
    {
        clock_gettime(CLOCK_REALTIME, &t1);
        cpu_fn.realize(out_cpu);
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("advec_cell_kernel_loop91 lifted cpu average cost time: %lfms\n\n",cost_time/times*1000);


    if (with_gpu)
    {
        // building gpu func
        Func gpu_fn = targetFunction("cal_deform_and_div_gpu", 
                                    out_cpu, hat_g, fnm_g, fnp_g, 
                                    Expr(jts), Expr(jte), 
                                    Expr(kts), Expr(kte), 
                                    Expr(its), Expr(ite));
        // gpu_target.set_feature(Halide::Target::Debug);
        gpu_fn.gpu_tile(i, k, j, bi, bk, bj, ti, tk, tj, 8, 8, 8)
              .compile_jit(gpu_target);
        
        // GPU IO initialization
        init3_gpu.realize(hat_g);
        init1_gpu.realize(fnm_g);
        init1_gpu.realize(fnp_g);
        
        // warmup
        try {
            zero_gpu.realize(out_gpu);
            gpu_fn.realize(out_gpu);
        } catch ( RuntimeError &e ) {
            std::cerr << e.what();
            return -1;
        }

        cost_time = 0;
        clock_gettime(CLOCK_REALTIME, &t1);
        for (int i = 0; i < times; i++)
        {
            gpu_fn.realize(out_gpu);
        }
        out_gpu.copy_to_host();
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
        printf("advec_cell_kernel_loop91 lifted gpu average cost time: %lfms\n\n",cost_time/times*1000);
    }

    return 0;
}