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
                           const Buffer<double, 2>& msfux,
                           const Buffer<double, 2>& msfvy,
                           const Expr jts, const Expr jte,
                           const Expr its, const Expr ite)
{
    Var d1("i"), d2("j");
    Func target(funcName);
    target(d1,d2) = Expr(0.25) * 
        (msfux(d1,d2) + msfux(d1,d2+1)) * 
        (msfvy(d1,d2) + msfvy(d1+1,d2));

    return target;
}

extern "C" {
    void cal_deform_and_div_(double *mm,
                             double *msfux,
                             double *msfvy,
                             const int* jts, const int* jte,
                             const int* its, const int* ite );
}


int main(int argc, char** argv)
{
    const int jte = 512+2;
    const int jts = 2;
    const int ite = 512+2;
    const int its = 2;

    // const int jte = 128;
    // const int jts = 0;
    // const int ite = 128;
    // const int its = 0;

    const int j_range = jte-jts;
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

    Func init2_cpu = init_nonzero<2>("init", i_range + 1 >= 8);
    Func zero_cpu = set_zero<2>("zero", i_range + 1 >= 8);
    Func init2_gpu = init2_cpu;
    Func zero_gpu = zero_cpu;

    init2_cpu.compile_jit(cpu_target);
    zero_cpu.compile_jit(cpu_target);
    if (with_gpu)
    {
        init2_gpu.compile_jit(gpu_target);
        zero_gpu.compile_jit(gpu_target);
    }

    Buffer<double,2> msfux({i_range+1, j_range+2}, "msfux"), msfux_g(msfux);
    Buffer<double,2> msfvy({i_range+2, j_range+1}, "msfvy"), msfvy_g(msfvy);

    // baseline input
    Buffer<double,2> out_base({i_range+1, j_range+1}, "mm");
    // halide cpu output
    Buffer<double,2> out_cpu({i_range+1, j_range+1}, "mm");
    // halide gpu output
    Buffer<double,2> out_gpu({i_range+1, j_range+1}, "mm");

    // --------------------------- advec_cell_kernel_loop91 kernel --------------------------
    printf("mdel18 kernel start!\n");

    // building baseline stencil
    Func cpu_fn = targetFunction("cal_deform_and_div_cpu", 
                                msfux, msfvy, 
                                Expr(jts), Expr(jte), 
                                Expr(its), Expr(ite));
    cpu_fn.parallel(j);
    if (i_range + 5 >= 8) cpu_fn.vectorize(i,8);
    cpu_fn.compile_jit(cpu_target);

    // IO initialization
    init2_cpu.realize(msfux);
    init2_cpu.realize(msfvy);

    zero_cpu.realize(out_base);

    // Calling baseline
    double *msfux_ = msfux.get()->begin();
    double *msfvy_ = msfvy.get()->begin();
    double *mm_ = out_base.get()->begin();
    cal_deform_and_div_(mm_,
                        msfux_,
                        msfvy_,
                        &jts, &jte, &its, &ite);

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
    for (int c = 0; c < j_range+1; c++) {
        for (int a = 0; a < i_range+1; a++) {
            double val_a = out_base(a,c);
            double val_b = out_cpu(a,c);
            double diff = abs(val_a - val_b);
            if ( diff >= 1e-3 )
            {
                printf("output1 [%d, %d] = A: %lf | B: %lf\n", 
                        a, c, val_a, val_b);
            }
            correctness_1 += diff;
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
        cal_deform_and_div_(mm_,
                            msfux_,
                            msfvy_,
                            &jts, &jte, &its, &ite);
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("advec_cell_kernel_loop91 legacy code cost time: %lfms\n\n",cost_time/times*1000);

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
                                msfux_g, msfvy_g, 
                                Expr(jts), Expr(jte), 
                                Expr(its), Expr(ite));

        gpu_fn.gpu_tile(i, k, j, bi, bk, bj, ti, tk, tj, 8, 8, 8)
              .compile_jit(gpu_target);
        
        // GPU IO initialization
        init2_gpu.realize(msfux_g);
        init2_gpu.realize(msfvy_g);
        
        try {
            // warmup
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