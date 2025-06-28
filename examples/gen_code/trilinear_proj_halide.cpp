//g++ trilinear_proj_halide.cpp -o trilinear_proj_halide -I ~/software/Halide-14.0.0-x86-64-linux/include/ -lHalide -ldl -lpthread -O3 -std=c++17
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
    Var d1, d2, d3, fused;
    Func nz(funcName);
    nz(d1, d2, d3) = ((((d1+1 + (d2+1)*10 + (d3+1)*100)) + Expr(1e-5)) / Expr(100.0)) * cast<double>(10);
    nz.fuse(d2, d3, fused);
    nz.parallel(fused);
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
    set_zero.parallel(d1)
            .parallel(d2);
    if (vectorize) set_zero.vectorize(d1, 8);
    return set_zero;
}

template <>
inline Func set_zero<3>(const std::string& funcName, bool vectorize)
{
    Var d1, d2, d3, fuse;
    Func set_zero(funcName);
    set_zero(d1, d2, d3) = Expr(0.0);
    set_zero.fuse(d2, d3, fuse)
            .parallel(fuse);
    if (vectorize) set_zero.vectorize(d1, 8);
    return set_zero;
}

Func targetFunction( const std::string& funcName,
        Buffer<double,3> r, 
        const int m1k, 
        const int m2k, 
        const int m3k
    ){
    Var j1("j1"), j2("j2"), j3("j3");
    Var offset1, offset2, offset3;
    int d1, d2, d3;
    
    d1 = m1k == 3 ? 2: 1;
    d2 = m2k == 3 ? 2: 1;
    d3 = m3k == 3 ? 2: 1;
    Func input_convert("input_convert");
    input_convert(j1,j2,j3) = r(j1+1,j2+1,j3+1);
    Func j2i_func("j2i_func");
    j2i_func(j1,j2,j3,offset1,offset2,offset3) = input_convert(2*(j1+1)-d1+offset1,2*(j2+1)-d2+offset2,2*(j3+1)-d3+offset3);
    Func x1_func("x1_func");
    x1_func(j1,j2,j3,offset1) = j2i_func(j1,j2,j3,0+offset1,-1,0)+j2i_func(j1,j2,j3,0+offset1,1,0)+j2i_func(j1,j2,j3,0+offset1,0,-1)+j2i_func(j1,j2,j3,0+offset1,0,1);
    Func y1_func("y1_func");
    y1_func(j1,j2,j3,offset1) = j2i_func(j1,j2,j3,0+offset1,-1,-1)+j2i_func(j1,j2,j3,0+offset1,-1,1)+j2i_func(j1,j2,j3,0+offset1,1,-1)+j2i_func(j1,j2,j3,0+offset1,1,1);

    Expr y2 = j2i_func(j1,j2,j3,0,-1,-1)+j2i_func(j1,j2,j3,0,-1,1)+j2i_func(j1,j2,j3,0,1,-1)+j2i_func(j1,j2,j3,0,1,1);
    Expr x2 = j2i_func(j1,j2,j3,0,-1,0)+j2i_func(j1,j2,j3,0,1,0)+j2i_func(j1,j2,j3,0,0,-1)+j2i_func(j1,j2,j3,0,0,1);
    Func result_func("funcName");
    result_func(j1,j2,j3) = Expr(0.5) * j2i_func(j1,j2,j3,0,0,0)
                                + Expr(0.25) * (j2i_func(j1,j2,j3,-1,0,0)+j2i_func(j1,j2,j3,1,0,0)+x2)
                                + Expr(0.125) * (x1_func(j1,j2,j3,-1)+x1_func(j1,j2,j3,1)+y2)
                                + Expr(0.0625) * (y1_func(j1,j2,j3,-1)+y1_func(j1,j2,j3,1));
    return result_func;
}

extern "C" {
    void trilinear_proj_rprj3_(
        double *r, 
        const int *m1k,
        const int *m2k,
        const int *m3k,
        double *s,
        const int *m1j,
        const int *m2j,
        const int *m3j,
        const int *m
    );
}

int main(int argc, char **argv)
{
    int m1k = 1030;
    int m2k = 1030;
    int m3k = 1030;
    int m1j = 512;
    int m2j = 512;
    int m3j = 512;
    int m = 1035;
    // int m1k = 12;
    // int m2k = 12;
    // int m3k = 12;
    // int m1j = 4;
    // int m2j = 4;
    // int m3j = 4;
    // int m = 15;

    // --------------------------- Preparation --------------------------
    bool with_gpu = true;
    Var fused_index, j1("j1"), j2("j2"), j3("j3"), j1o,j2o,j3o,j1i,j2i,j3i;

    Target gpu_target = find_gpu_target();
    if (!gpu_target.has_gpu_feature())
    {
        printf("Don't have a gpu!\n");
        with_gpu = false;
    }
    Target cpu_target = get_host_target();

    Func i3_cpu = init_nonzero<3>("input_random_3d", (m1j-2) > 8);
    Func z1_cpu = set_zero<1>("set_zero_1d", m > 8);
    Func z3_cpu = set_zero<3>("set_zero", (m1j-2) > 8);
    Func i3_gpu(i3_cpu), z3_gpu(z3_cpu);
    i3_cpu.compile_jit(cpu_target);
    z3_cpu.compile_jit(cpu_target);
    if ( with_gpu ) {
       i3_gpu.compile_jit(gpu_target);
       z3_gpu.compile_jit(gpu_target);
    }

    Buffer<double, 3> input({m1k,m2k,m3k}, "input");
    Buffer<double, 3> input_g(input);

    // Buffer<double, 1> x1(m, "x1"), y1(m, "y1");

    Buffer<double> out_base({m1j,m2j,m3j}, "out_base");
    Buffer<double> out_cpu({m1j-2,m2j-2,m3j-2}, "out_cpu");
    Buffer<double> out_gpu({m1j-2,m2j-2,m3j-2}, "out_gpu");


    // --------------------------- trilinear_proj kernel --------------------------
    printf("trilinear_proj start!\n");
    // gpu_target.set_feature(Halide::Target::Debug);

    auto res_cpu = targetFunction("res_cpu", input, m1k, m2k, m3k);
    try {
    res_cpu.fuse(j2,j3,fused_index);
    res_cpu.parallel(fused_index);
    if ((m1j-2) > 8) res_cpu.vectorize(j1,8);
    res_cpu.compile_jit(cpu_target);
    } catch ( CompileError &e ) {
        std::cerr << e.what();
        exit(1);
    }

    // z1_cpu.realize(x1);
    // z1_cpu.realize(y1);
    i3_cpu.realize(input);
    z3_cpu.realize(out_base);

    trilinear_proj_rprj3_(
        input.get()->begin(),
        &m1k, &m2k, &m3k,
        out_base.get()->begin(),
        &m1j, &m2j, &m3j, &m
    );

    try {
        i3_cpu.realize(out_cpu);
        res_cpu.realize(out_cpu);
    } catch ( RuntimeError &e ) {
        std::cerr << e.what();
        exit(1);
    }

    double correctness_1 = 0.0;
    for (int j = 0; j < m3j-2; j++) {
        for (int k = 0; k < m2j-2; k++) {
            for (int i = 0; i < m1j-2; i++) {
                int a = i+1;
                int b = k+1;
                int c = j+1;
                auto A = out_base(a,b,c);
                auto B = out_cpu(i,k,j);
                double diff1 = abs(A-B);
                if (diff1 >= 1e-3){
                    printf("[%d, %d, %d]\n", i,k,j);
                }
                correctness_1 += diff1;
            }
        }
    }
    printf("The number for correctness_1 is %d\n",int(correctness_1/(m3j*m2j*m1j)));

    int times = 5;
    double cost_time = 0;
    struct timespec t1, t2, elapsed;
    clock_gettime(CLOCK_REALTIME, &t1);
    for (int i = 0; i < times; i++)
    {
        clock_gettime(CLOCK_REALTIME, &t1);
        trilinear_proj_rprj3_(
            input.get()->begin(),
            &m1k, &m2k, &m3k,
            out_base.get()->begin(),
            &m1j, &m2j, &m3j, &m
        );
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("trilinear_proj legacy code cost time: %lfms\n\n",cost_time/times*1000);

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
    printf("trilinear_proj lifted cpu cost time: %lfms\n\n",cost_time/times*1000);
    
    if ( with_gpu ) {
        auto res_gpu = targetFunction("res_gpu", input_g, m1k, m2k, m3k);
        res_gpu.gpu_tile(j1,j2,j3,j1o,j2o,j3o,j1i,j2i,j3i,8,8,8);
        res_gpu.compile_jit(gpu_target);

        i3_cpu.realize(input);

        try {
            z3_cpu.realize(out_base);
            res_gpu.realize(out_gpu);
        } catch ( RuntimeError &e ) {
            std::cerr << e.what();
            exit(2);
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
        printf("trilinear_proj lifted gpu cost time: %lfms\n\n",cost_time/times*1000);
    }

    return 0;
}