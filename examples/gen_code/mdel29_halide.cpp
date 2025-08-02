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

void targetFunction(Func& elb, Func& elf,
                    Buffer<double, 3> dthrdn,
                    Buffer<double, 3> qtke,
                    Buffer<double, 2> sflux,
                    Buffer<double, 2> elt) {

    Var i("i"), k("k"), j("j");

    Expr g(9.81), alp2(.5), alp3(.5);
    Expr gtr = g/300;

    auto n2 = gtr * dthrdn(i,k,j);
    auto qcv = pow(gtr * max(sflux(i,j), Expr(0.)) * elt(i,j), Expr(1.0/3.0));
    
    auto elb_br = qtke(i,k,j)/sqrt(n2)*(alp2 + alp3*sqrt(qcv/(elt(i,j)*sqrt(n2))));
    auto elf_br = alp2*qtke(i,k,j)/sqrt(n2);

    elb(i,k,j) = select(
        dthrdn(i,k,j) > Expr(0.),
        elb_br,
        Expr(1.e10) 
    );

    elf(i,k,j) = select(
       dthrdn(i,k,j) > Expr(0.), 
       elf_br,
       Expr(1.e10)
    );
}

extern "C" {
    void meso_length_scale_(
        double* elb, double* elf,
        double* dthrdn, double* qtke,
        double* sflux, double* elt,
        const int* jts, const int* jte, 
        const int* kts, const int* kte, 
        const int* its, const int* ite
    );
}

#ifndef _3D_1
#define _3D_1 512
#define _3D_2 512
#define _3D_3 512
#endif

int main(int argc, char** argv)
{
    const int its = 0;              //changed to 1024
    const int ite = _3D_1;
    const int jts = 0;
    const int jte = _3D_2;
    const int kts = 0;
    const int kte = _3D_3 / 4;

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
    Buffer<double, 3> dthrdn({in_d1_range, in_d2_range, in_d3_range}, "dthrdn"), dthrdn_g(dthrdn);
    Buffer<double, 3> qtke({in_d1_range, in_d2_range, in_d3_range}, "qtke"), qtke_g(qtke);
    Buffer<double, 2> sflux({in_d1_range, in_d3_range}, "sflux"), sflux_g(sflux);
    Buffer<double, 2> elt({in_d1_range, in_d3_range}, "elt"), elt_g(elt);

    // Fortran output
    Buffer<double, 3> elb_base({out_d1_range, out_d2_range, out_d3_range}, "elb_raw"); // elb
    Buffer<double, 3> elf_base({out_d1_range, out_d2_range, out_d3_range}, "elf_raw"); // elf

    // Halide CPU output
    Buffer<double, 3> elb_cpu({out_d1_range, out_d2_range, out_d3_range}, "elb"); // elb
    Buffer<double, 3> elf_cpu({out_d1_range, out_d2_range, out_d3_range}, "elf"); // elf

    // Halide GPU roiput
    Buffer<double, 3> elb_gpu({out_d1_range, out_d2_range, out_d3_range}, "elb"); // elb
    Buffer<double, 3> elf_gpu({out_d1_range, out_d2_range, out_d3_range}, "elf"); // elf

    // --------------------------- horizontal_diffusion_u_2 kernel --------------------------
    printf("horizontal_diffusion_u_2 kernel start!\n");

    // Coordinate transformations for proper indexing
    Func elbfn_cpu("meso_length_scale_elb");
    Func elffn_cpu("meso_length_scale_elf");
    targetFunction(elbfn_cpu, elffn_cpu, 
                dthrdn, 
                qtke, 
                sflux, 
                elt);

    // CPU optimizations
    elbfn_cpu.parallel(d3);
    if (d1_range >= 8) elbfn_cpu.vectorize(d1,8);
    elbfn_cpu.compile_jit(cpu_target);

    elffn_cpu.parallel(d3);
    if (d1_range >= 8) elffn_cpu.vectorize(d1,8);
    elffn_cpu.compile_jit(cpu_target);
    
    i3_cpu.realize(dthrdn);
    i3_cpu.realize(qtke);
    i2_cpu.realize(sflux);
    i2_cpu.realize(elt);

    z3_cpu.realize(elb_base);
    z3_cpu.realize(elf_base);

    meso_length_scale_(
        elb_base.get()->begin(), 
        elf_base.get()->begin(), 
        dthrdn.get()->begin(),
        qtke.get()->begin(),
        sflux.get()->begin(),
        elt.get()->begin(), 
        &its, &ite, &jts, &jte, &kts, &kte
    );

    // CPU execution
    try {
        z3_cpu.realize(elb_cpu);
        z3_cpu.realize(elf_cpu);
        elbfn_cpu.realize(elb_cpu);
        elffn_cpu.realize(elf_cpu);
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
                auto A = elb_cpu(a,b,c);
                auto B = elb_cpu(i,k,j);
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
            elb_base.get()->begin(), 
            elf_base.get()->begin(), 
            dthrdn.get()->begin(),
            qtke.get()->begin(),
            sflux.get()->begin(),
            elt.get()->begin(), 
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
        elbfn_cpu.realize(elb_cpu);
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("horizontal_diffusion_u_2 lifted cpu average cost time: %lfms\n\n",cost_time/times*1000);



    if ( with_gpu )
    {
        Func elbfn_gpu("meso_length_scale_elb");
        Func elffn_gpu("meso_length_scale_elf");
        targetFunction(elbfn_gpu, elffn_gpu, 
                    dthrdn_g, 
                    qtke_g, 
                    sflux_g, 
                    elt_g);

        // GPU optimizations
        elbfn_gpu.gpu_tile(d1,d2,d3,d1o,d2o,d3o,d1i,d2i,d3i,8,8,4)
                .compile_jit(gpu_target);
        elffn_gpu.gpu_tile(d1,d2,d3,d1o,d2o,d3o,d1i,d2i,d3i,8,8,4)
                .compile_jit(gpu_target);

        // IO initialization
        i3_gpu.realize(dthrdn_g);
        i3_gpu.realize(qtke_g);
        i2_gpu.realize(sflux_g);
        i2_gpu.realize(elt_g);

        // GPU warm up
        try {
            z3_gpu.realize(elb_gpu);
            z3_gpu.realize(elf_gpu);
            elbfn_gpu.realize(elb_gpu);
            elffn_gpu.realize(elf_gpu);
        } catch ( RuntimeError &e ) {
            std::cerr << e.what();
            return 2;
        }

        cost_time = 0;
        clock_gettime(CLOCK_REALTIME, &t1);
        for (int i = 0; i < times; i++)
        {
            elbfn_gpu.realize(elb_gpu);
        }
        elb_gpu.copy_to_host();
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
        printf("horizontal_diffusion_u_2 lifted gpu average cost time: %lfms\n\n",cost_time/times*1000);
    }
    return 0;
} 