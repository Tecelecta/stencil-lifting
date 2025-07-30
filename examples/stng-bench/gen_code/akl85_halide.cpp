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

Func targetFunction(Buffer<double,2>& xarea, 
                    Buffer<double,2>& viscosity, 
                    Buffer<double,2>& stepbymass,
                    Buffer<double,2>& xvel1)
{
    Var d1("j"), d2("k");
    Func viscosity_cnvrt("viscosity_convert");
    viscosity_cnvrt(d1,d2) = viscosity(d1+2,d2+2);
    Func stepbymass_cnvrt("stepbymass_convert");
    stepbymass_cnvrt(d1,d2) = stepbymass(d1+2,d2+2);
    Func xvel1_cnvrt("xvel1_convert");
    xvel1_cnvrt(d1,d2) = xvel1(d1+2,d2+2);
    Func xarea_cnvrt("xarea_convert");
    xarea_cnvrt(d1,d2) = xarea(d1+2,d2+2);

    Func result_func("xvel1_fn");
    result_func(d1,d2) = xvel1_cnvrt(d1,d2)-stepbymass_cnvrt(d1,d2) 
                        * (xarea_cnvrt(d1,d2) * (viscosity_cnvrt(d1,d2)-viscosity_cnvrt(d1-1,d2))
                         + xarea_cnvrt(d1,d2-1) * (viscosity_cnvrt(d1,d2-1)-viscosity_cnvrt(d1-1,d2-1)));

    return result_func;
}

extern "C" {
    void accelerate_kernel_loop85_(
        double *stepbymass,
        double *viscosity,
        double *xarea,
        double *xvel1,
        const int *x_max,
        const int *x_min,
        const int *y_max,
        const int *y_min
    );
}


#ifndef _2D_1
#define _2D_1 2e4
#define _2D_2 2e4
#endif

int main(int argc, char **argv)
{
    printf("Caller Start!\n");
    const int x_max = _2D_1;
    const int x_min = 0;
    const int y_max = _2D_2;
    const int y_min = 0;

    // --------------------------- Preparation --------------------------
    printf("Prepare the random data\n");
    Var d1("j"), d2("k"), d1o, d2o, d1i, d2i;

    bool with_gpu = true;
    Target gpu_target = find_gpu_target();
    if (!gpu_target.has_gpu_feature())
    {
        printf("Don't have a gpu!\n");
        with_gpu = false;
    }
    Target cpu_target = get_host_target();

    int d1_base_range = x_max-x_min;
    int d2_base_range = y_max-y_min;

    Func i1_cpu("i1_cpu");
    i1_cpu(d1,d2) = (((((d2) * Expr(d1_base_range+5) + d1) % 13) + Expr(0.1)) / Expr(13.0)) * cast<double>(10);
    Func i1_gpu(i1_cpu);

    Func z_cpu("z_cpu");
    z_cpu(d1,d2) = Expr(0.0);
    Func z_gpu(z_cpu);

    Buffer<double, 2> viscosity({d1_base_range+5,d2_base_range+5}, "viscosity"), viscosity_g(viscosity);
    Buffer<double, 2> stepbymass({d1_base_range+6,d2_base_range+6}, "stepbymass"), stepbymass_g(stepbymass);
    Buffer<double, 2> xarea({d1_base_range+6,d2_base_range+5}, "xarea"), xarea_g(xarea);
    Buffer<double, 2> xvel1({d1_base_range+6,d2_base_range+6}, "xvel1"), xvel1_g(xvel1);

    // --------------------------- accelerate_kernel_loop85 kernel --------------------------
    printf("accelerate_kernel_loop85 start!\n");

    int output_d1_range = d1_base_range+2;
    int output_d2_range = d2_base_range+2;

    Buffer<double, 2> out_base(d1_base_range+6,d2_base_range+6);
    Buffer<double, 2> out_cpu(output_d1_range,output_d2_range);
    Buffer<double, 2> out_gpu(output_d1_range,output_d2_range);

    Func result_func_cpu = targetFunction(xarea, viscosity, stepbymass, xvel1);

    try
    {
        result_func_cpu.parallel(d2);
        if (output_d1_range >= 8) result_func_cpu.vectorize(d1,8);
        result_func_cpu.compile_jit(cpu_target);
    }
    catch (CompileError &e)
    {
        std::cerr << e.what();
        return 1;
    }

    i1_cpu.realize(viscosity);
    i1_cpu.realize(stepbymass);
    i1_cpu.realize(xarea);
    
    z_cpu.realize(out_base);
    accelerate_kernel_loop85_(
        stepbymass.get()->begin(),
        viscosity.get()->begin(),
        xarea.get()->begin(),
        out_base.get()->begin(),
        &x_max,
        &x_min,
        &y_max,
        &y_min
    );
    
    z_cpu.realize(xvel1);
    z_cpu.realize(out_cpu);
    result_func_cpu.realize(out_cpu);

    double correctness_1 = 0.0;
    for (int b = 0; b < output_d2_range; b++) {
        for (int a = 0; a < output_d1_range; a++) {
            auto A = out_base(a+2,b+2);
            auto B = out_cpu(a,b);
            auto diff = abs(A-B);

            if (diff >= 1e-3)
            {
                printf("output [%d, %d] = Fortran: %lf | Halide: %lf\n", 
                        a, b, A, B);
            }
        }
    } 

    int times = 5;
    double cost_time = 0;
    struct timespec t1, t2, elapsed;
    clock_gettime(CLOCK_REALTIME, &t1);
    for (int i = 0; i < times; i++)
    {
        clock_gettime(CLOCK_REALTIME, &t1);
        accelerate_kernel_loop85_(
            stepbymass.get()->begin(),
            viscosity.get()->begin(),
            xarea.get()->begin(),
            out_base.get()->begin(),
            &x_max,
            &x_min,
            &y_max,
            &y_min
        );
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("legacy code time: %lfms\n\n",cost_time/times*1000);

    cost_time = 0;
    clock_gettime(CLOCK_REALTIME, &t1);
    for (int i = 0; i < times; i++)
    {
        clock_gettime(CLOCK_REALTIME, &t1);
        result_func_cpu.realize(out_cpu);
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("lifted cpu average cost time: %lfms\n\n",cost_time/times*1000);

    if (with_gpu)
    {
        Func result_func_gpu = targetFunction(xarea, viscosity, stepbymass, xvel1);
        result_func_gpu.gpu_tile(d1,d2,d1o,d2o,d1i,d2i,8,8);
        result_func_gpu.compile_jit(gpu_target);

        i1_gpu.realize(viscosity_g);
        i1_gpu.realize(stepbymass_g);
        i1_gpu.realize(xarea_g);

        z_gpu.realize(xvel1_g);
        z_gpu.realize(out_gpu);
        result_func_gpu.realize(out_gpu);

        cost_time = 0;
        times = 100;
        clock_gettime(CLOCK_REALTIME, &t1);
        for (int i = 0; i < times; i++)
        {
            result_func_gpu.realize(out_gpu);
        }
        out_gpu.copy_to_host();
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time = toSec(elapsed);
        printf("lifted gpu: %lfms\n\n", cost_time/times*1000);
    }
    return 0;
}
