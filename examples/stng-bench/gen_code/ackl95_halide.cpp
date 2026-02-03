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

extern "C" {

void advec_cell_kernel_loop95_(
    double *post_vol,
    double *pre_vol,
    double *vol_flux_x,
    double *volume,
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

int main(int argc, char** argv)
{
    printf("Caller Start!\n");
    const int x_max = _2D_1;
    const int x_min = 0;
    const int y_max = _2D_2;
    const int y_min = 0;
   
    const int d1_base_range = x_max-x_min+1; // change along with different kernels
    const int d2_base_range = y_max-y_min+1; // change along with different kernels
    const int output_d1_range = d1_base_range+4;  // change along with different kernels
    const int output_d2_range = d2_base_range+4; // change along with different kernels

    // --------------------------- Preparation --------------------------
    printf("Prepare the random data\n");
    Var d1, d2, d1o, d2o, d1i, d2i;

    bool with_gpu = true;
    Target gpu_target = find_gpu_target();
    if (!gpu_target.has_gpu_feature())
    {
        printf("Don't have a gpu!\n");
        with_gpu = false;
    }
    Target cpu_target = get_host_target();

    Func i1_cpu("i1_cpu");
    int input1_d1_base_range = d1_base_range+4; // change along with different kernels
    i1_cpu(d1,d2) = (((((d2) * Expr(input1_d1_base_range) + d1) % 13) + Expr(0.1)) / Expr(13.0)) * cast<double>(10);
    Func i1_gpu(i1_cpu);

    Func i2_cpu("i2_cpu");
    int input2_d1_base_range = d1_base_range+4; // change along with different kernels
    i2_cpu(d1,d2) = (((((d2) * Expr(input2_d1_base_range) + d1) % 13) + Expr(0.2)) / Expr(13.0)) * cast<double>(10);
    Func i2_gpu(i2_cpu);

    Func z_cpu("z_cpu");
    z_cpu(d1,d2) = Expr(0.0);
    Func z_gpu(z_cpu);

    i1_cpu.compile_jit(cpu_target);
    i2_cpu.compile_jit(cpu_target);
    z_cpu.compile_jit(cpu_target);

    if (with_gpu)
    {
        i1_gpu.compile_jit(gpu_target);
        i2_gpu.compile_jit(gpu_target);
        z_gpu.compile_jit(gpu_target);
    }

    Buffer<double> vol_flux_y({d1_base_range+4,d2_base_range+5}, "vol_flux_y"), 
                   vol_flux_y_g(vol_flux_y);

    Buffer<double> volume({d1_base_range+4,d2_base_range+4}, "volume"),
                   volume_g(volume);

    // --------------------------- advec_cell_kernel_loop95 kernel -------------------------- // change along with different kernels
    printf("advec_cell_kernel_loop95 start!\n"); // change along with different kernels
    // gpu_target.set_feature(Halide::Target::Debug);

    Buffer<double, 2> out1_base({output_d1_range+1,output_d2_range+1}, "out1");
    Buffer<double, 2> out2_base({output_d1_range+1,output_d2_range+1}, "out2");

    Buffer<double, 2> out1_cpu({output_d1_range,output_d2_range}, "out1");
    Buffer<double, 2> out2_cpu({output_d1_range,output_d2_range}, "out2");

    Buffer<double, 2> out1_gpu({output_d1_range,output_d2_range}, "out1");
    Buffer<double, 2> out2_gpu({output_d1_range,output_d2_range}, "out2");

    // ------- result1_func --------
    Func result2_func_cpu("result2_func_cpu");
    result2_func_cpu(d1,d2) = volume(d1,d2) + vol_flux_y(d1,d2+1)-vol_flux_y(d1,d2);

    result2_func_cpu.parallel(d2);
    if (output_d1_range >= 8) result2_func_cpu.vectorize(d1,8);
    result2_func_cpu.compile_jit(cpu_target);

    // ------- result2_func --------
    Func result1_func_cpu("result1_func_cpu");
    result1_func_cpu(d1,d2) = volume(d1,d2);

    result1_func_cpu.parallel(d2);
    if (output_d1_range >= 8) result1_func_cpu.vectorize(d1,8);
    result1_func_cpu.compile_jit(cpu_target);

    i1_cpu.realize(vol_flux_y);
    i2_cpu.realize(volume);

    z_cpu.realize(out1_base);
    z_cpu.realize(out2_base);

    advec_cell_kernel_loop95_(
        out1_base.get()->begin(),
        out2_base.get()->begin(),
        vol_flux_y.get()->begin(),
        volume.get()->begin(),
        &x_max,
        &x_min,
        &y_max,
        &y_min  
    );
    
    z_cpu.realize(out1_cpu);
    z_cpu.realize(out2_cpu);

    result1_func_cpu.realize(out1_cpu);
    result2_func_cpu.realize(out2_cpu);
    double correctness_1 = 0.0;
    double correctness_2 = 0.0;
    for (int b = 0; b < output_d2_range; b++) {
        for (int a = 0; a < output_d1_range; a++) {
            auto A = out2_base(a,b);
            auto B = out2_cpu(a,b);
            auto diff = abs(A-B);

            if (diff >= 1e-3)
            {
                printf("output [%d, %d] = Fortran: %lf | Halide: %lf\n", 
                        a, b, A, B);
            }
        }
    }
    
    int times = 5;
    double cost_time;
    struct timespec t1, t2, elapsed;


    cost_time = 0;
    clock_gettime(CLOCK_REALTIME, &t1);
    for (int i = 0; i < times; i++)
    {
        clock_gettime(CLOCK_REALTIME, &t1);
        advec_cell_kernel_loop95_(
            out1_base.get()->begin(),
            out2_base.get()->begin(),
            vol_flux_y.get()->begin(),
            volume.get()->begin(),
            &x_max,
            &x_min,
            &y_max,
            &y_min  
        );
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("legacy code: %lfms\n\n",cost_time/times*1000); 


    cost_time = 0;
    clock_gettime(CLOCK_REALTIME, &t1);
    for (int i = 0; i < times; i++)
    {
        clock_gettime(CLOCK_REALTIME, &t1);
        result1_func_cpu.realize(out1_cpu);
        result2_func_cpu.realize(out2_cpu);
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("lifted cpu: %lfms\n\n",cost_time/times*1000); 

    if (with_gpu)
    {
        Func result2_func_gpu("result2_func_gpu");
        result2_func_gpu(d1,d2) = volume_g(d1,d2) + vol_flux_y_g(d1,d2+1) - vol_flux_y_g(d1,d2);

        Func result1_func_gpu("result1_func_gpu");
        result1_func_gpu(d1,d2) = volume_g(d1,d2);

        result1_func_gpu.gpu_tile(d1,d2,d1o,d2o,d1i,d2i,8,8);
        result1_func_gpu.compile_jit(gpu_target);

        result2_func_gpu.gpu_tile(d1,d2,d1o,d2o,d1i,d2i,8,8);
        result2_func_gpu.compile_jit(gpu_target);

        i1_gpu.realize(vol_flux_y_g);
        i2_gpu.realize(volume_g);

        z_gpu.realize(out1_gpu);
        z_gpu.realize(out2_gpu);

        cost_time = 0;
        times = 100;
        clock_gettime(CLOCK_REALTIME, &t1);
        for (int i = 0; i < times; i++)
        {
            result1_func_gpu.realize(out1_gpu);
            result2_func_gpu.realize(out2_gpu);
        }
        out2_gpu.copy_to_host();
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time = toSec(elapsed);
        printf("lifted gpu: %lfms\n\n", cost_time/times*1000);
    }

    return 0;
}