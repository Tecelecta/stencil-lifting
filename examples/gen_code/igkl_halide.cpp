#include "Halide.h"
#include <cstdio>
#include <ctime>
#include <algorithm>

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
    Target target = get_host_target();
    std::vector<Target::Feature> features_to_try;
    features_to_try.push_back(Target::CUDA);
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
inline Func init_nonzero<2>(const std::string& funcName, bool vectorize)
{
    Var d1, d2;
    Func nz(funcName);
    nz(d1, d2) = ((((d1 + d2*10)) + Expr(1e-5)) / Expr(100.0)) * cast<double>(10);
    return nz;
}

template <>
inline Func set_zero<2>(const std::string& funcName, bool vectorize)
{
    Var d1, d2;
    Func set_zero(funcName);
    set_zero(d1, d2) = Expr(0.0);
    return set_zero;
}

void ideal_gas_kernel_halide(Func& pressure_func, Func& soundspeed_func,
                            Buffer<double, 2> density,
                            Buffer<double, 2> energy,
                            int x_min, int x_max, int y_min, int y_max) {
    Var j("j"), k("k");
    Expr j_offset = Expr(2); // x_min-2
    Expr k_offset = Expr(2); // y_min-2
    auto j_buf = j + j_offset;
    auto k_buf = k + k_offset;
    Expr gamma = Expr(1.4);
    auto v = Expr(1.0) / density(j_buf, k_buf);
    auto pressure = (gamma - Expr(1.0)) * density(j_buf, k_buf) * energy(j_buf, k_buf);
    auto pressurebyenergy = (gamma - Expr(1.0)) * density(j_buf, k_buf);
    auto pressurebyvolume = -density(j_buf, k_buf) * pressure;
    auto sound_speed_squared = v * v * (pressure * pressurebyenergy - pressurebyvolume);
    pressure_func(j, k) = pressure;
    soundspeed_func(j, k) = sqrt(sound_speed_squared);
}

extern "C" {
    void ideal_gas_kernel_(
        int* x_min, int* x_max, int* y_min, int* y_max,
        double* density, double* energy, double* pressure, double* soundspeed
    );
}

#ifndef _2D_1
#define _2D_1 2e4
#define _2D_2 2e4
#endif

int main(int argc, char** argv)
{
    const int x_min = 0;
    const int x_max = _2D_1;
    const int y_min = 0;
    const int y_max = _2D_2;
    const int arr_x_size = (x_max + 2) - (x_min - 2) + 1;
    const int arr_y_size = (y_max + 2) - (y_min - 2) + 1;

    bool with_gpu = true;
    Var d1("j"), d2("k"), d1o, d2o, d1i, d2i;

    Target gpu_target = find_gpu_target();
    if (!gpu_target.has_gpu_feature())
    {
        printf("Don't have a gpu!\n");
        with_gpu = false;
    }
    Target cpu_target = get_host_target();

    auto i2_cpu = init_nonzero<2>("init2d", arr_x_size >= 8);
    auto z2_cpu = set_zero<2>("zero2d", arr_x_size >= 8);
    auto i2_gpu = i2_cpu;
    auto z2_gpu = z2_cpu;

    i2_cpu.compile_jit(cpu_target);
    z2_cpu.compile_jit(cpu_target);
    if (with_gpu) {
        i2_gpu.compile_jit(gpu_target);
        z2_gpu.compile_jit(gpu_target);
    }

    Buffer<double, 2> density({arr_x_size, arr_y_size}, "density"), density_g(density);
    Buffer<double, 2> energy({arr_x_size, arr_y_size}, "energy"), energy_g(energy);
    Buffer<double, 2> pressure({arr_x_size, arr_y_size}, "pressure"), pressure_g(pressure);
    Buffer<double, 2> soundspeed({arr_x_size, arr_y_size}, "soundspeed"), soundspeed_g(soundspeed);
    
    Buffer<double, 2> pressure_base({arr_x_size, arr_y_size}, "pressure_base");
    Buffer<double, 2> soundspeed_base({arr_x_size, arr_y_size}, "soundspeed_base");

    const int output_j_size = x_max - x_min + 1;
    const int output_k_size = y_max - y_min + 1;

    Buffer<double, 2> pressure_cpu({output_j_size, output_k_size}, "pressure_cpu");
    Buffer<double, 2> soundspeed_cpu({output_j_size, output_k_size}, "soundspeed_cpu");

    Buffer<double, 2> pressure_gpu({output_j_size, output_k_size}, "pressure_gpu");
    Buffer<double, 2> soundspeed_gpu({output_j_size, output_k_size}, "soundspeed_gpu");

    printf("kernel start!\n");

    Func pressure_func_cpu("pressure_func_cpu");
    Func soundspeed_func_cpu("soundspeed_func_cpu");
    
    ideal_gas_kernel_halide(pressure_func_cpu, soundspeed_func_cpu, 
                            density, energy, 
                            x_min, x_max, y_min, y_max);

    pressure_func_cpu.parallel(d2);
    soundspeed_func_cpu.parallel(d2);
    if (arr_x_size >= 8) {
        pressure_func_cpu.vectorize(d1, 8);
        soundspeed_func_cpu.vectorize(d1, 8);
    }
    pressure_func_cpu.compile_jit(cpu_target);
    soundspeed_func_cpu.compile_jit(cpu_target);

    i2_cpu.realize(density);
    i2_cpu.realize(energy);

    z2_cpu.realize(pressure_base);
    z2_cpu.realize(soundspeed_base);

    int x_min_i = x_min, x_max_i = x_max, y_min_i = y_min, y_max_i = y_max;
    ideal_gas_kernel_(&x_min_i, &x_max_i, &y_min_i, &y_max_i,
                     density.get()->begin(), 
                     energy.get()->begin(),
                     pressure_base.get()->begin(), 
                     soundspeed_base.get()->begin());

    try {
        z2_cpu.realize(pressure_cpu);
        z2_cpu.realize(soundspeed_cpu);

        pressure_func_cpu.realize(pressure_cpu);
        soundspeed_func_cpu.realize(soundspeed_cpu);
    } catch (RuntimeError &e) {
        std::cerr << e.what();
        return 1;
    }

    double correctness_p = 0.0, correctness_s = 0.0;
    int count = 0;
    for (int k = y_min; k <= y_max; k++) {
        for (int j = x_min; j <= x_max; j++) {
            int fort_idx_j = j - (x_min - 2);
            int fort_idx_k = k - (y_min - 2);

            int hal_idx_j = j - x_min;
            int hal_idx_k = k - y_min;

            auto p_diff = abs(pressure_base(fort_idx_j, fort_idx_k) - pressure_cpu(hal_idx_j, hal_idx_k));
            auto s_diff = abs(soundspeed_base(fort_idx_j, fort_idx_k) - soundspeed_cpu(hal_idx_j, hal_idx_k));
            
            if (p_diff >= 1e-6) {
                printf("pressure difference [%d, %d]: Fort=%lf | Hali=%lf\n", j, k, pressure_base(fort_idx_j, fort_idx_k), pressure_cpu(hal_idx_j, hal_idx_k));
            }
            if (s_diff >= 1e-6) {
                printf("soundspeed difference [%d, %d]: Fort=%lf | Hali=%lf\n", j, k, soundspeed_base(fort_idx_j, fort_idx_k), soundspeed_cpu(hal_idx_j, hal_idx_k));
            }

            correctness_p += p_diff;
            correctness_s += s_diff;
            count++;
        }
    }
    printf("pressure average error: %e\n", correctness_p / count);
    printf("soundspeed average error: %e\n", correctness_s / count);

    int times = 20;
    double cost_time = 0;
    struct timespec t1, t2, elapsed;

    cost_time = 0;
    clock_gettime(CLOCK_REALTIME, &t1);
    for (int i = 0; i < times; i++) {
        ideal_gas_kernel_(&x_min_i, &x_max_i, &y_min_i, &y_max_i,
                         density.get()->begin(), 
                         energy.get()->begin(),
                         pressure_base.get()->begin(), 
                         soundspeed_base.get()->begin());
    }
    clock_gettime(CLOCK_REALTIME, &t2);
    timespec_diff(t2, t1, elapsed);
    cost_time += toSec(elapsed);
    printf("legacy code average cost time: %lfms\n\n", cost_time/times*1000);

    cost_time = 0;
    for (int i = 0; i < times; i++) {
        clock_gettime(CLOCK_REALTIME, &t1);
        pressure_func_cpu.realize(pressure_cpu);
        soundspeed_func_cpu.realize(soundspeed_cpu);
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("lifted cpu average cost time: %lfms\n\n", cost_time/times*1000);

    if (with_gpu) {
        Func pressure_func_gpu("pressure_gpu");
        Func soundspeed_func_gpu("soundspeed_gpu");

        ideal_gas_kernel_halide(pressure_func_gpu, soundspeed_func_gpu, 
                                density_g, energy_g, 
                                x_min, x_max, y_min, y_max);

        pressure_func_gpu.gpu_tile(d1, d2, d1o, d2o, d1i, d2i, 16, 16)
                         .compile_jit(gpu_target);
        soundspeed_func_gpu.gpu_tile(d1, d2, d1o, d2o, d1i, d2i, 16, 16)
                           .compile_jit(gpu_target);

        i2_gpu.realize(density_g);
        i2_gpu.realize(energy_g);
        z2_gpu.realize(pressure_g);
        z2_gpu.realize(soundspeed_g);

        try {
            pressure_func_gpu.realize(pressure_gpu);
            soundspeed_func_gpu.realize(soundspeed_gpu);
        } catch (RuntimeError &e) {
            std::cerr << e.what();
            return 2;
        }

        cost_time = 0;
        clock_gettime(CLOCK_REALTIME, &t1);
        for (int i = 0; i < times; i++) {
            pressure_func_gpu.realize(pressure_gpu);
            soundspeed_func_gpu.realize(soundspeed_gpu);
        }
        soundspeed_gpu.copy_to_host();
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
        printf("lifted gpu average cost time: %lfms\n\n", cost_time/times*1000);
    }
    return 0;
} 