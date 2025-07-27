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

void pdv_kernel_halide(Func& density1_func, Func& energy1_func,
                      Buffer<double, 2> xarea,
                      Buffer<double, 2> yarea,
                      Buffer<double, 2> volume,
                      Buffer<double, 2> density0,
                      Buffer<double, 2> energy0,
                      Buffer<double, 2> pressure,
                      Buffer<double, 2> viscosity,
                      Buffer<double, 2> xvel0,
                      Buffer<double, 2> yvel0,
                      double dt,
                      int x_min, int x_max, int y_min, int y_max) {
    Var j("j"), k("k");
    Expr j_offset = Expr(2);
    Expr k_offset = Expr(2);
    auto j_buf = j + j_offset;
    auto k_buf = k + k_offset;

    auto left_flux = (xarea(j_buf, k_buf) * (xvel0(j_buf, k_buf) + xvel0(j_buf, k_buf+1) + xvel0(j_buf, k_buf) + xvel0(j_buf, k_buf+1))) * Expr(0.25) * Expr(dt) * Expr(0.5);
    auto right_flux = (xarea(j_buf+1, k_buf) * (xvel0(j_buf+1, k_buf) + xvel0(j_buf+1, k_buf+1) + xvel0(j_buf+1, k_buf) + xvel0(j_buf+1, k_buf+1))) * Expr(0.25) * Expr(dt) * Expr(0.5);
    auto bottom_flux = (yarea(j_buf, k_buf) * (yvel0(j_buf, k_buf) + yvel0(j_buf+1, k_buf) + yvel0(j_buf, k_buf) + yvel0(j_buf+1, k_buf))) * Expr(0.25) * Expr(dt) * Expr(0.5);
    auto top_flux = (yarea(j_buf, k_buf+1) * (yvel0(j_buf, k_buf+1) + yvel0(j_buf+1, k_buf+1) + yvel0(j_buf, k_buf+1) + yvel0(j_buf+1, k_buf+1))) * Expr(0.25) * Expr(dt) * Expr(0.5);
    auto total_flux = right_flux - left_flux + top_flux - bottom_flux;

    auto volume_change_s = volume(j_buf, k_buf) / (volume(j_buf, k_buf) + total_flux);
    auto recip_volume = Expr(1.0) / volume(j_buf, k_buf);
    auto energy_change = (pressure(j_buf, k_buf) / density0(j_buf, k_buf) + viscosity(j_buf, k_buf) / density0(j_buf, k_buf)) * total_flux * recip_volume;

    energy1_func(j, k) = energy0(j_buf, k_buf) - energy_change;
    density1_func(j, k) = density0(j_buf, k_buf) * volume_change_s;
}

extern "C" {
    void pdv_kernel_(
        int* x_min, int* x_max, int* y_min, int* y_max, double* dt,
        double* xarea, double* yarea, double* volume,
        double* density0, double* density1,
        double* energy0, double* energy1,
        double* pressure, double* viscosity,
        double* xvel0, double* yvel0
    );
}

int main(int argc, char** argv)
{
    const int x_min = 0;
    const int x_max = 2e4;
    const int y_min = 0;
    const int y_max = 2e4;
    const double dt = 0.1;

    const int xarea_x_size = (x_max + 3) - (x_min - 2) + 1;
    const int xarea_y_size = (y_max + 2) - (y_min - 2) + 1;
    const int yarea_x_size = (x_max + 2) - (x_min - 2) + 1;
    const int yarea_y_size = (y_max + 3) - (y_min - 2) + 1;
    const int arr_x_size = (x_max + 2) - (x_min - 2) + 1;
    const int arr_y_size = (y_max + 2) - (y_min - 2) + 1;

    const int output_j_size = x_max - x_min + 1;
    const int output_k_size = y_max - y_min + 1;

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

    Buffer<double, 2> xarea({xarea_x_size, xarea_y_size}, "xarea"), xarea_g(xarea);
    Buffer<double, 2> yarea({yarea_x_size, yarea_y_size}, "yarea"), yarea_g(yarea);
    Buffer<double, 2> volume({arr_x_size, arr_y_size}, "volume"), volume_g(volume);
    Buffer<double, 2> density0({arr_x_size, arr_y_size}, "density0"), density0_g(density0);
    Buffer<double, 2> energy0({arr_x_size, arr_y_size}, "energy0"), energy0_g(energy0);
    Buffer<double, 2> pressure({arr_x_size, arr_y_size}, "pressure"), pressure_g(pressure);
    Buffer<double, 2> viscosity({arr_x_size, arr_y_size}, "viscosity"), viscosity_g(viscosity);
    Buffer<double, 2> xvel0({xarea_x_size, yarea_y_size}, "xvel0"), xvel0_g(xvel0);
    Buffer<double, 2> yvel0({xarea_x_size, yarea_y_size}, "yvel0"), yvel0_g(yvel0);

    Buffer<double, 2> density1_base({arr_x_size, arr_y_size}, "density1_base");
    Buffer<double, 2> energy1_base({arr_x_size, arr_y_size}, "energy1_base");

    Buffer<double, 2> density1_cpu({output_j_size, output_k_size}, "density1_cpu");
    Buffer<double, 2> energy1_cpu({output_j_size, output_k_size}, "energy1_cpu");

    Buffer<double, 2> density1_gpu({output_j_size, output_k_size}, "density1_gpu");
    Buffer<double, 2> energy1_gpu({output_j_size, output_k_size}, "energy1_gpu");

    printf("kernel start!\n");

    Func density1_func_cpu("density1_func_cpu");
    Func energy1_func_cpu("energy1_func_cpu");

    pdv_kernel_halide(density1_func_cpu, energy1_func_cpu,
                      xarea, yarea, volume, density0, energy0, pressure, viscosity, xvel0, yvel0, dt,
                      x_min, x_max, y_min, y_max);

    density1_func_cpu.parallel(d2);
    energy1_func_cpu.parallel(d2);
    if (arr_x_size >= 8) {
        density1_func_cpu.vectorize(d1, 8);
        energy1_func_cpu.vectorize(d1, 8);
    }
    density1_func_cpu.compile_jit(cpu_target);
    energy1_func_cpu.compile_jit(cpu_target);

    i2_cpu.realize(xarea);
    i2_cpu.realize(yarea);
    i2_cpu.realize(volume);
    i2_cpu.realize(density0);
    i2_cpu.realize(energy0);
    i2_cpu.realize(pressure);
    i2_cpu.realize(viscosity);
    i2_cpu.realize(xvel0);
    i2_cpu.realize(yvel0);

    z2_cpu.realize(density1_base);
    z2_cpu.realize(energy1_base);

    int x_min_i = x_min, x_max_i = x_max, y_min_i = y_min, y_max_i = y_max;
    double dt_i = dt;
    pdv_kernel_(&x_min_i, &x_max_i, &y_min_i, &y_max_i, &dt_i,
                xarea.get()->begin(), yarea.get()->begin(), volume.get()->begin(),
                density0.get()->begin(), density1_base.get()->begin(),
                energy0.get()->begin(), energy1_base.get()->begin(),
                pressure.get()->begin(), viscosity.get()->begin(),
                xvel0.get()->begin(), yvel0.get()->begin());

    try {
        z2_cpu.realize(density1_cpu);
        z2_cpu.realize(energy1_cpu);
        density1_func_cpu.realize(density1_cpu);
        energy1_func_cpu.realize(energy1_cpu);
    } catch (RuntimeError &e) {
        std::cerr << e.what();
        return 1;
    }

    double correctness_d = 0.0, correctness_e = 0.0;
    int count = 0;
    for (int k = y_min; k <= y_max; k++) {
        for (int j = x_min; j <= x_max; j++) {
            int fort_idx_j = j - (x_min - 2);
            int fort_idx_k = k - (y_min - 2);
            int hal_idx_j = j - x_min;
            int hal_idx_k = k - y_min;
            auto d_diff = abs(density1_base(fort_idx_j, fort_idx_k) - density1_cpu(hal_idx_j, hal_idx_k));
            auto e_diff = abs(energy1_base(fort_idx_j, fort_idx_k) - energy1_cpu(hal_idx_j, hal_idx_k));
            if (d_diff >= 1e-6) {
                printf("density1 difference [%d, %d]: Fort=%lf | Hali=%lf\n", j, k, density1_base(fort_idx_j, fort_idx_k), density1_cpu(hal_idx_j, hal_idx_k));
            }
            if (e_diff >= 1e-6) {
                printf("energy1 difference [%d, %d]: Fort=%lf | Hali=%lf\n", j, k, energy1_base(fort_idx_j, fort_idx_k), energy1_cpu(hal_idx_j, hal_idx_k));
            }
            correctness_d += d_diff;
            correctness_e += e_diff;
            count++;
        }
    }
    printf("density1 average error: %e\n", correctness_d / count);
    printf("energy1 average error: %e\n", correctness_e / count);

    int times = 20;
    double cost_time = 0;
    struct timespec t1, t2, elapsed;
    cost_time = 0;
    clock_gettime(CLOCK_REALTIME, &t1);
    for (int i = 0; i < times; i++) {
        pdv_kernel_(&x_min_i, &x_max_i, &y_min_i, &y_max_i, &dt_i,
                    xarea.get()->begin(), yarea.get()->begin(), volume.get()->begin(),
                    density0.get()->begin(), density1_base.get()->begin(),
                    energy0.get()->begin(), energy1_base.get()->begin(),
                    pressure.get()->begin(), viscosity.get()->begin(),
                    xvel0.get()->begin(), yvel0.get()->begin());
    }
    clock_gettime(CLOCK_REALTIME, &t2);
    timespec_diff(t2, t1, elapsed);
    cost_time += toSec(elapsed);
    printf("legacy code average cost time: %lfms\n\n", cost_time/times*1000);

    cost_time = 0;
    for (int i = 0; i < times; i++) {
        clock_gettime(CLOCK_REALTIME, &t1);
        density1_func_cpu.realize(density1_cpu);
        energy1_func_cpu.realize(energy1_cpu);
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("lifted cpu average cost time: %lfms\n\n", cost_time/times*1000);

    if (with_gpu) {
        Func density1_func_gpu("density1_gpu");
        Func energy1_func_gpu("energy1_gpu");
        pdv_kernel_halide(density1_func_gpu, energy1_func_gpu,
                          xarea_g, yarea_g, volume_g, density0_g, energy0_g, pressure_g, viscosity_g, xvel0_g, yvel0_g, dt,
                          x_min, x_max, y_min, y_max);
        density1_func_gpu.gpu_tile(d1, d2, d1o, d2o, d1i, d2i, 16, 16).compile_jit(gpu_target);
        energy1_func_gpu.gpu_tile(d1, d2, d1o, d2o, d1i, d2i, 16, 16).compile_jit(gpu_target);
        i2_gpu.realize(xarea_g);
        i2_gpu.realize(yarea_g);
        i2_gpu.realize(volume_g);
        i2_gpu.realize(density0_g);
        i2_gpu.realize(energy0_g);
        i2_gpu.realize(pressure_g);
        i2_gpu.realize(viscosity_g);
        i2_gpu.realize(xvel0_g);
        i2_gpu.realize(yvel0_g);
        z2_gpu.realize(density1_gpu);
        z2_gpu.realize(energy1_gpu);
        try {
            density1_func_gpu.realize(density1_gpu);
            energy1_func_gpu.realize(energy1_gpu);
        } catch (RuntimeError &e) {
            std::cerr << e.what();
            return 2;
        }
        cost_time = 0;
        clock_gettime(CLOCK_REALTIME, &t1);
        for (int i = 0; i < times; i++) {
            density1_func_gpu.realize(density1_gpu);
            energy1_func_gpu.realize(energy1_gpu);
        }
        density1_gpu.copy_to_host();
        energy1_gpu.copy_to_host();
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
        printf("lifted gpu average cost time: %lfms\n\n", cost_time/times*1000);
    }
    return 0;
}
