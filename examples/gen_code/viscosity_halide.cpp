#include <cstdio>
#include <ctime>
#include <cstdlib>
#include <cmath>
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
   
    return nz;
}

template<>
inline Func init_nonzero<2>(const std::string& funcName, bool vectorize)
{
    Var d1, d2;
    Func nz(funcName);
    nz(d1, d2) = ((((d1 + d2) % 13) + Expr(0.1)) / Expr(13.0)) * cast<double>(10);

    return nz;
}

template <>
inline Func set_zero<1>(const std::string& funcName, bool vectorize)
{
    Var d;
    Func set_zero(funcName);
    set_zero(d) = Expr(0.0);
    if (vectorize) set_zero.vectorize(d, 8);
    return set_zero;
}

template <>
inline Func set_zero<2>(const std::string& funcName, bool vectorize)
{
    Var d1, d2;
    Func set_zero(funcName);
    set_zero(d1, d2) = Expr(0.0);
    set_zero.parallel(d2);
    if (vectorize) set_zero.vectorize(d1, 8);
    return set_zero;
}

extern "C" {
    void viscosity_kernel_(const int* x_min,
                           const int* x_max,
                           const int* y_min,
                           const int* y_max,
                           const double* celldx,
                           const double* celldy,
                           const double* density0,
                           const double* pressure,
                           double* viscosity,
                           const double *xvel0, 
                           const double *yvel0);

}

Func targetFunc(const Buffer<double,1> celldx,
                const Buffer<double,1> celldy,
                const Buffer<double,2> density0,
                const Buffer<double,2> pressure,
                const Buffer<double,2> xvel0,
                const Buffer<double,2> yvel0)
{
    Var k("k"), j("j");
    Func ret("density0_fn");
    Func ugrad, vgrad, div, strain2, pgradx, pgrady, pgradx2, pgrady2, limiter;

    Func celldx_a, celldy_a, density0_a, pressure_a, xvel0_a, yvel0_a;

    celldx_a(j) = celldx(j+2);
    celldy_a(k) = celldx(k+2);
    density0_a(j,k) = density0(j+2, k+2);
    pressure_a(j,k) = pressure(j+2, k+2);
    xvel0_a(j,k) = xvel0(j+2, k+2);
    yvel0_a(j,k) = yvel0(j+2, k+2);


    ugrad(j,k) = (xvel0_a(j+1, k) + xvel0_a(j+1, k+1)) - (xvel0_a(j  , k) + xvel0_a(j  , k+1));
    vgrad(j,k) = (yvel0_a(j, k+1) + yvel0_a(j+1, k+1)) - (yvel0_a(j  , k) + yvel0_a(j+1, k));
    div(j,k) = celldx_a(j)*ugrad(j,k) + celldy_a(k)*vgrad(j,k);
    strain2(j,k) = Expr(0.5)*(xvel0_a(j,  k+1) + xvel0_a(j+1,k+1)-xvel0_a(j  ,k  )-xvel0_a(j+1,k  ))/celldy_a(k)
                 + Expr(0.5)*(yvel0_a(j+1,k  ) + yvel0_a(j+1,k+1)-yvel0_a(j  ,k  )-yvel0_a(j  ,k+1))/celldx_a(j);
    
    pgradx(j,k) = (pressure_a(j+1,k)-pressure_a(j-1,k))/(celldx_a(j)+celldx_a(j+1));
    pgrady(j,k) = (pressure_a(j,k+1)-pressure_a(j,k-1))/(celldy_a(k)+celldy_a(k+1));

    pgradx2(j,k) = pgradx(j,k) * pgradx(j,k);
    pgrady2(j,k) = pgrady(j,k) * pgrady(j,k);

    limiter(j,k) = ( (Expr(0.5)*ugrad(j,k)/celldx_a(j))*pgradx2(j,k)
                    + (Expr(0.5)*(vgrad(j,k))/celldy_a(k))*pgrady2(j,k)
                    + strain2(j,k)*pgradx(j,k)*pgrady(j,k) ) 
                   / max(pgrady2(j,k)+pgradx2(j,k), Expr(1e-16));
    
    Func dirx, diry, pgradx_1, pgrady_1, xgrad, ygrad, pgrad, grad, grad2;
    dirx(j,k) = select( pgradx(j,k) < 0, Expr(-1.0), Expr(1.0));
    pgradx_1(j,k) = dirx(j,k) * max(Expr(1e-16), abs(pgradx(j,k)));
    diry(j,k) = select( pgradx_1(j,k) < 0, Expr(-1.0), Expr(1.0));
    pgrady_1(j,k) = diry(j,k) * max(Expr(1e-16), abs(pgrady(j,k)));
    pgrad(j,k) = sqrt(pgradx_1(j,k)*pgradx_1(j,k) +  pgrady_1(j,k)*pgrady_1(j,k));
    xgrad(j,k) = abs(celldx_a(j)*pgrad(j,k)/pgradx_1(j,k));
    ygrad(j,k) = abs(celldy_a(k)*pgrad(j,k)/pgrady_1(j,k));
    grad(j,k) = min(xgrad(j,k),ygrad(j,k));
    grad2(j,k) = grad(j,k)*grad(j,k);

    ret(j,k) = select(
        limiter(j,k) > 0 || div(j,k) >= 0, 
        Expr(0.),
        Expr(2.0) * density0_a(j,k) * grad2(j,k) * limiter(j,k) * limiter(j,k)
    );

    return ret;
}

int main(int argc, char** argv)
{
    // printf("Caller Start!\n");
    const int x_max = 2e4;
    const int x_min = 0;
    const int y_max = 2e4;
    const int y_min = 0;

    // const int x_max = 8;
    // const int x_min = 0;
    // const int y_max = 8;
    // const int y_min = 0;

    const int x_range = x_max - x_min;
    const int y_range = y_max - y_min;

    // --------------------------- Preparation --------------------------
    // printf("Prepare the random data\n");
    bool with_gpu = true;
    
    Var j("j"), k("k");
    Var bj, bk, tj, tk;

    Target gpu_target = find_gpu_target();
    if (!gpu_target.has_gpu_feature())
    {
        printf("Don't have a gpu!\n");
        with_gpu = false;
    }
    Target cpu_target = get_host_target();

    Func i2_cpu = init_nonzero<2>("i2", x_range + 5 >= 8);
    Func z2_cpu = set_zero<2>("z2", x_range + 5 >= 8);
    Func i1_cpu = init_nonzero<1>("i2", x_range + 5 >= 8);
    Func i1_gpu = i1_cpu;
    Func i2_gpu = i2_cpu;
    Func z2_gpu = z2_cpu;
    i2_cpu.compile_jit(cpu_target);
    z2_cpu.compile_jit(cpu_target);
    i1_cpu.compile_jit(cpu_target);
    if (with_gpu)
    {
        i2_gpu.compile_jit(gpu_target);
        z2_gpu.compile_jit(gpu_target);
        i1_gpu.compile_jit(gpu_target);
    }

    Buffer<double,1> celldx(x_range + 5, "celldx"), celldx_g(celldx);
    Buffer<double,1> celldy(x_range + 5, "celldy"), celldy_g(celldy);
    Buffer<double,2> density0({x_range + 5, y_range + 5}, "density0"), density0_g(density0);
    Buffer<double,2> pressure({x_range + 5, y_range + 5}, "pressure"), pressure_g(pressure);
    Buffer<double,2> xvel0({x_range + 6, y_range + 6}, "xvel0"), xvel0_g(xvel0);
    Buffer<double,2> yvel0({x_range + 6, y_range + 6}, "yvel0"), yvel0_g(yvel0);

    Buffer<double,2> viscosity_base({x_range + 5, y_range + 5}, "viscosity_base");
    
    Buffer<double,2> viscosity_cpu({x_range+1, y_range+1}, "viscosity_cpu");

    Buffer<double,2> viscosity_gpu({x_range+1, y_range+1}, "viscosity_gpu");

    // --------------------------- advec_mom_kernel_loop1y_min3 kernel --------------------------
    // printf("advec_mom_kernel_loop1y_min3 kernel start!\n");

    Func cpu_fn = targetFunc(celldx, celldx, density0, pressure, xvel0, yvel0);
    cpu_fn.parallel(k).vectorize(j, 8).compile_jit(cpu_target);

    i1_cpu.realize(celldx);
    i1_cpu.realize(celldy);
    i2_cpu.realize(density0);
    i2_cpu.realize(pressure);
    i2_cpu.realize(xvel0);
    i2_cpu.realize(yvel0);
    z2_cpu.realize(viscosity_base);

    // Calling baseline Fortran
    double *celldx_ = celldx.get()->begin();
    double *celldy_ = celldy.get()->begin();
    double *density0_ = density0.get()->begin();
    double *pressure_ = pressure.get()->begin();
    double *xvel0_ = xvel0.get()->begin();
    double *yvel0_ = yvel0.get()->begin();
    double *viscosity_ = viscosity_base.get()->begin();
    
    viscosity_kernel_(&x_min, &x_max, &y_min, &y_max,
        celldx_, celldy_, density0_, pressure_, viscosity_, xvel0_, yvel0_
    );
    
    // Calling halide cpu
    try {
        z2_cpu.realize(viscosity_cpu);
        cpu_fn.realize(viscosity_cpu);
    } catch (RuntimeError &e) {
        std::cerr << e.what();
        return 1;
    }

    double correctness = 0.0;
    int errors = 0;
    
    for (int k_idx = 0; k_idx <= y_range; k_idx++) {
        for (int j_idx = 0; j_idx <= x_range; j_idx++) {
            int cpp_j = j_idx+2;
            int cpp_k = k_idx+2;
            auto A=viscosity_base(cpp_j, cpp_k);
            auto B=viscosity_cpu(j_idx, k_idx);
            
            double diff = abs(A-B);
            
            if (diff >= 1e-3)
            {
                printf("[%d, %d] = Fortran: %lf | Halide: %lf | diff: %e\n", 
                        j_idx, k_idx, viscosity_base(cpp_j, cpp_k), viscosity_cpu(j_idx, k_idx), diff);
                errors++;
            }
            correctness += diff;
        }
    }

    // printf("Total errors for node_mass_pre: %d, sum of differences: %e\n", errors, correctness);

    int times = 5;
    double cost_time = 0;
    struct timespec t1, t2, elapsed;
    
    for (int i = 0; i < times; i++)
    {
        clock_gettime(CLOCK_REALTIME, &t1);
        viscosity_kernel_(&x_min, &x_max, &y_min, &y_max,
            celldx_, celldy_, density0_, pressure_, viscosity_, xvel0_, yvel0_
        );
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("legacy code: %lfms\n", cost_time/times*1000);

    cost_time = 0;
    for (int i = 0; i < times; i++)
    {
        clock_gettime(CLOCK_REALTIME, &t1);
        cpu_fn.realize(viscosity_cpu);
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time += toSec(elapsed);
    }
    printf("lifted cpu: %lfms\n", cost_time/times*1000);

    if (with_gpu)
    {
        // building gpu func
        Func gpu_fn = targetFunc(celldx_g, celldx_g, density0_g, pressure_g, xvel0_g, yvel0_g);
        gpu_fn.gpu_tile(j,k,bj,bk,tj,tk,8,8)
              .compile_jit(gpu_target);
        
        // GPU IO initialization

        i1_gpu.realize(celldx_g);
        i1_gpu.realize(celldy_g);
        i2_gpu.realize(density0_g);
        i2_gpu.realize(pressure_g);
        i2_gpu.realize(xvel0_g);
        i2_gpu.realize(yvel0_g);
        
        // warmup
        try {
            z2_gpu.realize(viscosity_gpu);
            gpu_fn.realize(viscosity_gpu);
        } catch ( RuntimeError &e ) {
            std::cerr << e.what();
            return -1;
        }

        cost_time = 0;
        clock_gettime(CLOCK_REALTIME, &t1);
        for (int i = 0; i < times; i++)
        {
            gpu_fn.realize(viscosity_gpu);
        }
        viscosity_gpu.copy_to_host();
        clock_gettime(CLOCK_REALTIME, &t2);
        timespec_diff(t2, t1, elapsed);
        cost_time = toSec(elapsed);
        printf("lifted gpu: %lfms\n\n", cost_time/times*1000);
    }

    return 0;
} 