# OOPSLA25 Stencil-Lifting

This is the artifact of submission **#898** (Stencil-Lifting: Hierarchical Recursive Lifting System for Extracting Summary of Stencil Kernel in Legacy Codes.) in OOPSLA 2025.

## Project Overview

Stencil-Lifting is a novel system for automatically converting stencil kernels written in low-level languages in legacy code into semantically equivalent Domain-Specific Language (DSL) implementations. This work addresses the efficiency bottlenecks of existing verified lifting systems through innovative **hierarchical recursive lifting theory** and **terminating hierarchical recursive lifting algorithm**.

We evaluate Stencil-Lifting on diverse stencil benchmarks from two different suites and on three real-world applications. 
Experimental results demonstrate that Stencil-Lifting achieves $77.05×$ and $5.8×$ speedups compared to the state-of-the-art verified lifting methods
STNG and Dexter, respectively, while maintaining full semantic equivalence.

## Project Architecture

```
stencil-lifting/
├── src/
│   ├── SDSL/          # SDSL language parser 
│   ├── VCG/           # Verified code generator
│   ├── Backend/       # Code generation backend
│   └── SDSLC/         # Compiler main program
├── examples/          # Examples of end-to-end experiments
├── include/           # Header files
├── translate.py       # Fortran to SDSL translator
├── sdsl.py            # SDSL data structure definitions
├── auto_translate.py  # Automatic translation script
└── auto_compile.py    # Automatic compilation script
```


## Running the Artifact

### Requirements

- CMake 3.16+
- C++17 compiler
- Z3 SMT solver 4.12+
- Python 3.x
- fparser2

### Building the Project


```bash
mkdir build
cd build
cmake . -B build -DZ3_SINGLE_THREADED=true -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=/PATH/
make
```

### Usage Examples

#### 1. Fortran to SDSL Translation

```bash
python auto_translate.py <fortran_source_dir>
```

This will process all `.f` files in the specified directory and generate corresponding `.sdsl` files in the `./tmp/` directory.

#### 2. SDSL Compilation

```bash
# Generate Halide code
./sdslc --target=halide input.sdsl -o output.cpp

# Generate CUDA code
./sdslc --target=cuda input.sdsl -o output.cuf
```

#### 3. Batch Compilation

```bash
python auto_compile.py ./sdslc <source_dir> <output_dir>
```
## Evaluation and Benchmarks

### Experimental Setup

We evaluate Stencil-Lifting on a server equipped with:
- **Hardware**: Two Intel Xeon Gold 6226R CPUs (2×16 cores, hyper-threading enabled), 256GB RAM, eight NVIDIA Tesla V100 GPUs
- **Software**: CUDA v11.7, Ubuntu Linux 20.04.5 LTS (kernel 5.15.0-48-generic), GCC 9.4.0, GNU Fortran 9.4.0, LLVM 15.0.2, Halide 14.0.0
- **Optimization**: Halide code is autotuned using the OpenTuner framework for optimal scheduling

We compare against state-of-the-art verified lifting methods:
- **STNG**: Counter-example guided inductive synthesis (CEGIS) based lifting method
- **Dexter**: Improved CEGIS-based method with limited search space

### Benchmark Suites

#### 1. **Standard Stencil Benchmark Suites**
- **CloverLeaf**: Standard Lagrangian-Eulerian hydrodynamics benchmark with explicit second-order methods on Cartesian grids, solving compressible Euler equations on 2D staggered grids
- **StencilMark**: Performance and compiler experimentation microbenchmarks featuring three 3D kernels with open-source Fortran implementations

#### 2. **Scientific Applications**
- **WRF Model**: State-of-the-art mesoscale numerical weather prediction system with ten commonly-used stencil kernels
- **NAS MG**: Well-known benchmark applying V-cycle multigrid method to solve discrete Poisson equations in 3D
- **MagEmul**: Magnetic field emulation program implementing complex Godunov methods with imperfect loop nesting and conditional branches

### Performance Results

#### 1. **Lifting Effectiveness and Efficiency**
- **Success Rate**: Stencil-Lifting successfully lifted all tested stencil kernels, including complex real-world cases where STNG and Dexter often failed or timed out.
- **Efficiency**: Stencil-Lifting achieves a geometric mean speedup of $83.1×$ over STNG and $5.8×$ over Dexter for the lifting process. For example, lifting the akl82 kernel (2D-4pt) takes 9.3 seconds with Stencil-Lifting, compared to 14,944 seconds with STNG and 104.8 seconds with Dexter.
- **Scalability**: As stencil complexity increases (measured by the product of the number of computational points and output arrays), Stencil-Lifting shows near-linear scaling in lifting time, while STNG's time grows rapidly due to exponential search space expansion.

#### 2. **Performance of Lifted Code**
- **CPU Backend**: Halide code generated from lifted summaries achieves a $7.62×$ geometric mean speedup over original single-threaded Fortran code when run on 64 CPU threads. Speedups range from $1.30×$ to $16.19×$ across benchmarks.
- **GPU Backend**: Halide code executed on a single V100 GPU (excluding data transfer) achieves a $83.85×$ geometric mean speedup over the Fortran baseline, with speedups from $25.46×$ to $1724.62×$.
- **Scientific Applications**: In WRF and NAS MG, DSL code generated by Stencil-Lifting achieves an average $5.57×$ CPU speedup and $72.14×$ GPU speedup over the original Fortran kernels.

#### 3. **Detailed Benchmark Table (Excerpt)**

| Benchmark | Kernel | Fortran Time (ms) | Halide CPU Time (ms) | Halide CPU Speedup | Halide GPU Time (ms) | Halide GPU Speedup | Lifting Time of STNG (s) | Lifting Time of Stencil-Lifting (s) | Stencil-Lifting Speedup over STNG | Num. of Output Arrays | Dimen. and Points | Num. of Loops |
|-----------|--------|-------------------|---------------------|-------------|---------------------|-------------|----------------------|-------------------------|----------------|--------|---------|-------|
| **CloverLeaf** | akl82 | 1792.00 | 185.35 | 9.67× | 13.58 | 131.95× | 14944 | 9.398 | 1590.23× | 1 | 2d-4pt | 2 |
| | akl83 | 3853.40 | 256.84 | 15.00× | 23.26 | 165.68× | 683 | 5.561 | 122.82× | 1 | 2d-4pt | 2 |
| | akl84 | 2798.40 | 272.30 | 10.28× | 54.27 | 51.57× | 631 | 5.589 | 112.90× | 1 | 2d-4pt | 2 |
| | akl85 | 2484.20 | 251.40 | 9.88× | 53.99 | 46.01× | 662 | 7.722 | 85.73× | 1 | 2d-4pt | 2 |
| | akl86 | 2639.60 | 234.03 | 11.28× | 57.47 | 45.93× | 919 | 9.355 | 98.45× | 1 | 2d-4pt | 2 |
| | ackl91 | 2774.60 | 380.18 | 7.30× | 30.86 | 89.92× | 6361 | 10.632 | 598.29× | 2 | 2d-3pt | 2 |
| | ackl92 | 2607.20 | 267.76 | 9.74× | 59.18 | 44.06× | 1542 | 5.817 | 265.09× | 2 | 2d-2pt | 2 |
| | ackl93 | 6221.20 | 1163.45 | 5.35× | 85.44 | 72.81× | – | 38.423 | – | 6 | 2d-2pt | 2 |
| | ackl94 | 3374.60 | 376.44 | 8.96× | 75.14 | 44.91× | 4273 | 10.171 | 420.12× | 2 | 2d-3pt | 2 |
| | ackl95 | 2591.20 | 308.26 | 8.41× | 56.10 | 46.19× | – | 5.768 | – | 2 | 2d-2pt | 2 |
| | ackl96 | 6354.60 | 987.16 | 6.44× | 85.29 | 74.51× | – | 38.381 | – | 6 | 2d-2pt | 2 |
| | amkl97 | 3294.00 | 374.53 | 8.80× | 26.67 | 123.50× | 4099 | 8.486 | 483.03× | 2 | 2d-3pt | 2 |
| | amkl98 | 3243.20 | 323.95 | 10.01× | 26.51 | 122.32× | 4191 | 8.433 | 496.98× | 2 | 2d-3pt | 2 |
| | amkl99 | 3001.40 | 327.83 | 9.16× | 56.50 | 53.13× | 1736 | 6.576 | 263.99× | 2 | 2d-2pt | 2 |
| | amkl100 | 2970.80 | 285.12 | 10.42× | 56.68 | 52.41× | 1512 | 6.530 | 231.55× | 2 | 2d-2pt | 2 |
| | amkl101 | 1283.80 | 128.43 | 10.00× | 25.60 | 50.14× | 1273 | 3.990 | 319.05× | 1 | 2d-4pt | 2 |
| | amkl102 | 2102.80 | 196.25 | 10.71× | 13.68 | 153.75× | – | 5.938 | – | 1 | 2d-4pt | 2 |
| | amkl103 | 1712.20 | 145.67 | 11.75× | 11.81 | 145.04× | 176 | 3.251 | 54.14× | 1 | 2d-2pt | 2 |
| | amkl104 | 2232.60 | 221.18 | 10.09× | 52.29 | 42.70× | – | 5.771 | – | 1 | 2d-2pt | 2 |
| | amkl105 | 1659.60 | 126.95 | 13.07× | 24.76 | 67.04× | 707 | 4.095 | 172.65× | 1 | 2d-4pt | 2 |
| | amkl107 | 1535.80 | 164.33 | 9.35× | 29.82 | 51.51× | 133 | 5.519 | 24.10× | 1 | 2d-2pt | 2 |
| | fckl89 | 2681.80 | 213.55 | 12.56× | 17.62 | 152.18× | 425 | 4.617 | 92.05× | 1 | 2d-2pt | 2 |
| | fckl90 | 2694.40 | 182.70 | 14.75× | 44.38 | 60.71× | 131 | 4.763 | 27.50× | 1 | 2d-2pt | 2 |
| | gckl77 | 630.00 | 73.60 | 8.56× | 21.54 | 29.24× | 7 | 3.533 | 1.98× | 1 | 2d-1pt | 2 |
| | gckl78 | 658.00 | 78.90 | 8.34× | 25.70 | 25.60× | 8 | 3.373 | 2.37× | 1 | 2d-1pt | 2 |
| | gckl79 | 636.60 | 76.97 | 8.27× | 21.61 | 29.46× | 6 | 2.198 | 2.73× | 1 | 2d-1pt | 2 |
| | gckl80 | 648.20 | 86.12 | 7.53× | 25.46 | 25.46× | 7 | 2.267 | 3.09× | 1 | 2d-1pt | 2 |
| | ickl10 | 91.40 | 70.46 | 1.30× | 1.29 | 71.08× | 2 | 1.972 | 1.01× | 1 | 1d-2pt | 1 |
| **StencilMark** | div0 | 10127.40 | 745.87 | 13.58× | 7.24 | 1398.93× | 6590 | 15.328 | 429.93× | 1 | 3d-7pt | 3 |
| | heat0 | 6845.20 | 746.62 | 9.17× | 3.97 | 1724.62× | 4668 | 15.113 | 308.87× | 1 | 3d-7pt | 3 |
| | grad0 | 11809.20 | 729.51 | 16.19× | 10.50 | 1124.82× | 18557 | 32.918 | 563.73× | 3 | 3d-7pt | 3 |
| **WRF-Dyn-em** | mdel3 | 2637.80 | 450.70 | 5.85× | 32.11 | 82.14× | – | 13.663 | – | 1 | 3d-4pt | 3 |
| | mdel5 | 4213.60 | 435.51 | 9.67× | 43.35 | 97.19× | – | 14.893 | – | 1 | 3d-4pt | 3 |
| | mdel6 | 3799.80 | 284.86 | 13.34× | 37.22 | 102.10× | – | 13.848 | – | 1 | 3d-2pt | 3 |
| | mdel10 | 3523.00 | 338.05 | 10.42× | 33.73 | 104.46× | – | 13.022 | – | 1 | 3d-4pt | 3 |
| | mdel13 | 4345.20 | 357.99 | 12.14× | 38.99 | 111.46× | – | 14.344 | – | 1 | 3d-2pt | 3 |
| | mdel16 | 4263.60 | 408.18 | 10.45× | 104.34 | 40.86× | – | 11.081 | – | 1 | 3d-2pt | 3 |
| | mdel18 | 1572.60 | 166.82 | 9.43× | 31.39 | 50.11× | – | 4.022 | – | 1 | 2d-3pt | 2 |
| | mdel33 | 4928.40 | 855.02 | 5.76× | 46.04 | 107.05× | – | 51.335 | – | 4 | 3d-11pt | 3 |
| | mdel35 | 3132.80 | 410.88 | 7.62× | 31.20 | 100.42× | – | 100.266 | – | 1 | 3d-14pt | 5 |
| | mdel37 | 3599.20 | 341.90 | 10.53× | 81.80 | 44.00× | – | 15.526 | – | 1 | 3d-8pt | 3 |
| **NAS MG** | mgl11 | 1723.40 | 258.54 | 6.67× | 28.26 | 60.98× | – | 4.846 | – | 1 | 3d-2pt | 2 |
| | mgl12 | 2428.00 | 218.48 | 11.11× | 16.49 | 147.28× | – | 5.235 | – | 1 | 3d-2pt | 2 |
| | mgl13 | 2254.80 | 259.54 | 8.69× | 16.55 | 136.26× | – | 5.188 | – | 1 | 3d-2pt | 2 |
| | mgl14 | 319.60 | 174.49 | 1.83× | 2.81 | 113.85× | – | 7.285 | – | 4 | 2d-1pt | 1 |
| | mgl3 | 353.00 | 206.73 | 1.71× | 3.89 | 90.69× | – | 11.629 | – | 3 | 1d-3pt | 2 |
| | mgl6 | 7006.60 | 3724.37 | 1.88× | 136.64 | 51.28× | – | 649.013 | – | 1 | 3d-21pt | 4 |
| | mgl5 | 5537.80 | 3853.21 | 1.44× | 123.15 | 44.97× | 46422 | 673.900 | 68.89× | 1 | 3d-19pt | 4 |
| | resid1 | 7143.60 | 3612.91 | 1.98× | 64.10 | 111.44× | – | 682.458 | – | 1 | 3d-27pt | 4 |
| | trilinear_proj | 2492.20 | 779.95 | 3.20× | 36.54 | 68.21× | – | 1413.083 | – | 1 | 3d-27pt | 4 |

**Note**: "–" indicates that STNG failed to produce valid solutions within the prescribed time limits. See additional tables or charts in the paper for more analysis results.

## License

This project is licensed under an open source license. Please see the LICENSE file for specific license information.


## Contact

For questions or suggestions, please contact us through **Submit GitHub Issues**.
