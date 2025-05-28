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

| Benchmark | Kernel | Fortran Time (ms) | Halide CPU Time (ms) | CPU Speedup | Halide GPU Time (ms) | GPU Speedup | STNG Lifting Time (s) | Stencil-Lifting Time (s) | Lifting Speedup | Arrays | Pattern | Loops |
|-----------|--------|-------------------|---------------------|-------------|---------------------|-------------|----------------------|-------------------------|----------------|--------|---------|-------|
| **CloverLeaf** | akl82 | 1792.00 | 185.35 | 9.67× | 13.58 | 131.95× | 14944 | 9.398 | 1590.23× | 1 | 2d-4pt | 2 |
| | ackl91 | 2774.60 | 380.18 | 7.30× | 30.86 | 89.92× | 6361 | 10.632 | 598.29× | 2 | 2d-3pt | 2 |
| | ackl93 | 6221.20 | 1163.45 | 5.35× | 85.44 | 72.81× | – | 38.423 | – | 6 | 2d-2pt | 2 |
| | ackl94 | 3374.60 | 376.44 | 8.96× | 75.14 | 44.91× | 4273 | 10.171 | 420.12× | 2 | 2d-3pt | 2 |
| | ackl96 | 6354.60 | 987.16 | 6.44× | 85.29 | 74.51× | – | 38.381 | – | 6 | 2d-2pt | 2 |
| | amkl97 | 3294.00 | 374.53 | 8.80× | 26.67 | 123.50× | 4099 | 8.486 | 483.03× | 2 | 2d-3pt | 2 |
| | amkl98 | 3243.20 | 323.95 | 10.01× | 26.51 | 122.32× | 4191 | 8.433 | 496.98× | 2 | 2d-3pt | 2 |
| | amkl99 | 3001.40 | 327.83 | 9.16× | 56.50 | 53.13× | 1736 | 6.576 | 263.99× | 2 | 2d-2pt | 2 |
| | amkl100 | 2970.80 | 285.12 | 10.42× | 56.68 | 52.41× | 1512 | 6.530 | 231.55× | 2 | 2d-2pt | 2 |
| | amkl101 | 1283.80 | 128.43 | 10.00× | 25.60 | 50.14× | 1273 | 3.990 | 319.05× | 1 | 2d-4pt | 2 |
| **StencilMark** | div0 | 10127.40 | 745.87 | 13.58× | 7.24 | 1398.93× | 6590 | 15.328 | 429.93× | 1 | 3d-7pt | 3 |
| | heat0 | 6845.20 | 746.62 | 9.17× | 3.97 | 1724.62× | 4668 | 15.113 | 308.87× | 1 | 3d-7pt | 3 |
| | grad0 | 11809.20 | 729.51 | 16.19× | 10.50 | 1124.82× | 18557 | 32.918 | 563.73× | 3 | 3d-7pt | 3 |
| **MagEmul** | compgrad | 1175.61 | 143.88 | 8.17× | 10.617 | 110.72× | 486 | 1.239 | 392.25× | 2 | 3d-6pt | 6 |
| | compwc | 131.65 | 13.37 | 9.85× | 0.544 | 242.00× | 2 | 0.388 | 5.15× | 1 | 2d-4pt | 2 |
| | advtare | 12200.71 | 606.55 | 20.11× | 39.341 | 310.12× | 787 | 1.273 | 618.22× | 1 | 3d-4pt | 4 |
| | movemesh | 61.09 | 27.21 | 2.25× | 1.623 | 37.64× | 11 | 0.767 | 14.34× | 2 | 2d-5pt | 2 |
| | wciter | 22.02 | 11.96 | 1.84× | 0.561 | 39.25× | 7 | 0.401 | 17.46× | 2 | 3d-8pt | 2 |

**Note**: "–" indicates that STNG failed to produce valid solutions within the prescribed time limits.

See other tables or graphs in the paper for more results.


## License

This project is licensed under an open source license. Please see the LICENSE file for specific license information.


## Contact

For questions or suggestions, please contact us through **Submit GitHub Issues**.
