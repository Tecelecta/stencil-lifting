# OOPSLA25 Stencil-Lifting

This is the artifact of submission #898 (Stencil-Lifting: Hierarchical Recursive Lifting System for Extracting Summary of Stencil Kernel in Legacy Codes.) in OOPSLA 2025.

## Project Overview

Stencil-Lifting is a novel system for automatically converting stencil kernels written in low-level languages in legacy code into semantically equivalent Domain-Specific Language (DSL) implementations. This work addresses the efficiency bottlenecks of existing verified lifting systems through innovative theoretical and algorithmic contributions.

## Project Architecture

### Core Components

- **SDSL**: Stencil domain-specific language parser and AST processing
- **VCG**: Verified code generator with various optimization passes
- **Backend**: Multi-target code generation backend
- **SDSLC**: SDSL compiler main program

### Directory Structure

```
stencil-lifting/
├── src/
│   ├── SDSL/          # SDSL language parser
│   ├── VCG/           # Verified code generator
│   ├── Backend/       # Code generation backend
│   └── SDSLC/         # Compiler main program
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
cmake ..
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

We evaluate Stencil-Lifting on diverse stencil benchmarks from different benchmark suites and real-world applications, comparing against state-of-the-art verified lifting methods: **STNG** and **Dexter**.

### Performance Results

| Method | Relative Performance | Semantic Equivalence |
|--------|---------------------|---------------------|
| STNG | 1× (baseline) | ✓ |
| Dexter | 13.3× faster than STNG | ✓ |
| **Stencil-Lifting** | **77.05× faster than STNG** | ✓ |
| **Stencil-Lifting** | **5.8× faster than Dexter** | ✓ |

### About Missing Baseline in the Artifact

This artifact does not include reproductions for xxx. These baselines were conducted by their respective authors with private code modifications, which are unavailable for inclusion. However, as these baselines are not the top-performing methods in our experiments, their absence does not impact the validity of our experimental results.

## License

This project is licensed under an open source license. Please see the LICENSE file for specific license information.

## Contact

For questions or suggestions, please contact us through:
- Submit GitHub Issues
- Email @???
