FROM nvcr.io/nvidia/cuda:12.1.0-devel-ubuntu22.04

#  === dev usage ===
ENV HTTPS_PROXY=http://10.18.86.214:7892
ENV HTTP_PROXY=http://10.18.86.214:7892

RUN { \
echo "deb https://mirrors.aliyun.com/ubuntu/ jammy main restricted universe multiverse"; \
echo "deb https://mirrors.aliyun.com/ubuntu/ jammy-security main restricted universe multiverse"; \
echo "deb https://mirrors.aliyun.com/ubuntu/ jammy-updates main restricted universe multiverse"; \
echo "deb https://mirrors.aliyun.com/ubuntu/ jammy-backports main restricted universe multiverse"; \
} > /etc/apt/sources.list
#  === dev usage end ===

RUN apt update 
RUN apt install -y python3

WORKDIR /apps

COPY . ./stencil-lifting

RUN tar vxf ./stencil-lifting/deps/llvm-project-llvmorg-19.1.5.tar.gz
RUN tar vxf ./stencil-lifting/deps/z3-z3-4.15.2.tar.gz
RUN tar vxf ./stencil-lifting/deps/boost-1.88.0-cmake.tar.xz
RUN tar vxf ./stencil-lifting/deps/Halide-19.0.0.tar.gz
RUN tar vxf ./stencil-lifting/deps/cmake-3.31.8.tar.gz

RUN mv llvm-project-llvmorg-19.1.5 llvm-project
RUN mv cmake-3.31.8 cmake
RUN mv z3-z3-4.15.2 z3 
RUN mv boost-1.88.0 boost
RUN mv Halide-19.0.0 Halide

WORKDIR /apps/cmake

RUN ./bootstrap -- -DCMAKE_USE_OPENSSL=OFF && make -j8 && make install

RUN make clean

WORKDIR /apps/llvm-project

        # -DLLVM_TARGETS_TO_BUILD="WebAssembly;X86;AArch64;ARM;Hexagon;NVPTX;PowerPC;RISCV" \
RUN cmake -S llvm -B build \
        -DCMAKE_BUILD_TYPE=Release \
        -DLLVM_ENABLE_PROJECTS="clang;lld;clang-tools-extra;flang" \
        -DLLVM_ENABLE_RUNTIMES=compiler-rt \
        -DLLVM_TARGETS_TO_BUILD="WebAssembly;X86;NVPTX" \
        -DLLVM_ENABLE_ASSERTIONS=ON \
        -DLLVM_ENABLE_EH=OFF \
        -DLLVM_ENABLE_RTTI=ON \
        -DLLVM_ENABLE_HTTPLIB=OFF \
        -DLLVM_ENABLE_LIBEDIT=OFF \
        -DLLVM_ENABLE_LIBXML2=OFF \
        -DLLVM_ENABLE_TERMINFO=OFF \
        -DLLVM_ENABLE_ZLIB=OFF \
        -DLLVM_ENABLE_ZSTD=OFF \
        -DLLVM_BUILD_32_BITS=OFF

RUN cmake --build build -j8 && \
    cmake --install build --prefix $(pwd)/distrib 

RUN cmake --build build -t clean

WORKDIR /apps/z3

RUN cmake -S . -B build && \
    cmake --build build -j8 && \
    cmake --install build --prefix $(pwd)/distrib 

RUN cmake --build build -t clean

WORKDIR /apps/boost

RUN ./bootstrap.sh --prefix=$(pwd)/distrib && ./b2 install

RUN ./b2 --clean

WORKDIR /apps/Halide

RUN cmake -S . -B build \
          -DCMAKE_BUILD_TYPE=Release \
          -DHalide_LLVM_ROOT=/apps/llvm-project/distrib && \
    cmake --build build -j8 && \
    cmake --install build --prefix $(pwd)/distrib

RUN cmake --build build -t clean

WORKDIR /apps/stencil-lifting

RUN cmake -S . -B build/release -DCMAKE_BUILD_TYPE=Release \
               -DZ3_DIR=/apps/z3/distrib/lib/cmake/z3 \
               -DBoost_DIR=/apps/boost/distrib/lib/cmake && \
    cmake --build build/release -j8
    

RUN pip3 install -r ./deps/requirements.txt

ENV HALIDE_DISTRIB_PATH=/apps/Halide/distrib
ENV LD_LIBRARY_PATH=${HALIDE_DISTRIB_PATH}/lib
ENV FORT=/apps/llvm-project/distrib/bin/flang-new

