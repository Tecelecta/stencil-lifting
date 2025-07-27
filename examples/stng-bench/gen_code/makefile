# HALIDE_DISTRIB_PATH ?= path/to/halide/distribution
HALIDE_DISTRIB_PATH ?= ~/Develop/Halide-19.0.0/distrib
TARGET = host
BIN = ../../../out/bin
OBJ = ../../../out/obj
FORT_DIR = ../fort

SRC = $(wildcard *_halide.cpp)
OBJS = $(SRC:_halide.cpp=.drv.o) $(SRC:_halide.cpp=.fort.o)
TARGET = $(SRC:_halide.cpp=.halide)

CXX = g++
FORT = gfortran
ifeq ($(MODE), debug)
CFLAGS = -g -O0
else
CFLAGS = 
endif

.SECONDARY: $(addprefix $(OBJ)/, $(OBJS))

build: $(addprefix $(BIN)/, $(TARGET))

$(OBJ)/%.fort.o: $(FORT_DIR)/%.f90
	@mkdir -p $(@D)
	$(FORT) $(CFLAGS) $< -c -o $@ 

$(OBJ)/%.fort.o: $(FORT_DIR)/%.f
	@mkdir -p $(@D)
	$(FORT) $(CFLAGS) $< -c -o $@ 

$(OBJ)/%.drv.o: %_halide.cpp
	$(CXX) $(CFLAGS) -std=c++17 -I $(HALIDE_DISTRIB_PATH)/include $< -c -o $@

$(BIN)/%.halide: $(OBJ)/%.fort.o $(OBJ)/%.drv.o
	@mkdir -p $(@D)
	$(CXX) $^ -o $@ -ldl -lpthread -L $(HALIDE_DISTRIB_PATH)/lib -lHalide \
	-L /opt/homebrew/Cellar/gcc/15.1.0/lib/gcc/current/ \
	-lgfortran

clean:
	rm -rf $(BIN) $(OBJ)