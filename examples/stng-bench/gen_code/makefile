# HALIDE_DISTRIB_PATH ?= path/to/halide/distribution
HALIDE_DISTRIB_PATH ?= ~/Develop/Halide-19.0.0/distrib
TARGET = host
BIN = ../../../out/bin
OBJ = ../../../out/obj
FORT_DIR = ../fort

SRC = $(wildcard *_halide.cpp)
OBJS = $(SRC:_halide.cpp=.drv.o) $(SRC:_halide.cpp=.fort.o)
TARGET = $(SRC:_halide.cpp=.halide)

CXX ?= g++
FORT ?= gfortran
FFLAGS = -O0

ifeq ($(SCALE), down)
CFLAGS = -D_3D_1=256 -D_3D_2=256 -D_3D_3=256 \
         -D_2D_1=4e3 -D_2D_2=4e3 \
		 -D_1D_1=1e7
endif

ifeq ($(MODE), debug)
CFLAGS += -g -O0
FFLAGS += -g
else
CFLAGS += -O3
endif

LDFLAGS = -ldl -lpthread -L $(HALIDE_DISTRIB_PATH)/lib -lHalide
ifeq ($(FORT), gfortran)
LDFLAGS += -lgfortran
endif

.SECONDARY: $(addprefix $(OBJ)/, $(OBJS))

build: $(addprefix $(BIN)/, $(TARGET))

$(OBJ)/%.fort.o: $(FORT_DIR)/%.f90
	@mkdir -p $(@D)
	$(FORT) $(FFLAGS) $< -c -o $@ 

$(OBJ)/%.fort.o: $(FORT_DIR)/%.f
	@mkdir -p $(@D)
	$(FORT) $(FFLAGS) $< -c -o $@ 

$(OBJ)/%.drv.o: %_halide.cpp
	$(CXX) $(CFLAGS) -std=c++17 -I $(HALIDE_DISTRIB_PATH)/include $< -c -o $@

$(BIN)/%.halide: $(OBJ)/%.fort.o $(OBJ)/%.drv.o
	@mkdir -p $(@D)
	$(CXX) $^ -o $@ $(LDFLAGS)

clean:
	rm -f $(addprefix $(OBJ)/, $(OBJS)) $(addprefix $(BIN)/, $(TARGET))