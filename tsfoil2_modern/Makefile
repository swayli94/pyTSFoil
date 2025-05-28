# Makefile for compiling TSFOIL2 modern Fortran code

# Compiler and flags
FC = gfortran
FFLAGS = -O2 -Wall -fcheck=all

# Target executable
TARGET = tsfoil2_modern

# Object files
OBJS = tsfoil_data.o spline_module.o numerical_solvers.o grid_module.o tsfoil_io.o airfoil_module.o solver_module.o output_module.o main.o

# Default target
all: $(TARGET)

# Linking rule
$(TARGET): $(OBJS)
	$(FC) $(FFLAGS) -o $@ $^

# Dependencies and compilation rules
tsfoil_data.o: tsfoil_data.f90
	$(FC) $(FFLAGS) -c $<

spline_module.o: spline_module.f90
	$(FC) $(FFLAGS) -c $<

numerical_solvers.o: numerical_solvers.f90
	$(FC) $(FFLAGS) -c $<

grid_module.o: grid_module.f90 tsfoil_data.o
	$(FC) $(FFLAGS) -c $<

tsfoil_io.o: tsfoil_io.f90 tsfoil_data.o
	$(FC) $(FFLAGS) -c $<

airfoil_module.o: airfoil_module.f90 tsfoil_data.o numerical_solvers.o spline_module.o
	$(FC) $(FFLAGS) -c $<

solver_module.o: solver_module.f90 tsfoil_data.o numerical_solvers.o grid_module.o
	$(FC) $(FFLAGS) -c $<

output_module.o: output_module.f90 tsfoil_data.o numerical_solvers.o
	$(FC) $(FFLAGS) -c $<

main.o: main.f90 tsfoil_data.o tsfoil_io.o spline_module.o numerical_solvers.o grid_module.o airfoil_module.o solver_module.o output_module.o
	$(FC) $(FFLAGS) -c $<

# Clean target
clean:
	rm -f *.o *.mod $(TARGET)

.PHONY: all clean
