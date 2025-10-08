## Build executable for the mesh translation tool Created by Chinmai Vellala 08/10/2025
FC      := gfortran                 # Fortran compiler
SRC     := types.f90 read_parameters.f90 read.f90 translate.f90 write.f90 main.f90
OBJ     := $(SRC:.f90=.o)           # Object files derived from sources
TARGET  := mesh_shift               # Final executable name

.PHONY: all clean run rebuild

all: $(TARGET)                       # Default target builds the executable

$(TARGET): $(OBJ)
	$(FC) $(OBJ) -o $@            # Link all objects into the final binary

%.o: %.f90
	$(FC) -c $<                   # Compile each source into an object file

run: $(TARGET)
	./$(TARGET)                   # Build (if needed) then run the program

rebuild: clean all                # Force a full rebuild from scratch

clean:
	rm -f $(OBJ) $(TARGET) *.mod   # Remove build outputs
