# Variables
FC = mpifort
FCFLAGS = -march=native -O3 -ffast-math -funroll-loops -std=f2018 
OBJS = prime_module.o print_module.o

# Targets
all: main

main: $(OBJS) main.f90
	$(FC) $(FCFLAGS) -o $@ $^

prime_module.o: prime_module.f90
	$(FC) $(FCFLAGS) -c -o $@ $<

print_module.o: print_module.f90
	$(FC) $(FCFLAGS) -c -o $@ $<

run: main
	@echo "Running main..."
	@time mpirun -np 4 ./main

clean:
	rm -f main $(OBJS) *.mod

.PHONY: all run clean