default: run

FC = gfortran

test_mod.o: test_mod.f90 Makefile
	$(FC) -c test_mod.f90

test.o: test.f90 test_mod.o Makefile
	$(FC) -c test.f90

test.exe: test.o test_mod.o Makefile 
	$(FC) -o test.exe test.o test_mod.o

run: test.exe Makefile
	./test.exe

clean:	
	rm -f test.exe test.o test.mod  test_mod.o test_mod.mod
