default: run

test_mod.o: test_mod.f90 Makefile
	gfortran -c test_mod.f90

test.o: test.f90 test_mod.o Makefile
	gfortran -c test.f90

test.exe: test.o Makefile 
	gfortran -o test.exe test.o test_mod.o

run: test.exe Makefile
	./test.exe

clean:	
	rm -f test.{exe,o,mod}  test_mod.{o,mod}
