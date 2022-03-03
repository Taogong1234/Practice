run:
	mpif90 poisson.f90 -o poisson.exe 
	mpirun -n 1 ./poisson.exe
	python painter.py 

.PHONY:clean
clean: 
	rm -f *.exe
	rm -f *.jpg
	rm -f *.dat
	rm -f poisson
