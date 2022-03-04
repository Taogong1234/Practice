run:
	mpif90 mpipoisson.f90 -o mpipoisson.exe 
	mpirun -n 4 ./mpipoisson.exe
	python painter.py 

	# gfortran poisson.f90 -o poisson.exe 
	# ./poisson.exe
	# python painter.py 
.PHONY:clean
clean: 
	rm -f *.exe
	# rm -f *.jpg
	rm -f *.dat
	rm -f poisson
