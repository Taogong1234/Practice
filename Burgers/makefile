run:
	mpif90 Burgers.f90 -o Burgers.exe 
	mpirun -n 6 ./Burgers.exe
	python painter.py 

.PHONY:clean
clean: 
	rm -f *.exe
	# rm -f *.gif
	rm -f *.dat
