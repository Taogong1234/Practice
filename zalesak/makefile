run:
	mpif90 zalesak.f90 -o zalesak.exe 
	mpirun -n 5 ./zalesak.exe
	python painter.py

.PHONY:clean
clean: 
	rm -f *.exe
	# rm -f *.gif
	rm -f *.dat
