MF=	Makefile

FC=mpif90
FFLAGS=
LFLAGS=-L/lustre/sw/hdf5parallel/1.10.6-intel18-mpt222/lib -lhdf5_fortran -lnetcdff -lnetcdf

EXE=	benchio

SRC= \
	benchio.f90 \
	mpiio.f90 \
	netcdf.f90 \
	hdf5.f90 \
	benchclock.f90


#
# No need to edit below this line
#

.SUFFIXES:
.SUFFIXES: .f90 .o

OBJ=	$(SRC:.f90=.o)

.f90.o:
	$(FC) $(FFLAGS) -c $<

all:	$(EXE)

$(EXE):	$(OBJ)
	$(FC) $(FFLAGS) -o $@ $(OBJ) $(LFLAGS)

$(OBJ):	$(MF)

benchio.o: mpiio.o benchclock.o netcdf.o hdf5.o

clean:
	rm -f $(OBJ) *.mod $(EXE) core

tar:
	tar --exclude-vcs -cvf $(EXE).tar $(MF) $(SRC) benchio.pbs \
		defstriped/README striped/README unstriped/README
