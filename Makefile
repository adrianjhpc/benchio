MF=	Makefile

FC=mpiifort
FFLAGS=
LFLAGS=-L/mnt/lustre/indy2lfs/sw/hdf5parallel/1.10.6-intel19-mpt225/lib -lhdf5_fortran -lnetcdff -lnetcdf

CC=mpiicc
CFLAGS=-I/home/nx01/nx01/adrianj/uuid/include
LCFLAGS=-L/home/nx01/nx01/adrianj/uuid/lib -luuid


EXE=	benchio

SRC= \
        benchutil.f90 \
	benchio.f90 \
	mpiio.f90 \
	netcdf.f90 \
	hdf5.f90 \
	benchclock.f90


#
# No need to edit below this line
#

.SUFFIXES:
.SUFFIXES: .f90 .o .c

OBJ=	$(SRC:.f90=.o)
COBJ=   $(SRC:.c=.o)

.f90.o:
	$(FC) $(FFLAGS) -c $<

.c.o:
	$(CC) $(CFLAGS) -c $<

all:	$(EXE) daos

$(EXE):	$(OBJ) $(COBJ)
	$(FC) $(FFLAGS) -o $@ $(OBJ) $(LFLAGS)

$(OBJ):	$(MF)
$(COBJ): $(MF)

benchio.o: mpiio.o benchclock.o netcdf.o hdf5.o daos.o

clean:
	rm -f $(OBJ) *.mod $(EXE) core
