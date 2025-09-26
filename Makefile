MF=	Makefile

FC=ftn
FFLAGS=-cpp -ffree-line-length-none
LFLAGS=-lhdf5_fortran -lnetcdff -lnetcdf

CC=cc
CFLAGS=
LCFLAGS=


EXE=	benchio

SRC= \
	serial.f90 \
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

all:	$(EXE) 

$(EXE):	$(OBJ) $(COBJ)
	$(FC) $(FFLAGS) -o $@ $(OBJ) $(LFLAGS)

$(OBJ):	$(MF)
$(COBJ): $(MF)

benchio.o: serial.o mpiio.o benchclock.o netcdf.o hdf5.o 

clean:
	rm -f $(OBJ) *.mod $(EXE) core
