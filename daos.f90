module daos

  use iso_c_binding
  use mpi
  use daos_c_interface
  use benchutil

  implicit none

contains

subroutine daoswrite(filename, iodata, n1, n2, n3, cartcomm)
  
  character*(*) :: filename
  
  integer :: n1, n2, n3
  double precision, dimension(0:n1+1,0:n2+1,0:n3+1) :: iodata
  double precision, dimension(n1*n2*n3) :: out_data, read_data

  integer*8, dimension(ndim) :: arraysize, arraystart
  integer*8, dimension(ndim) :: arraygsize, arraysubsize

  integer(kind=c_size_t) :: blocksize
  integer :: cartcomm, ierr, rank, size

  integer, dimension(ndim) :: dims, coords
  logical, dimension(ndim) :: periods
        
  call MPI_Comm_size(cartcomm, size, ierr)
  call MPI_Comm_rank(cartcomm, rank, ierr)

  call MPI_Cart_get(cartcomm, ndim, dims, periods, coords, ierr)

  arraysize(:) = [n1+2, n2+2, n3+2]

! Subtract halos for array subsize

  arraysubsize(:)   = [n1, n2, n3]

! Define the global array size and the start coordinates of this ranks array
  arraygsize(:) = arraysubsize(:) * dims(:)
  arraystart(:) = arraysubsize(:) * coords(:)

! Get a copy of the array with the halos removed
  out_data = pack(iodata(1:arraysubsize(1),1:arraysubsize(2),1:arraysubsize(3)),.true.)

! Open the pool and create a container
  call daos_initialise(filename, cartcomm)

  blocksize = 1024*1024

  call daos_write(arraysize, arraygsize, arraysubsize, arraystart, out_data, "OC_SX", blocksize, cartcomm)

  call daos_read(arraysize, arraygsize, arraysubsize, arraystart, read_data, "OC_SX", cartcomm)

  call daos_finish(cartcomm)

end subroutine daoswrite

end module daos
