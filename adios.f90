module adios

  use mpi
  use adios2
  use benchutil
  use benchclock

  implicit none

contains

subroutine adioswrite(filename, iodata, n1, n2, n3, repeats, cartcomm, initialise_time)

! ADIOS variables
  type(adios2_adios) :: adios2obj
  type(adios2_io) :: io
  type(adios2_engine) :: bp_writer
  type(adios2_variable) :: var_g 
  
  character*(*) :: filename
  
  integer :: n1, n2, n3, repeats
  double precision, dimension(0:n1+1,0:n2+1,0:n3+1) :: iodata
  double precision, dimension(n1,n2,n3) :: out_data

  double precision :: initialise_time, t0, t1

  integer*8, dimension(ndim) :: arraysize, arraystart
  integer*8, dimension(ndim) :: arraygsize, arraysubsize

  integer :: cartcomm, ierr, rank, size, repeat

  integer, dimension(ndim) :: dims, coords
  logical, dimension(ndim) :: periods


! initialise ADIOS using the MPI communicator and config file
  t0 = benchtime()
  call adios2_init(adios2obj, "adios2.xml", cartcomm, ierr)
  call adios2_declare_io(io, adios2obj, 'Output', ierr )

  t1 =  benchtime()
  initialise_time = t1 - t0
        
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
  out_data = iodata(1:arraysubsize(1),1:arraysubsize(2),1:arraysubsize(3))


! Define the global array
  call adios2_define_variable(var_g, io, "GlobalArray", adios2_type_dp, &
                              ndim, arraygsize, arraystart, arraysubsize, &
                              adios2_constant_dims, ierr)

! The approach used here is not ideal as it requires multiple file opens and
! closes if there are any repeats. However, ADIOS2 does not allow overwriting
! data in the file if you write multiple times (i.e. call put more than once) so
! doing I/O with repeats would end up with a file/storage output that would be 
! repeats times larger than other benchmarks. The alternative, as we are doing
! here, is to open and close the files each time and that overwrites the data.
  do repeat = 1, repeats

  call adios2_open (bp_writer, io, filename, adios2_mode_write, ierr)

  call adios2_begin_step( bp_writer, ierr)
 
      call adios2_put( bp_writer, var_g, out_data, ierr)

  call adios2_end_step(bp_writer, ierr)

  call adios2_close(bp_writer, ierr)

  end do

  call adios2_finalize(adios2obj, ierr)


end subroutine adioswrite


subroutine adiosread(filename, iodata, n1, n2, n3, repeats, cartcomm, initialise_time)

! ADIOS variables
  type(adios2_adios) :: adios2obj
  type(adios2_io) :: io
  type(adios2_engine) :: bp_reader
  type(adios2_variable) :: var_g 
  
  character*(*) :: filename
  
  integer :: n1, n2, n3, repeats
  integer :: i, j, k
  double precision, dimension(0:n1+1,0:n2+1,0:n3+1) :: iodata
  double precision, dimension(n1,n2,n3) :: in_data

  double precision :: initialise_time, t0, t1

  integer*8, dimension(ndim) :: arraysize, arraystart
  integer*8, dimension(ndim) :: arraygsize, arraysubsize

  integer :: cartcomm, ierr, rank, size, repeat

  integer, dimension(ndim) :: dims, coords
  logical, dimension(ndim) :: periods

! initialise ADIOS using the MPI communicator and config file
  t0 = benchtime()
!  call adios2_init(adios2obj, "adios2.xml", cartcomm, ierr)
  call adios2_init(adios2obj, cartcomm, ierr)
  if(ierr .ne. 0) then
    write(*,*) 'Error with adios2_init'
  end if
 
  call adios2_declare_io(io, adios2obj, 'Output', ierr )
  if(ierr .ne. 0) then
    write(*,*) 'Error with adios2_declare_io'
  end if
  t1 =  benchtime()
  initialise_time = t1 - t0
        
  call MPI_Comm_size(cartcomm, size, ierr)
  call MPI_Comm_rank(cartcomm, rank, ierr)

  call MPI_Cart_get(cartcomm, ndim, dims, periods, coords, ierr)

  arraysize(:) = [n1+2, n2+2, n3+2]

! Subtract halos for array subsize

  arraysubsize(:)   = [n1, n2, n3]

! Define the global array size and the start coordinates of this ranks array
  arraygsize(:) = arraysubsize(:) * dims(:)
  arraystart(:) = arraysubsize(:) * coords(:)

! Open the file
  call adios2_open (bp_reader, io, filename, adios2_mode_read, ierr)
  if(ierr .ne. 0) then
    write(*,*) 'Error with adios2_open'
    return 
 end if

  call adios2_begin_step(bp_reader, ierr)
  if(ierr .ne. 0) then
    write(*,*) 'Error with adios2_begin_step'
    return
  end if

! Define the global array
  call adios2_inquire_variable(var_g, io, "GlobalArray", ierr)
  if(ierr .ne. 0) then
    write(*,*) 'Error with adios2_inquire_variable'
    return
  end if
 
  if(.not. var_g%valid) then
     write(*,*) 'Read variable "GlobalArray" is not valid in the file',filename
     return
  end if

  call adios2_set_selection(var_g, ndim, arraystart, arraysubsize, ierr)
  if(ierr .ne. 0) then
    write(*,*) 'Error with adios2_set_selection'
    return
  end if

  do repeat = 1, repeats

      call adios2_get(bp_reader, var_g, in_data, ierr)
      if(ierr .ne. 0) then
        write(*,*) 'Error with adios2_get'
        return
      end if

  end do

! End the output
  call adios2_end_step(bp_reader, ierr)
  if(ierr .ne. 0) then
    write(*,*) 'Error with adios2_end_step'
    return
  end if

! Close the file
  call adios2_close(bp_reader, ierr)
  if(ierr .ne. 0) then
    write(*,*) 'Error with adios2_close'
    return
  end if

  call adios2_finalize(adios2obj, ierr)
  if(ierr .ne. 0) then
    write(*,*) 'Error with adios2_finalize'
    return
  end if

! Copy the data back
  iodata(1:arraysubsize(1),1:arraysubsize(2),1:arraysubsize(3)) = in_data


end subroutine adiosread


end module adios
