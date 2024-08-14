module daos

  use iso_c_binding
  use mpi
  use daos_c_interface
  use benchutil
  use benchclock

  implicit none

contains

subroutine daoswrite(filename, iodata, n1, n2, n3, repeats, cartcomm, daosconfig, initialise_time, cont_name)
  
  implicit none

  integer, parameter :: check_data = 0
  character*(*) :: filename, cont_name
  
  integer :: n1, n2, n3, repeats, daosconfig
  double precision, dimension(0:n1+1,0:n2+1,0:n3+1) :: iodata
  double precision, dimension(n1*n2*n3) :: out_data, read_data

  double precision :: t0, t1, initialise_time

  integer*8, dimension(ndim) :: arraysize, arraystart
  integer*8, dimension(ndim) :: arraygsize, arraysubsize

  integer(kind=c_size_t) :: blocksize
  integer :: cartcomm, ierr, rank, size, delim_index

  integer, dimension(ndim) :: dims, coords
  logical, dimension(ndim) :: periods
  
  character(len=7) :: object_class, object_class_c
  character*(maxlen) :: object_type_name, pool_name, pool_name_c, cont_name_c

  call MPI_Comm_size(cartcomm, size, ierr)
  call MPI_Comm_rank(cartcomm, rank, ierr)

  call MPI_Cart_get(cartcomm, ndim, dims, periods, coords, ierr)

  object_type_name = char(0)
  pool_name = char(0)

  call split_string(filename, object_type_name, pool_name, "/", delim_index)

  call convert_to_c_string(trim(pool_name), pool_name_c)
  call convert_to_c_string(trim(cont_name), cont_name_c)

  if(object_type_name(1:delim_index-1) == 'unstriped') then
     object_class =  "OC_S1"
  else if(object_type_name(1:delim_index-1) == 'defstriped') then
     object_class =  "OC_S8"
  else if(object_type_name(1:delim_index-1) == 'striped') then
     object_class =  "OC_SX"
  else
     write(*,*) 'Problem defining the object class in DAOS write'
     write(*,*) 'Aborting that test'
     return
  end if

  call convert_to_c_string(trim(object_class), object_class_c)

  if(rank == 0) then
     write(*,*) 'DAOS Object Class ', trim(object_class)
  end if

  arraysize(:) = [n1+2, n2+2, n3+2]

! Subtract halos for array subsize

  arraysubsize(:)   = [n1, n2, n3]

! Define the global array size and the start coordinates of this ranks array
  arraygsize(:) = arraysubsize(:) * dims(:)
  arraystart(:) = arraysubsize(:) * coords(:)

! Get a copy of the array with the halos removed
  out_data = pack(iodata(1:arraysubsize(1),1:arraysubsize(2),1:arraysubsize(3)),.true.)

! Open the pool and create a container
  t0 = benchtime()
  call daos_initialise(pool_name_c, cont_name_c, cartcomm)
  t1 = benchtime()
  initialise_time = t1 - t0

  blocksize = 1024*1024

     
  if(rank == 0) then
     
     if(daosconfig == 0) then
        
        write(*,*) 'DAOS separate arrays'
        
     else if(daosconfig == 1) then
        
        write(*,*) 'DAOS single array block'
     
     else if(daosconfig == 2) then
        
        write(*,*) 'DAOS array separate rows/columns'

     else if(daosconfig == 3) then
        
        write(*,*) 'DAOS separate objects'

     else if(daosconfig == 4) then
        
        write(*,*) 'DAOS single object block'

     else if(daosconfig == 5) then
        
        write(*,*) 'DAOS object separate rows/columns'

     else

        write(*,*) 'Problem with daosconfig parameter'

     end if
   
  end if
     
  if(daosconfig .ge. 0 .and. daosconfig .le. 2) then


     call daos_write_array(ndim, arraysize, arraygsize, arraysubsize, arraystart, out_data, object_class_c, blocksize, repeats, check_data, daosconfig, cartcomm)


  else if(daosconfig .ge. 3 .and. daosconfig .le. 5) then


     call daos_write_object(ndim, arraysize, arraygsize, arraysubsize, arraystart, out_data, object_class_c, blocksize, repeats, check_data, daosconfig, cartcomm)


  else

     write(*,*) 'Problem with daosconfig parameter'

  end if
  
  if(check_data == 1) then
     
     if(rank == 0) then
        write(*,*) 'checking data so do not trust the timings'
     end if
     
     if(daosconfig .ge. 0 .and. daosconfig .le. 2) then

        call daos_read_array(ndim, arraysize, arraygsize, arraysubsize, arraystart, read_data, object_class_c, repeats, daosconfig, cartcomm)
     
     else if(daosconfig .ge. 3 .and. daosconfig .le. 5) then

        call daos_read_object(ndim, arraysize, arraygsize, arraysubsize, arraystart, read_data, object_class_c, repeats, daosconfig, cartcomm)

     else
        
        write(*,*) 'Problem with daosconfig parameter'
        
     end if


     if(.not. all(out_data .eq. read_data)) then
        write(*,*) rank,'original ',out_data,' read ',read_data
        write(*,*) rank,'out data and read data do not match'
     end if
     
  end if
  
!  call daos_finish(cartcomm)

end subroutine daoswrite

subroutine daosread(filename, iodata, n1, n2, n3, repeats, cartcomm, daosconfig, initialise_time, cont_name)
  
  implicit none

  character*(*) :: filename, cont_name
  
  integer :: n1, n2, n3, repeats, daosconfig
  double precision, dimension(0:n1+1,0:n2+1,0:n3+1) :: iodata
  double precision, dimension(n1*n2*n3) :: read_data

  double precision :: t0, t1, initialise_time

  integer*8, dimension(ndim) :: arraysize, arraystart
  integer*8, dimension(ndim) :: arraygsize, arraysubsize

  integer(kind=c_size_t) :: blocksize
  integer :: cartcomm, ierr, rank, size, delim_index

  integer, dimension(ndim) :: dims, coords
  logical, dimension(ndim) :: periods
  
  character(len=7) :: object_class, object_class_c
  character*(maxlen) :: object_type_name, pool_name, pool_name_c, cont_name_c

  call MPI_Comm_size(cartcomm, size, ierr)
  call MPI_Comm_rank(cartcomm, rank, ierr)

  call MPI_Cart_get(cartcomm, ndim, dims, periods, coords, ierr)

  object_type_name = char(0)
  pool_name = char(0)

  call split_string(filename, object_type_name, pool_name, "/", delim_index)

  call convert_to_c_string(trim(pool_name), pool_name_c)
  call convert_to_c_string(trim(cont_name), cont_name_c)

  if(object_type_name(1:delim_index-1) == 'unstriped') then
     object_class =  "OC_S1"
  else if(object_type_name(1:delim_index-1) == 'defstriped') then
     object_class =  "OC_S8"
  else if(object_type_name(1:delim_index-1) == 'striped') then
     object_class =  "OC_SX"
  else
     write(*,*) 'Problem defining the object class in DAOS write'
     write(*,*) 'Aborting that test'
     return
  end if

  call convert_to_c_string(trim(object_class), object_class_c)

  if(rank == 0) then
     write(*,*) 'DAOS Object Class ', trim(object_class)
  end if

  arraysize(:) = [n1+2, n2+2, n3+2]

! Subtract halos for array subsize

  arraysubsize(:)   = [n1, n2, n3]

! Define the global array size and the start coordinates of this ranks array
  arraygsize(:) = arraysubsize(:) * dims(:)
  arraystart(:) = arraysubsize(:) * coords(:)

! Open the pool and create a container
  t0 = benchtime()
  call daos_initialise(pool_name_c, cont_name_c, cartcomm)
  t1 = benchtime()
  initialise_time = t1 - t0

  blocksize = 1024*1024

     
  if(rank == 0) then
     
     if(daosconfig == 0) then
        
        write(*,*) 'DAOS separate arrays'
        
     else if(daosconfig == 1) then
        
        write(*,*) 'DAOS single array block'
     
     else if(daosconfig == 2) then
        
        write(*,*) 'DAOS array separate rows/columns'

     else if(daosconfig == 3) then
        
        write(*,*) 'DAOS separate objects'

     else if(daosconfig == 4) then
        
        write(*,*) 'DAOS single object block'

     else if(daosconfig == 5) then
        
        write(*,*) 'DAOS object separate rows/columns'

     else

        write(*,*) 'Problem with daosconfig parameter'

     end if
   
  end if
     
  if(daosconfig .ge. 0 .and. daosconfig .le. 2) then

     call daos_read_array(ndim, arraysize, arraygsize, arraysubsize, arraystart, read_data, object_class_c, repeats, daosconfig, cartcomm)

  else if(daosconfig .ge. 3 .and. daosconfig .le. 5) then

     call daos_read_object(ndim, arraysize, arraygsize, arraysubsize, arraystart, read_data, object_class_c, repeats, daosconfig, cartcomm)

  else

     write(*,*) 'Problem with daosconfig parameter'

  end if
    
!  call daos_finish(cartcomm)

end subroutine daosread


subroutine convert_to_c_string(input_string, output_string)

  use iso_c_binding, only: c_char, c_null_char

  implicit none

  character(len=*), intent(in) :: input_string
  character(kind=c_char), dimension(*), intent(out) :: output_string
  
  integer :: i
  
  do i = 1, len(input_string)
     output_string(i) = input_string(i:i)
  end do
  output_string(len(input_string)+1) = c_null_char
  
end subroutine convert_to_c_string

end module daos
