module iohdf5

  use hdf5
  use mpi
  use benchutil

  implicit none

contains

subroutine hdf5write(filename, iodata, n1, n2, n3, cartcomm)

  character*(*) :: filename
  
  integer :: n1, n2, n3
  double precision, dimension(0:n1+1,0:n2+1,0:n3+1) :: iodata

  integer :: info = MPI_INFO_NULL
  integer(hsize_t), dimension(ndim) :: dimsf  ! dataset dimensions.

  character(len=8), parameter :: dsetname = "IntArray" ! Dataset name

  integer(hid_t) :: file_id       ! file identifier 
  integer(hid_t) :: dset_id       ! dataset identifier 
  integer(hid_t) :: filespace     ! dataspace identifier in file 
  integer(hid_t) :: memspace      ! dataspace identifier in memory
  integer(hid_t) :: plist_id      ! property list identifier 

  integer(hsize_t), dimension(ndim) :: count  
  integer(hssize_t), dimension(ndim) :: offset 

  integer, dimension(ndim) :: arraysize, arraystart
  integer, dimension(ndim) :: arraygsize, arraysubsize

  integer :: cartcomm, ierr, rank, size

  integer, dimension(ndim) :: dims, coords
  logical, dimension(ndim) :: periods

  integer :: ncid, varid, oldmode, dimids(ndim)
  integer :: x_dimid, y_dimid, z_dimid

  call MPI_Comm_size(cartcomm, size, ierr)
  call MPI_Comm_rank(cartcomm, rank, ierr)

  call MPI_Cart_get(cartcomm, ndim, dims, periods, coords, ierr)

  arraysize(:) = [n1+2, n2+2, n3+2]

! Subtract halos for array subsize

  arraysubsize(:)   = [n1, n2, n3]

!
! Define filetype for this process, ie what portion of the global array
! this process owns; starting positions use C-indexing (ie counting from 0).
!

  arraygsize(:) = arraysubsize(:) * dims(:)
  arraystart(:) = arraysubsize(:) * coords(:) + 1   ! Use Fortran indexing
 
  dimsf(:) = arraygsize(:)

  ! Initialise the count and offset arrays
  count(1) = n1     ! Defines the number of values each proc dumps to 
  count(2) = n2
  count(3) = n3                                   ! the HDF5 file. 

  offset(:) = coords(:) * count(:)  ! Defines the offset used in the HDF5 file

  ! Initialize FORTRAN predefined datatypes
  CALL h5open_f(ierr) 
  
  ! Setup file access property list with parallel I/O access.
  CALL h5pcreate_f(H5P_FILE_ACCESS_F, plist_id, ierr)
  CALL h5pset_fapl_mpio_f(plist_id, cartcomm, info, ierr)

  ! Create the file collectively.
  CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, ierr, &
      access_prp = plist_id)
  CALL h5pclose_f(plist_id, ierr)
  
  ! Create the data space for the  dataset. 
  CALL h5screate_simple_f(ndim, dimsf, filespace, ierr)
  
  ! Create the dataset with default properties.
  CALL h5dcreate_f(file_id, dsetname, H5T_NATIVE_DOUBLE, filespace, &
                      dset_id, ierr)
  CALL h5sclose_f(filespace, ierr)
  ! Each process defines dataset in memory and writes it to the hyperslab
  ! in the file. 
  CALL h5screate_simple_f(ndim, count, memspace, ierr) 

  ! Select hyperslab in the file.
  CALL h5dget_space_f(dset_id, filespace, ierr)
  CALL h5sselect_hyperslab_f (filespace, H5S_SELECT_SET_F, offset, &
      count, ierr)
     
  ! Create property list for collective dataset write
  CALL h5pcreate_f(H5P_DATASET_XFER_F, plist_id, ierr) 
  CALL h5pset_dxpl_mpio_f(plist_id, H5FD_MPIO_COLLECTIVE_F, ierr)
  
  ! Write the dataset collectively. 
  CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, iodata(1:n1, 1:n2, 1:n3), &
       dimsf, ierr, file_space_id = filespace, mem_space_id = memspace, &
      xfer_prp = plist_id)
! Write the dataset independently. Comment out the collective
! and use the following call to investigate non-collective performance.
!    CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, data, arraygsize, ierr, &
!                     file_space_id = filespace, mem_space_id = memspace)

  ! Close dataspaces.
  CALL h5sclose_f(filespace, ierr)
  CALL h5sclose_f(memspace, ierr)

  ! Close the dataset and property list.
  CALL h5dclose_f(dset_id, ierr)
  CALL h5pclose_f(plist_id, ierr)

  ! Close the file.
  CALL h5fclose_f(file_id, ierr)

  ! Close FORTRAN predefined datatypes.
  CALL h5close_f(ierr)

end subroutine hdf5write

subroutine hdf5read(filename, iodata, n1, n2, n3, cartcomm)

  character*(*) :: filename
  
  integer :: n1, n2, n3
  double precision, dimension(0:n1+1,0:n2+1,0:n3+1) :: iodata

  integer :: info = MPI_INFO_NULL
  integer(hsize_t), dimension(ndim) :: dimsf  ! dataset dimensions.

  character(len=8), parameter :: dsetname = "IntArray" ! Dataset name

  integer(hid_t) :: file_id       ! file identifier 
  integer(hid_t) :: dset_id       ! dataset identifier 
  integer(hid_t) :: filespace     ! dataspace identifier in file 
  integer(hid_t) :: memspace      ! dataspace identifier in memory
  integer(hid_t) :: plist_id      ! property list identifier 

  integer(hsize_t), dimension(ndim) :: count  
  integer(hssize_t), dimension(ndim) :: offset 

  integer, dimension(ndim) :: arraysize, arraystart
  integer, dimension(ndim) :: arraygsize, arraysubsize

  integer :: cartcomm, ierr, rank, size

  integer, dimension(ndim) :: dims, coords
  logical, dimension(ndim) :: periods

  integer :: ncid, varid, oldmode, dimids(ndim)
  integer :: x_dimid, y_dimid, z_dimid

  call MPI_Comm_size(cartcomm, size, ierr)
  call MPI_Comm_rank(cartcomm, rank, ierr)

  call MPI_Cart_get(cartcomm, ndim, dims, periods, coords, ierr)

  arraysize(:) = [n1+2, n2+2, n3+2]

! Subtract halos for array subsize

  arraysubsize(:)   = [n1, n2, n3]

!
! Define filetype for this process, ie what portion of the global array
! this process owns; starting positions use C-indexing (ie counting from 0).
!

  arraygsize(:) = arraysubsize(:) * dims(:)
  arraystart(:) = arraysubsize(:) * coords(:) + 1   ! Use Fortran indexing
 
  dimsf(:) = arraygsize(:)

  ! Initialise the count and offset arrays
  count(1) = n1     ! Defines the number of values each proc dumps to 
  count(2) = n2
  count(3) = n3                                   ! the HDF5 file. 

  offset(:) = coords(:) * count(:)  ! Defines the offset used in the HDF5 file

  ! Initialize FORTRAN predefined datatypes
  CALL h5open_f(ierr) 
  if(ierr .ne. 0) then
      write(*,*) 'Problem with h5open_f'
      return
  end if 
 
  ! Setup file access property list with parallel I/O access.
  CALL h5pcreate_f(H5P_FILE_ACCESS_F, plist_id, ierr)
  if(ierr .ne. 0) then
      write(*,*) 'Problem with h5pcreate_f'
      return
  end if
  CALL h5pset_fapl_mpio_f(plist_id, cartcomm, info, ierr)
  if(ierr .ne. 0) then
      write(*,*) 'Problem with h5pset_fapk_mpio_f'
      return
  end if

  ! Create the file collectively.
  CALL h5fopen_f(filename, H5F_ACC_RDONLY_F, file_id, ierr, &
      access_prp = plist_id)
  if(ierr .ne. 0) then
      write(*,*) 'Problem with h5fopen_f'
      return
  end if
  CALL h5pclose_f(plist_id, ierr)
  if(ierr .ne. 0) then
      write(*,*) 'Problem with h5pclose_f'
      return
  end if  

  ! Create the data space for the  dataset. 
  CALL h5screate_simple_f(ndim, dimsf, filespace, ierr)
  if(ierr .ne. 0) then
      write(*,*) 'Problem with h5screate_simple_f'
      return
  end if  

  ! Create the dataset with default properties.
  CALL h5dopen_f(file_id, dsetname, dset_id, ierr)
  if(ierr .ne. 0) then
      write(*,*) 'Problem with h5dcreate_f'
      return
  end if
  CALL h5sclose_f(filespace, ierr)
  if(ierr .ne. 0) then
      write(*,*) 'Problem with h5sclose_f'
      return
  end if
  ! Each process defines dataset in memory and reads it from the hyperslab
  ! in the file. 
  CALL h5screate_simple_f(ndim, count, memspace, ierr) 
  if(ierr .ne. 0) then
      write(*,*) 'Problem with h5screate_simple_f'
      return
  end if

  ! Select hyperslab in the file.
  CALL h5dget_space_f(dset_id, filespace, ierr)
  if(ierr .ne. 0) then
      write(*,*) 'Problem with h5dget_space_f'
      return
  end if
  CALL h5sselect_hyperslab_f (filespace, H5S_SELECT_SET_F, offset, &
      count, ierr)
  if(ierr .ne. 0) then
      write(*,*) 'Problem with h5sselect_hyperslab_f'
      return
  end if     

  ! Create property list for collective dataset read
  CALL h5pcreate_f(H5P_DATASET_XFER_F, plist_id, ierr) 
  if(ierr .ne. 0) then
      write(*,*) 'Problem with h5pcreate_f'
      return
  end if
  CALL h5pset_dxpl_mpio_f(plist_id, H5FD_MPIO_COLLECTIVE_F, ierr)
  if(ierr .ne. 0) then
      write(*,*) 'Problem with h5pset_dxpl_mpio_f'
      return
  end if

  ! Read the dataset collectively. 
  CALL h5dread_f(dset_id, H5T_NATIVE_DOUBLE, iodata(1:n1, 1:n2, 1:n3), &
       dimsf, ierr, file_space_id = filespace, mem_space_id = memspace, &
      xfer_prp = plist_id)
  if(ierr .ne. 0) then
      write(*,*) 'Problem with h5dread_f'
      return
  end if
! Read the dataset independently. Comment out the collective
! and use the following call to investigate non-collective performance.
!    CALL h5dread_f(dset_id, H5T_NATIVE_DOUBLE, data, arraygsize, ierr, &
!                     file_space_id = filespace, mem_space_id = memspace)

  ! Close dataspaces.
  CALL h5sclose_f(filespace, ierr)
  if(ierr .ne. 0) then
      write(*,*) 'Problem with h5sclose_f'
      return
  end if
  CALL h5sclose_f(memspace, ierr)
  if(ierr .ne. 0) then
      write(*,*) 'Problem with h5sclose_f'
      return
  end if

  ! Close the dataset and property list.
  CALL h5dclose_f(dset_id, ierr)
  if(ierr .ne. 0) then
      write(*,*) 'Problem with h5dclose_f'
      return
  end if
  CALL h5pclose_f(plist_id, ierr)
  if(ierr .ne. 0) then
      write(*,*) 'Problem with h5pclose_f'
      return
  end if

  ! Close the file.
  CALL h5fclose_f(file_id, ierr)
  if(ierr .ne. 0) then
      write(*,*) 'Problem with h5fclose_f'
      return
  end if

  ! Close FORTRAN predefined datatypes.
  CALL h5close_f(ierr)
  if(ierr .ne. 0) then
      write(*,*) 'Problem with h5close_f'
      return
  end if

end subroutine hdf5read


end module iohdf5
