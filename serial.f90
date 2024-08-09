!
! The "serial" IO routine takes a communicator argument. This enables
! it to be used for a variety of purposes:
!
! MPI_COMM_WORLD: standard "master" IO from a single process
! MPI_COMM_NODE:  file-per-node
! MPI_COMM_SELF:  file-per-process
!

module ioserial

  contains

subroutine serialwrite(filename, iodata, n1, n2, n3, repeats, comm)

  character*(*) :: filename
  
  integer :: n1, n2, n3, repeats
  double precision, dimension(0:n1+1,0:n2+1,0:n3+1) :: iodata
  double precision, dimension(:), allocatable :: tempdata
  integer :: comm, ierr, rank, size, repeat
  integer, parameter :: iounit = 10
  
  integer :: i, j, k
  integer :: approach
  logical :: first = .true.
  
  approach = 2 
  
  call MPI_Comm_size(comm, size, ierr)
  call MPI_Comm_rank(comm, rank, ierr)
  
!  Write same amount of data as the parallel write but do it all from rank 0
!  This is just to get a baseline figure for serial IO performance - note
!  that the contents of the file will be different from the parallel calls
  if (rank == 0) then

     ! Copy the data out for the arrays into a single array
     ! This removes the halos and allows for a single I/O operation
     if(approach .eq. 2) then
        allocate(tempdata(n1*n2*n3))
        do i=1,n3
           do j=1,n2
              do k=1,n1
                 tempdata(k+((j-1)*n1)+((i-1)*n1*n2)) = iodata(k,j,i)       
              end do
           end do
        end do
     end if
     
     if(approach == 0) then
        open(file=filename, unit=iounit, access='stream')
        do i = 1, size
           do repeat = 1, repeats
              write(iounit) iodata(1:n1, 1:n2, 1:n3)
              call fseek(iounit, 0, 0)
           end do
        end do
        close(iounit)
     else if(approach == 1) then
        open(file=filename, unit=iounit, access='direct', recl=8*n1)
        do i = 1, size
           do repeat = 1, repeats
              do k = 1,n3
                 do j = 1,n2
                    write(iounit, rec=1) iodata(1:n1, j, k)
                 end do
              end do
              call fseek(iounit, 0, 0)
           end do
        end do
        close(iounit)
     else if(approach == 2) then
        open(file=filename, unit=iounit, access='direct', recl=8*n1*n2*n3)
        do i = 1, size
           do repeat = 1, repeats
              write(iounit, rec=1) tempdata
              call fseek(iounit, 0, 0)
           end do
        end do
        close(iounit)
     else
        write(*,*) 'No I/O Occurring. Do not believe the bandwidth results!'
     end if
     
     if(approach .eq. 2) then
        deallocate(tempdata)
     end if
     
  end if
  
end subroutine serialwrite


subroutine serialread(filename, iodata, n1, n2, n3, repeats, comm)

  character*(*) :: filename

  integer :: n1, n2, n3, repeats
  double precision, dimension(0:n1+1,0:n2+1,0:n3+1) :: iodata
  double precision, dimension(:), allocatable :: tempdata
  integer :: comm, ierr, rank, size, repeat
  integer, parameter :: iounit = 10

  integer :: i,j,k
  integer :: approach

  approach = 2

  call MPI_Comm_size(comm, size, ierr)
  call MPI_Comm_rank(comm, rank, ierr)

!  Write same amount of data as the parallel read but do it all from rank 0
!  This is just to get a baseline figure for serial IO performance - note
!  that the contents of the file will be different from the parallel calls
  if (rank == 0) then
     
     if(approach .eq. 2) then
        allocate(tempdata(n1*n2*n3))
     end if
     
     if(approach == 0) then
        open(file=filename, unit=iounit, access='stream')
        do i = 1, size
           do repeat = 1, repeats
              read(iounit) iodata(1:n1, 1:n2, 1:n3)
              call fseek(iounit, 0, 0)
           end do
        end do
        close(iounit)
     else if(approach == 1) then
        open(file=filename, unit=iounit, access='direct', recl=8*n1)
        do i = 1, size
           do repeat = 1, repeats
              do k = 1,n3
                 do j = 1,n2
                    read(iounit, rec=1) iodata(1:n1, j, k)
                 end do
              end do
              call fseek(iounit, 0, 0)
           end do
        end do
        close(iounit)
     else if(approach == 2) then
        open(file=filename, unit=iounit, access='direct', recl=8*n1*n2*n3)
        do i = 1, size
           do repeat = 1, repeats
              read(iounit, rec=1) tempdata
              call fseek(iounit, 0, 0)
           end do
        end do
        close(iounit)
     else
        write(*,*) 'No I/O Occurring. Do not believe the bandwidth results!'
     end if
     
     if(approach .eq. 2) then
        do i=1,n3
           do j=1,n2
              do k=1,n1
                 iodata(k,j,i) = tempdata(k+((j-1)*n1)+((i-1)*n1*n2))
              end do
           end do
        end do
        
        deallocate(tempdata)
     end if
     
  end if
  
end subroutine serialread


end module ioserial

