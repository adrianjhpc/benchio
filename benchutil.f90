module benchutil

  use mpi

  implicit none

  integer :: iolayermulti, iolayernode
  integer, parameter :: numiolayer = 8
  integer, parameter :: numstriping = 3
  integer, parameter :: numioparam = 2
  integer, parameter :: maxlen = 64
  integer, parameter :: ndim = 3
  character*(maxlen), dimension(numiolayer)  :: iostring, iolayername
  character*(maxlen), dimension(numioparam)  :: ioparam, ioparamval
  character*(maxlen), dimension(numstriping) :: stripestring

  logical, dimension(numiolayer)  :: doio = .false.
  logical, dimension(numstriping) :: dostripe = .false.

  integer :: rank, size, comm, cartcomm, iocomm, dblesize
  integer :: nodecomm, nodebosscomm, nodenum

  integer, dimension(ndim) :: dims, coords
  logical, dimension(ndim) :: periods = (/.false., .false., .false./)

  integer :: n1, n2, n3

  logical :: globalflag = .false.
  logical :: ioflag = .false.
  logical :: ioparamflag = .false.
  logical :: stripeflag = .false.

  integer, parameter :: n1def = 128
  integer, parameter :: n2def = 128
  integer, parameter :: n3def = 128

  integer, parameter :: iounit = 12
  integer, parameter :: kib = 1024
  integer, parameter :: mib = kib*kib
  integer, parameter :: gib = kib*mib


contains

  subroutine benchioinit()

    use mpi

    implicit none

    integer :: ierr, irank
    integer :: nodesize, noderank, spansize
    character*(MPI_MAX_PROCESSOR_NAME) :: nodename
    integer :: colour, key, namelen, tag
    integer, dimension(MPI_STATUS_SIZE) :: status
    character*(maxlen) :: pool_name, cont_name

    iolayermulti = 2
    iolayernode  = 3 

    iostring(1) = 'Serial'
    iostring(2) = 'Proc'
    iostring(3) = 'Node'
    iostring(4) = 'MPI-IO'
    iostring(5) = 'HDF5'
    iostring(6) = 'NetCDF'
    iostring(7) = 'Adios2'
    iostring(8) = 'DAOS'
    
    iolayername(1) = 'serial'
    iolayername(2) = 'proc'
    iolayername(3) = 'node'
    iolayername(4) = 'mpiio'
    iolayername(5) = 'hdf5'
    iolayername(6) = 'netcdf'
    iolayername(7) = 'adios'
    iolayername(8) = 'daos'

    ioparam(1) = '--daos.pool'
    ioparam(2) = '--daos.cont'

    ioparamval(1) = 'daos'
    ioparamval(2) = 'benchio'
    
    stripestring(1) = 'unstriped'
    stripestring(2) = 'striped'
    stripestring(3) = 'defstriped'
    
    call MPI_Init(ierr)
    
    comm = MPI_COMM_WORLD
    
    call MPI_Comm_size(comm, size, ierr)
    call MPI_Comm_rank(comm, rank, ierr)

    ! Create node-local communicators        
    call MPI_Comm_split_type(comm, MPI_COMM_TYPE_SHARED, rank, &
                             MPI_INFO_NULL, nodecomm, ierr)

    call MPI_Comm_size(nodecomm, nodesize, ierr)
    call MPI_Comm_rank(nodecomm, noderank, ierr)

! Create spanning communicator for all the node bosses
! Put everyone else into the same (junk) comm

    colour = min(noderank, 1)
    key    = noderank

    call MPI_Comm_split(comm, colour, key, nodebosscomm, ierr)

    call MPI_Comm_size(nodebosscomm, spansize, ierr)
    call MPI_Comm_rank(nodebosscomm, nodenum,  ierr)

! Make sure all ranks on node know the node number

    call MPI_Bcast(nodenum, 1, MPI_INTEGER, 0, nodecomm, ierr)

    call MPI_Get_processor_name(nodename, namelen, ierr)

! Get the stats

    if (rank == 0) then

       if (spansize /= 1) then
          write(*,*) "Running on ", spansize, " nodes"
       else
          write(*,*) "Running on ", spansize, " node"
       end if

       do irank = 0, spansize-1

          tag = nodesize

          if (irank /= 0) then

             call MPI_Recv(nodename, MPI_MAX_PROCESSOR_NAME, MPI_CHARACTER, &
                  irank, MPI_ANY_TAG, nodebosscomm, status, ierr)

             tag = status(MPI_TAG)
             call MPI_Get_count(status, MPI_CHARACTER, namelen, ierr)

          end if

          if (tag /= 1) then

             write(*,*) "Node number", irank, " is ", nodename(1:namelen), &
                  " with ", tag, " processes"
          else

             write(*,*) "Node number", irank, " is ", nodename(1:namelen), &
                  " with ", tag, " process"
          end if

       end do

    else if (noderank == 0) then

       ! Send number of processes as tag

       tag = nodesize

       call MPI_Ssend(nodename, namelen, MPI_CHARACTER, 0, tag, nodebosscomm, ierr)

    end if

  end subroutine benchioinit

  subroutine leaderdelete(filename, comm)

    use mpi

    implicit none

    character *(*) :: filename
    integer :: comm
    
    integer, parameter :: iounit = 15
    integer :: rank, ierr, stat

    call MPI_Comm_rank(comm, rank, ierr)

    if (rank == 0) then
       open(unit=iounit, iostat=stat, file=filename, status='old')
       if (stat.eq.0) close(unit=iounit, status='delete')
    end if

  end subroutine leaderdelete

  subroutine processarguments()

    use mpi

    implicit none
  
    integer :: ierr, numargs, iarg, iargstep, iolayer, ioparami, istriping
    character*(maxlen) :: argstring

    ! Parse the arguments
    doio(:) = .false.
    dostripe(:) = .false.  

    numargs = command_argument_count()
    
    if (numargs < 4) then
       if (rank == 0) then
          write(*,*) "usage: benchio (n1, n2, n3) (local|global) [serial] [proc] [node]"
          write(*,*) "       [mpiio] [hdf5] [netcdf] [adios] [daos [--daos.pool <pool_name>] [--daos.cont <cont_name>]] [unstriped] [striped] [fullstriped]"
       end if
       
       call MPI_Finalize(ierr)
       stop
       
    end if
    
    call get_command_argument(1, argstring)
    read(argstring,*) n1
    call get_command_argument(2, argstring)
    read(argstring,*) n2
    call get_command_argument(3, argstring)
    read(argstring,*) n3
    
    globalflag = .true.
    
    call get_command_argument(4, argstring)
    if (argstring == "local") globalflag = .false.
    
    iarg = 5
    do while(iarg <= numargs)
       ioflag = .false.
       stripeflag = .false.
       ioparamflag = .false.
       iargstep = 1
       
       call get_command_argument(iarg, argstring)      

       do iolayer = 1, numiolayer
          if (iolayername(iolayer) == argstring) then
             ioflag = .true.
             doio(iolayer) = .true.
          end if
       end do

       do ioparami = 1, numioparam
          if (ioparam(ioparami) == argstring) then
             ioparamflag = .true.
             iargstep = 2
             call get_command_argument(iarg + 1, ioparamval(ioparami))
          end if
       end do
       
       do istriping = 1, numstriping
          if (stripestring(istriping) == argstring) then
             stripeflag = .true.
             dostripe(istriping) = .true.
          end if
       end do
       
       if (.not.ioflag .and. .not.stripeflag .and. .not.ioparamflag) then        
          write(*,*) "Illegal argument: ", argstring
          call MPI_Finalize(ierr)
          stop           
       end if

       iarg = iarg + iargstep
    end do
    
    ! Check defaults
    if (count(doio(:)) == 0) then
       doio(:) = .true.
    end if

    if (count(dostripe(:)) == 0) then
       dostripe(:) = .true.
    end if
        
  end subroutine processarguments

  subroutine split_string(original_string, string_1, string_2, delim, delim_index)

    implicit none

    character(*) :: original_string
    character :: delim
    character(*), intent(inout) :: string_1,string_2
    integer :: delim_index

    original_string = trim(original_string)

    delim_index = scan(original_string, delim)

    string_1 = original_string(1:delim_index-1)
    string_2 = original_string(delim_index+1:)

  end subroutine split_string

  subroutine benchiofinal

    implicit none
    
    integer :: ierr

    call MPI_Comm_free(nodecomm, ierr)
    call MPI_Comm_free(nodebosscomm, ierr)

    call MPI_Finalize(ierr)

  end subroutine benchiofinal

end module

