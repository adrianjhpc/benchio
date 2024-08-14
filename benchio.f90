program benchio

  use benchutil
  use benchclock
  use mpiio
  use ioserial
  use iohdf5
  use ionetcdf
  use adios
  use daos
  use daos_c_interface

  implicit none

  character*(maxlen) :: filename, suffix

  character*(6) :: operation

  integer :: iolayer, istriping, iomode, ierr
! Set local array size - global sizes l1, l2 and l3 are scaled
! by number of processes in each dimension

  integer :: i1, i2, i3, j1, j2, j3, l1, l2, l3, p1, p2, p3

  double precision, allocatable, dimension(:,:,:) :: iodata

  logical :: reorder = .false.

  logical :: benchmarked

  double precision :: t0, t1, time, initialise_time, iorate, ioratenoinitialise, kibdata, mibdata, gibdata
  
  call benchioinit()
  
  call processarguments()
  
  ! Set 3D processor grid
  dims = 0

  ! Set 3D processor grid
  call MPI_Dims_create(size, ndim, dims, ierr)

  ! Reverse dimensions as MPI assumes C ordering (this is not essential)
  p1 = dims(3)
  p2 = dims(2)
  p3 = dims(1)

  ! Compute global sizes
  l1 = p1*n1
  l2 = p2*n2
  l3 = p3*n3

  call MPI_Type_size(MPI_DOUBLE_PRECISION, dblesize, ierr)

  kibdata = float(dblesize)*float(n1)*float(n2)*float(n3)*float(p1)*float(p2)*float(p3)/float(kib)
  mibdata = float(dblesize)*float(n1)*float(n2)*float(n3)*float(p1)*float(p2)*float(p3)/float(mib)
  gibdata = float(dblesize)*float(n1)*float(n2)*float(n3)*float(p1)*float(p2)*float(p3)/float(gib)

  if (rank == 0) then
     write(*,*)
     write(*,*) 'Simple Parallel IO benchmark'
     write(*,*) '----------------------------'
     write(*,*)
     write(*,*) 'Running on ', size, ' process(es)'
     write(*,*) 'Process grid is (', p1, ', ', p2, ', ', p3, ')'
     write(*,*) 'Array size is   (', n1, ', ', n2, ', ', n3, ')'
     write(*,*) 'Global size is  (', l1, ', ', l2, ', ', l3, ')'
     write(*,*)
   !  write(*,'(a,f12.2,a)') 'Total amount of data = ', kibdata, ' KiB'
     write(*,'(a,f12.2,a)') 'Total amount of data = ', mibdata, ' MiB'
     write(*,'(a,f12.2,a)') 'Total amount of data = ', gibdata, ' GiB'
     write(*,*)
     write(*,*) 'Clock resolution is ', benchtick()*1.0e6, ', usecs'
     write(*,*) 'Repeating read/write operations ',repeats,' times'
     write(*,*) "Performing the following IO operations"
     write(*,*) "------------------------------"

     do iomode = 1, numiomode
        if (domode(iomode)) write(*,*) iomodestring(iomode)
     end do

     write(*,*)
     write(*,*) "Using the following IO methods"
     write(*,*) "------------------------------"

     do iolayer = 1, numiolayer
        if (doio(iolayer)) write(*,*) iolayername(iolayer)
     end do

     write(*,*)
     write(*,*) "Using the following stripings"
     write(*,*) "-----------------------------"
     
     do istriping = 1, numstriping
        if (dostripe(istriping)) write(*,*) stripestring(istriping)
     end do

     write(*,*)

  end if
  
  allocate(iodata(0:n1+1, 0:n2+1, 0:n3+1))

  dims(1) = p1
  dims(2) = p2
  dims(3) = p3

  call MPI_Cart_create(comm, ndim, dims, periods, reorder, cartcomm, ierr)

  ! Set halos to illegal values
  iodata(:,:,:) = -1
  
  ! Set iodata core to have unique values 1, 2, ..., p1*n1*p2*n2*p3*n3
  call MPI_Cart_coords(cartcomm, rank, ndim, coords, ierr)
  
  do i3 = 1, n3
     do i2 = 1, n2
        do i1 = 1, n1

           j1 = coords(1)*n1 + i1
           j2 = coords(2)*n2 + i2
           j3 = coords(3)*n3 + i3

           iodata(i1,i2,i3) = float((j3-1))*l1*l2 + (j2-1)*l1 + j1
        end do
     end do
  end do

  ! Write benchmarks
  do iolayer = 1, numiolayer

     if(doio(iolayer)) then

        if (rank == 0) then
           write(*,*)
           write(*,*) '------'
           write(*,*) iostring(iolayer)
           write(*,*) '------'
           write(*,*)
        end if
        
        do istriping = 1, numstriping

           if(dostripe(istriping)) then
           
              !filename = 'daos:/mnt/dfuse/'//trim(stripestring(istriping))//'/'//trim(iolayername(iolayer))
              if (iolayer == 8) then
                 filename = trim(stripestring(istriping))//'/'//trim(ioparamval(2))
              else
                 filename = trim(ioparamval(1))
                 if (filename == '') then
                    filename = trim(stripestring(istriping))//'/'//trim(iolayername(iolayer))
                 end if
              end if
              suffix = ""
              
              iocomm = cartcomm

              ! Deal with multiple files
              
              if (iolayer == iolayermulti) then
                 iocomm = MPI_COMM_SELF
                 write(suffix,fmt="(i6.6)") rank
              end if
              
              if (iolayer == iolayernode) then
                 iocomm = nodecomm
                 write(suffix,fmt="(i6.6)") nodenum
              end if
              
              suffix = trim(suffix)//".dat"
              if (iolayer /= 8) then
                 filename = trim(filename)//suffix
              end if

              if (rank == 0) then
                 write(*,*) 'I/O to ', filename
              end if
              
              initialise_time = 0.0

              do iomode = 1, numiomode

                 benchmarked = .false.
                            
                 call MPI_Barrier(comm, ierr)
                 t0 = benchtime()
                 
                 select case (iolayer)
                    
                 case(1:3)
                    if(iomode .eq. 1 .and. domode(iomode)) then
                       call serialwrite(filename, iodata, n1, n2, n3, repeats, iocomm)
                       benchmarked = .true.
                    end if
                    
                    if(iomode .eq. 2 .and. domode(iomode)) then
                       call serialread(filename, iodata, n1, n2, n3, repeats, iocomm)
                       benchmarked = .true.
                    end if
                    
                 case(4)
                    if(iomode .eq. 1 .and. domode(iomode)) then
                       call mpiiowrite(filename, iodata, n1, n2, n3, repeats, iocomm)
                       benchmarked = .true.
                    end if
                    
                    if(iomode .eq. 2 .and. domode(iomode)) then
                       call mpiioread(filename, iodata, n1, n2, n3, repeats, iocomm)
                       benchmarked = .true.
                    end if
                    
                 case(5)
                    if(iomode .eq. 1 .and. domode(iomode)) then
                       call hdf5write(filename, iodata, n1, n2, n3, repeats, iocomm)
                       benchmarked = .true.
                    end if
                    
                    if(iomode .eq. 2 .and. domode(iomode)) then
                       call hdf5read(filename, iodata, n1, n2, n3, repeats, iocomm)
                       benchmarked = .true.
                    end if
                    
                 case(6)
                    if(iomode .eq. 1 .and. domode(iomode)) then
                       call netcdfwrite(filename, iodata, n1, n2, n3, repeats, iocomm)
                       benchmarked = .true.
                    end if
                    
                    if(iomode .eq. 2 .and. domode(iomode)) then
                       call netcdfread(filename, iodata, n1, n2, n3, repeats, iocomm)
                       benchmarked = .true.
                    end if
                    
                 case(7)
                    if(iomode .eq. 1 .and. domode(iomode)) then
                       call adioswrite(filename, iodata, n1, n2, n3, repeats, iocomm, initialise_time)
                       benchmarked = .true.
                    end if
                    
                    if(iomode .eq. 2 .and. domode(iomode)) then
                       call adiosread(filename, iodata, n1, n2, n3, repeats, iocomm, initialise_time)
                       benchmarked = .true.
                    end if
                    
                 case(8)
                    if(iomode .eq. 1 .and. domode(iomode)) then
                       call daoswrite(filename, iodata, n1, n2, n3, repeats, iocomm, 0, initialise_time, trim(ioparamval(3)))
                       benchmarked = .true.
                    end if
                    
                    if(iomode .eq. 2 .and. domode(iomode)) then
                       call daosread(filename, iodata, n1, n2, n3, repeats, iocomm, 0, initialise_time, trim(ioparamval(3)))
                       benchmarked = .true.
                    end if
                    
                    
                 case default
                    write(*,*) 'Illegal value of iolayer = ', iolayer
                    stop
                    
                 end select                 

                 call MPI_Barrier(comm, ierr)
                 t1 = benchtime()

                 if(benchmarked) then
                 
                    time = t1 - t0

                    ! Don't rely on MPI_Barrier to give properly synced timings.
                    ! Select the maximum time across all workers. We have seen
                    ! situtations where even with barriers timings are very
                    ! variable so this seems the best approach.
                    if(rank == 0) then
                        call MPI_Reduce(MPI_IN_PLACE, time, 1, MPI_DOUBLE_PRECISION, MPI_MAX, 0, comm, ierr)
                    else                         
                        call MPI_Reduce(time, time, 1, MPI_DOUBLE_PRECISION, MPI_MAX, 0, comm, ierr)
                    end if

                    iorate = (repeats*mibdata)/time
                    ioratenoinitialise = (repeats*mibdata)/(time - initialise_time)
                    if (rank == 0) then
                       if(iomode .eq. 1) then
                          operation = 'write'
                       else
                          operation = 'read'
                       end if
                       write(*,'(a,a,f10.2,a,f12.2,a)') operation,' time = ', time, ', rate = ', iorate, ' MiB/s'
                       write(*,'(a,a,f10.2,a,f12.2,a)') operation,' (no initialise) time = ', time - initialise_time, ', rate = ', ioratenoinitialise, ' MiB/s'
                    end if
                    
                 end if

              end do

              if(.not. keepdataflag) then
                 
                 ! Rank 0 in iocomm deletes
                 if (iolayer == 4 .or. iolayer == 5 .or. iolayer == 6) then
                    if (rank == 0) then
                       !      call execute_command_line("rm -r /mnt/dfuse/"//trim(stripestring(istriping))//'/'//trim(iolayername(iolayer))//'.dat')
                       call execute_command_line('rm -r '//trim(stripestring(istriping))//'/'//trim(iolayername(iolayer))//'.dat')
                    end if
                    call MPI_Barrier(comm, ierr)
                 else if (iolayer == 7) then
                    ! ADIOS makes a directory so the file deletion function will not work
                    ! use the shell instead
                    
                    call MPI_Barrier(comm, ierr)
                    if (rank == 0) then
                       call execute_command_line("rm -r "//filename)
                    end if
                    call MPI_Barrier(comm, ierr)
                    
                 else if (iolayer == 8) then
                    
                    call daos_finish(iocomm)
                    
                    call daos_cleanup(iocomm)
                    
                 else
                    call leaderdelete(filename, iocomm)
                 end if
                 
              end if

           end if

        end do
                   
     end if

  end do

  if (rank == 0) then
     write(*,*)
     write(*,*) '--------'
     write(*,*) 'Finished'
     write(*,*) '--------'
     write(*,*)
  end if

  if(iocomm .ne. MPI_COMM_SELF) then
     call MPI_Comm_free(iocomm, ierr)
  end if

  call benchiofinal()
  
end program benchio
