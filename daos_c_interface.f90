module daos_c_interface

  use iso_c_binding
  use iso_fortran_env
  implicit none

  private
  
  public daos_initialise
  public daos_write
  public daos_read
  public daos_finish


  interface

     subroutine daos_initialise(pool_string, communicator) bind(c, name="daos_initialise_fortran")
       import :: c_char
       import :: c_int
       character(kind=c_char), dimension(*), intent(in) :: pool_string
       integer(kind=c_int), value, intent(in) :: communicator
     end subroutine daos_initialise

     subroutine daos_write(arraysize, arraygsize, arraysubsize, arraystart, data, obj_class, block_size, keep_data, daosconfig, communicator) bind(c, name="daos_write_array_fortran")
       import :: c_char
       import :: c_int
       import :: c_size_t
       import :: c_double
       import :: c_long
       integer(kind=c_long), dimension(*), intent(in) :: arraysize, arraygsize, arraysubsize, arraystart
       real(kind=c_double), dimension(*), intent(in) :: data
       character(kind=c_char), dimension(*), intent(in) :: obj_class
       integer(kind=c_size_t), value, intent(in)  :: block_size
       integer(kind=c_int), value, intent(in) :: keep_data, daosconfig, communicator
     end subroutine daos_write

     subroutine daos_read(arraysize, arraygsize, arraysubsize, arraystart, output_data, obj_class, daosconfig, communicator) bind(c, name="daos_read_array_fortran")
       import :: c_char
       import :: c_int
       import :: c_size_t
       import :: c_double
       import :: c_long
       integer(kind=c_long), dimension(*), intent(in) :: arraysize, arraygsize, arraysubsize, arraystart
       real(kind=c_double), dimension(*), intent(out) :: output_data
       character(kind=c_char), dimension(*), intent(in) :: obj_class
       integer(kind=c_int), value, intent(in) :: daosconfig, communicator
     end subroutine daos_read

     subroutine daos_finish(communicator) bind(c, name="daos_finish_fortran")
       import :: c_int
       integer(kind=c_int), value, intent(in) :: communicator
     end subroutine daos_finish


  end interface

end module daos_c_interface
