module daos_c_interface

  use iso_c_binding
  use iso_fortran_env
  implicit none

  private
  
  public daos_initialise
  public daos_write_array
  public daos_write_object
  public daos_read_array
  public daos_read_object
  public daos_finish
  public daos_cleanup


  interface

     subroutine daos_initialise(pool_string, communicator) bind(c, name="daos_initialise_fortran")
       import :: c_char
       import :: c_int
       character(kind=c_char), dimension(*), intent(in) :: pool_string
       integer(kind=c_int), value, intent(in) :: communicator
     end subroutine daos_initialise

     subroutine daos_write_array(num_dims, arraysize, arraygsize, arraysubsize, arraystart, data, obj_class, block_size, keep_data, daosconfig, communicator) bind(c, name="daos_write_array_fortran")
       import :: c_char
       import :: c_int
       import :: c_size_t
       import :: c_double
       import :: c_long
       integer(kind=c_long), dimension(*), intent(in) :: arraysize, arraygsize, arraysubsize, arraystart
       real(kind=c_double), dimension(*), intent(in) :: data
       character(kind=c_char), dimension(*), intent(in) :: obj_class
       integer(kind=c_size_t), value, intent(in)  :: block_size
       integer(kind=c_int), value, intent(in) :: num_dims, keep_data, daosconfig, communicator
     end subroutine daos_write_array

     subroutine daos_write_object(num_dims, objectsize, objectgsize, objectsubsize, objectstart, data, obj_class, block_size, keep_data, daosconfig, communicator) bind(c, name="daos_write_object_fortran")
       import :: c_char
       import :: c_int
       import :: c_size_t
       import :: c_double
       import :: c_long
       integer(kind=c_long), dimension(*), intent(in) :: objectsize, objectgsize, objectsubsize, objectstart
       real(kind=c_double), dimension(*), intent(in) :: data
       character(kind=c_char), dimension(*), intent(in) :: obj_class
       integer(kind=c_size_t), value, intent(in)  :: block_size
       integer(kind=c_int), value, intent(in) :: num_dims, keep_data, daosconfig, communicator
     end subroutine daos_write_object


     subroutine daos_read_array(num_dims, arraysize, arraygsize, arraysubsize, arraystart, output_data, obj_class, daosconfig, communicator) bind(c, name="daos_read_array_fortran")
       import :: c_char
       import :: c_int
       import :: c_size_t
       import :: c_double
       import :: c_long
       integer(kind=c_long), dimension(*), intent(in) :: arraysize, arraygsize, arraysubsize, arraystart
       real(kind=c_double), dimension(*), intent(out) :: output_data
       character(kind=c_char), dimension(*), intent(in) :: obj_class
       integer(kind=c_int), value, intent(in) :: num_dims, daosconfig, communicator
     end subroutine daos_read_array

     subroutine daos_read_object(num_dims, objectsize, objectgsize, objectsubsize, objectstart, output_data, obj_class, daosconfig, communicator) bind(c, name="daos_read_object_fortran")
       import :: c_char
       import :: c_int
       import :: c_size_t
       import :: c_double
       import :: c_long
       integer(kind=c_long), dimension(*), intent(in) :: objectsize, objectgsize, objectsubsize, objectstart
       real(kind=c_double), dimension(*), intent(out) :: output_data
       character(kind=c_char), dimension(*), intent(in) :: obj_class
       integer(kind=c_int), value, intent(in) :: num_dims, daosconfig, communicator
     end subroutine daos_read_object


     subroutine daos_finish(communicator) bind(c, name="daos_finish_fortran")
       import :: c_int
       integer(kind=c_int), value, intent(in) :: communicator
     end subroutine daos_finish

     subroutine daos_cleanup(communicator) bind(c, name="daos_cleanup_fortran")
       import :: c_int
       integer(kind=c_int), value, intent(in) :: communicator
     end subroutine daos_cleanup

  end interface

end module daos_c_interface
