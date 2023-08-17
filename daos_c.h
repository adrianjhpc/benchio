#include <uuid.h>
#include <daos.h>
#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

uuid_t seed;
int initialised = 0;
daos_handle_t pool_handle;
daos_handle_t container_handle;
uuid_t container_uuid;
char *container_name = "benchio";

#define ERROR -1
#define SUCCESS 0
#define BLKSIZE 1048576

void communicate_daos_handles(MPI_Comm communicator);
daos_oclass_id_t str_to_oc(char * in);
void daos_close(daos_handle_t handle);
void daos_initialise_fortran(char *pool_string, MPI_Fint communicator);
void daos_initialise(char *pool_string, MPI_Comm communicator);
void daos_write_array_fortran(int num_dims, long int *arraysize, long int *arraygsize, long int *arraysubsize, long int *arraystart, double *data, char *obj_class, size_t block_size, int keep_data, int daosconfig, MPI_Fint communicator);
void daos_write_separate_arrays(int num_dims, long int *arraysize, long int *arraygsize, long int *arraysubsize, long int *arraystart, double *data, char *obj_class, size_t block_size, int keep_data, MPI_Comm communicator);
void daos_write_single_array(int num_dims, long int *arraysize, long int *arraygsize, long int *arraysubsize, long int *arraystart, double *data, char *obj_class, size_t block_size, int keep_data, MPI_Comm communicator);
void daos_write_object_fortran(int num_dims, long int *objectsize, long int *objectgsize, long int *objectsubsize, long int *objectstart, double *data, char *obj_class, size_t block_size, int keep_data, int daosconfig, MPI_Fint communicator);
void daos_write_separate_objects(int num_dims, long int *objectsize, long int *objectgsize, long int *objectsubsize, long int *objectstart, double *data, char *obj_class, size_t block_size, int keep_data, MPI_Comm communicator);
void daos_write_single_object(int num_dims, long int *objectsize, long int *objectgsize, long int *objectsubsize, long int *objectstart, double *data, char *obj_class, size_t block_size, int keep_data, MPI_Comm communicator);
void daos_read_array_fortran(int num_dims, long int *arraysize, long int *arraygsize, long int *arraysubsize, long int *arraystart, double *output_data, char *obj_class, int daosconfig, MPI_Fint communicator);
void daos_read_separate_arrays(int num_dims, long int *arraysize, long int *arraygsize, long int *arraysubsize, long int *arraystart, double *output_data, char *obj_class, MPI_Comm communicator);
void daos_read_single_array(int num_dims, long int *arraysize, long int *arraygsize, long int *arraysubsize, long int *arraystart, double *output_data, char *obj_class, MPI_Comm communicator);
void daos_read_object_fortran(int num_dims, long int *objectsize, long int *objectgsize, long int *objectsubsize, long int *objectstart, double *output_data, char *obj_class, int daosconfig, MPI_Fint communicator);
void daos_read_separate_objects(int num_dims, long int *objectsize, long int *objectgsize, long int *objectsubsize, long int *objectstart, double *output_data, char *obj_class, MPI_Comm communicator);
void daos_read_single_object(int num_dims, long int *objectsize, long int *objectgsize, long int *objectsubsize, long int *objectstart, double *output_data, char *obj_class, MPI_Comm communicator);
void daos_close_container(MPI_Comm communicator);
void daos_destroy_container(MPI_Comm communicator);
void daos_close_pool(MPI_Comm communicator);
void daos_finalise(MPI_Comm communicator);
void daos_finish_fortran(MPI_Fint communicator);
void daos_finish(MPI_Comm communicator);
void daos_cleanup_fortran(MPI_Fint communicator);
void daos_cleanup(MPI_Comm communicator);
