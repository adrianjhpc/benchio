#include "daos_c.h"

daos_oclass_id_t str_to_oc(char * in) {
 
  daos_oclass_id_t oc = OC_RESERVED;
  int set = 0;

  if (strcmp(in, "OC_S1") == 0){ 
    oc = OC_S1;
    set = 1;
  }
  if (strcmp(in, "OC_S2") == 0) { 
    oc = OC_S2;
    set = 1;
  }
  if (strcmp(in, "OC_SX") == 0) {
    oc = OC_SX;
    set = 1;
  }

  if (set == 0) printf("Did not find a valid object class, the provided string was %s\n", in);
  return oc;

}

void daos_close(daos_handle_t array_handle) {
  
  int ierr;
  ierr = daos_cont_close(array_handle, NULL);
  
  if(ierr != 0) {
    printf("daos_cont_close failed with error code %d", ierr);
  }

  return;

}

void daos_initialise_fortran(char *pool_string, MPI_Fint communicator){

  MPI_Comm c_communicator;

  c_communicator = MPI_Comm_f2c(communicator);

  daos_initialise(pool_string, c_communicator);

  return;
  
}

void daos_initialise(char *pool_string, MPI_Comm communicator){

  int ierr, comm_rank;
  daos_pool_info_t pool_info;
  daos_cont_info_t container_info;
  char *container_name = "benchio";
  daos_prop_t *container_properties;
  char container_string[37];

  if(initialised){
    return;
  }

  MPI_Comm_rank(communicator, &comm_rank);

  ierr = daos_init();

  if(ierr){
    printf("Problem initialising DAOS\n");
    perror("daos_init");
    MPI_Abort(communicator, 0);
    return;
  }  

  if(comm_rank == 0) {
  
    ierr = daos_pool_connect("2475d6df-c6cf-459d-81c5-296ebbca0a6a", NULL, DAOS_PC_RW, &pool_handle, NULL, NULL);  
    if(ierr){
      printf("Problem connecting to the daos pool %s (%d)\n", pool_string, ierr);
      perror("daos_pool_connect");
      MPI_Abort(communicator, 0);
      return;
    }
    
    /*    ierr = uuid_parse("00000000-0000-0000-0000-000000000000", seed);
    if(ierr != 0){
      printf("Error doing the initial seed parse");
      perror("uuid_parse");
      MPI_Abort(communicator, 0);
      return;
    }
    
    uuid_generate_md5(container_uuid, seed, container_name, strlen(container_name));
    if(ierr != 0){
      printf("Error generating the container uuid");
      perror("uuid_generate_md5");
      MPI_Abort(communicator, 0);
      return;
    }
   
    uuid_unparse(container_uuid, container_string);*/

    ierr = daos_cont_open(pool_handle, container_name, DAOS_COO_RW, &container_handle, NULL, NULL);

    if (ierr == -1005) {

      container_properties = daos_prop_alloc(1);
      container_properties->dpp_entries[0].dpe_type = DAOS_PROP_CO_LABEL;
      ierr = daos_prop_set_str(container_properties, DAOS_PROP_CO_LABEL, container_name, strlen(container_name));
      if(ierr != 0){
	printf("Error doing the property set for the container %d",ierr);
	perror("daos_prop_set_str");
	MPI_Abort(communicator, 0);
	return;
      }      

      ierr = daos_cont_create(pool_handle, &container_uuid, container_properties, NULL);
      //ierr = daos_cont_create(pool_handle, &container_uuid, NULL, NULL);
      if(ierr != 0){
	printf("Error doing the container create");
	perror("daos_cont_create");
	MPI_Abort(communicator, 0);
	return;
      }      

      uuid_unparse(container_uuid, container_string);

      ierr = daos_cont_open(pool_handle, container_name, DAOS_COO_RW, &container_handle, NULL, NULL);
      if(ierr != 0){
	printf("Error opening the container %d\n", ierr);
	perror("daos_cont_open");
	MPI_Abort(communicator, 0);
	return;
      }      
    }

  }

  MPI_Barrier(communicator);

  if(comm_rank != 0){

    ierr = daos_pool_connect("2475d6df-c6cf-459d-81c5-296ebbca0a6a", NULL, DAOS_PC_RW, &pool_handle, NULL, NULL);  
    if(ierr){
      printf("Problem connecting to the daos pool %s (%d)\n", pool_string, ierr);
      perror("daos_pool_connect");
      MPI_Abort(communicator, 0);
      return;
    }

    ierr = daos_cont_open(pool_handle, container_name, DAOS_COO_RW, &container_handle, NULL, NULL);
    if(ierr != 0){
      printf("Error opening the container %d",ierr);
      MPI_Abort(communicator, 0);
      return;
    }
  }
   
  initialised = 1;

  return;

}

void communicate_daos_handles(MPI_Comm communicator){

  d_iov_t global_handle;
  int ierr, comm_rank;
  
  MPI_Comm_rank(communicator, &comm_rank);

  global_handle.iov_buf = NULL;
  global_handle.iov_buf_len = 0;
  global_handle.iov_len = 0;
  
  if(comm_rank == 0){
    ierr = daos_pool_local2global(pool_handle, &global_handle);
    if(ierr != 0){
      printf("Error converting local pool handle to global pool handle\n");
      perror("daos_cont_local2global");
      MPI_Abort(communicator, 0);
      return;
    }
  }

  ierr = MPI_Bcast(&global_handle.iov_buf_len, 1, MPI_UINT64_T, 0, communicator);

  // TODO work out why we need to two this twice
  global_handle.iov_len = global_handle.iov_buf_len;
  global_handle.iov_buf = malloc(global_handle.iov_buf_len);

  if(comm_rank == 0){
    ierr = daos_pool_local2global(pool_handle, &global_handle);
    if(ierr != 0){
      printf("Error converting local pool handle to global pool handle\n");
      perror("daos_cont_local2global");
      MPI_Abort(communicator, 0);
      return;
    }
  }

  ierr = MPI_Bcast(global_handle.iov_buf, global_handle.iov_buf_len, MPI_BYTE, 0, communicator);

  if (comm_rank != 0) {
    ierr = daos_pool_global2local(global_handle, &pool_handle);
    if(ierr != 0){
      printf("Error converting global pool handle to local pool handle\n");
      perror("daos_pool_global2local");
      MPI_Abort(communicator, 0);
      return;
    }
  }

  free(global_handle.iov_buf);
  global_handle.iov_buf = NULL;
  global_handle.iov_buf_len = 0;
  global_handle.iov_len = 0;
  
  if(comm_rank == 0){
    ierr = daos_cont_local2global(container_handle, &global_handle);
    if(ierr != 0){
      printf("Error converting local container handle to global container handle\n");
      perror("daos_cont_local2global");
      MPI_Abort(communicator, 0);
      return;
    }
  }

  ierr = MPI_Bcast(&global_handle.iov_buf_len, 1, MPI_UINT64_T, 0, communicator);

  global_handle.iov_len = global_handle.iov_buf_len;
  global_handle.iov_buf = malloc(global_handle.iov_buf_len);

  if(comm_rank == 0){
    printf("Communicate container handle rank 0 %ld\n", container_handle.cookie);
    ierr = daos_cont_local2global(container_handle, &global_handle);
    if(ierr != 0){
      printf("Error converting local container handle to global container handle\n");
      perror("daos_cont_local2global");
      MPI_Abort(communicator, 0);
      return;
    }
  }

  ierr = MPI_Bcast(global_handle.iov_buf, global_handle.iov_buf_len, MPI_BYTE, 0, communicator);

  if (comm_rank != 0) {
    ierr = daos_cont_global2local(pool_handle, global_handle, &container_handle);
    if(ierr != 0){
      printf("Error converting global container handle to local container handle\n");
      perror("daos_cont_global2local");
      MPI_Abort(communicator, 0);
      return;
    }
  }

  printf("Communicate container handle %ld\n", container_handle.cookie);

  return;

}

void daos_write_array_fortran(long int *arraysize, long int *arraygsize, long int *arraysubsize, long int *arraystart, double *data, char *obj_class, size_t block_size, int keep_data, int daosconfig, MPI_Fint communicator){

  MPI_Comm c_communicator;

  c_communicator = MPI_Comm_f2c(communicator);

  if(daosconfig == 0){

    daos_write_separate_arrays(arraysize, arraygsize, arraysubsize, arraystart, data, obj_class, block_size, keep_data, communicator);

  }else if(daosconfig == 1){

  }else if(daosconfig == 2){

  }else{

    printf("Error, unknown variant of DAOS config\n");

  }

  return;
  
}

void daos_write_separate_arrays(long int *arraysize, long int *arraygsize, long int *arraysubsize, long int *arraystart, double *data, char *obj_class, size_t block_size, int keep_data, MPI_Comm communicator){

  //TODO Fragile, assumes some arrays are 3 elements long (i.e. for a 3d problem).
  int ierr;
  int comm_rank;
  char array_name[100];
  daos_obj_id_t array_obj_id;
  uint64_t container_obj_id;
  uuid_t array_uuid;  
  daos_oclass_id_t array_obj_class;
  daos_handle_t array_handle;
  size_t local_block_size, cell_size, total_size;
  daos_array_iod_t iod;
  d_sg_list_t sgl;
  daos_range_t rg;
  d_iov_t iov;
  char container_handle_string[37];
  char array_uuid_string[37];
  
  MPI_Comm_rank(communicator, &comm_rank);

  sprintf(array_name,"%d",comm_rank);
  
  strcat(array_name, "-data");
  
  array_obj_class = str_to_oc(obj_class);  

  array_obj_id.hi = 0;
  array_obj_id.lo = 0;
  
  uuid_generate_md5(array_uuid, seed, array_name, strlen(array_name));

  memcpy(&(array_obj_id.hi), &(array_uuid[0]) + sizeof(uint64_t), sizeof(uint64_t));
  memcpy(&(array_obj_id.lo), &(array_uuid[0]), sizeof(uint64_t));
  
  /*
   * create and open array object
   */ 
  daos_array_generate_oid(container_handle, &array_obj_id, true, array_obj_class, 0, 0);
  //    daos_array_generate_oid(container_handle, &array_obj_id, true, 0, 0, 0);
      
  ierr = daos_array_create(container_handle, array_obj_id, DAOS_TX_NONE, 1, block_size, &array_handle, NULL);
  
  if (ierr == -1004) {
    ierr = daos_array_open(container_handle, array_obj_id, DAOS_TX_NONE, DAOS_OO_RW, &cell_size, &local_block_size, &array_handle, NULL);
    if (ierr != 0) {
      printf("array open failed with %d", ierr);
    }
  } else if (ierr != 0) {
    printf("array create failed with %d", ierr);
  }

  total_size = arraysubsize[0];
  total_size = total_size * arraysubsize[1];
  total_size = total_size * arraysubsize[2];
  total_size = total_size * sizeof(double);

  iod.arr_nr = 1;
  rg.rg_len = total_size;
  rg.rg_idx = 0;
  iod.arr_rgs = &rg;
  
  sgl.sg_nr = 1;
  d_iov_set(&iov, data, total_size);
  sgl.sg_iovs = &iov;
  
  ierr = daos_array_write(array_handle, DAOS_TX_NONE, &iod, &sgl, NULL);
  if(ierr != 0){
    printf("Error writing array\n");
    perror("daos_array_write");
    MPI_Abort(communicator, 0);
  }

  if(keep_data){

    ierr = daos_array_close(array_handle, NULL);
    if(ierr != 0){
      printf("Error closing array\n");
      perror("daos_array_close");
      MPI_Abort(communicator, 0);

    }

  }else{
  
    ierr =  daos_array_destroy(array_handle, DAOS_TX_NONE, NULL);
    if(ierr != 0){
      printf("Error destroying array %d\n", ierr);
      perror("daos_array_destroy");
      MPI_Abort(communicator, 0);
    }
  
  }

  return;
  
}

void daos_write_single_array(long int *arraysize, long int *arraygsize, long int *arraysubsize, long int *arraystart, double *data, char *obj_class, size_t block_size, int keep_data, MPI_Comm communicator){

  //TODO Fragile, assumes some arrays are 3 elements long (i.e. for a 3d problem).
  int ierr;
  int comm_rank;
  char array_name[100];
  daos_obj_id_t array_obj_id;
  uint64_t container_obj_id;
  uuid_t array_uuid;  
  daos_oclass_id_t array_obj_class;
  daos_handle_t array_handle;
  size_t local_block_size, cell_size, total_size;
  daos_array_iod_t iod;
  d_sg_list_t sgl;
  daos_range_t rg;
  d_iov_t iov;
  char container_handle_string[37];
  char array_uuid_string[37];
  
  MPI_Comm_rank(communicator, &comm_rank);
  
  strcat(array_name, "total-data");
  
  array_obj_class = str_to_oc(obj_class);  

  array_obj_id.hi = 0;
  array_obj_id.lo = 0;
  
  uuid_generate_md5(array_uuid, seed, array_name, strlen(array_name));

  memcpy(&(array_obj_id.hi), &(array_uuid[0]) + sizeof(uint64_t), sizeof(uint64_t));
  memcpy(&(array_obj_id.lo), &(array_uuid[0]), sizeof(uint64_t));
  
  /*
   * create and open array object
   */ 
  daos_array_generate_oid(container_handle, &array_obj_id, true, array_obj_class, 0, 0);

  if(comm_rank == 0){
      
    ierr = daos_array_create(container_handle, array_obj_id, DAOS_TX_NONE, 1, block_size, &array_handle, NULL);

    if (ierr == -1004) {
      ierr = daos_array_open(container_handle, array_obj_id, DAOS_TX_NONE, DAOS_OO_RW, &cell_size, &local_block_size, &array_handle, NULL);
      if (ierr != 0) {
	printf("array open failed with %d", ierr);
      }
    } else if (ierr != 0) {
      printf("array create failed with %d", ierr);
    }    
  
  }

  MPI_Barrier(communicator);

  if(comm_rank != 0){

      ierr = daos_array_open(container_handle, array_obj_id, DAOS_TX_NONE, DAOS_OO_RW, &cell_size, &local_block_size, &array_handle, NULL);
      if (ierr != 0) {
	printf("array open failed with %d", ierr);
      }

  }

  total_size = arraysubsize[0];
  total_size = total_size * arraysubsize[1];
  total_size = total_size * arraysubsize[2];
  total_size = total_size * sizeof(double);

  iod.arr_nr = 1;
  rg.rg_len = total_size;
  rg.rg_idx = 0;
  iod.arr_rgs = &rg;
  
  sgl.sg_nr = 1;
  d_iov_set(&iov, data, total_size);
  sgl.sg_iovs = &iov;
  
  ierr = daos_array_write(array_handle, DAOS_TX_NONE, &iod, &sgl, NULL);
  if(ierr != 0){
    printf("Error writing array\n");
    perror("daos_array_write");
    MPI_Abort(communicator, 0);
  }

  if(keep_data){

    ierr = daos_array_close(array_handle, NULL);
    if(ierr != 0){
      printf("Error closing array\n");
      perror("daos_array_close");
      MPI_Abort(communicator, 0);

    }

  }else{
  
    ierr =  daos_array_destroy(array_handle, DAOS_TX_NONE, NULL);
    if(ierr != 0){
      printf("Error destroying array %d\n", ierr);
      perror("daos_array_destroy");
      MPI_Abort(communicator, 0);
    }
  
  }

  return;
  
}


void daos_read_array_fortran(long int *arraysize, long int *arraygsize, long int *arraysubsize, long int *arraystart, double *output_data, char *obj_class, int daosconfig, MPI_Fint communicator){

  MPI_Comm c_communicator;

  c_communicator = MPI_Comm_f2c(communicator);

  if(daosconfig == 0){

    daos_read_separate_arrays(arraysize, arraygsize, arraysubsize, arraystart, output_data, obj_class, communicator);

  }else if(daosconfig == 1){

  }else if(daosconfig == 2){

  }else{

    printf("Error, unknown variant of DAOS config\n");

  }

  return;

}


void daos_read_separate_arrays(long int *arraysize, long int *arraygsize, long int *arraysubsize, long int *arraystart, double *output_data, char *obj_class, MPI_Comm communicator){

  //TODO Fragile, assumes some arrays are 3 elements long (i.e. for a 3d problem).
  int ierr;
  int comm_rank;
  char array_name[100];
  daos_obj_id_t array_obj_id;
  uuid_t array_uuid;  
  daos_oclass_id_t array_obj_class;
  daos_handle_t array_handle;
  size_t local_block_size, cell_size, total_size;
  daos_array_iod_t iod;
  daos_size_t array_size;
  d_sg_list_t sgl;
  daos_range_t rg;
  d_iov_t iov;

  MPI_Comm_rank(communicator, &comm_rank);

  sprintf(array_name,"%d",comm_rank);
  
  strcat(array_name, "-data"); 
  
  array_obj_class = str_to_oc(obj_class);  
  
  array_obj_id.hi = 0;
  array_obj_id.lo = 0;
  
  uuid_generate_md5(array_uuid, seed, array_name, strlen(array_name));

  memcpy(&(array_obj_id.hi), &(array_uuid[0]) + sizeof(uint64_t), sizeof(uint64_t));
  memcpy(&(array_obj_id.lo), &(array_uuid[0]), sizeof(uint64_t));
    
  /*
   * open array object
   */ 
  daos_array_generate_oid(container_handle, &array_obj_id, true, array_obj_class, 0, 0);
  
  ierr = daos_array_open(container_handle, array_obj_id, DAOS_TX_NONE, DAOS_OO_RW, &cell_size, &local_block_size, &array_handle, NULL);
  if (ierr != 0) {
    printf("array open failed with %d", ierr);
  }

  total_size = arraysubsize[0];
  total_size = total_size * arraysubsize[1];
  total_size = total_size * arraysubsize[2];
  total_size = total_size * sizeof(double);
 
  ierr = daos_array_get_size(array_handle, DAOS_TX_NONE, &array_size, NULL);

  if(array_size != total_size){
    printf("DAOS array sizes not the same as the calculated size %ld %ld\n", array_size, total_size);
  }

  iod.arr_nr = 1;
  rg.rg_len = total_size;
  rg.rg_idx = 0;
  iod.arr_rgs = &rg;
  
  sgl.sg_nr = 1;
  d_iov_set(&iov, output_data, total_size);
  sgl.sg_iovs = &iov;
  
  ierr = daos_array_read(array_handle, DAOS_TX_NONE, &iod, &sgl, NULL);
  if(ierr != 0){
    printf("Error reading array\n");
    perror("daos_array_read");
    MPI_Abort(communicator, 0);
  }

  ierr = daos_array_destroy(array_handle, DAOS_TX_NONE, NULL);
  if(ierr != 0){
    printf("Error destroying array\n");
    perror("daos_array_destroy");
    MPI_Abort(communicator, 0);
  }
 
  return;
  
}

void daos_read_single_array(long int *arraysize, long int *arraygsize, long int *arraysubsize, long int *arraystart, double *output_data, char *obj_class, MPI_Comm communicator){

  //TODO Fragile, assumes some arrays are 3 elements long (i.e. for a 3d problem).
  int ierr;
  int comm_rank;
  char array_name[100];
  daos_obj_id_t array_obj_id;
  uuid_t array_uuid;  
  daos_oclass_id_t array_obj_class;
  daos_handle_t array_handle;
  size_t local_block_size, cell_size, total_size;
  daos_array_iod_t iod;
  daos_size_t array_size;
  d_sg_list_t sgl;
  daos_range_t rg;
  d_iov_t iov;

  MPI_Comm_rank(communicator, &comm_rank);

  sprintf(array_name,"%d",comm_rank);
  
  strcat(array_name, "-data"); 
  
  array_obj_class = str_to_oc(obj_class);  
  
  array_obj_id.hi = 0;
  array_obj_id.lo = 0;
  
  uuid_generate_md5(array_uuid, seed, array_name, strlen(array_name));

  memcpy(&(array_obj_id.hi), &(array_uuid[0]) + sizeof(uint64_t), sizeof(uint64_t));
  memcpy(&(array_obj_id.lo), &(array_uuid[0]), sizeof(uint64_t));
    
  /*
   * open array object
   */ 
  daos_array_generate_oid(container_handle, &array_obj_id, true, array_obj_class, 0, 0);
  
  ierr = daos_array_open(container_handle, array_obj_id, DAOS_TX_NONE, DAOS_OO_RW, &cell_size, &local_block_size, &array_handle, NULL);
  if (ierr != 0) {
    printf("array open failed with %d", ierr);
  }

  total_size = arraysubsize[0];
  total_size = total_size * arraysubsize[1];
  total_size = total_size * arraysubsize[2];
  total_size = total_size * sizeof(double);
 
  ierr = daos_array_get_size(array_handle, DAOS_TX_NONE, &array_size, NULL);

  if(array_size != total_size){
    printf("DAOS array sizes not the same as the calculated size %ld %ld\n", array_size, total_size);
  }

  iod.arr_nr = 1;
  rg.rg_len = total_size;
  rg.rg_idx = 0;
  iod.arr_rgs = &rg;
  
  sgl.sg_nr = 1;
  d_iov_set(&iov, output_data, total_size);
  sgl.sg_iovs = &iov;
  
  ierr = daos_array_read(array_handle, DAOS_TX_NONE, &iod, &sgl, NULL);
  if(ierr != 0){
    printf("Error reading array\n");
    perror("daos_array_read");
    MPI_Abort(communicator, 0);
  }

  ierr = daos_array_destroy(array_handle, DAOS_TX_NONE, NULL);
  if(ierr != 0){
    printf("Error destroying array\n");
    perror("daos_array_destroy");
    MPI_Abort(communicator, 0);
  }
 
  return;
  
}



void daos_close_container(MPI_Comm communicator){

  int ierr;

  ierr = daos_cont_close(container_handle, NULL);
  if(ierr != 0){
    printf("Error closing container %d\n", ierr);
    perror("daos_cont_close");
    MPI_Abort(communicator, 0);
  }

  return;

}

void daos_destroy_container(MPI_Comm communicator){

  int ierr;

  daos_cont_destroy(pool_handle, container_uuid, 1, NULL);
  if(ierr != 0){
    printf("Error destroying container\n");
    perror("daos_destroy_container");
    MPI_Abort(communicator, 0);
  }

}

void daos_close_pool(MPI_Comm communicator){

  int ierr;

  ierr = daos_pool_disconnect(pool_handle, NULL);
  if(ierr != 0){
    printf("Error closing pool\n");
    perror("daos_pool_close");
    MPI_Abort(communicator, 0);
  }

  return;

}

void daos_finalise(MPI_Comm communicator){

  int ierr;

  ierr = daos_fini();
  if(ierr != 0){
    printf("Error finalising DAOS\n");
    perror("daos_fini");
    MPI_Abort(communicator, 0);
  }

  initialised = 0;

  return;

}

void daos_finish_fortran(MPI_Fint communicator){

  MPI_Comm c_communicator;

  c_communicator = MPI_Comm_f2c(communicator);

  daos_finish(c_communicator);

  return;

}

void daos_finish(MPI_Comm communicator){

  daos_close_container(communicator);

  daos_destroy_container(communicator);

  daos_close_pool(communicator);

  daos_finalise(communicator);

  return;

}
