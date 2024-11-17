#include "daos_c.h"

daos_oclass_id_t str_to_oc(char * in) {
 
  daos_oclass_id_t oc = OC_RESERVED;
  int set = 0;
  if (strcmp(in, "OC_S1") == 0){ 
    oc = OC_S1;
    set = 1;
  }else if (strcmp(in, "OC_S2") == 0) { 
    oc = OC_S2;
    set = 1;
  }else if (strcmp(in, "OC_S4") == 0) {
    oc = OC_S4;
    set = 1;
  }else if (strcmp(in, "OC_S8") == 0) {
    oc = OC_S8;
    set = 1;
  }else if (strcmp(in, "OC_S16") == 0) {
    oc = OC_S16;
    set = 1;
  }else if (strcmp(in, "OC_S32") == 0) {
    oc = OC_S32;
    set = 1;
  }else if (strcmp(in, "OC_SX") == 0) {
    oc = OC_SX;
    //if redundancy factor = 2
    //oc = OC_RP_2GX;
    set = 1;
  }
  if (set == 0) printf("Did not find a valid object class, the provided string was %s\n", in);
  return oc;

}

void daos_close(daos_handle_t handle) {
  
  int ierr;
  ierr = daos_cont_close(handle, NULL);
  
  if(ierr != 0) {
    printf("daos_cont_close failed with error code %d", ierr);
  }

  return;

}

void daos_initialise_fortran(char *pool_string, char *cont_string, MPI_Fint communicator){

  MPI_Comm c_communicator;

  c_communicator = MPI_Comm_f2c(communicator);

  daos_initialise(pool_string, cont_string, c_communicator);

  return;
  
}

void daos_initialise(char *pool_string, char *cont_string, MPI_Comm communicator){

  int ierr, comm_rank;
  size_t str_length;
  daos_pool_info_t pool_info;
  daos_cont_info_t container_info;
  daos_prop_t *container_properties;
  // char container_string[37];
  
  if(initialised){
    return;
  }
  
  str_length = strlen(cont_string);
  if(str_length >= MAX_CONT_NAME_LEN){
    printf("Container name too long\n");
    perror("daos_initialise");
    MPI_Abort(communicator, 0);
    return;
  }
  strncpy(container_name, cont_string, str_length);
  container_name[strlen(cont_string)] = '\0';

  MPI_Comm_rank(communicator, &comm_rank);
  
  ierr = daos_init();
  
  if(ierr){
    printf("Problem initialising DAOS\n");
    perror("daos_init");
    MPI_Abort(communicator, 0);
    return;
  }  

  if(comm_rank == 0){
  
    ierr = daos_pool_connect(pool_string, NULL, DAOS_PC_RW, &pool_handle, NULL, NULL);  
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
	printf("Error doing the property set for the container %d\n",ierr);
	perror("daos_prop_set_str");
	MPI_Abort(communicator, 0);
	return;
      }      
      
      ierr = daos_cont_create(pool_handle, NULL, container_properties, NULL);
      //ierr = daos_cont_create(pool_handle, &container_uuid, NULL, NULL);
      if(ierr != 0 && ierr != -1004){
	printf("Error doing the container create %d\n", ierr);
	perror("daos_cont_create");
	MPI_Abort(communicator, 0);
	return;
      }      
      
      ierr = daos_cont_open(pool_handle, container_name, DAOS_COO_RW, &container_handle, NULL, NULL);
      if(ierr != 0){
	printf("Error opening the container %d\n", ierr);
	perror("daos_cont_open");
	MPI_Abort(communicator, 0);
	return;
      }      
      
    }else if(ierr != 0){
      printf("Error opening the container %d\n", ierr);
      perror("daos_cont_open");
      MPI_Abort(communicator, 0);
      return;
    }
  

  }

  communicate_daos_handles(communicator);

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
    global_handle.iov_len = global_handle.iov_buf_len;
    global_handle.iov_buf = malloc(global_handle.iov_buf_len);
    ierr = daos_pool_local2global(pool_handle, &global_handle);

    if(ierr != 0){
      printf("Error converting local pool handle to global pool handle\n");
      perror("daos_cont_local2global");
      MPI_Abort(communicator, 0);
      return;
    }
  }
  
  ierr = MPI_Bcast(&global_handle.iov_buf_len, 1, MPI_UINT64_T, 0, communicator);
  
  if(comm_rank != 0){
    global_handle.iov_len = global_handle.iov_buf_len;
    global_handle.iov_buf = malloc(global_handle.iov_buf_len);
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
    global_handle.iov_len = global_handle.iov_buf_len;
    global_handle.iov_buf = malloc(global_handle.iov_buf_len);
    ierr = daos_cont_local2global(container_handle, &global_handle);
    if(ierr != 0){
      printf("Error converting local container handle to global container handle\n");
      perror("daos_cont_local2global");
      MPI_Abort(communicator, 0);
      return;
    }
  }

  ierr = MPI_Bcast(&global_handle.iov_buf_len, 1, MPI_UINT64_T, 0, communicator);
 
  if(comm_rank != 0){
    global_handle.iov_len = global_handle.iov_buf_len;
    global_handle.iov_buf = malloc(global_handle.iov_buf_len);
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

  return;

}

void daos_write_array_fortran(int num_dims, long int *arraysize, long int *arraygsize, long int *arraysubsize, long int *arraystart, double *data, char *obj_class, size_t block_size, int repeats, int keep_data, int daosconfig, MPI_Fint communicator){

  MPI_Comm c_communicator;

  c_communicator = MPI_Comm_f2c(communicator);

  if(daosconfig == 0){

    daos_write_separate_arrays(num_dims, arraysize, arraygsize, arraysubsize, arraystart, data, obj_class, block_size, repeats, keep_data, communicator);

  }else if(daosconfig == 1){

    daos_write_single_array(num_dims, arraysize, arraygsize, arraysubsize, arraystart, data, obj_class, block_size, repeats, keep_data, communicator);

  }else if(daosconfig == 2){

  }else{

    printf("Error, unknown variant of DAOS config\n");

  }

  return;
  
}

void daos_write_separate_arrays(int num_dims, long int *arraysize, long int *arraygsize, long int *arraysubsize, long int *arraystart, double *data, char *obj_class, size_t block_size, int repeats, int keep_data, MPI_Comm communicator){

  int ierr, i;
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
  daos_array_generate_oid(container_handle, &array_obj_id, DAOS_OT_ARRAY_BYTE, array_obj_class, 0, 0);
  //    daos_array_generate_oid(container_handle, &array_obj_id, true, 0, 0, 0);
      
  ierr = daos_array_create(container_handle, array_obj_id, DAOS_TX_NONE, 1, block_size, &array_handle, NULL);
  
  if (ierr == -1004) {
    ierr = daos_array_open(container_handle, array_obj_id, DAOS_TX_NONE, DAOS_OO_RW, &cell_size, &local_block_size, &array_handle, NULL);
    if (ierr != 0) {
      printf("array open failed with %d", ierr);
      perror("daos_array_open");
      MPI_Abort(communicator, 0);
    }
  } else if (ierr != 0) {
    printf("array create failed with %d", ierr);
    perror("daos_array_create");
    MPI_Abort(communicator, 0);
  }

  total_size = sizeof(double);
  for(i=0; i<num_dims; i++){
    total_size = total_size * arraysubsize[i];
  }

  iod.arr_nr = 1;
  rg.rg_len = total_size;
  rg.rg_idx = 0;
  iod.arr_rgs = &rg;
  
  sgl.sg_nr = 1;
  d_iov_set(&iov, &data[0], total_size);
  sgl.sg_iovs = &iov;

  // At the moment this just re-writes the same place in the array
  for(int repeat = 0; repeat < repeats; repeat++){
    ierr = daos_array_write(array_handle, DAOS_TX_NONE, &iod, &sgl, NULL);
    if(ierr != 0){
      printf("Error writing array %d\n", ierr);
      perror("daos_array_write");
      MPI_Abort(communicator, 0);
    }
  }

  if(keep_data){

    ierr = daos_array_close(array_handle, NULL);
    if(ierr != 0){
      printf("Error closing array\n");
      perror("daos_array_close");
      MPI_Abort(communicator, 0);
    }

  }else{

    ierr = daos_array_close(array_handle, NULL);  
    //    ierr =  daos_array_destroy(array_handle, DAOS_TX_NONE, NULL);
    if(ierr != 0){
      printf("Error destroying array %d\n", ierr);
      perror("daos_array_destroy");
      MPI_Abort(communicator, 0);
    }
  
  }

  return;
  
}

void daos_write_single_array(int num_dims, long int *arraysize, long int *arraygsize, long int *arraysubsize, long int *arraystart, double *data, char *obj_class, size_t block_size, int repeats, int keep_data, MPI_Comm communicator){

  //TODO Fragile, assumes some arrays are 3 elements long (i.e. for a 3d problem).
  int ierr, i, j, k, total_parts;
  int comm_rank;
  char array_name[100];
  daos_obj_id_t array_obj_id;
  uint64_t container_obj_id;
  uuid_t array_uuid;  
  daos_oclass_id_t array_obj_class;
  daos_handle_t array_handle;
  size_t  total_size, initial_offset, running_offset;
  daos_size_t local_block_size, local_cell_size;

  daos_array_iod_t iod;
  d_sg_list_t sgl;
  daos_range_t *rg;
  d_iov_t *iov;

  MPI_Comm_rank(communicator, &comm_rank);
  
  local_block_size = block_size;
  local_cell_size = sizeof(double);

  strcpy(array_name, "total-data");

  array_obj_class = str_to_oc(obj_class);  

  array_obj_id.hi = 0;
  array_obj_id.lo = 0;
  
  uuid_generate_md5(array_uuid, seed, array_name, strlen(array_name));

  memcpy(&(array_obj_id.hi), &(array_uuid[0]) + sizeof(uint64_t), sizeof(uint64_t));
  memcpy(&(array_obj_id.lo), &(array_uuid[0]), sizeof(uint64_t));
  
  /*
   * create and open array object
   */ 
  daos_obj_generate_oid(container_handle, &array_obj_id, DAOS_OT_ARRAY_BYTE, array_obj_class, 0, 0);

  ierr = daos_array_open_with_attr(container_handle, array_obj_id, DAOS_TX_NONE, DAOS_OO_RW, local_cell_size, local_block_size, &array_handle, NULL);

  //  if(ierr != 0){
  //    ierr = daos_array_create(container_handle, array_obj_id, DAOS_TX_NONE, 1, block_size, &array_handle, NULL);
  //  //  ierr = daos_array_create(container_handle, array_obj_id, DAOS_TX_NONE, sizeof(double), block_size, &array_handle, NULL);
  //    ierr = daos_array_open(container_handle, array_obj_id, DAOS_TX_NONE, DAOS_OO_RW, &cell_size, &local_block_size, &array_handle, NULL);

    //  if (ierr == -1004) {

    if (ierr != 0) {
      printf("%d array open failed with %d\n", comm_rank, ierr);
    }
    //  } else if (ierr != 0) {
    //    printf("%d array create failed with %d\n", comm_rank, ierr);
    //  }    
  
  // iod variables (ranges) represent the position in the DAOS array (i.e. the DAOS array where the data will be stored)
  // sgl variables (scatter/gather) represent the position in the source array (i.e. in memory array the data is coming from)

  // Total number of items to be written is the combined size of the first two dimensions
  total_parts =  arraysubsize[0]*arraysubsize[1];
  iod.arr_nr = total_parts;

  // Allocate an array of ranges to be populated
  rg = (daos_range_t *)malloc(sizeof(daos_range_t)*total_parts);
  // Setup the iod to link to that array of ranges
  iod.arr_rgs = rg;

  initial_offset = arraystart[0]*(arraygsize[1]*arraygsize[2]);
  initial_offset = initial_offset + (arraystart[1]*arraygsize[2]);
  initial_offset = initial_offset + arraystart[2];
  //initial_offset = initial_offset * sizeof(double);
   initial_offset = initial_offset;// * sizeof(double);

  running_offset = initial_offset;

  // Populate the array of ranges
  for(i=0; i<arraysubsize[0]; i++){
    for(j=0; j<arraysubsize[1]; j++){
      // rg_len is the amount of data to be written into the DAOS array for this operation
      //iod.arr_rgs[i*arraysubsize[1]+j].rg_len = arraysubsize[2]*sizeof(double);
       iod.arr_rgs[i*arraysubsize[1]+j].rg_len = arraysubsize[2];//*sizeof(double);
      // idx is the offset in the array where the data should be written
      // In this case it's the offset in the global array of this local portion
      iod.arr_rgs[i*arraysubsize[1]+j].rg_idx = running_offset;
      //running_offset = running_offset + (arraygsize[2]*sizeof(double));
      running_offset = running_offset + (arraygsize[2]);//*sizeof(double));
    }
    //running_offset = initial_offset + ((i+1)*(arraygsize[1]*arraygsize[2]*sizeof(double)));
    running_offset = initial_offset + ((i+1)*(arraygsize[1]*arraygsize[2]));//*sizeof(double)));
  }
    
  sgl.sg_nr = total_parts;
  // Allocate an array of scatter/gathers to be populated
  iov = (d_iov_t *)malloc(sizeof(d_iov_t)*total_parts);
  sgl.sg_iovs = iov;

  initial_offset = 0;

  for(i=0; i<arraysubsize[0]; i++){
    for(j=0; j<arraysubsize[1]; j++){
      d_iov_set(&sgl.sg_iovs[i*arraysubsize[1]+j], &data[initial_offset], arraysubsize[2]*sizeof(double));
      initial_offset = initial_offset + arraysubsize[2];
    }
  }

  // At the moment this just re-writes the same place in the array
  for(int repeat = 0; repeat < repeats; repeat++){
    ierr = daos_array_write(array_handle, DAOS_TX_NONE, &iod, &sgl, NULL);
    if(ierr != 0){
      printf("Error writing array %d\n", ierr);
      perror("daos_array_write");
      MPI_Abort(communicator, 0);
    }
  }

  if(keep_data){

    ierr = daos_array_close(array_handle, NULL);
    if(ierr != 0){
      printf("Error closing array\n");
      perror("daos_array_close");
      MPI_Abort(communicator, 0);

    }

  }else{
  
    //ierr =  daos_array_destroy(array_handle, DAOS_TX_NONE, NULL);
    ierr = daos_array_close(array_handle, NULL);
    if(ierr != 0){
      printf("Error destroying array %d\n", ierr);
      perror("daos_array_destroy");
      MPI_Abort(communicator, 0);
    }
  
  }

  free(rg);
  free(iov);

  return;
  
}



void daos_write_object_fortran(int num_dims, long int *objectsize, long int *objectgsize, long int *objectsubsize, long int *objectstart, double *data, char *obj_class, size_t block_size, int repeats, int keep_data, int daosconfig, MPI_Fint communicator){

  MPI_Comm c_communicator;

  c_communicator = MPI_Comm_f2c(communicator);

  if(daosconfig == 3){

    daos_write_separate_objects(num_dims, objectsize, objectgsize, objectsubsize, objectstart, data, obj_class, block_size, repeats, keep_data, communicator);

  }else if(daosconfig == 4){

    daos_write_single_object(num_dims, objectsize, objectgsize, objectsubsize, objectstart, data, obj_class, block_size, repeats, keep_data, communicator);

  }else if(daosconfig == 5){

  }else{

    printf("Error, unknown variant of DAOS config\n");

  }

  return;
  
}

void daos_write_separate_objects(int num_dims, long int *objectsize, long int *objectgsize, long int *objectsubsize, long int *objectstart, double *data, char *obj_class, size_t block_size, int repeats, int keep_data, MPI_Comm communicator){

  int ierr, i;
  int comm_rank;
  char object_name[100];
  daos_obj_id_t object_obj_id;
  uint64_t container_obj_id;
  uuid_t object_uuid;  
  daos_oclass_id_t object_obj_class;
  daos_handle_t obj_handle;
  size_t local_block_size, cell_size, total_size;
  daos_iod_t iod;
  d_sg_list_t sgl;
  daos_range_t rg;
  d_iov_t iov;
  daos_key_t dkey;
  /*  
  MPI_Comm_rank(communicator, &comm_rank);

  sprintf(object_name,"%d",comm_rank);
  
  strcat(object_name, "-data");

  d_iov_set(&dkey, object_name, strlen(object_name));
  
  object_obj_class = str_to_oc(obj_class);  

  object_obj_id.hi = 0;
  object_obj_id.lo = 0;
  
  uuid_generate_md5(object_uuid, seed, object_name, strlen(object_name));

  memcpy(&(object_obj_id.hi), &(object_uuid[0]) + sizeof(uint64_t), sizeof(uint64_t));
  memcpy(&(object_obj_id.lo), &(object_uuid[0]), sizeof(uint64_t));
  
  // create and open object
  daos_obj_generate_oid(container_handle, &object_obj_id, DAOS_OT_ARRAY_BYTE, object_obj_class, 0, 0);
  //    daos_obj_generate_oid(container_handle, &object_obj_id, true, 0, 0, 0);
      
  
  ierr = daos_obj_open(container_handle, object_obj_id, DAOS_TX_NONE, DAOS_OO_RW, &cell_size, &local_block_size, &obj_handle, NULL);
  if (ierr != 0) {
    printf("object open failed with %d", ierr);
  }

  total_size = sizeof(double);
  for(i=0; i<num_dims; i++){
    total_size = total_size * objectsubsize[i];
  }

  iod.iod_nr = 1;
  rg.rg_len = total_size;
  rg.rg_idx = 0;
  iod.arr_rgs = &rg;
  
  sgl.sg_nr = 1;
  d_iov_set(&iov, &data[0], total_size*sizeof(double));
  sgl.sg_iovs = &iov;
  

  ierr = daos_obj_update(obj_handle, DAOS_TX_NONE, DAOS_COND_DKEY_UPDATE, dkey, total_parts, &iod, &sgl, NULL);

  if(ierr != 0){
    printf("Error writing object %d\n", ierr);
    perror("daos_object_write");
    MPI_Abort(communicator, 0);
  }

  if(keep_data){

    ierr = daos_obj_close(obj_handle, NULL);
    if(ierr != 0){
      printf("Error closing object\n");
      perror("daos_object_close");
      MPI_Abort(communicator, 0);
    }

  }else{
  
    ierr =  daos_obj_destroy(obj_handle, DAOS_TX_NONE, NULL);
    if(ierr != 0){
      printf("Error destroying object %d\n", ierr);
      perror("daos_object_destroy");
      MPI_Abort(communicator, 0);
    }
  
  }
*/
  return;
  
}

void daos_write_single_object(int num_dims, long int *objectsize, long int *objectgsize, long int *objectsubsize, long int *objectstart, double *data, char *obj_class, size_t block_size, int repeats, int keep_data, MPI_Comm communicator){

  //TODO Fragile, assumes some objects are 3 elements long (i.e. for a 3d problem).
  int ierr, i, j, k, total_parts;
  int comm_rank;
  char object_name[100];
  daos_obj_id_t object_obj_id;
  uint64_t container_obj_id;
  uuid_t object_uuid;  
  daos_oclass_id_t object_obj_class;
  daos_handle_t obj_handle;
  size_t local_block_size, cell_size, total_size, initial_offset, running_offset;
  daos_iod_t iod;
  d_sg_list_t sgl;
  daos_range_t *rg;
  d_iov_t *iov;
  daos_key_t dkey;
  double start, end, a, b, c,d;

  /*
  
  start = MPI_Wtime();

  MPI_Comm_rank(communicator, &comm_rank);
  
  strcpy(object_name, "total-data");

  d_iov_set(&dkey, object_name, strlen(object_name));

  object_obj_class = str_to_oc(obj_class);  

  object_obj_id.hi = 0;
  object_obj_id.lo = 0;
  
  uuid_generate_md5(object_uuid, seed, object_name, strlen(object_name));

  memcpy(&(object_obj_id.hi), &(object_uuid[0]) + sizeof(uint64_t), sizeof(uint64_t));
  memcpy(&(object_obj_id.lo), &(object_uuid[0]), sizeof(uint64_t));
  
  // create and open object   
  daos_obj_generate_oid(container_handle, &object_obj_id, DAOS_OT_ARRAY_BYTE, object_obj_class, 0, 0);

  ierr = daos_obj_open(container_handle, object_obj_id, DAOS_OO_RW, &obj_handle, NULL);

  if(ierr != 0){
    
    printf("%d object open failed with %d\n", comm_rank, ierr);

  }    
  
  end = MPI_Wtime();
  a = end - start;

  start = MPI_Wtime();

  // iod variables (ranges) represent the position in the DAOS object (i.e. the DAOS object where the data will be stored)
  // sgl variables (scatter/gather) represent the position in the source object (i.e. in memory object the data is coming from)

  // Total number of items to be written is the combined size of the first two dimensions
  total_parts =  objectsubsize[0]*objectsubsize[1];
  iod.iod_nr = total_parts;

  // Allocate an object of ranges to be populated
  rg = (daos_range_t *)malloc(sizeof(daos_range_t)*total_parts);
  // Setup the iod to link to that object of ranges
  iod.arr_rgs = rg;

  initial_offset = objectstart[0]*(objectgsize[1]*objectgsize[2]);
  initial_offset = initial_offset + (objectstart[1]*objectgsize[2]);
  initial_offset = initial_offset + objectstart[2];
  initial_offset = initial_offset * sizeof(double);
  //  initial_offset = initial_offset;// * sizeof(double);

  running_offset = initial_offset;

  // Populate the object of ranges
  for(i=0; i<objectsubsize[0]; i++){
    for(j=0; j<objectsubsize[1]; j++){
      // rg_len is the amount of data to be written into the DAOS object for this operation
      iod.arr_rgs[i*objectsubsize[1]+j].rg_len = objectsubsize[2]*sizeof(double);
      // iod.arr_rgs[i*objectsubsize[1]+j].rg_len = objectsubsize[2];//*sizeof(double);
      // idx is the offset in the object where the data should be written
      // In this case it's the offset in the global object of this local portion
      iod.arr_rgs[i*objectsubsize[1]+j].rg_idx = running_offset;
      running_offset = running_offset + (objectgsize[2]*sizeof(double));
      //      running_offset = running_offset + (objectgsize[2]);//*sizeof(double));
    }
    running_offset = initial_offset + ((i+1)*(objectgsize[1]*objectgsize[2]*sizeof(double)));
    //    running_offset = initial_offset + ((i+1)*(objectgsize[1]*objectgsize[2]));//*sizeof(double)));
  }
    
  sgl.sg_nr = total_parts;
  // Allocate an object of scatter/gathers to be populated
  iov = (d_iov_t *)malloc(sizeof(d_iov_t)*total_parts);
  sgl.sg_iovs = iov;

  initial_offset = 0;

  for(i=0; i<objectsubsize[0]; i++){
    for(j=0; j<objectsubsize[1]; j++){
      d_iov_set(&sgl.sg_iovs[i*objectsubsize[1]+j], &data[initial_offset], objectsubsize[2]*sizeof(double));
      initial_offset = initial_offset + objectsubsize[2];
    }
  }

  end = MPI_Wtime();
  b = end - start;

  start = MPI_Wtime();

  ierr = daos_obj_update(obj_handle, DAOS_TX_NONE, DAOS_COND_DKEY_UPDATE, dkey, total_parts, &iod, &sgl, NULL);

  if(ierr != 0){
    printf("Error writing object %d\n", ierr);
    perror("daos_object_write");
    MPI_Abort(communicator, 0);
  }

  end = MPI_Wtime();
  c = end - start;

  start = MPI_Wtime();

  if(keep_data){

    ierr = daos_obj_close(obj_handle, NULL);
    if(ierr != 0){
      printf("Error closing object\n");
      perror("daos_object_close");
      MPI_Abort(communicator, 0);

    }

  }else{
  
    //    ierr =  daos_object_destroy(obj_handle, DAOS_TX_NONE, NULL);
    if(ierr != 0){
      printf("Error destroying object %d\n", ierr);
      perror("daos_object_destroy");
      MPI_Abort(communicator, 0);
    }
  
  }

  free(rg);
  free(iov);

  end = MPI_Wtime();
  d = end - start;

  printf("%d %lf %lf %lf %lf\n", comm_rank, a, b, c, d);
  */
  return;
  
}



void daos_read_array_fortran(int num_dims, long int *arraysize, long int *arraygsize, long int *arraysubsize, long int *arraystart, double *output_data, char *obj_class, int repeats, int daosconfig, MPI_Fint communicator){

  MPI_Comm c_communicator;

  c_communicator = MPI_Comm_f2c(communicator);

  if(daosconfig == 0){

    daos_read_separate_arrays(num_dims, arraysize, arraygsize, arraysubsize, arraystart, output_data, obj_class, repeats, communicator);

  }else if(daosconfig == 1){

    daos_read_single_array(num_dims, arraysize, arraygsize, arraysubsize, arraystart, output_data, obj_class, repeats, communicator);

  }else if(daosconfig == 2){

  }else{

    printf("Error, unknown variant of DAOS config\n");

  }

  return;

}


void daos_read_separate_arrays(int num_dims, long int *arraysize, long int *arraygsize, long int *arraysubsize, long int *arraystart, double *output_data, char *obj_class, int repeats, MPI_Comm communicator){

  //TODO Fragile, assumes some arrays are 3 elements long (i.e. for a 3d problem).
  int ierr, i;
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
  daos_array_generate_oid(container_handle, &array_obj_id, DAOS_OT_ARRAY_BYTE, array_obj_class, 0, 0);
  
  ierr = daos_array_open(container_handle, array_obj_id, DAOS_TX_NONE, DAOS_OO_RW, &cell_size, &local_block_size, &array_handle, NULL);
  if (ierr != 0) {
    printf("array open failed with %d\n", ierr);
    MPI_Abort(communicator, 0);
  }

  total_size = sizeof(double);
  for(i=0; i<num_dims; i++){
    total_size = total_size * arraysubsize[i];
  }
 
  //ierr = daos_array_get_size(array_handle, DAOS_TX_NONE, &array_size, NULL);

  if(array_size != total_size){
    printf("DAOS array sizes not the same as the calculated size %ld %ld\n", array_size, total_size);
    MPI_Abort(communicator, 0);
  }

  iod.arr_nr = 1;
  rg.rg_len = total_size;
  rg.rg_idx = 0;
  iod.arr_rgs = &rg;
  
  sgl.sg_nr = 1;
  d_iov_set(&iov, &output_data[0], total_size);
  sgl.sg_iovs = &iov;
 
  // At the moment this just re-reads the same place in the array
  for(int repeat = 0; repeat < repeats; repeat++){
    ierr = daos_array_read(array_handle, DAOS_TX_NONE, &iod, &sgl, NULL);
    if(ierr != 0){
      printf("Error reading array\n");
      perror("daos_array_read");
      MPI_Abort(communicator, 0);
    }
  }

  //  ierr = daos_array_destroy(array_handle, DAOS_TX_NONE, NULL);
  if(ierr != 0){
    printf("Error destroying array\n");
    perror("daos_array_destroy");
    MPI_Abort(communicator, 0);
  }
 
  return;
  
}

void daos_read_single_array(int num_dims, long int *arraysize, long int *arraygsize, long int *arraysubsize, long int *arraystart, double *output_data, char *obj_class, int repeats, MPI_Comm communicator){

  //TODO Fragile, assumes some arrays are 3 elements long (i.e. for a 3d problem).
  int ierr, i, j, k, total_parts;
  int comm_rank;
  char array_name[100];
  daos_obj_id_t array_obj_id;
  uuid_t array_uuid;  
  daos_oclass_id_t array_obj_class;
  daos_handle_t array_handle;
  size_t local_block_size, cell_size, total_size, initial_offset, running_offset;
  daos_array_iod_t iod;
  daos_size_t array_size;
  d_sg_list_t sgl;
  daos_range_t *rg;
  d_iov_t *iov;

  MPI_Comm_rank(communicator, &comm_rank);
  
  strcpy(array_name, "total-data");
  
  array_obj_class = str_to_oc(obj_class);  

  array_obj_id.hi = 0;
  array_obj_id.lo = 0;
  
  uuid_generate_md5(array_uuid, seed, array_name, strlen(array_name));

  memcpy(&(array_obj_id.hi), &(array_uuid[0]) + sizeof(uint64_t), sizeof(uint64_t));
  memcpy(&(array_obj_id.lo), &(array_uuid[0]), sizeof(uint64_t));
  
  /*
   * create and open array object
   */ 
  daos_array_generate_oid(container_handle, &array_obj_id, DAOS_OT_ARRAY_BYTE, array_obj_class, 0, 0);
      
  ierr = daos_array_open(container_handle, array_obj_id, DAOS_TX_NONE, DAOS_OO_RW, &cell_size, &local_block_size, &array_handle, NULL);
  if (ierr != 0) {
    printf("%d array open failed with %d\n", comm_rank, ierr);
    MPI_Abort(communicator, 0);
  }  

  // iod variables (ranges) represent the position in the DAOS array (i.e. the DAOS array where the data will be stored)
  // sgl variables (scatter/gather) represent the position in the source array (i.e. in memory array the data is coming from)

  // Total number of items to be written is the combined size of the first two dimensions
  total_parts =  arraysubsize[0]*arraysubsize[1];
  iod.arr_nr = total_parts;

  // Allocate an array of ranges to be populated
  rg = (daos_range_t *)malloc(sizeof(daos_range_t)*total_parts);
  // Setup the iod to link to that array of ranges
  iod.arr_rgs = rg;

  initial_offset = arraystart[0]*(arraygsize[1]*arraygsize[2]);
  initial_offset = initial_offset + (arraystart[1]*arraygsize[2]);
  initial_offset = initial_offset + arraystart[2];
  initial_offset = initial_offset * sizeof(double);
  //  initial_offset = initial_offset;// * sizeof(double);

  running_offset = initial_offset;

  // Populate the array of ranges
  for(i=0; i<arraysubsize[0]; i++){
    for(j=0; j<arraysubsize[1]; j++){  
      // rg_len is the amount of data to be written into the DAOS array for this operation
      rg[i*arraysubsize[1]+j].rg_len = arraysubsize[2]*sizeof(double);
      //      rg[i*arraysubsize[1]+j].rg_len = arraysubsize[2];//*sizeof(double);
      // idx is the offset in the array where the data should be written
      // In this case it's the offset in the global array of this local portion
      rg[i*arraysubsize[1]+j].rg_idx = running_offset;
      running_offset = running_offset + (arraygsize[2]*sizeof(double));
      //      running_offset = running_offset + (arraygsize[2]);//*sizeof(double));
    }
    running_offset = initial_offset + ((i+1)*arraygsize[1]*arraygsize[2]*sizeof(double));
    //    running_offset = initial_offset + ((i+1)*arraygsize[1]*arraygsize[2]);//*sizeof(double));
  }

    
  sgl.sg_nr = total_parts;
  // Allocate an array of scatter/gathers to be populated
  iov = (d_iov_t *)malloc(sizeof(d_iov_t)*total_parts);
  sgl.sg_iovs = iov;

  initial_offset = 0;

  for(i=0; i<arraysubsize[0]; i++){
    for(j=0; j<arraysubsize[1]; j++){
      d_iov_set(&iov[i*arraysubsize[1]+j], &output_data[initial_offset], arraysubsize[2]*sizeof(double));
      initial_offset = initial_offset + arraysubsize[2];
    }
  }
  
 // At the moment this just re-reads the same place in the array
  for(int repeat = 0; repeat < repeats; repeat++){
    ierr = daos_array_read(array_handle, DAOS_TX_NONE, &iod, &sgl, NULL);
    if(ierr != 0){
      printf("Error reading array\n");
      perror("daos_array_read");
      MPI_Abort(communicator, 0);
    }
  }

  //  ierr = daos_array_destroy(array_handle, DAOS_TX_NONE, NULL);
  if(ierr != 0){
    printf("Error destroying array\n");
    perror("daos_array_destroy");
    MPI_Abort(communicator, 0);
  }

  free(iov);
  free(rg);
 
  return;
  
}

void daos_read_object_fortran(int num_dims, long int *arraysize, long int *arraygsize, long int *arraysubsize, long int *arraystart, double *output_data, char *obj_class, int repeats, int daosconfig, MPI_Fint communicator){

  MPI_Comm c_communicator;

  c_communicator = MPI_Comm_f2c(communicator);

  if(daosconfig == 3){

    daos_read_separate_objects(num_dims, arraysize, arraygsize, arraysubsize, arraystart, output_data, obj_class, repeats, communicator);

  }else if(daosconfig == 4){

    daos_read_single_object(num_dims, arraysize, arraygsize, arraysubsize, arraystart, output_data, obj_class, repeats, communicator);

  }else if(daosconfig == 5){

  }else{

    printf("Error, unknown variant of DAOS config\n");

  }

  return;

}


void daos_read_separate_objects(int num_dims, long int *objectsize, long int *objectgsize, long int *objectsubsize, long int *objectstart, double *output_data, char *obj_class, int repeats, MPI_Comm communicator){

  //TODO Fragile, assumes some objects are 3 elements long (i.e. for a 3d problem).
  int ierr, i;
  int comm_rank;
  char object_name[100];
  daos_obj_id_t object_obj_id;
  uuid_t object_uuid;  
  daos_oclass_id_t object_obj_class;
  daos_handle_t obj_handle;
  size_t local_block_size, cell_size, total_size;
  daos_iod_t iod;
  daos_size_t object_size;
  d_sg_list_t sgl;
  daos_range_t rg;
  d_iov_t iov;
  /*
  MPI_Comm_rank(communicator, &comm_rank);

  sprintf(object_name,"%d",comm_rank);
  
  strcat(object_name, "-data"); 
  
  object_obj_class = str_to_oc(obj_class);  
  
  object_obj_id.hi = 0;
  object_obj_id.lo = 0;
  
  uuid_generate_md5(object_uuid, seed, object_name, strlen(object_name));

  memcpy(&(object_obj_id.hi), &(object_uuid[0]) + sizeof(uint64_t), sizeof(uint64_t));
  memcpy(&(object_obj_id.lo), &(object_uuid[0]), sizeof(uint64_t));
    
  
  // open object object   
  daos_obj_generate_oid(container_handle, &object_obj_id, DAOS_OT_ARRAY_BYTE, object_obj_class, 0, 0);
  
  ierr = daos_object_open(container_handle, object_obj_id, DAOS_TX_NONE, DAOS_OO_RW, &cell_size, &local_block_size, &obj_handle, NULL);
  if (ierr != 0) {
    printf("object open failed with %d", ierr);
  }

  total_size = sizeof(double);
  for(i=0; i<num_dims; i++){
    total_size = total_size * objectsubsize[i];
  }
 
  ierr = daos_object_get_size(obj_handle, DAOS_TX_NONE, &object_size, NULL);

  if(object_size != total_size){
    printf("DAOS object sizes not the same as the calculated size %ld %ld\n", object_size, total_size);
  }

  iod.iod_nr = 1;
  rg.rg_len = total_size;
  rg.rg_idx = 0;
  iod.arr_rgs = &rg;
  
  sgl.sg_nr = 1;
  d_iov_set(&iov, &output_data[0], total_size);
  sgl.sg_iovs = &iov;
  
  ierr = daos_object_read(obj_handle, DAOS_TX_NONE, &iod, &sgl, NULL);
  if(ierr != 0){
    printf("Error reading object\n");
    perror("daos_object_read");
    MPI_Abort(communicator, 0);
  }

  ierr = daos_object_destroy(obj_handle, DAOS_TX_NONE, NULL);
  if(ierr != 0){
    printf("Error destroying object\n");
    perror("daos_object_destroy");
    MPI_Abort(communicator, 0);
  }
  */
  return;
  
}

void daos_read_single_object(int num_dims, long int *objectsize, long int *objectgsize, long int *objectsubsize, long int *objectstart, double *output_data, char *obj_class, int repeats, MPI_Comm communicator){

  //TODO Fragile, assumes some objects are 3 elements long (i.e. for a 3d problem).
  int ierr, i, j, k, total_parts;
  int comm_rank;
  char object_name[100];
  daos_obj_id_t object_obj_id;
  uuid_t object_uuid;  
  daos_oclass_id_t object_obj_class;
  daos_handle_t obj_handle;
  size_t local_block_size, cell_size, total_size, initial_offset, running_offset;
  daos_iod_t iod;
  daos_size_t object_size;
  d_sg_list_t sgl;
  daos_range_t *rg;
  d_iov_t *iov;
  /*
  MPI_Comm_rank(communicator, &comm_rank);
  
  strcpy(object_name, "total-data");
  
  object_obj_class = str_to_oc(obj_class);  

  object_obj_id.hi = 0;
  object_obj_id.lo = 0;
  
  uuid_generate_md5(object_uuid, seed, object_name, strlen(object_name));

  memcpy(&(object_obj_id.hi), &(object_uuid[0]) + sizeof(uint64_t), sizeof(uint64_t));
  memcpy(&(object_obj_id.lo), &(object_uuid[0]), sizeof(uint64_t));
  
 
  // create and open object object
  daos_object_generate_oid(container_handle, &object_obj_id, true, object_obj_class, 0, 0);
      
  ierr = daos_object_open(container_handle, object_obj_id, DAOS_TX_NONE, DAOS_OO_RW, &cell_size, &local_block_size, &obj_handle, NULL);
  if (ierr != 0) {
    printf("%d object open failed with %d\n", comm_rank, ierr);
  }  

  // iod variables (ranges) represent the position in the DAOS object (i.e. the DAOS object where the data will be stored)
  // sgl variables (scatter/gather) represent the position in the source object (i.e. in memory object the data is coming from)

  // Total number of items to be written is the combined size of the first two dimensions
  total_parts =  objectsubsize[0]*objectsubsize[1];
  iod.iod_nr = total_parts;

  // Allocate an object of ranges to be populated
  rg = (daos_range_t *)malloc(sizeof(daos_range_t)*total_parts);
  // Setup the iod to link to that object of ranges
  iod.arr_rgs = rg;

  initial_offset = objectstart[0]*(objectgsize[1]*objectgsize[2]);
  initial_offset = initial_offset + (objectstart[1]*objectgsize[2]);
  initial_offset = initial_offset + objectstart[2];
  initial_offset = initial_offset * sizeof(double);
  //  initial_offset = initial_offset;// * sizeof(double);

  running_offset = initial_offset;

  // Populate the object of ranges
  for(i=0; i<objectsubsize[0]; i++){
    for(j=0; j<objectsubsize[1]; j++){  
      // rg_len is the amount of data to be written into the DAOS object for this operation
      rg[i*objectsubsize[1]+j].rg_len = objectsubsize[2]*sizeof(double);
      //      rg[i*objectsubsize[1]+j].rg_len = objectsubsize[2];//*sizeof(double);
      // idx is the offset in the object where the data should be written
      // In this case it's the offset in the global object of this local portion
      rg[i*objectsubsize[1]+j].rg_idx = running_offset;
      running_offset = running_offset + (objectgsize[2]*sizeof(double));
      //      running_offset = running_offset + (objectgsize[2]);//*sizeof(double));
    }
    running_offset = initial_offset + ((i+1)*objectgsize[1]*objectgsize[2]*sizeof(double));
    //    running_offset = initial_offset + ((i+1)*objectgsize[1]*objectgsize[2]);//*sizeof(double));
  }

    
  sgl.sg_nr = total_parts;
  // Allocate an object of scatter/gathers to be populated
  iov = (d_iov_t *)malloc(sizeof(d_iov_t)*total_parts);
  sgl.sg_iovs = iov;

  initial_offset = 0;

  for(i=0; i<objectsubsize[0]; i++){
    for(j=0; j<objectsubsize[1]; j++){
      d_iov_set(&iov[i*objectsubsize[1]+j], &output_data[initial_offset], objectsubsize[2]*sizeof(double));
      initial_offset = initial_offset + objectsubsize[2];
    }
  }
  
  ierr = daos_object_read(obj_handle, DAOS_TX_NONE, &iod, &sgl, NULL);
  if(ierr != 0){
    printf("Error reading object\n");
    perror("daos_object_read");
    MPI_Abort(communicator, 0);
  }

  ierr = daos_object_destroy(obj_handle, DAOS_TX_NONE, NULL);
  if(ierr != 0){
    printf("Error destroying object\n");
    perror("daos_object_destroy");
    MPI_Abort(communicator, 0);
  }

  free(iov);
  free(rg);
  */ 
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
  int comm_rank;

  MPI_Comm_rank(communicator, &comm_rank);

  MPI_Barrier(communicator);

  if(comm_rank == 0){
    ierr = daos_cont_destroy(pool_handle, container_name, 0, NULL);
    if(ierr != 0){
      printf("Error destroying container %d\n", ierr);
      perror("daos_destroy_container");
      MPI_Abort(communicator, 0);
    }
  }

  return;

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

  return;

}

void daos_cleanup_fortran(MPI_Fint communicator){

  MPI_Comm c_communicator;

  c_communicator = MPI_Comm_f2c(communicator);

  daos_cleanup(c_communicator);

  return;

}


void daos_cleanup(MPI_Comm communicator){

  daos_destroy_container(communicator);

  daos_close_pool(communicator);

  daos_finalise(communicator);

  return;

}
