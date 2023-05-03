MODULE NN_TensorFlow_IO

  USE HDF5
  USE ISO_C_BINDING
  USE NeuralNet_Define, ONLY: NeuralNet

IMPLICIT NONE

  ! ============
  !
  ! Visibilities
  !
  ! =============
  PRIVATE
  
  PUBLIC :: Inquire_num_layers
  PUBLIC :: Inquire_TensorFlow
  PUBLIC :: Read_TensorFlow

  ! ==================
  !
  ! Module Parameters
  !
  ! ==================

  INTEGER, PARAMETER :: NL = 30
  CHARACTER(LEN=*), PARAMETER :: bias_dset_name = 'bias:0'
  CHARACTER(LEN=*), PARAMETER :: weight_dset_name = 'kernel:0'

  ! ==================
  !
  ! Module TYPEs
  !
  ! ==================

  TYPE :: opdata
    INTEGER :: recurs
    INTEGER :: counter
    TYPE(opdata), POINTER :: prev
  END TYPE opdata

CONTAINS

  ! INTEGER(KIND=C_INT) FUNCTION get_weight_dims_func(loc_id, name, info, operator_data) BIND(C)
  !   !
  !   ! Data dictionary
  !   !
  !   INTEGER(HID_T), VALUE :: loc_id
  !   CHARACTER(LEN=1), DIMENSION(1:50) :: name
  !   TYPE(h5o_info_t), VALUE :: info
  !   CHARACTER(LEN=50) :: name_string = ' '
  !   TYPE(C_PTR), VALUE :: operator_data ! cptr
  !   TYPE(opdata), POINTER :: od

  !   INTEGER :: ii
  !   INTEGER :: intermediate
  !   INTEGER :: name_length
  !   !
  !   ! Instructions
  !   !
  !   DO ii = 1, 50
  !     IF(name(ii)(1:1) .EQ. C_NULL_CHAR) EXIT
  !     name_string(ii:ii) = name(ii)(1:1)
  !   END DO

  !   !WRITE(*,"('/')",ADVANCE="NO")
  !   CALL C_F_POINTER(operator_data, od)
  !   IF(name(1)(1:1) .EQ. '.') THEN
  !     WRITE(*,"(' (Group)')")
  !   ELSE
  !     name_length = LEN(TRIM(name_string))
  !     IF((info%type == H5O_TYPE_DATASET_F) &!)THEN 
  !         .AND. (TRIM(name_string(1:19)) == "model_weights/dense") & !)THEN
  !         .AND. (TRIM(name_string((name_length-7):name_length))) == "kernel:0") THEN
  !       !name_length = LEN(TRIM(name_string))


  !       !
  !       ! Read layer bias dimensions
  !       !

  !       ! Check if bias dataset exists
  !       CALL H5LEXISTS_F(loc_id, bias_dset_name, got, ErrorFlag)
  !       IF ( .NOT. got .OR. (ErrorFlag /= 0) ) THEN 
  !         RETURN
  !       END IF

  !       ! Open bias dataset
  !       CALL H5DOPEN_F(loc_id, bias_dset_name, dset_id, ErrorFlag)
  !       IF (ErrorFlag /= 0) THEN
  !         WRITE(*,'("cannot open bias HDF5 dataset",/)')
  !         RETURN
  !       END IF

  !       ! Get dataspace
  !       CALL h5DGET_SPACE_F(dset_id, space_id, ErrorFlag)     
  !       IF (ErrorFlag /= 0) THEN
  !         WRITE(*,'("cannot read bias HDF5 dataset",/)')
  !         RETURN
  !       END IF

  !       ! Get dimensions
  !       CALL H5SGET_SIMPLE_EXTENT_DIMS_F(space_id, dims, ErrorFlag)

  !       ! Close bias dataset
  !       CALL H5DCLOSE_F(dset_id, ErrorFlag)
  !       IF (ErrorFlag /= 0) THEN
  !         WRITE(*,'("cannot close HDF5 dataset",/)')
  !         RETURN
  !       END IF

  !       !
  !       ! Read layer weight dimensions
  !       !

  !       ! Check if weight dataset exists
  !       CALL H5LEXISTS_F(wgi_sub, weight_dset_name, got, ErrorFlag)
  !       IF ( .NOT. got ) THEN
  !         RETURN
  !       END IF

  !       ! Open weight dataset
  !       CALL H5DOPEN_F(wgi_sub, weight_dset_name, dset_id, ErrorFlag)
  !       IF (ErrorFlag /= 0) THEN
  !         WRITE(*,'("cannot open bias HDF5 dataset",/)')
  !         RETURN
  !       END IF

  !       ! Get dataspace
  !       CALL h5DGET_SPACE_F(dset_id, space_id, ErrorFlag)
  !       IF (ErrorFlag /= 0) THEN
  !         WRITE(*,'("cannot read bias HDF5 dataset",/)')
  !         RETURN
  !       END IF

  !       ! Get dimensions
  !       CALL H5SGET_SIMPLE_EXTENT_DIMS_F(space_id, dims, ErrorFlag)

  !       ! Close weight dataset
  !       CALL H5DCLOSE_F(dset_id, ErrorFlag)
  !       IF (ErrorFlag /= 0) THEN
  !         WRITE(*,'("cannot close HDF5 dataset",/)')
  !         RETURN
  !       END IF
  !     END IF
  !   END IF

   
  !   get_num_layers_func = 0

  ! END FUNCTION get_weight_dims_func


  INTEGER(KIND=C_INT) FUNCTION get_num_layers_func(loc_id, name, info, operator_data) BIND(C)
    !
    ! Data dictionary
    !
    INTEGER(HID_T), VALUE :: loc_id
    CHARACTER(LEN=1), DIMENSION(1:50) :: name
    TYPE(h5o_info_t), VALUE :: info
    CHARACTER(LEN=50) :: name_string = ' '
    TYPE(C_PTR), VALUE :: operator_data ! cptr
    TYPE(opdata), POINTER :: od
    !TYPE(opdata), TARGET :: nextod
    !TYPE(C_PTR) :: ptr2
    INTEGER :: ii
    INTEGER :: intermediate
    INTEGER :: name_length
    !
    ! Instructions
    !
    DO ii = 1, 50
      IF(name(ii)(1:1) .EQ. C_NULL_CHAR) EXIT
      name_string(ii:ii) = name(ii)(1:1)
    END DO

    !WRITE(*,"('/')",ADVANCE="NO")
    CALL C_F_POINTER(operator_data, od)
    IF(name(1)(1:1) .EQ. '.') THEN
      WRITE(*,"(' (Group)')")
    ELSE
      name_length = LEN(TRIM(name_string))
      IF((info%type == H5O_TYPE_GROUP_F) &!)THEN 
          .AND. (TRIM(name_string(1:19)) == "model_weights/dense") & !)THEN
          .AND. (TRIM(name_string((name_length-7):name_length))) == "kernel:0") THEN
        !name_length = LEN(TRIM(name_string))
        !WRITE(*,'(A," (Dataset)")') TRIM(name_string((name_length-7):name_length))
        od%recurs = od%recurs + 1
        !WRITE(*,*) "Iteration: ", od%recurs
      END IF
    END IF

    !CALL C_F_POINTER(operator_data, od)

    !WRITE(*,*) "Iteration: ", od%recurs
    !nextod%prev => od
    !od%recurs = od%recurs + 1 
    !WRITE(*,*) nextod%recurs
    !ptr2 = C_LOC(nextod)
   
    get_num_layers_func = 0

  END FUNCTION get_num_layers_func


  INTEGER(KIND=C_INT) FUNCTION op_func(loc_id, name, info, operator_data) BIND(C)
    !
    ! Data dictionary
    !
    INTEGER(HID_T), VALUE :: loc_id
    CHARACTER(LEN=1), DIMENSION(1:50) :: name
    TYPE(h5o_info_t), VALUE :: info
    CHARACTER(LEN=50) :: name_string = ' '
    TYPE(C_PTR), VALUE :: operator_data ! cptr
    TYPE(opdata), POINTER :: od
    TYPE(opdata), TARGET :: nextod
    TYPE(C_PTR) :: ptr2
    INTEGER :: ii
    INTEGER :: intermediate
    !
    ! Instructions
    !
    DO ii = 1, 50
      IF(name(ii)(1:1) .EQ. C_NULL_CHAR) EXIT
        name_string(ii:ii) = name(ii)(1:1)
      END DO

      WRITE(*,"('/')",ADVANCE="NO")
      IF(name(1)(1:1) .EQ. '.') THEN
        WRITE(*,"(' (Group)')")
      ELSE
        IF(info%type .EQ. H5O_TYPE_GROUP_F) THEN
          WRITE(*,'(A," (Group)")') TRIM(name_string)
        ELSE IF(info%type .EQ. H5O_TYPE_DATASET_F) THEN
          WRITE(*,'(A," (Dataset)")') TRIM(name_string)
        ELSE IF(info%type .EQ. H5O_TYPE_NAMED_DATATYPE_F) THEN
          WRITE(*,'(A," (Datatype)")') TRIM(name_string)
        ELSE
          WRITE(*,'(A," (Unkown)")') TRIM(name_string)
        END IF
      END IF

      CALL C_F_POINTER(operator_data, od)

      WRITE(*,*) "Iteration: ", od%recurs
      nextod%prev => od
      od%recurs = od%recurs + 1 
      WRITE(*,*) nextod%recurs
      ptr2 = C_LOC(nextod)
   
      op_func = 0

    END FUNCTION op_func


    INTEGER FUNCTION Inquire_num_layers(filename) RESULT(num_groups)

      !
      ! Data dictionary
      !
      
      CHARACTER(LEN=NL), INTENT(IN) :: filename
      INTEGER(HID_T) :: fileid
      INTEGER :: stat
      TYPE(opdata), TARGET :: od
      TYPE(opdata), POINTER :: od_out
      TYPE(C_FUNPTR) :: funptr
      TYPE(C_PTR) :: ptr
      INTEGER returnvalue

      !
      ! Instructions
      !
      
      CALL h5open_f(stat)
      CALL h5fopen_f(TRIM(ADJUSTL(filename)), H5F_ACC_RDONLY_F, fileid, stat)

      od%recurs = 0
      od%counter = 0
      od%prev => NULL()

      !WRITE(*,'(A)') "Objects in the file: "

      ptr = C_LOC(od)
      funptr = C_FUNLOC(get_num_layers_func)
      CALL h5ovisit_f(fileid, &
                      H5_INDEX_NAME_F, &
                      H5_ITER_NATIVE_F, &
                      funptr, &
                      ptr, &
                      returnvalue, &
                      stat)

      !CALL C_F_POINTER(ptr, od_out)
      !WRITE(*,*) "Final output: ", od_out%recurs, od%recurs
      !NULLIFY(od_out)
      CALL h5fclose_f(fileid, stat)
      num_groups = od%recurs
    END FUNCTION Inquire_num_layers


  ! FUNCTION Count_Groups(h5_filename) RESULT(num_groups)
  !   !
  !   ! Data dictionary
  !   !
  !   CHARACTER(LEN=NL), INTENT(IN) :: h5_filename
  !   ! Local variables
  !   CHARACTER(LEN=30) :: ErrorMessage
  !   INTEGER(HID_T) :: file_id
  !   INTEGER(HID_T) :: group_id
  !   INTEGER(HID_T) :: root_group_id
  !   INTEGER(HID_T) :: num_objs
  !   INTEGER(HID_T) :: ErrorFlag
  !   INTEGER :: num_groups
  !   INTEGER(HID_T) :: ii
  !   INTEGER(HSIZE_T) :: obj_count
  !   !
  !   ! Instructions
  !   !

  !   ! Initialize error flag
  !   CALL h5open_f(ErrorFlag)
  !   IF (ErrorFlag < 0) THEN
  !     ErrorMessage=" *** Error initialising HDF routines"
  !     RETURN
  !   END IF

  !   ! Open the HDF5 file
  !   CALL H5Fopen_f(TRIM(h5_filename), H5F_ACC_RDONLY, file_id, ErrorFlag)
  !   IF(ErrorFlag < 0) THEN
  !     WRITE(*,*) "Error opening file ", h5_filename
  !     num_groups = -1
  !     RETURN
  !   END IF

  !   ! Get the root group
  !   CALL H5Gopen_f(file_id, "/", root_group_id, ErrorFlag)
  !   IF (ErrorFlag /= 0) then
  !     WRITE(*,*) "Error getting root group."
  !     num_groups = -1 ! return error code
  !     CALL H5Gclose_f(root_group_id, ErrorFlag)
  !     IF (ErrorFlag /= 0) THEN
  !       WRITE(*,*) "Error closing root group."
  !       num_groups = -1 ! return error code
  !     END IF
  !     CALL H5Fclose_f(file_id, ErrorFlag)
  !     IF(ErrorFlag /= 0) THEN
  !       WRITE(*,*) "Error closing HDF5 file."
  !       num_groups = -1 ! return error code
  !     END IF
  !     RETURN
  !   END IF
    
  !   ! Get number of objects in root group
  !   CALL H5Gget_num_objs_f(root_group_id, num_objs, ErrorFlag)
  !   IF(ErrorFlag /= 0) THEN
  !     WRITE(*,*) "Error getting object count."
  !     num_groups = -1 ! return error code
  !     CALL H5Gclose_f(root_group_id, ErrorFlag)
  !     IF(ErrorFlag /= 0) THEN
  !       WRITE(*,*) "Error closing root group."
  !       num_groups = -1 ! return error code
  !     END IF
  !     CALL H5Fclose_f(file_id, ErrorFlag)
  !     IF(ErrorFlag /= 0) THEN
  !       WRITE(*,*) "Error closing HDF5 file."
  !       num_groups = -1 ! return error code
  !     END IF
  !     RETURN
  !   END IF

  !   ! Count the number of groups in the file
  !   num_groups = 0
  !   DO ii = 0, obj_count-1
  !     CALL H5Gopen_f(root_group_id, &
  !                    TRIM(H5Gget_objname_by_idx_f(root_group_id, ii)), &
  !                    group_id, &
  !                    ErrorFlag)
  !     ! group_id = H5Fget_obj_by_idx(file_id, ".", H5_INDEX_NAME, H5_ITER_INC, i, stat)
  !     IF(ErrorFlag < 0) THEN
  !       WRITE(*,*) "Error getting object by index for file ", h5_filename
  !       num_groups = -1
  !       RETURN
  !     END IF

  !     IF(H5Gget_info_f(group_id, ErrorFlag) == H5G_GROUP) then
  !       num_groups = num_groups + 1
  !     END IF

  !     CALL H5Gclose(group_id, ErrorFlag)
  !     IF(ErrorFlag < 0) THEN
  !       WRITE(*,*) "Error closing group for file ", h5_filename
  !       num_groups = -1
  !       CALL H5Gclose_f(root_group_id, ErrorFlag)
  !       IF (ErrorFlag /= 0) THEN
  !         WRITE(*,*) "Error closing root group."
  !         num_groups = -1 ! return error code
  !       END IF
  !       CALL H5Fclose_f(file_id, ErrorFlag)
  !       IF(ErrorFlag /= 0) THEN
  !         WRITE(*,*) "Error closing HDF5 file."
  !         num_groups = -1 ! return error code
  !       END IF
  !       RETURN
  !     END IF
  !   END DO

  !   ! Close the HDF5 file
  !   CALL H5Fclose_f(file_id, ErrorFlag)
  !   IF (stat < 0) THEN
  !     WRITE(*,*) "Error closing file ", h5_filename
  !     num_groups = -1
  !     RETURN
  !   END IF

  !   RETURN
  ! END FUNCTION Count_Groups


  SUBROUTINE Inquire_TensorFlow(h5_filename, &
                                n_layers, &
                                xshape_bias, &
                                xshape_weights)
    !
    ! Data dictionary
    !
    CHARACTER(LEN=NL), INTENT(IN) :: h5_filename
    INTEGER,           INTENT(IN) :: n_layers
    INTEGER                       :: ErrorFlag ! Output variable
    CHARACTER(LEN=30)             :: ErrorMessage
    LOGICAL                       :: got
    INTEGER(HID_T)                :: file_id   ! Output variable
    CHARACTER(LEN = 1)            :: rootname ! Input variable
    INTEGER(HID_T)                :: root_id  ! Output variable
    CHARACTER(LEN=10)             :: weight_group_name
    INTEGER(HID_T)                :: weight_group_id
    CHARACTER(LEN=10)             :: weight_group_name_base
    INTEGER(HID_T)                :: wgi
    INTEGER(HID_T)                :: wgi_sub
    CHARACTER(LEN=10)             :: wgn
    INTEGER(HID_T)                :: space_id
    INTEGER(HID_T)                :: dset_id
    INTEGER(HSIZE_T), DIMENSION(n_layers),   INTENT(OUT) :: xshape_bias
    INTEGER(HSIZE_T), DIMENSION(n_layers,2), INTENT(OUT) :: xshape_weights
    INTEGER(HSIZE_T), DIMENSION(n_layers) :: maxdim_bias
    INTEGER(HSIZE_T), DIMENSION(n_layers,2) :: maxdim_weights
    INTEGER                       :: ii

    !
    ! Instructions
    !

    ! Initialize error flag
    CALL H5Open_f(ErrorFlag)
    IF (ErrorFlag < 0) THEN
      ErrorMessage=" *** Error initialising HDF routines"
      RETURN
    END IF

    ! Open HDF5 file
    CALL H5FOpen_f(h5_filename, H5F_ACC_RDONLY_F, file_id, ErrorFlag)
    IF (ErrorFlag < 0) THEN
      ErrorMessage=" *** Error opening HDF file ***"
      RETURN
    END IF

    ! Open root group
    rootname = "/"
    CALL h5gopen_f(file_id, rootname, root_id, ErrorFlag)
    IF (ErrorFlag < 0) THEN
      ErrorMessage=" *** Error opening root group ***"
      RETURN
    END IF


    ! Get the dimensions of bias vectors and weight matrices in each group
    ! Loop over all layer groups
    weight_group_name_base = 'dense_'
    dim_loop: DO ii = 1, n_layers
      IF(ii .EQ. 1) THEN
        wgn = 'dense'
      ELSE
        WRITE(wgn, "(A6,I1)") TRIM(ADJUSTL(weight_group_name_base)), ii-1
      END IF
      ! Open layer group
      CALL h5gopen_f(weight_group_id, wgn, wgi, ErrorFlag)
      IF (ErrorFlag .LT. 0) THEN
        ErrorMessage=" *** Error opening Group model_weights ***"
        RETURN
      END IF
      ! Open layer sub-group
      CALL h5gopen_f(wgi, wgn, wgi_sub, ErrorFlag)
      IF (ErrorFlag .LT. 0) THEN
        ErrorMessage=" *** Error opening Group model_weights ***"
        RETURN
      END IF

      !
      ! Read layer bias dimensions
      !

      ! Check if bias dataset exists
      CALL H5LEXISTS_F(wgi_sub, bias_dset_name, got, ErrorFlag)
      IF ( .NOT. got .OR. (ErrorFlag /= 0) ) THEN 
        RETURN
      END IF

      ! Open bias dataset
      CALL H5DOPEN_F(wgi_sub, bias_dset_name, dset_id, ErrorFlag)
      IF (ErrorFlag /= 0) THEN
        WRITE(*,'("cannot open bias HDF5 dataset",/)')
        RETURN
      END IF

      ! Get dataspace
      CALL h5DGET_SPACE_F(dset_id, space_id, ErrorFlag)     
      IF (ErrorFlag /= 0) THEN
        WRITE(*,'("cannot read bias HDF5 dataset",/)')
        RETURN
      END IF

      ! Get dimensions
      CALL H5SGET_SIMPLE_EXTENT_DIMS_F(wgi_sub, &
                                       xshape_bias(ii), &
                                       maxdim_bias(ii), &
                                       ErrorFlag)

      ! Close bias dataset
      CALL H5DCLOSE_F(dset_id, ErrorFlag)
      IF (ErrorFlag /= 0) THEN
        WRITE(*,'("cannot close HDF5 dataset",/)')
        RETURN
      END IF

      !
      ! Read layer weight dimensions
      !

      ! Check if weight dataset exists
      CALL H5LEXISTS_F(wgi_sub, weight_dset_name, got, ErrorFlag)
      IF ( .NOT. got ) THEN
        RETURN
      END IF

      ! Open weight dataset
      CALL H5DOPEN_F(wgi_sub, weight_dset_name, dset_id, ErrorFlag)
      IF (ErrorFlag /= 0) THEN
        WRITE(*,'("cannot open bias HDF5 dataset",/)')
        RETURN
      END IF

      ! Get dataspace
      CALL h5DGET_SPACE_F(dset_id, space_id, ErrorFlag)
      IF (ErrorFlag /= 0) THEN
        WRITE(*,'("cannot read bias HDF5 dataset",/)')
        RETURN
      END IF

      ! Get dimensions
      CALL H5SGET_SIMPLE_EXTENT_DIMS_F(wgi_sub, &
                                       xshape_weights(ii,:), &
                                       maxdim_weights(ii,:), &
                                       ErrorFlag)

      ! Close bias dataset
      CALL H5DCLOSE_F(dset_id, ErrorFlag)
      IF (ErrorFlag /= 0) THEN
        WRITE(*,'("cannot close HDF5 dataset",/)')
        RETURN
      END IF

    END DO dim_loop

    ! Close all remaining id's
    CALL h5gclose_f(root_id,ErrorFlag)
    CALL h5fclose_f(file_id,ErrorFlag)
    CALL h5close_f(ErrorFlag)

    RETURN
  END SUBROUTINE Inquire_TensorFlow


  SUBROUTINE Read_TensorFlow(net, h5_filename)
    ! Data dictionary
    TYPE(NeuralNet), INTENT(INOUT) :: net
    CHARACTER(LEN=NL), INTENT(IN) :: h5_filename
    INTEGER :: ErrorFlag ! Output variable
    CHARACTER(LEN=30) :: ErrorMessage
    LOGICAL :: got
    INTEGER(HID_T) :: file_id   ! Output variable
    CHARACTER(LEN = 1)  :: rootname ! Input variable
    INTEGER(HID_T)      :: root_id  ! Output variable
    CHARACTER(LEN=10) :: main_group ! Input variable
    INTEGER(HID_T)    :: root_group_id   ! Output variable
    CHARACTER(LEN=10) :: weight_group_name
    INTEGER(HID_T)    :: weight_group_id
    CHARACTER(LEN=10) :: weight_group_name_base
    INTEGER(HID_T)    :: wgi
    INTEGER(HID_T)    :: wgi_sub
    CHARACTER(LEN=10) :: wgn
    INTEGER(HID_T)    :: dset_id
    INTEGER(HID_T)    :: dtype_id
    INTEGER(HSIZE_T), ALLOCATABLE  :: xshape_bias(:)
    INTEGER(HSIZE_T), ALLOCATABLE  :: xshape_weights(:,:)
    INTEGER(HSIZE_T), ALLOCATABLE  :: maxdim_bias(:)
    INTEGER(HSIZE_T), ALLOCATABLE  :: maxdim_weights(:,:)
    INTEGER           :: ii

    ! Instructions
    ! Initialize error flag
    CALL h5open_f(ErrorFlag)
    IF (ErrorFlag.lt.0) THEN
      ErrorMessage=" *** Error initialising HDF routines"
      RETURN
    END IF

    ! ALLOCATE dimension arrays
    ALLOCATE( xshape_bias(net%n_layers), &
              xshape_weights(net%n_layers,2), &
              maxdim_bias(net%n_layers), &
              maxdim_weights(net%n_layers,2) )

    ! Open HDF5 file
    CALL h5fopen_f(h5_filename, H5F_ACC_RDONLY_F, file_id, ErrorFlag)
    IF (ErrorFlag.lt.0) THEN
      ErrorMessage=" *** Error opening HDF file ***"
      RETURN
    END IF

    ! Open root group
    rootname = "/"
    CALL h5gopen_f(file_id, rootname, root_id, ErrorFlag)
    IF (ErrorFlag .LT. 0) THEN
      ErrorMessage=" *** Error opening root group ***"
      RETURN
    END IF

    CALL h5gopen_f(root_id, main_group, root_group_id, ErrorFlag)
    IF (ErrorFlag .LT. 0) THEN
      ErrorMessage=" *** Error opening Group 1 ***"
      RETURN
    END IF

    ! Open model_weights group on top of the root group '/'
    weight_group_name = "model_weights"
    CALL h5gopen_f(root_id, weight_group_name, weight_group_id, ErrorFlag)
    IF (ErrorFlag .LT. 0) THEN
      ErrorMessage=" *** Error opening Group model_weights ***"
      RETURN
    END IF

    ! Loop over all layer groups
    weight_group_name_base = 'dense_'
    GroupLoop: DO ii = 1, net%n_layers
      IF(ii .EQ. 1) THEN
        wgn = 'dense'
      ELSE
        WRITE(wgn, "(A6,I1)") TRIM(ADJUSTL(weight_group_name_base)), ii-1
      END IF
      ! Open layer group
      CALL h5gopen_f(weight_group_id, wgn, wgi, ErrorFlag)
      IF (ErrorFlag .LT. 0) THEN
        ErrorMessage=" *** Error opening Group model_weights ***"
        RETURN
      END IF
      ! Open layer sub-group
      CALL h5gopen_f(wgi, wgn, wgi_sub, ErrorFlag)
      IF (ErrorFlag .LT. 0) THEN
        ErrorMessage=" *** Error opening Group model_weights ***"
        RETURN
      END IF
      
      !dtype_id = H5T_NATIVE_REAL
      dtype_id = H5T_IEEE_F32LE

      !
      ! Read layer bias
      !

      ! Check if bias dataset exists
      CALL H5LEXISTS_F(wgi_sub, bias_dset_name, got, ErrorFlag)
      IF ( .NOT. got ) THEN 
        RETURN
      END IF

      ! Open bias dataset
      CALL H5DOPEN_F(wgi_sub, bias_dset_name, dset_id, ErrorFlag)
      IF (ErrorFlag /= 0) THEN
        WRITE(*,'("cannot open bias HDF5 dataset",/)')
        RETURN
      END IF

      ! Get bias vector dimension
      CALL H5SGET_SIMPLE_EXTENT_DIMS_F(wgi_sub, &
                                       xshape_bias(ii), &
                                       maxdim_bias(ii), &
                                       ErrorFlag)

      ! Read bias dataset
      CALL H5DREAD_F(dset_id, &
                     dtype_id, &
                     net%LayerSequence(ii)%Bias, &
                     (/xshape_bias(ii)/), &
                     ErrorFlag)
      IF (ErrorFlag /= 0) THEN
        WRITE(*,'("cannot read bias HDF5 dataset",/)')
        RETURN
      END IF

     ! Close bias dataset
     CALL H5DCLOSE_F(dset_id, ErrorFlag)
     IF (ErrorFlag /= 0) THEN
       WRITE(*,'("cannot close HDF5 dataset",/)')
       RETURN
     ENDIF

     !
     ! Read layer weights
     !

     ! Check if weight dataset exists
     CALL H5LEXISTS_F(wgi_sub, weight_dset_name, got, ErrorFlag)
     IF ( .NOT. got ) THEN
       RETURN
     END IF

     ! Open weight dataset
     CALL H5DOPEN_F(wgi_sub, weight_dset_name, dset_id, ErrorFlag)
     IF (ErrorFlag /= 0) THEN
       WRITE(*,'("cannot open weight HDF5 dataset",/)')
       RETURN
     END IF

     ! Get weight matrix dimensions
     CALL H5SGET_SIMPLE_EXTENT_DIMS_F(wgi_sub, &
                                      xshape_weights(ii,:), &
                                      maxdim_weights(ii,:), &
                                      ErrorFlag)

     ! Read weight dataset
     CALL H5DREAD_F(dset_id, &
                    dtype_id, &
                    net%LayerSequence(ii)%Weights, &
                    xshape_weights(ii,:), &
                    ErrorFlag)
     IF (ErrorFlag /= 0) THEN
       WRITE(*,'("cannot read weight HDF5 dataset",/)')
       RETURN
     END IF

     ! Close weight dataset
     CALL H5DCLOSE_F(dset_id, ErrorFlag)
     IF (ErrorFlag /= 0) THEN
       WRITE(*,'("cannot close HDF5 dataset",/)')
       RETURN
     ENDIF


     IF (ErrorFlag /= 0) got = .FALSE.

    END DO GroupLoop

    ! DEALLOCATE dimension arrays
    DEALLOCATE( xshape_bias, &
                xshape_weights, &
                maxdim_bias, &
                maxdim_weights )

    ! Close HDF5 file
    CALL h5gclose_f(root_group_id,ErrorFlag)
    CALL h5gclose_f(root_id,ErrorFlag)
    CALL h5fclose_f(file_id,ErrorFlag)
    CALL h5close_f(ErrorFlag)

    IF (ErrorFlag .LT. 0) THEN
      ErrorMessage=" *** Error closing HDF routines ***"
      RETURN
    END IF

    RETURN
  END SUBROUTINE Read_TensorFlow


END MODULE NN_TensorFlow_IO

