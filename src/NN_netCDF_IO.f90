MODULE NN_netCDF_IO

  USE NETCDF
  USE NeuralNet_Define, ONLY: NeuralNet

IMPLICIT NONE

  ! ============
  !
  ! Visibilities
  !
  ! =============
  PRIVATE
  PUBLIC :: Inquire_NLayers_netCDF
  PUBLIC :: Inquire_NetDim_netCDF
  PUBLIC :: Read_NN_netCDF
  PUBLIC :: Write_NN_netCDF

  ! ==================
  !
  ! Module Parameters
  !
  ! ==================

  INTEGER, PARAMETER :: NL = 30

  CHARACTER(LEN=*), PARAMETER :: NLAYERS_NAME = "Number of layers"
  CHARACTER(LEN=*), PARAMETER :: NNEURONS_NAME = "Number of neurons"
  CHARACTER(LEN=*), PARAMETER :: WEIGHT_NAME_BASE = "Weight matrix for layer #"
  CHARACTER(LEN=*), PARAMETER :: BIAS_NAME_BASE = "Bias vector for layer #"
  CHARACTER(LEN=*), PARAMETER :: ACTIVATION_FUNCTION_NAME_BASE = &
                                                  "Activation function for layer #"

CONTAINS


  SUBROUTINE Inquire_NLayers_netCDF(nc_filename, &
                                    n_layers)
    ! Data dictionary
    CHARACTER(LEN=*), INTENT(IN) :: nc_filename
    INTEGER :: ncid
    ! dim id's
    INTEGER :: neur_dimid
    ! var id's
    INTEGER :: layer_varid
    ! dimensions
    INTEGER, INTENT(OUT) :: n_layers

    ! Instructions

    ! Open the file
    CALL NC_CHECK( NF90_OPEN(nc_filename, NF90_NOWRITE, ncid) )

    ! Get the number of layers
    CALL NC_CHECK( NF90_INQ_VARID(ncid, NLAYERS_NAME, layer_varid) )
    CALL NC_CHECK( NF90_GET_VAR(ncid, layer_varid, n_layers) )

    ! Close the file
    CALL NC_CHECK( NF90_CLOSE(ncid) )

  END SUBROUTINE Inquire_NLayers_netCDF


  SUBROUTINE Inquire_NetDim_netCDF(nc_filename, &
                               n_layers, &
                               n_neurons, &
                               n_weights, &
                               n_biases, &
                               n_activations)
    ! Data dictionary
    CHARACTER(LEN=*), INTENT(IN) :: nc_filename
    INTEGER :: ncid
    INTEGER :: ii 
    ! dim id's
    INTEGER :: neur_dimid
    INTEGER :: lay_dimid
    INTEGER, DIMENSION(n_layers,2) :: weight_dimid   ! DIMENSION(net%n_layers,2)
    INTEGER, DIMENSION(n_layers) :: bias_dimid     ! DIMENSION(net%n_layers)
    INTEGER, DIMENSION(n_layers) :: actif_dimid    ! DIMENSION(net%n_layers)
    ! var id's
    INTEGER :: layer_varid
    ! dimensions
    INTEGER, INTENT(IN) :: n_layers
    INTEGER, INTENT(OUT) :: n_neurons
    INTEGER, DIMENSION(n_layers,2), INTENT(OUT) :: n_weights     ! DIMENSION(net%n_layers,2)
    INTEGER, DIMENSION(n_layers), INTENT(OUT) :: n_biases      ! DIMENSION(net%n_layers)
    INTEGER, DIMENSION(n_layers), INTENT(OUT) :: n_activations ! DIMENSION(net%n_layers)
    ! names
    CHARACTER(LEN=NL) :: WEIGHT_NAME
    CHARACTER(LEN=NL) :: BIAS_NAME
    CHARACTER(LEN=NL) :: ACTIVATION_FUNCTION_NAME

    ! Instructions

    ! Open the file
    CALL NC_CHECK( NF90_OPEN(nc_filename, NF90_NOWRITE, ncid) )

    ! Inquire the dimension ID's
    CALL NC_CHECK( NF90_INQ_DIMID(ncid, NNEURONS_NAME, neur_dimid) )
    Layer_DimIdInq_Loop: DO ii = 1, n_layers
      WRITE(WEIGHT_NAME,"(A,I0)") WEIGHT_NAME_BASE, ii
      WRITE(BIAS_NAME,"(A,I0)") BIAS_NAME_BASE, ii
      WRITE(ACTIVATION_FUNCTION_NAME,"(A,I0)") ACTIVATION_FUNCTION_NAME_BASE, ii
      CALL NC_CHECK( NF90_INQ_DIMID(ncid, &
                                  WEIGHT_NAME//'_x', &
                                  weight_dimid(ii,1)) )
      CALL NC_CHECK( NF90_INQ_DIMID(ncid, &
                                  WEIGHT_NAME//'_y', &
                                  weight_dimid(ii,2)) )
      CALL NC_CHECK( NF90_INQ_DIMID(ncid, &
                                  BIAS_NAME, &
                                  bias_dimid(ii)) )
      CALL NC_CHECK( NF90_INQ_DIMID(ncid, &
                                  ACTIVATION_FUNCTION_NAME, &
                                  actif_dimid(ii)) )
    END DO Layer_DimIdInq_Loop

    ! Inquire the dimension ID's
    CALL NC_CHECK( NF90_INQUIRE_DIMENSION(ncid, neur_dimid, LEN=n_neurons) )
    Layer_DimInq_Loop: DO ii = 1, n_layers
      WRITE(WEIGHT_NAME,"(A,I0)") WEIGHT_NAME_BASE, ii
      WRITE(BIAS_NAME,"(A,I0)") BIAS_NAME_BASE, ii
      WRITE(ACTIVATION_FUNCTION_NAME,"(A,I0)") ACTIVATION_FUNCTION_NAME_BASE, ii
      CALL NC_CHECK( NF90_INQUIRE_DIMENSION(ncid, &
                                            weight_dimid(ii,1), &
                                            LEN=n_weights(ii,1)) )
      CALL NC_CHECK( NF90_INQUIRE_DIMENSION(ncid, &
                                            weight_dimid(ii,2), &
                                            LEN=n_weights(ii,2)) )
      CALL NC_CHECK( NF90_INQUIRE_DIMENSION(ncid, &
                                            bias_dimid(ii), &
                                            LEN=n_biases(ii)) )
      CALL NC_CHECK( NF90_INQUIRE_DIMENSION(ncid, &
                                            actif_dimid(ii), &
                                            LEN=n_activations(ii)) )
    END DO Layer_DimInq_Loop

    ! Close the file
    CALL NC_CHECK( NF90_CLOSE(ncid) )

  END SUBROUTINE Inquire_NetDim_netCDF


  SUBROUTINE Read_NN_netCDF(nc_filename, net)
    ! Data dictionary
    TYPE(NeuralNet), INTENT(INOUT) :: net
    CHARACTER(LEN=*), INTENT(IN) :: nc_filename
    INTEGER :: ncid
    INTEGER :: ii
    ! var id's
    INTEGER :: lay_varid
    INTEGER :: neur_varid
    INTEGER, DIMENSION(net%n_layers) :: weight_varid
    INTEGER, DIMENSION(net%n_layers) :: bias_varid
    INTEGER, DIMENSION(net%n_layers) ::actif_varid
    ! names
    CHARACTER(LEN=NL) :: WEIGHT_NAME
    CHARACTER(LEN=NL) :: BIAS_NAME
    CHARACTER(LEN=NL) :: ACTIVATION_FUNCTION_NAME

    ! Instructions

    ! Open the file
    CALL NC_CHECK( NF90_OPEN(nc_filename, NF90_NOWRITE, ncid) )
    ! Get the varids of the members
    CALL NC_CHECK( NF90_INQ_VARID(ncid, NLAYERS_NAME, lay_varid) )
    CALL NC_CHECK( NF90_INQ_VARID(ncid, NNEURONS_NAME, neur_varid) )
    Layer_VarInq_Loop: DO ii = 1, net%n_layers
      WRITE(WEIGHT_NAME,"(A,I0)") WEIGHT_NAME_BASE, ii
      WRITE(BIAS_NAME,"(A,I0)") BIAS_NAME_BASE, ii
      WRITE(ACTIVATION_FUNCTION_NAME,"(A,I0)") ACTIVATION_FUNCTION_NAME_BASE, ii
      CALL NC_CHECK( NF90_INQ_VARID(ncid, &
                                  WEIGHT_NAME, &
                                  weight_varid(ii)) )
      CALL NC_CHECK( NF90_INQ_VARID(ncid, &
                                  BIAS_NAME, &
                                  bias_varid(ii)) )
      CALL NC_CHECK( NF90_INQ_VARID(ncid, &
                                  ACTIVATION_FUNCTION_NAME, &
                                  actif_varid(ii)) )
    END DO Layer_VarInq_Loop

    ! Read the data
    CALL NC_CHECK( NF90_GET_VAR(ncid, lay_varid, net%n_layers) )
    CALL NC_CHECK( NF90_GET_VAR(ncid, neur_varid, net%n_neurons) )
    Layer_VarRead_Loop: DO ii = 1, net%n_layers
      WRITE(WEIGHT_NAME,"(A,I0)") WEIGHT_NAME_BASE, ii
      WRITE(BIAS_NAME,"(A,I0)") WEIGHT_NAME_BASE, ii
      WRITE(ACTIVATION_FUNCTION_NAME,"(A,I0)") ACTIVATION_FUNCTION_NAME_BASE, ii
      CALL NC_CHECK( NF90_GET_VAR(ncid, &
                                  weight_varid(ii), &
                                  net%LayerSequence(ii)%Weights) )
      CALL NC_CHECK( NF90_GET_VAR(ncid, &
                                  bias_varid(ii), &
                                  net%LayerSequence(ii)%Bias) )
      CALL NC_CHECK( NF90_GET_VAR(ncid, &
                                  actif_varid(ii), &
                                  net%LayerSequence(ii)%ActivationFunctionName) )
    END DO Layer_VarRead_Loop

    ! Close the file
    CALL NC_CHECK( NF90_CLOSE(ncid) )

  END SUBROUTINE Read_NN_netCDF


  SUBROUTINE Write_NN_netCDF(net,nc_filename)
    ! Data dictionary
    TYPE(NeuralNet), INTENT(IN) :: net
    CHARACTER(LEN=*), INTENT(IN) :: nc_filename
    CHARACTER(LEN=NL) :: WEIGHT_NAME
    CHARACTER(LEN=NL) :: BIAS_NAME
    CHARACTER(LEN=NL) :: ACTIVATION_FUNCTION_NAME
    INTEGER :: ncid
    ! dim id's
    INTEGER :: lay_dimid
    INTEGER :: neur_dimid
    INTEGER, DIMENSION(net%n_layers,2) :: weight_dimid
    INTEGER, DIMENSION(net%n_layers) :: bias_dimid
    INTEGER, DIMENSION(net%n_layers) ::actif_dimid
    ! var id's
    INTEGER :: lay_varid
    INTEGER :: neur_varid
    INTEGER, DIMENSION(net%n_layers) :: weight_varid
    INTEGER, DIMENSION(net%n_layers) :: bias_varid
    INTEGER, DIMENSION(net%n_layers) :: actif_varid
    INTEGER :: ii

    ! Instructions

    ! Create the file.
    CALL NC_CHECK( NF90_CREATE(path = nc_filename, &
                               cmode = NF90_CLOBBER, &
                               ncid = ncid) )
    ! Define the dimensions
    CALL NC_CHECK( NF90_DEF_DIM(ncid, NLAYERS_NAME, 1, lay_dimid) )
    CALL NC_CHECK( NF90_DEF_DIM(ncid, NNEURONS_NAME, net%n_layers, neur_dimid) )
    Layer_DimDef_Loop: DO ii = 1, net%n_layers
      WRITE(WEIGHT_NAME,"(A,I0)") WEIGHT_NAME_BASE, ii
      WRITE(BIAS_NAME,"(A,I0)") BIAS_NAME_BASE, ii
      WRITE(ACTIVATION_FUNCTION_NAME,"(A,I0)") ACTIVATION_FUNCTION_NAME_BASE, ii
      CALL NC_CHECK( NF90_DEF_DIM(ncid, &
                                  WEIGHT_NAME//'_x', &
                                  net%n_neurons(ii), &
                                  weight_dimid(ii,1)) )
      CALL NC_CHECK( NF90_DEF_DIM(ncid, &
                                  WEIGHT_NAME//'_y', &
                                  net%n_neurons(ii), &
                                  weight_dimid(ii,2)) )
      CALL NC_CHECK( NF90_DEF_DIM(ncid, &
                                  BIAS_NAME, &
                                  net%n_neurons(ii), &
                                  bias_dimid(ii)) )
      CALL NC_CHECK( NF90_DEF_DIM(ncid, &
                                  ACTIVATION_FUNCTION_NAME, &
                                  NL, &
                                  actif_dimid(ii)) )
    END DO Layer_DimDef_Loop

    ! Define the variables
    CALL NC_CHECK( NF90_DEF_VAR(ncid, NLAYERS_NAME, NF90_INT, lay_dimid, lay_varid) )
    CALL NC_CHECK( NF90_DEF_VAR(ncid, NNEURONS_NAME, NF90_INT, neur_dimid, neur_varid) )
    Layer_VarDef_Loop: DO ii = 1, net%n_layers
      WRITE(WEIGHT_NAME,"(A,I0)") WEIGHT_NAME_BASE, ii
      WRITE(BIAS_NAME,"(A,I0)") BIAS_NAME_BASE, ii
      WRITE(ACTIVATION_FUNCTION_NAME,"(A,I0)") ACTIVATION_FUNCTION_NAME_BASE, ii
      CALL NC_CHECK( NF90_DEF_VAR(ncid, &
                                  WEIGHT_NAME, &
                                  NF90_REAL, &
                                  weight_dimid(ii,:), &
                                  weight_varid(ii)) )
      CALL NC_CHECK( NF90_DEF_VAR(ncid, &
                                  BIAS_NAME, &
                                  NF90_REAL, &
                                  bias_dimid(ii), &
                                  bias_varid(ii)) )
      CALL NC_CHECK( NF90_DEF_VAR(ncid, &
                                  ACTIVATION_FUNCTION_NAME, &
                                  NF90_CHAR, &
                                  actif_dimid(ii), &
                                  actif_varid(ii)) )
    END DO Layer_VarDef_Loop

    ! End define mode
    CALL NC_CHECK( NF90_ENDDEF(ncid) )

    ! Write data
    CALL NC_CHECK( NF90_PUT_VAR(ncid, lay_varid, net%n_layers) )
    CALL NC_CHECK( NF90_PUT_VAR(ncid, neur_varid, net%n_neurons) )
    Layer_VarPut_Loop: DO ii = 1, net%n_layers
      WRITE(WEIGHT_NAME,"(A,I0)") WEIGHT_NAME_BASE, ii
      WRITE(BIAS_NAME,"(A,I0)") BIAS_NAME_BASE, ii
      WRITE(ACTIVATION_FUNCTION_NAME,"(A,I0)") ACTIVATION_FUNCTION_NAME_BASE, ii
      CALL NC_CHECK( NF90_PUT_VAR(ncid, &
                                  weight_varid(ii), &
                                  net%LayerSequence(ii)%Weights) )
      CALL NC_CHECK( NF90_PUT_VAR(ncid, &
                                  bias_varid(ii), &
                                  net%LayerSequence(ii)%Bias) )
      CALL NC_CHECK( NF90_PUT_VAR(ncid, &
                                  actif_varid(ii), &
                                  net%LayerSequence(ii)%ActivationFunctionName) )
    END DO Layer_VarPut_Loop

    ! Close the file
    CALL NC_CHECK( NF90_CLOSE(ncid) )

  END SUBROUTINE

  SUBROUTINE NC_CHECK(status)
    INTEGER, INTENT(IN) :: status
    IF(status /= NF90_NOERR) THEN 
      WRITE(*,*) TRIM(NF90_STRERROR(status))
      STOP "NetCDF I/O stopped"
    END IF 
  END SUBROUTINE NC_CHECK

END MODULE NN_netCDF_IO

