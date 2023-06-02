MODULE NN_ActivationFunctions

  ! -----------------------
  ! Disable implicit typing
  ! -----------------------  
  IMPLICIT NONE

  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Public procedures

  PUBLIC :: SIGMOID
  PUBLIC :: SIGMOID_DERIV
  !PUBLIC :: SIGMOID_AD
  PUBLIC :: RELU
  PUBLIC :: RELU_DERIV

CONTAINS

  PURE ELEMENTAL FUNCTION SIGMOID(x) RESULT(y)
    REAL, INTENT(IN) :: x
    REAL :: y
    y = 1./(1.0 + EXP(-x))
    RETURN
  END FUNCTION SIGMOID
  
  PURE ELEMENTAL FUNCTION SIGMOID_DERIV(x) RESULT(y)
    REAL, INTENT(IN) :: x
    REAL :: y
    y = SIGMOID(x)*(1.0 - SIGMOID(x))
    !y = x*(1.0 - x)
    RETURN
  END FUNCTION SIGMOID_DERIV

  PURE ELEMENTAL FUNCTION RELU(x) RESULT(y)
    REAL, INTENT(in) :: x
    REAL :: y
    y = MAX(0.0, x)
    RETURN
  END FUNCTION RELU 

  PURE ELEMENTAL FUNCTION RELU_DERIV(x) RESULT(y)
    REAL, INTENT(IN) :: x
    REAL :: y
    IF(x > 0) THEN
      y = 1.0
    ELSE
      y = 0.0
    END IF 
    RETURN
  END FUNCTION RELU_DERIV

END MODULE NN_ActivationFunctions
