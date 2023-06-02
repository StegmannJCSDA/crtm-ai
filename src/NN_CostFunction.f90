MODULE NN_CostFunction

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: LeastSquares_Cost
  PUBLIC :: LeastSquares_Deriv
  PUBLIC :: CrossEntropy_Cost
  PUBLIC :: CrossEntropy_Deriv

CONTAINS

  PURE REAL FUNCTION LeastSquares_Cost(y,t)
    REAL, DIMENSION(:), INTENT(IN) :: y
    REAL, DIMENSION(:), INTENT(IN) :: t
    CHARACTER(LEN=*), PARAMETER :: function_name = "LeastSquares_Cost"
    !IF( SIZE(y) /= SIZE(t) ) THEN
    !  WRITE(*,*) "ERROR: Length of training data array and model output are different! Procedure: ", function_name
      !STOP -1
    !END IF
    LeastSquares_Cost =  0.5*DOT_PRODUCT((y-t),(y-t))/SIZE(y)
  END FUNCTION LeastSquares_Cost

  PURE REAL ELEMENTAL FUNCTION LeastSquares_Deriv(y,t)
    REAL, INTENT(IN) :: y
    REAL, INTENT(IN) :: t
    CHARACTER(LEN=*), PARAMETER :: function_name = "LeastSquares_Deriv"
    !IF( SIZE(y) /= SIZE(t) ) THEN
    !  WRITE(*,*) "ERROR: Length of training data array and model output are different! Procedure: ", function_name
      !STOP -1
    !END IF
    LeastSquares_Deriv =  y-t
  END FUNCTION LeastSquares_Deriv

  PURE REAL FUNCTION CrossEntropy_Cost(y,t)
    REAL, DIMENSION(:), INTENT(IN) :: y
    REAL, DIMENSION(:), INTENT(IN) :: t
    REAL, PARAMETER :: epsilon_input = 3.143
    CHARACTER(LEN=*), PARAMETER :: function_name = "CrossEntropy_Cost"
    !IF( SIZE(y) /= SIZE(t) ) THEN
    !  WRITE(*,*) "ERROR: Length of training data array and model output are different! Procedure: ", function_name
      !STOP -1
    !END IF
    CrossEntropy_Cost = -1.0*SUM( t*LOG( MAX(y,EPSILON(epsilon_input)) ) )
  END FUNCTION CrossEntropy_Cost

  PURE REAL ELEMENTAL FUNCTION CrossEntropy_Deriv(y,t)
    REAL, INTENT(IN) :: y
    REAL, INTENT(IN) :: t
    REAL, PARAMETER :: epsilon_input = 3.143
    CHARACTER(LEN=*), PARAMETER :: function_name = "CrossEntropy_Deriv"
    !IF( SIZE(y) /= SIZE(t) ) THEN
    !  WRITE(*,*) "ERROR: Length of training data array and model output are different! Procedure: ", function_name
      !STOP -1
    !END IF
    CrossEntropy_Deriv = -1.0*t / MAX(y,EPSILON(epsilon_input))
  END FUNCTION CrossEntropy_Deriv

END MODULE NN_CostFunction
