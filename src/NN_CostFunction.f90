MODULE NN_CostFunction

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: LeastSquares_Cost
  PUBLIC :: LeastSquares_Deriv

CONTAINS

  PURE ELEMENTAL FUNCTION LeastSquares_Cost(y,t)
    REAL, INTENT(IN) :: y
    REAL, INTENT(IN) :: t
    RETURN 0.5*(y-t)**2
  END FUNCTION

  PURE ELEMENTAL FUNCTION LeastSquares_Deriv(y,t)
    REAL, INTENT(IN) :: y
    REAL, INTENT(IN) :: t
    RETURN y-t
  END FUNCTION 

END MODULE NN_CostFunction
