MODULE NN_TrainingData

  IMPLICIT NONE

  TYPE :: Training_data
    REAL, DIMENSION(:), ALLOCATABLE :: x
    REAL, DIMENSION(:), ALLOCATABLE :: y
    INTEGER :: dataset_size = 0
    LOGICAL :: is_allocated = .FALSE.
  CONTAINS
    PROCEDURE, PUBLIC, PASS :: Training_data_Create
    FINAL                   :: Training_data_Destroy
    PROCEDURE, PUBLIC, PASS :: Training_data_Set
  END TYPE Training_data

CONTAINS

  !**************************************
  !
  ! CONSTRUCTOR
  !
  !**************************************
  SUBROUTINE Training_data_Create(this,n)
    CLASS(Training_data), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: n
    this%dataset_size = n
    IF( .NOT. this%is_allocated ) THEN
      ALLOCATE( this%x(n) )
      ALLOCATE( this%y(n) )
      this%dataset_size = n
      this%is_allocated = .TRUE.
    END IF
    RETURN
  END SUBROUTINE Training_data_Create

  !*********************************
  !
  ! FINALIZER
  !
  !*********************************
  SUBROUTINE Training_data_Destroy(this)
    TYPE(Training_data), INTENT(INOUT) :: this
    IF(this%is_allocated) THEN
      DEALLOCATE(this%x)
      DEALLOCATE(this%y)
    END IF
    this%dataset_size = 0
    this%is_allocated = .FALSE.
    RETURN
  END SUBROUTINE Training_data_Destroy
    
  SUBROUTINE Training_data_set(this,a,b)
    CLASS(Training_data), INTENT(INOUT) :: this
    REAL, DIMENSION(:), INTENT(IN) :: a
    REAL, DIMENSION(:), INTENT(IN) :: b
    this%x = a
    this%y = b
    RETURN
  END SUBROUTINE Training_data_set

END MODULE NN_TrainingData
