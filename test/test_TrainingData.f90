PROGRAM TEST
  
  USE NN_TrainingData, ONLY: Training_data, &
                             Training_data_Create, &
                             Training_data_Destroy, &
                             Training_data_Set
  
  IMPLICIT NONE

  ! Data Dictionary
  !TYPE(NeuralNet) :: net
  TYPE(Training_data) :: dat
  INTEGER :: n  
  CHARACTER(LEN=20) :: filename
  INTEGER :: ii, ios
  REAL, DIMENSION(:,:), ALLOCATABLE :: x
  REAL, DIMENSION(:,:), ALLOCATABLE :: sin_x
  ! Instructions:
  WRITE(*,*) "This is a test drive for the training data module."
  n = 100
  CALL dat%Training_data_Create(n,1,1)
  WRITE(*,*) "Dataset size: ", dat%dataset_size
 
  ALLOCATE(x(n,1), sin_x(n,1))

  filename = 'sin_values.txt'

  ! Open the file for reading
  OPEN(UNIT=10, FILE=filename, STATUS="OLD", ACTION="READ", IOSTAT=ios)
  IF (ios /= 0) THEN
    WRITE(*,*) "Error: Could not open file ", filename
    STOP
  END IF 

  ! Read the contents of the file
  ios = 0
  DO ii = 1, n
    READ(10, *, IOSTAT=ios) x(ii,1), sin_x(ii,1)
    IF (ios /= 0) THEN
      WRITE(*,*) "Error: Could not read data from file ", filename
      STOP -1
    END IF 
  END DO 

  ! Close the file
  CLOSE(10)

  CALL dat%Training_data_Set(x,sin_x)

  DEALLOCATE(x, sin_x)

  WRITE(*,*) "Finished test."
   
END PROGRAM TEST
