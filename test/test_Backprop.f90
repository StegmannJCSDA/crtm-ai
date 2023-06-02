PROGRAM TEST_BP
  
  USE NeuralNet_Define
  USE NN_TrainingData, ONLY: Training_data, &
                             Training_data_Create, &
                             Training_data_Destroy, &
                             Training_data_Set
  
  IMPLICIT NONE

  ! Data Dictionary:
  ! Neural network data
  TYPE(NeuralNet) :: net
  INTEGER, DIMENSION(4), PARAMETER :: neurons = (/1,2,2,1/)
  REAL, DIMENSION(1), PARAMETER :: test_vec = (/1./)
  REAL, DIMENSION(neurons(SIZE(neurons))) :: output_vec
  REAL, DIMENSION(neurons(SIZE(neurons)),10000) :: output_vec_final
  CHARACTER(*), PARAMETER :: test_activ_fn = 'relu'
  CHARACTER(*), PARAMETER :: test_cost_fn = 'LeastSquares'
  ! Training data
  TYPE(Training_data) :: dat
  INTEGER :: n  
  CHARACTER(LEN=20) :: filename
  CHARACTER(LEN=20) :: filename_out
  INTEGER :: ii
  INTEGER :: ios
  REAL, DIMENSION(:,:), ALLOCATABLE :: x
  REAL, DIMENSION(:,:), ALLOCATABLE :: sin_x
  ! Training parameters
  REAL,    PARAMETER :: learning_rate = 0.01
  INTEGER, PARAMETER :: BatchSize = 10000
  INTEGER, PARAMETER :: NumIterations = 400

  ! Instructions:
  WRITE(*,*) "This is a test drive for the neural network module."

  !====================================
  !
  ! Create neural network for training
  !
  !====================================
  WRITE(*,*) "Creating neural network."
  CALL net%NeuralNet_Create(neurons, &
                            SIZE(test_vec), &
                            test_activ_fn, &
                            test_cost_fn, &
                            training = .TRUE.)
  WRITE(*,*) net%n_neurons
  CALL net%Init()
  WRITE(*,*) "Network created."

  !==================================
  !
  ! Read square function training data
  !
  !==================================
  WRITE(*,*) "Reading training data from file."
  n = 10000
  !BatchSize = n
  CALL dat%Training_data_Create(n,1,1)
  WRITE(*,*) "Dataset size: ", dat%dataset_size
 
  ALLOCATE(x(n,1), sin_x(n,1))

  filename = 'square_values.txt'

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

  WRITE(*,*) "Training data read."

  !==================================
  !
  ! Start training neural network.
  !
  !==================================
  WRITE(*,*) "Start training neural network."
  CALL net%Batch_Gradient_Descent(learning_rate, &
                                  BatchSize, &
                                  NumIterations, &
                                  dat)
  WRITE(*,*) "Training completed."
  DO ii = 1, n
    output_vec_final(:,ii) = net%ForwardPass(x(ii,1))
  END DO 
  WRITE(*,*) net%LayerSequence(3)%Weights
  filename_out = "results.txt"
  OPEN(UNIT=20, FILE=filename_out, STATUS="REPLACE", ACTION="WRITE", IOSTAT=ios)
  DO ii = 1, n
    WRITE(20, *, IOSTAT=ios) x(ii,1), output_vec_final(:,ii)
    IF (ios /= 0) THEN
      WRITE(*,*) "Error: Could not write data to file ", filename
      STOP -1
    END IF 
  END DO 
  CLOSE(20)
  DEALLOCATE(x, sin_x)

  WRITE(*,*) "Finished test."
   
END PROGRAM TEST_BP
