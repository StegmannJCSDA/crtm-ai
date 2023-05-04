MODULE NeuralNet_Define

  USE NN_Layer_Define
  USE NN_TrainingData, ONLY: Training_data
  USE NN_ActivationFunctions, ONLY: SIGMOID, RELU
  
  IMPLICIT NONE

  PRIVATE

  !**************************************
  !
  ! PARAMETERS
  !
  !**************************************
  
  REAL, PARAMETER :: eps = 1.e-10
  INTEGER, PARAMETER :: MaxIter = 200

  !**************************************
  !
  ! TYPE DECLARATION
  !
  !**************************************

  TYPE, PUBLIC :: NeuralNet
    INTEGER :: n_layers
    INTEGER, ALLOCATABLE :: n_neurons(:)         ! DIMENSION(n_layers)
                                                 ! The number of neurons in each Layer.
    TYPE(Layer), ALLOCATABLE :: LayerSequence(:) ! DIMENSION(n_layers)
                                                 ! The list of Layer objects of the net.
    LOGICAL :: is_allocated = .FALSE.
    !INTEGER :: gradient_size 
  CONTAINS
    PROCEDURE, PUBLIC, PASS :: NeuralNet_Create  ! Constructor
    FINAL                   :: NeuralNet_Destroy ! Destructor
    PROCEDURE, PUBLIC, PASS :: Init
    PROCEDURE, PUBLIC, PASS :: Init_Gradients 
    PROCEDURE, PUBLIC, PASS :: Clear_Gradients
    PROCEDURE, PUBLIC, PASS :: Batch_Gradient_Descent
    PROCEDURE, PUBLIC, PASS :: ForwardPass
    PROCEDURE, PUBLIC, PASS :: BackPropagation
  END TYPE NeuralNet

CONTAINS
  

  !**************************************
  !
  ! CONSTRUCTOR
  !
  !**************************************
  SUBROUTINE NeuralNet_Create(this,sizes,input_size,activ_fn)
    CLASS(NeuralNet) :: this
    INTEGER, INTENT(IN) :: sizes(:)
    INTEGER, INTENT(IN) :: input_size
    CHARACTER(LEN=*), INTENT(IN) :: activ_fn
    INTEGER :: ii
    INTEGER :: alloc_stat
    ! Allocate neuron size storage
    this%n_layers = SIZE(sizes)
    IF(.NOT. ALLOCATED(this%n_neurons)) THEN
      ALLOCATE(this%n_neurons(this%n_layers), STAT=alloc_stat)
    END IF
    this%n_neurons = sizes
    ! Allocate Layer data structure
    IF(.NOT. ALLOCATED(this%LayerSequence)) THEN
      ALLOCATE(this%LayerSequence(this%n_layers), STAT=alloc_stat)
    END IF
    CALL this%LayerSequence(1)%Layer_Create( &
            input_size, &
            this%n_neurons(1), &
            activ_fn)
    LayerAllocLoop: DO ii = 2, this%n_layers
      CALL this%LayerSequence(ii)%Layer_Create( &
                this%n_neurons(ii-1), &
                this%n_neurons(ii), &
                activ_fn)
    END DO LayerAllocLoop
    this%is_allocated = .TRUE.
    RETURN
  END SUBROUTINE NeuralNet_Create


  !*********************************
  !
  ! FINALIZER
  !
  !*********************************
  SUBROUTINE NeuralNet_Destroy(this)
    TYPE(NeuralNet) :: this
    !INTEGER :: ii
    WRITE(*,*) 'Running NeuralNet Finalizer.' 
    this%is_allocated = .FALSE.
    IF( ALLOCATED(this%n_neurons) ) THEN
      DEALLOCATE(this%n_neurons)
    END IF
    WRITE(*,*) 'Deallocating Layer Sequence.'
    !DO ii = 1, this%n_layers
    !  CALL this%LayerSequence(ii)%Layer_Destroy()
    !END DO
    IF( ALLOCATED(this%LayerSequence) ) THEN
      DEALLOCATE(this%LayerSequence)
    END IF
    WRITE(*,*) 'Finished NeuralNet Finalizer.'
    RETURN
  END SUBROUTINE NeuralNet_Destroy

  !*********************************
  !
  ! Initialize Weights and Biases
  !
  !*********************************
  SUBROUTINE Init(this)
    CLASS(NeuralNet) :: this
    INTEGER, DIMENSION(1), PARAMETER :: SEED = (/ 142857 /)
    INTEGER :: ii 
    !CALL RANDOM_SEED(PUT=SEED)    
    LayerInitLoop: DO ii = 1, this%n_layers
      ! Initialize Layer weight matrices
      CALL RANDOM_NUMBER(this%LayerSequence(ii)%Weights)
      ! Initialize Layer biases
      CALL RANDOM_NUMBER(this%LayerSequence(ii)%Bias)
    END DO LayerInitLoop
    WRITE(*,*) 'Random intialization of network Layers in progress.'
    RETURN
  END SUBROUTINE Init 

  !************************************
  !
  ! Initialize Gradients for Training
  !
  !************************************
  SUBROUTINE Init_Gradients(this, input_size)
    CLASS(NeuralNet), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: input_size
    INTEGER :: ii 
    CALL this%LayerSequence(1)%Layer_Init_Gradients( &
            input_size, &
            this%n_neurons(1))
    LayerAllocLoop: DO ii = 2, this%n_layers
      CALL this%LayerSequence(ii)%Layer_Init_Gradients( &
                this%n_neurons(ii-1), &
                this%n_neurons(ii) )
    END DO LayerAllocLoop
    ! ! Compute the size of the network weight gradient
    ! ! for training.
    ! this%gradient_size = 0
    ! GradientSizeLoop: DO ii = 1, this%n_layers
    !   this%gradient_size = this%gradient_size + this%n_neurons(ii) * &
    !        SIZE(this%LayerSequence(ii)%Weights,1) * &
    !        SIZE(this%LayerSequence(ii)%Weights,2)
    ! END DO GradientSizeLoop
    WRITE(*,*) 'Initialization of network gradients in progress.'
    RETURN
  END SUBROUTINE Init_Gradients

  !************************************
  !
  ! Clear Gradients
  !
  !************************************
  SUBROUTINE Clear_Gradients(this)
    CLASS(NeuralNet) :: this
    INTEGER :: ii 
    LayerAllocLoop: DO ii = 1, this%n_layers
      CALL this%LayerSequence(ii)%FlushGradients()
    END DO LayerAllocLoop
    WRITE(*,*) 'Deallocation of network gradients in progress.'
    RETURN
  END SUBROUTINE Clear_Gradients

  !****************************************************
  !
  ! Stochastic Gradient Descent for model optimization
  !
  !****************************************************
  SUBROUTINE Batch_Gradient_Descent(this, &
                                         learning_rate, &
                                         !NumBatches, &
                                         BatchSize, &
                                         NumIterations, &
                                         training_datum)
    CLASS(NeuralNet) :: this
    REAL, INTENT(IN) :: learning_rate
    !INTEGER, INTENT(IN) :: NumBatches
    INTEGER, INTENT(IN) :: BatchSize
    INTEGER, INTENT(IN) :: NumIterations
    TYPE(Training_data), INTENT(IN) :: training_datum
    INTEGER :: ii
    INTEGER :: jj
    INTEGER :: kk
    REAL :: u 
    INTEGER :: randint
    REAL, DIMENSION(this%n_neurons(this%n_layers)) :: model_output

    ! Step 1: Initialize gradients randomly
    CALL this%Init_Gradients( SIZE(training_datum%x) )
    ! Step 2: Iterate over the dataset towards 
    !         (hopefully) convergence
    SampleLoop: DO ii = 1, NumIterations
      ! Step 3: Zero out the cost function gradient
      ZeroOutGradientsLoop: DO kk = 1, this%n_layers
        this%LayerSequence(kk)%Cumulative_Weight_gradient(:,:) = 0.0
        this%LayerSequence(kk)%Cumulative_Bias_gradient(:) = 0.0
      END DO ZeroOutGradientsLoop
      ! Step 4: Loop over batch members
      BatchLoop: DO jj = 1, BatchSize
        ! Step 5: Select random input from training data
        CALL RANDOM_NUMBER(u)
        randint = FLOOR((training_datum%dataset_size+1)*u)
        ! Step 6: Run a forward pass and save results
        model_output = this%ForwardPass( training_datum%x(randint) )
        ! Step 7: Backpropagation of the gradients
        CALL this%BackPropagation( training_datum%y(randint) )
          ! TBD: Compute Cost function
          ! TBD: Print Cost function
        ! Step 8: Add the gradients
        AddGradientsLoop: DO kk = 1, this%n_layers
          this%LayerSequence(kk)%Cumulative_Weight_gradient = &
                             this%LayerSequence(kk)%Cumulative_Weight_gradient &
                             + this%LayerSequence(kk)%Weight_gradient
          this%LayerSequence(kk)%Cumulative_Bias_gradient = &
                             this%LayerSequence(kk)%Cumulative_Bias_gradient &
                             + this%LayerSequence(kk)%Bias_gradient
        END DO AddGradientsLoop
      END DO BatchLoop
      ! Step 9: One gradient descent step
      GradientUpdateLoop: DO kk = 1, this%n_layers
        this%LayerSequence(kk)%Weights = this%LayerSequence(kk)%Weights &
                                       - learning_rate/BatchSize &
                                       *this%LayerSequence(kk)%Cumulative_Weight_gradient
        this%LayerSequence(kk)%Bias = this%LayerSequence(kk)%Bias &
                                    - learning_rate/BatchSize &
                                    *this%LayerSequence(kk)%Cumulative_Bias_gradient
      END DO GradientUpdateLoop
    END DO SampleLoop
    RETURN
  END SUBROUTINE Batch_Gradient_Descent

  !*********************************
  !
  ! Forward Pass (Inference step)
  !
  !*********************************
  FUNCTION ForwardPass(this,input_vec) RESULT(output_vec)
    CLASS(NeuralNet), INTENT(INOUT) :: this
    REAL, DIMENSION(this%n_neurons(1)), INTENT(IN) :: input_vec
    REAL, DIMENSION(this%n_neurons(SIZE(this%n_neurons))) :: output_vec
    INTEGER :: ii
    ASSOCIATE(Layer => this%LayerSequence)
    WRITE(*,*) 'Computing input layer.'
    Layer(1)%Output = &
            Layer(1)%ActivationFunction( & 
                 MATMUL(Layer(1)%Weights,input_vec) ) 
                     !+ Layer(1)%Bias) ! No bias for now
    WRITE(*,*) 'Entering Layer evaluation loop.'
    EvaluationLoop: DO ii = 2, this%n_layers
      WRITE(*,*) 'Layer #', ii
      Layer(ii)%Output = &
           Layer(ii)%ActivationFunction( &
                MATMUL(Layer(ii)%Weights,Layer(ii-1)%Output) )
                    ! + Layer(ii)%Bias)
    END DO EvaluationLoop
    WRITE(*,*) 'Assigning network output.'
    output_vec = Layer(this%n_layers)%Output
    END ASSOCIATE
    RETURN
  END FUNCTION ForwardPass

  !*********************************
  !
  ! Backpropagation algorithm
  !
  !*********************************
  SUBROUTINE  BackPropagation(this,training_data)
    ! 
    ! Use backpropagation to compute the gradient of the neural network
    ! output w.r.t. the neural network weight matrices.
    !
    CLASS(NeuralNet), INTENT(INOUT) :: this
    REAL, DIMENSION(this%n_neurons(this%n_layers)), INTENT(IN) :: training_data
    INTEGER :: ii
    ASSOCIATE(Layer => this%LayerSequence)
      ! Gradient of output layer N:
      !
      ! d_N = dL/dy * df/dz
      !   = (y_N - t) * (y_N*(1 - y__N))
      !
      Layer(this%n_layers)%delta = ( Layer(this%n_layers)%Output - training_data) &
                             *Layer(this%n_layers)%Output &
                             *( 1. - Layer(this%n_layers)%Output )
      !
      ! Bias gradients in output layer N:
      ! 
      ! dB_N = d_N
      ! 
      Layer(this%n_layers)%Bias_gradient = Layer(this%n_layers)%delta
      !
      ! Weight gradients in output layer N:
      !
      ! dW_N = y_N * d_N
      !
      Layer(this%n_layers)%Weight_gradient = MATMUL( RESHAPE(Layer(this%n_layers)%Output,(/SIZE(Layer(this%n_layers)%Output),1/)), &
                          RESHAPE(Layer(this%n_layers)%delta,(/1,SIZE(Layer(this%n_layers)%delta)/)) )
      !
      ! Inner layers ii recursion loop:
      ! 
      ! Internal Error term:
      !         ___
      !         \
      !          \
      !  d_i = ( /  W^T_(i+1) * d_(i+1) ) * ( y_i * (1 - y_i) )
      !         /__ 
      !        
      ! Internal Bias gradient:
      ! dB_i = d_i
      ! 
      ! Internal Weight gradients:
      ! dW_i = y_i * d_i
      ! 
      LayerRecursion: DO ii = (this%n_layers-1), 1, -1
        WRITE(*,*) 'sha', SHAPE(TRANSPOSE(Layer(ii)%Weights)), SHAPE(Layer(ii+1)%delta )
        Layer(ii)%delta = SUM( MATMUL( TRANSPOSE(Layer(ii+1)%Weights), Layer(ii+1)%delta ) ) &
                    *Layer(ii)%Output*( 1. - Layer(ii)%Output )
        Layer(ii)%Bias_gradient = Layer(ii)%delta
        Layer(ii)%Weight_gradient = MATMUL( RESHAPE(Layer(ii)%Output,(/SIZE(Layer(ii)%Output),1/)), &
                 RESHAPE(Layer(ii)%delta,(/1,SIZE(Layer(ii)%delta)/)) )
      END DO LayerRecursion
    
    END ASSOCIATE
    RETURN
  END SUBROUTINE BackPropagation


END MODULE NeuralNet_Define
