MODULE NN_Layer_Define

  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: dp => real64
  USE NN_ActivationFunctions, ONLY: SIGMOID, RELU

  ! -----------------------
  ! Disable implicit typing
  ! -----------------------  
  IMPLICIT NONE

  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE

  INTERFACE
    FUNCTION activation_function(x)
      REAL :: activation_function
      REAL, INTENT(IN) :: x
    END FUNCTION activation_function
  END INTERFACE

  TYPE, PUBLIC :: Layer
    REAL, ALLOCATABLE :: Input(:)
    REAL, ALLOCATABLE :: Output(:)
    REAL, ALLOCATABLE :: Weights(:,:)
    REAL, ALLOCATABLE :: delta(:)
    REAL, ALLOCATABLE :: Bias(:)
    REAL, ALLOCATABLE :: Weight_gradient(:,:)            ! Intermediate for backpropagation
    REAL, ALLOCATABLE :: Cumulative_Weight_gradient(:,:) ! For batch gradient descent
    REAL, ALLOCATABLE :: Bias_gradient(:)                ! Intermediate for backpropagation
    REAL, ALLOCATABLE :: Cumulative_Bias_gradient(:)     ! For batch gradient descent
    CHARACTER(LEN=12) :: ActivationFunctionName
    LOGICAL :: is_allocated = .FALSE.
    LOGICAL :: gradient_is_allocated = .FALSE.
    !PROCEDURE(activation_function), POINTER, NOPASS :: ActivationFunction => NULL()
  CONTAINS
    PROCEDURE, PUBLIC, PASS :: Layer_Create
    FINAL                   :: Layer_Destroy
    PROCEDURE, PUBLIC, PASS :: ActivationFunction
    PROCEDURE, PUBLIC, PASS :: Layer_Init_Gradients
    PROCEDURE, PASS         :: FlushGradients
  END TYPE Layer

CONTAINS

  !**************************************
  !
  ! CONSTRUCTOR
  !
  !**************************************
  SUBROUTINE Layer_Create(this,m,n,activ_func_name)
    ! Data dictionary
    INTEGER, INTENT(IN) :: m ! Size of the previous layer
    INTEGER, INTENT(IN) :: n ! Size of the current layer
    CLASS(Layer), INTENT(INOUT) :: this
    CHARACTER(*), INTENT(IN) :: activ_func_name
    ! Allocate layer data
    IF(.NOT. this%is_allocated ) THEN
      ALLOCATE(this%Input(m),  &
               this%Output(n), &
               this%Bias(n), & 
               this%Weights(n,m) )
      this%is_allocated = .TRUE.
    END IF
    this%ActivationFunctionName = activ_func_name
    !SELECT CASE(activ_func_name)
    !CASE('sigmoid')
    !    this%ActivationFunction => SIGMOID
    !CASE('relu')
    !    this%ActivationFunction => RELU
    !END SELECT
    WRITE(*,*) 'Creating Layer object with n_neurons #',n
  END SUBROUTINE Layer_Create

  !*********************************
  !
  ! FINALIZER
  !
  !*********************************
  SUBROUTINE Layer_Destroy(this)
    ! Data dictionary
    TYPE(Layer), INTENT(INOUT) :: this
    WRITE(*,*) 'Running Layer Finalizer.'
    ! Deallocate layer data
    IF( this%is_allocated ) THEN
      DEALLOCATE(this%Input, &
                 this%Output,&
                 this%Bias,  &
                 this%Weights)
      this%is_allocated = .FALSE.
    END IF
    !NULLIFY(this%ActivationFunction)
    WRITE(*,*) 'Finished Layer Finalizer.'
  END SUBROUTINE Layer_Destroy

  SUBROUTINE Layer_Init_Gradients(this,m,n)
    ! Data dictionary
    INTEGER, INTENT(IN) :: m ! Size of the previous layer
    INTEGER, INTENT(IN) :: n ! Size of the current layer
    CLASS(Layer), INTENT(INOUT) :: this
    ! Allocate layer data
    IF(.NOT. this%gradient_is_allocated ) THEN
      ALLOCATE( this%Weight_gradient(n,m) )
      ALLOCATE( this%Cumulative_Weight_gradient(n,m) )
      ALLOCATE( this%Bias_gradient(n) )
      ALLOCATE( this%Cumulative_Bias_gradient(n) )
      ALLOCATE( this%delta(n) )
      this%gradient_is_allocated = .TRUE.
    END IF
    WRITE(*,*) 'Initializing layer gradients.'
  END SUBROUTINE Layer_Init_Gradients

  PURE ELEMENTAL FUNCTION ActivationFunction(this,x) RESULT(y)
    ! 
    ! Activation function wrapper.
    !
    ! Data dictionary
    CLASS(Layer), INTENT(IN) :: this
    REAL, INTENT(in) :: x
    REAL :: y
    SELECT CASE(this%ActivationFunctionName)
    CASE('sigmoid')
      y = SIGMOID(x)
    CASE('relu')
      y = RELU(x)
    END SELECT
    RETURN
  END FUNCTION ActivationFunction

  SUBROUTINE FlushGradients(this)
    ! 
    ! Free up memory used by the 
    ! gradients of the neural net
    ! if they are not needed.
    !
    CLASS(Layer), INTENT(INOUT) :: this
    IF( this%gradient_is_allocated ) THEN
      DEALLOCATE(this%Weight_gradient)
      DEALLOCATE(this%Cumulative_Weight_gradient)
      DEALLOCATE(this%Bias_gradient)
      DEALLOCATE(this%Cumulative_Bias_gradient)
      DEALLOCATE(this%delta)
    END IF
    this%gradient_is_allocated = .FALSE.
    RETURN
  END SUBROUTINE FlushGradients

END MODULE NN_Layer_Define
