PROGRAM TEST
  
  USE NeuralNet_Define
  
  IMPLICIT NONE

  ! Data Dictionary
  !TYPE(NeuralNet) :: net
  TYPE(NeuralNet), POINTER :: net
  INTEGER, DIMENSION(4), PARAMETER :: neurons = (/2,4,3,3/)
  REAL, DIMENSION(2), PARAMETER :: test_vec = (/1., 1./)
  REAL, DIMENSION(neurons(SIZE(neurons))) :: output_vec
  CHARACTER(*), PARAMETER :: test_activ_fn = 'relu'
  CHARACTER(*), PARAMETER :: test_cost_fn = 'LeastSquares'
  
  ! Instructions:
  WRITE(*,*) "This is a test drive for the neural network module."
  ALLOCATE(net)  
  CALL net%NeuralNet_Create(neurons, &
                            SIZE(test_vec), &
                            test_activ_fn, &
                            test_cost_fn)
  WRITE(*,*) net%n_neurons
  CALL net%Init()
  output_vec = net%ForwardPass(test_vec)
  WRITE(*,*) output_vec
  CALL net%BackPropagation(output_vec)
  WRITE(*,*) net%LayerSequence(1)%Weights
  !CALL net%NeuralNet_Destroy()
  DEALLOCATE(net)
  NULLIFY(net)
  WRITE(*,*) "Finished test."
   
END PROGRAM TEST
