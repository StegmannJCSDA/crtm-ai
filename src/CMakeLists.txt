# bar library
SET(NEURAL_src ${SRCNEURAL}/NeuralNet_Define.f90
               ${SRCNEURAL}/NN_Layer_Define.f90
               ${SRCNEURAL}/NN_ActivationFunctions.f90
               ${SRCNEURAL}/NN_netCDF_IO.f90
               ${SRCNEURAL}/NN_TensorFlow_IO.f90
               ${SRCNEURAL}/NN_TrainingData.f90
               )
ADD_LIBRARY(${NEURALLIB} STATIC ${NEURAL_src})
# Add dependencies
#TARGET_LINK_LIBRARIES(${NEURALLIB} PRIVATE HDF5::HDF5)
TARGET_LINK_LIBRARIES(${NEURALLIB} netCDF::netcdf HDF5::HDF5)
