########################################
# Set up how to compile the source files
########################################

# Add the source files
SET(TEST_src ${SRCTEST}/test_NetConstructor.f90)
SET(TESTDT_src ${SRCTEST}/test_TrainingData.f90)
SET(TESTBP_src ${SRCTEST}/test_Backprop.f90)

# Define the executable in terms of the source files
ADD_EXECUTABLE(${TESTEXE} ${TEST_src})
ADD_EXECUTABLE(${TESTDATAEXE} ${TESTDT_src})
ADD_EXECUTABLE(${TESTBACKPROPEXE} ${TESTBP_src})

#####################################################
# Add the needed libraries and special compiler flags
#####################################################

# This links TESTEXE to the NEURALLIB library
TARGET_LINK_LIBRARIES(${TESTEXE} ${NEURALLIB})
TARGET_LINK_LIBRARIES(${TESTEXE} HDF5::HDF5)
TARGET_LINK_LIBRARIES(${TESTEXE} netCDF::netcdf)

# This links TRAININGDATAEXE to the NEURALLIB library
TARGET_LINK_LIBRARIES(${TESTDATAEXE} ${NEURALLIB})
TARGET_LINK_LIBRARIES(${TESTDATAEXE} HDF5::HDF5)
TARGET_LINK_LIBRARIES(${TESTDATAEXE} netCDF::netcdf)

# This links TESTBACKPROPEXE to the NEURALLIB library
TARGET_LINK_LIBRARIES(${TESTBACKPROPEXE} ${NEURALLIB})
TARGET_LINK_LIBRARIES(${TESTBACKPROPEXE} HDF5::HDF5)
TARGET_LINK_LIBRARIES(${TESTBACKPROPEXE} netCDF::netcdf)


ADD_TEST(NAME ConstructorTest COMMAND ${TESTEXE})
ADD_TEST(NAME CreateTrainingData COMMAND ${SRCTEST}/test_data.py)
ADD_TEST(NAME TrainingDataTest COMMAND ${TESTDATAEXE})
ADD_TEST(NAME BackPropagationTest COMMAND ${TESTBACKPROPEXE})


# Uncomment if you need to link to BLAS and LAPACK
#TARGET_LINK_LIBRARIES(${FOOEXE} ${BLAS_LIBRARIES}
#                                ${LAPACK_LIBRARIES}
#                                ${CMAKE_THREAD_LIBS_INIT})

# Uncomment if you have parallization
#IF(USE_OPENMP)
#    SET_TARGET_PROPERTIES(${FOOEXE} PROPERTIES
#                          COMPILE_FLAGS "${OpenMP_Fortran_FLAGS}"
#                          LINK_FLAGS "${OpenMP_Fortran_FLAGS}")
#ELSEIF(USE_MPI)
#    SET_TARGET_PROPERTIES(${FOOEXE} PROPERTIES
#                          COMPILE_FLAGS "${MPI_Fortran_COMPILE_FLAGS}"
#                          LINK_FLAGS "${MPI_Fortran_LINK_FLAGS}")
#    INCLUDE_DIRECTORIES(${MPI_Fortran_INCLUDE_PATH})
#    TARGET_LINK_LIBRARIES(${FOOEXE} ${MPI_Fortran_LIBRARIES})
#ENDIF(USE_OPENMP)

#####################################
# Tell how to install this executable
#####################################

#IF(WIN32)
#    SET(CMAKE_INSTALL_PREFIX "C:\\Program Files")
#ELSE()
#    SET(CMAKE_INSTALL_PREFIX /usr/local)
#ENDIF(WIN32)
#INSTALL(TARGETS ${FOOEXE} RUNTIME DESTINATION bin)
