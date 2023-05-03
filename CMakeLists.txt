# CMake project file for NeuralNetwork

##################################################
# Define the project and the depencies that it has
##################################################

CMAKE_MINIMUM_REQUIRED(VERSION 2.8.5)
PROJECT(NeuralNetwork LANGUAGES Fortran)

# Set the NeuralNetwork version
SET(VERSION 1.0.0)
ENABLE_TESTING() 

# Look for required libraries:
find_package(HDF5 REQUIRED COMPONENTS Fortran HL)
find_package(netCDF REQUIRED COMPONENTS Fortran)

# Add our local modlues to the module path
SET(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake/Modules/")

# Uncomment the below if you want the user to choose a parallelization library
#OPTION(USE_MPI "Use the MPI library for parallelization" OFF)
#OPTION(USE_OPENMP "Use OpenMP for parallelization" OFF)

INCLUDE(${CMAKE_MODULE_PATH}/SetFortranFlags.cmake) 
#INCLUDE(${CMAKE_MODULE_PATH}/SetParallelizationLibrary.cmake)
#INCLUDE(${CMAKE_MODULE_PATH}/SetUpLAPACK.cmake)

############################################################
# Define the actual files and folders that make up the build
############################################################

# Define the executable name
SET(TESTEXE test)

# Define the library name
SET(NEURALLIB neural)

# Define some directories
SET(SRC ${CMAKE_SOURCE_DIR}/src)
SET(LIB ${CMAKE_SOURCE_DIR}/lib)
SET(BIN ${CMAKE_SOURCE_DIR}/bin)
SET(SRCNEURAL ${SRC})
SET(SRCTEST ${CMAKE_SOURCE_DIR}/test)

# Have the .mod files placed in the lib folder
SET(CMAKE_Fortran_MODULE_DIRECTORY ${LIB})

# The source for the NEURAL library and have it placed in the lib folder
ADD_SUBDIRECTORY(${SRCNEURAL} ${LIB})

# The source for the TEST binary and have it placed in the bin folder
ADD_SUBDIRECTORY(${SRCTEST} ${BIN})

# Add a distclean target to the Makefile
ADD_CUSTOM_TARGET(distclean 
    COMMAND ${CMAKE_COMMAND} -P ${CMAKE_SOURCE_DIR}/distclean.cmake
)
