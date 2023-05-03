# crtm-ai

## Description

A Fortran deep learning library as part of the SIMOBS-24: CRTM-AI Epic of the AOP22.

## Compilation

In order to configure and compile the library with cmake simply type the following in the library directory:

```bash
mkdir build
cd build
cmake ..
make
```

## Testing

To run the ctests for this library, simply type the following command in the build directory:

```bash
ctest
```

To run an individual test, type:

```bash
ctest -VV -R {test_name}
```

