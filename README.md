# Accelerated computing with Fortran and Hipfort

A course in using hipfort for accelerated computing


## Building the examples
This repository comes with examples that you can test drive on your own GPU accelerated systems (AMD or Nvidia). To get started, you will need

* A 2008 Fortran standard compliant Fortran compiler
* [ROCm](https://rocm.docs.amd.com)
* [HIPFort](https://github.com/ROCm/hipfort)
* CMake (v 3.21 or greater)

After you clone this repository and navigate into its directory on your local workstation, 


1. Create a build directory
```
mkdir build/
```

2. Run `cmake`, specifying the `hipfc` wrapper as the Fortran compiler
```
FC=hipfc cmake -DCMAKE_INSTALL_PREFIX=${HOME}/.local/ ../ 
```

> [!NOTE]
> The `CMAKE_INSTALL_PREFIX` variable sets the destination for the example applications. The above example will install binaries at `${HOME}/.local/bin`
>


3. Build the examples
```
make
```

4. Install the examples
```
make install
```

5. Add the binary and library paths to your `PATH` and `LD_LIBRARY_PATH` environment variables
```
export PATH=$PATH:${HOME}/.local/bin
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:${HOME}/.local/lib
```

