cmake_minimum_required(VERSION 3.21) # HIP language support requires 3.21 or greater
cmake_policy(VERSION 3.21...3.27)

if(NOT DEFINED ROCM_PATH)
    if(NOT DEFINED ENV{ROCM_PATH})
        set(ROCM_PATH "/opt/rocm/" CACHE PATH "Path to which ROCm has been installed")
    else()
        set(ROCM_PATH $ENV{ROCM_PATH} CACHE PATH "Path to which ROCm has been installed")
    endif()
endif()

if(NOT DEFINED HIPFORT_ROOT)
    if(NOT DEFINED ENV{HIPFORT_ROOT})
        set(HIPFORT_ROOT "/opt/rocm/" CACHE PATH "Path to which Hipfort has been installed")
    else()
        set(HIPFORT_ROOT $ENV{HIPFORT_ROOT} CACHE PATH "Path to which Hipfort has been installed")
    endif()
endif()

project(hipfort_course VERSION 1.0.0
    DESCRIPTION "Example codes to accompany a course in hipfort"
    LANGUAGES C CXX Fortran
)

# Default Fortran compiler flags
# Fortran compiler flags
if( "${CMAKE_Fortran_COMPILER_ID}" STREQUAL "GNU" )
    set( CMAKE_Fortran_FLAGS "-cpp -ffree-line-length-512 -fPIC" )
    set( CMAKE_Fortran_FLAGS_DEBUG "-g -O1 -C -Wall -fbounds-check -fbacktrace -ffpe-trap=invalid,zero,overflow" )
    set( CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -lgfortran")
    set( CMAKE_Fortran_FLAGS_COVERAGE "${CMAKE_Fortran_FLAGS_DEBUG} --coverage")
    set( CMAKE_Fortran_FLAGS_PROFILE "-pg -O3")
    set( CMAKE_Fortran_FLAGS_RELEASE "-O3 -g" )
endif()

# ------ dependencies ------ #

# HIP
find_package(hip REQUIRED)

if ((DEFINED ENV{HIP_PLATFORM}) AND ($ENV{HIP_PLATFORM} STREQUAL nvidia))
    message("Performing compilation for an NVIDIA backend.")
    enable_language(CUDA)
    set(CUDA_SEPARABLE_COMPILATION ON)
        
    ## Find the CUDA toolkit, 
    ## it must be present if we use HIP_PLATFORM=nvidia 
    find_package(CUDAToolkit REQUIRED)

    # Set the kernel language for device code
    set(kernel_lang CUDA)

    ## Set libraries to link to for device code
    set(kernel_libs CUDA::cudart CUDA::cuda_driver)
        
    # Set GPU architectures
    set(CMAKE_CUDA_ARCHITECTURES $ENV{GPU_ARCH})

    # Add arch information to hipfc compiler flags as well
    foreach(arch $ENV{GPU_ARCH})
        set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} --offload-arch=sm_${arch}")
    endforeach()

    # Set compiler flags
    set(CMAKE_CUDA_FLAGS "-g -Xcompiler -fPIC")
    set(CMAKE_CUDA_FLAGS_DEBUG "-G -O0")
    set(CMAKE_CUDA_FLAGS_PROFILE "-pg -O3")
    set(CMAKE_CUDA_FLAGS_RELEASE "-O3")

    ## Change global preprocessor defintions for CUDA sources
        
    ## Remove any preprocessor definitions for AMD
    remove_definitions(-D__HIP_PLATFORM_HCC__ -D__HIP_PLATFORM_AMD__)
    ## Replace it with CUDA precprocessor definitions
    add_definitions(-D__HIP_PLATFORM_NVCC__ -D__HIP_PLATFORM_NVIDIA__)  

    # Locate the hipfort library
    find_library(HIPFORT_LIBRARIES NAMES libhipfort-nvptx.a HINTS ENV HIPFORT_ROOT PATH_SUFFIXES lib/ REQUIRED)
    find_path(HIPFORT_INCLUDE_DIRS hipfort.mod HINTS ENV HIPFORT_ROOT PATH_SUFFIXES include/hipfort/nvptx/ REQUIRED)


else()

    message("Performing compilation for an AMD backend.")
    
    # Enable the HIP language
    enable_language(HIP)

    # Set language for device code
    set(kernel_lang HIP)

    # Set libraries for kernel code
    set(kernel_libs hip::device)

    # Set the GPU architecture
    set(CMAKE_HIP_ARCHITECTURES, $ENV{GPU_ARCH})
    
    # Add arch information to hipfc Fortran compiler flags as well
    foreach(arch $ENV{GPU_ARCH})
        set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} --offload-arch=${arch}")
    endforeach()

    # Set HIP flags
    set(CMAKE_HIP_FLAGS "${CMAKE_HIP_FLAGS} -g")
    set(CMAKE_HIP_FLAGS_DEBUG, "-ggdb -O1")
    set(CMAKE_HIP_FLAGS_PROFILE, "-pg -O3")
    set(CMAKE_HIP_FLAGS_RELEASE, "-O3")

    # Locate the hipfort library
    find_library(HIPFORT_LIBRARIES NAMES libhipfort-amdgcn.a HINTS ENV HIPFORT_ROOT PATH_SUFFIXES lib/ REQUIRED)
    find_path(HIPFORT_INCLUDE_DIRS hipfort.mod HINTS ENV HIPFORT_ROOT PATH_SUFFIXES include/hipfort/amdgcn/ REQUIRED)

    
endif()

# Debugging and display
get_cmake_property(_variableNames VARIABLES)
list (SORT _variableNames)
foreach (_variableName ${_variableNames})
    message(STATUS "${_variableName}=${${_variableName}}")
endforeach()

# Example Codes
add_subdirectory(${CMAKE_SOURCE_DIR}/L1_Fortran_Refresher)
add_subdirectory(${CMAKE_SOURCE_DIR}/L5_Hipfort_Examples)
add_subdirectory(${CMAKE_SOURCE_DIR}/Exercises/E1_Chessboard_CPU)
add_subdirectory(${CMAKE_SOURCE_DIR}/Exercises/E2_Chessboard_GPU)
add_subdirectory(${CMAKE_SOURCE_DIR}/L6_Memory_Management/memcpy_benchmark)
