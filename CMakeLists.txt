cmake_minimum_required(VERSION 3.21) # HIP language support requires 3.21 or gretater
cmake_policy(VERSION 3.21...3.27)

if(NOT DEFINED ROCM_PATH)
    if(NOT DEFINED ENV{ROCM_PATH})
        set(ROCM_PATH "/opt/rocm/" CACHE PATH "Path to which ROCm has been installed")
    else()
        set(ROCM_PATH $ENV{ROCM_PATH} CACHE PATH "Path to which ROCm has been installed")
    endif()
endif()

# CXX language is needed to properly find "hip" package
project(hipfort_course VERSION 1.0.0
	DESCRIPTION "Example codes to accompany a course in hipfort"
	LANGUAGES Fortran HIP CXX)

# Default Fortran compiler flags
# Fortran compiler flags
if( "${CMAKE_Fortran_COMPILER_ID}" STREQUAL "GNU" )
    set( CMAKE_Fortran_FLAGS "-cpp -ffree-line-length-512" )
    set( CMAKE_Fortran_FLAGS_DEBUG "-g -O0 -C -Wall -fbounds-check -fbacktrace -ffpe-trap=invalid,zero,overflow" )
    set( CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -lgfortran")
    set( CMAKE_Fortran_FLAGS_COVERAGE "${CMAKE_Fortran_FLAGS_DEBUG} --coverage")
    set( CMAKE_Fortran_FLAGS_PROFILE "-pg -O3")
    set( CMAKE_Fortran_FLAGS_RELEASE "-O3" )
endif()

# Not sure if this is necessary
#if(DEFINED ENV{HIP_PLATFORM})
#    if(ENV{HIP_PLATFORM} STREQUAL "nvidia")
#        add_definitions(-D__HIP_PLATFORM_NVIDIA__)
#    else()
#        add_definitions(-D__HIP_PLATFORM_AMD__)
#    endif()
#else()
#    add_definitions(-D__HIP_PLATFORM_AMD__)   
#endif()

# ------ dependencies ------ #
# HIP
find_package(hip REQUIRED)

# HIPFort
find_package(hipfort REQUIRED)

# ----- end dependencies ----- #

# Example Codes
add_subdirectory(${CMAKE_SOURCE_DIR}/Memory_Management/memcpy_benchmark)
add_subdirectory(${CMAKE_SOURCE_DIR}/Fortran_Refresher)
add_subdirectory(${CMAKE_SOURCE_DIR}/Kernel_Example)
