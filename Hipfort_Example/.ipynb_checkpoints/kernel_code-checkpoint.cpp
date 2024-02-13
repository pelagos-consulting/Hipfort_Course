// Kernel code to compute 2D tensor addition
// Written by Dr. Toby Potter and Dr. Joseph Schoonover

#include <string>
#include <iostream>
#include <cstdint>
#include <cassert>
#include <hip/hip_runtime.h>

typedef float float_type;

void hipCheck(hipError_t error_code, std::string error_msg) {
    // Function to check a HIP return code
    // and query the result
    
    if (error_code != hipSuccess) {
        std::string error_string(hipGetErrorString(error_code));
        std::cerr << "Error, HIP call failed at " << error_msg << ".\n";
        std::cerr << "Error code is: " << error_string << std::endl;
        exit(1);
    }
}

// Macro to call hipCheck
#define HIPCHECK(code) \
{\
    std::string fname(__FILE__);\
    std::string lineno = std::to_string(__LINE__);\
    hipCheck(code, fname + ":" + lineno);\
}

// Kernel to perform 2D tensor addition
__global__ void tensoradd_2D (
	    // Memory allocations
        float_type* A, 
        float_type* B, 
        float_type* C,
        // Size of the problem
        int M,
        int N) {

    // Any dynamically allocated memory is available here
    extern __shared__ float_type shared[];

    // We adopt column-major indexing for this example
    
    // Compute (zero-based) indicies within grid
    int i = blockIdx.x * blockDim.x + threadIdx.x;
    int j = blockIdx.y * blockDim.y + threadIdx.y;

    // Due to block sizes 
    // i and j might lie outside M and N
    // make sure we don't run off the domain
    // of the grid
    if ((i<M) && (j<N)) {
        // 1D position within 2D arrays
        // stride down a column is 1
        // stride along a row is M
        
        size_t offset = i + j*M;

        // Now perform the 2D tensor addition
        C[offset] = A[offset] + B[offset];
    }
}

// C function to call the tensoradd_2D kernel
extern "C" {

    void launch_kernel_hip(
            float_type* A, 
            float_type* B,
            float_type* C,
            int M,
            int N) {
        
        // Grid size
        dim3 global_size = {
            (uint32_t)(M), 
            (uint32_t)(N)
        }; 
        
        // Block size, 
        dim3 block_size = {8,8,1};
        
        // Number of blocks in each dimension
        dim3 nblocks = {
            global_size.x/block_size.x,
            global_size.y/block_size.y,
            1
        };

        // Make sure there are enough blocks
        if (global_size.x % block_size.x) nblocks.x += 1;
        if (global_size.y % block_size.y) nblocks.y += 1;
        if (global_size.z % block_size.z) nblocks.z += 1;
            
        // Decide on the number of bytes to allocate for shared memory
        size_t sharedMemBytes = 0;

        // Launch the kernel
        hipLaunchKernelGGL(
                // Kernel name
                tensoradd_2D,
                // Number of blocks per dimension
                nblocks,
                // Number of threads along each dimension of the block
                block_size,
                // Number of bytes dynamically allocated for shared memory
                sharedMemBytes,
                // Stream to use (0 is the default or null stream)
                0,
                // Kernel arguments
                A, B, C,
                M, N);

        // The triple-chevron (non C++ compliant) way of launching kernels
        //tensoradd_2d<<<nblocks, block_size, sharedMemBytes, 0>>>(A, B, C, M, N);
        
        // Make sure the kernel launch went ok
        HIPCHECK(hipGetLastError());

    	// Wait for the kernel to finish
    	HIPCHECK(hipDeviceSynchronize()); 
    }
}