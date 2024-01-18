#include <cstdio>
#include <cstdlib>
#include <cstring>

typedef float float_type;

// C kernel to compute the tensor addition at a single point
void c_kernel(
    float_type* A, 
    float_type* B, 
    float_type* C,
    int i,
    int N) {
        
    // We could upload memory and launch 
    // a HIP kernel from this point
    // But because we are just using C++
    // Compute the answer directly here
    if ((i>=0) && (i<N)) {
        C[i] = A[i] + B[i];        
    }
}

extern "C" {

    // Function to launch c_kernel at every point
    // in the domain of A, B, and C
    void launch_c_kernel(
        float_type* A,
        float_type* B,
        float_type* C,
        int N) {

        // Launch an instance of c_kernel
        // at every point i in tensors A, B, C
        // and run c_kernel over it
        for(int i=0; i<N; i++) {
            c_kernel(A, B, C, i, N);
        }
    }

    // Function to allocate memory
    void* c_alloc(size_t nbytes) {
        
        // Allocate memory that is aligned 
        // to a 128 byte boundary
        void* ptr = aligned_alloc(128, nbytes);

        //void* ptr = calloc(1, nbytes);
        if (!ptr) {
            printf("Memory allocation failed, exiting.\n");
            exit(1);
        }

        // If all is good, initialise the memory to 0
        std::memset(ptr, '\0', nbytes);
        
        return ptr;
    }

    // Function to free memory
    void c_free(void* ptr) {
        free(ptr);
    }
}

