#include <cstdio>
#include <cstdlib>

typedef float float_type;

extern "C" {

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

    // Function to allocate memory
    void* c_alloc(size_t nbytes) {
        void* ptr = calloc(1, nbytes);
        if (!ptr) {
            printf("Memory allocation failed, exiting.\n");
            exit(1);
        }
        return ptr;
    }

    // Function to free memory
    void c_free(void* ptr) {
        free(ptr);
    }
}

