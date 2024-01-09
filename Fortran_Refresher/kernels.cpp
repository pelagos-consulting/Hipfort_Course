#include <cstdio>

typedef float float_type;

extern "C" {

    // C kernel to compute the tensor addition at a single point
    void ckernel(
        float_type* A, 
        float_type* B, 
        float_type* C,
        int i,
        int N) {

        printf("i is %d of %d \n", i, N);
        
        // We could upload memory and launch 
        // a HIP kernel from this point
        // But because we are just using C++
        // Compute the answer directly here
        if ((i>=0) && (i<N)) {
            C[i] = A[i] + B[i];        
        }
    }
}