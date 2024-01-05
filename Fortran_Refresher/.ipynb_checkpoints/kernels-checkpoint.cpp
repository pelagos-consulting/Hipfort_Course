typedef float ftype;

extern "C" {

    // C kernel to compute the tensor addition at a single point
    void launch_kernel(
        ftype* A, 
        ftype* B, 
        ftype* C,
        int i,
        int N) {
        
        // We could upload memory and launch 
        // a HIP kernel from this point
        // But because we are just using C++
        // Compute the answer directly here
        if (i<N) {
            C[i] = A[i] + B[i];        
        }
    }

}