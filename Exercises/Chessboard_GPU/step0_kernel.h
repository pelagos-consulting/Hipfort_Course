    if ((i0<M) && (i1<N)) {
        // 1D position within 2D arrays
        // stride down a column is 1
        // stride along a row is M
        
        size_t offset = i0 + i1*M;

        //// Step 0: Implement the kernel code
        //// Hint: if k = i0 + i1 % 2
        //// Then the chessboard values may be computed as
        //// ((k+1)%2)*light + (k%2)*dark
        
        int k = i0 + i1 % 2;
        B[offset] = ((k+1)%2)*light + (k%2)*dark;

    }