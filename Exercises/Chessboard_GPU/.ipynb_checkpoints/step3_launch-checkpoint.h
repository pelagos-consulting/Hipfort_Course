
    call launch_kernel_hip( &
        c_loc(B_d), &
        light, &
        dark, &
        int(M, c_int), &
        int(N, c_int) &
    )