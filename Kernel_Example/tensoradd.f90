program tensoradd
    !! Program to compute a 1D tensor addition
    !! Written by Dr. Toby Potter and Dr. Joseph Schoonover

    ! Add this to use the standard fortran environment module
    use iso_fortran_env

    ! The "only" option helps to know where things came from
    use tensor_lib, only : &
        ! Arrays on the GPU
        A_d, B_d, C_d, &
        ! Arrays on the host
        A_h, B_h, C_h, &
        ! Functions and subroutines
        init_gpu, reset_gpu, init_mem, free_mem, &
        launch_kernel, check, upload_2D, download_2D

    ! Add this to make sure that all variables must be declared
    ! and the compiler performs no type inferencing based on the 
    ! on the first letter of variable names
    implicit none

    ! Number of elements in the tensors
    integer, parameter :: M=14, N=16

    ! Epsilon multiplier
    ! How many floating point spacings
    ! Should the computed solution be from the answer
    real :: eps_mult = 2.0

    ! Outcome of the check
    logical :: success

    ! Find and set the GPU device. Use device 0 by default
    call init_gpu(0)

    ! Allocate memory on the GPU and the host 
    call init_mem(M, N)

    ! Fill arrays with random numbers using the
    ! Fortran intrinsic function "random_number"
    call random_number(A_h)
    call random_number(B_h)

    ! Upload memory from the host to the GPU 
    call upload_2D(A_d, A_h)
    call upload_2D(B_d, B_h)

    ! Launch the kernel
    call launch_kernel

    ! Download memory from the GPU to the host
    call download_2D(C_h, C_d)

    ! Check the answer
    success = check(eps_mult)

    ! Release resources
    call free_mem

    ! Make sure all resources on the GPU are released
    call reset_gpu

end program tensoradd

