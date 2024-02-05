program tensoradd
    !! Program to compute 2D tensor addition
    !! Using C pointers type(c_ptr) as the handle 
    !! for GPU allocations
    !! Written by Dr. Toby Potter and Dr. Joseph Schoonover

    ! Add this to use the standard Fortran environment module
    use iso_fortran_env

    ! C interopability 
    use iso_c_binding

    ! HIP modules
    use hipfort
    use hipfort_check

    ! GPU handling 
    use hip_utils, only : init_gpu, reset_gpu

    ! Maths check
    use math_utils, only : check => check_tensor_addition_2D

    ! Add this to make sure that all variables must be declared
    ! and the compiler performs no type inferencing based on the 
    ! on the first letter of variable names
    implicit none

    ! Interface to launch_kernel_hip function
    ! in the file kernel_code.cpp
    interface
        ! A C function with void return type
        ! is regarded as a subroutine in Fortran 
        subroutine launch_kernel_hip(A, B, C, M, N) bind(C)
            use iso_c_binding
            ! Fortran passes arguments by reference as the default
            ! Arguments must have the "value" option present to pass by value
            ! Otherwise launch_kernel will receive pointers of type void**
            ! instead of void*
            type(c_ptr), intent(in), value :: A, B, C
            integer(c_int), intent(in), value :: M, N
        end subroutine
        
    end interface

    ! Number of elements in the tensors
    integer, parameter :: M=14, N=16

    ! Epsilon multiplier
    ! How many floating point spacings
    ! Should the computed solution be from the answer
    real :: eps_mult = 2.0

    ! Outcome of the check
    logical :: success

    ! Fortran pointers to memory allocations on the host
    real(kind=c_float), dimension(:,:), pointer :: A_h, B_h, C_h

    ! C Pointers to memory allocations on the device
    type(c_ptr) :: A_d, B_d, C_d

    ! Find and set the GPU device. Use device 0 by default
    call init_gpu(0)   

    ! Allocate memory on host 
    allocate(A_h(M,N), B_h(M, N), C_h(M,N))

    ! Allocate tensors on the GPU
    call hipcheck(hipmalloc(A_d, sizeof(A_h)))
    call hipcheck(hipmalloc(B_d, sizeof(B_h)))
    call hipcheck(hipmalloc(C_d, sizeof(C_h)))

    ! Fill arrays with random numbers using the
    ! Fortran intrinsic function "random_number"
    call random_number(A_h)
    call random_number(B_h)

    ! Copy memory from the host to the GPU 
    call hipcheck(hipmemcpy(A_d, c_loc(A_h), sizeof(A_h), hipmemcpyhosttodevice))
    call hipcheck(hipmemcpy(B_d, c_loc(B_h), sizeof(B_h), hipmemcpyhosttodevice))

    ! Call the C function that launches the kernel
    call launch_kernel_hip( &
        A_d, &
        B_d, &
        C_d, &
        int(M, c_int), &
        int(N, c_int) &
    )

    ! Copy memory from the GPU to the host
    call hipcheck(hipmemcpy(c_loc(C_h), C_d, sizeof(C_h), hipmemcpydevicetohost))

    ! Check the answer
    success = check(A_h, B_h, C_h, eps_mult)

    ! Release resources

    ! Free host arrays
    deallocate(A_h, B_h, C_h)
    

    ! Free allocations on the GPU
    call hipcheck(hipfree(A_d))
    call hipcheck(hipfree(B_d))
    call hipcheck(hipfree(C_d))

    ! It is best practice to nullify all pointers 
    ! once we are done with them 
    nullify(A_h, B_h, C_h)

    ! Set C pointers to null as well
    A_d = c_null_ptr
    B_d = c_null_ptr
    C_d = c_null_ptr

    ! Make sure all resources on the selected GPU are released
    call reset_gpu
    
end program tensoradd

