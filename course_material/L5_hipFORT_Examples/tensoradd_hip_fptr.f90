program tensoradd
    !! Program to compute 2D tensor addition
    !! Using Fortran pointers as the handle 
    !! for device allocations
    !! Written by Dr. Toby Potter and Dr. Joseph Schoonover

    ! Add this to use the standard Fortran environment module
    use iso_fortran_env

    ! C interopability 
    use iso_c_binding

    ! Use the kinds module to make available the float_type kind
    use kinds

    ! HIP modules
    use hipfort
    use hipfort_check

    ! device handling 
    use hip_utils, only : init_device, reset_device

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
    ! should the computed solution be from the answer
    real :: eps_mult = 2.0

    ! Outcome of the check
    logical :: success

    ! Fortran pointers to memory allocations on the host
    real(float_type), dimension(:,:), pointer :: A_h, B_h, C_h

    ! Fortran pointers to memory allocations on the device
    real(float_type), dimension(:,:), pointer :: A_d, B_d, C_d

    ! Find and set the device. Use device 0 by default
    call init_device(0)   

    ! Allocate memory on host 
    allocate(A_h(M,N), B_h(M, N), C_h(M,N))

    ! Allocate memory on the device
    call hipcheck(hipMalloc(A_d, M, N))
    call hipcheck(hipMalloc(B_d, M, N))
    call hipcheck(hipMalloc(C_d, M, N))

    ! Could have also done this for the allocate instead
    !call hipcheck(hipMalloc_r4_2_c_size_t(A_d, int(M_in, c_size_t), int(N_in, c_size_t)))
    !call hipcheck(hipMalloc_r4_2_c_size_t(B_d, int(M_in, c_size_t), int(N_in, c_size_t)))
    !call hipcheck(hipMalloc_r4_2_c_size_t(C_d, int(M_in, c_size_t), int(N_in, c_size_t)))

    ! Fill arrays with random numbers using the
    ! Fortran intrinsic function "random_number"
    call random_number(A_h)
    call random_number(B_h)

    ! Copy memory from the host to the device
    ! Note that size for the copy is in elements, not bytes
    call hipcheck(hipMemcpy(A_d, A_h, size(A_h), hipMemcpyHostToDevice))
    call hipcheck(hipMemcpy(B_d, B_h, size(B_h), hipMemcpyHosttoDevice))

    ! Could also have done this for the copy instead
    !call hipcheck(hipMemcpy_r4_2_c_size_t(A_d, A_h, &
    !    int(size(A_h), c_size_t), hipMemcpyhosttodevice))
    !call hipcheck(hipMemcpy_r4_2_c_size_t(B_d, B_h, &
    !    int(size(B_h), c_size_t), hipMemcpyhosttodevice))

    ! Call the C function that launches the kernel
    call launch_kernel_hip( &
        c_loc(A_d), &
        c_loc(B_d), &
        c_loc(C_d), &
        int(M, c_int), &
        int(N, c_int) &
    )

    ! Copy from the device result back to the host
    call hipcheck(hipMemcpy(C_h, C_d, size(C_d), hipMemcpyDeviceToHost))

    ! Could have also done this instead for the copy
    !call hipcheck(hipMemcpy_r4_2_c_size_t(C_h, C_d, &
    !    int(size(C_d), c_size_t), hipMemcpydevicetohost))

    ! Check the answer
    success = check(A_h, B_h, C_h, eps_mult)

    ! Release resources

    ! Free host arrays
    deallocate(A_h, B_h, C_h)

    ! Free allocations on the device
    call hipcheck(hipFree(A_d))
    call hipcheck(hipFree(B_d))
    call hipcheck(hipFree(C_d))

    ! It is best practice to nullify all pointers 
    ! once we are done with them 
    nullify(A_h, B_h, C_h, A_d, B_d, C_d)

    ! Make sure all resources on the selected device are released
    call reset_device
    
end program tensoradd

