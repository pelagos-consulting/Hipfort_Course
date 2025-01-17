program tensoradd
    !! Program to compute 2D tensor addition
    !! Using C pointers type(c_ptr) as the handle 
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
    use hipfort_hiphostregister
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
    ! Should the computed solution be from the answer
    real :: eps_mult = 2.0

    ! Outcome of the check
    logical :: success

    ! C Pointers to pinned memory allocations on the host
    type(c_ptr) :: A_p, B_p, C_p

    ! C Pointers to pinned memory allocations on the device
    type(c_ptr) :: A_d, B_d, C_d

    ! Fortran pointers to the host memory allocations 
    real(float_type), dimension(:,:), pointer :: A_h, B_h, C_h

    ! Find and set the device. Use device 0 by default
    call init_device(0)   

    ! Allocate pinned memory on the host
    call hipcheck(hipMallocHost(A_p, int(M*N*c_sizeof(float_type), c_size_t)))
    call hipcheck(hipMallocHost(B_p, int(M*N*c_sizeof(float_type), c_size_t)))
    call hipcheck(hipMallocHost(C_p, int(M*N*c_sizeof(float_type), c_size_t)))
    
    ! Associate the Fortran pointers with the pinned host arrays
    call c_f_pointer(A_p, A_h, [M,N])
    call c_f_pointer(B_p, B_h, [M,N])
    call c_f_pointer(C_p, C_h, [M,N])

    ! Map pinned memory allocations to pointers 
    ! on the memory space of the GPU
    call hipcheck(hipHostGetDevicePointer(A_d, A_p, 0))
    call hipcheck(hipHostGetDevicePointer(B_d, B_p, 0))
    call hipcheck(hipHostGetDevicePointer(C_d, C_p, 0))

    ! Fill arrays with random numbers using the
    ! Fortran intrinsic function "random_number"
    call random_number(A_h)
    call random_number(B_h)

    ! Call the C function that launches the kernel
    call launch_kernel_hip( &
        A_d, &
        B_d, &
        C_d, &
        int(M, c_int), &
        int(N, c_int) &
    )

    ! Make sure the computation is finished
    call hipcheck(hipDeviceSynchronize())

    ! Check the answer
    success = check(A_h, B_h, C_h, eps_mult)

    ! Release resources

    ! Free pinned memory allocations on the device
    call hipcheck(hipFreeHost(A_p))
    call hipcheck(hipFreeHost(B_p))
    call hipcheck(hipFreeHost(C_p))

    ! It is best practice to nullify all pointers 
    ! once we are done with them 
    nullify(A_h, B_h, C_h)

    ! Set C pointers to null as well
    A_d = c_null_ptr
    B_d = c_null_ptr
    C_d = c_null_ptr

    ! Make sure all resources on the selected device are released
    call reset_device
    
end program tensoradd

