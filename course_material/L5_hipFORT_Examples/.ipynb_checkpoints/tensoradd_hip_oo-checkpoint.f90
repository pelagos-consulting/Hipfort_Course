program tensoradd
    !! Program to compute 2D tensor addition 
    !! with GPU allocations wrapped in a Fortran object-oriented type
    !! Written by Dr. Toby Potter and Dr. Joseph Schoonover

    ! Add this to use the standard fortran environment module
    use iso_fortran_env

    ! C interopability 
    use iso_c_binding

    ! Use the kinds module to make available the float_type kind
    use kinds

    ! GPU functionality 
    use hip_utils, only : init_device, reset_device

    ! Use the tensor type defined in tensor_hip.f90
    use tensor_hip, only : tensor_gpu => tensor

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
    real(float_type), dimension(:,:), pointer :: A_h, B_h, C_h

    ! Tensors on the GPU
    type(tensor_gpu) :: A_d, B_d, C_d

    ! Find and set the device device. Use device 0 by default
    call init_device(0)   

    ! Allocate memory on host 
    allocate(A_h(M,N), B_h(M, N), C_h(M,N))

    ! Allocate memory for tensors, 
    ! see tensor_hip.f90 for 
    ! definition of generic procedures 
    call A_d%malloc(int(sizeof(A_h), c_size_t))
    call B_d%malloc(int(sizeof(B_h), c_size_t))
    call C_d%malloc(int(sizeof(C_h), c_size_t))

    ! Fill host allocations with random numbers using the
    ! Fortran intrinsic function "random_number"
    call random_number(A_h)
    call random_number(B_h)

    ! Copy memory from the host 
    ! to the tensors on the device
    call A_d%copy_from(A_h)
    call B_d%copy_from(B_h)

    ! Call the C function that launches the kernel
    call launch_kernel_hip( &
        A_d%mem, &
        B_d%mem, &
        C_d%mem, &
        int(M, c_int), &
        int(N, c_int) &
    )

    ! Copy memory from the device to the host
    call C_d%copy_to(C_h)

    ! Check the answer
    success = check(A_h, B_h, C_h, eps_mult)

    ! Release resources

    ! Free host arrays
    deallocate(A_h, B_h, C_h)

    ! Free tensors on the device
    ! this step is not necessary because 
    ! the tensor type has a destructor
    ! that is called when the tensor is out of scope
    call A_d%free
    call B_d%free
    call C_d%free

    ! Make sure all resources on the device are released
    call reset_device
    
end program tensoradd

