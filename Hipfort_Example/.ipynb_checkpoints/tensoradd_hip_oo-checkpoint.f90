program tensoradd
    !! Program to compute 2D tensor addition 
    !! with GPU allocations wrapped in a Fortran object-oriented type
    !! Written by Dr. Toby Potter and Dr. Joseph Schoonover

    ! Add this to use the standard fortran environment module
    use iso_fortran_env

    ! C interopability 
    use iso_c_binding

    ! GPU functionality 
    use hip_utils, only : init_gpu, reset_gpu

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
    real(kind=c_float), dimension(:,:), pointer :: A_h, B_h, C_h

    ! Tensors on the GPU
    type(tensor_gpu) :: A_d, B_d, C_d

    ! Find and set the GPU device. Use device 0 by default
    call init_gpu(0)   

    ! Allocate memory on host 
    allocate(A_h(M,N), B_h(M, N), C_h(M,N))

    ! Allocate memory for tensors, 
    ! see tensor_hip.f90 for 
    ! definition of generic procedures 
    call A_d%malloc(sizeof(A_h))
    call B_d%malloc(sizeof(B_h))
    call C_d%malloc(sizeof(C_h))

    ! Fill host allocations with random numbers using the
    ! Fortran intrinsic function "random_number"
    call random_number(A_h)
    call random_number(B_h)

    ! Copy memory from the host 
    ! to the tensors on the GPU
    call A_d%copy_from(c_loc(A_h), sizeof(A_h))
    call B_d%copy_from(c_loc(B_h), sizeof(B_h))

    ! Call the C function that launches the kernel
    call launch_kernel_hip( &
        A_d%mem, &
        B_d%mem, &
        C_d%mem, &
        int(M, c_int), &
        int(N, c_int) &
    )

    ! Copy memory from the GPU to the host
    call C_d%copy_to(c_loc(C_h), sizeof(C_h))

    ! Check the answer
    success = check(A_h, B_h, C_h, eps_mult)

    ! Release resources

    ! Free host arrays
    deallocate(A_h, B_h, C_h)

    ! Free tensors on the GPU
    ! this step is not necessary because 
    ! the tensor type has a destructor
    ! that is called when the tensor is out of scope
    call A_d%free
    call B_d%free
    call C_d%free

    ! Make sure all resources on the GPU are released
    call reset_gpu
    
end program tensoradd

