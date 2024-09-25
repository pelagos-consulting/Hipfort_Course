program tensoradd
    !! Program to compute 2D tensor addition
    !! Using Fortran pointers as the handle 
    !! for device allocations
    !! Written by Dr. Toby Potter and Dr. Joseph Schoonover

    ! Add this to use the standard Fortran environment module
    use iso_fortran_env

    ! C interopability 
    use iso_c_binding

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
    real(kind=c_float), dimension(:,:), pointer :: A, B, C


    ! Find and set the device. Use device 0 by default
    call init_device(0)   

    ! Allocate managed memory that is accessible across host and device
    call hipCheck(hipMallocManaged(A, M, N, hipMemAttachGlobal))
    call hipCheck(hipMallocManaged(B, M, N, hipMemAttachGlobal))
    call hipCheck(hipMallocmanaged(C, M, N, hipMemAttachGlobal))

    ! Set coarse grained coherence for all variables
    call hipCheck(hipMemAdvise(c_loc(A), int(sizeof(A), c_size_t), hipMemAdviseSetCoarseGrain,0))
    call hipCheck(hipMemAdvise(c_loc(B), int(sizeof(B), c_size_t), hipMemAdviseSetCoarseGrain,0))
    call hipCheck(hipMemAdvise(c_loc(C), int(sizeof(C), c_size_t), hipMemAdviseSetCoarseGrain,0))

    ! Fill arrays with random numbers using the
    ! Fortran intrinsic function "random_number"
    call random_number(A)
    call random_number(B)

    ! Pre-fetch A and B to the device
    ! Copy memory from the host to the device
    ! Note that size for the copy is in elements, not bytes
    call hipCheck(hipMemPrefetchAsync(c_loc(A),int(sizeof(A),c_size_t),0,c_null_ptr))
    call hipCheck(hipMemPrefetchAsync(c_loc(B),int(sizeof(B),c_size_t),0,c_null_ptr))

    ! Call the C function that launches the kernel
    call launch_kernel_hip( &
        c_loc(A), &
        c_loc(B), &
        c_loc(C), &
        int(M, c_int), &
        int(N, c_int) &
    )

    ! Block the CPU (host) from progressing until the hip kernel finishes
    call hipcheck(hipDeviceSynchronize())

    ! Check the answer
    success = check(A, B, C, eps_mult)

    ! Release resources

    ! Free allocations
    call hipcheck(hipfree(A))
    call hipcheck(hipfree(B))
    call hipcheck(hipfree(C))

    ! It is best practice to nullify all pointers 
    ! once we are done with them 
    nullify(A, B, C)

    ! Make sure all resources on the selected device are released
    call reset_device
    
end program tensoradd

