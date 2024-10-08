program tensoradd
    !! Program to compute a 1D tensor addition
    !! Written by Dr. Toby Potter and Dr. Joseph Schoonover

    ! Add this to use the standard fortran environment module
    use iso_fortran_env

    ! The "only" helps to know where things came from
    ! can use the "=>" operator to use things in modules as something else
    ! use tensor_lib, init_mem => alloc_mem
    use tensor_lib, only : check, alloc_mem => init_mem, &
        free_mem, launch_kernel, A_h, B_h

    ! Add this to make sure that all variables must be declared
    ! and the compiler performs no type inferencing based on the 
    ! on the first letter of variable names
    implicit none

    ! Number of elements in the tensors
    integer, parameter :: N=16

    ! Epsilon multiplier
    ! How many floating point spacings
    ! Should the computed solution be from the answer
    real :: eps_mult = 2.0

    ! Outcome of the check
    logical :: success

    ! Allocate memory 
    call alloc_mem(N)

    ! Fill arrays with random numbers using the
    ! Fortran intrinsic function "random_number"
    call random_number(A_h)
    call random_number(B_h)

    ! Run the kernels
    call launch_kernel

    ! Check the answer
    success = check(eps_mult)

    ! Release resources
    call free_mem

end program tensoradd

