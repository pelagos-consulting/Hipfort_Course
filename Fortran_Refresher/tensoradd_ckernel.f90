program tensoradd
    !! Program to compute a 1D tensor addition
    !! Written by Dr. Toby Potter and Dr. Joseph Schoonover

    ! The "only" helps to know where things came from
    ! can use the "=>" operator to use things in modules as something else
    ! use tensor_lib, init_mem => alloc_mem
    
    use tensor_lib, only : check, alloc_mem => init_mem, &
        free_mem, kernel, A_h, B_h
    
    use iso_c_binding

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
    
    ! Index into tensors
    integer :: i

    ! Outcome of the check
    logical :: success

    ! Allocate memory 
    call alloc_mem(N)

    ! Fill arrays with random numbers using the
    ! Fortran intrinsic function "random_number"
    call random_number(A_h)
    call random_number(B_h)

    ! Run the C kernel function over each element of the array
    do i=1,N
        call kernel(i)
    end do

    ! Check the answer
    success = check(eps_mult)

    ! Release resources
    call free_mem

end program tensoradd

