program tensoradd
    !! Program to compute a 1D tensor addition
    !! Written by Dr. Toby Potter and Dr. Joseph Schoonover

    ! Add this to make sure that all variables must be declared
    ! and the compiler performs no type inferencing based on the 
    ! on the first letter of variable names
    implicit none

    ! Number of elements in the tensors
    integer, parameter :: N=16

    ! Declare the tensors to use
    ! Memory for these will be allocated on the stack
    real :: A_h(N), B_h(N), C_h(N)

    ! Upper and lower bounds for testing purposes
    real :: scratch, upper, lower

    ! Tensor index
    integer :: i

    ! Was the experiment successful?
    logical :: success = .true.

    ! Fill arrays with random numbers using the
    ! Fortran intrinsic function "random_number"
    call random_number(A_h)
    call random_number(B_h)

    ! Run the tensor addition one element at a time
    do i=1,N
        C_h(i) = A_h(i) + B_h(i)
    end do

    ! Could also do it this way, (best practice)
    C_h(:) = A_h(:) + B_h(:)

    ! Or even this way (not best practice)
    C_h = A_h + B_h

    ! Check the answer
    do i=1,N

        ! Compute the answer on the CPU
        scratch = A_h(i) + B_h(i)

        ! Get upper and lower bounds on the computed solution
        ! the spacing function gets the floating point spacing
        ! from one number to the next
        upper = scratch + 2.0*spacing(abs(scratch))
        lower = scratch - 2.0*spacing(abs(scratch))

        ! Check to see if the number is in floating point range of the answer
        if  ( .not. ((lower <= C_h(i)) .and. (C_h(i) <= upper))) then
            write(*,*) 'Error, calculated answer at index i = ', i, ' was not in range'
            success = .false.
        end if

    end do

    if (success) then
        write(*,*) 'Tensor addition validated successfully.'
    end if
    
end program tensoradd

