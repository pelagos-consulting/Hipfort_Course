
program tensoradd

    ! Add this to make sure that all variables must be declared
    ! and the compiler performs no type inferencing based on the 
    ! on the first letter of variable names
    implicit none

    ! Number of elements in the tensors
    integer, parameter :: N=16

    ! Upper and lower bounds for testing purposes
    real :: scratch, upper, lower

    ! Tensor index
    integer :: i

    ! Define the tensors to use
    ! Memory will be allocated on the stack
    real :: A_h(N), B_h(N), C_h(N)

    ! Fill arrays with random numbers using the
    ! Fortran intrinsic function "random_number"
    call random_number(A_h)
    call random_number(B_h)

    ! Run the tensor addition one element at a time
    do i=1,N
        C_h(i) = A_h(i) + B_h(i)
    end do

    ! Could also do it this way
    C_h(:) = A_h(:) + B_h(:)

    ! Or even this way
    C_h = A_h + B_h

    ! Check the answer
    do i=1,N
        ! The computed solution
        scratch = A_h(i) + B_h(i)

        ! Get upper and lower bounds on the computed solution
        ! the spacing function gets the floating point spacing
        ! from one number to the next
        upper = scratch + 2.0*spacing(abs(scratch))
        lower = scratch - 2.0*spacing(abs(scratch))

        ! Check to see if the number is in floating point range of the answer
        if  ( .not. ((lower <= C_h(i)) .and. (C_h(i) <= upper))) then
            write(*,*) 'Error, calculated answer at index i = ', i, ' was not in range' 
        end if

    end do

    write(*,*) 'Tensor addition validated successfully.'
    
end program tensoradd

