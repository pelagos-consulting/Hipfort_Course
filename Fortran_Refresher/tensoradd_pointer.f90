
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

    ! For error handling
    integer :: ierr

    ! Define pointers to memory
    real, pointer, dimension(:) :: A_h => null(), B_h => null(), C_h => null()

    ! Allocate memory for pointers on the heap and check for errors
    allocate(A_h(N), B_h(N), C_h(N), stat=ierr)
    if (ierr /= 0) then
        write(*,*) 'Error, array allocation failed with error code = ', ierr
        stop 
    end if

    ! Fill arrays with random numbers using the
    ! Fortran intrinsic function "random_number"
    call random_number(A_h)
    call random_number(B_h)

    ! Run the tensor addition one element at a time
    do i=1,N
        C_h(i) = A_h(i) + B_h(i)
    end do

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

    ! Always free heap memory when you no longer need it
    deallocate(A_h, B_h, C_h)
    
end program tensoradd

