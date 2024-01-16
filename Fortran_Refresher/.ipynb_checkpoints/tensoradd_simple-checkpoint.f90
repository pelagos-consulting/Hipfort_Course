program tensoradd
    !! Program to compute a 1D tensor addition
    !! Written by Dr. Toby Potter and Dr. Joseph Schoonover

    ! Add this to use the standard fortran environment module
    use iso_fortran_env

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

    ! Upper and lower bounds for testing purposes
    real :: scratch, upper, lower

    ! Tensor index
    integer :: i

    ! Was the experiment successful?
    logical :: success = .true.

    ! Base filename
    character(len=10) :: bname = 'array_'

    ! Declare the tensors to use
    ! Memory for these will be allocated on the stack
    real :: A_h(0:N), B_h(0:N), C_h(0:N)

    ! Fill arrays with random numbers using the
    ! Fortran intrinsic function "random_number"
    call random_number(A_h)
    call random_number(B_h)

    ! Run the tensor addition one element at a time
    ! using a deterministic loop
    do i=1,N
        ! Kernel math
        C_h(i) = A_h(i) + B_h(i)
    end do

    ! Do the operation again using a non-deterministic loop
    i=1
    do while (i<=N)
        ! Kernel math
        C_h(i) = A_h(i) + B_h(i)
        i = i + 1
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
        ! the "spacing" intrinsic function gets the floating point spacing
        ! from one number to the next
        upper = scratch + eps_mult*spacing(abs(scratch))
        lower = scratch - eps_mult*spacing(abs(scratch))

        ! Check to see if the number is in floating point range of the answer
        if  ( .not. ((lower <= C_h(i)) .and. (C_h(i) <= upper))) then
            ! Demonstrate line continuation
            write(error_unit,*) 'Error, calculated answer at index i = ', &
                i, ' was not in range'
            success = .false.
        end if

    end do

    if (success) then
        print *, 'Tensor addition passed validation.'

        ! Open the files for writing
        open(10, file=trim(bname)//'A_h.dat', &
            form='unformatted', status='new', access='stream')
        open(11, file=trim(bname)//'B_h.dat', &
            form='unformatted', status='new', access='stream')
        open(12, file=trim(bname)//'C_h.dat', &
            form='unformatted', status='new', access='stream')

        ! Write the contents of the arrays to the open files 
        write(10) A_h(:)
        write(11) B_h(:)
        write(12) C_h(:)

        ! Close the files
        close(10)
        close(11)
        close(12)
        
    end if
    
end program tensoradd

