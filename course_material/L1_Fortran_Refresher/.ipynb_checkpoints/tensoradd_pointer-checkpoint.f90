
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

    ! Define pointers to memory, initialise to null() for safety
    real, pointer, dimension(:) :: A_h => null(), B_h => null(), C_h => null()

    ! Demonstrate pointer remapping with D_h
    real, pointer, dimension(:,:) :: D_h

    ! Upper and lower bounds for testing purposes
    real :: scratch, upper, lower

    ! Tensor index and error handling
    integer :: i, ierr

    ! Was the experiment successful?
    logical :: success = .true.

    ! Allocate tensors on the heap and check for errors
    allocate(A_h(N), B_h(N), C_h(N), stat=ierr)
    
    if (ierr /= 0) then
        write(*,*) 'Error, array allocation failed with error code = ', ierr
        stop 
    end if

    ! Demonstrate pointer remapping.
    ! Point a 4x4 2D pointer at the allocated memory
    
    ! We normally wouldn't hardcode array sizes like this
    D_h(1:4, 1:4) => A_h    
    
    ! Point a 2D pointer of size 2x2 
    ! at the last 4 elements of A_h
    D_h(1:2, 1:2) => A_h((N-4):N)

    ! Fill arrays with random numbers using the
    ! Fortran intrinsic function "random_number"
    call random_number(A_h)
    call random_number(B_h)

    ! Compute the tensor addition one element at a time
    do i=1,N
        ! Kernel math
        C_h(i) = A_h(i) + B_h(i)
    end do

    ! Check the answer
    do i=1,N

        ! Compute the answer on the CPU
        scratch = A_h(i) + B_h(i)

        ! Get upper and lower bounds on the computed solution
        ! the spacing intrinsic function gets the floating point spacing
        ! from one number to the next
        upper = scratch + eps_mult*spacing(abs(scratch))
        lower = scratch - eps_mult*spacing(abs(scratch))

        ! Check to see if the number is in floating point range of the answer
        if  ( .not. ((lower <= C_h(i)) .and. (C_h(i) <= upper))) then
            write(*,*) 'Error, calculated answer at index i = ', i, ' was not in range'
            success = .false.
        end if

    end do

    if (success) then
        write(*,*) 'Tensor addition passed validation.'
    end if

    ! Always free heap memory when you no longer need it
    deallocate(A_h, B_h, C_h)
    
end program tensoradd

