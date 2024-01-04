!> Check to see if a tensor addition operation was successful
logical function check(A, B, C, N, eps_mult)
    
    ! Pointers to memory passed in
    real, dimension(:), pointer :: A, B, C

    ! N is the total number of elements
    integer :: N

    ! Epsilon multiplier, how many floating point spacings
    ! can the computed answer be from our benchmark answer?
    real :: eps_mult

    ! Scratch variables
    real :: scratch, upper, lower

    ! Loop index
    integer  :: i

    ! Set the return type of the function
    check = .true.

    ! Loop over all indices and check tensor addition
    do i=1, N
        scratch = a(i) + b(i)
        ! Spacing is an intrinsic function to get the spacing from
        ! one floating point representation to the next
        upper = scratch + eps_mult*abs(spacing(scratch))
        lower = scratch - eps_mult*abs(spacing(scratch))
        if (.not. ( (lower<=c(i)) .and. (c(i)<=upper) ) ) then
            write(*,*) "Error, tensor addition did not work at index = ", i
            check = .false.
            exit 
        end if
    end do
    
end function check

program tensoradd

    ! Add this to make sure that all variables must be declared
    ! and the compiler performs no type inferencing based on the 
    ! on the first letter of variable names

    implicit none

    ! Number of elements in the tensors
    integer, parameter :: N=16

    ! Upper and lower bounds for testing purposes
    real :: scratch, upper, lower

    ! Tensor index and error handling
    integer :: i, ierr

    ! Outcome of the check
    logical :: outcome = .false.

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

    ! Run the tensor addition kernel on each element of the array
    do i=1,N
        call kernel(A_h, B_h, C_h, i, N)
    end do

    ! Call a function to check the answer
    outcome = check(A_h, B_h, C_h, N, 2.0)

    ! Always free heap memory when you no longer need it
    deallocate(A_h, B_h, C_h)

    contains
    
        !> Execute a tensor addition kernel at a single point in A, B, C
        subroutine kernel(A, B, C, i, N)

            ! Input memory allocations
            real, pointer, dimension(:), intent(inout) :: A, B, C
    
            ! Index into the arrays
            integer, intent(in) :: i, N

            ! Kernel math with bounds checking
            if (i<=N) then
                C(i) = A(i) + B(i)
            end if
        end subroutine kernel
    
end program tensoradd

