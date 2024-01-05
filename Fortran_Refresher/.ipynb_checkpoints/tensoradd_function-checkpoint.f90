!> Function to check to if a tensor addition operation was successful
logical function check(A, B, C, N, eps_mult)
    
    ! Pointers to memory passed in
    real, pointer, dimension(:), intent(in) :: A, B, C

    ! N is the total number of elements
    integer, intent(in) :: N

    ! Epsilon multiplier, how many floating point spacings
    ! can the computed answer be from our benchmark answer?
    real, intent(in) :: eps_mult

    ! Scratch variables
    real :: scratch, upper, lower

    ! Loop index
    integer  :: i

    ! Set the return type of the function
    check = .true.

    ! Loop over all indices and check tensor addition
    do i=1, N
        scratch = A(i) + B(i)
        ! Spacing is an intrinsic function to get the spacing from
        ! one floating point representation to the next
        upper = scratch + eps_mult*abs(spacing(scratch))
        lower = scratch - eps_mult*abs(spacing(scratch))
        if (.not. ( (lower<=C(i)) .and. (C(i)<=upper) ) ) then
            write(*,*) "Error, tensor addition did not work at index = ", i
            check = .false.
            return
        end if
    end do

    ! We got to here because we didn't return on failure
    write(*,*) 'Tensor addition passed validation.'
    
end function check


!> A kernel to perform tensor addition at a single point in A, B, C
subroutine kernel(A, B, C, i, N)

    ! Input memory allocations
    real, pointer, dimension(:), intent(in) :: A, B, C
    
    ! Index into the arrays
    integer, intent(in) :: i, N

    ! Kernel math with bounds checking
    if (i<=N) then
        C(i) = A(i) + B(i)
    end if
end subroutine kernel


program tensoradd

    ! Add this to make sure that all variables must be declared
    ! and the compiler performs no type inferencing based on the 
    ! on the first letter of variable names

    implicit none

    ! Declare the external subroutine "kernel" and function "check" 
    ! to the program via an interface, this is usually not required, 
    ! but must be done because subroutine and function take pointer arguments
    interface
        subroutine kernel(A, B, C, i, N)
            real, pointer, dimension(:), intent(in) :: A, B, C
            integer, intent(in) :: i, N
        end subroutine kernel

        logical function check(A, B, C, N, eps_mult)
            real, pointer, dimension(:), intent(in) :: A, B, C
            integer, intent(in) :: N
            real, intent(in) :: eps_mult
        end function check
    end interface

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

    ! The subroutine "kernel" and the checking function "check"
    ! could also have gone here after the "contains" statement.
    ! Then the interface would not be required

end program tensoradd

