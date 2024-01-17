
logical function check(A, B, C, N, eps_mult)
    !! Function to check to if a tensor addition operation was successful

    real, pointer, dimension(:), intent(in) :: A, B, C
        !! Pointers to memory passed in

    integer, intent(in) :: N
        !! N is the total number of elements

    real, intent(in) :: eps_mult
        !! Epsilon multiplier, how many floating point spacings
        !! can the computed answer be from our benchmark answer?

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


! Run kernel math to perform tensor addition at 
! every point in input tensors A and B. 
! Put the result in C
subroutine launch_kernel(A, B, C, N)
    !! Run a tensor addition kernel over the elements of A, B, and C

    real, pointer, dimension(:), intent(inout) :: A, B, C
        !! Pointers to memory allocations

    integer, intent(in) :: N
        !! Total length of the tensors

    integer :: i
        !! Index into tensors

    ! Now run the kernel math at every point in the array
    do i=1,N
        C(i) = A(i) + B(i)
    end do
    
end subroutine launch_kernel


program tensoradd
    !! Program to compute a 1D tensor addition
    !! Written by Dr. Toby Potter and Dr. Joseph Schoonover

    ! Add this to use the standard fortran environment module
    use iso_fortran_env

    ! Add this to make sure that all variables must be declared
    ! and the compiler performs no type inferencing based on the 
    ! on the first letter of variable names

    implicit none

    ! Declare the external subroutine "kernel" and function "check" 
    ! to the program via an interface, this is usually not required, 
    ! but must be done because subroutine and function take pointer arguments
    interface
        subroutine launch_kernel(A, B, C, N)
            real, pointer, dimension(:), intent(in) :: A, B, C
            integer, intent(in) :: N
        end subroutine launch_kernel

        logical function check(A, B, C, N, eps_mult)
            real, pointer, dimension(:), intent(in) :: A, B, C
            integer, intent(in) :: N
            real, intent(in) :: eps_mult
        end function check
    end interface

    ! Number of elements in the tensors
    integer, parameter :: N=16

    ! Epsilon multiplier
    ! How many floating point spacings
    ! Should the computed solution be from the answer
    real :: eps_mult = 2.0

    ! Define pointers to memory, initialise to null() for safety
    real, pointer, dimension(:) :: A_h => null(), B_h => null(), C_h => null()

    ! Tensor index and error handling
    integer :: ierr

    ! Was the experiment successful?
    logical :: success = .true.

    ! Allocate tensors on the heap and check for errors
    allocate(A_h(N), B_h(N), C_h(N), stat=ierr)
    
    if (ierr /= 0) then
        write(*,*) 'Error, array allocation failed with error code = ', ierr
        stop 
    end if

    ! Fill arrays with random numbers using the
    ! Fortran intrinsic function "random_number"
    call random_number(A_h)
    call random_number(B_h)

    ! Call the tensor addition kernel for each element of the array
    call launch_kernel(A_h, B_h, C_h, N)

    ! Call the check function to check the answer
    success = check(A_h, B_h, C_h, N, eps_mult)

    ! Always free heap memory when you no longer need it
    deallocate(A_h, B_h, C_h)

    contains

    ! The subroutine "launch_kernel" and the checking function "check"
    ! could also have gone here after the "contains" statement.
    ! Then the interface would not be required

end program tensoradd

