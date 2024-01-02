
! Library module to work with tensors
module tensor_lib

    implicit none
 
contains 

    !> Check vector addition for tensors A, B, and C
    subroutine check_tensoradd(A, B, C, N, eps)
        ! Pointers to memory passed in
        real, pointer, intent(inout) :: A, B, C

        ! N is the total number of elements
        integer, intent(in) :: N

        ! Epsilon, a value that is multiplied by scratch to form the 
        ! upper and lower bounds. The answer must be within these bounds
        real, intent(in) :: eps

        ! Scratch variables
        real :: scratch, upper, lower

        ! Loop index
        integer  :: i

        ! Loop over all indices and check tensor addition
        do i=1, N
            scratch = a(i) + b(i)
            upper = scratch + eps*abs(scratch)
            lower = scratch - eps*abs(scratch)
            if (.not. ( (c(i)<=upper) .and. (c(i)>=lower) ) ) then
                write(*,*) "Error, tensor addition did not work at index = ", i
                stop 1
        end do  
    end subroutine check_tensoradd

end module tensor_lib

!> Module to contain array
module arrays

    ! Always put this in to make sure no variable has an assumed type
    implicit none 

    ! Have we allocated memory?
    logical :: allocated = .false.

    ! Number of elements in the vectors
    integer :: N

    ! Pointers to memory on the host, these store 
    real, pointer, dimension(:) :: A_h, B_h, C_h

    ! Declare private variables that belong only to the module
    private :: allocated, N
       
    ! Declare variables, functions, and subroutines that are public
    public :: alloc_mem, free_mem, launch_vecadd, A_h, B_h, C_h
    
contains

    !> Allocates all memory in the module
    subroutine alloc_mem(N_in)
        ! Data type for the number of elements
        integer, intent(in) :: N_in

        ! Checking errors on the allocation
        integer :: ierr

        if (.allocated. .eq. .true) then
            call free_memory()
        end if

        ! Assign private variables
        N = N_in

        ! Allocate memory for all arrays
        allocate(A_h(N), B_h(N), C_h(N), stat=ierr)

        if (ierr/=0) then 
            write(*,*) 'Allocating memory failed with error code = ', ierr
            stop ierr
        end if
        
        allocated = .true.

    end subroutine 

    !> Frees all memory in the module
    subroutine free_mem

        integer :: ierr

        ! Deallocate all memory
        deallocate(A_h, B_h, C_h, stat=ierr)

        if (ierr/= 0) then
            write(*,*) 'De-allocating memory failed with error code = ', ierr
            stop ierr
        end if

    end subroutine free_mem

end module arrays

!> Kernel function to compute an addition between tensors A and B
! the result is placed into C
function kernel(A, B, C, i, N)
    real, pointer, dimension(:), intent(inout) :: A, B, C
    
    ! Index into the arrays
    integer, intent(in) :: i

    ! Kernel math with bounds checking
    if (i<=N)
        C(i) = A(i) + B(i)
    end if

end function kernel

program tensoradd
    use tensor_lib
    use arrays

    implicit none

    ! Number of elements in the array
    integer, parameter :: N=16
    
    integer :: i

    ! Call a function to allocate arrays A_h, B_h, and C_h. 
    ! They are defined in the arrays module
    call alloc_mem(N)

    ! Initialise C
    C_h = 0.0

    ! Fill arrays with random numbers using the
    ! Fortran intrinsic function "random_number"
    call random_number(A_h)
    call random_number(B_h)

    ! Execute a kernel at every point in the tensor
    do i=1,N
        kernel(A_h, B_h, C_h, i, N)
    end do

    ! Call a subroutine in tensor_lib to check the answer
    call check_tensoradd(A_h, B_h, C_h, N, epsilon(1.0))

    ! Free all resources
    call free_mem()

end program vecadd

