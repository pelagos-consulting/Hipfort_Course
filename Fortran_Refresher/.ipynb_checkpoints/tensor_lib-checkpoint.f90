module tensor_lib
    !! Library module to work with tensors
    !! Written by Dr. Toby Potter and Dr. Joseph Schoonover
    
    implicit none

    ! Have we already allocated memory?
    logical :: allocd = .false.

    ! Number of elements in the vectors
    integer :: N

    ! Pointers to memory on the host
    real, pointer, dimension(:) :: A_h => null(), B_h => null(), C_h => null()

    ! Declare private variables that belong only to the module
    private :: allocd, N
       
    ! Declare variables, functions, and subroutines that are public
    public :: init_mem, free_mem, check, A_h, B_h, C_h

contains 

    function check(eps_mult) result(success)
        !! Function to check tensor addition

        real, intent(in) :: eps_mult
            !! Epsilon multiplier, how many floating point spacings
            !! can the computed answer be from our benchmark answer

        ! Scratch variables
        real :: scratch, upper, lower

        ! Loop index
        integer  :: i

        ! Set the outcome as positive until proven otherwise
        logical :: success
        success = .true.

        if (.not. allocd) then
            write(*,*) 'Memory has not been allocated, stopping'
            stop
        end if

        ! Loop over all indices and check tensor addition
        do i=1, N
            scratch = A_h(i) + B_h(i)
            upper = scratch + eps_mult*abs(spacing(scratch))
            lower = scratch - eps_mult*abs(spacing(scratch))
            if (.not. ( (lower<=C_h(i)) .and. (C_h(i)<=upper) ) ) then
                write(*,*) "Error, tensor addition did not work at index = ", i, &
                    ", value was: ", C_h(i), ", but should be:", scratch
                success = .false.
                return
            end if
        end do

        ! We got to here because we didn't return on failure
        write(*,*) 'Tensor addition passed validation.'
        
    end function check

    subroutine init_mem(N_in)
    
        !! Allocates memory for the tensors
    
        integer, intent(in) :: N_in
            !! Data type for the number of elements

        ! Checking errors on the allocation
        integer :: ierr

        ! Free memory first if already allocated
        if (allocd) then
            call free_mem
        end if

        ! Allocate memory for all arrays using a Fortran call
        allocate(A_h(1:N_in), B_h(1:N_in), C_h(1:N_in), stat=ierr)

        if (ierr /= 0) then
            write(*,*) 'Error, array allocation failed with error code = ', ierr
            stop 
        end if

        ! Assign private variables if everything worked
        N = N_in
        allocd = .true.

    end subroutine init_mem

    subroutine kernel(i)
        !! Fortran kernel to compute tensor addition at index i
    
        integer, intent(in) :: i
            !! Index to compute the kernel at
            !! All other variables are defined in the module

        ! Kernel math with bounds checking
        if (i<=N) then
            C_h(i) = A_h(i) + B_h(i)
        end if
    
    end subroutine kernel

    subroutine launch_kernel
    
        ! Run the kernel over every element 
        ! of the tensor space
        integer :: i
        do i=1,N
            call kernel(i)
        end do
        
    end subroutine launch_kernel

    subroutine free_mem
    
        !! Free all memory allocated for the module

        ! Error handling
        integer :: ierr

        ! Deallocate all memory

        ! De-allocate memory using a Fortran call
        deallocate(A_h, B_h, C_h, stat=ierr)
        
        if (ierr/= 0) then
            write(*,*) 'De-allocating memory failed with error code = ', ierr
            stop
        end if

        ! Repoint pointers at null() for safety
        nullify(A_h, B_h, C_h)

        ! Set private variables
        allocd = .false.
        N = 0

    end subroutine free_mem

end module tensor_lib
