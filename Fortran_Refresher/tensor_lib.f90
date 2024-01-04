! Library module to work with tensors
module tensor_lib

    implicit none

    ! Have we allocated memory?
    logical :: allocated = .false.

    ! Number of elements in the vectors
    integer :: N

    ! Pointers to memory on the host
    real, pointer, dimension(:) :: A_h = null(), B_h = null(), C_h = null()

    ! Declare private variables that belong only to the module
    private :: allocated, N
       
    ! Declare variables, functions, and subroutines that are public
    public :: tensor_init, tensor_free, tensor_check, A_h, B_h, C_h

contains 

    !> Allocates all memory in the module
    subroutine init_mem(N_in)
        ! Data type for the number of elements
        integer, intent(in) :: N_in

        ! Checking errors on the allocation
        integer :: ierr

        if (.allocated. .eq. .true) then
            call free_memory()
        end if


        ! Allocate memory for all arrays
        allocate(A_h(N), B_h(N), C_h(N), stat=ierr)

        if (ierr/=0) then 
            write(*,*) 'Allocating memory failed with error code = ', ierr
            stop 
        end if

        
        ! Assign private variables if everything worked
        N = N_in
        allocated = .true.

    end subroutine init_mem


    !> Frees all memory in the module
    subroutine free_mem

        integer :: ierr

        ! Deallocate all memory
        deallocate(A_h, B_h, C_h, stat=ierr)

        if (ierr/= 0) then
            write(*,*) 'De-allocating memory failed with error code = ', ierr
            stop
        end if

        allocated = .false.

    end subroutine free_mem


    !> Check vector addition for tensors A, B, and C
    function check(eps_mult)

        ! Set the return type and return value of the function
        real :: check = .false.

        ! Epsilon multiplier, how many floating point spacings
        ! can the computed answer be from our benchmark answer
        real, intent(in) :: eps_mult

        ! Scratch variables
        real :: scratch, upper, lower

        ! Loop index
        integer  :: i

        if (.not. allocated) then
            
        end if

        ! Loop over all indices and check tensor addition
        do i=1, N
            scratch = C_h(i) + B_h(i)
            upper = scratch + eps_mult*abs(spacing(scratch))
            lower = scratch - eps_mult*abs(spacing(scratch))
            if (.not. ( (lower<=C_h(i)) .and. (C_h(i)<=upper) ) ) then
                write(*,*) "Error, tensor addition did not work at index = ", i
                stop
            end if
        end do

        write(*,*) 'Tensor addition validated successfully.'
        
    end function check

end module tensor_lib