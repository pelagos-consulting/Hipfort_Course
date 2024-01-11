module tensor_lib
    !! Library module to work with tensors
    !! Written by Dr. Toby Potter and Dr. Joseph Schoonover

    use iso_fortran_env
    use iso_c_binding
    
    implicit none

    ! Interface to C kernel functions
    interface
    
        ! Fortran interprets a C function with void return type
        ! as a subroutine 
        ! This is the fortran interface to the C function
        subroutine c_kernel(A, B, C, i, N) bind(C)
            use iso_c_binding
            ! Fortran passes by reference as the default
            ! Must have the "value" option present to pass by value
            ! Otherwise ckernel will receive pointers of type void**
            ! instead of void*
            type(c_ptr), value :: A, B, C
            integer(c_int), value :: i, N
        end subroutine

        ! C function to allocate memory
        type(c_ptr) function c_alloc(nbytes) bind(C)
            use iso_c_binding
            ! Make sure we have the value option set
            ! to pass by value
            integer(c_size_t), intent(in), value :: nbytes
        end function c_alloc

        ! C function to free memory 
        subroutine c_free(ptr) bind(C)
            use iso_c_binding
            ! Make sure we have the value option set
            ! to pass by value
            type(c_ptr), intent(in), value :: ptr
        end subroutine c_free
        
    end interface

    ! Have we already allocated memory?
    logical :: allocd = .false.

    ! Number of elements in the vectors
    integer :: N

    ! Pointers to memory on the host
    real, pointer, dimension(:) :: A_h => null(), B_h => null(), C_h => null()

    ! Declare private variables functions and subroutines
    ! that belong only to the module
    private :: allocd, N
       
    ! Declare variables, functions, and subroutines that are public
    public :: init_mem, free_mem, check, kernel, A_h, B_h, C_h

contains 

    logical function check(eps_mult)
        !! Function to check tensor addition

        real, intent(in) :: eps_mult
            !! Epsilon multiplier, how many floating point spacings
            !! can the computed answer be from our benchmark answer

        ! Scratch variables
        real :: scratch, upper, lower

        ! Loop index
        integer  :: i

        if (.not. allocd) then
            write(*,*) 'Memory has not been allocated, stopping'
            stop
        end if

        ! Set the outcome as positive until proven otherwise
        check = .true.

        ! Loop over all indices and check tensor addition
        do i=1, N
            scratch = A_h(i) + B_h(i)
            upper = scratch + eps_mult*abs(spacing(scratch))
            lower = scratch - eps_mult*abs(spacing(scratch))
            if (.not. ( (lower<=C_h(i)) .and. (C_h(i)<=upper) ) ) then
                write(*,*) "Error, tensor addition did not work at index = ", i, &
                    ", value was: ", C_h(i), ", but should be:", scratch
                check = .false.
                return
            end if
        end do

        ! We got to here because we didn't return on failure
        write(*,*) 'Tensor addition validated successfully.'
        
    end function check

    subroutine init_mem(N_in)
    
        !! Allocates memory for the tensors using calls to 
        !! C functions
    
        integer, intent(in) :: N_in
            !! Data type for the number of elements

        ! Temporary pointer
        type(c_ptr) :: temp_cptr

        ! Variable just for getting the type
        real :: temp_real

        ! Free memory first if already allocated
        if (allocd) then
            call free_mem
        end if

        ! Allocate memory for arrays using C functions
        temp_cptr = c_alloc(int(N*sizeof(temp_real), c_size_t))
        call c_f_pointer(temp_cptr, A_h, [N])
        
        temp_cptr = c_alloc(int(N*sizeof(temp_real), c_size_t))
        call c_f_pointer(temp_cptr, B_h, [N])

        temp_cptr = c_alloc(int(N*sizeof(temp_real), c_size_t))
        call c_f_pointer(temp_cptr, C_h, [N])
        
        ! Assign private variables if everything worked
        N = N_in
        allocd = .true.

    end subroutine init_mem

    subroutine kernel(i)
        !! Fortran kernel to compute tensor addition at index i
    
        integer, intent(in) :: i
            !! Index to compute the kernel at

        ! Run the C kernel function at element i in the array
        call c_kernel( &
            ! Get the pointer addresses
            c_loc(A_h(1)), & 
            c_loc(B_h(1)), &
            c_loc(C_h(1)), &
            ! Make sure the datatypes are correct
            int(i, c_int), &
            int(N, c_int) &
        )
        
    end subroutine kernel

    subroutine free_mem
    
        !! Free all memory allocated for the module

        ! De-allocate memory using calls to C functions
        call c_free(c_loc(A_h))
        call c_free(c_loc(B_h))
        call c_free(c_loc(C_h))

        ! Repoint pointers at null for safety
        A_h => null()
        B_h => null()
        C_h => null()

        allocd = .false.

    end subroutine free_mem

end module tensor_lib
