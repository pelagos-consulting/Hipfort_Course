module tensor_lib
    !! Library module to work with tensors
    !! Written by Dr. Toby Potter and Dr. Joseph Schoonover

    ! Module to help us working with C code
    use iso_c_binding

    ! Use the ISO Fortran environment module
    use iso_fortran_env

    implicit none

    ! Interface to C kernel functions
    interface
    
        ! Fortran regards a C function with void return type
        ! as a subroutine 
        ! This is the Fortran interface to the C function
        subroutine launch_c_kernel(A, B, C, N) bind(C)
            use iso_c_binding
            ! Fortran passes by reference as the default
            ! Must have the "value" option present to pass by value
            ! Otherwise ckernel will receive pointers of type void**
            ! instead of void*
            type(c_ptr), value :: A, B, C
            integer(c_int), value :: N
        end subroutine

        ! C function to allocate memory
        function c_alloc(nbytes) result(ptr) bind(C)
            use iso_c_binding
            ! Make sure we have the value option set
            ! to pass by value
            integer(c_size_t), intent(in), value :: nbytes
            type(c_ptr) :: ptr
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
    real(kind=c_float), pointer, dimension(:) :: A_h => null(), B_h => null(), C_h => null()

    ! Declare private variables functions and subroutines
    ! that belong only to the module
    private :: allocd, N

contains 

    function check(eps_mult) result(success)
        !! Function to check tensor addition

        real, intent(in) :: eps_mult
            !! Epsilon multiplier, how many floating point spacings
            !! can the computed answer be from our benchmark answer

        ! Scratch variables
        real(kind=real32) :: scratch, upper, lower

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
    
        !! Allocates memory for the tensors using calls to 
        !! C functions
    
        integer, intent(in) :: N_in
            !! Data type for the number of elements

        ! Temporary pointer
        type(c_ptr) :: temp_cptr

        ! Variable just for getting the type
        real(kind=real32) :: temp_real

        ! Free memory first if already allocated
        if (allocd) then
            call free_mem
        end if

        ! Allocate memory for arrays using C functions
        temp_cptr = c_alloc(int(N_in*sizeof(temp_real), c_size_t))
        ! Translate the C pointer to a Fortran pointer
        call c_f_pointer(temp_cptr, A_h, [N_in])
        
        temp_cptr = c_alloc(int(N_in*sizeof(temp_real), c_size_t))
        ! Translate the C pointer to a Fortran pointer
        call c_f_pointer(temp_cptr, B_h, [N_in])

        temp_cptr = c_alloc(int(N_in*sizeof(temp_real), c_size_t))
        ! Translate the C pointer to a Fortran pointer
        call c_f_pointer(temp_cptr, C_h, [N_in])
        
        ! Assign private variables if everything worked
        N = N_in
        allocd = .true.

    end subroutine init_mem

    subroutine launch_kernel
        !! Call the C kernel launcher to execute the c_kernel
        !! Function at every point 

        call launch_c_kernel( &
            c_loc(A_h), &
            c_loc(B_h), &
            c_loc(C_h), &
            int(N, c_int) &
        )
        
    end subroutine launch_kernel

    subroutine free_mem
    
        !! Free all memory allocated for the module

        ! De-allocate memory using calls to C functions
        call c_free(c_loc(A_h))
        call c_free(c_loc(B_h))
        call c_free(c_loc(C_h))

        ! Repoint pointers at null for safety
        nullify(A_h, B_h, C_h)

        ! Set private variables
        allocd = .false.
        N = 0

    end subroutine free_mem

end module tensor_lib
