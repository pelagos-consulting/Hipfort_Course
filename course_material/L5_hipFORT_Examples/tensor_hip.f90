module tensor_hip
    !! Module to work with HIP memory allocations 
    !! with a measure of memory safety
    !! Written by Dr. Toby Potter and Dr. Joseph Schoonover

    ! Module to help us work with C datatypes and functions
    use iso_c_binding

    ! Use the ISO Fortran environment module
    use iso_fortran_env

    ! Use the kinds module to bring in the float_type kind 
    use kinds

    ! Use the Hipfort modules
    use hipfort
    use hipfort_check

    ! Make sure we do not use implicit typing rules
    implicit none

    type :: tensor
        !! Object to represent a tensor allocated on the GPU

        ! Is this tensor allocated?
        logical :: allocd = .false.
        
        ! Pointer to the memory
        type(c_ptr) :: mem = c_null_ptr
        
        ! Number of bytes in the allocation
        integer(c_size_t) :: nbytes = 0
    
        contains
        
            ! Upload procedures
            procedure :: copy_from_host_cptr
            procedure :: copy_from_host_float_type_1
            procedure :: copy_from_host_float_type_2
            
            ! Download procedures
            procedure :: copy_to_host_cptr
            procedure :: copy_to_host_float_type_1
            procedure :: copy_to_host_float_type_2
            
            ! Allocation and de-allocation procedures
            procedure :: malloc
            procedure :: free
            
            ! Generic procedures for different types of data
            generic :: copy_from => copy_from_host_cptr, &
                copy_from_host_float_type_1, &
                copy_from_host_float_type_2 !, can specify more comma-separated functions here
            generic :: copy_to => copy_to_host_cptr, &
                copy_to_host_float_type_1, &
                copy_to_host_float_type_2 !, can specify more comma-separated functions here
            
            ! Final is a cleanup function when the object goes out of scope
            final :: destructor
            
    end type tensor

contains

    ! Functions for the tensor class
    subroutine malloc(this, nbytes)
        !! Allocate memory for a tensor on the GPU
        
        ! Import the HIP modules
        use hipfort
        use hipfort_check

        ! Polymorphic variable for the class
        class(tensor), intent(inout) :: this

        ! Number of bytes to allocate
        integer(c_size_t), intent(in) :: nbytes

        ! Check to make sure we are not already allocated
        if (this%allocd) then
            call this%free
        end if

        ! Now allocate memory for the tensor on the GPU
        call hipCheck(hipmalloc(this%mem, nbytes))

        ! Set the allocated flag
        this%allocd = .true.

        ! Set the number of bytes in the allocation
        this%nbytes = nbytes
        
    end subroutine malloc

    subroutine free(this)
        !! Free all memory in the allocation

        ! Import the HIP modules
        use hipfort
        use hipfort_check
    
        !! De-allocate memory for the tensor on the GPU
        class(tensor), intent(inout) :: this

        ! Free the memory if necessary
        if (this%allocd) then
            call hipCheck(hipfree(this%mem))
        endif

        ! Unset the allocated flag
        this%allocd = .false.

        ! Set the number of allocated bytes to 0
        this%nbytes = 0
        
    end subroutine free

    subroutine destructor(this)
        !! Destructor, `this` must be of type(tensor) because it is valid only for instances
        !! of this type
        type(tensor), intent(inout) :: this
        call this%free
    end subroutine destructor

    subroutine copy_from_host_cptr(this, host_cptr, nbytes)
        use iso_c_binding
        use iso_fortran_env
        use hipfort
        use hipfort_check

        ! The tensor object
        class(tensor), intent(inout) :: this

        ! Pointer to host memory
        type(c_ptr), intent(in) :: host_cptr
        
        ! Number of bytes in host memory
        integer(c_size_t), intent(in) :: nbytes

        ! Necessary checks
        if (.not. this%allocd) then
            write(error_unit, *) "Error, tensor was not allocated."
            stop
        end if

        if (nbytes>this%nbytes) then
            write(error_unit, *) "Error, number of bytes to copy from =", nbytes, &
                " is greater than the number of bytes allocated =", this%nbytes
            stop
        end if

        ! Now perform the copy
        call hipCheck(hipmemcpy(this%mem, host_cptr, &
                nbytes, hipmemcpyhosttodevice))

    end subroutine copy_from_host_cptr

    subroutine copy_to_host_cptr(this, host_cptr, nbytes)
        use iso_c_binding
        use iso_fortran_env
        use hipfort
        use hipfort_check

        ! The tensor object
        class(tensor), intent(inout) :: this

        ! Pointer to host memory
        type(c_ptr), intent(in) :: host_cptr
        
        ! Number of bytes in the host memory
        integer(c_size_t), intent(in) :: nbytes

        ! Necessary checks
        if (.not. this%allocd) then
            write(error_unit, *) "Error, tensor was not allocated."
            stop
        end if

        if (nbytes<this%nbytes) then
            write(error_unit, *) "Error, number of bytes to copy to =", nbytes, &
                " is less than the number of bytes allocated =", this%nbytes
            stop
        end if

        ! Now perform the copy
        call hipCheck(hipmemcpy(host_cptr, this%mem, &
                this%nbytes, hipmemcpydevicetohost))

    end subroutine copy_to_host_cptr

    ! Generic procedures for copy_from, and copy_to

    subroutine copy_from_host_float_type_1(this, host_fptr)
        !! Copy from a 1D Fortran pointer float_types
        use iso_c_binding
        ! Use class(tensor) for `this` so it also can also be any derived type
        class(tensor), intent(inout) :: this
        real(kind=float_type), dimension(:), intent(in), pointer :: host_fptr
        call this%copy_from(c_loc(host_fptr), int(sizeof(host_fptr), c_size_t))
    end subroutine copy_from_host_float_type_1

    subroutine copy_to_host_float_type_1(this, host_fptr)
        !! Copy to a 1D Fortran pointer to float_types
        use iso_c_binding
        ! Use class(tensor) for `this` so it also can also be any derived type
        class(tensor), intent(inout) :: this
        real(kind=float_type), dimension(:), intent(inout), pointer :: host_fptr
        call this%copy_to(c_loc(host_fptr), int(sizeof(host_fptr), c_size_t))
    end subroutine copy_to_host_float_type_1

    subroutine copy_from_host_float_type_2(this, host_fptr)
        !! Copy from a 2D Fortran pointer to float_types
        use iso_c_binding
        ! Use class(tensor) for `this` so it also can also be any derived type
        class(tensor), intent(inout) :: this
        real(kind=float_type), dimension(:,:), intent(in), pointer :: host_fptr
        call this%copy_from(c_loc(host_fptr), int(sizeof(host_fptr), c_size_t))
    end subroutine copy_from_host_float_type_2

    subroutine copy_to_host_float_type_2(this, host_fptr)
        !! Copy to a 2D Fortran pointer to float_types
        use iso_c_binding
        ! Use class(tensor) for `this` so it also can also be any derived type
        class(tensor), intent(inout) :: this
        real(kind=float_type), dimension(:,:), intent(inout), pointer :: host_fptr
        call this%copy_to(c_loc(host_fptr), int(sizeof(host_fptr), c_size_t))
    end subroutine copy_to_host_float_type_2

end module tensor_hip
