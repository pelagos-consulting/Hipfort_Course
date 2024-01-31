module tensor

    ! Module to help us work with C datatypes and functions
    use iso_c_binding

    ! Use the ISO Fortran environment module
    use iso_fortran_env

    ! Use the Hipfort modules
    use hipfort
    use hipfort_check

    ! Make sure we do not use implicit typing rules
    implicit none

    type :: tensor_gpu
        !! Object to represent a tensor allocated on the GPU

        ! Is this tensor allocated?
        logical :: allocd
        ! Pointer to the memory
        type(c_ptr) :: mem
        ! Number of bytes in the allocation
        integer(c_size_t) :: nbytes
    
        contains
        
            ! Upload procedures
            procedure :: fh_cptr => copy_from_host_cptr
            procedure :: fh_fptr_c_float_2D => copy_from_host_c_float_fptr_2D
            ! Download procedures
            procedure :: th_cptr => copy_to_host_cptr
            procedure :: th_fptr_c_float_2D => copy_to_host_c_float_fptr_2D
            ! Allocation and de-allocation processures
            procedure :: alloc => allocate_tensor
            procedure :: free => deallocate_tensor
            ! Generic procedures to have polymorphism
            generic :: copy_from => fh_cptr, fh_fptr_c_float_2D !, can specify more comma-separated functions here
            generic :: copy_to => th_cptr, th_fptr_c_float_2D !, can specify more comma-separated functions here
            ! Final is a cleanup function when the object goes out of scope
            final :: destroy_tensor
    end type tensor_gpu

contains

    ! Functions for the tensor_gpu class
    subroutine allocate_tensor(this, nbytes)
        !! Allocate memory for a tensor on the GPU
        
        ! Import the Hip modules
        use hipfort
        use hipfort_check
    
        class(tensor_gpu), intent(inout) :: this
        integer(c_size_t) :: nbytes

        ! Check to make sure we are not already allocated
        if (this%allocd) then
            call deallocate_tensor(this)
        end if

        ! Now allocate memory for the tensor on the GPU
        call hipCheck(hipmalloc(this%mem, nbytes))

        ! Set the allocated flag
        this%allocd = .true.

        ! Set the number of bytes in the allocation
        this%nbytes = nbytes
        
    end subroutine allocate_tensor

    subroutine deallocate_tensor(this)

        !! De-allocate all memory allocations

        ! Import the Hip modules
        use hipfort
        use hipfort_check
    
        !! De-allocate memory for the tensor on the GPU
        class(tensor_gpu), intent(inout) :: this

        ! Free the memory if necessary
        if (this%allocd) then
            call hipCheck(hipfree(this%mem))
        endif

        ! Unset the allocated flag
        this%allocd = .false.

        ! Set the number of allocated bytes to 0
        this%nbytes = 0
        
    end subroutine deallocate_tensor

    subroutine destroy_tensor(this)
        !! Destructor
        type(tensor_gpu), intent(inout) :: this
        call this%free
    end subroutine destroy_tensor

    subroutine copy_from_host_cptr(this, host_cptr, nbytes)
        use iso_c_binding
        use iso_fortran_env
        use hipfort
        use hipfort_check

        ! The tensor_gpu object
        class(tensor_gpu), intent(inout) :: this

        ! Pointer to host memory
        type(c_ptr), intent(in) :: host_cptr
        
        ! Number of bytes in host memory
        integer(c_size_t), intent(in) :: nbytes

        ! Necessary checks
        if (.not. this%allocd) then
            write(error_unit, *) "Error, memory for tensor was not allocated on the GPU."
            stop
        end if

        if (nbytes/=this%nbytes) then
            write(error_unit, *) "Error, number of bytes uploaded =", nbytes, &
                " is not equal to the number of bytes allocated =", this%nbytes
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

        ! The tensor_gpu object
        class(tensor_gpu), intent(inout) :: this

        ! Pointer to host memory
        type(c_ptr), intent(in) :: host_cptr
        
        ! Number of bytes in the host memory
        integer(c_size_t), intent(in) :: nbytes

        ! Necessary checks
        if (.not. this%allocd) then
            write(error_unit, *) "Error, memory for tensor was not allocated on the GPU."
            stop
        end if

        if (nbytes/=this%nbytes) then
            write(error_unit, *) "Error, number of bytes uploaded =", nbytes, &
                " is not equal to the number of bytes allocated =", this%nbytes
            stop
        end if

        ! Now perform the copy
        call hipCheck(hipmemcpy(host_cptr, this%mem, &
                nbytes, hipmemcpydevicetohost))

    end subroutine copy_to_host_cptr

    subroutine copy_from_host_c_float_fptr_2D(this, host_fptr)
        !! Copy from a pointer to 2D c_floats
        use iso_c_binding
        class(tensor_gpu), intent(inout) :: this
        real(kind=c_float), dimension(:,:), intent(in), pointer :: host_fptr
        call copy_from_host_cptr_tensor(this, c_loc(host_fptr), sizeof(host_fptr))
    end subroutine copy_from_host_c_float_fptr_2D

    subroutine copy_to_host_c_float_fptr_2D(this, host_fptr)
        !! Copy to a pointer to 2D c_floats
        use iso_c_binding
        class(tensor_gpu), intent(inout) :: this
        real(kind=c_float), dimension(:,:), intent(inout), pointer :: host_fptr
        call copy_to_host_cptr_tensor(this, c_loc(host_fptr), sizeof(host_fptr))
    end subroutine copy_to_host_c_float_fptr_2D

end module tensor