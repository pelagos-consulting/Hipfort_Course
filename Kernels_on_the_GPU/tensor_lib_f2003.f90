module tensor_lib
    !! Library module to work with tensors and HIP
    !! Written by Dr. Toby Potter and Dr. Joseph Schoonover

    ! Module to help us working with C code
    use iso_c_binding

    ! Use the ISO Fortran environment module
    use iso_fortran_env

    ! Use the Hipfort modules
    use hipfort
    use hipfort_check

    implicit none

    ! Interface to launch_kernel function
    interface
        ! Fortran regards a C function with void return type
        ! as a subroutine 
        ! This is the fortran interface to the C function
        subroutine launch_kernel_hip(A, B, C, M, N) bind(C)
            use iso_c_binding
            ! Fortran passes by reference as the default
            ! Must have the "value" option present to pass by value
            ! Otherwise launch_kernel will receive pointers of type void**
            ! instead of void*
            type(c_ptr), value :: A, B, C
            integer(c_int), value :: M, N
        end subroutine
        
    end interface

    type :: tensor_gpu
        contains
            logical :: allocd
            type(c_ptr) :: mem
            integer(c_size_t) :: nbytes
            ! Upload functions
            procedure :: ur32_2D => upload_real32_2D
            ! Download functions
            procedure :: dr32_2D => download_real32_2D
            procedure :: alloc => allocate_tensor
            procedure :: free => deallocate_tensor
            generic :: upload => ur32_2D !, can specify more functions here
            generic :: download => dr32_2D !, can specify more functions here
            ! Cleanup
            final :: destructor => dealloc
    end type 

    ! Have we already allocated memory and acquired the GPU?
    logical :: allocd = .false.
    logical :: acquired = .false.

    ! Number of elements in each dimension
    integer :: M, N, device_id

    ! Pointers to memory on the compute device
    type(c_ptr) :: A_d=c_null_ptr, B_d=c_null_ptr, C_d=c_null_ptr

    ! Declare private variables functions and subroutines
    ! that belong only to the module
    private :: allocd, M, N, upload, download

contains 

    subroutine allocate_tensor(this, nbytes)
        !! Allocate memory for the tensor on the GPU
        
        ! Import the Hip modules
        use hipfort
        use hipfort_check
    
        class(tensor_gpu), intent(in) :: this
        integer(c_size_t) :: nbytes

        if (this%allocd) then
            call deallocate_tensor(this)
        end if

        ! Now allocate memory for the tensor on the GPU
        hipCheck(hipmalloc(this%mem, nbytes))

        ! Set the allocated flag
        this%allocd = .true.
        
    end subroutine allocate_tensor

    subroutine deallocate_tensor(this)

        ! Import the Hip modules
        use hipfort
        use hipfort_check
    
        !! De-allocate memory for the tensor on the GPU
        class(tensor_gpu), intent(in) :: this

        ! Free the memory
        call hipCheck(hipfree(this%mem))

        ! Unset the allocated flag
        this%allocd = .false.
        
    end subroutine deallocate_tensor

    subroutine upload_real32_2D(this, A)
        use iso_fortran_env
        use hipfort
        use hipfort_check
        use iso_c_binding
    
        !! Upload a 2D array to the tensor
        class(tensor_gpu), intent(in) :: this

        !! Input array
        real(kind=real32), dimension(:,:), intent(in) :: A

        integer(c_size_t) :: nbytes
        nbytes = sizeof(A)

        if (nbytes<=this%nbytes) then
            hipCheck(hipmemcpy(this%mem, c_loc(A), nbytes, hipmemcpyhosttodevice))
        else
            write(error_unit, *) 'Error, memory upload failed because it was too big for the allocation'
            stop 1
        end if

    end subroutine upload_real32_2D

    subroutine download_real32_2D(this, A)
        use iso_fortran_env
        use hipfort
        use hipfort_check
        use iso_c_binding
    
        !! Upload a 2D array to the tensor
        class(tensor_gpu), intent(in) :: this

        !! Input array
        real(kind=real32), dimension(:,:), intent(out) :: A

        integer(c_size_t) :: nbytes
        nbytes = min(sizeof(A), this%nbytes)

        ! Copy the minimum of the contents of the array or the GPU allocation
        hipCheck(hipmemcpy(c_loc(A), this%mem, nbytes, hipmemcpydevicetohost))

    end subroutine upload_real32_2D


    subroutine get_gpu(dev_id)

        ! The id of the device to use
        integer, intent(in) :: dev_id
        
        ! Number of compute devices
        integer :: ndevices

        ! Initialise resources the best practice way
        if (.not. acquired) then
            
            ! Initialise HIP
            call hipCheck(hipinit(0))
            
            ! Get the number of compute devices
            call hipCheck(hipgetdevicecount(ndevices))
            
            if ((dev_id .ge. 0) .and. (dev_id .lt. ndevices)) then
               ! Choose a compute device
               call hipCheck(hipsetdevice(dev_id))
            else
                write(error_unit,*) 'Error, dev_id was not inside the range of available devices.'
                stop 1
            end if

            ! We have now acquired the GPU
            acquired = .true.

        end if

        ! Reset the GPU and all resources allocated on it
        call reset_gpu

        ! Set the device id for the GPU
        device_id = dev_id

    end subroutine get_gpu

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

    subroutine init_mem(M_in, N_in)
    
        !! Allocates memory for the tensors on the GPU
    
        integer, intent(in) :: M_in, N_in
            !! Data type for the number of elements

        ! Variable just for getting the type
        real(kind=real32) :: temp_real

        ! Free memory first if already allocated
        if (allocd) then
            call free_mem
        end if

        ! Allocate memory on the compute device 
        hipCheck(hipmalloc(A_d, int(M_in*N_in*sizeof(temp_real), c_size_t)))
        hipCheck(hipmalloc(A_d, int(M_in*N_in*sizeof(temp_real), c_size_t)))
        hipCheck(hipmalloc(A_d, int(M_in*N_in*sizeof(temp_real), c_size_t)))
        
        ! Assign private variables if everything worked
        M = M_in
        N = N_in
        allocd = .true.

    end subroutine init_mem

    subroutine launch_kernel
        !! Call the C kernel launcher to execute the c_kernel
        !! Function at every point 

        call launch_kernel_hip( &
            c_loc(A_h), &
            c_loc(B_h), &
            c_loc(C_h), &
            int(N, c_int) &
        )
        
    end subroutine launch_kernel

    subroutine free_mem
    
        !! Free all memory allocated for the module

        ! De-allocate memory using calls to C functions
        call hipCheck(hipfree(A_d))
        call hipCheck(hipfree(B_d))
        call hipCheck(hipfree(C_d))

        ! Set all pointers to null
        A_d = c_null_ptr
        B_d = c_null_ptr
        C_d = c_null_ptr

        ! Set private variables
        allocd = .false.
        M = 0
        N = 0

    end subroutine free_mem

    subroutine reset_gpu
        ! Release all resources on the gpu
        if (acquired) then
            call hipCheck(hipdevicereset())
        end if
    end subroutine reset_gpu

end module tensor_lib