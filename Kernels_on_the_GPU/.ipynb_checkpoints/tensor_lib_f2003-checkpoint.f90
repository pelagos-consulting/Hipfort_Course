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
        !! Object to represent a Tensor allocated on the GPU
    
        contains
            ! Is this tensor allocated?
            logical :: allocd
            ! Pointer to the memory
            type(c_ptr) :: mem
            ! Number of bytes in the allocation
            integer(c_size_t) :: nbytes
            ! Upload procedures
            procedure :: ur32_2D => upload_real32_2D_tensor
            ! Download procedures
            procedure :: dr32_2D => download_real32_2D_tensor
            ! Allocation and de-allocation processures
            procedure :: alloc => allocate_tensor
            procedure :: free => deallocate_tensor
            ! Generic procedures to have polymorphism
            generic :: upload => ur32_2D !, can specify more comma-separated functions here
            generic :: download => dr32_2D !, can specify more comma-separated functions here
            ! Final is a cleanup function when the object goes out of scope
            final :: destructor => deallocate_tensor
    end type 

    ! Have we already allocated memory?
    logical :: allocd = .false.

    ! Number of elements in each dimension of the tensors
    integer :: M, N

    ! Tensors that live on the GPU
    class(tensor_gpu) :: A_d, B_d, C_d

    ! Declare private variables functions and subroutines
    ! that belong only to the module
    private :: allocd, M, N
    

contains 

    ! Functions for the tensor_gpu class

    subroutine allocate_tensor(this, nbytes)
        !! Allocate memory on the GPU
        
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

    subroutine upload_real32_2D_tensor(this, A)
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
            hipCheck(hipmemcpy(this%mem, c_loc(A), &
                nbytes, hipmemcpyhosttodevice))
        else
            write(error_unit, *) 'Error, memory upload failed because it was too big for the allocation'
            stop 1
        end if

    end subroutine upload_real32_2D

    subroutine download_real32_2D_tensor(this, A)
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

    end subroutine download_real32_2D

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

    subroutine init_mem(M_in, N_in)
    
        !! Allocate memory for the tensors 
        integer, intent(in) :: M_in, N_in
            !! Data type for the number of elements

        ! Number of bytes in the allocation
        integer(c_size_t) :: nbytes

        ! Variable just for getting the type
        real(kind=real32) :: temp_real

        ! Number of bytes to allocate
        nbytes = M_in*N_in*sizeof(temp_real)

        ! Free memory first if already allocated
        if (allocd) then
            call free_mem
        end if

        ! Allocate all tensors
        A_d%allocate(nbytes)
        B_d%allocate(nbytes)
        C_d%allocate(nbytes)
        
        ! Assign private variables if everything worked
        M = M_in
        N = N_in
        allocd = .true.

    end subroutine init_mem

    function check(eps_mult) result(success)
        !! Function to check the outcome of tensor addition
        real, intent(in) :: eps_mult
            !! Epsilon multiplier, how many floating point spacings
            !! can the computed answer be from our benchmark answer

        ! Scratch variables
        real(kind=real32) :: scratch, upper, lower

        ! Temporary arrays on the host
        real(kind=real32), dimension(:,:), allocatable :: A, B, C

        ! Loop indices and error code
        integer  :: i, j, ierr

        ! Set the outcome as positive until proven otherwise
        logical :: success
        success = .true.

        if (.not. allocd) then
            write(*,*) 'Memory has not been allocated, stopping'
            stop
        end if

        ! Allocate host memory
        allocate(A(M,N), B(M,N), C(M,N), stat=ierr)

        if (ierr /= 0) then
            write(*,*) 'Error, array allocation failed with error code = ', ierr 
            stop 
        end if

        ! Download tensors into host memory
        A_d%download(A)
        B_d%download(B)
        C_d%download(C)

        ! Loop over all indices and check tensor addition
        do j=1, M
            do i=1, N
                scratch = A(i,j) + B(i,j)
                upper = scratch + eps_mult*abs(spacing(scratch))
                lower = scratch - eps_mult*abs(spacing(scratch))
                if (.not. ( (lower<=C(i,j) .and. (C(i,j)<=upper) ) ) then
                    write(*,*) "Error, tensor addition did not work at index = (", i, ", ", j &
                        "), value was: ", C(i,j), ", but should be:", scratch
                    success = .false.
                    return
                end if
            end do
        end do

        ! We got to here because we didn't return on failure
        write(*,*) 'Tensor addition passed validation.'

        ! Free temporary arrays
        deallocate(A,B,C)
        
    end function check

    subroutine launch_kernel
        !! Call the function to execute the HIP kernel
        call launch_kernel_hip( &
            A_d%mem, &
            B_d%mem, &
            C_d%mem, &
            int(M, c_int), &
            int(N, c_int) &
        )
        
    end subroutine launch_kernel

    subroutine free_mem
    
        !! Free all memory allocated for the module

        ! De-allocate memory using calls to C functions
        A_d%free
        B_d%free
        C_d%free

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