module tensor_lib
    !! Library module to work with tensors and HIP
    !! Written by Dr. Toby Potter and Dr. Joseph Schoonover

    ! Module to help us work with C datatypes and functions
    use iso_c_binding

    ! Use the ISO Fortran environment module
    use iso_fortran_env

    ! Use the Hipfort modules
    use hipfort
    use hipfort_check

    ! Make sure we do not use implicit typing rules
    implicit none

    ! Interface to launch_kernel_hip function
    ! in the file kernel_code.cpp
    interface
        ! A C function with void return type
        ! is regarded as a subroutine in Fortran 
        subroutine launch_kernel_hip(A, B, C, M, N) bind(C)
            use iso_c_binding
            ! Fortran passes arguments by reference as the default
            ! Arguments must have the "value" option present to pass by value
            ! Otherwise launch_kernel will receive pointers of type void**
            ! instead of void*
            type(c_ptr), intent(in), value :: A, B, C
            integer(c_int), intent(in), value :: M, N
        end subroutine
        
    end interface

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
            procedure :: fh_cptr => copy_from_host_cptr_tensor
            procedure :: fh_fptr_c_float_2D => copy_from_host_c_float_fptr_2D_tensor
            ! Download procedures
            procedure :: th_cptr => copy_to_host_cptr_tensor
            procedure :: th_fptr_c_float_2D => copy_to_host_c_float_fptr_2D_tensor
            ! Allocation and de-allocation processures
            procedure :: alloc => allocate_tensor
            procedure :: free => deallocate_tensor
            ! Generic procedures to have polymorphism
            generic :: copy_from => fh_cptr, fh_fptr_c_float_2D !, can specify more comma-separated functions here
            generic :: copy_to => th_cptr, th_fptr_c_float_2D !, can specify more comma-separated functions here
            ! Final is a cleanup function when the object goes out of scope
            final :: destroy_tensor
    end type 

    ! Have we already allocated memory?
    logical :: allocd = .false.

    ! Have we initialized HIP
    logical :: acquired = .false.

    ! Which device are we using on the GPU?
    integer :: device_id

    ! Number of elements in each dimension of the tensors
    integer :: M, N

    ! Tensors that reside on the GPU
    type(tensor_gpu) :: A_d, B_d, C_d

    ! Memory allocations (tensors) that reside on the host
    real(kind=c_float), dimension(:,:), pointer :: A_h, B_h, C_h

    ! Declare private any variables, functions, and subroutines
    ! that belong only to the module
    private :: allocd, M, N, device_id
    
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

    subroutine copy_from_host_cptr_tensor(this, host_cptr, nbytes)
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

    end subroutine copy_from_host_cptr_tensor

    subroutine copy_to_host_cptr_tensor(this, host_cptr, nbytes)
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

    end subroutine copy_to_host_cptr_tensor

    subroutine copy_from_host_c_float_fptr_2D_tensor(this, host_fptr)
        !! Copy from a pointer to 2D c_floats
        use iso_c_binding
        class(tensor_gpu), intent(inout) :: this
        real(kind=c_float), dimension(:,:), intent(in), pointer :: host_fptr
        call copy_from_host_cptr_tensor(this, c_loc(host_fptr), sizeof(host_fptr))
    end subroutine copy_from_host_c_float_fptr_2D_tensor

    subroutine copy_to_host_c_float_fptr_2D_tensor(this, host_fptr)
        !! Copy to a pointer to 2D c_floats
        use iso_c_binding
        class(tensor_gpu), intent(inout) :: this
        real(kind=c_float), dimension(:,:), intent(inout), pointer :: host_fptr
        call copy_to_host_cptr_tensor(this, c_loc(host_fptr), sizeof(host_fptr))
    end subroutine copy_to_host_c_float_fptr_2D_tensor


    subroutine init_gpu(dev_id)
        use hipfort
        use hipfort_check

        !! Initialise HIP and set GPU device to use

        ! The id of the device to use
        integer, intent(in) :: dev_id
        
        ! Number of compute devices
        integer :: ndevices

        ! Initialise resources the best practice way
        if (.not. acquired) then
            ! Initialise HIP
            call hipCheck(hipinit(0))

            ! We have now acquired HIP
            acquired = .true.
            
        end if
            
        ! Get the number of compute devices
        call hipCheck(hipgetdevicecount(ndevices))
            
        if ((dev_id .ge. 0) .and. (dev_id .lt. ndevices)) then
            ! Choose a compute device
            call hipCheck(hipsetdevice(dev_id))
        else
            write(error_unit,*) 'Error, dev_id was not inside the range of available devices.'
            stop 1
        end if

        ! Reset the GPU and all resources allocated on it
        call reset_gpu

        ! Set the device id for the GPU
        device_id = dev_id

    end subroutine init_gpu

    subroutine init_mem(M_in, N_in)
        use iso_fortran_env
        use hipfort
        use hipfort_check
    
        !! Allocate memory for the tensors 
        integer, intent(in) :: M_in, N_in
            !! Data type for the number of elements

        ! For error handling
        integer :: ierr

        ! Free memory first if already allocated
        if (allocd) then
            call free_mem
        end if

        ! Allocate host memory
        allocate(A_h(M_in,N_in), B_h(M_in,N_in), C_h(M_in,N_in), stat=ierr)

        if (ierr /= 0) then
            write(*,*) 'Error, array allocation failed with error code = ', ierr 
            stop 
        end if

        ! Allocate all tensors on the GPU
        call A_d%alloc(sizeof(A_h))
        call B_d%alloc(sizeof(B_h))
        call C_d%alloc(sizeof(C_h))

        ! Assign private variables if everything worked
        M = M_in
        N = N_in
        allocd = .true.

    end subroutine init_mem

    function check(eps_mult) result(success)
        !! Function to check the outcome of tensor addition
        !! only check the host arrays
        real, intent(in) :: eps_mult
            !! Epsilon multiplier, how many floating point spacings
            !! can the computed answer be from our benchmark answer

        ! Scratch variables
        real(kind=c_float) :: scratch, upper, lower

        ! Loop indices and error code
        integer  :: i, j

        ! Set the outcome as positive until proven otherwise
        logical :: success
        success = .true.

        if (.not. allocd) then
            write(*,*) 'Memory has not been allocated, stopping'
            stop
        end if

        ! Loop over all indices and check tensor addition
        do j=1, N
            do i=1, M
                scratch = A_h(i,j) + B_h(i,j)
                upper = scratch + eps_mult*abs(spacing(scratch))
                lower = scratch - eps_mult*abs(spacing(scratch))
                if (.not. ( (lower<=C_h(i,j) .and. (C_h(i,j)<=upper) ) )) then
                    write(*,*) "Error, tensor addition did not work at index = (", i, ", ", &
                        j, "), value was: ", C_h(i,j), ", but should be:", scratch
                    success = .false.
                    return
                end if
            end do
        end do

        ! We got to here because we didn't return on failure
        write(*,*) 'Tensor addition passed validation.'
        
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

        ! De-allocate memory
        call A_d%free
        call B_d%free
        call C_d%free

        ! Deallocate all tensors on the host
        deallocate(A_h, B_h, C_h)

        ! Nullify all host pointers
        nullify(A_h, B_h, C_h)

        ! Set private variables
        allocd = .false.
        M = 0
        N = 0

    end subroutine free_mem

    subroutine reset_gpu

        use hipfort
        use hipfort_check
    
        ! Release all resources on the gpu
        if (acquired) then
            call hipCheck(hipdevicereset())
        end if
    end subroutine reset_gpu

end module tensor_lib
