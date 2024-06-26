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

    ! Use the tensor type from tensor_hip
    use tensor_hip

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

    ! Have we already allocated memory?
    logical :: allocd = .false.

    ! Have we initialized HIP
    logical :: acquired = .false.

    ! Which device are we using on the GPU?
    integer :: device_id

    ! Number of elements in each dimension of the tensors
    integer :: M, N

    ! Tensors that reside on the GPU
    type(tensor) :: A_d, B_d, C_d

    ! Memory allocations (tensors) that reside on the host
    real(kind=c_float), dimension(:,:), pointer :: A_h, B_h, C_h

    ! Declare private any variables, functions, and subroutines
    ! that belong only to the module
    private :: allocd, M, N, device_id
    
contains 

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
        call A_d%malloc(sizeof(A_h))
        call B_d%malloc(sizeof(B_h))
        call C_d%malloc(sizeof(C_h))

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
