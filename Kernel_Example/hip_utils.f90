module hip_utils

    ! Module to help us work with C datatypes and functions
    use iso_c_binding

    ! Use the ISO Fortran environment module
    use iso_fortran_env

    ! Use the Hipfort modules
    use hipfort
    use hipfort_check

    ! Make sure we do not use implicit typing rules
    implicit none

    ! Have we initialized HIP?
    logical :: acquired = .false.

    ! Which device are we using?
    integer :: device_id = 0

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

    subroutine reset_gpu

        use hipfort
        use hipfort_check
    
        ! Release all resources on the gpu
        if (acquired) then
            call hipCheck(hipdevicereset())
        end if
    end subroutine reset_gpu

end module hip_utils