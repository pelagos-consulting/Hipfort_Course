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

    subroutine init_device(dev_id)
        use hipfort
        use hipfort_check

        !! Initialise HIP and set device device to use

        ! The id of the device to use
        integer, intent(in) :: dev_id
        
        ! Number of compute devices
        integer :: ndevices

        ! Initialise resources the best practice way
        if (.not. acquired) then

#ifdef __HIP_PLATFORM_AMD__
            ! Initialise HIP
            call hipcheck(hipinit(0))
#endif

            ! We have now acquired HIP
            acquired = .true.
            
        end if
            
        ! Get the number of compute devices
        call hipcheck(hipgetdevicecount(ndevices))
            
        if ((dev_id .ge. 0) .and. (dev_id .lt. ndevices)) then
            ! Choose a compute device
            call hipcheck(hipsetdevice(dev_id))
        else
            write(error_unit,*) 'Error, dev_id was not inside the range of available devices.'
            stop 1
        end if

        ! Set the device id for the device
        device_id = dev_id

    end subroutine init_device

    subroutine reset_device

        use hipfort
        use hipfort_check
    
        ! Release all resources on the device
        if (acquired) then
            ! Make sure the device is finished
            ! with all pending activity
            call hipcheck(hipdevicesynchronize())

            ! Now free all resources on the primary context
            ! of the selected device
            call hipcheck(hipdevicereset())
        end if
    end subroutine reset_device

    function supports_managed_memory(dev_id) result(decision)
        use hipfort
        use hipfort_check
        use iso_c_binding

        !! check if the device `dev_id` supports managed memory

        ! The id of the device to use
        integer, intent(in) :: dev_id

        logical :: decision

        ! Structure to store GPU properties
        type(hipdeviceprop_t) :: prop

        ! Get device properties
        call hipcheck(hipgetdeviceproperties(prop, dev_id))        

        ! Check if managed memory is supported on dev_id
        ! this method is not cross-platform
        !call hipcheck(hipDeviceGetAttribute(c_loc(supported), &
        !    hipDeviceAttributeManagedMemory,dev_id)) 

        ! Default answer
        decision = .false. 

        if (prop%managedmemory /= 0) then
            decision = .true. ! convert to logical and output
        endif

    end function supports_managed_memory


end module hip_utils