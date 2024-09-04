program managed_mem
  
    use hipfort
    use hipfort_check
    use iso_c_binding
    use iso_fortran_env
    
    use hip_utils
    
    implicit none
    
    real,pointer,dimension(:) :: f, x
    
    type(c_ptr) :: stream
    
    integer, parameter :: N = 1000

    ! Choose device 0 by default
    integer :: dev_id = 0

    ! Initialise the compute device
    call init_device(dev_id)

    if (.not. supports_managed_memory(dev_id)) then
        write (error_unit, *) "Sorry, device, ", dev_id, " does not support managed memory."
        stop
    endif

    ! Allocate managed memory 
    call hipcheck(hipMallocManaged(f,N,hipMemAttachGlobal))
    call hipcheck(hipMallocManaged(x,N,hipMemAttachGlobal))

#ifdef __HIP_PLATFORM_AMD__ 
    ! Set coarse-grained coherence for f on device id 0
    call hipcheck(hipMemAdvise(c_loc(f),sizeof(f),hipMemAdviseSetCoarseGrain,dev_id))
    
    ! Set coarse-grained coherence for x on device id 0
    call hipcheck(hipMemAdvise(c_loc(x),sizeof(x),hipMemAdviseSetCoarseGrain,dev_id))
#endif

    ! Create the stream
    call hipcheck(hipStreamCreate(stream))
    
    ! Pre-fetch `x` to device id 0 on stream 0
    call hipcheck(hipMemPrefetchAsync(c_loc(x),sizeof(x),dev_id,stream))
    
    ! call device_subroutine(f, x)
    
    call hipcheck(hipDeviceSynchronize())
    
    ! call host_subroutine(f, x)

    ! Release all resources and reset the compute device
    call reset_device()

end program managed_mem