program managed_mem
  
    use hipfort
    use hipfort_check
    use iso_c_binding
    use iso_fortran_env
    
    use hip_utils
    use kinds
    
    implicit none
    
    real(float_type), pointer, dimension(:,:) :: f, x
    
    type(c_ptr) :: stream

    integer, parameter :: M = 10
    integer, parameter :: N = 50

    ! Choose device 0 by default
    integer :: dev_id = 0

    ! Initialise the compute device
    call init_device(dev_id)

    if (.not. supports_managed_memory(dev_id)) then
        write (error_unit, *) "Sorry, device, ", dev_id, " does not support managed memory."
        stop
    endif

    ! Allocate managed memory 
    call hipcheck(hipMallocManaged(f, dims=(/M, N/), flags=hipMemAttachGlobal))
    call hipcheck(hipMallocManaged(x, dims=(/M, N/), flags=hipMemAttachGlobal))

#ifdef __HIP_PLATFORM_AMD__ 
    ! Set coarse-grained coherence for f on device id 0
    call hipcheck(hipMemAdvise(c_loc(f), int(sizeof(f), c_size_t), hipMemAdviseSetCoarseGrain, dev_id))
    
    ! Set coarse-grained coherence for x on device id 0
    call hipcheck(hipMemAdvise(c_loc(x), int(sizeof(x), c_size_t), hipMemAdviseSetCoarseGrain, dev_id))
#endif

    ! Create the stream
    call hipcheck(hipStreamCreate(stream))
    
    ! Pre-fetch `x` to device id 0 on stream 0
    call hipcheck(hipMemPrefetchAsync(c_loc(x), int(sizeof(x), c_size_t), dev_id,stream))
    
    ! call device_subroutine(f, x, stream)
    
    call hipcheck(hipDeviceSynchronize())
    
    ! call host_subroutine(f, x)

    ! Free up memory
    call hipcheck(hipFree(f))
    call hipcheck(hipFree(x))

    ! Release all resources and reset the compute device
    call reset_device()

end program managed_mem
