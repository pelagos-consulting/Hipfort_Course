program memcpy_async

  use hipfort
  use hipfort_check
  use iso_c_binding
  
  implicit none

  real :: real_element
  
  integer, parameter :: N=1000
  integer, parameter :: M=5000
  
  type(c_ptr) :: f_host
  type(c_ptr) :: f_device
  type(c_ptr) :: stream
  
  ! Allocate pinned host memory
  integer(c_size_t) nbytes

  nbytes = M*N*sizeof(real_element)

  ! Latest API to allocate pinned host memory
  ! call hipcheck(hipHostMalloc(f_host, nbytes, hipHostMallocDefault))

  ! Deprecated but still portable API
  call hipcheck(hipMallocHost(f_host, nbytes))
  
  ! Allocate device memory
  call hipcheck(hipMalloc(f_device, nbytes))
  
  ! Create a stream
  call hipcheck(hipStreamCreate(stream))
  
  ! Initiate and asynchronous memory copy from host to device
  call hipcheck(hipMemcpyAsync(f_device, f_host, nbytes, hipMemcpyHostToDevice,stream))

  ! Wait on the stream to finish
  call hipcheck(hipStreamSynchronize(stream))

  ! Free memory (New API)
  !call hipcheck(hipHostFree(f_host))

  ! Free memory (deprecated API)
  call hipcheck(hipFreeHost(f_host))  
  
  call hipcheck(hipFree(f_device))

end program memcpy_async