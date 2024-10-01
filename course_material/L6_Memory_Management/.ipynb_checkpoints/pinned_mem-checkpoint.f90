program pinned_mem
  
  use hipfort
  use hipfort_hiphostregister
  use hipfort_check
  use iso_c_binding
  
  implicit none
  
  integer, parameter :: N=1000
  integer, parameter :: M=5000

  real(c_float) :: real_element
  
  type(c_ptr) :: f_host
  type(c_ptr) :: f_device
  
  ! Allocate pinned host memory

  ! Latest API
  ! call hipcheck(hipHostMalloc(f_host, int(M*N*sizeof(real_element), c_size_t), hipHostMallocDefault))

  ! Deprecated but still portable API
  call hipcheck(hipMallocHost(f_host, int(M*N*sizeof(real_element), c_size_t)))
  
  ! Map pinned host memory into the memory space of the GPU
  call hipcheck(hipHostGetDevicePointer(f_device, f_host, 0))

  ! can now pass f_device to a kernel launch function
  
  ! Latest API
  !call hipcheck(hipHostFree(f_host))

  ! Deprecated but still portable API 
  call hipcheck(hipFreeHost(f_host))
  
  ! hipFree does not need to be called, but f_device must no longer be used.
  f_device = c_null_ptr


end program pinned_mem