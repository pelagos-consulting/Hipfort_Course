program pinned_mem
  
  use hipfort
  use hipfort_hiphostregister
  use hipfort_check
  use iso_c_binding
  
  implicit none
  
  integer, parameter :: N=1000
  integer, parameter :: M=5000
  
  real, pointer, dimension(:) :: f_host
  type(c_ptr) :: f_device
  
  ! Allocate pinned host memory
  call hipcheck(hipHostMalloc(f_host, M*N, hipHostMallocDefault))
  
  ! Map pinned host memory into the memory space of the GPU
  call hipcheck(hipHostGetDevicePointer(f_device, c_loc(f_host(1)), 0))

  ! can now pass f_device to a kernel
  
  call hipcheck(hipHostFree(f_host))
  
  ! hipFree does not need to be called, but f_device must no longer be used.

end program pinned_mem