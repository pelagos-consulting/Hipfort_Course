program paged_mem

  use hipfort
  use hipfort_check
  use iso_c_binding
  
  implicit none
  
  integer, parameter :: N=1000
  integer, parameter :: M=5000
  real, pointer, dimension(:,:) :: f_host
  type(c_ptr) :: f_device
  
  allocate(f_host(1:N,1:M))
  
  call hipcheck(hipMalloc(f_device, int(sizeof(f_host),c_size_t)))
  
  !...
  
  deallocate(f_host)
  call hipcheck(hipFree(f_device))

end program paged_mem
