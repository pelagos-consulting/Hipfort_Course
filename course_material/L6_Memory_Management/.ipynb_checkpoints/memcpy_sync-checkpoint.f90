program memcpy_sync

  use hipfort
  use hipfort_check
  
  implicit none
  
  integer, parameter :: N=1000
  integer, parameter :: M=5000
  real, pointer, dimension(:,:) :: f_host
  real, pointer, dimension(:,:) :: f_device
  
  allocate(f_host(1:N,1:M))
  ! Allocate memory on the device
  call hipcheck(hipMalloc(f_device, mold=f_host))

  ! Copy memory from the host to the device
  call hipcheck(hipMemcpy(f_device,f_host,hipMemcpyHostToDevice))

  ! Copy memory from the device to the host
  call hipcheck(hipMemcpy(f_host,f_device,hipMemcpyDeviceToHost))

  ! Free memory on the host
  deallocate(f_host)

  ! Free memory on the device
  call hipcheck(hipFree(f_device))

end program memcpy_sync