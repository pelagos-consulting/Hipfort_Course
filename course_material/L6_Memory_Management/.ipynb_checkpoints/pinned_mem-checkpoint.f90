program pinned_mem
  
  use hipfort
  !use hipfort_hiphostregister
  use hipfort_check
  use iso_c_binding
  
  implicit none
  
  integer, parameter :: N=1000
  integer, parameter :: M=5000

  real(c_float) :: real_element

  ! Define C pointers
  type(c_ptr) :: f_host_cptr
  type(c_ptr) :: f_device

  ! Try using a Fortran pointer
  real(c_float), pointer, dimension(:,:) :: f_host_fptr
  
  ! Allocate pinned host memory

  ! Latest API with C pointer
  !call hipcheck(hipHostMalloc(f_host_cptr, int(M*N*sizeof(real_element), c_size_t), hipHostMallocDefault))

  ! Latest API with Fortran pointer
  ! call hipcheck(hipHostMalloc(f_host_fptr, M, N, hipHostMallocDefault))

  ! Deprecated but still portable API to CUDA
  call hipcheck(hipMallocHost(f_host_cptr, int(M*N*sizeof(real_element), c_size_t)))
  
  ! Map pinned host memory into the memory space of the GPU
  call hipcheck(hipHostGetDevicePointer(f_device, f_host_cptr, 0))

  ! can now pass f_device to a kernel launch function
  
  ! Latest API with C and Fortran
  !call hipcheck(hipHostFree(f_host_cptr))
  !call hipcheck(hipHostFree(f_host_fptr))

  ! Deprecated but still portable API 
  call hipcheck(hipFreeHost(f_host_cptr))
  
  ! hipFree does not need to be called, but f_device must no longer be used.
  f_device = c_null_ptr


end program pinned_mem