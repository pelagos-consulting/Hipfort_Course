module memcpy_bench_tools

use iso_fortran_env
use iso_c_binding
use hipfort
use hipfort_check

implicit none

  interface to_char
    module procedure :: real64_to_char
    module procedure :: int_to_char
  end interface to_char

contains

function real64_to_char( val ) result( val_as_char )
  implicit none 
  real(real64) :: val
  character(24) :: val_as_char

  write (val_as_char, "(E10.4)") val

end function real64_to_char

function int_to_char( val ) result( val_as_char )
  implicit none 
  integer :: val
  character(24) :: val_as_char

  write (val_as_char, "(I10)") val

end function int_to_char

subroutine float32_pageable_memcpy_h2d( array_length, nreps, walltime, bandwidth_gb_s )
!! This subroutine does the following actions
!!  * allocates pageable memory on the host using a real32 fortran pointer of size array_length
!!  * allocates a device pointer using a type(c_ptr) of the same size
!!  * performs "nreps" host-to-device memory copies and measures wall time
!!  * calculates per memcpy wall time and bandwidth in units of gb/s
!!
  implicit none
  integer, intent(in) :: array_length
  integer, intent(in) :: nreps 
  real(real64), intent(out) :: walltime
  real(real64), intent(out) :: bandwidth_gb_s
  ! Local
  real(real32), pointer :: host_data(:)
  type(c_ptr) :: dev_data
  integer :: i
  real(real64) :: t1
  real(real64) :: t2

  allocate(host_data(1:array_length))

  call hipcheck(hipmalloc(dev_data,sizeof(host_data)))

  call cpu_time(t1)
  do i = 1, nreps
  
    call hipcheck(hipmemcpy(dev_data, &
                            c_loc(host_data), &
                            sizeof(host_data), &
                            hipMemcpyHostToDevice))
  enddo
  call cpu_time(t2)

  walltime = (t2-t1)/real(nreps,real64)
  bandwidth_gb_s = real(sizeof(host_data),real64)/walltime/1.0E9

  call hipcheck(hipfree(dev_data))
  deallocate(host_data)

end subroutine float32_pageable_memcpy_h2d

subroutine float32_pageable_memcpy_d2h( array_length, nreps, walltime, bandwidth_gb_s )
!! This subroutine does the following actions
!!  * allocates pageable memory on the host using a real32 fortran pointer of size array_length
!!  * allocates a device pointer using a type(c_ptr) of the same size
!!  * performs "nreps" device-to-host memory copies and measures wall time
!!  * calculates per memcpy wall time and bandwidth in units of gb/s
!!
  implicit none
  integer, intent(in) :: array_length
  integer, intent(in) :: nreps 
  real(real64), intent(out) :: walltime
  real(real64), intent(out) :: bandwidth_gb_s
  ! Local
  real(real32), pointer :: host_data(:)
  type(c_ptr) :: dev_data
  integer :: i
  real(real64) :: t1
  real(real64) :: t2

  allocate(host_data(1:array_length))

  call hipcheck(hipmalloc(dev_data,sizeof(host_data)))

  call cpu_time(t1)
  do i = 1, nreps
  
    call hipcheck(hipmemcpy(c_loc(host_data), &
                            dev_data, &
                            sizeof(host_data), &
                            hipMemcpyDeviceToHost))
  enddo
  call cpu_time(t2)

  walltime = (t2-t1)/real(nreps,real64)
  bandwidth_gb_s = real(sizeof(host_data),real64)/walltime/1.0E9

  call hipcheck(hipfree(dev_data))
  deallocate(host_data)

end subroutine float32_pageable_memcpy_d2h

subroutine float32_pinned_memcpy_h2d( array_length, nreps, walltime, bandwidth_gb_s )
!! This subroutine does the following actions
!!  * allocates pinned memory on the host using a real32 fortran pointer of size array_length
!!  * allocates a device pointer using a type(c_ptr) of the same size
!!  * performs "nreps" host-to-device memory copies and measures wall time
!!  * calculates per memcpy wall time and bandwidth in units of gb/s
!!
  implicit none
  integer, intent(in) :: array_length
  integer, intent(in) :: nreps 
  real(real64), intent(out) :: walltime
  real(real64), intent(out) :: bandwidth_gb_s
  ! Local
  real(real32), pointer :: host_data(:)
  type(c_ptr) :: dev_data
  integer :: i
  integer(c_size_t) :: array_size
  real(real64) :: t1
  real(real64) :: t2

  array_size = array_length*real32

  call hipcheck(hipHostMalloc(host_data,array_length,hipHostMallocDefault))

  call hipcheck(hipmalloc(dev_data,array_size))

  call cpu_time(t1)
  do i = 1, nreps
  
    call hipcheck(hipmemcpy(dev_data, &
                            c_loc(host_data), &
                            array_size, &
                            hipMemcpyHostToDevice))
  enddo
  call cpu_time(t2)

  walltime = (t2-t1)/real(nreps,real64)
  bandwidth_gb_s = real(array_size,real64)/walltime/1.0E9

  call hipcheck(hipfree(dev_data))
  call hipcheck(hipHostFree(host_data))

end subroutine float32_pinned_memcpy_h2d

subroutine float32_pinned_memcpy_d2h( array_length, nreps, walltime, bandwidth_gb_s )
!! This subroutine does the following actions
!!  * allocates pinned memory on the host using a real32 fortran pointer of size array_length
!!  * allocates a device pointer using a type(c_ptr) of the same size
!!  * performs "nreps" device-to-host memory copies and measures wall time
!!  * calculates per memcpy wall time and bandwidth in units of gb/s
!!
  implicit none
  integer, intent(in) :: array_length
  integer, intent(in) :: nreps 
  real(real64), intent(out) :: walltime
  real(real64), intent(out) :: bandwidth_gb_s
  ! Local
  real(real32), pointer :: host_data(:)
  type(c_ptr) :: dev_data
  integer :: i
  integer(c_size_t) :: array_size
  real(real64) :: t1
  real(real64) :: t2

  array_size = array_length*real32
  call hipcheck(hipHostMalloc(host_data,array_length,hipHostMallocDefault))

  call hipcheck(hipmalloc(dev_data,array_size))

  call cpu_time(t1)
  do i = 1, nreps
  
    call hipcheck(hipmemcpy(c_loc(host_data), &
                            dev_data, &
                            array_size, &
                            hipMemcpyDeviceToHost))
  enddo
  call cpu_time(t2)

  walltime = (t2-t1)/real(nreps,real64)
  bandwidth_gb_s = real(array_size,real64)/walltime/1.0E9

  call hipcheck(hipfree(dev_data))
  call hipcheck(hipfreehost(host_data))

end subroutine float32_pinned_memcpy_d2h

subroutine float64_pageable_memcpy_h2d( array_length, nreps, walltime, bandwidth_gb_s )
!! This subroutine does the following actions
!!  * allocates pageable memory on the host using a real64 fortran pointer of size array_length
!!  * allocates a device pointer using a type(c_ptr) of the same size
!!  * performs "nreps" host-to-device memory copies and measures wall time
!!  * calculates per memcpy wall time and bandwidth in units of gb/s
!!
  implicit none
  integer, intent(in) :: array_length
  integer, intent(in) :: nreps 
  real(real64), intent(out) :: walltime
  real(real64), intent(out) :: bandwidth_gb_s
  ! Local
  real(real64), pointer :: host_data(:)
  type(c_ptr) :: dev_data
  integer :: i
  real(real64) :: t1
  real(real64) :: t2

  allocate(host_data(1:array_length))

  call hipcheck(hipmalloc(dev_data,sizeof(host_data)))

  call cpu_time(t1)
  do i = 1, nreps
  
    call hipcheck(hipmemcpy(dev_data, &
                            c_loc(host_data), &
                            sizeof(host_data), &
                            hipMemcpyHostToDevice))
  enddo
  call cpu_time(t2)

  walltime = (t2-t1)/real(nreps,real64)
  bandwidth_gb_s = real(sizeof(host_data),real64)/walltime/1.0E9

  call hipcheck(hipfree(dev_data))
  deallocate(host_data)

end subroutine float64_pageable_memcpy_h2d

subroutine float64_pageable_memcpy_d2h( array_length, nreps, walltime, bandwidth_gb_s )
!! This subroutine does the following actions
!!  * allocates pageable memory on the host using a real64 fortran pointer of size array_length
!!  * allocates a device pointer using a type(c_ptr) of the same size
!!  * performs "nreps" device-to-host memory copies and measures wall time
!!  * calculates per memcpy wall time and bandwidth in units of gb/s
!!
  implicit none
  integer, intent(in) :: array_length
  integer, intent(in) :: nreps 
  real(real64), intent(out) :: walltime
  real(real64), intent(out) :: bandwidth_gb_s
  ! Local
  real(real64), pointer :: host_data(:)
  type(c_ptr) :: dev_data
  integer :: i
  real(real64) :: t1
  real(real64) :: t2

  allocate(host_data(1:array_length))

  call hipcheck(hipmalloc(dev_data,sizeof(host_data)))

  call cpu_time(t1)
  do i = 1, nreps
  
    call hipcheck(hipmemcpy(c_loc(host_data), &
                            dev_data, &
                            sizeof(host_data), &
                            hipMemcpyDeviceToHost))
  enddo
  call cpu_time(t2)

  walltime = (t2-t1)/real(nreps,real64)
  bandwidth_gb_s = real(sizeof(host_data),real64)/walltime/1.0E9

  call hipcheck(hipfree(dev_data))
  deallocate(host_data)

end subroutine float64_pageable_memcpy_d2h

subroutine float64_pinned_memcpy_h2d( array_length, nreps, walltime, bandwidth_gb_s )
!! This subroutine does the following actions
!!  * allocates pinned memory on the host using a real64 fortran pointer of size array_length
!!  * allocates a device pointer using a type(c_ptr) of the same size
!!  * performs "nreps" host-to-device memory copies and measures wall time
!!  * calculates per memcpy wall time and bandwidth in units of gb/s
!!
  implicit none
  integer, intent(in) :: array_length
  integer, intent(in) :: nreps 
  real(real64), intent(out) :: walltime
  real(real64), intent(out) :: bandwidth_gb_s
  ! Local
  real(real64), pointer :: host_data(:)
  type(c_ptr) :: dev_data
  integer :: i
  integer(c_size_t) :: array_size
  real(real64) :: t1
  real(real64) :: t2

  array_size = array_length*real64

  call hipcheck(hipHostMalloc(host_data,array_length,hipHostMallocDefault))

  call hipcheck(hipmalloc(dev_data,array_size))

  call cpu_time(t1)
  do i = 1, nreps
  
    call hipcheck(hipmemcpy(dev_data, &
                            c_loc(host_data), &
                            array_size, &
                            hipMemcpyHostToDevice))
  enddo
  call cpu_time(t2)

  walltime = (t2-t1)/real(nreps,real64)
  bandwidth_gb_s = real(array_size,real64)/walltime/1.0E9

  call hipcheck(hipfree(dev_data))
  call hipcheck(hipHostFree(host_data))

end subroutine float64_pinned_memcpy_h2d

subroutine float64_pinned_memcpy_d2h( array_length, nreps, walltime, bandwidth_gb_s )
!! This subroutine does the following actions
!!  * allocates pinned memory on the host using a real64 fortran pointer of size array_length
!!  * allocates a device pointer using a type(c_ptr) of the same size
!!  * performs "nreps" device-to-host memory copies and measures wall time
!!  * calculates per memcpy wall time and bandwidth in units of gb/s
!!
  implicit none
  integer, intent(in) :: array_length
  integer, intent(in) :: nreps 
  real(real64), intent(out) :: walltime
  real(real64), intent(out) :: bandwidth_gb_s
  ! Local
  real(real64), pointer :: host_data(:)
  type(c_ptr) :: dev_data
  integer :: i
  integer(c_size_t) :: array_size
  real(real64) :: t1
  real(real64) :: t2

  array_size = array_length*real64
  call hipcheck(hipHostMalloc(host_data,array_length,hipHostMallocDefault))

  call hipcheck(hipmalloc(dev_data,array_size))

  call cpu_time(t1)
  do i = 1, nreps
  
    call hipcheck(hipmemcpy(c_loc(host_data), &
                            dev_data, &
                            array_size, &
                            hipMemcpyDeviceToHost))
  enddo
  call cpu_time(t2)

  walltime = (t2-t1)/real(nreps,real64)
  bandwidth_gb_s = real(array_size,real64)/walltime/1.0E9

  call hipcheck(hipfree(dev_data))
  call hipcheck(hipHostFree(host_data))

end subroutine float64_pinned_memcpy_d2h

end module memcpy_bench_tools

program memcpy_bench

use iso_fortran_env
use memcpy_bench_tools
use hipfort_auxiliary
use hipfort_types
use hipfort_check

implicit none

  ! ============================================================================================ !
  ! Parameters
  integer, parameter :: float32_array_low = 50          ! Minimum array size (float32) ~> 200 B
  integer, parameter :: float32_array_high = 500000   ! Maximum array size (float32) ~> 2 MB
  integer, parameter :: float64_array_low = 25          ! Minimum array size (float64) ~> 200 B
  integer, parameter :: float64_array_high = 250000   ! Maximum array size (float64) ~> 2 MB
  integer, parameter :: n_experiments = 50              ! Number of points between low and high
                                                         ! to measure
  integer, parameter :: n_repetitions = 100              ! Number of times to perform memcpy                                                         
  integer, parameter :: deviceId = 0                     ! device ID
  ! ============================================================================================ !
  integer :: i
  integer :: array_length
  integer :: stat
  real(real64) :: walltime
  real(real64) :: bandwidth
  character(24) :: array_length_char
  character(24) :: size_bytes_char
  character(24) :: wall_time_char
  character(24) :: bandwidth_char
  character(24) :: hostname
  character(6) :: gpuarch
  type(hipDeviceProp_t) :: prop

  call hipcheck(hipGetDeviceProperties_(prop,deviceId))

! Temporary fix for multi-platform
#ifdef __HIP_PLATFORM_AMD__
  do i = 1, 6
    gpuarch(i:i) = prop % gcnArchName(i)
  enddo
#else
  gpuarch='NVIDIA'
#endif

  stat = hostnm(hostname)
  
  print*, '"Data Type","Array Length","Host Memory Type",'//&
          '"Size (Bytes)","Direction","GPU Model",'//&
          '"Hostname","Wall Time (s)","Bandwidth (GB/s)"'

  do i = 0, n_experiments

    array_length = float32_array_low + &
            ceiling( real((float32_array_high - float32_array_low))/&
                     real(n_experiments) )*i

    ! float32 pageable h2d
    call float32_pageable_memcpy_h2d( array_length, &
                                      n_repetitions, &
                                      walltime, &
                                      bandwidth ) 

    print*, 'real32,'//&
            trim(adjustl(to_char(array_length)))//','//&
            'pageable,'//&
            trim(adjustl(to_char(array_length*4)))//','//&
            'h2d,'//&
            trim(adjustl(gpuarch))//','//&
            trim(adjustl(hostname))//','//&
            trim(adjustl(to_char(walltime)))//','//&
            trim(adjustl(to_char(bandwidth)))

    ! float32 pageable d2h
    call float32_pageable_memcpy_d2h( array_length, &
                                      n_repetitions, &
                                      walltime, &
                                      bandwidth ) 

    print*, 'real32,'//&
            trim(adjustl(to_char(array_length)))//','//&
            'pageable,'//&
            trim(adjustl(to_char(array_length*4)))//','//&
            'd2h,'//&
            trim(adjustl(gpuarch))//','//&
            trim(adjustl(hostname))//','//&
            trim(adjustl(to_char(walltime)))//','//&
            trim(adjustl(to_char(bandwidth)))
    

    ! float32 pinned h2d
    call float32_pinned_memcpy_h2d( array_length, &
                                      n_repetitions, &
                                      walltime, &
                                      bandwidth ) 

    print*, 'real32,'//&
            trim(adjustl(to_char(array_length)))//','//&
            'pinned,'//&
            trim(adjustl(to_char(array_length*4)))//','//&
            'h2d,'//&
            trim(adjustl(gpuarch))//','//&
            trim(adjustl(hostname))//','//&
            trim(adjustl(to_char(walltime)))//','//&
            trim(adjustl(to_char(bandwidth)))

    ! float32 pinned d2h
    call float32_pinned_memcpy_d2h( array_length, &
                                      n_repetitions, &
                                      walltime, &
                                      bandwidth ) 

    print*, 'real32,'//&
            trim(adjustl(to_char(array_length)))//','//&
            'pinned,'//&
            trim(adjustl(to_char(array_length*4)))//','//&
            'd2h,'//&
            trim(adjustl(gpuarch))//','//&
            trim(adjustl(hostname))//','//&
            trim(adjustl(to_char(walltime)))//','//&
            trim(adjustl(to_char(bandwidth)))


    ! ///////////////////////////////////////////////////////// !
    ! Float 64 
    ! ///////////////////////////////////////////////////////// !

    array_length = float64_array_low + &
            ceiling( real((float64_array_high - float64_array_low))/&
                     real(n_experiments) )*i

    ! float64 pageable h2d
    call float64_pageable_memcpy_h2d( array_length, &
                                      n_repetitions, &
                                      walltime, &
                                      bandwidth ) 

    print*, 'real64,'//&
            trim(adjustl(to_char(array_length)))//','//&
            'pageable,'//&
            trim(adjustl(to_char(array_length*8)))//','//&
            'h2d,'//&
            trim(adjustl(gpuarch))//','//&
            trim(adjustl(hostname))//','//&
            trim(adjustl(to_char(walltime)))//','//&
            trim(adjustl(to_char(bandwidth)))

    ! float64 pageable d2h
    call float64_pageable_memcpy_d2h( array_length, &
                                      n_repetitions, &
                                      walltime, &
                                      bandwidth ) 

    print*, 'real64,'//&
            trim(adjustl(to_char(array_length)))//','//&
            'pageable,'//&
            trim(adjustl(to_char(array_length*8)))//','//&
            'd2h,'//&
            trim(adjustl(gpuarch))//','//&
            trim(adjustl(hostname))//','//&
            trim(adjustl(to_char(walltime)))//','//&
            trim(adjustl(to_char(bandwidth)))
    

    ! float64 pinned h2d
    call float64_pinned_memcpy_h2d( array_length, &
                                      n_repetitions, &
                                      walltime, &
                                      bandwidth ) 

    print*, 'real64,'//&
            trim(adjustl(to_char(array_length)))//','//&
            'pinned,'//&
            trim(adjustl(to_char(array_length*8)))//','//&
            'h2d,'//&
            trim(adjustl(gpuarch))//','//&
            trim(adjustl(hostname))//','//&
            trim(adjustl(to_char(walltime)))//','//&
            trim(adjustl(to_char(bandwidth)))

    ! float64 pinned d2h
    call float64_pinned_memcpy_d2h( array_length, &
                                      n_repetitions, &
                                      walltime, &
                                      bandwidth ) 

    print*, 'real64,'//&
            trim(adjustl(to_char(array_length)))//','//&
            'pinned,'//&
            trim(adjustl(to_char(array_length*8)))//','//&
            'd2h,'//&
            trim(adjustl(gpuarch))//','//&
            trim(adjustl(hostname))//','//&
            trim(adjustl(to_char(walltime)))//','//&
            trim(adjustl(to_char(bandwidth)))


  enddo

end program memcpy_bench
