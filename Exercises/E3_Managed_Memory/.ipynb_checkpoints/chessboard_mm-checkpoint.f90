
program chessboard
    !! Program to fill a chessboard
    !! using a HIP kernel on the compute device 
    !! 
    !! Written by Dr. Toby Potter and Dr. Joseph Schoonover

    ! Add this to use the standard Fortran environment module
    use iso_fortran_env

    ! C interopability 
    use iso_c_binding

    ! HIP modules
    use hipfort
    use hipfort_check

    ! device handling 
    use hip_utils, only : init_device, reset_device

    ! Add this to make sure that all variables must be declared
    ! and the compiler performs no type inferencing based on the 
    ! on the first letter of variable names
    implicit none

    ! Interface to launch_kernel_hip function
    ! in the file kernel_code.cpp
    interface
        ! A C function with void return type
        ! is regarded as a subroutine in Fortran 
        subroutine launch_kernel_hip(B, light, dark, M, N) bind(C)
            use iso_c_binding
            ! Fortran passes arguments by reference as the default
            ! Arguments must have the "value" option present to pass by value
            ! Otherwise launch_kernel will receive pointers of type void**
            ! instead of void*
            ! The memory allocation for the chessboard
            type(c_ptr), intent(in), value :: B
            ! Floating point values for light and dark cells
            real(c_float), intent(in), value :: light, dark
            ! Size of the problem
            integer(c_int), intent(in), value :: M, N
        end subroutine
        
    end interface

    ! Number of elements in the tensors
    integer, parameter :: M=8, N=8

    ! Matrix indices
    integer :: i, j

    ! Declare the chessboard on the host as a static array
    real(c_float), target :: B_h(M,N)

    ! Define what light and dark means
    real(c_float) :: light = 0.0
    real(c_float) :: dark = 1.0

    ! Fortran pointer to the chessboard on the device
    real(c_float), dimension(:,:), pointer :: B_d
    
    !! Step 1: Find and set the device. 
    !! Use device 0 by default
    call init_device(0)   

    ! Step 2: Allocate memory for pointer B_d
    call hipcheck(hipmalloc(B_d, M, N))

    ! Step 3: Call the C function that launches the kernel
    call launch_kernel_hip( &
        c_loc(B_d(1,1)), &
        light, &
        dark, &
        int(M, c_int), &
        int(N, c_int) &
    )

    ! Step 4: Copy from the device back to the host
    call hipcheck(hipmemcpy(B_h, B_d, size(B_h), hipmemcpydevicetohost))

    ! Check the answer by printing it
    do i=1,N
        do j=1,M
            ! Print values in the chessboard
            write(*, '(F3.1XX) ', advance="no") B_h(i,j)
        end do
        ! Print a new line
        print *, ""
    end do

    ! Release resources

    ! Step 5: Free allocation B_d on the device
    call hipcheck(hipfree(B_d))

    ! Step 6: Reset the GPU to make sure all resources are released
    call reset_device
    
end program chessboard

