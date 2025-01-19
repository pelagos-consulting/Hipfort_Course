
program chessboard
    !! Program to fill a chessboard
    !! using a HIP kernel on the compute device 
    !! 
    !! Written by Dr. Toby Potter and Dr. Joseph Schoonover

    ! Add this to use the standard Fortran environment module
    use iso_fortran_env

    ! C interopability 
    use iso_c_binding

    ! Use the kinds module to make available the float_type kind
    use kinds

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
            use kinds
            ! Fortran passes arguments by reference as the default
            ! Arguments must have the "value" option present to pass by value
            ! Otherwise launch_kernel will receive pointers of type void**
            ! instead of void*
            ! The memory allocation for the chessboard
            type(c_ptr), intent(in), value :: B
            ! Floating point values for light and dark cells
            real(float_type), intent(in), value :: light, dark
            ! Size of the problem
            integer(c_int), intent(in), value :: M, N
        end subroutine
        
    end interface

    ! Number of elements in the tensors
    integer, parameter :: M=8, N=8

    ! Matrix indices
    integer :: i, j

    ! Declare the chessboard on the host as a static array
    real :: B_h(M,N)

    ! Define what light and dark means
    real(float_type) :: light = 0.0
    real(float_type) :: dark = 1.0

    ! Fortran pointer to the chessboard on the device
    real(kind=float_type), dimension(:,:), pointer :: B_d

    !!!! Step 1: Find and set the device. 
    !! Use device 0 by default

    !! Uncomment for the shortcut solution to Step 1.
    !include 'step1_init.h'   

    !!!! Step 2: Allocate memory for pointer B_d

    !! Uncomment for the shortcut solution to Step 2.
    !include 'step2_malloc.h'

    !!!! Step 3: Call the C function that launches the kernel
    
    !! Uncomment for the shortcut solution to Step 3.
    !include 'step3_launch.h'

    !!!! Step 4: Copy from the device back to the host
    
    !! Uncomment for the shortcut solution to Step 4.
    !include 'step4_copy.h'

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

    !!!! Step 5: Free allocation B_d on the device

    !! Uncomment for the shortcut solution to Step 5.
    !include 'step5_free.h'

    !!!! Step 6: Reset the GPU to make sure all resources are released

    !! Uncomment for the shortcut solution to Step 6.
    !include 'step6_reset.h'
    
end program chessboard

