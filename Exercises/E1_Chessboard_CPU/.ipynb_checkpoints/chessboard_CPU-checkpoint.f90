
program chessboard
    !! Program to fill a chessboard with values and print the result

    ! Add this to use the standard Fortran environment module
    use iso_fortran_env
    use iso_c_binding

    ! Add this to make sure that all variables must be declared
    ! and the compiler performs no type inferencing based on the 
    ! on the first letter of variable names
    implicit none

    ! Number of elements in the tensors
    integer, parameter :: M=8, N=8

    ! Define the values for light and dark squares
    real :: light = 0.0
    real :: dark = 1.0

    ! Array indices
    integer :: i0, i1

    !! Step 0: Declare the chessboard as a 8x8 array
    !! and allocate memory for it

    !! Step 1: Use nested loops to fill the chessboard

    ! Print out the array
    do i1=1,N
        do i0=1,M
            ! Print values in the chessboard
            !write(*, '(F3.1XX) ', advance="no") B(i0,i1)
        end do
        ! Print a new line
        print *, ""
    end do

    !! Step 3: Deallocate any allocated memory here
    
end program chessboard

