
program chessboard
    !! Program to fill a chessboard with values and print the result
    !! Written by Dr. Toby Potter and Dr. Joseph Schoonover

    ! Add this to use the standard Fortran environment module
    use iso_fortran_env
    use iso_c_binding

    ! Add this to make sure that all variables must be declared
    ! and the compiler performs no type inferencing based on the 
    ! on the first letter of variable names
    implicit none

    ! Number of elements in the tensors
    integer, parameter :: M=8, N=8

    ! Declare the chessboard
    real :: A(M,N)

    ! Define what light and dark means
    real :: light = 0.0
    real :: dark = 1.0

    ! Array indices
    integer :: i, j, k=0

    ! Fill the chessboard
    do j=1,M
        do i=1,N
            ! Use modulo arithmetic to fill the chessboard
            A(i, j) = mod(k+1, 2)*light + mod(k, 2)*dark
            k=k+1
        end do
        ! Cycle k at the end of each column
        k=k+1
    end do

    ! Print out the array
    do i=1,N
        do j=1,M
            ! Print values in the chessboard
            write(*, '(F3.1XX) ', advance="no") A(i,j)
        end do
        ! Print a new line
        print *, ""
    end do
    
end program chessboard

