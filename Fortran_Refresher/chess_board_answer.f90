
program chess_board
    !! Program to compute a 1D tensor addition
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

    ! Define the grid
    real :: A(M,N)

    ! Define what light and dark means
    real :: light = 0.0
    real :: dark = 1.0

    ! Matrix indices
    integer :: i, j, k

    ! Make the chess board
    do j=1,M
        do i=1,N
            ! Kernel math
            A(i, j) = mod(j,2)*light + mod(i,2)*dark
        end do
    end do

    ! Print out the array
    do i=1,N
        do j=1,M
            ! Kernel math
            print *, A(i,j)
        end do
        ! Print a new line
        print *, "\n"
    end do
    
end program chess_board

