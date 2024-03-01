
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

    ! Matrix indices
    integer :: i, j

    ! Declare a chessboard here

    ! Fill the chessboard

    ! Print out the array
    do i=1,N
        do j=1,M
            ! Print values in the chessboard
            !write(*, '(F3.1XX) ', advance="no") B(i,j)
        end do
        ! Print a new line
        print *, ""
    end do

    ! Deallocate any allocated memory
    
end program chessboard

