module math_utils

    ! Import this make available the "float_type" kind 
    use kinds

    implicit none

    contains

        function check_tensor_addition_2D(A, B, C, eps_mult) result(success)
            !! Function to check the outcome of tensor addition
            !! only check the host arrays

            real(float_type), dimension(:,:), intent(in), pointer :: A, B, C
        
            real, intent(in) :: eps_mult
                !! Epsilon multiplier, how many floating point spacings
                !! can the computed answer be from our benchmark answer

            ! Shape as determined from the pointer
            integer :: M, N

            ! Scratch variables
            real(float_type) :: scratch, upper, lower

            ! Loop indices and error code
            integer  :: i, j

            ! Set the outcome as positive until proven otherwise
            logical :: success
            success = .true.

            ! Trust the shape for now
            M=size(C, 1)
            N=size(C, 2)

            ! Loop over all indices and check tensor addition
            do j=1, N
                do i=1, M
                    scratch = A(i,j) + B(i,j)
                    upper = scratch + eps_mult*abs(spacing(scratch))
                    lower = scratch - eps_mult*abs(spacing(scratch))
                    if (.not. ( (lower<=C(i,j) .and. (C(i,j)<=upper) ) )) then
                        write(*,*) "Error, tensor addition did not work at index = (", i, ", ", &
                            j, "), value was: ", C(i,j), ", but should be:", scratch
                        success = .false.
                        return
                    end if
                end do
            end do

            ! We got to here because we didn't return on failure
            write(*,*) 'Tensor addition passed validation.'
        
        end function check_tensor_addition_2D

end module math_utils