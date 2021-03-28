module conjugate_gradient_method
    use input
    use commonCalcuation
    implicit none
    contains 

    subroutine ConjugateGradient (index, location)
        type(vector_x)                                        :: x 
        type(multiDimensional_index), intent(in)              :: index
        real(8), dimension(:,:), allocatable                  :: HessianMatrix
        real(8), dimension(:,:), allocatable                  :: gradient
        real(8), dimension(:,:), allocatable                  :: r, r_next, r_previous            ! Residual
        real(8), dimension(:,:), allocatable                  :: x_location, x_next, x_previous
        real(8), dimension(:,:), allocatable                  :: p, p_previous                    ! Orthogonal direction
        real(8), dimension(:,:), allocatable                  :: direction_i, direction_sum, direction_p
        real(8), dimension(:,:), allocatable                  :: step_length 
        integer                                               :: n = 2 
        real(8)                                               :: h = 0.01
        real(8)                                               :: location_y
        real(8)                                               :: minimum_difference = 0.001
        integer                                               :: i, iteration = 1

        interface
           subroutine location (indexinput, value_x, value_y)
              import                                          :: multiDimensional_index
              import                                          :: vector_x
              type(multiDimensional_index), intent(in)        :: indexinput
              type(vector_x), intent(in)                      :: value_x
              real(8), intent(out)                            :: value_y
           end subroutine location 
        end interface

        allocate (HessianMatrix (n,n))
        allocate (x_location (1,n), x_next (1,n), x_previous (1,n))
        allocate (gradient (1,n))
        allocate (r (1,n), r_next (1,n), r_previous (1,n))
        allocate (p (1,n), p_previous (1,n))
        allocate (direction_p(iteration,n),direction_i (1,n), direction_sum (1,n))
        allocate (step_length (1,n))

        !################################################################################
        ! Initialize the value of x0, r0, and p0.
        x%x1 = -2.5
        x%x2 = 3.0
        x_location(1,1) = x%x1
        x_location(1,2) = x%x2
        call location (index, x, location_y)
        print *, 'y0', location_y
        call HessianMatrix_evaluation (index, x, h, n, HessianMatrix, gradient)
        print *, 'Hessian Matrix', HessianMatrix
        print *, 'gradient', gradient 
        call Residual (x_location, gradient, HessianMatrix, r)
        direction_p = r 
        
        direction_sum = 0.0
        
        print *, 'residual', r 
        
        !##############################################################################
! 'Location' to 'Hessian Matrix and gradient' to 'residual' to 'orthogonal direction' to 'steplength' to 'new location'. Do the loop.
        do while (iteration <= 100)
            p(1,:) = direction_p(iteration,:)
            print *,'p', p
            call stepMultiDimensional (r, HessianMatrix, p, step_length)
            !###############################################################
            ! Calculate the values of x_k+1.
            call newLocationMulti (x_location, step_length, p, x_next)   
            x_previous = x_location
            x_location = x_next
             
            x%x1 = x_location(1,1)
            x%x2 = x_location(1,2)
            call location (index, x, location_y)
            print *, 'y_k+1', location_y
            call HessianMatrix_evaluation (index, x, h, n, HessianMatrix, gradient)
            print *, 'HessianMatrix', HessianMatrix
            print *, 'gradient', gradient
            call Residual (x_location, gradient, HessianMatrix, r_next)
            if ((abs(r_next(1,1)) <= minimum_difference) .and. (abs(r_next(1,2)) <= minimum_difference)) exit 
            r_previous = r
            r = r_next
            iteration = iteration + 1
            print *, 'iteration', iteration 
            !##################################################################
            ! Calucation the orthogonal direction p.
            do i = 1, iteration
                p(1,:) = direction_p (i,:)
                direction_i = ((transpose(p)*HessianMatrix*r) / (transpose(p)*HessianMatrix*p)) * p
                direction_sum = direction_sum + direction_i
            enddo
            direction_p (iteration,:) = r(1,:) - direction_sum(1,:)

            !call directionMultiDimensional (r, r_previous, p_previous, p)
        enddo
        print *, 'iteration', iteration
        print *, 'location', 'x=', x_location, 'f(x)=', location_y
        print *, 'residual', r_next
    end subroutine ConjugateGradient

    subroutine Residual (x_location, gradient, HessianMatrix, r)
        real(8), intent(in), dimension(:,:), allocatable              :: x_location
        real(8), intent(in), dimension(:,:), allocatable              :: gradient
        real(8), intent(in), dimension(:,:), allocatable              :: HessianMatrix
        real(8), intent(inout), dimension(:,:), allocatable           :: r
        r = gradient - HessianMatrix * x_location
        print *, ' residual', r 
    end subroutine Residual

    subroutine stepMultiDimensional (r, HessianMatrix, p, step_length)
        real(8), intent(in), dimension(:,:), allocatable      :: HessianMatrix
        real(8), intent(in), dimension(:,:), allocatable      :: r, p 
        real(8), intent(inout), dimension(:,:), allocatable   :: step_length
        step_length = transpose(r) * r / (transpose(p) * HessianMatrix * p)
        print *, 'step_length', step_length
    end subroutine stepMultiDimensional

    subroutine newLocationMulti (x_location, step_length, p, x_next) 
        real(8), intent(in),dimension(:,:), allocatable       :: step_length
        real(8), intent(in), dimension(:,:), allocatable      :: p
        real(8), intent(in), dimension(:,:), allocatable      :: x_location
        real(8), intent(inout), dimension(:,:), allocatable   :: x_next        
        x_next = x_location + transpose(step_length * p)
        print *, 'x_next', x_next
    end subroutine newLocationMulti

    subroutine residual_1 (HessianMatrix, step_length, r, p, r_next)
        real(8), intent(in), dimension(:,:), allocatable              :: HessianMatrix  
        real(8), intent(in), dimension(:,:), allocatable              :: r, p 
        real(8), intent(in), dimension(:,:), allocatable              :: step_length
        real(8), intent(inout),dimension(:,:), allocatable            :: r_next
        r_next = r - transpose(step_length * HessianMatrix * p) 
        print *, 'residual', r_next
        !print *, 'residual', r_next  
    end subroutine residual_1      

    !*********************************************************************************************
    ! Subroutine about orthogonal direction.
    subroutine directionMultiDimensional (r, r_previous, p_previous, p)
        real(8), intent(in), dimension(:,:), allocatable              :: r, r_previous, p_previous  
        real(8), intent(inout), dimension(:,:), allocatable           :: p
        p = r -( (transpose(r) * r) / (transpose(r_previous) * r_previous)) * p_previous
        print *, 'direction', p 
    end subroutine directionMultiDimensional

end module conjugate_gradient_method
