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
        real(8), dimension(:,:), allocatable                  :: r, r_next, r_previous
        real(8), dimension(:,:), allocatable                  :: x_location, x_next, x_previous
        real(8), dimension(:,:), allocatable                  :: p, p_previous
        real(8), dimension(:,:), allocatable                  :: step_length 
        integer                                               :: n = 2 
        real(8)                                               :: h = 0.01
        real(8)                                               :: location_y
        real(8)                                               :: minimum_difference = 0.001
        integer                                               :: iteration = 0

        interface
           subroutine location (indexinput, value_x, value_y)
              import                                          :: multiDimensional_index
              import                                          :: vector_x
              type(multiDimensional_index), intent(in)        :: indexinput
              type(vector_x), intent(in)                      :: value_x
              real(8), intent(out)                            :: value_y
           end subroutine location 
        end interface

        allocate (HessianMatrix(n,n))
        allocate (x_location(1,n), x_next(1,n), x_previous(1,n))
        allocate (gradient(1,n))
        allocate (r(1,n), r_next(1,n), r_previous(1,n))
        allocate (p(1,n), p_previous(1,n))
        allocate (step_length(1,n))

        x%x1 = -5.0
        x%x2 = -5.0
        x_location(1,1) = x%x1
        x_location(1,2) = x%x2
        call HessianMatrix_evaluation (index, x, h, n, HessianMatrix, gradient)
        call Initialization (x_location, gradient, HessianMatrix, r, p)
        ! Initialize the value of x0, r0, and p0.
        
        do while (iteration <= 100)
            call stepMultiDimensional (r, HessianMatrix, p, step_length)
            print *, 'step length', step_length
            call newLocationMulti (x_location, step_length, p, x_next)
            x_previous = x_location
            x_location = x_next
            call location (index, x, location_y)
            iteration = iteration + 1 
            call residual (HessianMatrix, step_length, r, p, r_next)
            if ((abs(r_next(1,1)) <= minimum_difference) .and. (abs(r_next(1,2)) <= minimum_difference)) exit 
            r_previous = r
            r = r_next
            p_previous = p 
            call directionMultiDimensional (r, r_previous, p_previous, p)
        enddo
        print *, 'iteration', iteration
        print *, 'location', 'x=', x_location, 'f(x)=', location_y
        print *, 'residual', r_next
    end subroutine ConjugateGradient

    subroutine Initialization (x_location, gradient, HessianMatrix, r, p)
        real(8), intent(in), dimension(:,:), allocatable              :: x_location
        real(8), intent(in), dimension(:,:), allocatable              :: gradient
        real(8), intent(in), dimension(:,:), allocatable              :: HessianMatrix
        real(8), intent(out), dimension(:,:), allocatable             :: r, p 
        r = gradient - HessianMatrix * x_location
        p = r
    end subroutine Initialization

    subroutine residual (HessianMatrix, step_length, r, p, r_next)
        real(8), intent(in), dimension(:,:), allocatable              :: HessianMatrix  
        real(8), intent(in), dimension(:,:), allocatable              :: r, p 
        real(8), intent(in), dimension(:,:), allocatable              :: step_length
        real(8), intent(out),dimension(:,:), allocatable              :: r_next
        r_next = r - step_length * HessianMatrix * p 
        print *, 'residual', r_next  
    end subroutine residual      

    subroutine directionMultiDimensional (r, r_previous, p_previous, p)
        real(8), intent(in), dimension(:,:), allocatable              :: r, r_previous, p_previous  
        real(8), intent(out), dimension(:,:), allocatable             :: p
        p = r +( (transpose(r) * r) / transpose(r_previous) * r_previous) * p_previous
    end subroutine directionMultiDimensional

    subroutine stepMultiDimensional (r, HessianMatrix, p, step_length)
        real(8), intent(in), dimension(:,:), allocatable      :: HessianMatrix
        real(8), intent(in), dimension(:,:), allocatable      :: r, p 
        real(8), intent(out), dimension(:,:), allocatable     :: step_length
        step_length = transpose(r) * r / (transpose(p) * HessianMatrix * p)
    end subroutine stepMultiDimensional

    subroutine newLocationMulti (x_location, step_length, p, x_next) 
        real(8), intent(in),dimension(:,:), allocatable       :: step_length
        real(8), intent(in), dimension(:,:), allocatable      :: p
        real(8), intent(in), dimension(:,:), allocatable      :: x_location
        real(8), intent(out), dimension(:,:), allocatable     :: x_next        
        x_next = x_location + step_length * p
    end subroutine newLocationMulti

end module conjugate_gradient_method