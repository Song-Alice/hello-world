module commonCalcuation 
    use input 
    implicit none 

    contains

    subroutine gradient_evaluation (location_y1, location_y2, h, g)
        real(8), intent(in)                                :: h
        real(8), intent(in)                                :: location_y1, location_y2
        real(8), intent(out)                               :: g 
        g = (location_y2 - location_y1) / (2.0*h)
    end subroutine gradient_evaluation

    subroutine HessianMatrix_evaluation (index, x, h, n, HessianMatrix, gradient)
        type(multiDimensional_index), intent(in)           :: index 
        type(vector_x), intent(in)                         :: x
        real(8), intent(in)                                :: h
        integer, intent(in)                                :: n  
        real(8),intent(inout), dimension(:,:), allocatable   :: gradient
        real(8),intent(inout), dimension(:,:), allocatable   :: HessianMatrix
        type(vector_x)                                     :: location_x
        real(8), dimension(:), allocatable                 :: y 
        real(8)                                            :: g1, g2, g3, gx1, gx2
        integer                                            :: l, i 
        real(8)                                            :: y_value
        l = 2*n + 1
        allocate (y(l))
        location_x%x1 = x%x1 - 2.0*h
        location_x%x2 = x%x2
        do i = 1, l
            call Multidimensional_quadratic_function (index, location_x, y_value)
            y(i) = y_value
            location_x%x1 = location_x%x1 + h 
            location_x%x2 = location_x%x2
        enddo
        call gradient_evaluation (y(3), y(1), h, g1)
        call gradient_evaluation (y(5), y(3), h, g3)
        call gradient_evaluation (y(4), y(2), h, g2)
        gradient(1,1) = g2
        ! Second derivative function.
        call gradient_evaluation (g3, g1, h, gx1)
        HessianMatrix(1,1) = gx1
        
        location_x%x1 = x%x1
        location_x%x2 = x%x2 - 2.0*h 
        do i = 1, l
            call Multidimensional_quadratic_function (index, location_x, y_value)
            y(i) = y_value
            location_x%x2 = location_x%x2 + h 
            location_x%x1 = location_x%x1
        enddo
        call gradient_evaluation (y(3), y(1), h, g1)
        call gradient_evaluation (y(5), y(3), h, g3)
        call gradient_evaluation (y(4), y(2), h, g2)
        gradient(1,2) = g2
        ! Second derivative function.
        call gradient_evaluation (g3, g1, h, gx2)
        HessianMatrix(2,2) = gx2
        HessianMatrix(1,2) = index%c
        HessianMatrix(2,1) = index%c 
        print *, 'h', HessianMatrix
        
    end subroutine HessianMatrix_evaluation

end module commonCalcuation 