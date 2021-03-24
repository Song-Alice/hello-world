module commonCalcuation 
    use input 
    implicit none 

    contains
    subroutine gradient_evaluation (index, location, h, location_x, g)
        type(oneDimensional_index), intent(in)           :: index
        real(8), intent(in)                              :: h, location_x
        real(8)                                          :: location_y1, location_y2
        real(8), intent(out)                             :: g 
        interface
            subroutine location (index, value_x, value_y)
              type(oneDimensional_index), intent(in)     :: index
              real(8), intent(in)                        :: value_x
              real(8), intent(out)                       :: value_y
            end subroutine location 
        end interface
        call location (index, location_x + h, location_y1)
        call location (index, location_x - h, location_y2)
        g = (location_y1 - location_y2) / (2.0*h)
    end subroutine gradient_evaluation

    subroutine HessianMatrix_evaluation (index, x, h, HessianMatrix)
        type(vector_x), intent(in)                         :: x
        type(vector_x)                                     :: a, b 
        real(8),intent(inout), dimension(:,:), allocatable :: HessianMatrix
        real(8)                                            :: g1
        integer                                            :: n 
        n = 4 
        allocate (HessianMatrix(n,n))
        a%x1 = h 
        a%x2 = 0.0 
        call multi_gradient_evaluation (index, x+a, x-a, g1)
        HessianMatrix(1,1) = g1 **2.0 
        b%x1 = 0.0
        b%x2 = h 
        call multi_gradient_evaluation (index, x+b, x-b, g2)
        HessianMatrix(2,2) = g2 **2.0
        

        
    end subroutine HessianMatrix_evaluation

    subroutine multi_gradient_evaluation (index, a, b, g)
        type(multiDimensional_index), intent(in)           :: index 
        real(8), intent(in)                                :: h
        type(vector_x), intent(in)                         :: a , b 
        real(8)                                            :: g1, g2
        real(8)                                            :: location_y1, location_y2
        interface
            subroutine location (index, value_x, value_y)
              type(multiDimensional_index), intent(in)     :: index
              real(8), intent(in)                          :: value_x
              real(8), intent(out)                         :: value_y
            end subroutine location 
        end interface
        call location (index, a, location_y1)
        call location (index, b, location_y2)
        g = (location_y1 - location_y2) / (2.0*h)
    end subroutine multi_gradient_evaluation



end module commonCalcuation 