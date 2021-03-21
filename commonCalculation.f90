module commonCalcuation 
    implicit none 

    contains
    subroutine gradient_evaluation (index_H, index_b, index_c, location, h, location_x, g)
        real(8), intent(in)                 :: index_H, index_b, index_c
        real(8), intent(in)                 :: h, location_x
        real(8)                             :: location_y1, location_y2
        real(8), intent(out)                :: g 
        interface
            subroutine location (index_H, index_b, index_c, value_x, value_y)
              real(8), intent(in)           :: index_H, index_b, index_c
              real(8), intent(in)           :: value_x
              real(8), intent(out)          :: value_y
            end subroutine location 
        end interface
        call location (index_H, index_b, index_c, location_x + h, location_y1)
        call location (index_H, index_b, index_c, location_x - h, location_y2)
        g = (location_y1 - location_y2) / (2.0*h)
    end subroutine gradient_evaluation

end module commonCalcuation 