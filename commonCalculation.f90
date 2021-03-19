module commonCalcuation
    implicit none 

    contains
    subroutine gradient_evaluation (h, location_x, g)
        real(8), intent(in)                 :: h, location_x
      !  real(8), intent(in), dimension(:)   :: x, fx 
        real(8), intent(out)                :: g 
        g = ((location_x + h)**2.0 - (location_x - h)**2.0) / (2.0*h)
    end subroutine gradient_evaluation

end module commonCalcuation 