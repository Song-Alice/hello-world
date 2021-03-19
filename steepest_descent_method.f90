module steepest_descent_method
    implicit none
    contains
    subroutine step (g, previous_g, location_x, previous_location_x, step_length)
        real(8), intent(in)                :: g, previous_g, location_x, previous_location_x
        real(8), intent(out)               :: step_length
        step_length = (location_x - previous_location_x) * (g - previous_g) / (g - previous_g)**2.0
    end subroutine step

    subroutine direction (g, p)
        real(8), intent(in)                :: g
        real(8), intent(out)               :: p 
        p = g 
    end subroutine direction 

    subroutine newLocation (location_x, step_length, p, next_location_x)
        real(8), intent(in)                :: location_x, step_length, p 
        real(8), intent(out)               :: next_location_x
        next_location_x = location_x + step_length * p 
    end subroutine newLocation 

    subroutine y (previous_location_x, location_x, previous_location_y, location_y) 
        real(8), intent(in)                :: previous_location_x, location_x
        real(8), intent(out)               :: previous_location_y, location_y 
        previous_location_y = previous_location_x ** 2
        location_y = location_x ** 2
    end subroutine y 

end module steepest_descent_method