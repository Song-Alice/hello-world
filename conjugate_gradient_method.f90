module conjugate_gradient_method
    use input
    implicit none
    contains 

    subroutine Initialize (x, location_r, p)
        type(multiDimensional_index), intent(in)      :: x 


    subroutine residual (index_H, step_length, location_r, p, next_location_r)
        real(8), intent(in)                           :: index_H  
        real(8), intent(in)                           :: location_r, step_length, p
        real(8), intent(out)                          :: next_location_r
        next_location_r = location_r - step_length * index_H * p 
        print *, 'residual', location_r 
    end subroutine residual      

    subroutine directionMultiDimensional (location_r, previous_location_r, previous_p, p)
        real(8), intent(in)                :: location_r, previous_location_r, previous_p  
        real(8), intent(out)               :: p 
        p = location_r +( (location_r * location_r) / (previous_location_r * previous_location_r) ) * previous_p
    end subroutine directionMultiDimensional

    subroutine stepMultiDimensional (location_r, index_H, p, step_length)
        real(8), intent(in)                :: location_r, index_H, p 
        real(8), intent(out)               :: step_length
        step_length = location_r * location_r / (p * index_H * p)
    end subroutine stepMultiDimensional

    subroutine newLocationMulti (location_x, step_length, p, next_location_x)
        real(8), intent(in)                :: location_x, step_length
        real(8), intent(in)                :: p
        real(8), intent(out)               :: next_location_x
        next_location_x = location_x + step_length * p
    end subroutine newLocationMulti

end module conjugate_gradient_method