module steepest_descent_method
    use input 
    use commonCalcuation

    implicit none
    contains

    subroutine steepest_descent (index, location)
        type(oneDimensional_index), intent(in)          :: index 
        real(8)                                         :: internal = 0.1, h = 0.01, initial_x = -10.0
        real(8)                                         :: g, previous_g 
        real(8)                                         :: location_x, previous_location_x, next_location_x 
        real(8)                                         :: p, step_length
        real(8)                                         :: previous_location_y, location_y, location_y1, location_y2
        real(8)                                         :: minimum_difference = 0.01
        integer                                         :: iteration = 0
        
        interface

           subroutine location (indexinput, value_x, value_y)
              import                                        :: oneDimensional_index
              type(oneDimensional_index), intent(in)        :: indexinput 
              real(8), intent(in)                           :: value_x
              real(8), intent(out)                          :: value_y
           end subroutine location 
        end interface
        !Initialize the values.
        location_x = initial_x 
        call location (index, location_x + h, location_y2)
        call location (index, location_x - h, location_y1)
        call gradient_evaluation (location_y1, location_y2, h, g)
        previous_g = g 
        previous_location_x = location_x 
        location_x = previous_location_x + internal 
        iteration = 1
        print *,'location_x', location_x, 'g', g 
  
       !Calculate by steepest descent method.
        do while (iteration <= 100)
          !Calculation gradient at the location.
          call location (index, location_x + h, location_y2)
          call location (index, location_x - h, location_y1)
          call gradient_evaluation (location_y1, location_y2, h, g)
          !call residual (index_H, index_b, location_x, location_r)
          !call stepmultiDimensional (location_r, index_H, step_length)
          !call newLocationMulti (location_x, step_length, location_r, next_location_x)
          call stepOneDimensional (g, previous_g, location_x, previous_location_x, step_length)
          call direction (g, p)
          call newLocation (location_x, step_length, p, next_location_x)
          previous_location_x = location_x
          location_x = next_location_x
          previous_g = g 
          iteration = iteration + 1 
          print *, 'previous_x', previous_location_x, 'previous_g', previous_g, 'steplength', step_length
          print *, 'x', location_x
          call location (index, previous_location_x, previous_location_y)
          call location (index, location_x, location_y)
          if ((location_x - previous_location_x) <= minimum_difference) exit 
        enddo
        print *, 'iteration', iteration
        print *, 'location', 'x=', location_x, 'f(x)=', location_y
    end subroutine steepest_descent

    subroutine stepOneDimensional (g, previous_g, location_x, previous_location_x, step_length)
        real(8), intent(in)                :: g, previous_g, location_x, previous_location_x
        real(8), intent(out)               :: step_length
        step_length = (location_x - previous_location_x) * (g - previous_g) / (g - previous_g)**2.0
    end subroutine stepOneDimensional

    subroutine direction (g, p)
        real(8), intent(in)                :: g
        real(8), intent(out)               :: p 
        p = -g 
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