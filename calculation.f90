module calculation
    use input 
    use commonCalcuation
    use steepest_descent_method
    use conjugate_gradient_method
    implicit none

    contains 
    subroutine steepest_descent (index, location)
        type(oneDimensional_index), intent(in)              :: index
       ! real(8)                                            :: location_r 
        real(8)                                             :: internal = 0.1, h = 0.01, initial_x = -10.0
        real(8)                                             :: g, previous_g, location_x, previous_location_x, step_length
        real(8)                                             :: p 
        real(8)                                             :: next_location_x, previous_location_y, location_y
        real(8)                                             :: minimum_difference = 0.01
        integer                                             :: literation = 0
        
        interface
           subroutine location (index, value_x, value_y)
              type(oneDimensional_index), intent(in)        :: index
              real(8), intent(in)                           :: value_x
              real(8), intent(out)                          :: value_y
           end subroutine location 
        end interface
        !Initialize the values.
        call gradient_evaluation (index, location, h, initial_x, g)
        previous_g = g 
        previous_location_x = initial_x 
        location_x = previous_location_x + internal 
        literation = 1
        print *,'location_x', location_x, 'g',g 
  
       !Calculate by steepest descent method.
        do while (literation <= 100)
          !Calculation gradient at the location.
          call gradient_evaluation (index, location, h, location_x, g)
          !call residual (index_H, index_b, location_x, location_r)
          !call stepmultiDimensional (location_r, index_H, step_length)
          !call newLocationMulti (location_x, step_length, location_r, next_location_x)
          call stepOneDimensional (g, previous_g, location_x, previous_location_x, step_length)
          call direction (g, p)
          call newLocation (location_x, step_length, p, next_location_x)
          previous_location_x = location_x
          location_x = next_location_x
          previous_g = g 
          literation = literation + 1 
          print *, 'previous_x', previous_location_x, 'previous_g', previous_g, 'steplength', step_length
          print *, 'x', location_x
          call location (index, previous_location_x, previous_location_y)
          call location (index, location_x, location_y)
          if ((location_x - previous_location_x) <= minimum_difference) exit 
        enddo
        print *, 'literation', literation
        print *, 'location', 'x=', location_x, 'f(x)=', location_y
    end subroutine steepest_descent

    subroutine ConjugateGradient (index, location)
        type(vector_x)                                        :: x 
        type(multiDimensional_index), intent(in)              :: index
        real(8), dimension(:,:), allocatable                  :: HessianMatrix
        integer                                               :: n = 2 
   
        real(8)                                               :: location_r, next_location_r, previous_location_r 
        real(8)                                               :: internal = 0.1, h = 0.01, initial_x = -10.0
        real(8)                                               :: step_length
        real(8)                                               :: p, previous_p 
        real(8)                                               :: previous_location_y, location_y
        real(8)                                               :: minimum_difference = 0.01
        integer                                               :: literation = 0

        allocate(HessianMatrix(n,n))

        interface
           subroutine location (index, value_x, value_y)
              type(multiDimensional_index), intent(in)        :: index
              real(8), intent(in)                             :: value_x
              real(8), intent(out)                            :: value_y
           end subroutine location 
        end interface

        ! Initialize the value of x0, r0, and p0.
        x%x1 = initial_x
        x%x2 = initial_x
        location_r = index_b - index_H * location_x
        p = location_r 
        do while (literation <= 100)
            call stepMultiDimensional (location_r, index_H, p, step_length)
            print *, 'step length', step_length
            call newLocationMulti (location_x, step_length, p, next_location_x)
            previous_location_x = location_x 
            location_x = next_location_x
            call location (index_H, index_b, index_c, location_x, location_y)
            literation = literation + 1 
            call residual (index_H, step_length, location_r, p, next_location_r)
            if (next_location_r <= minimum_difference) exit 
            previous_location_r = location_r
            location_r = next_location_r
            previous_p = p 
            call directionMultiDimensional (location_r, previous_location_r, previous_p, p)
        enddo
        print *, 'literation', literation
        print *, 'location', 'x=', location_x, 'f(x)=', location_y
        print *, 'residual', next_location_r
    end subroutine ConjugateGradient
            



    
end module calculation