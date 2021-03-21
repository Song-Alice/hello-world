program main
   use input
  ! use calculation 
   use commonCalcuation
   use steepest_descent_method
   implicit none
   real(8), dimension(:), allocatable  :: fx, x 
   real(8)                             :: index_H, index_b, index_c
   real(8)                             :: internal = 0.1, h = 0.01, initial_x = -10.0
   real(8)                             :: g, previous_g, location_x, previous_location_x, step_length
   real(8)                             :: p 
   real(8)                             :: next_location_x, previous_location_y, location_y
   real(8)                             :: minimum_difference = 0.001
   integer                             :: literation = 0

   
   !print *,'For one_dimensional_quadratic_function "𝑓(𝑥) = 𝐻 𝑥^2 + 𝑏 𝑥 + 𝑐", enter the value of indexes.'
   !print *,'H = (enter a real number)'
   !read *, index_H
   !print *,'b = (enter a real number)'
   !read *, index_b
   !print *,'c = (enter a real number)'
   !read *, index_c

   call inputindex (index_H, index_b, index_c)
   call create_one_dimensional_quadratic_function (internal, x, fx)
 
   !call mainCalculation (one_dimensional_quadratic_function)
      
      !Initialize the values.
      call gradient_evaluation (index_H, index_b, index_c, one_dimensional_quadratic_function, h, initial_x, g)
      previous_g = g 
      previous_location_x = initial_x 
      location_x = previous_location_x + internal 
      literation = 1
      print *,'location_x', location_x, 'g',g 
  
     !Calculate by steepest descent method.
      do  
        !Calculation gradient at the location.
        call gradient_evaluation (index_H, index_b, index_c, one_dimensional_quadratic_function, h, location_x, g)
        call stepOneDimensional (g, previous_g, location_x, previous_location_x, step_length)
        call direction (g, p)
        call newLocation (location_x, step_length, p, next_location_x)
        previous_location_x = location_x
        location_x = next_location_x
        previous_g = g 
        literation = literation + 1 
        print *, 'previous_x', previous_location_x, 'previous_g', previous_g, 'steplength', step_length
        print *, 'x', location_x
        call one_dimensional_quadratic_function (index_H, index_b, index_c, previous_location_x, previous_location_y)
        call one_dimensional_quadratic_function (index_H, index_b, index_c, location_x, location_y)
        if ((location_x - previous_location_x) <= minimum_difference) exit 
      enddo
      print *, 'literation', literation
      print *, 'location', 'x=', location_x, 'f(x)=', location_y
   



end program main