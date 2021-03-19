program main
   use input 
   use commonCalcuation
   use steepest_descent_method
   implicit none
   real(8)                             :: internal, h, initial_x = 0.0
   real(8)                             :: g, previous_g, location_x, previous_location_x, step_length
   real(8)                             :: p 
   real(8)                             :: next_location_x, previous_location_y, location_y
   real(8)                             :: minimum_difference = 0.01
   real(8), dimension(:), allocatable  :: fx, x 
   integer                             :: literation = 0

   internal = 0.1
   h=0.01
   call create_one_dimensional_quadratic_function (internal, x, fx)

   !Initialize the values.
   call gradient_evaluation (h, initial_x, g)
   previous_g = g 
   previous_location_x = initial_x 
   location_x = previous_location_x + internal 
   literation = 1

   !Calculate by steepest descent method.
   do  
      !Calculation gradient at the location.
      call gradient_evaluation (h, location_x, g)
      call step (g, previous_g, location_x, previous_location_x, step_length)
      call direction (g, p)
      call newLocation (location_x, step_length, p, next_location_x)
      previous_location_x = location_x
      location_x = next_location_x
      previous_g = g 
      literation = literation + 1 
      call y (previous_location_x, location_x, previous_location_y, location_y)
      if ((location_y - previous_location_y) <= minimum_difference) exit 
   enddo

   print *, 'literation', literation
   print *, 'location', 'x=', location_x, 'f(x)=', location_y


end program main