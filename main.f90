program main
   use input
   use calculation 
   use commonCalcuation
   use steepest_descent_method
   implicit none


   !real(8), dimension(:), allocatable  :: fx, x 
   real(8)                              :: index_H, index_b, index_c
   real(8)                              :: location_x 
   
   
   


   !call create_one_dimensional_quadratic_function (internal, x, fx)

   call inputindex (index_H, index_b, index_c)
   call steepest_descent (index_H, index_b, index_c, one_dimensional_quadratic_function)
   call ConjugateGradient (Multidimensional_quadratic_function)



   
   ! Calculate Hessian matrix.

      
      
   



end program main