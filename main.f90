program main
   use input
   use calculation 
   use commonCalcuation
   use steepest_descent_method
   implicit none


   !real(8), dimension(:), allocatable  :: fx, x 
   type (oneDimensional_index)                      :: index_OneDimensional
   type (multiDimensional_index)                    :: index_MultiDimensional
   real(8)                                          :: location_x 
   
   
   


   !call create_one_dimensional_quadratic_function (internal, x, fx)

   !Calculate 1D quadratic function by Steepest Descent Method.

   call oneDimensional_inputindex (index_OneDimensional)
   call steepest_descent (index_OneDimensional, one_dimensional_quadratic_function)

   !Calculate 2D quadratic function by Conjugate Gradient Method.
   call multiDimensional_inputindex (index_MultiDimensional)
   call ConjugateGradient (index_MultiDimensional, Multidimensional_quadratic_function)



   
   ! Calculate Hessian matrix.

      
      
   



end program main