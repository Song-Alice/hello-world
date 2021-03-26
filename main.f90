program main
   use input 
   use commonCalcuation
   use steepest_descent_method
   use conjugate_gradient_method
   use example
   implicit none


   !real(8), dimension(:), allocatable  :: fx, x 
   type (oneDimensional_index)                      :: index_OneDimensional
   type (multiDimensional_index)                    :: index_MultiDimensional 
   integer                                          :: Dimension
   !integer                                          :: One_Dimension = 1, Two_Dimension = 2

   !call create_one_dimensional_quadratic_function (internal, x, fx)
   print *, 'Please choose the dimension of quadratic function. Enter ''1'' for 1D-function. Enter ''2'' for 2D-function.&
    & Enter other integer for both.'
   read *, Dimension 
   select case (Dimension)
      case(1)
        !Calculate 1D quadratic function by Steepest Descent Method.
        call oneDimensional_inputindex (index_OneDimensional)
        call steepest_descent (index_OneDimensional, one_dimensional_quadratic_function)
      case(2)
        !Calculate 2D quadratic function by Conjugate Gradient Method.
        call multiDimensional_inputindex (index_MultiDimensional)
        call ConjugateGradient (index_MultiDimensional, Multidimensional_quadratic_function)
      case(3)
        call multiDimensional_inputindex (index_MultiDimensional)
        call example2 (index_MultiDimensional)
      case default
        call oneDimensional_inputindex (index_OneDimensional)
        call steepest_descent (index_OneDimensional, one_dimensional_quadratic_function)
        call multiDimensional_inputindex (index_MultiDimensional)
        call ConjugateGradient (index_MultiDimensional, Multidimensional_quadratic_function)
   end select

end program main