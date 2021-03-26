module test
    use input 
    use commonCalcuation
    use steepest_descent_method
    use conjugate_gradient_method
    implicit none
    contains

    subroutine test_steepest_descent_method (location)
        type(oneDimensional_index)               :: index_OneDimensional
        interface
          subroutine location (indexinput, value_x, value_y)
            import                                        :: oneDimensional_index
            type(oneDimensional_index), intent(in)        :: indexinput 
            real(8), intent(in)                           :: value_x
            real(8), intent(out)                          :: value_y
          end subroutine location 
        end interface
        index_OneDimensional%H = 2.0
        index_OneDimensional%b = -2.0
        index_OneDimensional%c = 3.0
        print *,' The test function is f(x) = 2.0 * x^2 - 2.0 * x + 3.0 '
        call steepest_descent (index_OneDimensional, location)
    end subroutine test_steepest_descent_method

    subroutine test_conjugate_gradient_method (location)
      type(multiDimensional_index)               :: index_multiDimensional
      interface
        subroutine location (indexinput, value_x, value_y)
        import                                          :: multiDimensional_index
        import                                          :: vector_x
        type(multiDimensional_index), intent(in)        :: indexinput
        type(vector_x), intent(in)                      :: value_x
        real(8), intent(out)                            :: value_y
      end subroutine location 
      end interface
      index_multiDimensional%A1 = -2.0
      index_multiDimensional%A2 =  2.0
      index_multiDimensional%b1 = -3.0
      index_multiDimensional%b2 = -2.0
      index_multiDimensional%c  =  4.0
      index_multiDimensional%d  = -5.0
      print *, 'f(x1,x2) = -2.0 * x1^2 + 2.0 * x2^2 - 3.0 * x1 - 2.0 * x2 + 4.0 * x1 x2 - 5.0'
      call ConjugateGradient (index_multiDimensional, location)
    end subroutine test_conjugate_gradient_method

end module test
