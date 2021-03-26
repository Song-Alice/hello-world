module input
    
    implicit none
    ! For one-dimensional quadratic function 
    !𝑓(𝑥)=𝑥^𝑇 𝑔^((𝑥))+1/2 𝑥^𝑇 𝐻𝑥
    type vector_x
    real(8)                                           :: x1
    real(8)                                           :: x2
    end type vector_x

    type oneDimensional_index
    real(8)                               :: H
    real(8)                               :: b 
    real(8)                               :: c
    end type oneDimensional_index

    type multiDimensional_index
    real(8)                               :: A1
    real(8)                               :: A2
    real(8)                               :: b1
    real(8)                               :: b2
    real(8)                               :: c
    real(8)                               :: d 
    end type MultiDimensional_index

    contains

    subroutine oneDimensional_inputindex (index)
        type(oneDimensional_index), intent(inout)                               :: index
        print *,'For one_dimensional_quadratic_function "f(x) = H x^2 + b x + c", enter the value of indexes.'
        print *,'H = (enter a real number)'
        read *, index%H
        print *,'b = (enter a real number)'
        read *, index%b
        print *,'c = (enter a real number)'
        read *, index%c
    end subroutine oneDimensional_inputindex

    subroutine one_dimensional_quadratic_function (index, location_x, location_y)
        type(oneDimensional_index), intent(in)             :: index 
        real(8), intent(in)                                :: location_x 
        real(8), intent(out)                               :: location_y
        location_y = index%H*location_x**2.0 + index%b*location_x + index%c
    end subroutine one_dimensional_quadratic_function

    subroutine multiDimensional_inputindex (index)
        type(multiDimensional_index), intent(inout)        :: index
        print *,'For function "f(x1,x2) = A1 x1^2 + A2 x2^2 + b1 x1 +b2 x2 + c x1 x2 + d", enter the value of indexes.'
        print *,'A1 = (enter a real number)'
        read *, index%A1
        print *,'A2 = (enter a real number)'
        read *, index%A2
        print *,'b1 = (enter a real number)'
        read *, index%b1
        print *,'b2 = (enter a real number)'
        read *, index%b2
        print *,'c = (enter a real number)'
        read *, index%c
        print *,'d = (enter a real number)'
        read *, index%d
    end subroutine multiDimensional_inputindex

    subroutine Multidimensional_quadratic_function (index, x, y)
        type(multiDimensional_index), intent(in)           :: index 
        type(vector_x), intent(in)                         :: x
        real(8), intent(out)                               :: y
        y = index%A1 * x%x1**2.0 + index%A2 * x%x2**2.0 + index%b1 * x%x1 + index%b2 * x%x2 + index%c * x%x1 * x%x2 + index%d
    end subroutine Multidimensional_quadratic_function

end module input 