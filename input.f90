module input
    implicit none
    ! For one-dimensional quadratic function 
    !𝑓(𝑥)=𝑥^𝑇 𝑔^((𝑥))+1/2 𝑥^𝑇 𝐻𝑥
    contains 

    subroutine create_one_dimensional_quadratic_function (internal, x, fx)
        real(8), intent(in)                               :: internal 
        real(8), intent(inout), dimension(:), allocatable :: x, fx         
        real(8)                                           :: a, b
        integer                                           :: ui=17, size=0, i 
        a = -5.0
        open (unit=ui, file='input.txt', status='old', action='write') 
        do while (a<=5.0)
            b = a**2.0     !y=x^2
            write (ui,*) a, b
            a = a + internal
            size = size+1
        enddo
        close(ui) 
        allocate (fx(size), x(size))
        open (unit=ui, file='input.txt', status='old', action='read')
        do i = 1, size 
            read (ui,*) a, b
            x(i) = a
            fx(i) = b
        enddo
        close(ui)
        
    end subroutine create_one_dimensional_quadratic_function

    subroutine inputindex (index_H, index_b, index_c)
        real(8), intent(inout)                               :: index_H, index_b, index_c
        print *,'For one_dimensional_quadratic_function "f(x) = H x^2 + b x + c", enter the value of indexes.'
        print *,'H = (enter a real number)'
        read *, index_H
        print *,'b = (enter a real number)'
        read *, index_b
        print *,'c = (enter a real number)'
        read *, index_c
    end subroutine inputindex

    subroutine one_dimensional_quadratic_function (index_H, index_b, index_c, location_x, location_y)
        real(8), intent(in)                                :: location_x, index_H, index_b, index_c 
        real(8), intent(out)                               :: location_y
        location_y = index_H*location_x**2.0 + index_b*location_x + index_c
    end subroutine one_dimensional_quadratic_function

end module input 