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

end module input 