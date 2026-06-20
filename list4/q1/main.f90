program q1
    real(8) :: x, xi, term
    integer:: N

    N = 1000
    x = 2
    xi = x

    term = fun(xi)/difffun(xi)
        
    do while(term > 1e-30)
        xi = x
        term = fun(xi)/difffun(xi)
        x = xi - term

    end do

    print*,x

    contains
        real(8) function fun(x_val)
            real(8), intent(inout)::x_val
            fun = -2d0*x_val**2 + 1d0
            !print *, fun
        end

        real(8) function difffun(x_val)
            real(8), intent(inout)::x_val
            difffun = -4d0*x_val
        end

end program