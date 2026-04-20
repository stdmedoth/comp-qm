program ex1

        real(4) :: dx
        real(4) :: y_i, y_ii
        real(4) :: x_i, x_ii
        real(4) :: summation
        real(4) :: a, b
        integer :: n

        a = 0e0
        b = 1e0
        dx = 0.000001
        n = nint((b-a)/dx)
        summation = 0
        
        
        x_i = a

        ! regra do trapezio
        call exponential(x_i, y_i)
        do i=1, n, 1
            x_ii = x_i + dx
            call exponential(x_ii, y_ii)
            !call exponential(x_i, y_i)

            summation = summation +  dx/2*(y_i + y_ii)
            x_i = x_i + dx
            y_i = y_ii
        end do 

        print *, summation

        a = 0e0
        b = 1e0
        x_i = a
        n = nint((b-a)/dx)

        ! regra de simpson
        call exponential(x_i, y_i)
        summation = dx/3*y_i    

        do i=1, n-1, 1

            x_i = a + i*dx 
            call exponential(x_i, y_i)
            
            if(mod(i,2) == 0)then
                summation = summation +  dx/3*(2*y_i)
            else
                summation = summation +  dx/3*(4*y_i)
            end if 

            
        end do 

        call exponential(b, y_i)
        summation = summation + dx/3e0*y_i
        

        print *, summation

        contains
            subroutine exponential(val, res)
                real(4), intent(in)::val 
                real(4), intent(out)::res
                real(4) :: term, accumulator
                integer:: signal
                
                res = 1
                signal = -1
                accumulator = 1

                do i2=1, 10000000, 1
                    term = val/i2
                    accumulator = signal*accumulator*term
                    if (abs(accumulator) <= 1e-8) then
                        exit
                    end if

                    res = res + accumulator
                end do

            end subroutine
end program


