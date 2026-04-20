program ex3
    
    real (8) :: random_x, random_y, mean_sum
    integer :: N, steps
    real (8) :: theoretical
    theoretical = 1.718281828

    call random_seed()
    
    N = 100000000
    steps = 10000000

    do i=steps, N, steps
        mean_sum = 0
        
        ! retangulo de base 1 e altura 
        do i2 = 1, i
    
            call random_number(random_x)    
            
            call exp(random_x, random_y)
    
            mean_sum  = mean_sum + random_y
        end do
    
        mean_sum = mean_sum/i
    
        print *, 'N:', i ,'value: ', mean_sum, 'error:', abs(theoretical-mean_sum), 1/sqrt(real(i))
    end do

    contains 
        subroutine exponential(val, res)
            real(8), intent(in)::val 
            real(8), intent(out)::res
            real(8) :: term, accumulator
            integer:: signal
            
            res = 1
            signal = 1
            accumulator = 1

            do i3=1, 1000000, 1
                term = val/i3
                accumulator = signal*accumulator*term
                if (abs(accumulator) <= 1e-10) then
                    exit
                end if

                res = res + accumulator
            end do

        end subroutine

end program