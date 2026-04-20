program e4
    real(8) :: x = 0.10, result = 0
    do i=0, 4, 1
        call exponential(x, result)
        print *, "result for ", x, result
        x = x*10
    end do

    contains
        subroutine exponential(val, res)
            real(8), intent(in)::val 
            real(8), intent(out)::res
            real(8) :: term, accumulator
            integer:: signal
            
            res = 1
            signal = -1
            accumulator = 1

            do i2=1, 1000000, 1
                term = val/i2
                accumulator = signal*accumulator*term
                if (abs(accumulator) <= 1e-10) then
                    exit
                end if

                res = res + accumulator
            end do

        end subroutine
end program