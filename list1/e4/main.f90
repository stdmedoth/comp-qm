program e4

    real(8) :: x = 0.1, result = 0

    do i=1, 4
        call exponential(x, result)
        print *, i, 'Valor para ', x , ' : ', result
        x = x*10
        ! exit
    end do


    contains
        subroutine exponential(x_val, res)
            real(8), intent(in) :: x_val
            real(8), intent(out) :: res
            real(8) :: termo
            integer :: i2

            res = 1.0d0
            termo = 1.0d0
            
            do i2 = 1, 1000
                ! A relação correta de Taylor:
                ! Termo_n = Termo_{n-1} * (-x / n)
                termo = -termo * x_val / dble(i2) 
                res = res + termo
                
                if (abs(termo) < 1.0d-8) exit
            end do
        end subroutine


end program