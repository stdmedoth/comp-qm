! calcular integral e^x usando importance sampling

program ex4
    real(8) :: y, x, sum, wx
    real(8) :: teoretical
    integer :: N, step
    call random_seed()
    N = 1e2
    
    teoretical = exp(1d0)-1d0
        
    do step=1, 6
        sum = 0
        N = N * 10

        do i=1, N*step
            call random_number(y)
            x = 1d0-sqrt(1d0-y)
            wx = 2d0 -2d0*x
            !x = sqrt(1d0+3d0*y) - 1
            !wx = 2d0*(1+x)/3

            sum = sum + exp(x)/wx
        end do

        sum = real(sum)/real(N*step)
        print *, step, sum, teoretical, abs(teoretical - sum)
    end do

 
end program