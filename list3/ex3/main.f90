program ex3
    real(8) :: sum, u, x, xi, alpha, y
    integer(8) :: i, N

    sum = 0d0
    N = 1e6
    xi = 0
    call random_seed()


    do i=1, N
        call random_number(y)
        x = -log(y)
        !alpha = (x/xi)*exp(xi-x) !acceptance ratio
        alpha = (x/xi)
        
        call random_number(u)
        if(u < alpha) then
            xi = x
        end if

        sum = sum + (xi+2)
    end do
    print *, sum/N

end program