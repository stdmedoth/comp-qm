program ex1
    real(8) :: del, r, u, x_new, p
    real(8) :: xi(100000)
    real(8) :: xi_mean, xi_xii_mean, xi_squared_mean, ck
    integer :: N, accepted, k

    call random_seed()
    N = 100000
    
    x_new = 0
    xi(1) = 0
    del = 2.5
    accepted = 0
    do i=1, N-1
        call random_number(r)
        r = -1 + 2*r
        x_new = xi(i) + del*r

        p = exp(0.5*(xi(i)*xi(i) - x_new*x_new))
        !print *, x_new, xi(i), p, del*r
        call random_number(u)
        if(u<p) then
            xi(i+1) = x_new
            accepted = accepted + 1
        else
            xi(i+1) = xi(i)
        end if
    end do
    print *, 'Aceitação: ', real(accepted)/real(N-1)

    
    open(1, file='graph_xi.csv')
    do i=1, N-1
        write(1, '(I8, A, F5.2)') i, ',', xi(i)
    end do

    close(1)

    open(1, file='graph_corr.csv')

    do i=1, N-1
        do k=0, i
            if (i+k > N-1) then
                exit
            end if
            xi_xii_mean = xi_xii_mean + xi(i)*xi(i+k)
            xi_mean = xi_mean + xi(i)
            xi_squared_mean = xi_squared_mean + xi(i)*xi(i)
            ck = (xi_xii_mean/i - (xi_mean/i)*(xi_mean/i))/(xi_squared_mean/i - xi_mean*xi_mean)
        end do
        write(1, '(I8, A, F8.2)') k, ',', ck
    end do
    close(1)

    open(1, file='graph_hist.csv')
    do i=1, N-1
        write(1, '(F8.2)') xi(i)
    end do
    close(1)

end program 