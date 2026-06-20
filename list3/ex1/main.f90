program ex1
    integer :: N
    real(8) :: rn, del, proposal_x
    real(8) :: acceptance_ratio
    real(8), dimension(10000) :: x
    real(8) :: mean_xi_xipk, mean_xi, mean_squared_xi, mean_xi_squared
    
    integer :: i, accepted
    call random_seed()
    x(1) = 0
    del = 20d0
    N = 1e4
    accepted = 0

    do i=1, N-1
        call random_number(rn)
        rn = -1 + 2*rn ! -interval + length * number 
        !print *, rn
        proposal_x = x(i) + del*rn
        acceptance_ratio = exp((x(i)**2 - proposal_x**2)/2)
        
        call random_number(rn)
        if(rn < acceptance_ratio)then
            x(i+1) = proposal_x
            accepted = accepted+1
        else
            x(i+1) = x(i)
        end if
    end do
    print *, "Acceptance ratio" , real(accepted)/real(N-1)

    ! ploting x for steps
    open(1, file='xi.csv')
    do i=1, N-1
        write (1, *) i, x(i)
    end do
    close(1)

    open(1, file='ck.csv')

    ! ploting the autocorrelation
    do k=1, 100
        mean_xi_xipk = 0
        mean_xi = 0
        mean_xi_squared = 0
        mean_squared_xi = 0
        do i=1, N-k
            mean_xi_xipk = mean_xi_xipk + x(i)*x(i+k)
            mean_xi = mean_xi + x(i)
            mean_squared_xi = mean_squared_xi + x(i)*x(i)
            
        end do 
        mean_xi_xipk = mean_xi_xipk/real(N-k)
        mean_xi = mean_xi/real(N-k)
        mean_squared_xi = mean_squared_xi/real(N-k)
        mean_xi_squared = (mean_xi*mean_xi)
        write (1, *) k,  (mean_xi_xipk - mean_xi_squared)/(mean_squared_xi - mean_xi_squared)
    end do
    close(1)

    open(1, file='hist.dat')
    do i=1, N-1
        write(1, *) x(i)
    end do
    close(1)

end program