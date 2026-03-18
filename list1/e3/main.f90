program e3
    real :: epsilon_sp = 1
    real(8) :: epsilon_dp = 1
    
    print *, 'Iniciando teste com precisao simples'
    do i=1, 1000000000, 1
        epsilon_sp = epsilon_sp/1.1
        if(epsilon_sp + 1 == 1) then
            print *, 'Encontrado ', epsilon_sp
            exit
        end if
    end do


    print *, 'Iniciando teste com precisao dupla'
    do i=1, 1000000000, 1
        epsilon_dp = epsilon_dp/1.1
        if(epsilon_dp + 1 == 1) then
            print *, 'Encontrado ', epsilon_dp
            exit
        end if
    end do


end program e3