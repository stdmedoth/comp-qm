program ex2

    real(8) :: random, sum, r, v, two_d
    real(8), dimension(100000) :: correct, errors
    real(8), dimension(100000,3) ::coords
    integer :: correct_qnt, errors_qnt, d
    
    print *, 'Insira a dimensão d'
    read (*,*) d 

    v = 0
    correct_qnt = 0
    errors_qnt = 0
    
    call random_seed()
    do i=1, 100000,1
        sum = 0
        do i2=1, d,1
            call random_number(random)
            random = -1.0 + (2.0 * random)
            coords(i, i2) = random
            sum = sum + random*random
        end do

        r = sqrt(sum)
        if(r <= 1)then
            correct(correct_qnt+1) = r
            correct_qnt = correct_qnt+1
        else 
            errors(errors_qnt+1) = r
            errors_qnt = errors_qnt+1
        end if
        
    end do
    print *, correct_qnt, errors_qnt

    two_d = 1
    do i=1,d
        two_d = two_d*2
    end do 
    
    v = (real(correct_qnt)/real(correct_qnt+errors_qnt))*two_d
    print *, v, two_d

    open(1, file='data.csv')
    do i=1, correct_qnt,1    
        sum = 0
        do i2=1, d,1
            sum = sum + coords(i, i2)*coords(i, i2)
        end do
        r = sqrt(sum)
        ! Escreve as coordenadas na mesma linha separadas por espaço
        if(r <= 1)then
            if (d == 2) then
                write(1, '(2F12.6)') coords(i, 1), coords(i, 2)
            else if (d == 3) then
                write(1, '(3F12.6)') coords(i, 1), coords(i, 2), coords(i, 3)
            end if
        end if
    end do
    close(1)


end program