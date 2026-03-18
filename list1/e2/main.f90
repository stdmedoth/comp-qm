program e2
    real :: number_sp = 1, number_sp2
    real(8) :: number_dp = 1, number_dp2
    
    integer :: has_overflow = 0, has_underflow = 0
    
    print *, 'Iniciando overflow simple precision'

    do i = 1, 1000000, 1

        number_sp2 = number_sp*1.5
        ! print *, number_sp2
        if (number_sp2*1.5 == number_sp2) then
            print *, 'Overflow em ', number_sp
            has_overflow = 1
            exit
        end if
        number_sp = number_sp2
    end do

    if(has_overflow == 0) then
        print *, 'Sem overflow'
    end if

    print *, 'Iniciando underflow simple precision'

    number_sp = 1
    do i = 1, 100000, 1

        number_sp2 = number_sp*0.009
        ! print *, number_sp2
        if (number_sp2 == 0) then
            print *, 'Underflow em ', number_sp
            has_underflow = 1
            exit
        end if
        number_sp = number_sp2
    end do

    if(has_overflow == 0) then
        print *, 'Ainda nada'
    end if

        if(has_underflow == 0) then
        print *, 'Sem underflow'
    end if

    print *, 'Iniciando overflow double precision'
    has_overflow = 0
    do i = 1, 1000000, 1

        number_dp2 = number_dp*1.5
        ! print *, number_sp2
        if (number_dp2*1.5 == number_dp2) then
            print *, 'Overflow em ', number_dp
            has_overflow = 1
            exit
        end if
        number_dp = number_dp2
    end do

    if(has_overflow == 0) then
        print *, 'Sem overflow'
    end if

    print *, 'Iniciando underflow double precision'
    has_underflow = 0
    number_dp = 1
    do i = 1, 100000, 1

        number_dp2 = number_dp*0.009
        ! print *, number_sp2
        if (number_dp2 == 0) then
            print *, 'Underflow em ', number_dp
            has_underflow = 1
            exit
        end if
        number_dp = number_dp2
    end do

    if(has_overflow == 0) then
        print *, 'Ainda nada'
    end if

        if(has_underflow == 0) then
        print *, 'Sem underflow'
    end if


end program e2