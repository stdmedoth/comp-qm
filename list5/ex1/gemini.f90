module poly_mod
    implicit none
contains
    ! Subrotina para calcular P(lambda) e sua derivada P'(lambda)
    subroutine eval_poly(n, lambda, p, dp)
        integer, intent(in) :: n
        real(8), intent(in) :: lambda
        real(8), intent(out) :: p, dp
        
        real(8) :: p0, p1, p2
        real(8) :: dp0, dp1, dp2
        integer :: i
        
        ! Valores iniciais para m=0 e m=1
        p0 = 1.0d0
        dp0 = 0.0d0
        
        p1 = -2.0d0 - lambda
        dp1 = -1.0d0
        
        if (n == 0) then
            p = p0; dp = dp0
            return
        end if
        if (n == 1) then
            p = p1; dp = dp1
            return
        end if
        
        ! Relacao de recorrencia para m >= 2
        do i = 2, n
            p2 = (-2.0d0 - lambda) * p1 - p0
            dp2 = -p1 + (-2.0d0 - lambda) * dp1 - dp0
            
            ! Atualiza os valores para o proximo passo
            p0 = p1;  p1 = p2
            dp0 = dp1; dp1 = dp2
        end do
        
        p = p2
        dp = dp2
    end subroutine eval_poly
end module poly_mod

program lista5_ex1
    use poly_mod
    implicit none
    
    integer :: n, i, j, max_iter
    real(8) :: lambda, p, dp, lambda_new, erro
    real(8) :: lambda_exato
    real(8), allocatable :: raizes(:)
    real(8) :: pi
    real(8) :: scan_step, l_test, p_test, p_prev
    
    pi = 4.0d0 * atan(1.0d0)
    
    ! O tamanho n eh um input do codigo
    write(*,*) "Digite o valor de n:"
    read(*,*) n
    
    allocate(raizes(n))
    
    max_iter = 100
    ! Passo fino para varredura entre -4 e 0
    scan_step = 4.0d0 / (20.0d0 * n) 
    l_test = -4.0d0
    call eval_poly(n, l_test, p_prev, dp)
    
    j = 0 ! Contador de raizes encontradas
    
    ! Busca das raizes (Varredura + Newton-Raphson)
    do while (l_test <= 0.0d0 .and. j < n)
        l_test = l_test + scan_step
        call eval_poly(n, l_test, p_test, dp)
        
        ! Verifica se houve mudanca de sinal (indica que uma raiz foi cruzada)
        if (p_prev * p_test <= 0.0d0) then
            ! Chute inicial para o Newton-Raphson: o meio do intervalo atual
            lambda = l_test - scan_step / 2.0d0
            
            ! Iteracoes do Metodo de Newton-Raphson
            do i = 1, max_iter
                call eval_poly(n, lambda, p, dp)
                
                if (abs(dp) < 1.0d-14) exit ! Evita divisao por zero
                
                lambda_new = lambda - p / dp
                erro = abs(lambda_new - lambda)
                lambda = lambda_new
                
                ! Criterio de parada
                if (erro < 1.0d-10) exit
            end do
            
            j = j + 1
            raizes(j) = lambda
        end if
        
        p_prev = p_test
    end do
    
    ! Comparacao numerica vs exata
    write(*,*) " k |    Numerico    |      Exato     |      Erro"
    write(*,*) "----------------------------------------------------"
    do i = 1, n
        ! Formula exata fornecida
        lambda_exato = -4.0d0 * (sin( (i * pi) / (2.0d0 * (n + 1.0d0)) ))**2
        
        write(*,'(I3,A,F14.8,A,F14.8,A,E12.4)') &
            i, " | ", raizes(i), " | ", lambda_exato, " | ", abs(raizes(i) - lambda_exato)
    end do
    
    deallocate(raizes)
end program lista5_ex1
