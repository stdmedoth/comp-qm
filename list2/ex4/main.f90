program ex4
    integer :: N 
    real(8) :: sum, x,y,w
    N = 1000000000
    sum = 0
    call random_seed()
    do i=1, N
        call random_number(y)
        x = 1 - sqrt(1-y)
        w = 2 - 2*x
        sum = sum + exp(x)/w
    end do

    sum = sum/N

    print *, sum
end program