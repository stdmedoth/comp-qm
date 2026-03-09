subroutine calculate_area


end subroutine

program e1
    implicit none


    integer :: radius
    real :: area

    print *, 'Insira o raio'
    read(*,*) radius 

    area = 3.14 * radius * radius

    print *, 'A area é ', area


end program