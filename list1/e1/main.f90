
program e1
    implicit none

    real :: area, radius ! simple precision  
    !real(8) :: area, radius ! double precision


    print *, 'Insira o raio'
    read(*,*) radius 

    call calculate_area(radius, area)

    print *, 'A area é ', area

    contains

        subroutine calculate_area(r, result)
            real, intent(in) ::   r
            real, intent(out) :: result
            result =  3.14159265358979323846 * r * r

        end subroutine
        

end program