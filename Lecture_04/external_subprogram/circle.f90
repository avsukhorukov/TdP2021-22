subroutine get_area(radius, area)
    implicit none
    real, intent(in) :: radius(:)
    real, intent(out) :: area(:)
    area = 3.1415926 * radius**2
end subroutine get_area

program circle
    interface
        subroutine get_area(radius, area)
            real, intent(in) :: radius(:)
            real, intent(out) :: area(:)
        end subroutine get_area
    end interface
    real :: r(3), a(3)
    r = [1.0, 2.0, 3.0]
    call get_area(r, a)
    print '(3(f0.2, 2x))', a ! 3.14  12.57  28.27
end program circle