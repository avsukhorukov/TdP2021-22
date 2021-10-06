! Fortran passes by reference
program both_ways
    implicit none
    real :: a = 0.0
    real :: b = 0.0
    call sub(a, b)
    print "(f3.1, x, f3.1)", a, b
    ! 1.0 0.0
contains
    subroutine sub(x, y)
        ! x is passed by reference,
        ! y is passed by value
        real :: x
        real, value :: y
        x = x + 1.0
        y = y + 1.0
    end subroutine sub
end program both_ways
