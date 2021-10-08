program main
    implicit none
    integer :: n
    print "(a)", "Numbers n is "
    read *, n
    if (is_even(n)) then
        print "(a)", "n is even"
    else
        print "(a)", "n is odd"
    end if
contains
    recursive function is_even(n) result(even)
        logical             :: even
        integer, intent(in) :: n
        if (n > 0) then
            even = is_odd(n - 1)
        else if (n == 0) then
            even = .true.
        end if
    end function is_even

    recursive function is_odd(n) result(odd)
        logical             :: odd
        integer, intent(in) :: n
        if (n > 0) then
            odd = is_even(n - 1)
        else if (n == 0) then
            odd = .false.
        end if
    end function is_odd
end program main