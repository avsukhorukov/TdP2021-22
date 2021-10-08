! Power function using recursion
!
! An integer base \( b > 0 \) can be raised to an integer power \( p > 0 \)
! using a simple recurrence relation,
! \[
!   b^p = b \cdot b^{p - 1}.
! \]
! The base case is \( b^0 = 1 \).
!
! TODO: try to incorporate case p < 0 with divisions for real numbers.
program main
    implicit none
    integer :: n, m, ans
    print "(a)", "Numbers n and m are "
    read *, n, m
    print "(a, i0)", "n^m is ", power_f(n, m)
    call power_s(n, m, ans)
    print "(a, i0)", "n^m is ", ans
contains
    recursive function power_f(b, p) result(answer)
        integer             :: answer
        integer, intent(in) :: b, p
        if (p == 0) then
            answer = 1
        else
            answer = b * power_f(b, p - 1)
        end if
    end function power_f

    recursive subroutine power_s(b, p, answer)
        integer, intent(in)    :: b, p
        integer, intent(inout) :: answer
        if (p == 0) then
            answer = 1
        else
            call power_s(b, p - 1, answer)
            answer = b * answer
        end if
    end subroutine power_s
end program main