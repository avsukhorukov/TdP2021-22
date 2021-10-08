program main
    implicit none
    integer :: n, ans
    print "(a)", "Number n is "
    read *, n
    print "(a, i0)", "n! is ", factorial_f(n)
    call factorial_s(n, ans)
    print "(a, i0)", "n! is ", ans
contains
    recursive function factorial_f(i) result(answer)
        integer             :: answer
        integer, intent(in) :: i

        if (i == 0) then
            answer = 1
        else
            answer = i * factorial_f(i - 1)
        end if
    end function factorial_f

    recursive subroutine factorial_s(i, answer)
        integer, intent(in)    :: i
        integer, intent(inout) :: answer

        if (i == 0) then
            answer = 1
        else
            call factorial_s(i - 1, answer)
            answer = i * answer
        end if
    end subroutine factorial_s
end program main
