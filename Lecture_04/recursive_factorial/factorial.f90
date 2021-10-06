program main
    implicit none
    integer :: n
    print "(a)", "Number n is "
    read *, n
    print "(a, i0)", "n! is ", factorial(n)
contains
    recursive function factorial(i) result(answer)
        integer             :: answer
        integer, intent(in) :: i
        if (i == 0) then
            answer = 1
        else
            answer = i * factorial(i - 1)
        end if
    end function factorial
end program main