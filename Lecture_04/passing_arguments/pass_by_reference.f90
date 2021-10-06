program pass_by_reference
    implicit none
    integer :: i = 1                           ! actual argument
    print '(a, z0)', "Host: &i = ", loc(i)     ! 5588708F3010
    call sub(i)
    print *, i ! now `i` is 5
contains
    subroutine sub(n)
        integer :: n                           ! dummy argument
        print '(a, z0)', "Sub:  &n = ", loc(n) ! 5588708F3010
        n = 5
    end subroutine sub
end program pass_by_reference