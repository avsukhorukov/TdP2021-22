! 1) a(i) = a(i)**2
! 2) deallocate(a)
! 3) deallocate(iptr)
program array_valgrind
    integer, parameter :: N = 5
    integer :: i
    real, allocatable :: a(:)
    integer, pointer :: iptr

    allocate(a(N))
    a = [ (real(i), i = 1, N) ]
    do i = 1, N
        a(i) = a(i + 1)**2 
    end do

    allocate(iptr)
    allocate(iptr)
end program array_valgrind