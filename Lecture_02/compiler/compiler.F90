program compiler
    implicit none
    integer :: k = 5
    integer :: f
    print '(i0)', f(k)
#ifdef DEBUG
    print *, "Debugging message!"
#endif
end program compiler
