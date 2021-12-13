! Note: I changed `n` to `s(ize)` to have indices ordered alphabetically.
program tsp_7loops
    implicit none
    integer :: i, j, k, l, m, n, o, s, distance, min_distance
    integer, allocatable :: d(:, :), min_route(:)

    print '(a)', "How many places?"
    read *, s
    if (s /= 7) stop "This version works only if `s` is 7."

    allocate(d(s, s))
    print '(a)', "Distances between the places?"
    do i = 1, s
        read *, d(i, :)
    end do

    allocate(min_route(s))
    min_distance = huge(0)

    first: do i = 1, s
        second: do j = 1, s
            if (i == j) cycle
            third: do k = 1, s
                if (i == k .or. j == k) cycle
                fourth: do l = 1, s
                    if (i == l .or. j == l .or. k == l) cycle
                    fifth: do m = 1, s
                        if (i == m .or. j == m .or. k == m .or. l == m) cycle
                        sixth: do n = 1, s
                            if (i == n .or. j == n .or. k == n .or. l == n .or. m == n) cycle
                            seventh: do o = 1, s
                                if (i == o .or. j == o .or. k == o .or. l == o .or. m == o .or. n == o) cycle
                                distance = d(i, j) + d(j, k) + d(k, l) + d(l, m) &
                                           + d(m, n) + d(n, o) + d(o, i) 
                                if (distance < min_distance) then
                                    min_distance = distance
                                    min_route(:) = [i, j, k, l, m, n, o]
                                end if
                            end do seventh
                        end do sixth
                    end do fifth
                end do fourth
            enddo third
        end do second
    end do first

    print '(a, i0, a)', "The minimum value ", min_distance, " is for the following route:"
    print '(*(i2, :, 2x))', min_route

    deallocate(d)
    deallocate(min_route)
end program tsp_7loops