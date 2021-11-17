! Heat transport in 2D.
!
! Parallel version of the serial code.  Run it with `-np 4`.
!
!     $ mpifort -g -O0 -Wall -Wextra -Wpedantic -fcheck=all -fbacktrace heat_parallel.f90
!     $ mpirun -np 4 --oversubscribe ./a.out < in.txt
program heat_parallel
    implicit none
    real, parameter :: alpha = 2.5e-4
    real, dimension(:, :), pointer :: prev_t, next_t, temp_t
    real :: diff_t
    integer :: s, i, time

    read *, s
    allocate(prev_t(0:s + 1, 0:s + 1))
    do i = 0, s + 1
        read *, prev_t(i, 0:s + 1)
    end do
    allocate(next_t(0:s + 1, 0:s + 1))
    next_t(:, :) = prev_t(:, :) ! copy boundaries

    time = 0
    do
        next_t(1:s, 1:s) = &
            (1.0 - 4.0 * alpha) *     prev_t(1:s,     1:s)     &
                       + alpha  * (   prev_t(0:s - 1, 1:s    ) &
                                    + prev_t(2:s + 1, 1:s    ) &
                                    + prev_t(1:s    , 0:s - 1) &
                                    + prev_t(1:s    , 2:s + 1) )
        diff_t = maxval(abs(next_t(1:s, 1:s) - prev_t(1:s, 1:s)))
        if (modulo(time, 10000) == 0) then
            print "(a, i0, a, es12.5)", "time = ", time, ", diff_t = ", diff_t
            do i = 0, s + 1
                print "(*(f4.0, 1x))", prev_t(i, 0:s + 1)
            end do
            call sleep(1)
        end if
        if (diff_t < epsilon(0.0)) exit
        temp_t => prev_t; prev_t => next_t; next_t => temp_t
        time = time + 1
    end do
    print "(a, i0)", "Final time is ", time
    deallocate(prev_t)
    deallocate(next_t)
end program heat_parallel
