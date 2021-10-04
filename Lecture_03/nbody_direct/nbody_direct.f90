program nbody_direct
    implicit none
    real, parameter :: SOFTENING_SQR = 1.0e+4 * epsilon(0.0)
    real :: current ! current time
    real :: total   ! total time
    real :: step    ! integration time step
    real :: timer   ! cyclic time count to output results
    real :: lap     ! time cycle to output results
    integer :: n    ! number of bodies
    integer :: i, j
    real, allocatable :: m(:), r(:, :), v(:, :), a(:, :)
    real :: r_ij(2), a_i(2), a_ij(2)

    read *, step
    read *, lap
    read *, total
    read *, n
    allocate(m(n), r(2, n), v(2, n), a(2, n))
    do i = 1, n
        read *, m(i), r(:, i), v(:, i)
    end do

    ! Get the unknown accelerations.
    do i = 1, n
        a_i(:) = 0.0
        do j = 1, n
            if (j == i) cycle
            r_ij(:) = r(:, j) - r(:, i)
            a_ij(:) = (m(j) / norm2(r_ij)**3) * r_ij(:)
            a_i(:) = a_i(:) + a_ij(:)
        end do
        a(:, i) = a_i(:)
    end do

    ! Print the number of bodies.
    print *, n

    ! Start iterations.
    current = 0.0
    timer   = lap
    time: do while (current <= total)
        ! If the timer has elapsed, then print the current coordinates and
        ! re-set the timer.
        if (timer <= 0.0) then
            do i = 1, n
                print *, r(:, i)
            end do
            timer = timer + lap
        end if
        ! Half-kick velocities.
        v(:, :) = v(:, :) + (0.5 * step) * a(:, :)
        ! Drift coordinates.
        r(:, :) = r(:, :) + step * v(:, :)
        ! Update accelerations.
        do i = 1, n
            a_i(:) = 0.0
            do j = 1, n
                if (j == i) cycle
                r_ij(:) = r(:, j) - r(:, i)
                a_ij(:) = (m(j) / norm2(r_ij)**3) * r_ij(:)
                a_i(:) = a_i(:) + a_ij(:)
            end do
            a(:, i) = a_i(:)
        end do
        ! Half-kick velocities.
        v(:, :) = v(:, :) + (0.5 * step) * a(:, :)
        current = current + step
        timer   = timer   - step
    end do time

    deallocate(m, r, v, a)
end program nbody_direct