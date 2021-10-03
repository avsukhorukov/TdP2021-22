! This exercise is based on Exercise 9 on p. 66 from Chirila & Lohmann (2015).
!
! Try with two different compiler set-ups, with no optimization
!
!     $ gfortran -g -O0 -fcheck=all -fbacktrace time_array_add.f90
!
! and with full optimization
!
!     $ gfortran -O3 -ffast-math -funroll-loops -mtune=native time_array_add.f90
!
! Don't show correct randomization of arrays, setting the seed is technical and
! hard to explain to students, better skip it.
!
! Might be useful to change the declaration of arrays to a static form to show
! that compiler complains about the default stack size not being enough to
! accomodate them so big.
program time_array_add
    implicit none
    integer, parameter :: N_REPS = 8
    integer, parameter :: N = 256
    real, dimension(:, :, :), allocatable :: a, b
    integer :: i, j, k, r
    real :: t1, t2
    !integer :: seed_size
    !integer, allocatable :: seed(:)

    allocate(a(N, N, N))
    allocate(b(N, N, N))

    !call random_seed(size=seed_size)
    !allocate(seed(seed_size), source=0)
    !call random_seed(put=seed)
    !deallocate(seed)
    call random_number(a)
    call random_number(b)

    call cpu_time(t1)
    kji: do r = 1, N_REPS
        do i = 1, N
            do j = 1, N
                do k = 1, N
                    a(i, j, k) = a(i, j, k) + b(i, j, k)
                end do ! k
            end do ! j
        end do ! i
    end do kji
    call cpu_time(t2)
    print "(a, f6.3, a)", "Order k-j-i took ", (t2 - t1) / N_REPS, " s."

    call cpu_time(t1)
    ijk: do r = 1, N_REPS
        do k = 1, N
            do j = 1, N
                do i = 1, N
                    a(i, j, k) = a(i, j, k) + b(i, j, k)
                end do ! i
            end do ! j
        end do ! k
    end do ijk
    call cpu_time(t2)
    print "(a, f6.3, a)", "Order i-j-k took ", (t2 - t1) / N_REPS, " s."

    call cpu_time(t1)
    vector: do r = 1, N_REPS
        a(:, :, :) = a(:, :, :) + b(:, :, :) ! Equivalent to `a = a + b`.
    end do vector
    call cpu_time(t2)
    print '(a, f6.3, a)', "Vector sum took  ", (t2 - t1) / N_REPS, " s."

    deallocate(a, b)
end program time_array_add