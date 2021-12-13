! Test against the sequence of prime numbers:
!
!   https://oeis.org/A000040
!
! First implementation: just two nested loops: over numbers and over divisors up
! to sqrt(n).
!
! Second implementation: do the sieve of Eratosthenes.
! 1) Create an array of natural numbers from 2 to n to contain primes and zeros.
! 2) Start from multiple p = 1.
! 3) Repeat until p doesn't exceed n.  Increment p.  If primes(p) is not marked, then
! 3) Mark all multiples of p in primes from p^2 to n with a step of p.
! 4) Repeat step 3).
! Generate a sequence of integers from 2 through n.
! Let p be the current prime number.  Start with p = 2.
! Mark the multiples of p in the sequence from 2p, 3p, ..., n (don't mark p itself).
! Find the smallest number in the sequence greater than p that is not marked.
! If there was no such number, stop.
! Otherwise, this is the next prime.  Set p to it and repeat marking again.
! Once done, all unmarked numbers in the sequence are primes.
program sieve
    implicit none
    integer :: n, i, j, root, p
    integer, allocatable :: primes(:)
    logical, allocatable :: is_prime(:)

    print '(a)', "Enter n"
    read *, n

    ! 1st solution.
    !numbers: do i = 2, n
    !    if (2 < i) then
    !        root = int(sqrt(real(i)))
    !        do j = 2, root
    !            if (mod(i, j) == 0) cycle numbers
    !        end do
    !    end if
    !    write(*, '(i0)') i
    !end do numbers

    ! 2nd solution.
    !allocate(primes(2:n))
    !primes = [ (i, i = 2, n) ]

    !root = int(sqrt(real(n)))
    !p = 1
    !do while (p <= root)
    !    p = p + 1
    !    if (primes(p) == 0) cycle
    !    do i = p * p, n, p
    !        primes(i) = 0
    !    end do
    !end do
    !print '(a)', "Primes are"
    !do i = 2, n
    !    if (primes(i) /= 0) print '(i0)', primes(i)
    !end do
    !deallocate(primes)

    ! 3rd solution
    allocate(is_prime(2:n), source=.true.)
    root = int(sqrt(real(n)))
    p = 1
    do while (p <= root)
        p = p + 1
        if (.not.is_prime(p)) cycle
        do i = p * p, n, p
            is_prime(i) = .false.
        end do
    end do
    print "(a)", "Primes are"
    do i = 2, n
        if (is_prime(i)) print "(i0)", i
    end do
    deallocate(is_prime)
end program sieve