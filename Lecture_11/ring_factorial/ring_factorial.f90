! Factorial, calculated in a 1D ring topology.
!
! 1) Rank 0 reads in a number, which must be 1.
! 2) Each rank multiplies this number by my_rank + 1 and sends it to the next
!    neighbor.
! 3) The last rank does the same and sends this number to rank 0, where it is
!    received and printed out.
!
! Compile and run:
!
!   $ mpifort -O0 -Wall -Wextra -Wpedantic -fcheck=all -fbacktrace ring_factorial.f90
!   $ mpirun -np 6 --oversubscribe ./a.out
!   720
program ring_factorial
    use mpi_f08
    implicit none
    type(MPI_Comm)   :: comm
    type(MPI_Status) :: status
    integer :: my_rank, n_ranks, prev, next, f

    call MPI_Init()
    comm = MPI_COMM_WORLD
    call MPI_Comm_rank(comm, my_rank)
    call MPI_Comm_size(comm, n_ranks)

    ! Create a 1D ring topology.
    prev = get_rank(my_rank - 1, n_ranks)
    next = get_rank(my_rank + 1, n_ranks)

    ! Read in the number `f` at rank 0.  Other ranks are waiting to receive.
    if (my_rank == 0) then
        read *, f
    else
        call MPI_Recv(f, 1, MPI_INTEGER, prev, 0, comm, status)
    end if

    ! All ranks multiply the number.
    f = f * (my_rank + 1)

    ! Send the result to the next rank in the ring.
    call MPI_Send(f, 1, MPI_INTEGER, next, 0, comm)

    ! Now rank 0 receives the result and prints it out.
    if (my_rank == 0) then
        call MPI_Recv(f, 1, MPI_INTEGER, prev, 0, comm, status)
        print "(i0)", f
    end if

    ! Another, shorter solution is possible:
    !if (my_rank == 0) then
    !    read *, f
    !    f = f * (my_rank + 1)
    !    call MPI_Send(f, 1, MPI_INTEGER, next, 0, comm)
    !    call MPI_Recv(f, 1, MPI_INTEGER, prev, 0, comm, status)
    !    print "(i0)", f
    !else
    !    call MPI_Recv(f, 1, MPI_INTEGER, prev, 0, comm, status)
    !    f = f * (my_rank + 1)
    !    call MPI_Send(f, 1, MPI_INTEGER, next, 0, comm)
    !end if

    call MPI_Finalize()

contains

    ! Use the modulo division to find the corresponding rank in a 1D ring
    ! topology of `n` processes.
    integer function get_rank(rnk, n)
        integer, intent(in) :: rnk, n
        get_rank = modulo(rnk, n)
        return
    end function get_rank

end program ring_factorial
