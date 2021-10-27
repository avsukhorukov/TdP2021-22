! Run with `-np 2` and then with `-np 3` or more.
!
! Demonstrate first what happens if you just use first and last: rank 0
! deadlocks, draw on the blackboard the diagram, as rank 1 sends to itself, but
! rank 0 tries to recieve from itself.
!
program deadlock
    use mpi_f08
    implicit none
    integer, parameter :: N = 5
    integer :: my_rank, n_ranks, prev, next, tag
    integer, allocatable :: a(:), b(:)
    type(MPI_Status) :: status

    call MPI_Init()
    call MPI_Comm_rank(MPI_COMM_WORLD, my_rank)
    call MPI_Comm_size(MPI_COMM_WORLD, n_ranks)

    prev = modulo(my_rank - 1, n_ranks)
    next = modulo(my_rank + 1, n_ranks)

    allocate(a(N), source=my_rank)
    allocate(b(N), source=-1)

    tag = 0

    print "(a, i0, a, *(1x, i2))", "before at rank=", my_rank, ", a(:)=", a(:)
    print "(a, i0, a, *(1x, i2))", "before at rank=", my_rank, ", b(:)=", b(:)

    call MPI_Sendrecv(                                                          &
        sendbuf=a, sendcount=N, sendtype=MPI_INTEGER, dest=next,   sendtag=tag, &
        recvbuf=b, recvcount=N, recvtype=MPI_INTEGER, source=prev, recvtag=tag, &
        comm=MPI_COMM_WORLD, status=status                                      )

    print "(a, i0, a, *(1x, i2))", "after  at rank=", my_rank, ", a(:)=", a(:)
    print "(a, i0, a, *(1x, i2))", "after  at rank=", my_rank, ", b(:)=", b(:)

    deallocate(a)
    deallocate(b)

    call MPI_Finalize()
end program deadlock