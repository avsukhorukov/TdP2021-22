! 1) Run with `-np 2`.
! 2) Swap two lines at send.
! 3) Set tag=MPI_ANY_TAG at receive.
! 4) Set random tags at send.
! 5) Choose ordered tags at receive.
program tree_sends
    use mpi_f08
    implicit none
    integer :: my_rank, n_ranks, src, dst
    type(MPI_Status) :: status
    integer :: a, b, c

    call MPI_Init()
    call MPI_Comm_rank(MPI_COMM_WORLD, my_rank)
    call MPI_Comm_size(MPI_COMM_WORLD, n_ranks)

    if (my_rank == 0) then
        src = 1
        call MPI_Recv(a, 1, MPI_INTEGER, src, 0, MPI_COMM_WORLD, status)
        call MPI_Recv(b, 1, MPI_INTEGER, src, 0, MPI_COMM_WORLD, status)
        call MPI_Recv(c, 1, MPI_INTEGER, src, 0, MPI_COMM_WORLD, status)
        print '(3(i0, 1x))', a, b, c
    else if (my_rank == 1) then
        dst = 0
        call MPI_Send(1, 1, MPI_INTEGER, dst, 0, MPI_COMM_WORLD)
        call MPI_Send(2, 1, MPI_INTEGER, dst, 0, MPI_COMM_WORLD)
        call MPI_Send(3, 1, MPI_INTEGER, dst, 0, MPI_COMM_WORLD)
    end if

    call MPI_Finalize()
end program tree_sends