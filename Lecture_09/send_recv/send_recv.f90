! Try first with 2 processes, then with 3 or more.
program main
    use mpi_f08
    implicit none
    integer :: my_rank, n_ranks, first, last
    integer :: i = 0
    type(MPI_Status) :: status

    call MPI_Init()

    call MPI_Comm_size(MPI_COMM_WORLD, n_ranks)
    call MPI_Comm_rank(MPI_COMM_WORLD, my_rank)
    first = 0
    last  = n_ranks - 1

    print "(2(a, i0))", "Before: rank=", my_rank, ", i=", i

    if (my_rank == first) then
        i = 999
        call MPI_Send(i, 1, MPI_INTEGER, last, 0, MPI_COMM_WORLD)
    else if (my_rank == last) then
        call MPI_Recv(i, 1, MPI_INTEGER, first, 0, MPI_COMM_WORLD, status)
    end if

    print "(2(a, i0))", "After:  rank=", my_rank, ", i=", i

    call MPI_Finalize()
end program main
