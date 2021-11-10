! Run with -np 4 and s = 17, better use MPI_SUM.
program scan
    use mpi_f08
    implicit none
    type(MPI_Comm) :: comm
    integer :: my_rank, n_ranks, full_s, remainder, quotient, size, disp, b, e

    call MPI_Init()
    comm = MPI_COMM_WORLD
    call MPI_Comm_size(comm, n_ranks)
    call MPI_Comm_rank(comm, my_rank)

    if (my_rank == 0) read *, full_s
    call MPI_Bcast(full_s, 1, MPI_INTEGER, 0, comm)

    remainder = modulo(full_s, n_ranks)
    quotient  = (full_s - remainder) / n_ranks

    size = quotient + merge(1, 0, my_rank < remainder)
    disp = 0

    call MPI_Exscan(size, disp, 1, MPI_INTEGER, MPI_SUM, comm)
    b = disp + 1
    call MPI_Scan(size, e, 1, MPI_INTEGER, MPI_SUM, comm)

    call print_vals()

    call MPI_Finalize()

contains

    subroutine print_vals()
        integer :: rnk

        do rnk = 0, n_ranks - 1
            if (rnk == my_rank) then
                print '(3(a, i0))', "Rank ", rnk, ", b:e=", b, ":", e
            end if
            call MPI_Barrier(comm)
        end do
        if (my_rank == 0) print *
    end subroutine print_vals

end program scan
