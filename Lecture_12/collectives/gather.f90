! Run with -np 5 or 10
program gather
    use mpi_f08
    implicit none
    integer :: my_rank, n_ranks, root, r_size, s_size, i
    type(MPI_Comm) :: comm
    integer, allocatable :: rbuf(:), sbuf(:)

    call MPI_Init()
    comm = MPI_COMM_WORLD
    call MPI_Comm_size(comm, n_ranks)
    call MPI_Comm_rank(comm, my_rank)

    root = 0

    ! Read in size `r_size` at rank 0 and create rbuf(r_size) at root, and
    ! rbuf(0) at the other ranks.
    r_size = 0
    if (my_rank == root) read *, r_size
    allocate(rbuf(r_size), source=0)

    ! Broadcast `r_size` to create sbuf(r_size / n_ranks) at each rank.
    call MPI_Bcast(r_size, 1, MPI_INTEGER, 0, comm)

    s_size = r_size / n_ranks
    allocate(sbuf(s_size))
    sbuf(:) = [ (10 * my_rank + i, i = 1, s_size) ]

    call print_barrier()

    call MPI_Gather(sbuf, s_size, MPI_INTEGER, &
                    rbuf, s_size, MPI_INTEGER, root, comm)

    call print_barrier()

    if (allocated(sbuf)) deallocate(sbuf)
    if (allocated(rbuf)) deallocate(rbuf)

    call MPI_Finalize()

contains

    !---------------------------------------------------------------------------
    subroutine print_barrier()
        implicit none
        integer :: rank

        do rank = 0, n_ranks - 1
            if (rank == my_rank) then
                print "(a, i0, a, *(i2, 1x))", "Rank ", rank, ", sbuf(:)=", sbuf
            end if
            call MPI_Barrier(comm)
        end do
        do rank = 0, n_ranks - 1
            if (rank == my_rank) then
                print "(a, i0, a, *(i2, 1x))", "Rank ", rank, ", rbuf(:)=", rbuf
            end if
            call MPI_Barrier(comm)
        end do
        if (my_rank == 0) print *
    end subroutine print_barrier
    !---------------------------------------------------------------------------

end program gather
