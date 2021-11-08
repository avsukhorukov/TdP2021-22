! Run with -np 5 or 10
program gather
    use mpi_f08
    implicit none
    integer :: my_rank, n_ranks, root, s, i, count
    type(MPI_Comm) :: comm
    integer, allocatable :: rbuf(:), sbuf(:)

    call MPI_Init()
    comm = MPI_COMM_WORLD
    call MPI_Comm_size(comm, n_ranks)
    call MPI_Comm_rank(comm, my_rank)

    root = 0

    ! Read size `s` in on rank 0 and create rbuf(s) on root, and rbuf(0) on
    ! other ranks.
    s = 0
    if (my_rank == root) read *, s
    allocate(rbuf(s), source=0)

    ! Broadcast `s` to create sbuf(s / n) on each rank.
    call MPI_Bcast(s, 1, MPI_INTEGER, 0, comm)

    count = s / n_ranks
    allocate(sbuf(count))
    sbuf(:) = [ (10 * my_rank + i, i = 1, count) ]

    call print_barrier()

    call MPI_Gather(sbuf, count, MPI_INTEGER, &
                    rbuf, count, MPI_INTEGER, root, comm)

    call print_barrier()

    if (allocated(sbuf)) deallocate(sbuf)
    if (allocated(rbuf)) deallocate(rbuf)

    call MPI_Finalize()
contains

    !---------------------------------------------------------------------------
    subroutine print_barrier()
        implicit none
        integer :: r

        do r = 0, n_ranks - 1
            if (r == my_rank) then
                print '(a, i0, a, *(i2, 1x))', "Rank ", r, ", sbuf(:)=", sbuf
            end if
            call MPI_Barrier(comm)
        end do
        do r = 0, n_ranks - 1
            if (r == my_rank) then
                print '(a, i0, a, *(i2, 1x))', "Rank ", r, ", rbuf(:)=", rbuf
            end if
            call MPI_Barrier(comm)
        end do
        if (my_rank == 0) print *
    end subroutine print_barrier

end program gather
