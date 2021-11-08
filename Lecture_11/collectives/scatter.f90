! Run with -np 5 or 10
program scatter
    use mpi_f08
    implicit none
    integer :: my_rank, n_ranks, root, s_size, r_size, i
    type(MPI_Comm) :: comm
    integer, allocatable :: rbuf(:), sbuf(:)

    call MPI_Init()
    comm = MPI_COMM_WORLD
    call MPI_Comm_rank(comm, my_rank)
    call MPI_Comm_size(comm, n_ranks)

    root = 0

    ! Allocate the send buffer `sbuf` of size `s_size` only at rank 0 to scatter
    ! it to the other ranks.
    s_size = 0
    if (my_rank == root) read *, s_size
    allocate(sbuf(s_size))

    if (my_rank == root) sbuf(:) = [ (i, i = 1, s_size) ]

    ! Broadcast the array size and create the receive buffer `sbuf` of size
    ! r_size = s / n.  Initialize it to 0 at all ranks.
    call MPI_Bcast(s_size, 1, MPI_INTEGER, root, comm)
    r_size = s_size / n_ranks
    allocate(rbuf(r_size), source=0)

    call print_barrier()

    call MPI_Scatter(sbuf, r_size, MPI_INTEGER, &
                     rbuf, r_size, MPI_INTEGER, root, comm)

    call print_barrier()

    if (allocated(sbuf)) deallocate(sbuf)
    if (allocated(rbuf)) deallocate(rbuf)

    call MPI_Finalize()

contains

    !---------------------------------------------------------------------------
    subroutine print_barrier()
        integer :: r

        if (my_rank == 0) print *
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
    end subroutine print_barrier

end program scatter
