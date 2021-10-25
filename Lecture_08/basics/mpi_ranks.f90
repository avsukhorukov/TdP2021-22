program mpi_ranks
    use mpi_f08 ! or mpi
    implicit none
    integer :: n_ranks, my_rank
    ! integer :: err

    call MPI_Init() !, ierror=err)

    call MPI_Comm_size(comm=MPI_COMM_WORLD, size=n_ranks) !, ierror=err)
    call MPI_Comm_rank(comm=MPI_COMM_WORLD, rank=my_rank) !, ierror=err)

    print '(2(a, i0))', "Rank ", my_rank, " of ", n_ranks

    call MPI_Finalize() !, ierror=err)
end program mpi_ranks