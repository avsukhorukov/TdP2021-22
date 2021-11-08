program test_read_print
    use mpi_f08
    implicit none
    integer :: n_ranks, my_rank
    integer :: i

    call MPI_Init()
    call MPI_Comm_size(MPI_COMM_WORLD, n_ranks)
    call MPI_Comm_rank(MPI_COMM_WORLD, my_rank)

    read *, i
    print *, "my_rank=", my_rank, ", i=", i

    call MPI_Finalize()
end program test_read_print
