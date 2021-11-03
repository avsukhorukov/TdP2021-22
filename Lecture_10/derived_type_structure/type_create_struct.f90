program type_create_struct
    use mpi_f08
    implicit none
    integer :: n_ranks, my_rank, first, last
    type(MPI_Comm) :: comm
    type(MPI_Status) :: status
    integer(kind=MPI_ADDRESS_KIND) :: addrs(3)
    integer          :: i = 0
    real             :: x = 0.0
    character(len=6) :: name = ''
    type(MPI_Datatype) :: a_struct

    call MPI_Init()

    comm = MPI_COMM_WORLD

    call MPI_Comm_size(comm, n_ranks)
    call MPI_Comm_rank(comm, my_rank)

    call MPI_Get_address(i,    addrs(1))
    call MPI_Get_address(x,    addrs(2))
    call MPI_Get_address(name, addrs(3))
    addrs(:) = addrs(:) - addrs(1)
    call MPI_Type_create_struct(3, [1, 1, len(name)], addrs, &
                                [MPI_INTEGER, MPI_REAL, MPI_CHARACTER], a_struct)
    call MPI_Type_commit(a_struct)

    first = 0
    last  = n_ranks - 1

    if (my_rank == first) then
        i    = 5
        x    = -1.0
        name = 'test'
        call MPI_Send(i, 1, a_struct, last, 0, comm)
    else if (my_rank == last) then
        print "(2(a, i0), a, f5.1, 3a)", &
            "Rank ", my_rank, ": i=", i, ", x=", x, ", name='", name, "'."

        call MPI_Recv(i, 1, a_struct, first, 0, comm, status)

        print "(2(a, i0), a, f5.1, 3a)", &
            "Rank ", my_rank, ": i=", i, ", x=", x, ", name='", name, "'."
    end if

    call MPI_Type_free(a_struct)
    call MPI_Finalize()
end program type_create_struct
