program type_vector
    use mpi_f08
    implicit none
    integer :: n_ranks, my_rank, first, last
    type(MPI_Comm) :: comm
    type(MPI_Status) :: status
    integer, parameter :: SIDE = 5
    integer :: a(SIDE, SIDE)
    integer :: i
    type(MPI_Datatype) :: a_row
    !type(MPI_Datatype) :: a_col

    call MPI_Init()
    comm = MPI_COMM_WORLD
    call MPI_Comm_size(comm, n_ranks)
    call MPI_Comm_rank(comm, my_rank)

    first = 0
    last  = n_ranks - 1

    call MPI_Type_vector(SIDE, 1, SIDE, MPI_INTEGER, a_row)
    call MPI_Type_commit(a_row)

    !call MPI_Type_contiguous(SIDE, MPI_INTEGER, a_col)
    !call MPI_Type_commit(a_col)

    a(:, :) = 0

    if (my_rank == first) then
        a(:, :) = reshape([(i, i = 1, SIDE * SIDE)], shape=[SIDE, SIDE])
        call MPI_Send(a(1, 1), 1, a_row, last, 0, comm)
    else if (my_rank == last) then
        call MPI_Recv(a(1, 1), 1, a_row, first, 0, comm, status)
    end if

    call serial_print(a, my_rank, n_ranks, comm)

    !call MPI_Type_free(a_col)
    call MPI_Type_free(a_row)
    call MPI_Finalize()

contains

    subroutine serial_print(arr, my_id, n_ids, comm)
        integer,        intent(in) :: arr(:, :), my_id, n_ids
        type(MPI_Comm), intent(in) :: comm

        if (my_id == 0) then
            block
                integer              :: i, rank
                type(MPI_Status)     :: status
                integer, allocatable :: arr_out(:, :)
                allocate(arr_out, mold=arr)
                do rank = 0, n_ids - 1
                    if (rank == 0) then
                        arr_out(:, :) = arr(:, :)
                    else
                        call MPI_Recv(arr_out, size(arr_out), MPI_INTEGER, rank, 9, comm, status)
                    end if
                    print "(a, i0)", "Rank ", rank
                    do i = 1, size(arr_out, dim=1)
                        print "(*(i2, 1x))", arr_out(i, :)
                    end do
                end do
                deallocate(arr_out)
            end block
        else
            call MPI_Send(arr, size(arr), MPI_INTEGER, 0, 9, comm)
        end if
        return
    end subroutine serial_print

end program type_vector
