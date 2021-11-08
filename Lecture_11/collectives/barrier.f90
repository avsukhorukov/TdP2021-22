! Rank 0 reads in the array size.  This size is sent to the other ranks.  All
! ranks allocate the same array and set its values to zero.  Rank 0 populates
! this array with some arithmetic progression.
!
! Compile with
!
!   $ mpifort -g -O0 -Wall -Wextra -Wpedantic -fcheck=all -fbacktrace barrier.f90
!
! Run with
!
!   $ mpirun -np 5 --oversubscribe ./a.out
program barrier
    use mpi_f08
    implicit none
    type(MPI_Comm)   :: comm
    type(MPI_Status) :: status
    integer :: my_rank, n_ranks, i, a_size
    integer, allocatable :: a(:)

    call MPI_Init()

    comm = MPI_COMM_WORLD

    call MPI_Comm_size(comm, n_ranks)
    call MPI_Comm_rank(comm, my_rank)

    a_size = 0
    if (my_rank == 0) then
        read *, a_size
        do i = 1, n_ranks - 1
            call MPI_Send(a_size, 1, MPI_INTEGER, i, 0, comm)
        end do
    else
        call MPI_Recv(a_size, 1, MPI_INTEGER, 0, 0, comm, status)
    end if
    allocate(a(a_size), source=0)

    call print_p2p(a, my_rank, n_ranks, comm)

    if (my_rank == 0) then
        a(:) = [ (10 * my_rank + i, i = 1, a_size) ]
    end if

    call print_p2p(a, my_rank, n_ranks, comm)

    if (allocated(a)) deallocate(a)
    call MPI_Finalize()

contains

    !---------------------------------------------------------------------------
    subroutine print_p2p(arr, my_id, n_ids, comm)
        integer,        intent(in) :: arr(:), my_id, n_ids
        type(MPI_Comm), intent(in) :: comm

        if (my_id == 0) then
            block
                integer              :: rank
                type(MPI_Status)     :: status
                integer, allocatable :: arr_out(:)
                allocate(arr_out, mold=arr)
                do rank = 0, n_ids - 1
                    if (rank == 0) then
                        arr_out(:) = arr(:)
                    else
                        call MPI_Recv(arr_out, size(arr_out), MPI_INTEGER, rank, 9, comm, status)
                    end if
                    print "(a, i0, a, *(i2, 1x))", "Rank ", rank, ", a(:)=", arr_out
                end do
                print *
                deallocate(arr_out)
            end block
        else
            call MPI_Send(arr, size(arr), MPI_INTEGER, 0, 9, comm)
        end if
        return
    end subroutine print_p2p
    !---------------------------------------------------------------------------
    subroutine print_barrier(arr, my_id, n_ids, comm)
        integer,        intent(in) :: arr(:), my_id, n_ids
        type(MPI_Comm), intent(in) :: comm
        integer :: rank

        do rank = 0, n_ids - 1
            if (rank == my_id) then
                print "(a, i0, a, *(i2, 1x))", "Rank ", rank, ", a(:)=", arr
            end if
            call MPI_Barrier(comm)
        end do
        if (my_id == 0) print *
        return
    end subroutine print_barrier

end program barrier
