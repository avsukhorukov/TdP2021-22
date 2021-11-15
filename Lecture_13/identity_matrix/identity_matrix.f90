! Identity matrix distribution
!
! Write a program that distributes an identity matrix over the processes.  The
! dimension and the number of processes are given.  Distribute by rows (in
! Fortran).  The program reads the matrix size S, takes the number of processes
! N, and calculates how many rows each process must have.  Better if you work
! with through indiexing of subarrays at each rank.  At the end, rank 0 collects
! parts of the matrix from the other ranks and prints the entire thing.
!
! Compile and run:
!
!     $ mpifort -O0 -Wall -Wextra -fcheck=bounds -fbacktrace identity_matrix.f90
!     $ mpirun -np 3 --oversubscribe ./a.out
!
! Enter size 10.
program identity_matrix
    use mpi_f08
    implicit none
    type(MPI_Comm) :: comm
    type(MPI_Status) :: status
    integer :: my_rank, n_ranks !, top_rank, bottom_rank
    integer :: ib, ie, i, s, rank
    integer, allocatable :: e(:, :)
    integer(kind=MPI_ADDRESS_KIND) :: lb, integer_extent
    type(MPI_Datatype) :: a_tmp_row, a_row

    call MPI_Init()
    comm = MPI_COMM_WORLD
    call MPI_Comm_rank(comm, my_rank)
    call MPI_Comm_size(comm, n_ranks)

    ! top_rank    = get_rank(my_rank - 1, n_ranks)
    ! bottom_rank = get_rank(my_rank + 1, n_ranks)

    if (my_rank == 0) read *, s
    call MPI_Bcast(s, 1, MPI_INTEGER, 0, comm)

    call partition(my_rank, n_ranks, s, ib, ie)

    allocate(e(ib:ie, s), source=0)
    do i = ib, ie
        e(i, i) = 1
    end do

    ! call print_barrier(e, my_rank, n_ranks, comm)

    call MPI_Type_get_extent(MPI_INTEGER, lb, integer_extent)
    call MPI_Type_vector(s, 1, ie - ib + 1, MPI_INTEGER, a_tmp_row)
    call MPI_Type_create_resized(a_tmp_row, lb, integer_extent, a_row)
    call MPI_Type_commit(a_row)

    if (my_rank == 0) then
        ! Rank 0 prints its own matrix,
        do i = ib, ie
            print "(*(i1, 1x))", e(i, :)
        end do
        print *
        ! then receives rows from the other processes and prints them.
        do rank = 1, n_ranks - 1
            call partition(rank, n_ranks, s, ib, ie)
            call MPI_Recv(e(1, 1), ie - ib + 1, a_row, rank, 0, comm, status)
            do i = 1, ie - ib + 1
                print "(*(i1, 1x))", e(i, :)
            end do
            print *
        end do
    else
        call MPI_Send(e(ib, 1), ie - ib + 1, a_row, 0, 0, comm)
    end if

    ! call MPI_Type_free(a_row)
    deallocate(e)

    call MPI_Finalize()

contains

    subroutine partition(id, n_ids, size, b, e)
        integer, intent(in)    :: id, n_ids, size
        integer, intent(inout) :: b, e
        integer :: remainder, quotient

        remainder = modulo(size, n_ids)
        quotient  = (size - remainder) / n_ids
        b = 1 + quotient * (id    ) + min(remainder, id    )
        e =     quotient * (id + 1) + min(remainder, id + 1)
        return
    end subroutine partition

    ! integer function get_rank(rnk, n_ids)
    !     integer, intent(in) :: rnk, n_ids

    !     if (0 <= rnk .and. rnk < n_ids) then
    !         get_rank = rnk
    !     else
    !         get_rank = MPI_PROC_NULL
    !     end if
    !     return
    ! end function get_rank

    ! subroutine print_barrier(mtx, id, n_ids, comm)
    !     integer,        intent(in) :: mtx(:, :), id, n_ids
    !     type(MPI_Comm), intent(in) :: comm
    !     integer :: rnk, row

    !     do rnk = 0, n_ids - 1
    !         if (rnk == id) then
    !             print "(a, i0)", "Rank ", id
    !             do row = 1, size(mtx, dim=1)
    !                 print "(*(i1, 1x))", mtx(row, :)
    !             end do
    !         end if
    !         call MPI_Barrier(comm)
    !     end do
    !     return
    ! end subroutine print_barrier

end program identity_matrix
