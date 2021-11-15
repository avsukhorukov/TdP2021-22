! Matrix distribution
!
! Distribute a global N x N matrix over P processes as evenly as possible so
! that each process has a local portion of this matrix.  Initialize such a
! portion with the process rank.  N is not necessarily divisible by P.  Each
! process sends its first and last columns (in Fortran) to neighbors: first to
! the left rank, last to the right rank.  Each process must allocate extra ghost
! cells with two columns.
!
! Compile and run:
!
!     $ mpifort -g -O0 -Wall -Wextra -Wpedantic -fcheck=all -fbacktrace matrix_distribution.f90
!     $ mpirun -np 3 --oversubscribe ./a.out
!
! Enter size 10.
program matrix_distribution
    use mpi_f08
    implicit none
    type(MPI_Comm) :: comm
    type(MPI_Status) :: status
    type(MPI_Datatype) :: a_col
    integer :: my_rank, n_ranks, left_rank, right_rank, s, jb, je
    integer, allocatable :: matrix(:, :)

    call MPI_Init()
    comm = MPI_COMM_WORLD
    call MPI_Comm_rank(comm, my_rank)
    call MPI_Comm_size(comm, n_ranks)

    left_rank  = get_rank(my_rank - 1, n_ranks)
    right_rank = get_rank(my_rank + 1, n_ranks)

    if (my_rank == 0) read *, s
    call MPI_Bcast(s, 1, MPI_INTEGER, 0, comm)

    call partition(my_rank, n_ranks, s, jb, je)
    allocate(matrix(s, jb - 1 : je + 1), source=my_rank)

    call print_gatherv(matrix, my_rank, n_ranks, comm)

    call MPI_Type_contiguous(s, MPI_INTEGER, a_col)
    call MPI_Type_commit(a_col)

    call MPI_Sendrecv(matrix(1, jb),     1, a_col, left_rank,  0, &
                      matrix(1, je + 1), 1, a_col, right_rank, 0, comm, status)

    call MPI_Sendrecv(matrix(1, je),     1, a_col, right_rank, 1, &
                      matrix(1, jb - 1), 1, a_col, left_rank,  1, comm, status)

    call MPI_Type_free(a_col)

    call print_gatherv(matrix, my_rank, n_ranks, comm)

    deallocate(matrix)
    call MPI_Finalize()

contains

    subroutine print_gatherv(mtx, id, n_ids, comm)
        integer,        intent(in) :: mtx(:, :), id, n_ids
        type(MPI_Comm), intent(in) :: comm
        character(len=:), allocatable :: line, lines
        character(len=12) :: i_fmt ! integer format string
        integer :: line_len, lines_len, i, n_rows, n_cols, n_proc, disp
        integer, parameter :: I_WIDTH = 1 ! "(i2)" descriptor
        integer, allocatable :: sizes(:), disps(:)

        write(unit=i_fmt, fmt="(a, i0, a)") "(*(i", I_WIDTH,", 1x))"

        n_rows = size(mtx, dim=1)
        n_cols = size(mtx, dim=2)
        line_len = (I_WIDTH + 1) * (1 + n_cols)
        lines_len = 0
        call MPI_Reduce(line_len, lines_len, 1, MPI_INTEGER, MPI_SUM, 0, comm)
        allocate(character(len=line_len)  :: line)
        allocate(character(len=lines_len) :: lines)

        disp = 0
        call MPI_Exscan(line_len, disp, 1, MPI_INTEGER, MPI_SUM, comm)

        n_proc = merge(n_ids, 0, id == 0)
        allocate(sizes(n_proc))
        allocate(disps(n_proc))
        call MPI_Gather(line_len, 1, MPI_INTEGER, &
                        sizes,    1, MPI_INTEGER, 0, comm)
        call MPI_Gather(disp,     1, MPI_INTEGER, &
                        disps,    1, MPI_INTEGER, 0, comm)

        do i = 0, n_rows
            if (i == 0) then
                write(line, "(a, i0)") "Rank ", id
            else
                write(line, i_fmt) mtx(i, :)
            end if
            call MPI_Gatherv(line,  line_len,     MPI_CHARACTER, &
                             lines, sizes, disps, MPI_CHARACTER, 0, comm)
            if (id == 0) print "(a)", lines
        end do

        if (allocated(line)) deallocate(line)
        if (allocated(lines)) deallocate(lines)
        if (allocated(sizes)) deallocate(sizes)
        if (allocated(disps)) deallocate(disps)
        return
    end subroutine print_gatherv

    ! subroutine print_barrier(mtx, id, n_ids, comm)
    !     integer,        intent(in) :: mtx(:, :), id, n_ids
    !     type(MPI_Comm), intent(in) :: comm
    !     integer :: rnk, row

    !     do rnk = 0, n_ids - 1
    !         if (rnk == id) then
    !             print "(a, i0)", "Rank ", id
    !             do row = 1, size(mtx, dim=1)
    !                 print "(*(i2, 2x))", mtx(row, :)
    !             end do
    !         end if
    !         call MPI_Barrier(comm)
    !     end do
    !     return
    ! end subroutine print_barrier

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

    integer function get_rank(rnk, n_ids)
        integer, intent(in) :: rnk, n_ids
        get_rank = modulo(rnk, n_ids)
        return
    end function get_rank

end program matrix_distribution
