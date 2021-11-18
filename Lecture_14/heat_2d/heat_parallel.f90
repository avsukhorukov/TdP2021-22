! Heat transport in 2D.
!
! A parallel version of the same serial code.  Run it with `-np 4`.  Compile and
! run:
!
!   $ mpifort -g -O0 -Wall -Wextra -Wpedantic -fcheck=all -fbacktrace heat_parallel.f90
!   $ mpirun -np 4 --oversubscribe ./a.out < in.txt
program heat_parallel
    use mpi_f08
    implicit none
    real, parameter :: alpha = 2.5e-4
    real, dimension(:, :), pointer :: prev_t, next_t, temp_t
    real :: diff_t, global_diff_t
    type(MPI_Comm) :: comm
    type(MPI_Status) :: status
    integer(kind=MPI_ADDRESS_KIND) :: lb, real_extent
    type(MPI_Datatype) :: a_col, a_row, a_tmp_row
    integer :: n_ranks, my_rank, root, north_rank, south_rank, west_rank, east_rank
    integer :: n_rows, n_cols, row, col, ib, ie, jb, je, rb, re, cb, ce
    integer :: s, i, time

    call MPI_Init()
    comm = MPI_COMM_WORLD
    call MPI_Comm_size(comm, n_ranks)
    call MPI_Comm_rank(comm, my_rank)

    root = 0

    ! Create a 2D topology.
    n_rows = 2
    n_cols = 2
    if (n_rows * n_cols /= n_ranks) then
        print "(a)", "Run this code with `-np 4`."
        call MPI_Abort(comm, MPI_ERR_TOPOLOGY)
    end if
    call get_coords(my_rank, n_rows, n_cols, row, col)

    ! Define neighbor ranks with non-periodic boundaries.
    north_rank = get_rank(row - 1, col,     n_rows, n_cols)
    south_rank = get_rank(row + 1, col,     n_rows, n_cols)
    west_rank  = get_rank(row,     col - 1, n_rows, n_cols)
    east_rank  = get_rank(row,     col + 1, n_rows, n_cols)

    ! Read in the array size and distribute it between processes.
    if (my_rank == root) read *, s
    call MPI_Bcast(s, 1, MPI_INTEGER, root, comm)

    ! Partition the global 2D array into a Cartisian product of two 1D array
    ! partitions for each of the two dimensions.
    call partition(row, n_rows, s, ib, ie)
    call partition(col, n_cols, s, jb, je)

    ! Create sub-arrays for the temperature grid.  Include two outer cells on
    ! both sides for ghost columns and rows.
    allocate(prev_t(ib - 1:ie + 1, jb - 1:je + 1))
    allocate(next_t(ib - 1:ie + 1, jb - 1:je + 1))

    ! One column covers the internal range, [ib:ie].
    call MPI_Type_contiguous(ie - ib + 1, MPI_REAL, a_col)
    call MPI_Type_commit(a_col)

    ! One row covers the internal range and two outer ghost cells, [jb-1:je+1].
    call MPI_Type_get_extent(MPI_REAL, lb, real_extent)
    call MPI_Type_vector(je - jb + 3, 1, ie - ib + 3, MPI_REAL, a_tmp_row)
    call MPI_Type_create_resized(a_tmp_row, lb, real_extent, a_row)
    call MPI_Type_commit(a_row)

    ! Read in the `prev_t` array at root and distribute it to other processes.
    call read_array()

    ! Synchronize rows.
    call MPI_Sendrecv(prev_t(ib,     jb - 1), 1, a_row, north_rank, 3, &
                      prev_t(ie + 1, jb - 1), 1, a_row, south_rank, 3, comm, status)
    call MPI_Sendrecv(prev_t(ie,     jb - 1), 1, a_row, south_rank, 4, &
                      prev_t(ib - 1, jb - 1), 1, a_row, north_rank, 4, comm, status)

    ! Copy boundary conditions to the next array.
    next_t(:, :) = prev_t(:, :)

    time = 0
    do
        next_t(ib:ie, jb:je) = &
            (1.0 - 4.0 * alpha) *     prev_t(ib    :ie,     jb    :je    ) &
                       + alpha  * (   prev_t(ib - 1:ie - 1, jb    :je    ) &
                                    + prev_t(ib + 1:ie + 1, jb    :je    ) &
                                    + prev_t(ib    :ie    , jb - 1:je - 1) &
                                    + prev_t(ib    :ie    , jb + 1:je + 1) )

        ! Synchronize columns.
        call MPI_Sendrecv(next_t(ib, jb    ), 1, a_col, west_rank, 1, &
                          next_t(ib, je + 1), 1, a_col, east_rank, 1, comm, status)
        call MPI_Sendrecv(next_t(ib, je    ), 1, a_col, east_rank, 2, &
                          next_t(ib, jb - 1), 1, a_col, west_rank, 2, comm, status)

        ! Synchronize rows.
        call MPI_Sendrecv(next_t(ib,     jb - 1), 1, a_row, north_rank, 3, &
                          next_t(ie + 1, jb - 1), 1, a_row, south_rank, 3, comm, status)
        call MPI_Sendrecv(next_t(ie,     jb - 1), 1, a_row, south_rank, 4, &
                          next_t(ib - 1, jb - 1), 1, a_row, north_rank, 4, comm, status)

        diff_t = maxval(abs(next_t(ib:ie, jb:je) - prev_t(ib:ie, jb:je)))
        call MPI_Allreduce(diff_t, global_diff_t, 1, MPI_REAL, MPI_MAX, comm)

        if (modulo(time, 10000) == 0) then
            if (my_rank == root) then
                print "(a, i0, a, es12.5)", "time = ", time, ", diff_t = ", global_diff_t
            end if
            ! Print the `prev_t` array by sending it to root.
            call print_array()
        end if
        if (global_diff_t < epsilon(0.0)) exit
        temp_t => prev_t; prev_t => next_t; next_t => temp_t
        time = time + 1
    end do
    if (my_rank == root) print "(a, i0)", "Final time is ", time

    deallocate(prev_t)
    deallocate(next_t)

    call MPI_Type_free(a_col)
    call MPI_Type_free(a_row)

    call MPI_Finalize()

contains

    !---------------------------------------------------------------------------
    ! Direct procedure: get the process rank from its coordinates in a 2D
    ! Cartesian topology.  The row and column numbers can be oustide the range
    ! of definition.  The Cartesian topology is column-major as in Fortran.  The
    ! boundaries are not periodic to avoid communicating utmost ghost cells to
    ! neighbors to keep the boundary conditions constant.
    integer function get_rank(row, col, n_rows, n_cols)
        integer, intent(in) :: row, col, n_rows, n_cols

        if (      0 <= col .and. col < n_cols &
            .and. 0 <= row .and. row < n_rows) then
                get_rank = row + col * n_rows
        else
            get_rank = MPI_PROC_NULL
        end if
        return
    end function get_rank
    !---------------------------------------------------------------------------
    ! Inverse procedure: get the row and column coordinates in a 2D Cartesian
    ! topology for the given process rank.  The process rank may be outside the
    ! range of definition but should preferably be not.  The Cartesian topology
    ! is column-major as in Fortran.
    subroutine get_coords(rank, n_rows, n_cols, row, col)
        integer, intent(in)    :: rank, n_rows, n_cols
        integer, intent(inout) :: row, col
        
        row = modulo(rank, n_rows)
        col = (rank - row) / n_rows
        if (0 <= col .and. col < n_cols) then
            return
        else
            print "(a, 2(i0, a))", "get_coords: rank ", rank, &
                " is outside the column range [0, ", n_cols, ")."
            call MPI_Abort(comm, MPI_ERR_TOPOLOGY)
        end if
    end subroutine get_coords
    !---------------------------------------------------------------------------
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
    !---------------------------------------------------------------------------
    ! Read in the global array and distribute to other processes row-by-row.
    subroutine read_array()
        if (my_rank == root) then
            block
                real, dimension(:), allocatable :: t_row
                integer :: dst
                allocate(t_row(0:s + 1))
                do row = 0, n_rows - 1
                    call partition(row, n_rows, s, rb, re)
                    rb = merge(rb - 1, rb, row == 0)
                    re = merge(re + 1, re, row == n_rows - 1)
                    do i = rb, re
                        read *, t_row(:)
                        do col = 0, n_cols - 1
                            call partition(col, n_cols, s, cb, ce)
                            dst = get_rank(row, col, n_rows, n_cols)
                            if (dst == root) then
                                prev_t(i, cb - 1:ce + 1) = t_row(cb - 1:ce + 1)
                            else
                                call MPI_Send(t_row(cb - 1), ce - cb + 3, MPI_REAL, dst, 0, comm)
                            end if
                        end do
                    end do
                end do
                deallocate(t_row)
            end block
        else
            rb = merge(ib - 1, ib, row == 0)
            re = merge(ie + 1, ie, row == n_rows - 1)
            do i = rb, re
                call MPI_Recv(prev_t(i, jb - 1), 1, a_row, root, 0, comm, status)
            end do
        end if
        return
    end subroutine read_array
    !---------------------------------------------------------------------------
    ! Homework:
    subroutine print_array()
        if (my_rank == root) then
            continue
        else
            continue
        end if
        return
    end subroutine print_array
    !---------------------------------------------------------------------------
end program heat_parallel
