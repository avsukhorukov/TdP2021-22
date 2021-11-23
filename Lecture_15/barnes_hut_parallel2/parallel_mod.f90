! In this solution the tree is partitioned into four parts at the first level.
module parallel_mod
    use :: mpi_f08
    implicit none
    integer, parameter :: ROOT_RANK = 0
    integer :: n_ranks
    integer :: my_rank
    integer :: n_rows = 2
    integer :: n_cols = 2
    integer, dimension(2) :: topology ! [row, col]
    type(MPI_Comm) :: comm
    type(MPI_Datatype) :: a_body_datatype ! entire body structure
    type(MPI_Op) :: sum_body_a_op
contains
    !---------------------------------------------------------------------------
    subroutine init_parallel()
        call MPI_Init()
        comm = MPI_COMM_WORLD
        call MPI_Comm_size(comm, n_ranks)
        call MPI_Comm_rank(comm, my_rank)
        call create_topology()
        return
    end subroutine init_parallel
    !---------------------------------------------------------------------------
    subroutine finish_parallel()
        call MPI_Finalize()
        return
    end subroutine finish_parallel
    !---------------------------------------------------------------------------
    ! Create a 2x2 2D Cartesian topology.  The order of blocks follows the
    ! Fortran order of array elements: the topological row is the fastest along
    ! the ranks.
    subroutine create_topology()
        integer :: tmp, r, c

        if (n_rows * n_cols /= n_ranks) then
            if (my_rank == ROOT_RANK) print "(a)", "Run with `-np 4` only."
            call MPI_Abort(comm, MPI_ERR_TOPOLOGY)
        end if
        tmp = my_rank               ! = my_rank = r + c * n_rows
        r   = modulo(tmp, n_rows)
        tmp = (tmp - r) / n_rows    ! tmp = c
        c   = modulo(tmp, n_cols)
        topology = [r, c]
        return
    end subroutine create_topology
    !---------------------------------------------------------------------------
end module parallel_mod