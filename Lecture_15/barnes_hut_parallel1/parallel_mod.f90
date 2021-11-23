! In this solution the array of bodies is virtually separated into chunks.
! Accelerations, velocities, and coordinates are upadted only for this local
! chunk by each process.  At the end, new coordinates a gathered to all ranks
! from different chunks.  This gives the same exact solution as the serial code
! does.
module parallel_mod
    use mpi_f08
    implicit none
    integer, parameter :: ROOT_RANK = 0
    integer :: n_ranks
    integer :: my_rank
    integer :: first ! topological range of bodies
    integer :: last 
    integer :: count ! last - first + 1
    integer :: disp
    integer, dimension(:), allocatable :: counts, disps
    type(MPI_Comm)     :: comm 
    type(MPI_Datatype) :: a_body_datatype   ! entire structure
    type(MPI_Datatype) :: a_body_r_datatype ! only the r-component

contains
    !---------------------------------------------------------------------------
    subroutine init_parallel()
        call MPI_Init()
        comm = MPI_COMM_WORLD
        call MPI_Comm_size(comm, n_ranks)
        call MPI_Comm_rank(comm, my_rank)
        return
    end subroutine init_parallel
    !---------------------------------------------------------------------------
    subroutine finish_parallel()
        if (allocated(counts)) deallocate(counts)
        if (allocated(disps)) deallocate(disps)
        call MPI_Finalize()
        return
    end subroutine finish_parallel
    !---------------------------------------------------------------------------
    subroutine partition(size)
        integer, intent(in)    :: size
        integer :: remainder, quotient

        remainder = modulo(size, n_ranks)
        quotient  = (size - remainder) / n_ranks
        first = 1 + quotient * (my_rank    ) + min(remainder, my_rank    )
        last  =     quotient * (my_rank + 1) + min(remainder, my_rank + 1)
        count = last - first + 1
        allocate(counts(0:n_ranks - 1))
        allocate(disps(0:n_ranks - 1))
        call MPI_Allgather(count,     1, MPI_INTEGER, counts, 1, MPI_INTEGER, comm)
        call MPI_Allgather(first - 1, 1, MPI_INTEGER, disps,  1, MPI_INTEGER, comm)
        return
    end subroutine partition
    !---------------------------------------------------------------------------
end module parallel_mod
