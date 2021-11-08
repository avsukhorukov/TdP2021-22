! Rank 0 reads in the array size.  This size is sent to the other ranks.  Later
! change this to a broadcast.  All ranks allocate the same array and set its
! values to zero.  Rank 0 populates this array with some arithmetic progression.
! This array is broacast to all ranks.
!
! Compile with
!
!   $ mpifort -g -O0 -Wall -Wextra -Wpedantic -fcheck=all -fbacktrace broadcast.f90
!
! Run with
!
!   $ mpirun -np 5 --oversubscribe ./a.out
program broadcast
    use mpi_f08
    implicit none
    integer :: my_rank, n_ranks, root, a_size, i
    type(MPI_Comm) :: comm
    integer, allocatable :: a(:)

    call MPI_Init()
    comm = MPI_COMM_WORLD
    call MPI_Comm_size(comm, n_ranks)
    call MPI_Comm_rank(comm, my_rank)

    root = 0

    ! Size s must be zero at ranks but 0, where it is read from the terminal.
    a_size = 0
    !call print_barrier()

    if (my_rank == root) read *, a_size
    !call print_barrier()

    call MPI_Bcast(a_size, 1, MPI_INTEGER, root, comm)
    !call print_barrier()

    allocate(a(a_size), source=0)
    if (my_rank == root) then
        a(:) = [ (i, i = 1, a_size) ]
    end if
    call print_barrier()

    call MPI_Bcast(a, a_size, MPI_INTEGER, root, comm) ! try a(2), count=2 and so on.
    call print_barrier()

    if (allocated(a)) deallocate(a)
    call MPI_Finalize()

contains

    !---------------------------------------------------------------------------
    subroutine print_barrier()
        integer :: r

        do r = 0, n_ranks - 1
            if (r == my_rank) then
                ! 1st: print `a_size` only
                !print '(a, i0, a, i0)', "Rank ", r, ", a_size=", a_size
                ! 2nd: print `a(:)` only.
                print '(a, i0, a, *(i2, 1x))', "Rank ", r, ", a(:)=", a
            end if
            call MPI_Barrier(comm)
        end do
        if (my_rank == 0) print *
    end subroutine print_barrier

end program broadcast
