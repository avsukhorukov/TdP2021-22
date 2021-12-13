! Parallel sum of a 1D array
!
! Write a program to compute the sum of a 1D array.
! 1) Rank 0 read the size of a 1D array, it allocates the array and reads it in.
!    The array size must be divisible by the number of processes.
! 2) Rank 0 finds the number of array elements each process must receive, and
!    sends this number to every other rank as well as the data.
! 3) Each rank receives the data, stores it in a temporary array and calculates
!    the partial sum, then sends it back to rank 0.
! 4) Rank 0 calculates the total sum and prints the result.
!
! Compile and run:
! $ mpifort -g -O0 -Wall -Wextra -Wpedantic -fcheck=all -fbacktrace \
!   parallel_sum_uneven.f90
! $ mpirun -np 5 --oversubscribe ./a.out < in17.txt
! 153
!
! This is a general version when s is not evenly divisible by n.
program parallel_sum_uneven
    use mpi_f08
    implicit none
    integer :: my_rank, n_ranks, rank
    integer :: full_size, net_sum, b, e
    integer, allocatable :: arr(:)
    type(MPI_Comm)   :: comm
    type(MPI_Status) :: status

    call MPI_Init()

    ! Once the environment has been established you can assign the
    ! MPI_COMM_WORLD name to a communicator variable to avoid typing this long
    ! name.
    comm = MPI_COMM_WORLD

    call MPI_Comm_rank(comm, my_rank)
    call MPI_Comm_size(comm, n_ranks)

    ! Read in and broadcast the array size to other ranks.  In a more general
    ! case, when the array cannot be divided evenly between all the ranks, it
    ! is better to send the full size to them all so as each of them can
    ! compute the local size and the beginning index of the subarray.
    if (my_rank == 0) then
        read *, full_size
        do rank = 1, n_ranks - 1
            call MPI_Send(full_size, 1, MPI_INTEGER, rank, 0, comm)
        end do
    else
        call MPI_Recv(full_size, 1, MPI_INTEGER, 0, 0, comm, status)
    end if

    ! Create local sub-arrays with through indexing.
    call partition(my_rank, n_ranks, full_size, b, e)
    allocate(arr(b:e))

    ! Read in and distribute the full array.
    if (my_rank == 0) then
        block
            integer, allocatable :: full_arr(:)
            allocate(full_arr(full_size))
            read *, full_arr(:)
            ! Send all chunks but first to the other ranks.
            do rank = 1, n_ranks - 1
                call partition(rank, n_ranks, full_size, b, e)
                call MPI_Send(full_arr(b), e - b + 1, MPI_INTEGER, rank, 0, comm)
            end do
            ! Copy the first chunk directly.
            call partition(0, n_ranks, full_size, b, e)
            arr(b:e) = full_arr(b:e)
            deallocate(full_arr)
        end block
    else
        call MPI_Recv(arr(b), e - b + 1, MPI_INTEGER, 0, 0, comm, status)
    end if

    ! Calculate local sums.
    net_sum = sum(arr(:))
    deallocate(arr)

    ! Add up sums at rank 0 and print the result.
    if (my_rank == 0) then
        block
            integer :: full_sum
            full_sum = net_sum ! copy from rank 0
            do rank = 1, n_ranks - 1
                call MPI_Recv(net_sum, 1, MPI_INTEGER, rank, 0, comm, status)
                full_sum = full_sum + net_sum
            end do
            print "(i0)", full_sum
        end block
    else
        call MPI_Send(net_sum, 1, MPI_INTEGER, 0, 0, comm)
    end if

    call MPI_Finalize()

contains

    subroutine partition(id, n_ids, size, b, e)
        integer, intent(in)    :: id, n_ids, size
        integer, intent(inout) :: b, e
        integer :: r, q

        r = modulo(size, n_ids) ! remainder
        q = (size - r) / n_ids  ! quotient
        b = 1 + q * (id    ) + min(r, id    )
        e =     q * (id + 1) + min(r, id + 1)
        return
    end subroutine partition

end program parallel_sum_uneven