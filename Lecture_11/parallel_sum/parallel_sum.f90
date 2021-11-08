! The parallel summation of elements of a 1D array, which has been distributed
! between all ranks as evenly as possible.
!
! Compile and run:
!
! $ mpifort -g -O0 -Wall -Wextra -Wpedantic -fcheck=all -fbacktrace parallel_sum.f90
! $ mpirun -np 4 --oversubscribe ./a.out < in16.txt
! 136
! $ mpirun -np 7 --oversubscribe ./a.out < in17.txt
! 153
!
! 2021-11-08 Note: students get confused if they are asked to broadcast the size
! of a local subarray.  It is better to broadcast the size of the full array and
! let all processes calculate sizes and index ranges locally.
program parallel_sum
    use mpi_f08
    implicit none
    integer :: my_rank, n_ranks, rank, full_size, net_sum, b, e
    integer, allocatable :: arr(:)
    type(MPI_Comm)   :: comm
    type(MPI_Status) :: status

    call MPI_Init()

    ! Once the environment has been established you can assign the
    ! MPI_COMM_WORLD name to a communicator variable to avoid typing it.
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
    call partition_uneven(my_rank, n_ranks, full_size, b, e)
    allocate(arr(b:e))

    ! Read in and distribute the full array.
    if (my_rank == 0) then
        block
            integer, allocatable :: full_arr(:)
            allocate(full_arr(full_size))
            read *, full_arr(:)
            ! Send subarrays to all processes but zero.
            do rank = 1, n_ranks - 1
                call partition_uneven(rank, n_ranks, full_size, b, e)
                call MPI_Send(full_arr(b), e - b + 1, MPI_INTEGER, rank, 0, comm)
            end do
            ! Direct copy at rank 0.
            call partition_uneven(0, n_ranks, full_size, b, e)
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
            full_sum = net_sum ! copy at rank 0
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

    ! For the given rank `id` from the total number of processes `n_ids`
    ! partition an array of `size` elements and find the beginning and ending
    ! indices `b` and `e`.  This version only works when the partition is even,
    ! that is, the remainder is zero.
    subroutine partition_even(id, n_ids, size, b, e)
        integer, intent(in)    :: id, n_ids, size
        integer, intent(inout) :: b, e
        integer :: q

        q = size / n_ids ! quotient
        b = 1 + q * (id    )
        e =     q * (id + 1)
        return
    end subroutine partition_even

    ! This version works for uneven partitions (the general case).
    subroutine partition_uneven(id, n_ids, size, b, e)
        integer, intent(in)    :: id, n_ids, size
        integer, intent(inout) :: b, e
        integer :: r, q

        r = modulo(size, n_ids) ! remainder
        q = (size - r) / n_ids  ! quotient
        b = 1 + q * (id    ) + min(r, id    )
        e =     q * (id + 1) + min(r, id + 1)
        return
    end subroutine partition_uneven


end program parallel_sum