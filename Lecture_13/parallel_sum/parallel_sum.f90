! A parallel summation of elements of a 1D array, which has been distributed
! between all ranks as evenly as possible.
!
! Compile and run:
!
! $ mpifort -g -O0 -Wall -Wextra -Wpedantic -fcheck=all -fbacktrace parallel_sum.f90
! $ mpirun -np 4 --oversubscribe ./a.out < in16.txt
! 136
! $ mpirun -np 7 --oversubscribe ./a.out < in17.txt
! 153
program parallel_sum
    use mpi_f08
    implicit none
    type(MPI_Comm)   :: comm
    integer :: my_rank, n_ranks, rank, full_size, net_sum, b, e, root
    integer, allocatable :: arr(:)

    call MPI_Init()

    comm = MPI_COMM_WORLD

    call MPI_Comm_rank(comm, my_rank)
    call MPI_Comm_size(comm, n_ranks)

    root = 0

    if (my_rank == 0) read *, full_size
    call MPI_Bcast(full_size, 1, MPI_INTEGER, root, comm)

    block
        integer :: remainder, quotient, size, disp
        integer, dimension(:), allocatable :: full_arr, sizes, disps

        remainder = modulo(full_size, n_ranks)
        quotient  = (full_size - remainder) / n_ranks

        if (my_rank == root) then
            allocate(full_arr(full_size))
            read *, full_arr(:)
            allocate(sizes(0:n_ranks - 1))
            allocate(disps(0:n_ranks - 1))
        else
            allocate(full_arr(0))
            allocate(sizes(0))
            allocate(disps(0))
        end if

        size = quotient + merge(1, 0, my_rank < remainder)
        disp = 0
        call MPI_Exscan(size, disp, 1, MPI_INTEGER, MPI_SUM, comm)
        b = disp + 1
        e = disp + size
        !call MPI_Scan(size, e, 1, MPI_INTEGER, MPI_SUM, comm)
        allocate(arr(b:e))

        call MPI_Gather(size,  1, MPI_INTEGER, &
                        sizes, 1, MPI_INTEGER, root, comm)
        call MPI_Gather(disp,  1, MPI_INTEGER, &
                        disps, 1, MPI_INTEGER, root, comm)
        call MPI_Scatterv(full_arr, sizes, disps, MPI_INTEGER, &
                          arr,      size,         MPI_INTEGER, root, comm)

        if (allocated(full_arr)) deallocate(full_arr)
        if (allocated(sizes)) deallocate(sizes)
        if (allocated(disps)) deallocate(disps)
    end block

    ! Calculate local sums.
    net_sum = sum(arr(:))

    deallocate(arr)

    block
        integer :: full_sum
        !if (my_rank == root) then
        !    call MPI_Reduce(MPI_IN_PLACE, net_sum, 1, MPI_INTEGER, MPI_SUM, root, comm)
        !else
        !    call MPI_Reduce(net_sum, net_sum, 1, MPI_INTEGER, MPI_SUM, root, comm)
        !end if
        call MPI_Reduce(net_sum, full_sum, 1, MPI_INTEGER, MPI_SUM, root, comm)
        !if (my_rank == root) print "(i0)", net_sum
        if (my_rank == root) print "(i0)", full_sum
    end block

    call MPI_Finalize()

end program parallel_sum