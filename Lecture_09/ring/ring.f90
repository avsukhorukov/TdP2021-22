! Messages communicated in a 1D ring topology with a forward shift:
!
!   previous_rank -> my_rank -> next_rank.
!
! The standard MPI_Send/MPI_Recv buffers the message if its size is < 4040 b.
! The total size of the envelope is around 4 kb.  When the limit is exceeded,
! the standard MPI_Send/MPI_Recv becomes a synchronous MPI_Ssend/MPI_Recv.
!
! The symmetry can be broken by flipping the order of send-recv in one of the
! ranks (e.g., last n_ranks - 1).  This serializes the communication---the
! messages are sent backwards from the last rank to the first one, then the last
! one sends its message back to rank 0.  This is not a full parallelism.
!
! It is better to flip the order in every other rank to get a semi-parallel
! solution.  If the communicator's size is even, then ever ranks send and odd
! ranks receive, next odd ranks send and even receive.  If the communicator's
! size is odd, then the last rank waits until the second step as it has to send
! to a receive in rank 0, which is only called after a send.
!
! For a single process a blocking MPI_Send will wait for a blocking MPI_Recv in
! the same process and will not succed.
program ring
    use mpi_f08
    implicit none
    integer :: my_rank, n_ranks, tag, prev_rank, next_rank
    integer, parameter      :: MSG_SIZE = 30 ! use 100 000 000 for last
    character(len=MSG_SIZE) :: message
    type(MPI_Status) :: status
    character(len=10) :: comm_mode
    integer :: r

    real(kind=kind(0.d0)) :: init_time

    call MPI_Init()

    call MPI_Comm_rank(MPI_COMM_WORLD, my_rank)
    call MPI_Comm_size(MPI_COMM_WORLD, n_ranks)

    if (n_ranks == 1) then
        print "(a)", "No communication with 1 process."
        call MPI_Finalize()
        stop
    end if

    next_rank = get_rank(my_rank + 1, n_ranks)
    prev_rank = get_rank(my_rank - 1, n_ranks)

    write(message, "(a, i0, a)") "Greetings from rank ", my_rank, "."

    tag = 0

    if (my_rank == 0) then
        print "(a)", "Which communication mode (all, last, even-odd)?"
        read *, comm_mode
        do r = 1, n_ranks - 1
            call MPI_Send(comm_mode, len(comm_mode), MPI_CHARACTER, r, 0, &
                          MPI_COMM_WORLD)
        end do
    else ! (my_rank /= 0)
        call MPI_Recv(comm_mode, len(comm_mode), MPI_CHARACTER, 0, 0, &
                      MPI_COMM_WORLD, status)
    end if

    !print "(a, i0, 3a)", "my_rank=", my_rank, ", mode=", trim(comm_mode), "."
    init_time = MPI_Wtime()

    select case (trim(comm_mode))
    case ("all")
        call MPI_Send(message, MSG_SIZE, MPI_CHARACTER, next_rank, tag, MPI_COMM_WORLD)
        print "(i1, '> ', i1, ':', f12.9)", my_rank, next_rank, MPI_Wtime()
        call MPI_Recv(message, MSG_SIZE, MPI_CHARACTER, prev_rank, tag, MPI_COMM_WORLD, status)
        print "(i1, ' >', i1, ':', f12.9)", prev_rank, my_rank, MPI_Wtime()
    case ("last")
        if (my_rank == n_ranks - 1) then
            call MPI_Recv(message, MSG_SIZE, MPI_CHARACTER, prev_rank, tag, MPI_COMM_WORLD, status)
            print "(i1, ' >', i1, ':', f12.9)", prev_rank, my_rank, MPI_Wtime()
            call MPI_Send(message, MSG_SIZE, MPI_CHARACTER, next_rank, tag, MPI_COMM_WORLD)
            print "(i1, '> ', i1, ':', f12.9)", my_rank, next_rank, MPI_Wtime()
        else
            call MPI_Send(message, MSG_SIZE, MPI_CHARACTER, next_rank, tag, MPI_COMM_WORLD)
            print "(i1, '> ', i1, ':', f12.9)", my_rank, next_rank, MPI_Wtime()
            call MPI_Recv(message, MSG_SIZE, MPI_CHARACTER, prev_rank, tag, MPI_COMM_WORLD, status)
            print "(i1, ' >', i1, ':', f12.9)", prev_rank, my_rank, MPI_Wtime()
        end if
    case ("even-odd")
        if (modulo(my_rank, 2) == 0) then ! even
            call MPI_Send(message, MSG_SIZE, MPI_CHARACTER, next_rank, tag, MPI_COMM_WORLD)
            print "(i1, '> ', i1, ':', f12.9)", my_rank, next_rank, MPI_Wtime()
            call MPI_Recv(message, MSG_SIZE, MPI_CHARACTER, prev_rank, tag, MPI_COMM_WORLD, status)
            print "(i1, ' >', i1, ':', f12.9)", prev_rank, my_rank, MPI_Wtime()
        else ! odd
            call MPI_Recv(message, MSG_SIZE, MPI_CHARACTER, prev_rank, tag, MPI_COMM_WORLD, status)
            print "(i1, ' >', i1, ':', f12.9)", prev_rank, my_rank, MPI_Wtime()
            call MPI_Send(message, MSG_SIZE, MPI_CHARACTER, next_rank, tag, MPI_COMM_WORLD)
            print "(i1, '> ', i1, ':', f12.9)", my_rank, next_rank, MPI_Wtime()
        end if
    case default
        if (my_rank == 0) then
            print "(3a)", "Wrong communication mode '", trim(comm_mode), "'."
        end if
        call MPI_Finalize()
        stop
    end select

    call MPI_Finalize()

contains

    ! Use the modulo division to get the corresponding rank in the ring topology
    ! with `n` processes.
    function get_rank(i, n) result(rank)
        integer :: rank
        integer :: i, n
        rank = modulo(i, n)
        return
    end function get_rank

end program ring
