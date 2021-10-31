! Messages communicated in a 1D ring topology with a forward shift:
!
!   previous_rank -> my_rank -> next_rank.
!
! The standard mode MPI_Send and MPI_Recv have the message buffered if its size
! is < 4040 byte.  When this eager limit is exceeded, then the buffered mode is
! replaced with the synchronous one.
!
! The symmetry can be broken by flipping the order of send---receive in one of
! the ranks, say, last (n_ranks - 1).  This serializes the communication:
! messages are sent backward from the last rank to the first one by one
! send--receive pair per time step, until the last rank can send to the first
! rank.  This solution is not parall.
!
! It is better to flip the order of send---receive in every other rank to get
! the so-called ``bucket brigade'' parallelizm.  If the number of processes is
! even, then even ranks send and odd ranks receive, next odd ranks send and even
! receive.  If the number of processes is odd, then there will be the third step
! when the last rank receives from the one before last.
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
    integer function get_rank(rnk, n)
        integer, intent(in) :: rnk, n
        get_rank = modulo(rnk, n)
        return
    end function get_rank

end program ring
