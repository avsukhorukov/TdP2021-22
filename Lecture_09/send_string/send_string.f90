!+
! Send a message from all non-zero ranks to rank 0.  Rank 0 prints the message
! received and correctly trimmed.
!-
program send_string
    use mpi_f08
    implicit none

    integer :: my_rank, n_ranks, src
    integer, parameter      :: MSG_SIZE = 40
    integer                 :: msg_len
    character(len=MSG_SIZE) :: message
    type(MPI_Status) :: status

    call MPI_Init()
    call MPI_Comm_rank(MPI_COMM_WORLD, my_rank)
    call MPI_Comm_size(MPI_COMM_WORLD, n_ranks)

    if (my_rank /= 0) then
        write(message, "(a, i0)") 'Greetings from rank ', my_rank
        msg_len = len_trim(message)
        call MPI_Send(buf=message, count=msg_len, datatype=MPI_CHARACTER, &
                      dest=0, tag=0, comm=MPI_COMM_WORLD)
    else ! my_rank == 0
        do src = 1, n_ranks - 1
            call MPI_Recv(buf=message, count=MSG_SIZE, datatype=MPI_CHARACTER, &
                          source=src, tag=0, comm=MPI_COMM_WORLD, status=status)
            !call MPI_Get_count(status=status, datatype=MPI_CHARACTER, count=msg_len)
            print "(3a)", "|", message, "|"
            !print "(3a)", "|", message(1:msg_len), "|"
        end do
    end if

    call MPI_Finalize()
end program send_string