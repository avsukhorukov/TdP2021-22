!+
! Send a generated string from all ranks but 0 to rank 0, where they are
! received and printed.  Use some default tag first, and then send again with a
! different tag.
!
! At rank 0 receive messages sent with the default tag one by one and print
! them.  Now receive messages from the second group with a different tag using
! MPI_ANY_SOURCE in random order and print them too.
!
! Compile and run:
!   $ mpifort -g -O0 -Wall -Wextra -Wpedantic -fcheck=all -fbacktrace \
!     send_string_tags.f90
!   $ mpirun -np 4 --oversubscribe ./a.out
!
! Changet `src` to MPI_ANY_SOURCE in the 1st group and show that they come in
! random order.
program send_string_tags
    use mpi_f08
    implicit none

    integer :: my_rank, n_ranks, src, tag
    integer, parameter      :: MSG_SIZE = 40
    integer                 :: msg_len
    character(len=MSG_SIZE) :: message
    type(MPI_Status) :: status

    call MPI_Init()
    call MPI_Comm_size(MPI_COMM_WORLD, n_ranks)
    call MPI_Comm_rank(MPI_COMM_WORLD, my_rank)

    tag = 0

    if (my_rank /= 0) then

        write(message, "(a, i0)") "Greetings from rank ", my_rank
        msg_len = len_trim(message)
        call MPI_Send(message, msg_len, MPI_CHARACTER, 0, tag, MPI_COMM_WORLD)

        write(message, "(a, i0)") "Another greetings from rank ", my_rank
        msg_len = len_trim(message)
        call MPI_Send(message, msg_len, MPI_CHARACTER, 0, tag + 1, MPI_COMM_WORLD)

    else ! my_rank == 0

        do src = 1, n_ranks - 1
            call MPI_Recv(message, MSG_SIZE, MPI_CHARACTER, src, &
                          tag, MPI_COMM_WORLD, status)
            call MPI_Get_count(status, MPI_CHARACTER, msg_len)
            print "(3a)", "|", message(1:msg_len), "|"
        end do

        print *
        do src = 1, n_ranks - 1
            call MPI_Recv(message, MSG_SIZE, MPI_CHARACTER, MPI_ANY_SOURCE, &
                          tag + 1, MPI_COMM_WORLD, status)
            call MPI_Get_count(status, MPI_CHARACTER, msg_len)
            print "(3a)", "|", message(1:msg_len), "|"
        end do

    end if

    call MPI_Finalize()
contains
end program send_string_tags