!
! Send a two unique messages from all non-zero ranks to rank 0.  The first
! message should be the same as in the previous example (send_string.f90), the
! other one should be a bit different.  Send them using two different tags.
!
! At rank 0 receive messages sent with the default tag, one-by-one from all
! non-zero ranks and print them.  Now receive the other group of messages with
! the different tag using source=MPI_ANY_SOURCE in random order and print them
! too.
!
! Compile and run:
!
!   $ mpifort -g -O0 -Wall -Wextra -Wpedantic -fcheck=all -fbacktrace \
!     send_string_tags.f90
!   $ mpirun -np 4 --oversubscribe ./a.out
!
! Observe that both groups are printed in the correct order.
!
! On receive, use source=MPI_ANY_SOURCE for the 1st group and demonstrate that
! messages are printed in random order.
!
! TODO: this example requires too many technical details to be explaiend: what
! and how is matched in the envelope, what's the difference between the buffered
! and synchronous mode (the buffered mode uses the same internal 4 KiB buffer
! and orders messages on arrival before flushing the stdout stream, while the
! synchronous mode receives messages completely shuffled in time), and why on
! systems that emulate many processors, what we have with --ovsersubscribe,
! messages are serialized in the order of PIDs of their processes because the
! operative system emulates multitasking and schedules their execution in a
! simple serial way.
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
        call MPI_Send(message, MSG_SIZE, MPI_CHARACTER, 0, tag, MPI_COMM_WORLD)

        write(message, "(a, i0)") "Another greetings from rank ", my_rank
        msg_len = len_trim(message)
        call MPI_Send(message, MSG_SIZE, MPI_CHARACTER, 0, tag + 1, MPI_COMM_WORLD)

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