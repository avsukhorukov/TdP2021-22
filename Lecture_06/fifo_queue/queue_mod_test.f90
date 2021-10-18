! (A. Stolyarov) Problem 2: code a program that
! 1) reads in numbers from the stdin until a wrong input is given or ctrl+d
!    (EoF) is pressed,
! 2) it stores the numbers in a singly linked list by adding them to the list's
!    tail,
! 3) it prints the stored numbers in the normal order,
! 4) it deallocates the list.
!
! Tip: for the normal order, the new nodes must be added to the list's tail.
! This emulates the put/get functionality of a FIFO queue.
!
! In this example we code everything in a separate module.  Compile both sources
! twice:
!
!   $ gfortran ... queue_mod_test.f90 queue_mod.f90
!
program queue_mod_test
    use :: queue_mod
    implicit none
    integer :: iostatus, val
    type(a_sll_node), pointer :: head, tail

    call queue_init(head, tail)
    do
        read(*, *, iostat=iostatus) val
        if (iostatus /= 0) exit
        call queue_put(head, tail, val) ! append to the tail
    end do

    call queue_display(head, tail)

    call queue_destroy(head, tail)
end program queue_mod_test