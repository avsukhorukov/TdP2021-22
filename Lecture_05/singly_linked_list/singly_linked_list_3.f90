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
! In this example we code everything in a separate module.  An example of
! compilation:
!
!   $ gfortran ... singly_linked_list_3.f90 fifo_queue_mod.f90
!
program singly_linked_list_3
    use :: fifo_queue_mod
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

    ! Destroy the list (delete all elements) by getting its all its nodes from
    ! the until the list is empty.
    do
        if (queue_is_empty(head, tail)) exit
        val = queue_get(head, tail)
    end do
end program singly_linked_list_3