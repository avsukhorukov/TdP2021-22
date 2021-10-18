! (A. Stolyarov) Problem 1: code a program that
! 1) reads in numbers from the stdin until a wrong input is given or ctrl+d
!    (EoF) is pressed (iostat=...),
! 2) it stores the numbers in a singly linked list by adding them to the list's
!    head,
! 3) it prints the stored numbers in the reversed order,
! 4) it deallocates the list.
!
! Tip: if the reversed order is needed, then new nodes must be added to the
! list's head.  This emulates the push/pop functionality of a LIFO stack.
program stack_mod_test
    use :: stack_mod
    implicit none
    integer :: iostatus, val
    type(a_sll_node), pointer :: head

    call stack_init(head)
    do
        read(*, *, iostat=iostatus) val
        if (iostatus /= 0) exit
        call stack_push(head, val)
    end do

    call stack_display(head)

    call stack_destroy(head)
end program stack_mod_test