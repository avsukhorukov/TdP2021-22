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
!
! This is another version that uses a_sl_list class from sllists_class.f90.
! Compile it twice:
!
!   $ gfortran ... -c lifo_stack.f90
!   $ gfortran ... singly_linked_list_stack.f90 lifo_stack.f90
!
program singly_linked_list_stack
    use :: lifo_stack, only: a_stack
    implicit none
    type(a_stack) :: stack
    integer :: iostatus, val
    logical :: is_empty

    call stack%init()
    do
        read(*, *, iostat=iostatus) val
        if (iostatus /= 0) exit
        call stack%push(val) ! prepend to the head
    end do

    call stack%display()

    do
        val = stack%pop(is_empty)
        if (is_empty) exit
    end do
end program singly_linked_list_stack