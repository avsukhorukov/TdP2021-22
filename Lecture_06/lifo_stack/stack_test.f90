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
program stack_test
    implicit none

    type :: a_sll_node
        integer :: val
        type(a_sll_node), pointer :: next => null()
    end type a_sll_node

    type(a_sll_node), pointer :: head, temp

    integer :: iostatus, val

    head => null()
    do
        read(*, *, iostat=iostatus) val
        if (iostatus /= 0) exit

        ! Prepend (push) a new node the list's head in the LIFO stack model.
        ! Pushing to an empty and non-empty list is the same.  There are three
        ! steps:
        ! 1) create a new node taget in the heap using an extra pointer (temp);
        ! 2) initialize the new node: add the value and set the next pointer to
        !    the list's head;
        ! 3) point the list's head to this node to make it first in the list.
        allocate(temp)
        temp%val  =  val
        temp%next => head
        head => temp
    end do

    ! Walk through the list and print the stored values.
    ! 1) point the temp pointer to the head;
    ! 2) advance the temp pointer to temp%next until temp is null();
    ! 3) while moving, print the value of the current node.
    temp => head
    do while (associated(temp))
        write(*, "(a, i0, a)", advance="no") "(", temp%val, ")->"
        temp => temp%next
    end do
    print "(a)", "null()"

    ! Destroy the list (delete all elements) by popping its first node from the
    ! head.  There are two equivalent ways of doing this.  To my mind, second is
    ! more didactic as one deallocates not the head pointer but the temporary
    ! one.
    ! Method 1:
    ! - the temporary pointer remembers the next node;
    ! - deallocate the head pointer;
    ! - point the head pointer to the temporary pointer.
    ! Method 2:
    ! - the temporary pointer remembers the head;
    ! - point the head to the next node;
    ! - deallocate the temporary pointer (the 1st node).
    do while (associated(head))
        temp => head
        head => head%next
        deallocate(temp)
    end do
end program stack_test