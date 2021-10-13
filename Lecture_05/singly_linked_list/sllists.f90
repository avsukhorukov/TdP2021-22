! This module implements some basic functionality for a singly-linked list that
! is managed with the head and tail pointers to emulate a FIFO queue.  The
! following methods, a typical minimum of a FIFO queue, are provided:
! - sll_init (nullify both pointers);
! - sll_is_empty (test for null head);
! - sll_put (append to the tail);
! - sll_get (remove from the head);
! - sll_display (walk and print values).
! methods are given: put (append to the tail) and get (remove from the tail).
module sllists
    implicit none

    type :: a_sll_node
        integer :: val
        type(a_sll_node), pointer :: next => null()
    end type a_sll_node

contains

    subroutine sll_init(head, tail)
        type(a_sll_node), pointer, intent(inout) :: head, tail
        head => null()
        tail => null()
        return
    end subroutine sll_init

    ! Test if the list is empty.
    logical function sll_is_empty(head, tail)
        type(a_sll_node), pointer, intent(in) :: head, tail
        sll_is_empty = .not.associated(head)
        return
    end function sll_is_empty

    ! Put a new node to the tail of the list.  The head and tail pointers to the
    ! list are needed to avoid walking through the entire list from its head as
    ! in the basic case win only one head pointer.  There are two cases:
    ! 1) The list is non-empty, so tail%next exists.  Allocate a new node, set
    !    the value, nullify the next component, point tail%next to it, point
    !    tail to it.
    ! 2) The list is empty, so tail%next doesn't exist and tail is null.
    !    Allocate a new node, set the value, nullify the next component, point
    !    head to it, point tail to it.
    subroutine sll_put(head, tail, val)
        type(a_sll_node), pointer, intent(inout) :: head, tail
        integer,                   intent(in)    :: val
        type(a_sll_node), pointer :: temp

        allocate(temp)
        temp%val = val
        temp%next => null()
        if (associated(head)) then  ! list is not empty
            tail%next => temp
        else                        ! list is empty
            head => temp
        end if
        tail => temp

        ! An alternative solution with no extra pointer.
        ! if (associated(tail)) then
        !     allocate(tail%next)
        !     tail => tail%next
        ! else
        !     allocate(head)
        !     tail => head
        ! end if
        ! tail%val = val
        ! tail%next => null()
        return
    end subroutine sll_put

    ! Walk through the list and print the stored values.  Same as in
    ! singly_linked_list_2:
    ! 1) point the temp pointer to the head;
    ! 2) advance the temp pointer to temp%next until temp is null();
    ! 3) while moving, print the value of the current node.
    subroutine sll_display(head, tail)
        type(a_sll_node), pointer, intent(in) :: head, tail
        type(a_sll_node), pointer :: temp

        temp => head
        do while (associated(temp))
            write(*, "(a, i0, a)", advance="no") "(", temp%val, ")->"
            temp => temp%next
        end do
        print "(a)", "null()"
        return
    end subroutine sll_display

    ! Get the first node of the head and return its value.  Same as in
    ! singly_linked_list_2.  Use the second method:
    ! - the temporary pointer remembers the head;
    ! - point the head to the next element;
    ! - deallocate the temporary pointer (the 1st element).
    integer function sll_get(head, tail, is_empty) ! returns val
        type(a_sll_node), pointer, intent(inout) :: head, tail
        logical,                   intent(out)   :: is_empty
        type(a_sll_node), pointer :: temp

        if (sll_is_empty(head, tail)) then
            is_empty = .true.
            tail => null()
        else
            is_empty = .false.
            sll_get = head%val ! return value
            temp => head
            head => head%next
            deallocate(temp)
        end if
        return
    end function sll_get

end module sllists
