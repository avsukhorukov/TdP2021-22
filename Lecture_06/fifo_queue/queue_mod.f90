! This module implements some basic functionality for a singly-linked list that
! is managed with the head and tail pointers to emulate a FIFO queue.  The
! following methods, a typical minimum of a FIFO queue, are provided:
! - queue_init (nullify both pointers);
! - queue_is_empty (test for null head);
! - queue_put (append to the tail);
! - queue_get (remove from the head);
! - queue_display (walk and print values).
module queue_mod
    implicit none

    type :: a_sll_node
        integer :: val
        type(a_sll_node), pointer :: next => null()
    end type a_sll_node

contains

    subroutine queue_init(head, tail)
        type(a_sll_node), pointer, intent(inout) :: head, tail

        head => null()
        tail => null()
        return
    end subroutine queue_init

    logical function queue_is_empty(head, tail)
        type(a_sll_node), pointer, intent(in) :: head, tail

        queue_is_empty = .not.associated(head)
        return
    end function queue_is_empty

    ! Put a new node to the tail of the list.  The head and tail pointers are
    ! needed to avoid walking through the entire list from its head, which is
    ! an O(n) operation.  There are two cases:
    ! 1) The list is non-empty, so tail%next exists.  Allocate a new node, set
    !    the value, nullify the next component, point tail%next to it, point
    !    tail to it.
    ! 2) The list is empty, so tail%next doesn't exist and tail is null.
    !    Allocate a new node, set the value, nullify the next component, point
    !    head to it, point tail to it.
    subroutine queue_put(head, tail, val)
        type(a_sll_node), pointer, intent(inout) :: head, tail
        integer,                   intent(in)    :: val
        !
        type(a_sll_node), pointer :: temp

        allocate(temp)
        temp%val  = val
        temp%next => null()
        ! An alternative initialization using a derived-type constructor:
        !temp = a_sll_node(val=val, next=null())
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
        return
    end subroutine queue_put

    ! Get the first node of the head and return its value.  Same as in
    ! singly_linked_list_2.  Use the second method:
    ! - the temporary pointer remembers the head;
    ! - point the head to the next element;
    ! - deallocate the temporary pointer (the 1st element).
    integer function queue_get(head, tail) ! returns val
        type(a_sll_node), pointer, intent(inout) :: head, tail
        !
        type(a_sll_node), pointer :: temp

        queue_get = head%val ! return value
        temp => head
        head => head%next
        deallocate(temp)
        ! if the head is empty, so the tail must be too
        if (.not.associated(head)) tail => null()
        return
    end function queue_get

    ! Walk through the list and print the stored values.  Same as in
    ! singly_linked_list_2:
    ! 1) point the temp pointer to the head;
    ! 2) advance the temp pointer to temp%next until temp is null();
    ! 3) while moving, print the value of the current node.
    subroutine queue_display(head, tail)
        type(a_sll_node), pointer, intent(in) :: head, tail
        !
        type(a_sll_node), pointer :: temp

        temp => head
        do while (associated(temp))
            write(*, "(a, i0, a)", advance="no") "(", temp%val, ")->"
            temp => temp%next
        end do
        print "(a)", "null()"
        return
    end subroutine queue_display

    ! Destroy the queue (delete all nodes) by getting all values from the head
    ! until the queue is empty.
    subroutine queue_destroy(head, tail)
        type(a_sll_node), pointer, intent(inout) :: head, tail
        !
        integer :: val

        do
            if (queue_is_empty(head, tail)) exit
            val = queue_get(head, tail)
        end do
        return
    end subroutine queue_destroy
end module queue_mod
