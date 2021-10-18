! This module implements a basic functionality for a singly-linked list that is
! managed with the head pointers to emulate a LIFO stack.  The interface methods
! are given as module procedures:
! - stack_init (nullify both pointers);
! - stack_is_empty (test for null head);
! - stack_push (append to the tail);
! - stack_pop (remove from the head);
! - stack_display (walk and print values).
module stack_mod
    implicit none

    type :: a_sll_node
        integer :: val
        type(a_sll_node), pointer :: next => null()
    end type a_sll_node

contains

    subroutine stack_init(head)
        type(a_sll_node), pointer, intent(inout) :: head
        head => null()
        return
    end subroutine stack_init

    logical function stack_is_empty(head)
        type(a_sll_node), pointer, intent(in) :: head
        !
        stack_is_empty = .not.associated(head)
        return
    end function stack_is_empty

    ! Prepend (push) a new node the list's head in the LIFO stack model.
    ! Pushing to an empty or non-empty list is the same.  There are three steps:
    !  1) create a new node taget in the heap using an extra pointer (temp);
    !  2) initialize the new node: add the value and set the next pointer to the
    !     list's head;
    !  3) point the list's head to this node to make it first in the list.
    subroutine stack_push(head, val)
        type(a_sll_node), pointer, intent(inout) :: head
        integer,                   intent(in)    :: val
        !
        type(a_sll_node), pointer :: temp

        allocate(temp)
        temp%val = val
        temp%next => head
        head => temp
        return
    end subroutine stack_push

    ! Pop the first value from the list's head and remove the corresponding
    ! node.  Do it in three steps (deletion method 2):
    !  1) the temporary pointer remembers the head;
    !  2) point the head to the next node;
    !  3) deallocate the temporary pointer (the 1st node).
    integer function stack_pop(head)
        type(a_sll_node), pointer, intent(inout) :: head
        !
        type(a_sll_node), pointer :: temp

        temp => head
        head => head%next
        stack_pop = temp%val ! return value
        deallocate(temp)
        return
    end function stack_pop

    ! Walk through the list and print the stored values.
    ! 1) point the temp pointer to the head;
    ! 2) advance the temp pointer to temp%next until temp is null();
    ! 3) while moving, print the value of the current node.
    subroutine stack_display(head)
        type(a_sll_node), pointer, intent(in) :: head
        !
        type(a_sll_node), pointer :: temp

        temp => head
        do while (.not.stack_is_empty(temp))
            write(*, "(a, i0, a)", advance="no") "(", temp%val, ")->"
            temp => temp%next
        end do
        print "(a)", "null()"
        return
    end subroutine stack_display

    ! Destroy the stack (delete all elements) by popping every first node from
    ! the head util the list is empty.
    subroutine stack_destroy(head)
        type(a_sll_node), pointer, intent(inout) :: head
        !
        integer :: val

        do
            if (stack_is_empty(head)) exit
            val = stack_pop(head)
        end do
        return
    end subroutine stack_destroy

end module stack_mod
