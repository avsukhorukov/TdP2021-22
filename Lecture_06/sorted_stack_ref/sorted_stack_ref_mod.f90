module sorted_stack_ref_mod
    implicit none

    type :: a_sll_ref
        type(a_sll_node), pointer :: ref => null()
    end type a_sll_ref

    type :: a_sll_node
        integer :: val
        type(a_sll_ref) :: next
    end type a_sll_node

contains

    subroutine sorted_stack_init(head)
        type(a_sll_ref), intent(inout) :: head

        head%ref => null()
        return
    end subroutine sorted_stack_init

    logical function sorted_stack_is_empty(head)
        type(a_sll_ref), intent(in) :: head

        sorted_stack_is_empty = .not.associated(head%ref)
        return
    end function sorted_stack_is_empty

    ! Insert a new node in a sorted order.
    subroutine sorted_stack_insert(head, val)
        type(a_sll_ref), target, intent(inout) :: head
        integer,                 intent(in)    :: val
        !
        type(a_sll_ref),  pointer :: current
        type(a_sll_node), pointer :: tail

        ! Search for the current node to insert into it.
        current => head
        next_node: do while (associated(current%ref))
            if (current%ref%val < val) then ! advance by one node
                current => current%ref%next
            else
                exit next_node
            end if
        end do next_node
        tail => current%ref ! remember the tail
        allocate(current%ref)
        current%ref%val = val
        current%ref%next%ref => tail
        return
    end subroutine sorted_stack_insert

    ! Removing a node from a sorted stack for the given value `val` assuming
    ! that the stack is not empty (this is checked before calling this
    ! subroutine.
    subroutine sorted_stack_remove(head, val)
        type(a_sll_ref), target, intent(inout) :: head
        integer,                 intent(in)    :: val
        !
        type(a_sll_ref),  pointer :: current
        type(a_sll_node), pointer :: tail

        current => head
        next_node: do while (associated(current%ref)) ! search for current%val >= val
            if (current%ref%val < val) then
                current => current%ref%next
            else
                exit next_node
            end if
        end do next_node
        if (.not.associated(current%ref)) return ! empty or after the last node
        if (current%ref%val == val) then ! found
            tail => current%ref%next%ref ! remember the tail
            deallocate(current%ref)
            current%ref => tail
        end if
        return
    end subroutine sorted_stack_remove

    subroutine sorted_stack_display(head)
        type(a_sll_ref), target, intent(in) :: head
        !
        type(a_sll_ref), pointer :: temp

        temp => head
        do while (associated(temp%ref))
            write (*, "(a, i0, a)", advance="no") "(", temp%ref%val, ")->"
            temp => temp%ref%next
        end do
        print "(a)", "null()"
        return
    end subroutine sorted_stack_display

    subroutine sorted_stack_destroy(head)
        type(a_sll_ref), intent(inout) :: head
        !
        type(a_sll_node), pointer :: temp

        do
            if (sorted_stack_is_empty(head)) exit
            temp => head%ref%next%ref
            deallocate(head%ref)
            head%ref => temp
        end do
        return
    end subroutine sorted_stack_destroy

end module sorted_stack_ref_mod