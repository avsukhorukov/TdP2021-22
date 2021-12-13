! TODO: don't put this in a separate module, students think they do different
! thigs.  Better keep all the stack functionalities in one single module.
module sorted_stack_mod
    implicit none
    
    type :: a_sll_node
        integer :: val
        type(a_sll_node), pointer :: next => null()
    end type a_sll_node

    type :: a_sorted_stack
        type(a_sll_node), pointer :: head => null()
    end type a_sorted_stack

contains

    subroutine sorted_stack_init(stack)
        type(a_sorted_stack), intent(inout) :: stack
        
        stack%head => null()
        return
    end subroutine sorted_stack_init

    logical function sorted_stack_is_empty(stack)
        type(a_sorted_stack), intent(in) :: stack

        sorted_stack_is_empty = .not.associated(stack%head)
        return
    end function sorted_stack_is_empty

    ! Insert a new node in a sorted order.  There are three cases:
    !  1) empty stack (head is null);
    !  2) non-empty stack, the new node is inserted before the 1st one (you must
    !     re-point the head);
    !  3) non-empty stack, the new node is inserted after the 1st one.
    ! You need three extra pointers: `temp` for the target, which is inserted
    ! before node `current` and after node `previous`.
    subroutine sorted_stack_insert(stack, val)
        type(a_sorted_stack), intent(inout) :: stack
        integer,              intent(in)    :: val
        !
        type(a_sll_node), pointer :: temp, current, previous

        allocate(temp)
        temp%val = val
        if (sorted_stack_is_empty(stack)) then
            stack%head => temp
        else ! stack is not empty
            ! Start searching for the current node to insert before it.
            previous => null()
            current  => stack%head
            next_node: do while (associated(current))
                if (current%val < val) then ! advance by one node
                    previous => current
                    current  => current%next
                else
                    exit next_node
                end if
            end do next_node
            temp%next => current
            if (associated(previous)) then
                previous%next => temp ! insert after the 1st node
            else
                stack%head => temp ! insert before the 1st node
            end if
        end if
        return
    end subroutine sorted_stack_insert

    ! Other way of inserting elements in the sorted order, using three branches
    ! instead of two with the search in the last one.
    subroutine sorted_stack_insert2(stack, val)
        type(a_sorted_stack), intent(inout) :: stack
        integer,              intent(in)    :: val
        !
        type(a_sll_node), pointer :: temp, current

        allocate(temp)
        temp%val = val
        if (sorted_stack_is_empty(stack)) then
            stack%head => temp
        else ! stack is not empty
            if (val < stack%head%val) then ! insert before the 1st item
                temp%next => stack%head
                stack%head => temp
            else ! insert after the 1st item
                current => stack%head
                next_item: do while(associated(current%next))
                    if (val >= current%next%val) then ! advance by on item
                        current => current%next
                    else
                        exit next_item
                    end if
                end do next_item
                temp%next => current%next
                current%next => temp
            end if
        end if
        return
    end subroutine sorted_stack_insert2

    ! Recursive insertion.  Observe that we must use the head pointer component
    ! and not the stack structure as the last one is not recursive.  The right
    ! way would be to wrap pointers into a derived-type ref for a pointer-to-
    ! pointer technique.
    !
    ! The base of recursion is the empty head.  Once the list is not empty, you
    ! either insert before the 1st element (no recursion) or after it (with
    ! recursion).
    recursive subroutine sorted_stack_insert_rec(head, val)
        type(a_sll_node), pointer, intent(inout) :: head
        integer,                   intent(in)    :: val
        !
        type(a_sll_node), pointer :: temp

        if (.not.associated(head)) then ! insert into the head
            allocate(head)
            head = a_sll_node(val=val, next=null())
        else if (val <= head%val) then ! insert before the 1st node
            allocate(temp)
            temp = a_sll_node(val=val, next=head)
            head => temp
        else ! insert after the 1st node recursively
            call sorted_stack_insert_rec(head%next, val)
        end if
        return
    end subroutine sorted_stack_insert_rec

    ! Removing a node from a sorted stack for the given value `val` assuming
    ! that the stack is not empty (this is checked before calling this
    ! subroutine.  Check if the value is before the 1st node.  Walk through the
    ! list so that the searched node is between `previous` and `current`
    ! (the last is included in the interval).  If `current` is null, then the
    ! value is after the last node.  Finally, if the current value is the
    ! searched value, then remove `current` considering two cases: `current` is
    ! the 1st node (so `previous` is null), or `current` is not the 1st node.
    subroutine sorted_stack_remove(stack, val)
        type(a_sorted_stack), intent(inout) :: stack
        integer,              intent(in)    :: val
        !
        type(a_sll_node), pointer :: previous, current

        if (val < stack%head%val) return ! before the 1st node
        previous => null()
        current  => stack%head
        next_node: do while (associated(current)) ! search for current%val >= val
            if (current%val < val) then
                previous => current
                current  => current%next
            else
                exit next_node
            end if
        end do next_node
        if (.not.associated(current)) return ! after the last node
        if (current%val == val) then ! found
            if (associated(previous)) then
                previous%next => current%next
            else
                stack%head => current%next
            end if
            deallocate(current)
        end if
        return
    end subroutine sorted_stack_remove

    ! Recursive remove.  Observe that we must use the head pointer component
    ! and not the stack structure as the last one is not recursive.  The right
    ! way would be to wrap pointers into a derived-type ref for a pointer-to-
    ! pointer technique.
    !
    ! The base of recursion is the empty head.  Here it is needed explicitly
    ! even if you check for the emptiness before calling this subroutine,
    ! because this case happens once you come to the end of the stack.  If the
    ! stack is not empty, then you either remove the 1st element (no recursion)
    ! for val = head%val or you remove recursively from the remaining part of
    ! the stack.
    recursive subroutine sorted_stack_remove_rec(head, val)
        type(a_sll_node), pointer, intent(inout) :: head
        integer,                   intent(in)    :: val
        !
        type(a_sll_node), pointer :: temp

        if (.not.associated(head)) then ! nothing to remove
            return
        else if (val < head%val) then ! the value is before the 1st node
            return
        else if (val == head%val) then ! the value is in the 1st node
            temp => head
            head => head%next
            deallocate(temp)
        else ! val > head%val, is in the remaining part of the stack
            call sorted_stack_remove_rec(head%next, val)
        end if
        return
    end subroutine sorted_stack_remove_rec

    subroutine sorted_stack_display(stack)
        type(a_sorted_stack), intent(in) :: stack
        !
        type(a_sll_node), pointer :: temp

        temp => stack%head
        do while (associated(temp))
            write (*, "(a, i0, a)", advance="no") "(", temp%val, ")->"
            temp => temp%next
        end do
        print "(a)", "null()"
        return
    end subroutine sorted_stack_display

    subroutine sorted_stack_destroy(stack)
        type(a_sorted_stack), intent(inout) :: stack
        !
        type(a_sll_node), pointer :: temp

        do
            if (sorted_stack_is_empty(stack)) exit
            temp => stack%head
            stack%head => stack%head%next
            deallocate(temp)
        end do
        return
    end subroutine sorted_stack_destroy

end module sorted_stack_mod