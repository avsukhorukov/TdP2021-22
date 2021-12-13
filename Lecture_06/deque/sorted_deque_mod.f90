! This module implements a basic functionality for a doubly-linked list that is
! managed with the head and tail pointers to emulate a sorted deque
! (double-ended queue).
module sorted_deque_mod
    implicit none

    type :: a_dll_node
        integer :: val
        type(a_dll_node), pointer :: prev => null()
        type(a_dll_node), pointer :: next => null()
    end type a_dll_node

    type :: a_sorted_deque
        type(a_dll_node), pointer :: head => null()
        type(a_dll_node), pointer :: tail => null()
    end type a_sorted_deque

contains

    subroutine sorted_deque_init(deque)
        type(a_sorted_deque), intent(inout) :: deque
        deque%head => null()
        deque%tail => null()
        return
    end subroutine sorted_deque_init

    logical function sorted_deque_is_empty(deque)
        type(a_sorted_deque), intent(in) :: deque

        sorted_deque_is_empty = .not.associated(deque%head)
        return
    end function sorted_deque_is_empty

    subroutine sorted_deque_insert(deque, val)
        type(a_sorted_deque), intent(inout) :: deque
        integer,              intent(in)    :: val
        !
        type(a_dll_node), pointer :: temp, left, right

        allocate(temp)
        if (.not.associated(deque%head)) then ! deque is empty, special case
            temp = a_dll_node(val=val, prev=null(), next=null())
            deque%head => temp
            deque%tail => temp
        else if (val <= deque%head%val) then
            temp = a_dll_node(val=val, prev=null(), next=deque%head)
            deque%head%prev => temp
            deque%head => temp
        else if (val > deque%tail%val) then
            temp = a_dll_node(val=val, prev=deque%tail, next=null())
            deque%tail%next => temp
            deque%tail => temp
        else
            ! Here at least one interval between the head and tail is present
            ! so as head ≠ tail because the two previous cases of val <=
            ! head.val and tail.val < val exclude the one-node case, when head
            ! == tail.
            left  => deque%head
            right => deque%tail
            do while (.not.associated(left%next, right)) ! until one interval is left
                if (val >  left%next%val)  left  => left%next
                if (val <= right%prev%val) right => right%prev
            end do
            temp = a_dll_node(val=val, prev=left, next=right)
            left%next  => temp
            right%prev => temp
        end if
        return
    end subroutine sorted_deque_insert

    subroutine sorted_deque_remove(deque, val)
        type(a_sorted_deque), intent(inout) :: deque
        integer,              intent(in)    :: val
        type(a_dll_node), pointer :: current

        ! Deque is empty?
        if (.not.associated(deque%head)) return
        ! Val is outside the deque range?
        if (val < deque%head%val .or. deque%tail%val < val) return
        ! If the sorted deque has only one item, then this item's value is
        ! exactly `val`, which we must delete.  The head and tail must be
        ! nullified for an empty deque that remains.
        if (associated(deque%head, deque%tail)) then
            deallocate(deque%head)
            deque%tail => null()
        else
            ! Otherwise the deque has at least two items.  Move the current
            ! pointer from the head forward until val <= current%val.  If val is
            ! not in the deque, then the current pointer will skip it so as
            ! val < current%val.  The current item will never be disassociated
            ! because of the previous range check.
            current => deque%head
            do while (current%val < val)
                current => current%next
            end do
            ! If current%val is val then delete the current item.
            if (current%val == val) then
                if (associated(current, deque%head)) then        ! the head item
                    current%next%prev => current%prev
                    deque%head => current%next
                else if (associated(current, deque%tail)) then   ! the tail item
                    current%prev%next => current%next
                    deque%tail => current%prev
                else                                ! nor head neither tail item
                    current%next%prev => current%prev
                    current%prev%next => current%next
                end if
                deallocate(current)
            end if
        end if
        return
    end subroutine sorted_deque_remove

    ! Not implemented so far, because we insert and delete nodes in the sorted
    ! oder:
    ! - push_front
    ! - push_back
    ! - pop_back

    integer function sorted_deque_pop_front(deque)
        type(a_sorted_deque), intent(inout) :: deque
        !
        type(a_dll_node), pointer :: temp

        temp => deque%head
        if (associated(deque%head, deque%tail)) then ! one node is left only
            deque%head => null()
            deque%tail => null()
        else
            temp%next%prev => null()
            deque%head => temp%next
        end if
        sorted_deque_pop_front = temp%val ! return value
        deallocate(temp)
        return
    end function sorted_deque_pop_front

    subroutine sorted_deque_display(deque)
        type(a_sorted_deque), intent(in) :: deque
        !
        type(a_dll_node), pointer :: temp

        temp => deque%head
        write(*, "(a)", advance="no") "null()="
        do while (associated(temp))
            write(*, "(a, i0, a)", advance="no") "(", temp%val, ")="
            temp => temp%next
        end do
        print "(a)", "null()"
        return
    end subroutine sorted_deque_display

    subroutine sorted_deque_destroy(deque)
        type(a_sorted_deque), intent(inout) :: deque
        !
        integer :: val

        do
            if (sorted_deque_is_empty(deque)) exit
            val = sorted_deque_pop_front(deque)
        end do
        return
    end subroutine sorted_deque_destroy

end module sorted_deque_mod
