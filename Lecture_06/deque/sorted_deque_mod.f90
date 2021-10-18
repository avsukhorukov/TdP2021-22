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
            ! The two cases of val <= head.val and tail.val < val exclude the
            ! one-node case when head == tail, so in this else-branch we always
            ! have at least one interval between head and tail.
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


    !---------------------------------------------------------------------------
    ! Homework (hard):
    !
    ! sorted_deque_remove(deque, val)
    ! --- remove a node from a sorted deque for the given value.
    !---------------------------------------------------------------------------

    !---------------------------------------------------------------------------
    ! Homework (simple):
    !
    ! sorted_deque_push_front(deque, val)
    ! sorted_deque_push_back(deque, val)
    ! sorted_deque_pop_back(deque) ! function
    !---------------------------------------------------------------------------

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
