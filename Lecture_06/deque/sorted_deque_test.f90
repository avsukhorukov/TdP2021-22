! This is an example of a doubly-linked list that implements an abstract type of
! a deque (double-ended queue).
program sorted_deque_test
    use :: sorted_deque_mod
    implicit none
    type(a_sorted_deque) :: deque
    integer :: iostatus, val

    ! Insertion
    call sorted_deque_init(deque)
    print "(a)", "Enter list values one per line:"
    do
        read(*, *, iostat=iostatus) val
        if (iostatus /= 0) exit
        call sorted_deque_insert(deque, val)
    end do

    call sorted_deque_display(deque)

! Homework:
!    print "(a)", "Values to delete, one per line:"
!    do
!        read(*, *, iostat=iostatus) val
!        if (iostatus /= 0) exit
!        if (sorted_deque_is_empty(deque)) exit
!        call sorted_deque_remove(deque, val)
!        call sorted_deque_display(deque)
!    end do

    call sorted_deque_destroy(deque)
end program sorted_deque_test