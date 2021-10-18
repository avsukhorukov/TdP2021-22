! Same a s Problem 1 with a LIFO stack.  Here new elements are added in the
! sorted ascending order.
program sorted_stack_mod_test
    use :: sorted_stack_mod
    implicit none
    integer :: iostatus, val
    type(a_sorted_stack) :: stack

    ! Insertion
    call sorted_stack_init(stack)
    print "(a)", "Enter list values one per line:"
    do
        read(*, *, iostat=iostatus) val
        if (iostatus /= 0) exit
        call sorted_stack_insert(stack, val)
        !call sorted_stack_insert_rec(stack%head, val)
    end do

    call sorted_stack_display(stack)

    ! Deletion
    print "(a)", "Values to delete, one per line:"
    do
        read(*, *, iostat=iostatus) val
        if (iostatus /= 0) exit
        if (sorted_stack_is_empty(stack)) exit
        call sorted_stack_remove(stack, val)
        !call sorted_stack_remove_rec(stack%head, val)
        call sorted_stack_display(stack)
    end do

    call sorted_stack_destroy(stack)
end program sorted_stack_mod_test