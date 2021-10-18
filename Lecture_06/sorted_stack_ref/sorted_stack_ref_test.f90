! Same a s Problem 1 with a LIFO stack.  Here new elements are added in the
! sorted ascending order.  A pointer-to-pointer trich is used.
program sorted_stack_ref_test
    use :: sorted_stack_ref_mod
    implicit none
    type(a_sll_ref) :: head
    integer :: iostatus, val

    ! Insertion
    call sorted_stack_init(head)
    print "(a)", "Enter list values one per line:"
    do
        read(*, *, iostat=iostatus) val
        if (iostatus /= 0) exit
        call sorted_stack_insert(head, val)
    end do

    call sorted_stack_display(head)

    ! Deletion
    print "(a)", "Values to delete, one per line:"
    do
        read(*, *, iostat=iostatus) val
        if (iostatus /= 0) exit
        if (sorted_stack_is_empty(head)) exit
        call sorted_stack_remove(head, val)
        call sorted_stack_display(head)
    end do

    call sorted_stack_destroy(head)
end program sorted_stack_ref_test