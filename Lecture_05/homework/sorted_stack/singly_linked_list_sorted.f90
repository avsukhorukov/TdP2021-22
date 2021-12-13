! Same a s Problem 1 with a LIFO stack.  Here new elements are added in the
! sorted ascending order.
program singly_linked_list_sorted
    implicit none

    type :: a_sll_node
        integer :: val
        type(a_sll_node), pointer :: next => null()
    end type a_sll_node

    type(a_sll_node), pointer :: head, temp, current, previous

    integer :: iostatus, val

    ! Insertion
    print "(a)", "Enter the list values one by line:"
    head => null()
    do
        read(*, *, iostat=iostatus) val
        if (iostatus /= 0) exit

        ! Insert a new node in sorted order.  There are three cases:
        ! 1) empty list (head is null);
        ! 2) non-empty list with insertion before the 1st element (you must
        !    re-point head);
        ! 3) non-empty list with insertion after the 1st element.
        ! You need three extra pointers: `temp` for the target, `current` for
        ! the node to insert before, `previous` for the node to insert after.
        allocate(temp)
        temp%val = val
        previous => null()
        current  => head
        if (.not.associated(head)) then
            head => temp
        else
            do while (associated(current))
                if (current%val < val) then
                    ! Advance by one node.
                    previous => current
                    current  => current%next
                else
                    ! Node found
                    exit
                end if
            end do
            temp%next => current
            if (associated(previous)) then
                ! Insert after the 1st element.
                previous%next => temp
            else
                ! Insert before the 1st element.
                head => temp
            end if
        end if
    end do

    ! Walk through the list and print the stored values.
    temp => head
    do while (associated(temp))
        write(*, "(a, i0, a)", advance="no") "(", temp%val, ")->"
        temp => temp%next
    end do
    print "(a)", "null()"

    ! Deletion
    ! TODO: I don't like this solution, it's too difficult.
    print "(a)", "Values to delete, one by line"
    do
        read(*, *, iostat=iostatus) val
        if (iostatus /= 0) exit

        ! Delete a node with the given value.
        previous => null()
        current  => head
        if (.not.associated(head)) then
            continue ! nothing to delete from an empty list
        else
            if (current%val <= val) then
                do while (associated(current))
                    if (current%val < val) then
                        previous => current     ! advance by one node
                        current  => current%next
                    else
                        exit ! node found
                    end if
                end do
                if (associated(current)) then ! current is not the null() tail.
                    if (associated(previous)) then
                        previous%next => current%next ! delete after the 1st element.
                    else
                        head => current%next ! delete the 1st element.
                    end if
                    deallocate(current)
                end if
            end if
        end if
    end do

    ! Walk through the list and print the stored values.
    temp => head
    do while (associated(temp))
        write(*, "(a, i0, a)", advance="no") "(", temp%val, ")->"
        temp => temp%next
    end do
    print "(a)", "null()"

    ! Destroy the list (delete all elements) by popping its first node from the
    ! head.
    do while (associated(head))
        temp => head
        head => head%next
        deallocate(temp)
    end do
end program singly_linked_list_sorted