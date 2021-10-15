program singly_linked_list
    implicit none

    type :: a_sll_node
        integer :: val
        type(a_sll_node), pointer :: next => null()
    end type a_sll_node

    type(a_sll_node), pointer :: head => null()
    !head => null()
    !nullify(head)

    allocate(head)
    head%val = 1
    head%next => null() ! not necessary, do it for good programming style

    allocate(head%next)
    head%next%val = 2
    head%next%next => null()

    allocate(head%next%next)
    head%next%next%val = 3
    head%next%next%next => null()

    ! Show in gdb what's inside head, *head, *(head%next) and so on.  Note that
    ! the address step between the two pointers is 0x10 or 16.  The
    ! integer(kind=4) and pointer (8 b) components are aligned to 8-byte words.

    ! Use Valgrind to show memory leaks.
    ! --- What happens if we just deallocate the head?
    deallocate(head%next%next)
    deallocate(head%next)
    deallocate(head)

    ! How to automate this?  If you advance head => head%next, there will be a
    ! memory leak.
    ! 1) You need at least one more pointer (temp or tail).
    ! 2) You need to decide whether you add new nodes to the head or to the
    !    tail of the list.

    ! Go to singly_linked_list_2.f90
end program singly_linked_list