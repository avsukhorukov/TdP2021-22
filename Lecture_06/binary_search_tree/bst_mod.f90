module bst_mod
    implicit none

    type :: a_bst_node
        integer                   :: val
        type(a_bst_node), pointer :: left  => null()
        type(a_bst_node), pointer :: right => null()
    end type a_bst_node

contains

    subroutine bst_init(root)
        type(a_bst_node), pointer, intent(inout) :: root

        root => null()
    end subroutine bst_init

    logical function bst_is_empty(root)
        type(a_bst_node), pointer, intent(inout) :: root

        bst_is_empty = .not.associated(root)
    end function bst_is_empty

    recursive subroutine bst_insert(root, val)
        type(a_bst_node), pointer, intent(inout) :: root
        integer,                   intent(in)    :: val

        if (bst_is_empty(root)) then
            allocate(root)
            root = a_bst_node(val=val, left=null(), right=null())
        else if (val < root%val) then
            call bst_insert(root%left, val)
        else if (val > root%val) then
            call bst_insert(root%right, val)
        else ! val == root%val, node already exists, skip it.
            continue
        end if
        return
    end subroutine bst_insert

    ! This is not working as expected because a pointer to 0x0 is always
    ! returned and when a new target is allocated in `temp`, it is not saved in
    ! the tree.
    recursive subroutine bst_insert_search(root, val)
        type(a_bst_node), pointer, intent(inout) :: root
        integer,                   intent(in)    :: val

        type(a_bst_node), pointer :: temp
        temp => bst_search(root, val)
        if (.not.associated(temp)) then
            ! This is not working as allocating 0x0 produces a target,
            ! disconnected from the tree.
            allocate(temp)
            temp = a_bst_node(val=val, left=null(), right=null())
        else
            continue ! do nothing if the node exists
        end if
        return
    end subroutine bst_insert_search

    recursive subroutine bst_destroy(root)
        type(a_bst_node), pointer, intent(inout) :: root

        if (associated(root)) then
            call bst_destroy(root%left)
            call bst_destroy(root%right)
            deallocate(root)
        end if
        return
    end subroutine bst_destroy

    ! This is an in-order tree walk.
    recursive subroutine bst_display(root)
        type(a_bst_node), pointer, intent(in) :: root

        if (associated(root)) then
            call bst_display(root%left)
            print "(i0)", root%val
            call bst_display(root%right)
        end if
        return
    end subroutine bst_display

    recursive function bst_has(root, val) result(has_it)
        logical                               :: has_it
        type(a_bst_node), pointer, intent(in) :: root
        integer,                   intent(in) :: val

        if (.not.associated(root)) then
            has_it = .false.
        else if (val < root%val) then
            has_it = bst_has(root%left, val)
        else if (val > root%val) then
            has_it = bst_has(root%right, val)
        else ! val == root%val, node exists
            has_it = .true.
        end if
    end function bst_has

    recursive function bst_has_search(root, val) result(has_it)
        logical                               :: has_it
        type(a_bst_node), pointer, intent(in) :: root
        integer,                   intent(in) :: val

        has_it = associated(bst_search(root, val))
        return
    end function bst_has_search

    ! Search the tree starting from the `root` node for a given value `v` and
    ! return a pointer to the node with this value, if it exists, or a null-
    ! pointer, if it doesn't.
    recursive function bst_search(root, val) result(found)
        type(a_bst_node), pointer             :: found
        type(a_bst_node), pointer, intent(in) :: root
        integer,                   intent(in) :: val

        if (.not.associated(root)) then
            ! Here comes the trouble: this pointer is nullified, so 0x0 address
            ! is returned and you cannot do anything with it.
            found => root
        else if (val < root%val) then
            found => bst_search(root%left, val)
        else if (val > root%val) then
            found => bst_search(root%right, val)
        else ! (val == root%val)
            found => root
        end if
    end function bst_search

    !---------------------------------------------------------------------------
    !
    ! Homework: the size of a tree is the total number of nodes in it.
    recursive function bst_size(root) result(n_nodes)
        integer                               :: n_nodes
        type(a_bst_node), pointer, intent(in) :: root

        ! TODO
        return
    end function bst_size

    ! Homework: the height of the node is the largest number of edges from this
    ! node down to a leaf (a node with no children).
    recursive function bst_height(root) result(height)
        integer                               :: height
        type(a_bst_node), pointer, intent(in) :: root

        ! TODO
        return
    end function bst_height

    ! Homework: non-recursive version of `bst_search()`.
    function bst_search_nonrec(root, val) result(current)
        type(a_bst_node), pointer             :: current
        type(a_bst_node), pointer, intent(in) :: root
        integer,                   intent(in) :: val

        ! TODO
        return
    end function bst_search_nonrec

    ! Homework: for a tree given by the `root` pointer follow all left/right
    ! children until there are no more left and return a pointer to this node
    ! holding the minimum/maximum value.
    !
    ! Try making a generic versino that works both with min and max.
    function bst_find_minmax(root, which) result(extremum)
        type(a_bst_node), pointer             :: extremum
        type(a_bst_node), pointer, intent(in) :: root
        character(len=*),          intent(in) :: which ! `min` or `max`

        ! TODO
        return
    end function bst_find_minmax

    ! Homework (difficult): non-recursive version of `bst_insert()`.
    recursive subroutine bst_insert_nonrec(root, val)
        type(a_bst_node), pointer, intent(inout) :: root
        integer,                   intent(in)    :: val

        ! TODO
        return
    end subroutine bst_insert_nonrec

end module bst_mod