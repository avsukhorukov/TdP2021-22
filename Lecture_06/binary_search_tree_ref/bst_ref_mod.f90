module bst_ref_mod
    implicit none

    type :: a_bst_ref
        type(a_bst_node), pointer :: ref => null()
    end type a_bst_ref

    type :: a_bst_node
        integer         :: val
        type(a_bst_ref) :: left
        type(a_bst_ref) :: right
    end type a_bst_node

contains

    subroutine bst_init(tree)
        type(a_bst_ref), intent(inout) :: tree

        tree%ref => null()
    end subroutine bst_init

    logical function bst_is_empty(tree)
        type(a_bst_ref), intent(inout) :: tree

        bst_is_empty = .not.associated(tree%ref)
    end function bst_is_empty

    ! Search the tree starting from the `tree` node for a given value `v` and
    ! return a pointer to the node referennnce with this value.
    recursive function bst_search(tree, val) result(found)
        type(a_bst_ref), pointer            :: found
        type(a_bst_ref), target, intent(in) :: tree
        integer,                 intent(in) :: val

        if (.not.associated(tree%ref)) then
            found => tree
        else if (val < tree%ref%val) then
            found => bst_search(tree%ref%left, val)
        else if (val > tree%ref%val) then
            found => bst_search(tree%ref%right, val)
        else ! (val == tree%ref%val)
            found => tree
        end if
    end function bst_search

    ! Insert using bst_search().
    subroutine bst_insert(tree, val)
        type(a_bst_ref), intent(inout) :: tree
        integer,         intent(in)    :: val

        type(a_bst_ref), pointer :: temp
        temp => bst_search(tree, val)
        if (.not.associated(temp%ref)) then
            allocate(temp%ref)
            temp%ref%val       =  val
            temp%ref%left%ref  => null()
            temp%ref%right%ref => null()
        else
            continue ! do nothing if the node exists
        end if
        return
    end subroutine bst_insert

    recursive subroutine bst_destroy(tree)
        type(a_bst_ref), intent(inout) :: tree

        if (associated(tree%ref)) then
            call bst_destroy(tree%ref%left)
            call bst_destroy(tree%ref%right)
            deallocate(tree%ref)
        end if
        return
    end subroutine bst_destroy

    ! This is an in-order tree walk.
    recursive subroutine bst_print(tree)
        type(a_bst_ref), intent(in) :: tree

        if (associated(tree%ref)) then
            call bst_print(tree%ref%left)
            print "(i0)", tree%ref%val
            call bst_print(tree%ref%right)
        end if
        return
    end subroutine bst_print

    function bst_has(tree, val) result(has_it)
        logical                     :: has_it
        type(a_bst_ref), intent(in) :: tree
        integer,         intent(in) :: val

        type(a_bst_ref), pointer :: temp
        temp => bst_search(tree, val)
        has_it = associated(temp%ref)
        return
    end function bst_has


    ! Homework -----------------------------------------------------------------

    ! Homework: non-recursive version of `bst_search()`.
    function bst_search_nonrec(tree, val) result(current)
        type(a_bst_ref), pointer            :: current
        type(a_bst_ref), target, intent(in) :: tree
        integer,                 intent(in) :: val

        current => tree
        do
            if (.not.associated(current%ref)) exit ! is null()
            if (current%ref%val == val) then       ! found
                exit
            else if (val < current%ref%val) then
                current => current%ref%left
            else ! (current%ref%val <= val)
                current => current%ref%right
            end if
        end do
        return
    end function bst_search_nonrec

    ! Homework: non-recursive version of `bst_insert`.
    recursive subroutine bst_insert_nonrec(tree, val)
        type(a_bst_ref), target, intent(inout) :: tree
        integer,                 intent(in)    :: val
        type(a_bst_ref), pointer :: current

        current => tree
        do while (associated(current%ref))
            if (val < current%ref%val) then
                current => current%ref%left
            else
                current => current%ref%right
            end if
        end do
        allocate(current%ref)
        current%ref%val       =  val
        current%ref%left%ref  => null()
        current%ref%right%ref => null()
        return
    end subroutine bst_insert_nonrec

end module bst_ref_mod