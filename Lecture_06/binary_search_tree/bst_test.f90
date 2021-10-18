program bst_test
    use :: bst_mod
    implicit none
    integer :: iostatus, val
    type(a_bst_node), pointer :: root

    call bst_init(root)
    do
        read(*, *, iostat=iostatus) val
        if (iostatus /= 0) exit
        call bst_insert(root, val)
    end do

    call bst_display(root)

    !call bst_has(5) ! and so on

    call bst_destroy(root)
end program bst_test