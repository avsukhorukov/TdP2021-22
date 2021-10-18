program bst_ref_test
    use :: bst_ref_mod
    implicit none
    integer :: iostatus, val
    type(a_bst_ref) :: tree

    type(a_bst_ref), pointer :: tgt

    call bst_init(tree)
    do
        read(*, *, iostat=iostatus) val
        if (iostatus /= 0) exit
        call bst_insert(tree, val)
        !
        !call bst_insert_nonrec(tree, val)
        !
        !tgt => bst_search(tree, val)
        !allocate(tgt%ref)
        !tgt%ref%val       =  val
        !tgt%ref%left%ref  => null()
        !tgt%ref%right%ref => null()
    end do

    call bst_print(tree)

    call bst_destroy(tree)
end program bst_ref_test