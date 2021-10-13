! This implements class functionality of a LIFO stack with a singly-linked list,
! that stores one integer and has a single pointer to the next node.
module lifo_stack
    implicit none
    private

    type :: a_sll_node
        integer :: val
        type(a_sll_node), pointer :: next => null()
    end type a_sll_node

    type, public :: a_stack
        type(a_sll_node), pointer :: head => null()
    contains
        procedure, public :: init    => sll_init
        procedure, public :: push    => sll_push
        procedure, public :: pop     => sll_pop
        procedure, public :: display => sll_display
        final             :: sll_destroy
    end type a_stack

contains

    subroutine sll_init(self)
        class(a_stack), intent(inout) :: self
        self%head => null()
        return
    end subroutine sll_init

    ! Push the given value into the new node at the head of the stack.
    subroutine sll_push(self, val)
        class(a_stack), intent(inout) :: self
        integer,        intent(in)    :: val
        !
        type(a_sll_node), pointer :: temp
        !
        allocate(temp)
        temp%val  =  val
        temp%next => self%head
        self%head => temp
        return
    end subroutine sll_push

    logical function sll_is_empty(self)
        class(a_stack), intent(in) :: self
        !
        sll_is_empty = .not.associated(self%head)
        return
    end function sll_is_empty

    integer function sll_pop(self, is_empty)
        class(a_stack), intent(inout) :: self
        logical,        intent(out)   :: is_empty
        !
        type(a_sll_node), pointer :: temp
        !
        if (sll_is_empty(self)) then
            is_empty = .true.
        else
            is_empty = .false.
            temp      => self%head
            self%head => self%head%next
            sll_pop = temp%val
            deallocate(temp)
        end if
        return
    end function sll_pop

    subroutine sll_display(self)
        class(a_stack), intent(in) :: self
        !
        type(a_sll_node), pointer :: temp
        !
        temp => self%head
        do while (associated(temp))
            write(*, "(a, i0, a)", advance="no") "(", temp%val, ")->"
            temp => temp%next
        end do
        print "(a)", "null()"
        return
    end subroutine sll_display

    ! Destructor in case you forget to pop all nodes from the list.
    subroutine sll_destroy(self)
        type(a_stack), intent(inout) :: self
        integer :: val
        logical :: is_empty
        do
            val = sll_pop(self, is_empty)
            if (is_empty) exit
        end do
        return
    end subroutine sll_destroy

end module lifo_stack