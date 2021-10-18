! This implements class functionality of a LIFO stack with a singly-linked list,
! that stores one integer and has a single pointer to the next node.
module stack_class
    implicit none
    private

    type :: a_sll_node
        integer :: val
        type(a_sll_node), pointer :: next => null()
    ! TODO: the final destructor must be here.
    end type a_sll_node

    type, public :: a_stack
        type(a_sll_node), pointer :: head => null()
    contains
        procedure, public :: init     => stack_init
        procedure, public :: is_empty => stack_is_empty
        procedure, public :: push     => stack_push
        procedure, public :: pop      => stack_pop
        procedure, public :: display  => stack_display
        !final             :: stack_destroy
    end type a_stack

contains

    subroutine stack_init(self)
        class(a_stack), intent(inout) :: self

        self%head => null()
        return
    end subroutine stack_init

    logical function stack_is_empty(self)
        class(a_stack), intent(in) :: self

        stack_is_empty = .not.associated(self%head)
        return
    end function stack_is_empty

    ! Push the given value into the new node at the head of the stack.
    subroutine stack_push(self, val)
        class(a_stack), intent(inout) :: self
        integer,        intent(in)    :: val
        !
        type(a_sll_node), pointer :: temp

        allocate(temp)
        temp%val  =  val
        temp%next => self%head
        self%head => temp
        return
    end subroutine stack_push

    ! Popping from an empty stack is an error. Before popping, check for the
    ! emptiness using the stack_is_empty() method.
    integer function stack_pop(self)
        class(a_stack), intent(inout) :: self
        !
        type(a_sll_node), pointer :: temp

        temp      => self%head
        self%head => self%head%next
        stack_pop = temp%val
        deallocate(temp)
        return
    end function stack_pop

    subroutine stack_display(self)
        class(a_stack), intent(in) :: self
        !
        type(a_sll_node), pointer :: temp

        temp => self%head
        do while (associated(temp))
            write(*, "(a, i0, a)", advance="no") "(", temp%val, ")->"
            temp => temp%next
        end do
        print "(a)", "null()"
        return
    end subroutine stack_display

!    ! Destructor in case you forget to pop all nodes from the list.
!    subroutine stack_destroy(self)
!        type(a_stack), intent(inout) :: self
!        integer :: val
!        !
!        do
!            if (stack_is_empty(self)) exit
!            val = stack_pop(self)
!        end do
!        return
!    end subroutine stack_destroy

end module stack_class