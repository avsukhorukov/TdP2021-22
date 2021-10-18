! (A. Stolyarov) Problem 1: code a program that
! 1) reads in numbers from the stdin until a wrong input is given or ctrl+d
!    (EoF) is pressed (iostat=...),
! 2) it stores the numbers in a singly linked list by adding them to the list's
!    head,
! 3) it prints the stored numbers in the reversed order,
! 4) it deallocates the list.
!
! Tip: if the reversed order is needed, then new nodes must be added to the
! list's head.  This emulates the push/pop functionality of a LIFO stack.
!
! This version uses the `stack_class.f90` module to emulate a stack class.
! Compile it twice:
!
!   $ gfortran ... stack_class_test.f90 stack_class.f90
!
program stack_class_test
    use :: stack_class, only: a_stack
    implicit none
    type(a_stack) :: stack
    integer :: iostatus, val

    call stack%init()
    do
        read(*, *, iostat=iostatus) val
        if (iostatus /= 0) exit
        call stack%push(val) ! prepend to the head
    end do

    call stack%display()

    do
        if (stack%is_empty()) exit
        val = stack%pop()
    end do
end program stack_class_test