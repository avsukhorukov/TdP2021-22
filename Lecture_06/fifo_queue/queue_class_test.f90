! (A. Stolyarov) Problem 2: code a program that
! 1) reads in numbers from the stdin until a wrong input is given or ctrl+d
!    (EoF) is pressed,
! 2) it stores the numbers in a singly linked list by adding them to the list's
!    tail,
! 3) it prints the stored numbers in the normal order,
! 4) it deallocates the list.
!
! Tip: for the normal order, the new nodes must be added to the list's tail.
! This emulates the put/get functionality of a FIFO queue.
!
! Compile this twice:
!
!   $ gfortran ... queue_class_test.f90 queue_class.f90
!
program queue_class_test
    use :: queue_class, only: a_queue
    implicit none
    type(a_queue) :: queue
    integer :: iostatus, val

    call queue%init()
    do
        read(*, *, iostat=iostatus) val
        if (iostatus /= 0) exit
        call queue%put(val) ! append to the tail
    end do

    call queue%display()

    ! TODO: write a proper destructor instead of this.
    do
        if (queue%is_empty()) exit
        val = queue%get()
    end do
end program queue_class_test