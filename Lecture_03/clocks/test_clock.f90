program test_clock
    use :: clocks, only: clock
    implicit none

    call clock%show()
    call clock%set(5.0)
    call clock%show()

    ! Direct access is forbidden:
    ! print *, clock%t
    ! call clock%set_time(10.0)
end program test_clock
