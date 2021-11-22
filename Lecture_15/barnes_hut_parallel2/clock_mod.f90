module clock_mod
    implicit none

    type :: a_clock
        real :: current ! current time
        real :: total   ! total integration time
        real :: step    ! integration step
        real :: timer   ! cyclic time count for printing out
        real :: lap     ! cyclic period for printing out
    end type a_clock

    type(a_clock) :: clock

contains
    !---------------------------------------------------------------------------
    ! - read-in the integration step, the cyclic lap period, and the total time;
    ! - set the current time to zero;
    ! - set the cyclic time count to the cyclic lap period.
    subroutine clock_init()
        read *, clock%step
        read *, clock%lap
        read *, clock%total
        clock%current = 0.0
        clock%timer   = clock%lap
        return
    end subroutine clock_init
    !---------------------------------------------------------------------------
    ! - increment the current time by one step;
    ! - decremet the cyclic timer by one step.
    subroutine clock_next_step()
        clock%current = clock%current + clock%step
        clock%timer   = clock%timer   - clock%step
        return
    end subroutine clock_next_step
    !---------------------------------------------------------------------------
    logical function clock_timer_has_elapsed() result(has_elapsed)
        has_elapsed = ( clock%timer <= 0.0 )
        if (has_elapsed) clock%timer = clock%timer + clock%lap
        return
    end function clock_timer_has_elapsed
    !---------------------------------------------------------------------------
    logical function clock_time_is_not_over()
        clock_time_is_not_over = ( clock%current <= clock%total )
        return
    end function clock_time_is_not_over
    !---------------------------------------------------------------------------
end module clock_mod
