module clock_class
    implicit none
    private

    type :: a_clock
        real :: current ! current time
        real :: total   ! total integration time
        real :: step    ! integration step
        real :: timer   ! cyclic time count for printing out
        real :: lap     ! cyclic period for printing out
    contains
        procedure, public :: init
        procedure, public :: next_step
        procedure, public :: timer_has_elapsed
        procedure, public :: time_is_not_over
        procedure, public :: get_step
    end type a_clock

    type(a_clock), public :: clock

contains

    !---------------------------------------------------------------------------
    ! - read-in the integration step, the cyclic lap period, and the total time;
    ! - set the current time to zero;
    ! - set the cyclic time count to the cyclic lap period.
    subroutine init(self)
        class(a_clock), intent(inout) :: self
        read *, self%step
        read *, self%lap
        read *, self%total
        self%current = 0.0
        self%timer   = self%lap
        return
    end subroutine init

    !---------------------------------------------------------------------------
    ! - increment the current time by one step;
    ! - decremet the cyclic timer by one step.
    subroutine next_step(self)
        class(a_clock), intent(inout) :: self
        self%current = self%current + self%step
        self%timer   = self%timer   - self%step
        return
    end subroutine next_step

    !---------------------------------------------------------------------------
    logical function timer_has_elapsed(self) result(has_elapsed)
        class(a_clock), intent(inout) :: self
        has_elapsed = ( self%timer <= 0.0 )
        if (has_elapsed) self%timer = self%timer + self%lap
        return
    end function timer_has_elapsed

    !---------------------------------------------------------------------------
    logical function time_is_not_over(self)
        class(a_clock), intent(in) :: self
        time_is_not_over = ( self%current <= self%total )
        return
    end function time_is_not_over

    !---------------------------------------------------------------------------
    real function get_step(self) result(step)
        class(a_clock), intent(in) :: self
        step = self%step
        return
    end function get_step

end module clock_class