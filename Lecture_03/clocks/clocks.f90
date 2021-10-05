module clocks
    implicit none
    private

    type :: a_clock
        real, private :: t = 0.0
    contains
        procedure :: show => show_clock
        procedure :: set  => set_clock
    end type a_clock

    type(a_clock), public :: clock

contains

    subroutine show_clock(self)
        class(a_clock), intent(in) :: self
        print "(a, f0.3)", "time is ", self%t
    end subroutine show_clock

    subroutine set_clock(self, new_time)
        class(a_clock), intent(inout) :: self
        real, intent(in) :: new_time
        self%t = new_time
    end subroutine set_clock

end module clocks