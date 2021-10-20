program nbody_direct
    use :: clock_class, only: clock
    use :: particles, only: cloud
    implicit none
    real :: step ! integration time step

    call clock%init()
    step = clock%get_step()

    call cloud%init()
    call cloud%update_accelerations()

    print *, cloud%get_size() ! the number of bodies

    time: do while (clock%time_is_not_over())
        if (clock%timer_has_elapsed()) then
            call cloud%print_coordinates()
        end if
        call cloud%half_kick(step)
        call cloud%drift(step)
        call cloud%update_accelerations()
        call cloud%half_kick(step)
        call clock%next_step()
    end do time

    call cloud%destroy()
end program nbody_direct