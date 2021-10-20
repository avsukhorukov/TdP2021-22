program nbody_direct
    use :: clock_class, only: clock
    use :: particles, only: cloud, tree
    implicit none
    real :: step ! integration time step

    call clock%init()
    step = clock%get_step()

    call cloud%init()
    call tree%create_from(cloud)
    call cloud%update_accelerations(tree)

    print *, cloud%get_size() ! the number of bodies

!goto 111
    time: do while (clock%time_is_not_over())
        if (clock%timer_has_elapsed()) then
            call cloud%print_coordinates()
        end if
        call cloud%half_kick(step)
        call cloud%drift(step)
        call tree%destroy()
        call tree%create_from(cloud)
        call cloud%update_accelerations(tree)
        call cloud%half_kick(step)
        call clock%next_step()
    end do time
!111 continue

    call tree%destroy()
    call cloud%destroy()
end program nbody_direct