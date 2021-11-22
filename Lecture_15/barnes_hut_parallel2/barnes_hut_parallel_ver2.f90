program barnes_hut_parallel
    use :: clock_mod
    use :: particles_mod
    implicit none

    call clock_init()

    call init_bodies()
    call create_tree()
    call update_accelerations()

    do while (clock_time_is_not_over())
        if (clock_timer_has_elapsed()) then
            call print_coordinates()
        end if
        call half_kick(clock%step)
        call drift(clock%step)
        call destroy_tree()
        call create_tree()
        call update_accelerations()
        call half_kick(clock%step)
        call clock_next_step()
    end do
    call destroy_tree()
    call destroy_bodies()
end program barnes_hut_parallel