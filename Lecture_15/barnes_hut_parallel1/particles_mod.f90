module particles_mod
    use :: parallel_mod
    implicit none

    integer, parameter :: NODE_IS_EMPTY  = 0
    integer, parameter :: NODE_IS_A_BODY = 1
    integer, parameter :: NODE_IS_A_CELL = 2

    type :: a_body
        real               :: m = 0.0
        real, dimension(2) :: r = [0.0, 0.0]
        real, dimension(2) :: v = [0.0, 0.0]
        real, dimension(2) :: a = [0.0, 0.0]
    end type a_body

    type(a_body), allocatable, target :: bodies(:)

    type :: the_dims
        real, dimension(2) :: min
        real, dimension(2) :: max
    end type the_dims

    ! quadtree treference
    type :: a_node_ref
        type(a_node), pointer :: ref => null()
    end type a_node_ref

    ! quadtree node
    type a_node
        real                      :: M = 0.0
        real, dimension(2)        :: R = [0.0, 0.0]
        type(the_dims)            :: dims
        type(a_body),     pointer :: body => null()
        type(a_node_ref), pointer :: subtree(:, :) => null()
    end type a_node

    type(a_node_ref) :: tree

contains
    !---------------------------------------------------------------------------
    subroutine half_kick(step)
        real, intent(in) :: step
        integer :: i

        do i = first, last
            bodies(i)%v(:) = bodies(i)%v(:) + (step / 2.0) * bodies(i)%a(:)
        end do
        return
    end subroutine half_kick
    !---------------------------------------------------------------------------
    subroutine drift(step)
        real, intent(in) :: step
        integer :: i

        do i = first, last
            bodies(i)%r(:) = bodies(i)%r(:) + step * bodies(i)%v(:)
        end do
        ! Synchronize all new coordinates.
        call MPI_Allgatherv(bodies(first), count,         a_body_r_datatype, &
                            bodies,        counts, disps, a_body_r_datatype, comm)
        return
    end subroutine drift
    !---------------------------------------------------------------------------
    subroutine create_body_datatypes()
        integer(kind=MPI_ADDRESS_KIND) :: addrs(4), lb, extent
        type(MPI_Datatype) :: a_tmp_body ! temporary name
        integer :: i

        call MPI_Get_address(bodies(1),   lb)
        call MPI_Get_address(bodies(1)%m, addrs(1))
        call MPI_Get_address(bodies(1)%r, addrs(2))
        call MPI_Get_address(bodies(1)%v, addrs(3))
        call MPI_Get_address(bodies(1)%a, addrs(4))
        !call MPI_Get_address(bodies(2), extent)
        do i = 1, 4
            addrs(i) = MPI_Aint_diff(addrs(i), lb)
        end do
        !extent = MPI_Aint_diff(extent, lb)
        ! Whole body structure.
        call MPI_Type_create_struct(4, [1, 2, 2, 2], addrs, &
                                    [MPI_REAL, MPI_REAL, MPI_REAL, MPI_REAL], &
                                    a_tmp_body)
        call MPI_Type_get_extent(a_tmp_body, lb, extent)
        call MPI_Type_create_resized(a_tmp_body, lb, extent, a_body_datatype)
        call MPI_Type_commit(a_body_datatype)
        ! Only body%r component.
        call MPI_Type_create_struct(1, [2], addrs([2]), &
                                    [MPI_REAL], &
                                    a_tmp_body)
        call MPI_Type_create_resized(a_tmp_body, lb, extent, a_body_r_datatype)
        call MPI_Type_commit(a_body_r_datatype)
        return
    end subroutine create_body_datatypes
    !---------------------------------------------------------------------------
    subroutine init_bodies()
        integer :: nbodies, i

        if (my_rank == ROOT_RANK) then
            read *, nbodies
            print *, nbodies
        end if
        call MPI_Bcast(nbodies, 1, MPI_INTEGER, ROOT_RANK, comm)
        allocate(bodies(nbodies))
        if (my_rank == ROOT_RANK) then
            do i = 1, nbodies
                read *, bodies(i)%m, bodies(i)%r(:), bodies(i)%v(:)
            end do
        end if
        call create_body_datatypes()
        call MPI_Bcast(bodies, nbodies, a_body_datatype, ROOT_RANK, comm)
        call partition(nbodies) ! find `first` and `last` indices.
        return
    end subroutine init_bodies
    !---------------------------------------------------------------------------
    subroutine destroy_bodies()
        if (allocated(bodies)) deallocate(bodies)
        call MPI_Type_free(a_body_datatype)
        call MPI_Type_free(a_body_r_datatype)
        return
    end subroutine destroy_bodies
    !---------------------------------------------------------------------------
    subroutine print_coordinates()
        integer :: i
        if (my_rank == ROOT_RANK) then
            do i = 1, size(bodies)
                print "(2(es11.3, 1x))", bodies(i)%r(:)
            end do
        end if
    end subroutine print_coordinates
    !---------------------------------------------------------------------------
    ! Get dimensions of the square bounding box that circumscribes all the
    ! bodies.
    type(the_dims) function get_dimensions()
        real, dimension(2) :: mins, maxs, centers
        real               :: span
        integer            :: i

        mins(:) =  huge(0.0)
        maxs(:) = -huge(0.0)
        do i = 1, size(bodies)
            mins = min(mins, bodies(i)%r)
            maxs = max(maxs, bodies(i)%r)
        end do
        span = maxval(maxs - mins) * 1.001 ! +0.1% to cover the border.
        centers = (mins + maxs) / 2.0
        get_dimensions%min = centers - span / 2.0
        get_dimensions%max = centers + span / 2.0
        return
    end function get_dimensions
    !---------------------------------------------------------------------------
    ! For the given `node` and the body coordinate `r` subdivide the bounding
    ! box and find the corresponding subcell `quadrant` and subdimensions
    ! `sub_dims`.
    ! 
    ! Subdivision works only if internal dimensions are known and stored in the
    ! node, which must be associated.
    subroutine subdivide(node, r, quadrant, sub_dims)
        type(a_node),               intent(in) :: node
        real,         dimension(2), intent(in) :: r(2)
        integer,      dimension(2), intent(inout) :: quadrant(2)
        type(the_dims),             intent(inout) :: sub_dims
        integer :: rank
        real    :: left, center, right

        do rank = 1, 2
            left  = node%dims%min(rank)
            right = node%dims%max(rank)
            center = (left + right) / 2.0
            if (left <= r(rank) .and. r(rank) < center) then
                quadrant(rank)     = 1
                sub_dims%min(rank) = left
                sub_dims%max(rank) = center
            else if (center <= r(rank) .and. r(rank) < right) then
                quadrant(rank)     = 2
                sub_dims%min(rank) = center
                sub_dims%max(rank) = right
            else
                print "(a, i0, a, 3(es11.4, a))", &
                    "particles_mod::subdivide: coordinate r(", rank, ") = ", r(rank), &
                    " is out of the cell range [", left, ", ", right, ")."
                stop
            end if
        end do
        return
    end subroutine subdivide
    !---------------------------------------------------------------------------
    subroutine update_centroid(node, body)
        type(a_node), intent(inout) :: node
        type(a_body), intent(in)    :: body
        real, dimension(2) :: RM ! centroid mass momentum

        RM = node%R * node%M
        node%M = node%M + body%m
        node%R = (RM + body%r * body%m) / node%M
        return
    end subroutine update_centroid
    !---------------------------------------------------------------------------
    subroutine create_tree()
        type(the_dims) :: dims
        integer :: i

        dims = get_dimensions()
        do i = 1, size(bodies)
            call insert_node(tree, bodies(i), dims)
        end do
    end subroutine create_tree
    !---------------------------------------------------------------------------
    integer function node_type(node)
        type(a_node_ref), intent(in) :: node

        if (.not.associated(node%ref)) then
            node_type = NODE_IS_EMPTY
        else
            if (.not.associated(node%ref%subtree) &
                .and.associated(node%ref%body)) then
                node_type = NODE_IS_A_BODY
            else if (.not.associated(node%ref%body) &
                     .and.associated(node%ref%subtree)) then
                node_type = NODE_IS_A_CELL
            else
                print "(a)", "particles_mod::node_type: wrong node type, " // &
                    "body and subtree pointers cannot have the same "      // &
                    "association status."
                stop
            end if
        end if
        return
    end function node_type
    !---------------------------------------------------------------------------
    recursive subroutine insert_node(node, body, dims)
        type(a_node_ref),         intent(inout) :: node
        type(a_body),     target, intent(in)    :: body
        type(the_dims),           intent(in)    :: dims
        integer        :: quadrant(2), i, j
        type(the_dims) :: sub_dims

        select case (node_type(node))
        case (NODE_IS_EMPTY)
            allocate(node%ref)
            node%ref%M       =  body%m
            node%ref%R       =  body%r
            node%ref%dims    =  dims
            node%ref%body    => body
            node%ref%subtree => null()
        case (NODE_IS_A_BODY)
            ! One more body turns this node into a cell.
            allocate(node%ref%subtree(2, 2))
            do j = 1, 2
                do i = 1, 2
                    node%ref%subtree(i, j)%ref => null()
                end do
            end do

            ! Move the old body into a subcell.
            call subdivide(node%ref, node%ref%R, quadrant, sub_dims)
            call insert_node(node%ref%subtree(quadrant(1), quadrant(2)), node%ref%body, sub_dims)
            node%ref%body => null()

            ! Store the new body into a subcell.
            call subdivide(node%ref, body%r, quadrant, sub_dims)
            call insert_node(node%ref%subtree(quadrant(1), quadrant(2)), body, sub_dims)
            call update_centroid(node%ref, body)
        case (NODE_IS_A_CELL)
            ! Add the new body into the corresponding subcell.
            call update_centroid(node%ref, body)
            call subdivide(node%ref, body%r, quadrant, sub_dims)
            call insert_node(node%ref%subtree(quadrant(1), quadrant(2)), body, sub_dims)
        case default
            stop "particles_mod::insert_node: wrong node type."
        end select
        return
    end subroutine insert_node
    !---------------------------------------------------------------------------
    recursive subroutine destroy_node(node)
        type(a_node_ref), intent(inout) :: node
        integer :: i, j

        select case (node_type(node))
        case (NODE_IS_EMPTY)
            return
        case (NODE_IS_A_BODY)
            deallocate(node%ref)
        case (NODE_IS_A_CELL)
            do j = 1, 2
                do i = 1, 2
                    call destroy_node(node%ref%subtree(i, j))
                end do
            end do
            deallocate(node%ref%subtree)
            deallocate(node%ref)
        case default
            stop "particles_mod::destroy_node: wrong node type."
        end select
        return
    end subroutine destroy_node
    !---------------------------------------------------------------------------
    subroutine destroy_tree()
        call destroy_node(tree)
    end subroutine destroy_tree
    !---------------------------------------------------------------------------
    subroutine update_accelerations()
        integer :: i

        do i = first, last
            bodies(i)%a(:) = acceleration_body_tree(bodies(i), tree)
        end do
    end subroutine update_accelerations
    !---------------------------------------------------------------------------
    function acceleration_body_body(body, other_body) result(a_ij)
        real, dimension(2)       :: a_ij
        type(a_body), intent(in) :: body, other_body
        real, dimension(2) :: r_ij

        r_ij(:) = other_body%r(:) - body%r(:)
        a_ij(:) = ( other_body%m / norm2(r_ij(:))**3 ) * r_ij(:)
        return
    end function acceleration_body_body
    !---------------------------------------------------------------------------
    recursive function acceleration_body_tree(body, node) result(a_ij)
        real, dimension(2)                    :: a_ij
        type(a_body),      target, intent(in) :: body
        type(a_node_ref),          intent(in) :: node
        integer :: i, j
        real, dimension(2) :: r_ij
        type(a_body), pointer :: p_body

        a_ij(:) = 0.0
        select case (node_type(node))
        case (NODE_IS_EMPTY)
            return
        case (NODE_IS_A_BODY)
            p_body => body
            if (associated(p_body, node%ref%body)) return
            a_ij(:) = acceleration_body_body(body, node%ref%body)
        case (NODE_IS_A_CELL)
            if (is_far(body, node)) then
                r_ij(:) = node%ref%R(:) - body%r(:)
                a_ij(:) = ( node%ref%M / norm2(r_ij)**3 ) * r_ij(:)
            else
                do j = 1, 2
                    do i = 1, 2
                        a_ij(:) = a_ij(:) + acceleration_body_tree(body, node%ref%subtree(i, j))
                    end do
                end do
            end if
        case default
            stop "particles_mod::acceleration_body_tree: wrong node type."
        end select
        return
    end function acceleration_body_tree
    !---------------------------------------------------------------------------
    logical function is_far(body, node)
        class(a_body),    intent(in) :: body
        type(a_node_ref), intent(in) :: node
        real, parameter :: THETA = 0.5 ! opening angle
        real            :: side, r

        side = node%ref%dims%max(1) - node%ref%dims%min(1)
        r    = norm2(node%ref%R - body%r)
        is_far = ( side / r <= THETA )
        return
    end function is_far
    !---------------------------------------------------------------------------
end module particles_mod
