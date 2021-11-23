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

        do i = 1, size(bodies)
            bodies(i)%v(:) = bodies(i)%v(:) + (step / 2.0) * bodies(i)%a(:)
        end do
        return
    end subroutine half_kick
    !---------------------------------------------------------------------------
    subroutine drift(step)
        real, intent(in) :: step
        integer :: i

        do i = 1, size(bodies)
            bodies(i)%r(:) = bodies(i)%r(:) + step * bodies(i)%v(:)
        end do
        return
    end subroutine drift
    !---------------------------------------------------------------------------
    ! Define an MPI_User_function (procedure in Fortran) that will be used to
    ! create a new MPI_Op operation for reducing accelerations.  The header is
    ! standard: one must convert C pointers to F pointers for arrays and do the
    ! reduction in a loop over 1..len values.  In C examples this is 0..len-1
    ! but in Fortran looping starts at base-1.  Don't specify the intent for all
    ! four arguments otherwise the compiler will not recognize the right
    ! argument signature of the MPI_Op_create() call.
    subroutine sum_body_a(c_invec, c_inoutvec, len, datatype)
        use, intrinsic :: iso_c_binding, only: c_ptr, c_f_pointer
        type(c_ptr), value :: c_invec, c_inoutvec
        integer            :: len
        type(MPI_Datatype) :: datatype
        type(a_body), dimension(:), pointer :: f_invec, f_inoutvec
        integer :: i

        if (datatype == a_body_datatype) then
            call c_f_pointer(c_invec,    f_invec,    [len])
            call c_f_pointer(c_inoutvec, f_inoutvec, [len])
            do i = 1, len
                f_inoutvec(i)%a(:) = f_inoutvec(i)%a(:) + f_invec(i)%a(:)
            end do
        end if
        return
    end subroutine sum_body_a
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
        do i = 1, 4
            addrs(i) = MPI_Aint_diff(addrs(i), lb)
        end do
        ! Whole body structure.
        call MPI_Type_create_struct(4, [1, 2, 2, 2], addrs, &
                                    [MPI_REAL, MPI_REAL, MPI_REAL, MPI_REAL], &
				    a_tmp_body)
        call MPI_Type_get_extent(a_tmp_body, lb, extent)
        call MPI_Type_create_resized(a_tmp_body, lb, extent, a_body_datatype)
        call MPI_Type_commit(a_body_datatype)
        ! Once the body type is defined you can create the reduction operation
        ! for the %a(:) component.
        call MPI_Op_create(user_fn=sum_body_a, commute=.true., op=sum_body_a_op)
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
        return
    end subroutine init_bodies
    !---------------------------------------------------------------------------
    subroutine destroy_bodies()
        if (allocated(bodies)) deallocate(bodies)
        call MPI_Type_free(a_body_datatype)
        call MPI_Op_free(sum_body_a_op)
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
    ! bodies.  The global ranges are decomposed into four quadrants at the first
    ! level of the tree.  The topology array defines [r=0..1, c=0..1] indices,
    ! which can be used to quickly find the dimensions of this decomposition.
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
        get_dimensions%min = centers + (span / 2.0) * (topology - 1)
        get_dimensions%max = centers + (span / 2.0) * (topology)
        return
    end function get_dimensions
    !---------------------------------------------------------------------------
    ! Check if a particle with the given coordinates `r` belongs to the box with
    ! the given dimensinos `dims`.  This is used at the first level of the tree
    ! to check into which sub-tree the current particle must be inserted based
    ! on the process topology.
    logical function belongs(r, dims)
        real, dimension(2), intent(in) :: r
        type(the_dims),     intent(in) :: dims

        belongs = ( dims%min(1) <= r(1) .and. r(1) < dims%max(1) &
            .and.   dims%min(2) <= r(2) .and. r(2) < dims%max(2) )
        return
    end function belongs
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
            if (.not.belongs(bodies(i)%r, dims)) cycle
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

        do i = 1, size(bodies)
            bodies(i)%a(:) = acceleration_body_tree(bodies(i), tree)
            !call MPI_Allreduce(MPI_IN_PLACE, bodies(i)%a, 2, MPI_REAL, MPI_SUM, comm)
        end do
        call MPI_Allreduce(MPI_IN_PLACE, bodies, size(bodies), a_body_datatype, sum_body_a_op, comm)
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
