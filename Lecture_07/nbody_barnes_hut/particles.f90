module particles
    implicit none
    private
    ! real, parameter :: SOFTENING_SQR = 1.0e+4 * epsilon(0.0)

    type :: a_body
        real :: m = 0.0
        real :: r(2) = [0.0, 0.0]
        real :: v(2) = [0.0, 0.0]
        real :: a(2) = [0.0, 0.0]
    contains
        procedure, public :: half_kick         => body_half_kick
        procedure, public :: drift             => body_drift
        procedure, public :: is_far_from       => body_is_far_from
        procedure, private :: body_acceleration_from_body
        procedure, private :: body_acceleration_from_tree
        generic, public    :: acceleration_from => body_acceleration_from_body, body_acceleration_from_tree
    end type a_body

    type :: a_cloud
        integer :: size = -1
        type(a_body), allocatable, dimension(:) :: bodies
    contains
        procedure, public :: init                 => cloud_init
        procedure, public :: destroy              => cloud_destroy
        procedure, public :: half_kick            => cloud_half_kick
        procedure, public :: drift                => cloud_drift
        procedure, public :: get_size             => cloud_get_size
        procedure, public :: update_accelerations => cloud_update_accelerations
        procedure, public :: print_coordinates    => cloud_print_coordinates
        !
        procedure, public :: get_dimensions       => cloud_get_dimensions
    end type a_cloud

    type(a_cloud), public :: cloud

    ! dimensions of a cell
    type :: the_dims
        real, dimension(2) :: min
        real, dimension(2) :: max
    end type the_dims

    integer, parameter :: NODE_IS_EMPTY  = 0
    integer, parameter :: NODE_IS_A_BODY = 1
    integer, parameter :: NODE_IS_A_CELL = 2

    ! quadtree reference
    type :: a_node_ref
        type(a_node), pointer :: ref => null()
    contains
        procedure, public :: type        => node_ref_type
        procedure, public :: insert      => node_ref_insert
        procedure, public :: destroy     => node_ref_destroy
        procedure, public :: create_from => node_ref_create_from
    end type a_node_ref

    ! quadtree node
    type :: a_node
        real                      :: M = 0.0
        real, dimension(2)        :: R = [0.0, 0.0]
        type(the_dims)            :: dims
        type(a_body),     pointer :: body => null()
        type(a_node_ref), pointer :: subtree(:, :) => null()
    contains
        procedure, public :: subdivide       => node_subdivide
        procedure, public :: update_centroid => node_update_centroid
    end type a_node

    type(a_node_ref), public :: tree

contains

    !===========================================================================
    ! a_body type-bound procedures:

    !---------------------------------------------------------------------------
    subroutine body_half_kick(self, step)
        class(a_body), intent(inout) :: self
        real,          intent(in)    :: step

        self%v(:) = self%v(:) + (step / 2.0) * self%a(:)
        return
    end subroutine body_half_kick

    !---------------------------------------------------------------------------
    subroutine body_drift(self, step)
        class(a_body), intent(inout) :: self
        real,          intent(in)    :: step

        self%r(:) = self%r(:) + step * self%v(:)
        return
    end subroutine body_drift

    !---------------------------------------------------------------------------
    function body_acceleration_from_body(self, other_body) result(a_ij)
        real, dimension(2)           :: a_ij
        class(a_body), intent(inout) :: self
        type(a_body),  intent(in)    :: other_body
        real, dimension(2)           :: r_ij

        r_ij(:) = other_body%r(:) - self%r(:)
        a_ij(:) = ( other_body%m / norm2(r_ij(:))**3 ) * r_ij(:)
        return
    end function body_acceleration_from_body

    !---------------------------------------------------------------------------
    recursive function body_acceleration_from_tree(self, node) result(a_ij)
        real, dimension(2)                       :: a_ij
        class(a_body),     target, intent(inout) :: self
        type(a_node_ref),          intent(in)    :: node
        !
        integer :: i, j
        real, dimension(2) :: r_ij
        type(a_body), pointer :: body

        a_ij(:) = 0.0
        select case (node%type())
        case (NODE_IS_EMPTY)
            return
        case (NODE_IS_A_BODY)
            body => self
            if (associated(body, node%ref%body)) return
            a_ij(:) = self%acceleration_from(node%ref%body)
        case (NODE_IS_A_CELL)
            if (self%is_far_from(node)) then
                r_ij(:) = node%ref%R(:) - self%r(:)
                a_ij(:) = ( node%ref%M / norm2(r_ij)**3 ) * r_ij(:)
            else
                do j = 1, 2
                    do i = 1, 2
                        a_ij(:) = a_ij(:) + self%acceleration_from(node%ref%subtree(i, j))
                    end do
                end do
            end if
        case default
            stop "particles::body_acceleration_from_tree: wrong node type."
        end select

        return
    end function body_acceleration_from_tree

    !---------------------------------------------------------------------------
    logical function body_is_far_from(self, node)
        class(a_body),    intent(in) :: self
        type(a_node_ref), intent(in) :: node
        real, parameter :: THETA = 0.5 ! opening angle
        real            :: side, r

        side = node%ref%dims%max(1) - node%ref%dims%min(1)
        r    = norm2(node%ref%R - self%r)
        body_is_far_from = ( side / r <= THETA )
        return
    end function body_is_far_from

    !===========================================================================
    ! a_cloud type-bound procedures:

    !---------------------------------------------------------------------------
    subroutine cloud_init(self)
        class(a_cloud), intent(inout) :: self
        integer :: i

        read *, self%size
        allocate(self%bodies(self%size))
        do i = 1, self%size
            associate( b_i => self%bodies(i) )
                read *, b_i%m, b_i%r(:), b_i%v(:)
            end associate
        end do
    end subroutine cloud_init

    !---------------------------------------------------------------------------
    integer function cloud_get_size(self)
        class(a_cloud), intent(in) :: self
        cloud_get_size = self%size
        return
    end function cloud_get_size

    !---------------------------------------------------------------------------
    subroutine cloud_destroy(self)
        class(a_cloud), intent(inout) :: self
        if (allocated(self%bodies)) deallocate(self%bodies)
        self%size = -1
        return
    end subroutine cloud_destroy

    !---------------------------------------------------------------------------
    subroutine cloud_half_kick(self, step)
        class(a_cloud), intent(inout) :: self
        real,           intent(in)    :: step
        integer :: i
        do i = 1, self%size
            call self%bodies(i)%half_kick(step)
        end do
        return
    end subroutine cloud_half_kick

    !---------------------------------------------------------------------------
    subroutine cloud_drift(self, step)
        class(a_cloud), intent(inout) :: self
        real,           intent(in)    :: step
        integer :: i
        do i = 1, self%size
            call self%bodies(i)%drift(step)
        end do
        return
    end subroutine cloud_drift

    !---------------------------------------------------------------------------
    subroutine cloud_update_accelerations(self, tree)
        class(a_cloud),   intent(inout) :: self
        type(a_node_ref), intent(in)    :: tree
        integer :: i

        do i = 1, self%size
            associate( b_i => self%bodies(i) )
                b_i%a(:) = b_i%acceleration_from(tree)
            end associate
        end do
        return
    end subroutine cloud_update_accelerations

    !---------------------------------------------------------------------------
    subroutine cloud_print_coordinates(self)
        class(a_cloud), intent(in) :: self
        integer                    :: i
        do i = 1, self%size
            print "(2(es11.3, 1x))", self%bodies(i)%r(:)
        end do
    end subroutine cloud_print_coordinates

    !---------------------------------------------------------------------------
    ! Get dimensions of the square bounding box that circumscribes all the
    ! bodies.
    type(the_dims) function cloud_get_dimensions(self)
        class(a_cloud), intent(in) :: self
        real, dimension(2) :: mins, maxs, centers
        real               :: span
        integer            :: i

        mins =  huge(0.0)
        maxs = -huge(0.0)
        do i = 1, self%size
            mins = min(mins, self%bodies(i)%r)
            maxs = max(maxs, self%bodies(i)%r)
        end do
        span = maxval(maxs - mins) * 1.001 ! +0.1% to cover the border
        centers = (mins + maxs) / 2.0
        cloud_get_dimensions%min = centers - span / 2.0
        cloud_get_dimensions%max = centers + span / 2.0
        return
    end function cloud_get_dimensions

    !===========================================================================
    ! a node ref type-bound procedures:

    !---------------------------------------------------------------------------
    integer function node_ref_type(self)
        class(a_node_ref), intent(in) :: self

        if (.not.associated(self%ref)) then
            node_ref_type = NODE_IS_EMPTY
        else
            if (.not.associated(self%ref%subtree) &
                .and.associated(self%ref%body)) then
                node_ref_type = NODE_IS_A_BODY
            else if (.not.associated(self%ref%body) &
                     .and.associated(self%ref%subtree)) then
                node_ref_type = NODE_IS_A_CELL
            else
                print "(a)", "particles::node_ref_type: wrong node type, " // &
                    "body and subtree pointers cannot have the same "      // &
                    "association status."
                stop
            end if
        end if
        return
    end function node_ref_type

    !---------------------------------------------------------------------------
    recursive subroutine node_ref_insert(self, body, dims)
        class(a_node_ref),          intent(inout) :: self
        type(a_body),      pointer, intent(in)    :: body
        type(the_dims),             intent(in)    :: dims

        integer        :: quadrant(2), i, j
        type(the_dims) :: sub_dims

        select case (self%type())
        case (NODE_IS_EMPTY)
            allocate(self%ref)
            associate( node => self%ref )
                node%M       = body%m
                node%R       = body%r
                node%dims    = dims
                node%body    => body
                node%subtree => null()
            end associate
        case (NODE_IS_A_BODY)
            associate( node => self%ref )
                ! One more body turns this node into a cell.
                allocate(node%subtree(2, 2))
                do j = 1, 2
                    do i = 1, 2
                        node%subtree(i, j)%ref => null()
                    end do
                end do

                ! Move the old body into a subcell.
                call node%subdivide(node%R, quadrant, sub_dims)
                call node%subtree(quadrant(1), quadrant(2))%insert(node%body, sub_dims)
                node%body => null()

                ! Store the new body into a subcell.
                call node%subdivide(body%r, quadrant, sub_dims)
                call node%subtree(quadrant(1), quadrant(2))%insert(body, sub_dims)
                call node%update_centroid(body)
            end associate
        case (NODE_IS_A_CELL)
            associate( node => self%ref )
                ! Add the new body into the corresponding subcell.
                call node%update_centroid(body)
                call node%subdivide(body%r, quadrant, sub_dims)
                call node%subtree(quadrant(1), quadrant(2))%insert(body, sub_dims)
            end associate
        case default
            stop "particles::node_ref_insert: wrong node type."
        end select
        return
    end subroutine node_ref_insert

    !---------------------------------------------------------------------------
    subroutine node_ref_create_from(self, cloud)
        class(a_node_ref),         intent(inout) :: self
        type(a_cloud),     target, intent(in)    :: cloud
        !
        type(the_dims)        :: dims
        type(a_body), pointer :: body
        integer               :: i

        dims = cloud%get_dimensions()
        do i = 1, cloud%size
            body => cloud%bodies(i)
            call self%insert(body, dims)
        end do
        return
    end subroutine node_ref_create_from

    !---------------------------------------------------------------------------
    recursive subroutine node_ref_destroy(self)
        class(a_node_ref),          intent(inout) :: self

        integer :: i, j

        select case (self%type())
        case (NODE_IS_EMPTY)
            return
        case (NODE_IS_A_BODY)
            deallocate(self%ref)
        case (NODE_IS_A_CELL)
            do j = 1, 2
                do i = 1, 2
                    call self%ref%subtree(i, j)%destroy()
                end do
            end do
            deallocate(self%ref%subtree)
            deallocate(self%ref)
        case default
            stop "particles::node_ref_destroy: wrong node type."
        end select
        return
    end subroutine node_ref_destroy

    !===========================================================================
    ! a node type-bound procedures:

    !---------------------------------------------------------------------------
    ! For the current node `self` and the given body coordinate `r` subdivide
    ! the bounding box and find the corresponding subcell through its quadrant
    ! tuple and subdimensions.
    ! 
    ! Subdivision works only if internal dimensions are known and are stored in
    ! the a_node structure, which must be associated.
    subroutine node_subdivide(self, r, quadrant, sub_dims)
        class(a_node),                intent(in)  :: self
        real,           dimension(2), intent(in)  :: r
        integer,        dimension(2), intent(out) :: quadrant
        type(the_dims),               intent(out) :: sub_dims
        !
        integer :: rank
        real    :: left, center, right

        do rank = 1, 2
            left   = self%dims%min(rank)
            right  = self%dims%max(rank)
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
                    "particles::node_subdivide: coordinate r(", rank, ") = ", r(rank), &
                    " is out of the cell range [", left, ", ", right, ")."
                stop
            end if
        end do
        return
    end subroutine node_subdivide

    !---------------------------------------------------------------------------
    subroutine node_update_centroid(self, body)
        class(a_node), intent(inout) :: self
        type(a_body),  intent(in)    :: body
        real, dimension(2) :: RM ! centroid mass momentum

        RM = self%R * self%M
        self%M = self%M + body%m
        self%R = (RM + body%r * body%m) / self%M
        return
    end subroutine node_update_centroid

end module particles