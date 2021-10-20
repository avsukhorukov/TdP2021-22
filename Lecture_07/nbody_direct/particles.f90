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
        procedure :: half_kick         => body_half_kick
        procedure :: drift             => body_drift
        procedure :: acceleration_from => body_acceleration_from
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
    end type a_cloud

    type(a_cloud), public :: cloud

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
    function body_acceleration_from(self, other_body) result(a_ij)
        real, dimension(2)           :: a_ij
        class(a_body), intent(inout) :: self
        type(a_body),  intent(in)    :: other_body
        real, dimension(2)           :: r_ij

        r_ij(:) = other_body%r(:) - self%r(:)
        a_ij(:) = ( other_body%m / norm2(r_ij(:))**3 ) * r_ij(:)
        return
    end function body_acceleration_from

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
    subroutine cloud_update_accelerations(self)
        class(a_cloud), intent(inout) :: self
        integer :: i, j
        real, dimension(2) :: a_i
        do i = 1, self%size
            associate( b_i => self%bodies(i) )
                a_i(:) = 0.0
                do j = 1, self%size
                    if (j == i) cycle
                    associate( b_j => self%bodies(j) )
                        a_i(:)  = a_i(:) + b_i%acceleration_from(b_j)
                    end associate
                end do
                b_i%a(:) = a_i(:)
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

end module particles