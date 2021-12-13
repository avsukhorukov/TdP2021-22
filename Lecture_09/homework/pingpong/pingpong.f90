! The MPI library is realized in a way that it takes
! \[
!   t = alpha + beta * sizeof(buf)
! \]
! seconds to send an array `buf` of sizeof(buf) bytes (messages are sent in
! packages).  If you measure the time per a round-trip movement from one process
! to another and back, you can find these two numbers from a simple linear
! regression.
!
! Run with 2 processes only (there are reductions) !
!
! This exercies was suggested by V. Eijkhout
program main
    use mpi_f08
    implicit none
    integer, parameter :: REPS      = 1000
    integer, parameter :: SIZE_MAX  = 100000
    integer, parameter :: SIZE_STEP = 5000
    integer, parameter :: DPK = kind(0.d0)
    integer, parameter :: REAL4_SIZE = storage_size(0.0) / 8

    integer :: n_steps

    integer :: buf_size
    real, allocatable :: buf(:)   ! Buffer to send-receive
    real, allocatable :: s_arr(:) ! Size of message
    real, allocatable :: t_arr(:) ! Time per message
    real :: alpha, beta, rho

    real(kind=DPK) :: start_time, end_time, time_step

    type(MPI_Comm) :: comm
    integer :: n_ranks, my_rank, first_rank, last_rank
    integer :: i, c

    logical :: iam_first_rank, iam_last_rank

    call MPI_Init()
    comm = MPI_COMM_WORLD
    call MPI_Comm_size(comm, n_ranks)
    call MPI_Comm_rank(comm, my_rank)

    first_rank = 0
    last_rank  = n_ranks - 1
    iam_first_rank = (my_rank == first_rank)
    iam_last_rank  = (my_rank == last_rank)

    if (iam_first_rank) then
        n_steps = SIZE_MAX / SIZE_STEP + 1
        allocate(s_arr(n_steps))
        allocate(t_arr(n_steps))
    end if

    buf_size = 0
    c = 1
    do while (buf_size <= SIZE_MAX)
        allocate(buf(buf_size), source=3.14)
        start_time = MPI_Wtime()
        if (iam_first_rank) then
            do i = 1, REPS
                call MPI_Recv(buf, buf_size, MPI_REAL, last_rank, 0, comm, MPI_STATUS_IGNORE)
                call MPI_Send(buf, buf_size, MPI_REAL, last_rank, 0, comm)
            end do
        else if (iam_last_rank) then
            do i = 1, REPS
                call MPI_Send(buf, buf_size, MPI_REAL, first_rank, 0, comm)
                call MPI_Recv(buf, buf_size, MPI_REAL, first_rank, 0, comm, MPI_STATUS_IGNORE)
            end do
        end if
        end_time = MPI_Wtime()
        time_step = (end_time - start_time) / (2.0 * REPS * REAL4_SIZE)
        deallocate(buf)

        if (iam_last_rank) then
            call MPI_Reduce(time_step, time_step, 1, MPI_REAL8, MPI_SUM, first_rank, comm)
        else if (iam_first_rank) then
            call MPI_Reduce(MPI_IN_PLACE, time_step, 1, MPI_REAL8, MPI_SUM, first_rank, comm)
            print '(i8, 1x, es9.2)', buf_size, time_step * 1.d+6 ! mks
            s_arr(c) = real(buf_size)
            t_arr(c) = real(time_step * 1.d+6) ! mks
        end if
        buf_size = buf_size + SIZE_STEP
        c = c + 1
    end do

    if (iam_first_rank) then
        call linear_regression(s_arr, t_arr, alpha, beta, rho)
        print '(2(a, es9.2), a, f0.3)', "alpha=", alpha, " mks, beta=", beta * 1e+3, " ns/byte, r=", rho
        deallocate(s_arr)
        deallocate(t_arr)
    end if

    call MPI_Finalize()

contains

    !---------------------------------------------------------------------------
    ! Does a least-square fit for the model function y(x) = a + b x for the data
    ! provided in arrays x_arr and y_arr of the same size.  The correlation
    ! coefficient is returned in r.
    subroutine linear_regression(x_arr, y_arr, a, b, r)
        implicit none
        real, intent(in)  :: x_arr(:)
        real, intent(in)  :: y_arr(:)
        real, intent(out) :: a
        real, intent(out) :: b
        real, intent(out) :: r
        integer :: n
        real :: sum_x, sum_y, sum_xx, sum_yy, sum_xy, det

        n = size(x_arr)
        if (n /= size(y_arr)) then
            stop "linear_regression: arrays must be of the same size."
        end if
        sum_x  = sum(x_arr)
        sum_y  = sum(y_arr)
        sum_xx = sum(x_arr * x_arr)
        sum_yy = sum(y_arr * y_arr)
        sum_xy = sum(x_arr * y_arr)
        det = n * sum_xx - sum_x**2
        b = (n * sum_xy - sum_x * sum_y) / det
        a = (sum_y - b * sum_x) / n
        r = b * sqrt( det / (n * sum_yy - sum_y**2) )
        return
    end subroutine linear_regression

end program main