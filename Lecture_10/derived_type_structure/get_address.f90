program get_address
    use mpi_f08
    implicit none
    integer(kind=MPI_ADDRESS_KIND) :: addr1, addr2
    integer :: i
    real    :: x(3, 4)

    call MPI_Init()

    call MPI_Get_address(i, addr1)
    call MPI_Get_address(x(2, 3), addr2)
    print "(z0, 1x, z0)", addr1, abs(addr2 - addr1)

    call MPI_Finalize()
end program get_address