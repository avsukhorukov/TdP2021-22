program get_address
    use mpi_f08
    implicit none
    integer :: i, j
    integer(kind=MPI_ADDRESS_KIND) :: addr_i, addr_j, lb, extent

    call MPI_Init()

    ! Variables i and j are in the local stack, therefore addr(i) > addr(j).
    call MPI_Get_address(i, addr_i)
    call MPI_Get_address(j, addr_j)

    call MPI_Type_get_extent(MPI_INTEGER, lb, extent)

    print "(z0)", addr_i
    print "(z0)", MPI_Aint_add(addr_j, extent)

    print "(z0)", MPI_Aint_diff(addr_i, addr_j)

    call MPI_Finalize()
end program get_address