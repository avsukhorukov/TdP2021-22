! In this solution the array of bodies is virtually separated into chunks.
! Accelerations, velocities, and coordinates are upadted only for this local
! chunk by each process.  At the end, new coordinates a gathered to all ranks
! from different chunks.  This gives the same exact solution as the serial code
! does.
module parallel_mod

contains
    !---------------------------------------------------------------------------

end module parallel_mod
