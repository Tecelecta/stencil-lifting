MODULE initialise_chunk_kernel_loop10_mod
CONTAINS
SUBROUTINE initialise_chunk_kernel_loop10(j,cellx,vertexx,x_max,x_min)
INTEGER :: j
INTEGER :: x_max
INTEGER :: x_min
REAL(kind=8), DIMENSION((x_min - 2):(x_max + 2)) :: cellx
REAL(kind=8), DIMENSION((x_min - 2):(x_max + 3)) :: vertexx
DO j = x_min - 2, x_max + 2
cellx(j) = 0.5 * (vertexx(j) + vertexx(j + 1))
END DO
END SUBROUTINE 

END MODULE initialise_chunk_kernel_loop10_mod

