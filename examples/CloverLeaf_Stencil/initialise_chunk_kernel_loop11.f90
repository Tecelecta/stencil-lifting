MODULE initialise_chunk_kernel_loop11_mod
CONTAINS
SUBROUTINE initialise_chunk_kernel_loop11(j,celldx,dx,x_max,x_min)
INTEGER :: j
REAL(kind=8) :: dx
INTEGER :: x_max
INTEGER :: x_min
REAL(kind=8), DIMENSION((x_min - 2):(x_max + 2)) :: celldx
DO j = x_min - 2, x_max + 2
celldx(j) = dx
END DO
END SUBROUTINE 

END MODULE initialise_chunk_kernel_loop11_mod

