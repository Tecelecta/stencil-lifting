MODULE initialise_chunk_kernel_loop16_mod
CONTAINS
SUBROUTINE initialise_chunk_kernel_loop16(j,k,celldx,x_max,x_min,y_max,y_min,yarea)
INTEGER :: j
INTEGER :: k
INTEGER :: x_max
INTEGER :: x_min
INTEGER :: y_max
INTEGER :: y_min
REAL(kind=8), DIMENSION((x_min - 2):(x_max + 2)) :: celldx
REAL(kind=8), DIMENSION((x_min - 2):(x_max + 2),(y_min - 2):(y_max + 3)) :: yarea
DO k = y_min - 2, y_max + 2
DO j = x_min - 2, x_max + 2
yarea(j,k) = celldx(j)
END DO
END DO
END SUBROUTINE 

END MODULE initialise_chunk_kernel_loop16_mod

