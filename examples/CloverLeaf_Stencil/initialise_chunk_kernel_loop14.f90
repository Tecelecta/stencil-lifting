MODULE initialise_chunk_kernel_loop14_mod
CONTAINS
SUBROUTINE initialise_chunk_kernel_loop14(j,k,dx,dy,volume,x_max,x_min,y_max,y_min)
INTEGER :: j
INTEGER :: k
REAL(kind=8) :: dx
REAL(kind=8) :: dy
INTEGER :: x_max
INTEGER :: x_min
INTEGER :: y_max
INTEGER :: y_min
REAL(kind=8), DIMENSION((x_min - 2):(x_max + 2),(y_min - 2):(y_max + 2)) :: volume
DO k = y_min - 2, y_max + 2
DO j = x_min - 2, x_max + 2
volume(j,k) = dx * dy
END DO
END DO
END SUBROUTINE 

END MODULE initialise_chunk_kernel_loop14_mod

