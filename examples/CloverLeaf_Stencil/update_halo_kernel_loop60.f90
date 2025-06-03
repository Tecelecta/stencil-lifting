MODULE update_halo_kernel_loop60_mod
CONTAINS
SUBROUTINE update_halo_kernel_loop60(j,k,depth,x_max,x_min,y_max,y_min,yvel1)
INTEGER :: j
INTEGER :: k
INTEGER :: depth
INTEGER :: x_max
INTEGER :: x_min
INTEGER :: y_max
INTEGER :: y_min
REAL(kind=8), DIMENSION((x_min - 2):(x_max + 3),(y_min - 2):(y_max + 3)) :: yvel1
DO k = y_min - depth, y_max + 1 + depth
DO j = 1, depth
yvel1(x_max + 1 + j,k) = yvel1(x_max + 1 - j,k)
END DO
END DO
END SUBROUTINE 

END MODULE update_halo_kernel_loop60_mod

