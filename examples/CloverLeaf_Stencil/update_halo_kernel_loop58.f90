MODULE update_halo_kernel_loop58_mod
CONTAINS
SUBROUTINE update_halo_kernel_loop58(j,k,depth,x_max,x_min,y_max,y_min,yvel1)
INTEGER :: j
INTEGER :: k
INTEGER :: depth
INTEGER :: x_max
INTEGER :: x_min
INTEGER :: y_max
INTEGER :: y_min
REAL(kind=8), DIMENSION((x_min - 2):(x_max + 3),(y_min - 2):(y_max + 3)) :: yvel1
DO j = x_min - depth, x_max + 1 + depth
DO k = 1, depth
yvel1(j,y_max + 1 + k) = -yvel1(j,y_max + 1 - k)
END DO
END DO
END SUBROUTINE 

END MODULE update_halo_kernel_loop58_mod

