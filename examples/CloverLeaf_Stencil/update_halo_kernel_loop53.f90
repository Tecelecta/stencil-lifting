MODULE update_halo_kernel_loop53_mod
CONTAINS
SUBROUTINE update_halo_kernel_loop53(j,k,depth,x_max,x_min,y_max,y_min,yvel0)
INTEGER :: j
INTEGER :: k
INTEGER :: depth
INTEGER :: x_max
INTEGER :: x_min
INTEGER :: y_max
INTEGER :: y_min
REAL(kind=8), DIMENSION((x_min - 2):(x_max + 3),(y_min - 2):(y_max + 3)) :: yvel0
DO j = x_min - depth, x_max + 1 + depth
DO k = 1, depth
yvel0(j,1 - k) = -yvel0(j,1 + k)
END DO
END DO
END SUBROUTINE 

END MODULE update_halo_kernel_loop53_mod

