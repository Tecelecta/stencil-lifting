MODULE update_halo_kernel_loop50_mod
CONTAINS
SUBROUTINE update_halo_kernel_loop50(j,k,depth,x_max,x_min,xvel1,y_max,y_min)
INTEGER :: j
INTEGER :: k
INTEGER :: depth
INTEGER :: x_max
INTEGER :: x_min
INTEGER :: y_max
INTEGER :: y_min
REAL(kind=8), DIMENSION((x_min - 2):(x_max + 3),(y_min - 2):(y_max + 3)) :: xvel1
DO j = x_min - depth, x_max + 1 + depth
DO k = 1, depth
xvel1(j,y_max + 1 + k) = xvel1(j,y_max + 1 - k)
END DO
END DO
END SUBROUTINE 

END MODULE update_halo_kernel_loop50_mod

