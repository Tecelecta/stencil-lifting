MODULE update_halo_kernel_loop45_mod
CONTAINS
SUBROUTINE update_halo_kernel_loop45(j,k,depth,x_max,x_min,xvel0,y_max,y_min)
INTEGER :: j
INTEGER :: k
INTEGER :: depth
INTEGER :: x_max
INTEGER :: x_min
INTEGER :: y_max
INTEGER :: y_min
REAL(kind=8), DIMENSION((x_min - 2):(x_max + 3),(y_min - 2):(y_max + 3)) :: xvel0
DO j = x_min - depth, x_max + 1 + depth
DO k = 1, depth
xvel0(j,1 - k) = xvel0(j,1 + k)
END DO
END DO
END SUBROUTINE 

END MODULE update_halo_kernel_loop45_mod

