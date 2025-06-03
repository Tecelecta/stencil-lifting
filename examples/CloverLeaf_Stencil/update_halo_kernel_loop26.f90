MODULE update_halo_kernel_loop26_mod
CONTAINS
SUBROUTINE update_halo_kernel_loop26(j,k,depth,energy0,x_max,x_min,y_max,y_min)
INTEGER :: j
INTEGER :: k
INTEGER :: depth
INTEGER :: x_max
INTEGER :: x_min
INTEGER :: y_max
INTEGER :: y_min
REAL(kind=8), DIMENSION((x_min - 2):(x_max + 2),(y_min - 2):(y_max + 2)) :: energy0
DO j = x_min - depth, x_max + depth
DO k = 1, depth
energy0(j,y_max + k) = energy0(j,y_max + 1 - k)
END DO
END DO
END SUBROUTINE 

END MODULE update_halo_kernel_loop26_mod

