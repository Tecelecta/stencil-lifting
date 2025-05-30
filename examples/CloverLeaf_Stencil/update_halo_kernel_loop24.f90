MODULE update_halo_kernel_loop24_mod
CONTAINS
SUBROUTINE update_halo_kernel_loop24(j,k,density1,depth,x_max,x_min,y_max,y_min)
INTEGER :: j
INTEGER :: k
INTEGER :: depth
INTEGER :: x_max
INTEGER :: x_min
INTEGER :: y_max
INTEGER :: y_min
REAL(kind=8), DIMENSION((x_min - 2):(x_max + 2),(y_min - 2):(y_max + 2)) :: density1
DO k = y_min - depth, y_max + depth
DO j = 1, depth
density1(x_max + j,k) = density1(x_max + 1 - j,k)
END DO
END DO
END SUBROUTINE 

END MODULE update_halo_kernel_loop24_mod

