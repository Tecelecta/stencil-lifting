MODULE update_halo_kernel_loop36_mod
CONTAINS
SUBROUTINE update_halo_kernel_loop36(j,k,depth,pressure,x_max,x_min,y_max,y_min)
INTEGER :: j
INTEGER :: k
INTEGER :: depth
INTEGER :: x_max
INTEGER :: x_min
INTEGER :: y_max
INTEGER :: y_min
REAL(kind=8), DIMENSION((x_min - 2):(x_max + 2),(y_min - 2):(y_max + 2)) :: pressure
DO k = y_min - depth, y_max + depth
DO j = 1, depth
pressure(x_max + j,k) = pressure(x_max + 1 - j,k)
END DO
END DO
END SUBROUTINE 

END MODULE update_halo_kernel_loop36_mod

