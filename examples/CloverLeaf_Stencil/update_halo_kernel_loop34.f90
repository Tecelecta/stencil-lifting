MODULE update_halo_kernel_loop34_mod
CONTAINS
SUBROUTINE update_halo_kernel_loop34(j,k,depth,pressure,x_max,x_min,y_max,y_min)
INTEGER :: j
INTEGER :: k
INTEGER :: depth
INTEGER :: x_max
INTEGER :: x_min
INTEGER :: y_max
INTEGER :: y_min
REAL(kind=8), DIMENSION((x_min - 2):(x_max + 2),(y_min - 2):(y_max + 2)) :: pressure
DO j = x_min - depth, x_max + depth
DO k = 1, depth
pressure(j,y_max + k) = pressure(j,y_max + 1 - k)
END DO
END DO
END SUBROUTINE 

END MODULE update_halo_kernel_loop34_mod

