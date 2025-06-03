MODULE update_halo_kernel_loop39_mod
CONTAINS
SUBROUTINE update_halo_kernel_loop39(j,k,depth,viscosity,x_max,x_min,y_max,y_min)
INTEGER :: j
INTEGER :: k
INTEGER :: depth
INTEGER :: x_max
INTEGER :: x_min
INTEGER :: y_max
INTEGER :: y_min
REAL(kind=8), DIMENSION((x_min - 2):(x_max + 2),(y_min - 2):(y_max + 2)) :: viscosity
DO k = y_min - depth, y_max + depth
DO j = 1, depth
viscosity(1 - j,k) = viscosity(0 + j,k)
END DO
END DO
END SUBROUTINE 

END MODULE update_halo_kernel_loop39_mod

