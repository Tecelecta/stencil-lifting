MODULE update_halo_kernel_loop63_mod
CONTAINS
SUBROUTINE update_halo_kernel_loop63(j,k,depth,vol_flux_x,x_max,x_min,y_max,y_min)
INTEGER :: j
INTEGER :: k
INTEGER :: depth
INTEGER :: x_max
INTEGER :: x_min
INTEGER :: y_max
INTEGER :: y_min
REAL(kind=8), DIMENSION((x_min - 2):(x_max + 3),(y_min - 2):(y_max + 2)) :: vol_flux_x
DO k = y_min - depth, y_max + depth
DO j = 1, depth
vol_flux_x(1 - j,k) = -vol_flux_x(1 + j,k)
END DO
END DO
END SUBROUTINE 

END MODULE update_halo_kernel_loop63_mod

