MODULE update_halo_kernel_loop70_mod
CONTAINS
SUBROUTINE update_halo_kernel_loop70(j,k,depth,vol_flux_y,x_max,x_min,y_max,y_min)
INTEGER :: j
INTEGER :: k
INTEGER :: depth
INTEGER :: x_max
INTEGER :: x_min
INTEGER :: y_max
INTEGER :: y_min
REAL(kind=8), DIMENSION((x_min - 2):(x_max + 2),(y_min - 2):(y_max + 3)) :: vol_flux_y
DO j = x_min - depth, x_max + depth
DO k = 1, depth
vol_flux_y(j,y_max + k + 1) = -vol_flux_y(j,y_max + 1 - k)
END DO
END DO
END SUBROUTINE 

END MODULE update_halo_kernel_loop70_mod

