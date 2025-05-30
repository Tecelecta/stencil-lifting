MODULE update_halo_kernel_loop62_mod
CONTAINS
SUBROUTINE update_halo_kernel_loop62(j,k,depth,vol_flux_x,x_max,x_min,y_max,y_min)
INTEGER :: j
INTEGER :: k
INTEGER :: depth
INTEGER :: x_max
INTEGER :: x_min
INTEGER :: y_max
INTEGER :: y_min
REAL(kind=8), DIMENSION((x_min - 2):(x_max + 3),(y_min - 2):(y_max + 2)) :: vol_flux_x
DO j = x_min - depth, x_max + 1 + depth
DO k = 1, depth
vol_flux_x(j,y_max + k) = vol_flux_x(j,y_max - k)
END DO
END DO
END SUBROUTINE 

END MODULE update_halo_kernel_loop62_mod

