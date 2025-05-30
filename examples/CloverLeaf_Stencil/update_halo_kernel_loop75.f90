MODULE update_halo_kernel_loop75_mod
CONTAINS
SUBROUTINE update_halo_kernel_loop75(j,k,depth,mass_flux_y,x_max,x_min,y_max,y_min)
INTEGER :: j
INTEGER :: k
INTEGER :: depth
INTEGER :: x_max
INTEGER :: x_min
INTEGER :: y_max
INTEGER :: y_min
REAL(kind=8), DIMENSION((x_min - 2):(x_max + 2),(y_min - 2):(y_max + 3)) :: mass_flux_y
DO k = y_min - depth, y_max + 1 + depth
DO j = 1, depth
mass_flux_y(1 - j,k) = mass_flux_y(1 + j,k)
END DO
END DO
END SUBROUTINE 

END MODULE update_halo_kernel_loop75_mod

