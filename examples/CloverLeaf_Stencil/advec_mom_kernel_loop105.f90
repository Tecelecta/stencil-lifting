MODULE advec_mom_kernel_loop105_mod
CONTAINS
SUBROUTINE advec_mom_kernel_loop105(j,k,mass_flux_y,node_flux,x_max,x_min,y_max,y_min)
INTEGER :: j
INTEGER :: k
INTEGER :: x_max
INTEGER :: x_min
INTEGER :: y_max
INTEGER :: y_min
REAL(kind=8), DIMENSION((x_min - 2):(x_max + 2),(y_min - 2):(y_max + 3)) :: mass_flux_y
REAL(kind=8), DIMENSION((x_min - 2):(x_max + 3),(y_min - 2):(y_max + 3)) :: node_flux
DO k = y_min - 2, y_max + 2
DO j = x_min, x_max + 1
        ! Find staggered mesh mass fluxes and nodal masses and volumes.
node_flux(j,k) = 0.25_8 * (mass_flux_y(j - 1,k) + mass_flux_y(j,k) + mass_flux_y(j - 1,k + 1) + mass_flux_y(j,k + 1))
END DO
END DO
END SUBROUTINE 

END MODULE advec_mom_kernel_loop105_mod

