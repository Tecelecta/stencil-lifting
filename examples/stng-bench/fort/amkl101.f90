SUBROUTINE advec_mom_kernel_loop101(j,k,mass_flux_x,node_flux,x_max,x_min,y_max,y_min)
    INTEGER :: j
    INTEGER :: k
    INTEGER :: x_max
    INTEGER :: x_min
    INTEGER :: y_max
    INTEGER :: y_min
    REAL(kind=8), DIMENSION((x_min - 2):(x_max + 3),(y_min - 2):(y_max + 2)) :: mass_flux_x
    REAL(kind=8), DIMENSION((x_min - 2):(x_max + 3),(y_min - 2):(y_max + 3)) :: node_flux
    DO k = y_min, y_max + 1
       DO j = x_min - 2, x_max + 2
          ! Find staggered mesh mass fluxes, nodal masses and volumes.
          node_flux(j,k) = 0.25_8 * (mass_flux_x(j,k - 1) + mass_flux_x(j,k) + mass_flux_x(j + 1,k - 1) + mass_flux_x(j + 1,k))
       END DO
    END DO
 END SUBROUTINE