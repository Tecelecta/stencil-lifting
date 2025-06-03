MODULE advec_mom_kernel_loop107_mod
CONTAINS
SUBROUTINE advec_mom_kernel_loop107(j,k,node_flux,node_mass_post,node_mass_pre,x_max,x_min,y_max,y_min)
INTEGER :: j
INTEGER :: k
INTEGER :: x_max
INTEGER :: x_min
INTEGER :: y_max
INTEGER :: y_min
REAL(kind=8), DIMENSION((x_min - 2):(x_max + 3),(y_min - 2):(y_max + 3)) :: node_flux
REAL(kind=8), DIMENSION((x_min - 2):(x_max + 3),(y_min - 2):(y_max + 3)) :: node_mass_post
REAL(kind=8), DIMENSION((x_min - 2):(x_max + 3),(y_min - 2):(y_max + 3)) :: node_mass_pre
DO k = y_min - 1, y_max + 2
DO j = x_min, x_max + 1
node_mass_pre(j,k) = node_mass_post(j,k) - node_flux(j,k - 1) + node_flux(j,k)
END DO
END DO
END SUBROUTINE 

END MODULE advec_mom_kernel_loop107_mod

