MODULE advec_mom_kernel_loop108_mod
CONTAINS
SUBROUTINE advec_mom_kernel_loop108(j,k,mom_flux,node_mass_post,node_mass_pre,vel1,x_max,x_min,y_max,y_min)
INTEGER :: j
INTEGER :: k
INTEGER :: x_max
INTEGER :: x_min
INTEGER :: y_max
INTEGER :: y_min
REAL(kind=8), DIMENSION((x_min - 2):(x_max + 3),(y_min - 2):(y_max + 3)) :: mom_flux
REAL(kind=8), DIMENSION((x_min - 2):(x_max + 3),(y_min - 2):(y_max + 3)) :: node_mass_post
REAL(kind=8), DIMENSION((x_min - 2):(x_max + 3),(y_min - 2):(y_max + 3)) :: node_mass_pre
REAL(kind=8), POINTER, DIMENSION(:,:) :: vel1
DO k = y_min, y_max + 1
DO j = x_min, x_max + 1
vel1(j,k) = (vel1(j,k) * node_mass_pre(j,k) + mom_flux(j,k - 1) - mom_flux(j,k)) / node_mass_post(j,k)
END DO
END DO
END SUBROUTINE 

END MODULE advec_mom_kernel_loop108_mod

