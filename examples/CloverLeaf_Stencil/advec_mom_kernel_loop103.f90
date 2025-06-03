MODULE advec_mom_kernel_loop103_mod
CONTAINS
SUBROUTINE advec_mom_kernel_loop103(j,k,node_flux,node_mass_post,node_mass_pre,x_max,x_min,y_max,y_min)
INTEGER :: j
INTEGER :: k
INTEGER :: x_max
INTEGER :: x_min
INTEGER :: y_max
INTEGER :: y_min
REAL(kind=8), DIMENSION((x_min - 2):(x_max + 3),(y_min - 2):(y_max + 3)) :: node_flux
REAL(kind=8), DIMENSION((x_min - 2):(x_max + 3),(y_min - 2):(y_max + 3)) :: node_mass_post
REAL(kind=8), DIMENSION((x_min - 2):(x_max + 3),(y_min - 2):(y_max + 3)) :: node_mass_pre
DO k = y_min, y_max + 1
DO j = x_min - 1, x_max + 2
        ! Stagered cell mass pre advection
node_mass_pre(j,k) = node_mass_post(j,k) - node_flux(j - 1,k) + node_flux(j,k)
END DO
END DO
END SUBROUTINE 

END MODULE advec_mom_kernel_loop103_mod

