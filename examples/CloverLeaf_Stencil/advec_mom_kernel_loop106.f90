MODULE advec_mom_kernel_loop106_mod
CONTAINS
SUBROUTINE advec_mom_kernel_loop106(j,k,density1,node_mass_post,post_vol,x_max,x_min,y_max,y_min)
INTEGER :: j
INTEGER :: k
INTEGER :: x_max
INTEGER :: x_min
INTEGER :: y_max
INTEGER :: y_min
REAL(kind=8), DIMENSION((x_min - 2):(x_max + 2),(y_min - 2):(y_max + 2)) :: density1
REAL(kind=8), DIMENSION((x_min - 2):(x_max + 3),(y_min - 2):(y_max + 3)) :: node_mass_post
REAL(kind=8), DIMENSION((x_min - 2):(x_max + 3),(y_min - 2):(y_max + 3)) :: post_vol
DO k = y_min - 1, y_max + 2
DO j = x_min, x_max + 1
node_mass_post(j,k) = 0.25_8 * (density1(j,k - 1) * post_vol(j,k - 1) + density1(j,k) * post_vol(j,k) + density1(j - 1,k - 1) * post_vol(j - 1,k - 1) + density1(j - 1,k) * post_vol(j - 1,k))
END DO
END DO
END SUBROUTINE 

END MODULE advec_mom_kernel_loop106_mod

