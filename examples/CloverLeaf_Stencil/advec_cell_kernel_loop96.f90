MODULE advec_cell_kernel_loop96_mod
CONTAINS
SUBROUTINE advec_cell_kernel_loop96(j,k,advec_vol,density1,ener_flux,energy1,mass_flux_y,post_ener,post_mass,pre_mass,pre_vol,vol_flux_y,x_max,x_min,y_max,y_min)
INTEGER :: j
INTEGER :: k
INTEGER :: x_max
INTEGER :: x_min
INTEGER :: y_max
INTEGER :: y_min
REAL(kind=8), DIMENSION((x_min - 2):(x_max + 3),(y_min - 2):(y_max + 3)) :: advec_vol
REAL(kind=8), DIMENSION((x_min - 2):(x_max + 2),(y_min - 2):(y_max + 2)) :: density1
REAL(kind=8), DIMENSION((x_min - 2):(x_max + 3),(y_min - 2):(y_max + 3)) :: ener_flux
REAL(kind=8), DIMENSION((x_min - 2):(x_max + 2),(y_min - 2):(y_max + 2)) :: energy1
REAL(kind=8), DIMENSION((x_min - 2):(x_max + 2),(y_min - 2):(y_max + 3)) :: mass_flux_y
REAL(kind=8), DIMENSION((x_min - 2):(x_max + 3),(y_min - 2):(y_max + 3)) :: post_ener
REAL(kind=8), DIMENSION((x_min - 2):(x_max + 3),(y_min - 2):(y_max + 3)) :: post_mass
REAL(kind=8), DIMENSION((x_min - 2):(x_max + 3),(y_min - 2):(y_max + 3)) :: pre_mass
REAL(kind=8), DIMENSION((x_min - 2):(x_max + 3),(y_min - 2):(y_max + 3)) :: pre_vol
REAL(kind=8), DIMENSION((x_min - 2):(x_max + 2),(y_min - 2):(y_max + 3)) :: vol_flux_y
DO k = y_min, y_max
DO j = x_min, x_max
pre_mass(j,k) = density1(j,k) * pre_vol(j,k)
post_mass(j,k) = pre_mass(j,k) + mass_flux_y(j,k) - mass_flux_y(j,k + 1)
post_ener(j,k) = (energy1(j,k) * pre_mass(j,k) + ener_flux(j,k) - ener_flux(j,k + 1)) / post_mass(j,k)
advec_vol(j,k) = pre_vol(j,k) + vol_flux_y(j,k) - vol_flux_y(j,k + 1)
density1(j,k) = post_mass(j,k) / advec_vol(j,k)
energy1(j,k) = post_ener(j,k)
END DO
END DO
END SUBROUTINE 

END MODULE advec_cell_kernel_loop96_mod

