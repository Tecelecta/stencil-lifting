SUBROUTINE advec_cell_kernel_loop96(advec_vol,density1,ener_flux,energy1,mass_flux_y,&
    post_ener,post_mass,pre_mass,pre_vol,vol_flux_y,x_max,x_min,y_max,y_min)
    INTEGER :: j
    INTEGER :: k
    INTEGER :: x_max
    INTEGER :: x_min
    INTEGER :: y_max
    INTEGER :: y_min
    REAL(kind=8), DIMENSION((0 - 2):(100 + 3),(0 - 2):(100 + 3)) :: advec_vol ! output4
    REAL(kind=8), DIMENSION((0 - 2):(100 + 2),(0 - 2):(100 + 2)) :: density1 ! output5
    REAL(kind=8), DIMENSION((0 - 2):(100 + 3),(0 - 2):(100 + 3)) :: ener_flux
    REAL(kind=8), DIMENSION((0 - 2):(100 + 2),(0 - 2):(100 + 2)) :: energy1 ! output6
    REAL(kind=8), DIMENSION((0 - 2):(100 + 2),(0 - 2):(100 + 3)) :: mass_flux_y
    REAL(kind=8), DIMENSION((0 - 2):(100 + 3),(0 - 2):(100 + 3)) :: post_ener ! output3
    REAL(kind=8), DIMENSION((0 - 2):(100 + 3),(0 - 2):(100 + 3)) :: post_mass ! output2
    REAL(kind=8), DIMENSION((0 - 2):(100 + 3),(0 - 2):(100 + 3)) :: pre_mass ! output1
    REAL(kind=8), DIMENSION((0 - 2):(100 + 3),(0 - 2):(100 + 3)) :: pre_vol
    REAL(kind=8), DIMENSION((0 - 2):(100 + 2),(0 - 2):(100 + 3)) :: vol_flux_y
    DO k = 0, 100
       DO j = 0, 100
          pre_mass(j,k) = density1(j,k) * pre_vol(j,k)
          post_mass(j,k) = pre_mass(j,k) + mass_flux_y(j,k) - mass_flux_y(j,k + 1)
          post_ener(j,k) = (energy1(j,k) * pre_mass(j,k) + ener_flux(j,k) - ener_flux(j,k + 1)) / post_mass(j,k)
          advec_vol(j,k) = pre_vol(j,k) + vol_flux_y(j,k) - vol_flux_y(j,k + 1)
          density1(j,k) = post_mass(j,k) / advec_vol(j,k)
          energy1(j,k) = post_ener(j,k)
       END DO
    END DO
 END SUBROUTINE