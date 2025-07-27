SUBROUTINE advec_cell_kernel_loop93(j, k, advec_vol, density1, ener_flux, energy1, &
    mass_flux_x, mass_flux_y, post_ener, post_mass, pre_mass, pre_vol, &
    vol_flux_x, vol_flux_y, x_max, x_min, y_max, y_min)
  IMPLICIT NONE
  
  INTEGER, INTENT(IN) :: j, k
  INTEGER, INTENT(IN) :: x_max, x_min, y_max, y_min
  
  REAL(kind=8), DIMENSION((x_min - 2):(x_max + 3),(y_min - 2):(y_max + 3)), INTENT(INOUT) :: advec_vol
  REAL(kind=8), DIMENSION((x_min - 2):(x_max + 2),(y_min - 2):(y_max + 2)), INTENT(INOUT) :: density1
  REAL(kind=8), DIMENSION((x_min - 2):(x_max + 3),(y_min - 2):(y_max + 3)), INTENT(IN) :: ener_flux
  REAL(kind=8), DIMENSION((x_min - 2):(x_max + 2),(y_min - 2):(y_max + 2)), INTENT(INOUT) :: energy1
  REAL(kind=8), DIMENSION((x_min - 2):(x_max + 3),(y_min - 2):(y_max + 2)), INTENT(IN) :: mass_flux_x
  REAL(kind=8), DIMENSION((x_min - 2):(x_max + 2),(y_min - 2):(y_max + 3)), INTENT(IN) :: mass_flux_y
  REAL(kind=8), DIMENSION((x_min - 2):(x_max + 3),(y_min - 2):(y_max + 3)), INTENT(INOUT) :: post_ener
  REAL(kind=8), DIMENSION((x_min - 2):(x_max + 3),(y_min - 2):(y_max + 3)), INTENT(INOUT) :: post_mass
  REAL(kind=8), DIMENSION((x_min - 2):(x_max + 3),(y_min - 2):(y_max + 3)), INTENT(INOUT) :: pre_mass
  REAL(kind=8), DIMENSION((x_min - 2):(x_max + 3),(y_min - 2):(y_max + 3)), INTENT(INOUT) :: pre_vol
  REAL(kind=8), DIMENSION((x_min - 2):(x_max + 3),(y_min - 2):(y_max + 2)), INTENT(IN) :: vol_flux_x
  REAL(kind=8), DIMENSION((x_min - 2):(x_max + 2),(y_min - 2):(y_max + 3)), INTENT(IN) :: vol_flux_y
  
  INTEGER :: local_j, local_k
  
  DO local_k = y_min, y_max
    DO local_j = x_min, x_max
      pre_mass(local_j, local_k) = density1(local_j, local_k) * pre_vol(local_j, local_k)
      post_mass(local_j, local_k) = pre_mass(local_j, local_k) + mass_flux_y(local_j, local_k) - mass_flux_y(local_j, local_k+1)
      post_ener(local_j, local_k) = (energy1(local_j, local_k) * pre_mass(local_j, local_k) + &
                                    ener_flux(local_j, local_k) - ener_flux(local_j, local_k+1)) / post_mass(local_j, local_k)
      advec_vol(local_j, local_k) = pre_vol(local_j, local_k) + vol_flux_y(local_j, local_k) - vol_flux_y(local_j, local_k+1)
      density1(local_j, local_k) = post_mass(local_j, local_k) / advec_vol(local_j, local_k)
      energy1(local_j, local_k) = post_ener(local_j, local_k)
    ENDDO
  ENDDO
END SUBROUTINE advec_cell_kernel_loop93 

