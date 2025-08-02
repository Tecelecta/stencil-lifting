SUBROUTINE advec_mom_kernel_loop98(post_vol,pre_vol,vol_flux_x,vol_flux_y,volume,x_max,x_min,y_max,y_min)
    INTEGER :: j
    INTEGER :: k
    INTEGER :: x_max
    INTEGER :: x_min
    INTEGER :: y_max
    INTEGER :: y_min
    REAL(kind=8), DIMENSION((x_min - 2):(x_max + 3),(y_min - 2):(y_max + 3)) :: post_vol
    REAL(kind=8), DIMENSION((x_min - 2):(x_max + 3),(y_min - 2):(y_max + 3)) :: pre_vol
    REAL(kind=8), DIMENSION((x_min - 2):(x_max + 3),(y_min - 2):(y_max + 2)) :: vol_flux_x
    REAL(kind=8), DIMENSION((x_min - 2):(x_max + 2),(y_min - 2):(y_max + 3)) :: vol_flux_y
    REAL(kind=8), DIMENSION((x_min - 2):(x_max + 2),(y_min - 2):(y_max + 2)) :: volume
    DO k = y_min - 2, y_max + 2
       DO j = x_min - 2, x_max + 2
          post_vol(j,k) = volume(j,k) + vol_flux_x(j + 1,k) - vol_flux_x(j,k)
          pre_vol(j,k) = post_vol(j,k) + vol_flux_y(j,k + 1) - vol_flux_y(j,k)
       END DO
    END DO
 END SUBROUTINE