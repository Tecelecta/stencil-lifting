SUBROUTINE advec_cell_kernel_loop95(post_vol,pre_vol,vol_flux_y,volume,x_max,x_min,y_max,y_min)
    INTEGER :: j
    INTEGER :: k
    INTEGER :: x_max
    INTEGER :: x_min
    INTEGER :: y_max
    INTEGER :: y_min
    REAL(kind=8), DIMENSION((0 - 2):(100 + 3),(0 - 2):(100 + 3)) :: post_vol
    REAL(kind=8), DIMENSION((0 - 2):(100 + 3),(0 - 2):(100 + 3)) :: pre_vol
    REAL(kind=8), DIMENSION((0 - 2):(100 + 2),(0 - 2):(100 + 3)) :: vol_flux_y
    REAL(kind=8), DIMENSION((0 - 2):(100 + 2),(0 - 2):(100 + 2)) :: volume
    DO k = 0 - 2, 100 + 2
       DO j = 0 - 2, 100 + 2
          pre_vol(j,k) = volume(j,k) + vol_flux_y(j,k + 1) - vol_flux_y(j,k)
          post_vol(j,k) = volume(j,k)
       END DO
    END DO
 END SUBROUTINE