SUBROUTINE flux_calc_kernel_loop90(j,k,dt,vol_flux_y,x_max,x_min,y_max,y_min,yarea,yvel0,yvel1)
    INTEGER :: j
    INTEGER :: k
    REAL(kind=8) :: dt
    INTEGER :: x_max
    INTEGER :: x_min
    INTEGER :: y_max
    INTEGER :: y_min
    REAL(kind=8), DIMENSION((x_min - 2):(x_max + 2),(y_min - 2):(y_max + 3)) :: vol_flux_y
    REAL(kind=8), DIMENSION((x_min - 2):(x_max + 2),(y_min - 2):(y_max + 3)) :: yarea
    REAL(kind=8), DIMENSION((x_min - 2):(x_max + 3),(y_min - 2):(y_max + 3)) :: yvel0
    REAL(kind=8), DIMENSION((x_min - 2):(x_max + 3),(y_min - 2):(y_max + 3)) :: yvel1
    DO k = y_min, y_max + 1
        DO j = x_min, x_max
            vol_flux_y(j,k) = 0.25_8 * dt * yarea(j,k) * &
            (yvel0(j,k) + yvel0(j + 1,k) + yvel1(j,k) + yvel1(j + 1,k))
        END DO
    END DO
END SUBROUTINE 


