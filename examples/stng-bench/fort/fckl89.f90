SUBROUTINE flux_calc_kernel_loop89(dt,vol_flux_x,x_max,x_min,xarea,xvel0,xvel1,y_max,y_min)
INTEGER :: j
INTEGER :: k
REAL(kind=8) :: dt
INTEGER :: x_max
INTEGER :: x_min
INTEGER :: y_max
INTEGER :: y_min
REAL(kind=8), DIMENSION((x_min - 2):(x_max + 3),(y_min - 2):(y_max + 2)) :: vol_flux_x
REAL(kind=8), DIMENSION((x_min - 2):(x_max + 3),(y_min - 2):(y_max + 2)) :: xarea
REAL(kind=8), DIMENSION((x_min - 2):(x_max + 3),(y_min - 2):(y_max + 3)) :: xvel0
REAL(kind=8), DIMENSION((x_min - 2):(x_max + 3),(y_min - 2):(y_max + 3)) :: xvel1
DO k = y_min, y_max
DO j = x_min, x_max + 1
vol_flux_x(j,k) = 0.25_8 * dt * xarea(j,k) * (xvel0(j,k) + xvel0(j,k + 1) + xvel1(j,k) + xvel1(j,k + 1))
END DO
END DO
END SUBROUTINE 
