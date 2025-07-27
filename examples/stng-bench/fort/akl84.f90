SUBROUTINE accelerate_kernel_loop84(pressure,stepbymass,x_max,x_min,y_max,y_min,yarea,yvel0,yvel1)
INTEGER :: j
INTEGER :: k
INTEGER :: x_max
INTEGER :: x_min
INTEGER :: y_max
INTEGER :: y_min
REAL(kind=8), DIMENSION((x_min - 2):(x_max + 2),(y_min - 2):(y_max + 2)) :: pressure
REAL(kind=8), DIMENSION((x_min - 2):(x_max + 3),(y_min - 2):(y_max + 3)) :: stepbymass
REAL(kind=8), DIMENSION((x_min - 2):(x_max + 2),(y_min - 2):(y_max + 3)) :: yarea
REAL(kind=8), DIMENSION((x_min - 2):(x_max + 3),(y_min - 2):(y_max + 3)) :: yvel0
REAL(kind=8), DIMENSION((x_min - 2):(x_max + 3),(y_min - 2):(y_max + 3)) :: yvel1
DO k = y_min, y_max + 1
DO j = x_min, x_max + 1
yvel1(j,k) = yvel0(j,k) - stepbymass(j,k) * (yarea(j,k) * (pressure(j,k) - pressure(j,k - 1)) + yarea(j - 1,k) * (pressure(j - 1,k) - pressure(j - 1,k - 1)))
END DO
END DO
END SUBROUTINE 

