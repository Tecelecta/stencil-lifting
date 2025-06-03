MODULE accelerate_kernel_loop83_mod
CONTAINS
SUBROUTINE accelerate_kernel_loop83(j,k,pressure,stepbymass,x_max,x_min,xarea,xvel0,xvel1,y_max,y_min)
INTEGER :: j
INTEGER :: k
INTEGER :: x_max
INTEGER :: x_min
INTEGER :: y_max
INTEGER :: y_min
REAL(kind=8), DIMENSION((x_min - 2):(x_max + 2),(y_min - 2):(y_max + 2)) :: pressure
REAL(kind=8), DIMENSION((x_min - 2):(x_max + 3),(y_min - 2):(y_max + 3)) :: stepbymass
REAL(kind=8), DIMENSION((x_min - 2):(x_max + 3),(y_min - 2):(y_max + 2)) :: xarea
REAL(kind=8), DIMENSION((x_min - 2):(x_max + 3),(y_min - 2):(y_max + 3)) :: xvel0
REAL(kind=8), DIMENSION((x_min - 2):(x_max + 3),(y_min - 2):(y_max + 3)) :: xvel1
DO k = y_min, y_max + 1
DO j = x_min, x_max + 1
xvel1(j,k) = xvel0(j,k) - stepbymass(j,k) * (xarea(j,k) * (pressure(j,k) - pressure(j - 1,k)) + xarea(j,k - 1) * (pressure(j,k - 1) - pressure(j - 1,k - 1)))
END DO
END DO
END SUBROUTINE 

END MODULE accelerate_kernel_loop83_mod

