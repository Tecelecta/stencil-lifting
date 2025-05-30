MODULE accelerate_kernel_loop85_mod
CONTAINS
SUBROUTINE accelerate_kernel_loop85(j,k,stepbymass,viscosity,x_max,x_min,xarea,xvel1,y_max,y_min)
INTEGER :: j
INTEGER :: k
INTEGER :: x_max
INTEGER :: x_min
INTEGER :: y_max
INTEGER :: y_min
REAL(kind=8), DIMENSION((x_min - 2):(x_max + 3),(y_min - 2):(y_max + 3)) :: stepbymass
REAL(kind=8), DIMENSION((x_min - 2):(x_max + 2),(y_min - 2):(y_max + 2)) :: viscosity
REAL(kind=8), DIMENSION((x_min - 2):(x_max + 3),(y_min - 2):(y_max + 2)) :: xarea
REAL(kind=8), DIMENSION((x_min - 2):(x_max + 3),(y_min - 2):(y_max + 3)) :: xvel1
DO k = y_min, y_max + 1
DO j = x_min, x_max + 1
xvel1(j,k) = xvel1(j,k) - stepbymass(j,k) * (xarea(j,k) * (viscosity(j,k) - viscosity(j - 1,k)) + xarea(j,k - 1) * (viscosity(j,k - 1) - viscosity(j - 1,k - 1)))
END DO
END DO
END SUBROUTINE 

END MODULE accelerate_kernel_loop85_mod

