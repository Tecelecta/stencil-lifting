MODULE reset_field_kernel_loop111_mod
CONTAINS
SUBROUTINE reset_field_kernel_loop111(j,k,x_max,x_min,xvel0,xvel1,y_max,y_min)
INTEGER :: j
INTEGER :: k
INTEGER :: x_max
INTEGER :: x_min
INTEGER :: y_max
INTEGER :: y_min
REAL(kind=8), DIMENSION((x_min - 2):(x_max + 3),(y_min - 2):(y_max + 3)) :: xvel0
REAL(kind=8), DIMENSION((x_min - 2):(x_max + 3),(y_min - 2):(y_max + 3)) :: xvel1
DO k = y_min, y_max + 1
DO j = x_min, x_max + 1
xvel0(j,k) = xvel1(j,k)
END DO
END DO
END SUBROUTINE 

END MODULE reset_field_kernel_loop111_mod

