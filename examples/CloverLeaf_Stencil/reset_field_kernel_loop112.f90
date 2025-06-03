MODULE reset_field_kernel_loop112_mod
CONTAINS
SUBROUTINE reset_field_kernel_loop112(j,k,x_max,x_min,y_max,y_min,yvel0,yvel1)
INTEGER :: j
INTEGER :: k
INTEGER :: x_max
INTEGER :: x_min
INTEGER :: y_max
INTEGER :: y_min
REAL(kind=8), DIMENSION((x_min - 2):(x_max + 3),(y_min - 2):(y_max + 3)) :: yvel0
REAL(kind=8), DIMENSION((x_min - 2):(x_max + 3),(y_min - 2):(y_max + 3)) :: yvel1
DO k = y_min, y_max + 1
DO j = x_min, x_max + 1
yvel0(j,k) = yvel1(j,k)
END DO
END DO
END SUBROUTINE 

END MODULE reset_field_kernel_loop112_mod

