SUBROUTINE reset_field_kernel_loop109(density0,density1,x_max,x_min,y_max,y_min)
INTEGER :: j
INTEGER :: k
INTEGER :: x_max
INTEGER :: x_min
INTEGER :: y_max
INTEGER :: y_min
REAL(kind=8), DIMENSION((x_min - 2):(x_max + 2),(y_min - 2):(y_max + 2)) :: density0
REAL(kind=8), DIMENSION((x_min - 2):(x_max + 2),(y_min - 2):(y_max + 2)) :: density1
DO k = y_min, y_max
DO j = x_min, x_max
density0(j,k) = density1(j,k)
END DO
END DO
END SUBROUTINE 
