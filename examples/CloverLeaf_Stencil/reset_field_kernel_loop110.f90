MODULE reset_field_kernel_loop110_mod
CONTAINS
SUBROUTINE reset_field_kernel_loop110(j,k,energy0,energy1,x_max,x_min,y_max,y_min)
INTEGER :: j
INTEGER :: k
INTEGER :: x_max
INTEGER :: x_min
INTEGER :: y_max
INTEGER :: y_min
REAL(kind=8), DIMENSION((x_min - 2):(x_max + 2),(y_min - 2):(y_max + 2)) :: energy0
REAL(kind=8), DIMENSION((x_min - 2):(x_max + 2),(y_min - 2):(y_max + 2)) :: energy1
DO k = y_min, y_max
DO j = x_min, x_max
energy0(j,k) = energy1(j,k)
END DO
END DO
END SUBROUTINE 

END MODULE reset_field_kernel_loop110_mod

