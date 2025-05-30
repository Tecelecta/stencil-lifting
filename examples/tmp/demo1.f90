SUBROUTINE stencil(a, b, x_min, x_max, y_min, y_max)
    IMPLICIT NONE

    INTEGER :: x_min, x_max, y_min, y_max
    REAL(KIND=8), DIMENSION(x_min-1:x_max, y_min:y_max) :: a
    REAL(KIND=8), DIMENSION(x_min:x_max, y_min:y_max) :: b

    INTEGER :: i, j
    REAL(KIND=8) :: t 
    
    DO j = y_min, y_max
        t = a(x_min-1, j)
        DO i = x_min, x_max
            b(i,j) = t + a(i,j)
            t = a(i,j)
        ENDDO
    ENDDO   

END SUBROUTINE