SUBROUTINE stencil(a, b, x_min, x_max)
    IMPLICIT NONE

    INTEGER :: x_min, x_max
    REAL(KIND=8), DIMENSION(x_min:x_max) :: a
    REAL(KIND=8), DIMENSION(x_min:x_max+3) :: b

    INTEGER :: i

    DO i = x_min, x_max 
        B(i) = B(i) + A(i)
        B(i+3) = A(i)
    END DO  

END SUBROUTINE