SUBROUTINE stencil(a, b, x_min, x_max, y_min, y_max)
    IMPLICIT NONE

    INTEGER :: x_min, x_max, y_min, y_max
    REAL(KIND=8), DIMENSION(x_min:x_max) :: a1,a2
    REAL(KIND=8), DIMENSION(x_min:x_max) :: b

    INTEGER :: i, beg, e
    
    ! DO i = 1, 10, 3
    !     b(i) = a1(i) + a2(i)
    !     b(i+1) = a1(i+1) + a2(i+1)
    !     b(i+2) = a1(i+2) + a2(i+2)
    ! ENDDO

    DO ti = 0, 12, 4
        DO i = 1,4
            b(i) = a(ti+i) + a(ti+i-1)
        ENDDO
    ENDDO

END SUBROUTINE