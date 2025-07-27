SUBROUTINE initialise_chunk_kernel_loop15(j,k,celldy,x_max,x_min,xarea,y_max,y_min)
    INTEGER :: j
    INTEGER :: k
    INTEGER :: x_max
    INTEGER :: x_min
    INTEGER :: y_max
    INTEGER :: y_min
    REAL(kind=8), DIMENSION((y_min - 2):(y_max + 2)) :: celldy
    REAL(kind=8), DIMENSION((x_min - 2):(x_max + 3),(y_min - 2):(y_max + 2)) :: xarea
    DO k = y_min - 2, y_max + 2
        DO j = x_min - 2, x_max + 2
            xarea(j,k) = celldy(k)
        END DO
    END DO
END SUBROUTINE 

