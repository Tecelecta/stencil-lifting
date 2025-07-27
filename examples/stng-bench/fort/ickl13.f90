SUBROUTINE initialise_chunk_kernel_loop13(k,celldy,dy,y_max,y_min)
    INTEGER :: k
    REAL(kind=8) :: dy
    INTEGER :: y_max
    INTEGER :: y_min
    REAL(kind=8), DIMENSION((y_min - 2):(y_max + 2)) :: celldy
    DO k = y_min - 2, y_max + 2
        celldy(k) = dy
    END DO
END SUBROUTINE 

