MODULE initialise_chunk_kernel_loop9_mod
CONTAINS
SUBROUTINE initialise_chunk_kernel_loop9(k,dy,vertexdy,y_max,y_min)
INTEGER :: k
REAL(kind=8) :: dy
INTEGER :: y_max
INTEGER :: y_min
REAL(kind=8), DIMENSION((y_min - 2):(y_max + 3)) :: vertexdy
DO k = y_min - 2, y_max + 3
vertexdy(k) = dy
END DO
END SUBROUTINE 

END MODULE initialise_chunk_kernel_loop9_mod

