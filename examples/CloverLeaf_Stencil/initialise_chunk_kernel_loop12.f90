MODULE initialise_chunk_kernel_loop12_mod
CONTAINS
SUBROUTINE initialise_chunk_kernel_loop12(k,celly,vertexy,y_max,y_min)
INTEGER :: k
INTEGER :: y_max
INTEGER :: y_min
REAL(kind=8), DIMENSION((y_min - 2):(y_max + 2)) :: celly
REAL(kind=8), DIMENSION((y_min - 2):(y_max + 3)) :: vertexy
DO k = y_min - 2, y_max + 2
celly(k) = 0.5 * (vertexy(k) + vertexy(k + 1))
END DO
END SUBROUTINE 

END MODULE initialise_chunk_kernel_loop12_mod

