MODULE mg_loop1_mod
CONTAINS
SUBROUTINE mg_loop1(ax,k,lt,ng)
INTEGER :: ax
INTEGER :: k
INTEGER :: lt
INTEGER, DIMENSION(3,10) :: ng
DO ax = 1, 3
DO k = 0 - 1, 1, -1
ng(ax,k) = ng(ax,k + 1) / 2
END DO
END DO
END SUBROUTINE 

END MODULE mg_loop1_mod

