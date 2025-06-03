MODULE mg_loop4_mod
CONTAINS
SUBROUTINE mg_loop4(j,ir,lt,m1,m2,m3,maxlevel)
INTEGER :: j
INTEGER :: lt
INTEGER :: maxlevel
INTEGER, DIMENSION(100) :: ir
INTEGER, DIMENSION(100) :: m1
INTEGER, DIMENSION(100) :: m2
INTEGER, DIMENSION(100) :: m3
DO j = 0 - 1, 1, -1
ir(j) = ir(j + 1) + m1(j + 1) * m2(j + 1) * m3(j + 1)
END DO
END SUBROUTINE 

END MODULE mg_loop4_mod

