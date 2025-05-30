MODULE mg_loop3_mod
CONTAINS
SUBROUTINE mg_loop3(ax,k,lt,m1,m2,m3,maxlevel,mi,ng)
INTEGER :: ax
INTEGER :: k
INTEGER :: lt
INTEGER :: maxlevel
INTEGER, DIMENSION(100) :: m1
INTEGER, DIMENSION(100) :: m2
INTEGER, DIMENSION(100) :: m3
INTEGER, DIMENSION(3,10) :: mi
INTEGER, DIMENSION(3,10) :: ng
DO k = 0, 1, -1
DO ax = 1, 3
mi(ax,k) = 2 + ng(ax,k)
END DO
m1(k) = mi(1,k)
m2(k) = mi(2,k)
m3(k) = mi(3,k)
END DO
END SUBROUTINE 

END MODULE mg_loop3_mod

