MODULE mg_loop5_mod
CONTAINS
SUBROUTINE mg_loop5(i1,i2,i3,c,m,n1,n2,n3,r1,r2,r,u)
INTEGER :: i1
INTEGER :: i2
INTEGER :: i3
INTEGER :: m
INTEGER :: n1
INTEGER :: n2
INTEGER :: n3
DOUBLE PRECISION, DIMENSION(0:3) :: c
DOUBLE PRECISION, DIMENSION(100) :: r1
DOUBLE PRECISION, DIMENSION(100) :: r2
DOUBLE PRECISION, DIMENSION(100,100,100) :: r
DOUBLE PRECISION, DIMENSION(100,100,100) :: u
DO i3 = 2, 100 - 1
DO i2 = 2, 100 - 1
DO i1 = 1, 100
r1(i1) = r(i1,i2 - 1,i3) + r(i1,i2 + 1,i3) + r(i1,i2,i3 - 1) + r(i1,i2,i3 + 1)
r2(i1) = r(i1,i2 - 1,i3 - 1) + r(i1,i2 + 1,i3 - 1) + r(i1,i2 - 1,i3 + 1) + r(i1,i2 + 1,i3 + 1)
END DO
DO i1 = 2, 100 - 1
u(i1,i2,i3) = u(i1,i2,i3) + c(0) * r(i1,i2,i3) + c(1) * (r(i1 - 1,i2,i3) + r(i1 + 1,i2,i3) + r1(i1)) + c(2) * (r2(i1) + r1(i1 - 1) + r1(i1 + 1))
!---------------------------------------------------------------------
!  Assume c(3) = 0    (Enable line below if c(3) not= 0)
!---------------------------------------------------------------------
!    >                     + c(3) * ( r2(i1-1) + r2(i1+1) )
!---------------------------------------------------------------------
END DO
END DO
END DO
END SUBROUTINE 

END MODULE mg_loop5_mod

