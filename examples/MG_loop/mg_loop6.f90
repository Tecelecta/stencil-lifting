MODULE mg_loop6_mod
CONTAINS
SUBROUTINE mg_loop6(i1,i2,i3,a,m,n1,n2,n3,r,u1,u2,u,v)
INTEGER :: i1
INTEGER :: i2
INTEGER :: i3
INTEGER :: m
INTEGER :: n1
INTEGER :: n2
INTEGER :: n3
DOUBLE PRECISION, DIMENSION(0:3) :: a
DOUBLE PRECISION, DIMENSION(100,100,100) :: r
DOUBLE PRECISION, DIMENSION(100) :: u1
DOUBLE PRECISION, DIMENSION(100) :: u2
DOUBLE PRECISION, DIMENSION(100,100,100) :: u
DOUBLE PRECISION, DIMENSION(100,100,100) :: v
DO i3 = 2, 100 - 1
DO i2 = 2, 100 - 1
DO i1 = 1, 100
u1(i1) = u(i1,i2 - 1,i3) + u(i1,i2 + 1,i3) + u(i1,i2,i3 - 1) + u(i1,i2,i3 + 1)
u2(i1) = u(i1,i2 - 1,i3 - 1) + u(i1,i2 + 1,i3 - 1) + u(i1,i2 - 1,i3 + 1) + u(i1,i2 + 1,i3 + 1)
END DO
DO i1 = 2, 100 - 1
r(i1,i2,i3) = v(i1,i2,i3) - a(0) * u(i1,i2,i3) - a(2) * (u2(i1) + u1(i1 - 1) + u1(i1 + 1)) - a(3) * (u2(i1 - 1) + u2(i1 + 1))
!---------------------------------------------------------------------
!  Assume a(1) = 0      (Enable 2 lines below if a(1) not= 0)
!---------------------------------------------------------------------
!    >                     - a(1) * ( u(i1-1,i2,i3) + u(i1+1,i2,i3)
!    >                              + u1(i1) )
!---------------------------------------------------------------------
END DO
END DO
END DO
END SUBROUTINE 

END MODULE mg_loop6_mod

