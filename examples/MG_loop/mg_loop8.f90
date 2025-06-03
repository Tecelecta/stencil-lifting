MODULE mg_loop8_mod
CONTAINS
SUBROUTINE mg_loop8(i1,i2,i3,m,mm1,mm2,mm3,n1,n2,n3,u,z1,z2,z3,z)
INTEGER :: i1
INTEGER :: i2
INTEGER :: i3
INTEGER :: m
INTEGER :: mm1
INTEGER :: mm2
INTEGER :: mm3
INTEGER :: n1
INTEGER :: n2
INTEGER :: n3
DOUBLE PRECISION, DIMENSION(100,100,100) :: u
DOUBLE PRECISION, DIMENSION(100) :: z1
DOUBLE PRECISION, DIMENSION(100) :: z2
DOUBLE PRECISION, DIMENSION(100) :: z3
DOUBLE PRECISION, DIMENSION(100,100,100) :: z
DO i3 = 1, 100 - 1
DO i2 = 1, 100 - 1
DO i1 = 1, 100
z1(i1) = z(i1,i2 + 1,i3) + z(i1,i2,i3)
z2(i1) = z(i1,i2,i3 + 1) + z(i1,i2,i3)
z3(i1) = z(i1,i2 + 1,i3 + 1) + z(i1,i2,i3 + 1) + z1(i1)
END DO
DO i1 = 1, 100 - 1
u(2 * i1 - 1,2 * i2 - 1,2 * i3 - 1) = u(2 * i1 - 1,2 * i2 - 1,2 * i3 - 1) + z(i1,i2,i3)
u(2 * i1,2 * i2 - 1,2 * i3 - 1) = u(2 * i1,2 * i2 - 1,2 * i3 - 1) + 0.5d0 * (z(i1 + 1,i2,i3) + z(i1,i2,i3))
END DO
DO i1 = 1, 100 - 1
u(2 * i1 - 1,2 * i2,2 * i3 - 1) = u(2 * i1 - 1,2 * i2,2 * i3 - 1) + 0.5d0 * z1(i1)
u(2 * i1,2 * i2,2 * i3 - 1) = u(2 * i1,2 * i2,2 * i3 - 1) + 0.25d0 * (z1(i1) + z1(i1 + 1))
END DO
DO i1 = 1, 100 - 1
u(2 * i1 - 1,2 * i2 - 1,2 * i3) = u(2 * i1 - 1,2 * i2 - 1,2 * i3) + 0.5d0 * z2(i1)
u(2 * i1,2 * i2 - 1,2 * i3) = u(2 * i1,2 * i2 - 1,2 * i3) + 0.25d0 * (z2(i1) + z2(i1 + 1))
END DO
DO i1 = 1, 100 - 1
u(2 * i1 - 1,2 * i2,2 * i3) = u(2 * i1 - 1,2 * i2,2 * i3) + 0.25d0 * z3(i1)
u(2 * i1,2 * i2,2 * i3) = u(2 * i1,2 * i2,2 * i3) + 0.125d0 * (z3(i1) + z3(i1 + 1))
END DO
END DO
END DO
END SUBROUTINE 

END MODULE mg_loop8_mod

