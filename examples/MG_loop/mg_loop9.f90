MODULE mg_loop9_mod
CONTAINS
SUBROUTINE mg_loop9(i1,i2,i3,d1,d2,d3,mm1,mm2,mm3,n1,n2,n3,t1,t2,u,z)
INTEGER :: i1
INTEGER :: i2
INTEGER :: i3
INTEGER :: d1
INTEGER :: d2
INTEGER :: d3
INTEGER :: mm1
INTEGER :: mm2
INTEGER :: mm3
INTEGER :: n1
INTEGER :: n2
INTEGER :: n3
INTEGER :: t1
INTEGER :: t2
DOUBLE PRECISION, DIMENSION(100,100,100) :: u
DOUBLE PRECISION, DIMENSION(100,100,100) :: z
DO i3 = 1, 100 - 1
DO i2 = 1, 100 - 1
DO i1 = 1, 100 - 1
u(2 * i1 - d1,2 * i2 - d2,2 * i3 - d3) = u(2 * i1 - d1,2 * i2 - d2,2 * i3 - d3) + z(i1,i2,i3)
END DO
DO i1 = 1, 100 - 1
u(2 * i1 - t1,2 * i2 - d2,2 * i3 - d3) = u(2 * i1 - t1,2 * i2 - d2,2 * i3 - d3) + 0.5D0 * (z(i1 + 1,i2,i3) + z(i1,i2,i3))
END DO
END DO
DO i2 = 1, 100 - 1
DO i1 = d1, 100 - 1
u(2 * i1 - d1,2 * i2 - t2,2 * i3 - d3) = u(2 * i1 - d1,2 * i2 - t2,2 * i3 - d3) + 0.5D0 * (z(i1,i2 + 1,i3) + z(i1,i2,i3))
END DO
DO i1 = 1, 100 - 1
u(2 * i1 - t1,2 * i2 - t2,2 * i3 - d3) = u(2 * i1 - t1,2 * i2 - t2,2 * i3 - d3) + 0.25D0 * (z(i1 + 1,i2 + 1,i3) + z(i1 + 1,i2,i3) + z(i1,i2 + 1,i3) + z(i1,i2,i3))
END DO
END DO
END DO
END SUBROUTINE 

END MODULE mg_loop9_mod

