SUBROUTINE mg_loop11(u, n1, n2, n3)
INTEGER :: n1
INTEGER :: n2
INTEGER :: n3
DOUBLE PRECISION, DIMENSION(n1,n2,n3) :: u
DO i3 = 2, n3 - 1
DO i2 = 2, n2 - 1
u(1,i2,i3) = u(n1 - 1,i2,i3)
u(n1,i2,i3) = u(2,i2,i3)
END DO
END DO
END SUBROUTINE 
