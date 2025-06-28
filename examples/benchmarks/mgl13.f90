SUBROUTINE mg_loop13(u,n1,n2,n3)
INTEGER :: i1
INTEGER :: i2
INTEGER :: n1
INTEGER :: n2
INTEGER :: n3
DOUBLE PRECISION, DIMENSION(n1,n2,n3) :: u
DO i2 = 1, n2
DO i1 = 1, n1
u(i1,i2,1) = u(i1,i2,n3 - 1)
u(i1,i2,n3) = u(i1,i2,2)
END DO
END DO
END SUBROUTINE 

