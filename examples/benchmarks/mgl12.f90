SUBROUTINE mg_loop12(u, n1, n2, n3)
INTEGER :: i1
INTEGER :: i3
INTEGER :: n1
INTEGER :: n2
INTEGER :: n3
DOUBLE PRECISION, DIMENSION(n1,n2,n3) :: u
DO i3 = 2, n3 - 1
DO i1 = 1, n1
u(i1,1,i3) = u(i1,n2 - 1,i3)
u(i1,n2,i3) = u(i1,2,i3)
END DO
END DO
END SUBROUTINE 

