SUBROUTINE mg_loop6(r, u, v, a, u1, u2, n1, n2, n3)
INTEGER :: n1
INTEGER :: n2
INTEGER :: n3
DOUBLE PRECISION, DIMENSION(0:3) :: a
DOUBLE PRECISION, DIMENSION(n1,n2,n3) :: r
DOUBLE PRECISION, DIMENSION(n1) :: u1
DOUBLE PRECISION, DIMENSION(n1) :: u2
DOUBLE PRECISION, DIMENSION(n1,n2,n3) :: u
DOUBLE PRECISION, DIMENSION(n1,n2,n3) :: v
DO i3 = 2, n3 - 1
DO i2 = 2, n2 - 1
DO i1 = 1, n1
u1(i1) = u(i1, i2-1, i3  ) + &
         u(i1, i2+1, i3  ) + &
         u(i1, i2  , i3-1) + &
         u(i1, i2  , i3+1)

u2(i1) = u(i1, i2-1, i3-1) + &
         u(i1, i2+1, i3-1) + &
         u(i1, i2-1, i3+1) + &
         u(i1, i2+1, i3+1)
END DO
DO i1 = 2, n1 - 1
r(i1,i2,i3) = v(i1,i2,i3) - &
             a(0) * u(i1,i2,i3) - & 
             a(2) * (u2(i1) + u1(i1 - 1) + u1(i1 + 1)) - &
             a(3) * (u2(i1 - 1) + u2(i1 + 1))
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


