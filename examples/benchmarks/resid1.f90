SUBROUTINE resid(r, u, v, u1, u2, n1, n2, n3)
   implicit none
   INTEGER :: n1,n2,n3
   REAL(kind=8), DIMENSION(n1) :: u1, u2
   REAL(kind=8), DIMENSION(n1,n2,n3) :: u, r, v
   INTEGER :: i3, i2, i1

   do i3=2,n3-1
      do i2=2,n2-1
         do i1=1,n1
            u1(i1) = u(i1,i2-1,i3) + u(i1,i2+1,i3)&
               + u(i1,i2,i3-1) + u(i1,i2,i3+1)
            u2(i1) = u(i1,i2-1,i3-1) + u(i1,i2+1,i3-1)&
               + u(i1,i2-1,i3+1) + u(i1,i2+1,i3+1)
         enddo
         do i1=2,n1-1
            r(i1,i2,i3) = v(i1,i2,i3)&
               - (-1.4) * u(i1,i2,i3)&
               - (1.26) * ( u2(i1) + u1(i1-1) + u1(i1+1) )&
               - (-1.53) * ( u2(i1-1) + u2(i1+1) )
         enddo
      enddo
   enddo

END SUBROUTINE