SUBROUTINE trilinear_proj_rprj3(r, m1k, m2k, m3k, s, m1j, m2j, m3j, m)
! m1k >= 2* m1j, m2k >= 2* m2j, m2k >= 2* m2j

   INTEGER :: m1k, m2k, m3k, m1j, m2j, m3j, m
   REAL(kind=8) :: x2, y2
   REAL(kind=8), DIMENSION(m1k,m2k,m3k) :: r
   REAL(kind=8), DIMENSION(m1j,m2j,m3j) :: s
   REAL(kind=8), DIMENSION(m) :: x1, y1
   INTEGER :: j3, j2, j1, i3, i2, i1, d1, d2, d3

   if(m1k.eq.3)then
      d1 = 2
   else
      d1 = 1
   endif

   if(m2k.eq.3)then
      d2 = 2
   else
      d2 = 1
   endif

   if(m3k.eq.3)then
      d3 = 2
   else
      d3 = 1
   endif

   do  j3=2,m3j-1
      i3 = 2*j3-d3
      do  j2=2,m2j-1
         i2 = 2*j2-d2
         do j1=2,m1j
            i1 = 2*j1-d1
            x1(i1-1) = r(i1-1,i2-1,i3  ) + r(i1-1,i2+1,i3  )&
               + r(i1-1,i2,  i3-1) + r(i1-1,i2,  i3+1)
            y1(i1-1) = r(i1-1,i2-1,i3-1) + r(i1-1,i2-1,i3+1)&
               + r(i1-1,i2+1,i3-1) + r(i1-1,i2+1,i3+1)
         enddo
         do  j1=2,m1j-1
            i1 = 2*j1-d1
            y2 = r(i1,  i2-1,i3-1) + r(i1,  i2-1,i3+1)&
               + r(i1,  i2+1,i3-1) + r(i1,  i2+1,i3+1)
            x2 = r(i1,  i2-1,i3  ) + r(i1,  i2+1,i3  )&
               + r(i1,  i2,  i3-1) + r(i1,  i2,  i3+1)

            s(j1,j2,j3) =&
               0.5D0 * r(i1,i2,i3)&
               + 0.25D0 * ( r(i1-1,i2,i3) + r(i1+1,i2,i3) + x2)&
               + 0.125D0 * ( x1(i1-1) + x1(i1+1) + y2)&
               + 0.0625D0 * ( y1(i1-1) + y1(i1+1) )
         enddo
      enddo
   enddo
END SUBROUTINE