SUBROUTINE cal_deform_and_div(tmp1, hat, mm, rdx, &
   jts, jte, kts, kte, its, ite)

   INTEGER :: jts, jte, kts, kte, its, ite, i, j, k
   REAL(kind=8) :: rdx
   REAL(kind=8), DIMENSION( its:ite, kts:kte, jts:jte ) :: tmp1
   REAL(kind=8), DIMENSION( its-2:ite+2, kts:kte, jts-2:jte+2 ) :: hat
   REAL(kind=8), DIMENSION( its:ite, jts:jte ) :: mm
   DO j = jts, jte
      DO k = kts, kte
         DO i = its, ite
            tmp1(i,k,j) = mm(i,j) * ( rdx * ( hat(i,k,j+1) - hat(i,k,j) ) -  &
               tmp1(i,k,j))
         END DO
      END DO
   END DO
 
 END SUBROUTINE