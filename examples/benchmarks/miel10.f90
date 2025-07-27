SUBROUTINE advect_ph_implicit( rt, &
                               pho, tendency, phb,        &
                               wwI,           &
                               c1, c2,                        &
                               mut,             &
                               msfty,                  &
                               rdzw,                &
                               its, ite, jts, jte, kts, kte  )

   IMPLICIT NONE

   
   INTEGER :: its, ite, jts, jte, kts, kte


   REAL , DIMENSION( its:ite , kts-1:kte+1, jts:jte ) :: pho, phb ! IN
   REAL , DIMENSION( its:ite , kts:kte , jts:jte ) :: tendency, wwI! IN

   REAL , DIMENSION( its:ite , jts:jte ) :: msfty, mut ! IN

   REAL , DIMENSION( kts-1:kte ) :: rdzw ! IN
   REAL , DIMENSION( kts:kte ) :: c1, c2 ! IN

   REAL :: dt_rk = 0.0375

   REAL, DIMENSION(its:ite, kts:kte, jts:jte) :: rt
   
   INTEGER :: i, j, k
   REAL    :: wiC

   REAL, DIMENSION(its:ite,kts:kte) :: at, ct
   REAL, DIMENSION(its:ite)         :: btmp
   


   DO j = jts, jte
     DO k = kts, kte
       DO i = its, ite
         wiC     = 0.5*wwI(i,k,j)*(rdzw(k-1)+rdzw(k)) * msfty(i,j) / (c1(k)*mut(i,j)+c2(k))
         at(i,k) = - dt_rk*max(wiC,0.0)
         ct(i,k) =   dt_rk*min(wiC,0.0)
         btmp(i) =   - at(i,k) - ct(i,k)
         rt(i,k,j) = tendency(i,k,j) * dt_rk * msfty(i,j) / (c1(k)*mut(i,j)+c2(k))      &          
                 - at(i,k)*pho(i,k-1,j) -     btmp(i)*pho(i,k,j) - ct(i,k)*pho(i,k+1,j) &
                 - at(i,k)*phb(i,k-1,j) -     btmp(i)*phb(i,k,j) - ct(i,k)*phb(i,k+1,j)  
       ENDDO
     ENDDO
   ENDDO
    
   
RETURN
END SUBROUTINE advect_ph_implicit