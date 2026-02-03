SUBROUTINE advect_s_implicit( rt, s_old, tendency,            &
                              rom,                   &
                              c1, c2,                        &
                              mut_old, mut_new,         &
                              rdzw,                &
                              its, ite, jts, jte, kts, kte  )

   IMPLICIT NONE
   

   INTEGER :: its, ite, jts, jte, kts, kte

   REAL , DIMENSION( its:ite , kts:kte+1, jts:jte ) :: rom !IN
   REAL , DIMENSION( its:ite , kts:kte  , jts:jte ) :: s_old


   REAL , DIMENSION( its:ite , jts:jte ) :: mut_old, mut_new

   
   REAL , DIMENSION( its:ite , kts:kte , jts:jte ) , INTENT(INOUT) :: tendency

   REAL , DIMENSION( kts:kte ) :: rdzw, c1, c2

   REAL , DIMENSION( its:ite , kts:kte , jts:jte ) :: rt

   REAL :: dt_rk = 0.375
   
   INTEGER :: i, j, k

   REAL    :: wiL, wiR

   REAL, DIMENSION(its:ite,kts:kte) :: at, bt, ct   
   REAL, DIMENSION(its:ite)         :: btmp
   
   INTEGER :: kp1, km1


   DO j = its, ite    
     DO k = kts, kte
       km1 = MAX(k-1, kts)
       kp1 = MIN(k+1, kte)
       DO i = its, ite       
         wiL   = rom(i,k,  j) * rdzw(k) / (c1(k)*mut_new(i,j)+c2(k))
         wiR   = rom(i,k+1,j) * rdzw(k) / (c1(k)*mut_new(i,j)+c2(k))

         at(i,k) = - dt_rk*max(wiL,0.0)
         ct(i,k) =   dt_rk*min(wiR,0.0)
         btmp(i) =   dt_rk*(max(wiR,0.0) - min(wiL,0.0))
         bt(i,k) = 1.0 + btmp(i)
         rt(i,k,j) = dt_rk*tendency(i,k,j)  &
                   - (c1(k)*mut_old(i,j)+c2(k))*(at(i,k)*s_old(i,km1,j) + &
                   btmp(i)*s_old(i,k,j) + ct(i,k)*s_old(i,kp1,j))
       ENDDO
     ENDDO
   ENDDO  

RETURN
END SUBROUTINE advect_s_implicit