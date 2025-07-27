SUBROUTINE advect_v_implicit( rt, v_old, tendency,            &
                               rom,                   &
                              c1, c2,                        &
                              muv_old, muv_new,         &
                              msfvy, msfvx, &
                              rdzw,                &
                              its, ite, jts, jte, kts, kte  )

   IMPLICIT NONE
   
! Input data
   
   INTEGER :: its, ite, jts, jte, kts, kte

   REAL , DIMENSION( its:ite , kts-1:kte+1, jts-1:jte ) :: rom ! IN
   REAL , DIMENSION( its:ite , kts  :kte  , jts:jte ) :: v_old, tendency
!-------------------------------------------------------------------------------
! LWJ: definitions of various column masses 
! mut    ==> current column mass from sub-step
! mu_old ==> "n" tite level column mass
! mu_new ==> "n+1*" estimated column needed for dynamical variables where we dont
!            have tite-avg column mass.  For scalars (not theta) mut_new == mut

   REAL , DIMENSION( its:ite , jts:jte ) :: muv_old, muv_new ! IN

!-------------------------------------------------------------------------------

   REAL , DIMENSION( its:ite , jts:jte ) :: msfvy, msfvx
   REAL , DIMENSION( kts:kte ) :: rdzw, c1, c2

   
   REAL, DIMENSION(its:ite, kts:kte, jts:jte) :: rt

   REAL :: dt_rk = 0.375
   ! Local data
   
   INTEGER :: i, j, k

   REAL    :: wiL, wiR
   
   REAL, DIMENSION(its:ite,kts:kte) :: at, bt, ct
   REAL, DIMENSION(its:ite)         :: btmp
      
   INTEGER :: kp1, km1

! Vertical loop to do implicit advection.....
   ! LOOP 12 MARK
   DO j = jts, jte
   
     DO k = kts, kte

       km1 = MAX(kts, k-1)
       kp1 = MIN(kte, k+1)

       DO i = its, ite

         wiL   = 0.5*(rom(i,k,  j-1)+rom(i,k,  j)) * rdzw(k) * msfvy(i,j) / (c1(k)*muv_new(i,j)+c2(k)) 
         wiR   = 0.5*(rom(i,k+1,j-1)+rom(i,k+1,j)) * rdzw(k) * msfvy(i,j) / (c1(k)*muv_new(i,j)+c2(k))
       
         at(i,k) = - dt_rk*max(wiL,0.0)
         ct(i,k) =   dt_rk*min(wiR,0.0)
         btmp(i) =   dt_rk*(max(wiR,0.0) - min(wiL,0.0))
         bt(i,k) = 1.0 + btmp(i)
         rt(i,k,j) = dt_rk*tendency(i,k,j) * msfvx(i,j) &
                   - (c1(k)*muv_old(i,j)+c2(k))*(at(i,k)*v_old(i,km1,j) + btmp(i)*v_old(i,k,j) + ct(i,k)*v_old(i,kp1,j))
       
       ENDDO
     ENDDO
   ENDDO ! J-LOOP
    
RETURN
END SUBROUTINE advect_v_implicit