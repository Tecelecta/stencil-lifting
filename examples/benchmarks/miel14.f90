SUBROUTINE advect_v_implicit( rt, w_old, tendency,            &
                               rom,                   &
                              c1, c2,                        &
                              mut_old, mut_new,         &
                              msfty, rdzu, &
                              its, ite, jts, jte, kts, kte  )

   IMPLICIT NONE
   
! Input data
   
   INTEGER :: its, ite, jts, jte, kts, kte

   REAL , DIMENSION( its:ite , kts-1:kte+1, jts:jte ) :: rom, w_old! IN
   REAL , DIMENSION( its:ite , kts  :kte  , jts:jte ) :: tendency
!-------------------------------------------------------------------------------
! LWJ: definitions of various column masses 
! mut    ==> current column mass from sub-step
! mu_old ==> "n" tite level column mass
! mu_new ==> "n+1*" estimated column needed for dynamical variables where we dont
!            have tite-avg column mass.  For scalars (not theta) mut_new == mut

   REAL , DIMENSION( its:ite , jts:jte ) , INTENT(IN) :: mut_old, mut_new

!-------------------------------------------------------------------------------

   REAL , DIMENSION( its:ite , jts:jte ) :: msfty
   REAL , DIMENSION( kts:kte ) :: rdzu, c1, c2

   
   REAL, DIMENSION(its:ite, kts:kte, jts:jte) :: rt

   REAL :: dt_rk = 0.375
   ! Local data
   
   INTEGER :: i, j, k

   REAL    :: wiL, wiR

   REAL, DIMENSION(its:ite,kts:kte) :: at, bt, ct
   REAL, DIMENSION(its:ite)         :: btmp
      
! Vertical loop to do implicit advection.....
   ! LOOP 12 MARK
   DO j = jts, jte
   
     DO k = kts, kte

       DO i = its, ite

! SO IMPORTANT to * 1/DZ HERE CAUSE IT CHANGES Wi* SIGN TO BE CORRECT FOR UPWINDING!!!!
         wiL   = 0.5*(rom(i,k-1,j)+rom(i,k,j)) * rdzu(k) * msfty(i,j) / (c1(k)*mut_new(i,j)+c2(k)) 
         wiR   = 0.5*(rom(i,k+1,j)+rom(i,k,j)) * rdzu(k) * msfty(i,j) / (c1(k)*mut_new(i,j)+c2(k))

         at(i,k) = - dt_rk*max(wiL,0.0)
         ct(i,k) =   dt_rk*min(wiR,0.0) 
         btmp(i) =   dt_rk*(max(wiR,0.0) - min(wiL,0.0)) 
         bt(i,k) =   1.0 + btmp(i)
         rt(i,k,j) = dt_rk*tendency(i,k,j) * msfty(i,j)  &
                 - (c1(k)*mut_old(i,j)+c2(k))*(at(i,k)*w_old(i,k-1,j) + btmp(i)*w_old(i,k,j) + ct(i,k)*w_old(i,k+1,j))

       ENDDO
     ENDDO
   ENDDO ! J-LOOP
    
RETURN
END SUBROUTINE advect_v_implicit