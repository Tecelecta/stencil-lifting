SUBROUTINE WW_SPLIT(wwE, wwI,                         &
                    u, v, ww,                         &
                    mut, rdnw,                        &
                    c1f, c2f,                         &
                    its, ite, jts, jte, kts, kte )
                     
   IMPLICIT NONE
   INTEGER :: its, ite, jts, jte, kts, kte

   REAL, DIMENSION( its:ite, kts:kte , jts:jte ) ::  ww ! IN
   REAL, DIMENSION( its:ite+1, kts-1:kte , jts:jte   ) ::  u
   REAL, DIMENSION( its:ite  , kts-1:kte , jts:jte+1 ) ::  v

   REAL, DIMENSION( its:ite, jts:jte ) :: mut ! IN 

   REAL, DIMENSION( kts:kte ) :: rdnw  ! IN 
   REAL, DIMENSION( kts:kte ) :: c1f, c2f ! IN

   REAL, DIMENSION( its:ite, kts:kte , jts:jte ) ::  wwE, wwI ! OUT

   REAL    :: cr, cx, cy, cw_max, cw_max2, cw_min, cw, cff, wfrac
   INTEGER         :: i, j, k

   
   REAL :: alpha_max, alpha_min, Ceps
   REAL :: dt, rdx, rdy
   REAL :: cmnx_ratio, cutoff, r4cmx

   dt  = 0.5
   rdx = 0.5
   rdy = 0.5
   alpha_max  = 1.1
   alpha_min  = 0.9
   Ceps       = 0.9

   cmnx_ratio = alpha_min/alpha_max
   cutoff     = 2.0 - cmnx_ratio
   r4cmx      = 1.0/(4.0 - 4.0*cmnx_ratio )

     ! LOOP 3 MARK 
     DO j = jts, jte
       DO k = kts+1, kte-1
         DO i = its, ite
            cx = 0.25*rdx*(u(i+1,k,j  ) + u(i,k,j) + u(i+1,k-1,j  ) + u(i,k-1,j)) 
            cy = 0.25*rdy*(v(i,  k,j+1) + v(i,k,j) + v(i  ,k-1,j+1) + v(i,k-1,j)) 
            cw_max = max(alpha_max - dt*Ceps*sqrt(cx**2 + cy**2),0.0)       
            IF( cw_max > 0.0 ) THEN
              cr     = ww(i,k,j) * dt * rdnw(k) / (c1f(k)*mut(i,j)+c2f(k))   
                  

              cw_max2 = cw_max**2
              cw_min  = cw_max*cmnx_ratio
              cw      = abs(cr)
              cff     = 0.
        
              if ( cw < cw_min ) then
                cff = cw_max2
              elseif ( cw < cutoff*cw_min ) then
                cff = cw_max2 + r4cmx*(cw-cw_min)**2
              else
                cff = cw_max*cw
              endif
          
              wfrac = cw_max2 / cff
              wfrac = max(min(wfrac, 1.0), 0.0)

              wwE(i,k,j) = ww(i,k,j) * wfrac
              wwI(i,k,j) = ww(i,k,j) * (1.0 - wfrac)     
            ELSE  
              wwE(i,k,j) = 0.0
              wwI(i,k,j) = ww(i,k,j)
            ENDIF

         ENDDO
       ENDDO
     ENDDO

RETURN
END SUBROUTINE WW_SPLIT