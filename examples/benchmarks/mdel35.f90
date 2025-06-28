SUBROUTINE cal_helicity ( hatavg, hat, fnm, fnp, cf1, cf2, cf3, &
                          cft1, cft2, &
                          jts, jte, kts, kte, its, ite )

INTEGER :: jts, jte, kts, kte, its, ite
REAL(kind=8) :: cf1, cf2, cf3, cft1, cft2
REAL(kind=8), DIMENSION( kts:kte ):: fnm, fnp
REAL(kind=8), DIMENSION( its-3:ite+2, kts:kte, jts-3:jte+2 ) :: hat                                
REAL(kind=8), DIMENSION( its:ite, kts:kte, jts:jte ) :: hatavg                                

DO j = jts, jte
    DO k = kts, kte
        DO i = its-1, ite
            hat(i,k,j) = 1.0  
        END DO
    END DO
END DO


DO j = jts, jte
    DO k = kts+1, kte
        DO i = its, ite
        hatavg(i,k,j) = 0.5 * (  &
                        fnm(k) * ( hat(i-1,k  ,j) + hat(i,k  ,j) ) +  &
                        fnp(k) * ( hat(i-1,k-1,j) + hat(i,k-1,j) ) )
        END DO
    END DO
END DO

DO j = jts, jte
    DO i = its, ite
        hatavg(i,1,j)   = 0.5 * (  &
                          cf1 * hat(i-1,1,j) +  &
                          cf2 * hat(i-1,2,j) +  &
                          cf3 * hat(i-1,3,j) +  &
                          cf1 * hat(i  ,1,j) +  &
                          cf2 * hat(i  ,2,j) +  &
                          cf3 * hat(i  ,3,j) )
        hatavg(i,kte,j) = 0.5 * (  &
                          cft1 * ( hat(i,kte,j) + hat(i-1,kte,j) ) +  &
                          cft2 * ( hat(i,kte-1,j) + hat(i-1,kte-1,j) ) )
    END DO
END DO

END SUBROUTINE