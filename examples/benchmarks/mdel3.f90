SUBROUTINE cal_deform_and_div(    &
    hatavg, hat,                  &
    fnm, fnp,                     &
    jts, jte, kts, kte, its, ite)
    
    
    ! Average in x and z.
    INTEGER :: jts, jte, kts, kte, its, ite, i, j, k

    REAL(kind=8), DIMENSION( kts:kte ) :: fnm, fnp
    REAL(kind=8), DIMENSION( its-2:ite+2, kts:kte, jts-2:jte+2 ) :: hat, hatavg

    DO j=jts,jte
    DO k=kts+1,kte
    DO i=its,ite
        hatavg(i,k,j) = 0.5 *  &
                    ( fnm(k) * ( hat(i,k  ,j) + hat(i+1,  k,j) ) +  &
                      fnp(k) * ( hat(i,k-1,j) + hat(i+1,k-1,j) ) )
    END DO
    END DO
    END DO
END SUBROUTINE