SUBROUTINE cal_deform_and_div( hatavg, hat, fnm, fnp, &
   jts, jte, kts, kte, its, ite)
 
    INTEGER ::  its, ite, kts, kte, jts, jte, i, j, k
    REAL(kind=8), DIMENSION( its-2:ite+2, kts:kte, jts-2:jte+2 ) :: hat, hatavg
    REAL(kind=8), DIMENSION(kts:kte) :: fnm, fnp
 
    DO j=jts,jte
       DO k=kts+1,kte
          DO i=its,ite
             hatavg(i,k,j) = 0.5 *  &
                ( fnm(k) * ( hat(i,k  ,j) + hat(i,  k,j+1) ) +  &
                  fnp(k) * ( hat(i,k-1,j) + hat(i,k-1,j+1) ) )
          END DO
       END DO
    END DO
 END SUBROUTINE