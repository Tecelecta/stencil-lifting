SUBROUTINE cal_deform_and_div( tmp1, w, rdzw, jts, jte, kts, kte, its, ite )

    INTEGER :: its, ite, jts, jte, kts, kte, i, j, k
    REAL(kind=8), DIMENSION( its:ite, kts:kte, jts:jte ) :: w, rdzw
    REAL(kind=8), DIMENSION( jts:jte, kts:kte, jts:jte ) :: tmp1 
 
    DO j = jts, jte
       DO k = kts, kte
          DO i = its, ite
             tmp1(i,k,j) = ( w(i,k+1,j) - w(i,k,j) ) * rdzw(i,k,j)
          END DO
       END DO
    END DO
 
    ! End calculation of dw/dz.
 END SUBROUTINE