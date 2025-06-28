SUBROUTINE cal_deform_and_div( &
    tmp1, zx, hatavg, rdzw, &
    jts, jte, kts, kte, its, ite )

    INTEGER :: its, ite, jts, jte, kts, kte
    REAL(kind=8) tmpzx
     
    REAL(kind=8), DIMENSION( its-2:ite+2, kts:kte, jts-2:jte+2 ) :: zx, hatavg
    REAL(kind=8), DIMENSION( its:ite, kts:kte, jts:jte ) :: tmp1, rdzw

    DO j = jts, jte
        DO k = kts, kte
            DO i = its, ite
                tmpzx       = 0.25 * ( &
                            zx(i,k  ,j) + zx(i+1,k  ,j) +  &
                            zx(i,k+1,j) + zx(i+1,k+1,j) )
                tmp1(i,k,j) = ( hatavg(i,k+1,j) - hatavg(i,k,j) ) * tmpzx * rdzw(i,k,j)
                ! tmp1 to here = partial dpsi/dx * partial du^/dpsi:
            END DO
        END DO
    END DO
END SUBROUTINE 