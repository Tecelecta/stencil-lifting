SUBROUTINE cal_helicity (wavg, w, jts, jte, kts, kte, its, ite)

    INTEGER :: jts, jte, kts, kte, its, ite, i, j, k
    REAL(kind=8), DIMENSION( its-1:ite, kts:kte+1, jts-1:jte ) :: w
    REAL(kind=8), DIMENSION( its:ite, kts:kte, jts:jte ) :: wavg
 
    DO j = jts, jte
        DO k = kts, kte
            DO i = its, ite
                wavg(i,k,j) = 0.125_8 * (  &
                    w(i,k  ,j  ) + w(i-1,k  ,j  ) +  &
                    w(i,k  ,j-1) + w(i-1,k  ,j-1) +  &
                    w(i,k+1,j  ) + w(i-1,k+1,j  ) +  &
                    w(i,k+1,j-1) + w(i-1,k+1,j-1) )
            END DO
        END DO
    END DO
 END SUBROUTINE