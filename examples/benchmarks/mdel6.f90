SUBROUTINE cal_deform_and_div(      &
    tmp1, hat, mm,                  &
    rdx,                            &
    jts, jte, kts, kte, its, ite   )
    
    INTEGER :: its, ite, jts, jte, kts, kte

    REAL(kind=8) rdx
    
    REAL(kind=8), DIMENSION( its:ite, kts:kte, jts:jte ) :: tmp1
    REAL(kind=8), DIMENSION( its-2:ite+2, kts:kte, jts-2:jte+2 ) :: hat
    REAL(kind=8), DIMENSION( its:ite, jts:jte ) :: mm

    DO j = jts, jte
        DO k = kts, kte
            DO i = its, ite
                tmp1(i,k,j) = mm(i,j) * ( rdx * ( hat(i+1,k,j) - hat(i,k,j) ) -  &
                              tmp1(i,k,j))
            END DO
        END DO
    END DO
  
END SUBROUTINE 