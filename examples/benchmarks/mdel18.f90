SUBROUTINE cal_deform_and_div( mm, msfux, msfvy, jts, jte, its, ite )

    INTEGER :: jts, jte, its, ite, i, j
    REAL(kind=8), DIMENSION( its:ite, jts-1:jte ) :: msfux
    REAL(kind=8), DIMENSION( its-1:ite , jts:jte ) :: msfvy
    REAL(kind=8), DIMENSION( its:ite, jts:jte ) :: mm
 
    DO j = jts, jte
       DO i = its, ite
          mm(i,j) = 0.25 * ( msfux(i,j-1) + msfux(i,j) ) * ( msfvy(i-1,j) + msfvy(i,j) )
       END DO
    END DO
 END SUBROUTINE