
SUBROUTINE cal_helicity (tmp1,hatavg,zx,rdzw,n1,n2,n3)

    INTEGER :: i,j,k,n1,n2,n3
REAL(kind=8), DIMENSION( 100, 100, 100 ), INTENT( IN ) :: zx, rdzw !需要初始化
REAL(kind=8), DIMENSION(100, 100, 100 ) ::  hatavg        !需要初始化

REAL(kind=8), DIMENSION( 100, 100, 100 ) ::  tmp1
REAL(kind=8) :: tmpzx

    DO j = 1, 100
        DO k = 1, 100
        DO i = 1, 100
          tmpzx       = 0.25 * (  &
                        zx(i,k  ,j-1) + zx(i,k  ,j) +  &
                        zx(i,k+1,j-1) + zx(i,k+1,j) )
          tmp1(i,k,j) = ( hatavg(i,k+1,j) - hatavg(i,k,j) ) *  &
                        0.25 * tmpzx * ( rdzw(i,k,j) + rdzw(i,k,j-1) + &
                                         rdzw(i-1,k,j-1) + rdzw(i-1,k,j) )
        END DO
        END DO
        END DO
END SUBROUTINE
    