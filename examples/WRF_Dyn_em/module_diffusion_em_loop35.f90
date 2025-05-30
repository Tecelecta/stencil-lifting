SUBROUTINE cal_helicity ()

    INTEGER :: i,j,k,n1,n2,n3
    REAL(kind=8), DIMENSION(100,100,100) :: hatavg, hat
    REAL(kind=8), DIMENSION(100) :: fnm, fnp

DO j = 1, 100
    DO k = 1+1, 100
    DO i = 1, 100
      hatavg(i,k,j) = 0.5 * (  &
                      fnm(k) * ( hat(i-1,k  ,j) + hat(i,k  ,j) ) +  &
                      fnp(k) * ( hat(i-1,k-1,j) + hat(i,k-1,j) ) )
    END DO
    END DO
    END DO

DO j = 1, 100
    DO i = 1, 100
       hatavg(i,1,j)   =  0.5 * (  &
                        !   cf1 * hat(i-1,1,j) +  &
                        !   cf2 * hat(i-1,2,j) +  &
                        !   cf3 * hat(i-1,3,j) +  &
                        !   cf1 * hat(i  ,1,j) +  &
                        !   cf2 * hat(i  ,2,j) +  &
                        !   cf3 * hat(i  ,3,j) )
                          1 * hat(i-1,1,j) +  &
                          1 * hat(i-1,2,j) +  &
                          1 * hat(i-1,3,j) +  &
                          1 * hat(i  ,1,j) +  &
                          1 * hat(i  ,2,j) +  &
                          1 * hat(i  ,3,j) )
       hatavg(i,2,j) =  0.5 * (  &
                        !   cft1 * ( hat(i,ktes1,j) + hat(i-1,ktes1,j) ) +  &
                        !   cft2 * ( hat(i,ktes2,j) + hat(i-1,ktes2,j) ) )
                            1 * ( hat(i,1,j) + hat(i-1,1,j) ) +  &
                          1 * ( hat(i,1,j) + hat(i-1,1,j) ) )
    END DO
    END DO
END SUBROUTINE