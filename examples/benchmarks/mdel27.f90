SUBROUTINE meso_length_scale(dthrdn, &
  rdzw, rdz, p8w,t8w,theta,           &
  its, ite, jts, jte, kts, kte )

  IMPLICIT NONE
! The mesoscale length scale based on Nakanishi and Niino (2009)
! and modified by X. Zhang DO j = jts, jte
  INTEGER :: its, ite, jts, jte, kts, kte

  REAL(kind=8), DIMENSION( its:ite, kts:kte, jts:jte ) :: rdzw, rdz, & ! 2p*2
                                                          p8w, t8w, & ! 1p*2
                                                          theta !2p

  ! selected param
  REAL(kind=8) :: r_d          = 287.04     ! gas constant for dry air (J deg^-1 kg^-1)
  REAL(kind=8) :: cp           = 1004.6     ! Specific heat of dry air at constant pressure (J deg^-1 kg^-1)
  REAL(kind=8) :: p1000mb      = 100000.    ! pressure at 1000 hPa (pa)
  
  REAL(kind=8), DIMENSION( its:ite, kts:kte, jts:jte )  :: dthrdn !out

  ! local vars
  INTEGER :: i, k, j
  REAL(kind=8) :: tmpdz, thetasfc, thetatop

  DO j = jts, jte  !2518 to 
  DO k = kts+1, kte-1
  DO i = its, ite
    tmpdz         = 1.0 / rdz(i,k+1,j) + 1.0 / rdz(i,k,j)
    dthrdn(i,k,j) = ( theta(i,k+1,j) - theta(i,k-1,j) ) / tmpdz
  END DO
  END DO
  END DO

  k = kts
  DO j = jts, jte
  DO i = its, ite
    tmpdz         = 1.0 / rdzw(i,k+1,j) + 1.0 / rdzw(i,k,j)
    thetasfc      = t8w(i,kts,j) / ( p8w(i,k,j) / p1000mb )**( r_d / cp )
    dthrdn(i,k,j) = ( theta(i,k+1,j) - thetasfc ) / tmpdz
  END DO
  END DO

  k = kte
  DO j = jts, jte
  DO i = its, ite
    tmpdz         = 1.0 / rdz(i,k,j) + 0.5 / rdzw(i,k,j)
    thetatop      = t8w(i,kte,j) / ( p8w(i,kte,j) / p1000mb )**( r_d / cp )
    dthrdn(i,k,j) = ( thetatop - theta(i,k-1,j) ) / tmpdz
  END DO
  END DO

END SUBROUTINE