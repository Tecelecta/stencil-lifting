SUBROUTINE meso_length_scale(dthrdn,rdzw, rdz, p8w, t8w,theta)
!
! The mesoscale length scale based on Nakanishi and Niino (2009)
! and modified by X. Zhang DO j = j_start, j_end

  REAL(kind=8), DIMENSION( 100, 100, 100 ), INTENT( IN ) :: rdzw, rdz, p8w, t8w
  REAL(kind=8), DIMENSION( 100, 100, 100 ), INTENT( IN ) :: theta
!下面的参数来自 WRF/share/module_model_constants.F，这里选择了其中一组
  REAL(kind=8), PARAMETER :: r_d          = 287.04     ! gas constant for dry air (J deg^-1 kg^-1)
  REAL(kind=8), PARAMETER :: cp           = 1004.6     ! Specific heat of dry air at constant pressure (J deg^-1 kg^-1)
  REAL(kind=8), PARAMETER :: p1000mb      = 100000.    ! pressure at 1000 hPa (pa)
  
integer :: i, j, k
REAL(kind=8), DIMENSION( 100, 100, 100 )  :: dthrdn
REAL(kind=8) :: tmpdz, thetasfc, thetatop


  DO j = 1, 100  !2518 to 展示边界的处理
  DO k = 1+1, 100-1
  DO i = 1, 100
    tmpdz         = 1.0 / rdz(i,k+1,j) + 1.0 / rdz(i,k,j)
    dthrdn(i,k,j) = ( theta(i,k+1,j) - theta(i,k-1,j) ) / tmpdz
  END DO
  END DO
  END DO

  k = 1
  DO j = 1, 100
  DO i = 1, 100
    tmpdz         = 1.0 / rdzw(i,k+1,j) + 1.0 / rdzw(i,k,j)
    thetasfc      = T8w(i,1,j) / ( p8w(i,k,j) / p1000mb )**( R_d / Cp )
    dthrdn(i,k,j) = ( theta(i,k+1,j) - thetasfc ) / tmpdz
  END DO
  END DO
  k = 2
  DO j = 1, 100
  DO i = 1, 100
    tmpdz         = 1.0 / rdz(i,k,j) + 0.5 / rdzw(i,k,j)
    thetatop      = T8w(i,kde,j) / ( p8w(i,kde,j) / p1000mb )**( R_d / Cp )
    dthrdn(i,k,j) = ( thetatop - theta(i,k-1,j) ) / tmpdz
  END DO
  END DO

  end SUBROUTINE