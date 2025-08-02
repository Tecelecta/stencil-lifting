SUBROUTINE tke_km( xkmh, xkhh, &
  rdzw, def2, zx, zy,           &
  msftx, msfty,                &
  its, ite, jts, jte, kts, kte )
  implicit none

  INTEGER :: its, ite, jts, jte, kts, kte
  REAL(kind=8), DIMENSION( its:ite, jts:jte) :: msftx, msfty ! IN 1p*2
  REAL(kind=8), DIMENSION( its:ite+1, kts:kte+1, jts:jte+1 ) :: zx,zy ! IN 4p*2
  REAL(kind=8), DIMENSION( its:ite  , kts:kte  , jts:jte   ) :: rdzw ! IN 1p
  REAL(kind=8), DIMENSION( its:ite  , kts:kte  , jts:jte   ) ::  def2 ! IN 1p
  REAL(kind=8), DIMENSION( its:ite  , kts:kte  , jts:jte   ) ::  xkmh, xkhh !out*2

  ! param
  REAL(kind=8) :: c_s = 0.25_8
  REAL(kind=8) :: pr = 0.125_8
  REAL(kind=8) :: dx = 0.8_8
  REAL(kind=8) :: dy = 0.8_8
  
  ! local vars
  INTEGER :: i,k,j
  REAL(kind=8) :: dxm, dym, tmpzx, tmpzy, alpha, def_limit, mlen_h, tmp

  DO j = jts, jte
    DO k = kts, kte
      DO i = its, ite
        mlen_h = sqrt(dx/msftx(i,j) * dy/msfty(i,j))
        tmp    = sqrt(def2(i,k,j))
        xkmh(i,k,j) = c_s*c_s*mlen_h*mlen_h*tmp
        xkmh(i,k,j) = min(xkmh(i,k,j), 10.*mlen_h )
        xkhh(i,k,j) = xkmh(i,k,j)/pr

        dxm=dx/msftx(i,j)
        dym=dy/msfty(i,j)
        tmpzx = (0.25*( abs(zx(i,k,j))+ abs(zx(i+1,k,j  )) + abs(zx(i,k+1,j))+ abs(zx(i+1,k+1,j  )))*rdzw(i,k,j)*dxm)
        tmpzy = (0.25*( abs(zy(i,k,j))+ abs(zy(i  ,k,j+1)) + abs(zy(i,k+1,j))+ abs(zy(i  ,k+1,j+1)))*rdzw(i,k,j)*dym)
        alpha = max(sqrt(tmpzx*tmpzx+tmpzy*tmpzy),1.0)
        def_limit = max(10./mlen_h,1.e-3)
        IF ( tmp .GT. def_limit ) THEN
          xkmh(i,k,j)=xkmh(i,k,j)/(alpha*alpha)
        ELSE
          xkmh(i,k,j)=xkmh(i,k,j)/(alpha)
        ENDIF
        xkhh(i,k,j)=xkmh(i,k,j)/pr
      ENDDO
    ENDDO
  ENDDO
END SUBROUTINE 