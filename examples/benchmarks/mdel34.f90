! partial function for nonlocal heat flux
FUNCTION pthnl(d,h)
   IMPLICIT NONE
   REAL(kind=8) :: pthnl, pthnl_1, pthnl_2
   REAL(kind=8) :: pmin = 0.0,pmax = 1.0
   REAL(kind=8) :: a1 = 1.000, a2 = 0.936, a3 = -1.110,         &
                   a4 = 1.000, a5 = 0.312, a6 = 0.329, a7 = 0.243
   REAL(kind=8) :: b1 = 2.0, b2 = 0.875
   REAL(kind=8) :: d,h,doh,num,den

   if (h .ne. 0) then
      doh   = d/h
      num   = a1*(doh)**b1 + a2*(doh)**b2+a3
      den   = a4*(doh)**b1 + a5*(doh)**b2+a6
      pthnl_1 = a7*num/den + (1. - a7)
   else
      pthnl_1 = 1.
   endif

   pthnl_2 = max(pthnl_1,pmin)
   pthnl = min(pthnl_2,pmax)

   IF(d.LE.100.) THEN  ! assume dx<=100m as LES
      pthnl = 0.0
   ENDIF
 
   RETURN
END FUNCTION

SUBROUTINE nonlocal_flux (nlflux, hfxpbl, deltaoh, zfacmf, &
    hpbl, kpbl, msftx ,msfty, sflux, enlfrac2, &
    zq, entfacmf,     &
    its, ite, jts, jte, kts, kte      )

    IMPLICIT NONE

    INTEGER :: its, ite, jts, jte, kts, kte

    REAL(kind=8), DIMENSION( its:ite, jts:jte ) :: hpbl, kpbl ! 1p*2
    REAL(kind=8), DIMENSION( its:ite, jts:jte ) :: msftx, msfty    ! IN 1p*2
    REAL(kind=8), DIMENSION( its:ite, jts:jte ) :: sflux, enlfrac2 ! 1p*2
    REAL(kind=8), DIMENSION( its:ite, kts:kte+1, jts:jte ) :: zq ! IN 1p
    REAL(kind=8), DIMENSION( its:ite, kts:kte,  jts:jte ) :: zfacmf, entfacmf ! 1p*2

    REAL(kind=8), DIMENSION( its:ite, jts:jte )  :: hfxpbl, deltaoh  !1p*2
    
    REAL(kind=8), DIMENSION( its:ite, kts:kte, jts:jte ) :: nlflux !out

    REAL(kind=8) :: pthnl

    ! param
    REAL(kind=8) :: dx = 0.8, dy = 0.8, &
                    mltop = 1.0, &
                    sfcfracn1 = 0.075, &
                    zfmin = 0.075, &
                    a11 = 0.8, a12 = 0.8, &
                    nlfrac = 1.0


    !local vars
    INTEGER :: i, j, k
    REAL(kind=8) :: delxy, mlfrac, ezfrac, sfcfracn, sflux0, snlflux0, amf1, amf2, &
                    bmf2, amf3, bmf3, pth1

!赋值结束，下面是kernel
    DO j = jts, jte
    DO i = its, ite

        deltaoh(i,j) = sflux(i,j)/hpbl(i,j)
        delxy = sqrt(dx/msftx(i,j)*dy/msfty(i,j))
        mlfrac        = mltop-deltaoh(i,j)
        ezfrac        = mltop+deltaoh(i,j)
        zfacmf(i,kts,j) = min(max((zq(i,kts+1,j)/hpbl(i,j)),zfmin),1.)
        sfcfracn      = max(sfcfracn1,zfacmf(i,kts,j))
    !
        sflux0      = (a11+a12*sfcfracn)*sflux(i,j)
        snlflux0    = nlfrac*sflux0
        amf1        = snlflux0/sfcfracn
        amf2        = -snlflux0/(mlfrac-sfcfracn)
        bmf2        = -mlfrac*amf2
        amf3        = snlflux0*enlfrac2(i,j)/deltaoh(i,j)
        bmf3        = -amf3*mlfrac
        hfxpbl(i,j) = amf3+bmf3
        pth1        = pthnl(delxy,hpbl(i,j))
        hfxpbl(i,j) = hfxpbl(i,j)*pth1
        DO k = kts, kte
            zfacmf(i,k,j) = max((zq(i,k+1,j)/hpbl(i,j)),zfmin)
            IF(k .LT. kpbl(i,j)) THEN
                IF(zfacmf(i,k,j).LE.sfcfracn) THEN
                    nlflux(i,k,j) =  amf1*zfacmf(i,k,j)
                ELSE IF (zfacmf(i,k,j).LE.mlfrac) THEN
                    nlflux(i,k,j) =  amf2*zfacmf(i,k,j)+bmf2
                ENDIF
                nlflux(i,k,j) = nlflux(i,k,j) + hfxpbl(i,j)*exp(-entfacmf(i,k,j))
                nlflux(i,k,j) = nlflux(i,k,j)*pth1
            ELSE
                nlflux(i,k,j) = 0.
            ENDIF
        ENDDO

    ENDDO
    ENDDO
END SUBROUTINE nonlocal_flux