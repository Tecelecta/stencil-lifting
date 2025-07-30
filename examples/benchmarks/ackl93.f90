  SUBROUTINE advec_cell_kernel(x_min,       &
                               x_max,       &
                               y_min,       &
                               y_max,       &
                               vertexdx,    &
                               density1,    &
                               energy1,     &
                               mass_flux_x, &
                               vol_flux_x,  &
                               pre_vol,     &
                               ener_flux    )

    IMPLICIT NONE

    INTEGER :: x_min,x_max,y_min,y_max

    REAL(KIND=8), DIMENSION(x_min-2:x_max+2,y_min-2:y_max+2) :: density1 !3p
    REAL(KIND=8), DIMENSION(x_min-2:x_max+2,y_min-2:y_max+2) :: energy1 !3p
    REAL(KIND=8), DIMENSION(x_min-2:x_max+3,y_min-2:y_max+2) :: vol_flux_x !1p
    REAL(KIND=8), DIMENSION(x_min-2:x_max+3,y_min-2:y_max+2) :: mass_flux_x !out
    REAL(KIND=8), DIMENSION(x_min-2:x_max+3,y_min-2:y_max+3) :: pre_vol !1p
    REAL(KIND=8), DIMENSION(x_min-2:x_max+3,y_min-2:y_max+3) :: ener_flux !out

    REAL(KIND=8), DIMENSION(x_min-2:x_max+3) :: vertexdx

    INTEGER :: j,k,upwind,donor,downwind,dif

    REAL(KIND=8) :: wind,sigma,sigmat,sigmav,sigmam,sigma3,sigma4
    REAL(KIND=8) :: diffuw,diffdw,limiter
    REAL(KIND=8) :: one_by_six=1.0_8/6.0_8

    !$OMP PARALLEL

    !$OMP DO PRIVATE(upwind,donor,downwind,dif,sigmat,sigma3,sigma4,sigmav,sigma,sigmam, &
    !$OMP            diffuw,diffdw,limiter,wind)
    DO k=y_min,y_max
      DO j=x_min,x_max+2

        IF(vol_flux_x(j,k).GT.0.0)THEN
          upwind   =j-2
          donor    =j-1
          downwind =j
          dif      =donor
        ELSE
          upwind   =MIN(j+1,x_max+2)
          donor    =j
          downwind =j-1
          dif      =upwind
        ENDIF

        sigmat=ABS(vol_flux_x(j,k))/pre_vol(donor,k)
        sigma3=(1.0_8+sigmat)*(vertexdx(j)/vertexdx(dif))
        sigma4=2.0_8-sigmat

        sigma=sigmat
        sigmav=sigmat

        diffuw=density1(donor,k)-density1(upwind,k)
        diffdw=density1(downwind,k)-density1(donor,k)
        wind=1.0_8
        IF(diffdw.LE.0.0) wind=-1.0_8
        IF(diffuw*diffdw.GT.0.0)THEN
          limiter=(1.0_8-sigmav)*wind*MIN(ABS(diffuw),ABS(diffdw)&
            ,one_by_six*(sigma3*ABS(diffuw)+sigma4*ABS(diffdw)))
        ELSE
          limiter=0.0
        ENDIF
        mass_flux_x(j,k)=vol_flux_x(j,k)*(density1(donor,k)+limiter)

        sigmam=ABS(mass_flux_x(j,k))/(density1(donor,k)*pre_vol(donor,k))
        diffuw=energy1(donor,k)-energy1(upwind,k)
        diffdw=energy1(downwind,k)-energy1(donor,k)
        wind=1.0_8
        IF(diffdw.LE.0.0) wind=-1.0_8
        IF(diffuw*diffdw.GT.0.0)THEN
          limiter=(1.0_8-sigmam)*wind*MIN(ABS(diffuw),ABS(diffdw)&
            ,one_by_six*(sigma3*ABS(diffuw)+sigma4*ABS(diffdw)))
        ELSE
          limiter=0.0
        ENDIF

        ener_flux(j,k)=mass_flux_x(j,k)*(energy1(donor,k)+limiter)

      ENDDO
    ENDDO
    !$OMP END DO


  !$OMP END PARALLEL

  END SUBROUTINE advec_cell_kernel
