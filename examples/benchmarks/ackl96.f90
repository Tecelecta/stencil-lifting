  SUBROUTINE advec_cell_kernel(x_min,       &
                               x_max,       &
                               y_min,       &
                               y_max,       &
                               vertexdy,    &
                               density1,    &
                               energy1,     &
                               mass_flux_y, &
                               vol_flux_y,  &
                               pre_vol,     &
                               ener_flux    )

    IMPLICIT NONE

    INTEGER :: x_min,x_max,y_min,y_max

    REAL(KIND=8), DIMENSION(x_min-2:x_max+2,y_min-2:y_max+2) :: density1
    REAL(KIND=8), DIMENSION(x_min-2:x_max+2,y_min-2:y_max+2) :: energy1
    REAL(KIND=8), DIMENSION(x_min-2:x_max+2,y_min-2:y_max+3) :: vol_flux_y
    REAL(KIND=8), DIMENSION(x_min-2:x_max+2,y_min-2:y_max+3) :: mass_flux_y
    REAL(KIND=8), DIMENSION(x_min-2:x_max+3,y_min-2:y_max+3) :: pre_vol
    REAL(KIND=8), DIMENSION(x_min-2:x_max+3,y_min-2:y_max+3) :: ener_flux

    REAL(KIND=8), DIMENSION(y_min-2:y_max+3) :: vertexdy

    INTEGER :: j,k,upwind,donor,downwind,dif

    REAL(KIND=8) :: wind,sigma,sigmat,sigmav,sigmam,sigma3,sigma4
    REAL(KIND=8) :: diffuw,diffdw,limiter
    REAL(KIND=8) :: one_by_six=1.0_8/6.0_8

  !$OMP PARALLEL
    !$OMP DO PRIVATE(upwind,donor,downwind,dif,sigmat,sigma3,sigma4,sigmav,sigma,sigmam, &
    !$OMP            diffuw,diffdw,limiter,wind)
    DO k=y_min,y_max+2
      DO j=x_min,x_max

        IF(vol_flux_y(j,k).GT.0.0)THEN
          upwind   =k-2
          donor    =k-1
          downwind =k
          dif      =donor
        ELSE
          upwind   =MIN(k+1,y_max+2)
          donor    =k
          downwind =k-1
          dif      =upwind
        ENDIF

        sigmat=ABS(vol_flux_y(j,k))/pre_vol(j,donor)
        sigma3=(1.0_8+sigmat)*(vertexdy(k)/vertexdy(dif))
        sigma4=2.0_8-sigmat

        sigma=sigmat
        sigmav=sigmat

        diffuw=density1(j,donor)-density1(j,upwind)
        diffdw=density1(j,downwind)-density1(j,donor)
        wind=1.0_8
        IF(diffdw.LE.0.0) wind=-1.0_8
        IF(diffuw*diffdw.GT.0.0)THEN
          limiter=(1.0_8-sigmav)*wind*MIN(ABS(diffuw),ABS(diffdw)&
            ,one_by_six*(sigma3*ABS(diffuw)+sigma4*ABS(diffdw)))
        ELSE
          limiter=0.0
        ENDIF
        mass_flux_y(j,k)=vol_flux_y(j,k)*(density1(j,donor)+limiter)

        sigmam=ABS(mass_flux_y(j,k))/(density1(j,donor)*pre_vol(j,donor))
        diffuw=energy1(j,donor)-energy1(j,upwind)
        diffdw=energy1(j,downwind)-energy1(j,donor)
        wind=1.0_8
        IF(diffdw.LE.0.0) wind=-1.0_8
        IF(diffuw*diffdw.GT.0.0)THEN
          limiter=(1.0_8-sigmam)*wind*MIN(ABS(diffuw),ABS(diffdw)&
            ,one_by_six*(sigma3*ABS(diffuw)+sigma4*ABS(diffdw)))
        ELSE
          limiter=0.0
        ENDIF
        ener_flux(j,k)=mass_flux_y(j,k)*(energy1(j,donor)+limiter)

      ENDDO
    ENDDO
    !$OMP END DO
  !$OMP END PARALLEL

  END SUBROUTINE advec_cell_kernel
