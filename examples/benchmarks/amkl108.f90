SUBROUTINE advec_mom_kernel_loop108(mom_flux,node_flux,node_mass_pre,x_max,x_min,y_max,y_min,celldy,vel1)

    INTEGER :: x_max, x_min, y_max, y_min
    INTEGER :: j,k

    INTEGER :: upwind,donor,downwind,dif

    REAL(KIND=8) :: sigma,wind,width
    REAL(KIND=8) :: vdiffuw,vdiffdw,auw,adw,limiter
    REAL(KIND=8) :: advec_vel_s

    REAL(KIND=8), DIMENSION((x_min - 2):(x_max + 3),(y_min - 2):(y_max + 3)) :: mom_flux
    REAL(kind=8), DIMENSION((x_min - 2):(x_max + 3),(y_min - 2):(y_max + 3)) :: node_flux
    REAL(kind=8), DIMENSION((x_min - 2):(x_max + 3),(y_min - 2):(y_max + 3)) :: node_mass_pre
    REAL(kind=8), DIMENSION((y_min - 2):(y_max + 3)) :: celldy
    REAL(kind=8), DIMENSION((x_min - 2):(x_max + 3),(y_min - 2):(y_max + 3)) :: vel1

    !$OMP DO PRIVATE(upwind,donor,downwind,dif,sigma,width,limiter,vdiffuw,vdiffdw,auw,adw,wind,advec_vel_s)
    DO k=y_min-1,y_max+1
    DO j=x_min,x_max+1
        IF(node_flux(j,k).LT.0.0)THEN
            upwind=k+2
            donor=k+1
            downwind=k
            dif=donor
        ELSE
            upwind=k-1
            donor=k
            downwind=k+1
            dif=upwind
        ENDIF

        sigma=ABS(node_flux(j,k))/(node_mass_pre(j,donor))
        width=celldy(k)
        vdiffuw=vel1(j,donor)-vel1(j,upwind)
        vdiffdw=vel1(j,downwind)-vel1(j,donor)
        limiter=0.0
        IF(vdiffuw*vdiffdw.GT.0.0)THEN
            auw=ABS(vdiffuw)
            adw=ABS(vdiffdw)
            wind=1.0_8
        IF(vdiffdw.LE.0.0) wind=-1.0_8
            limiter=wind*MIN(width*((2.0_8-sigma)*adw/width+(1.0_8+sigma)*auw/celldy(dif))/6.0_8,auw,adw)
        ENDIF
        advec_vel_s=vel1(j,donor)+(1.0_8-sigma)*limiter
        mom_flux(j,k)=advec_vel_s*node_flux(j,k)
    ENDDO
    ENDDO

END SUBROUTINE 
