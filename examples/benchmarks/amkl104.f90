SUBROUTINE advec_mom_kernel_loop104(mom_flux,node_flux,node_mass_pre,x_max,x_min,y_max,y_min,celldx,vel1)

    INTEGER :: x_max, x_min, y_max, y_min
    INTEGER :: j,k

    INTEGER :: upwind,donor,downwind,dif

    REAL(KIND=8) :: sigma,wind,width
    REAL(KIND=8) :: vdiffuw,vdiffdw,auw,adw,limiter
    REAL(KIND=8) :: advec_vel_s

    REAL(KIND=8), DIMENSION((x_min - 2):(x_max + 3),(y_min - 2):(y_max + 3)) :: mom_flux
    REAL(kind=8), DIMENSION((x_min - 2):(x_max + 3),(y_min - 2):(y_max + 3)) :: node_flux
    REAL(kind=8), DIMENSION((x_min - 2):(x_max + 3),(y_min - 2):(y_max + 3)) :: node_mass_pre
    REAL(kind=8), DIMENSION((x_min - 2):(x_max + 3)) :: celldx
    REAL(kind=8), DIMENSION((x_min - 2):(x_max + 3),(y_min - 2):(y_max + 3)) :: vel1

    !$OMP DO PRIVATE(upwind,downwind,donor,dif,sigma,width,limiter,vdiffuw,vdiffdw,auw,adw,wind,advec_vel_s)
    DO k=y_min,y_max+1
    DO j=x_min-1,x_max+1
        IF(node_flux(j,k).LT.0.0)THEN
            upwind=j+2
            donor=j+1
            downwind=j
            dif=donor
        ELSE
            upwind=j-1
            donor=j
            downwind=j+1
            dif=upwind
        ENDIF
        sigma=ABS(node_flux(j,k))/(node_mass_pre(donor,k))
        width=celldx(j)
        vdiffuw=vel1(donor,k)-vel1(upwind,k)
        vdiffdw=vel1(downwind,k)-vel1(donor,k)
        limiter=0.0
        IF(vdiffuw*vdiffdw.GT.0.0)THEN
            auw=ABS(vdiffuw)
            adw=ABS(vdiffdw)
            wind=1.0_8
        IF(vdiffdw.LE.0.0) wind=-1.0_8
            limiter=wind*MIN(width*((2.0_8-sigma)*adw/width+(1.0_8+sigma)*auw/celldx(dif))/6.0_8,auw,adw)
        ENDIF
        advec_vel_s=vel1(donor,k)+(1.0-sigma)*limiter
        mom_flux(j,k)=advec_vel_s*node_flux(j,k)
    ENDDO
    ENDDO

END SUBROUTINE 
