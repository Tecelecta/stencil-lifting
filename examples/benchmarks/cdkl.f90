SUBROUTINE calc_dt_kernel(x_min,x_max,y_min,y_max,             &
                        xarea,                               &
                        yarea,                               &
                        celldx,                              &
                        celldy,                              &
                        volume,                              &
                        density0,                            &
                        viscosity_a,                         &
                        soundspeed,                          &
                        xvel0,yvel0,                         &
                        dt_min_val)

IMPLICIT NONE

INTEGER :: x_min,x_max,y_min,y_max
REAL(KIND=8)  :: g_small,g_big,dtmin
REAL(KIND=8)  :: dtc_safe,dtu_safe,dtv_safe,dtdiv_safe

REAL(KIND=8), DIMENSION(x_min:x_max, y_min:y_max) :: dt_min_val
REAL(KIND=8), DIMENSION(x_min-2:x_max+3,y_min-2:y_max+2) :: xarea
REAL(KIND=8), DIMENSION(x_min-2:x_max+2,y_min-2:y_max+3) :: yarea
REAL(KIND=8), DIMENSION(x_min-2:x_max+2)             :: celldx
REAL(KIND=8), DIMENSION(y_min-2:y_max+2)             :: celldy
REAL(KIND=8), DIMENSION(x_min-2:x_max+2,y_min-2:y_max+2) :: volume
REAL(KIND=8), DIMENSION(x_min-2:x_max+2,y_min-2:y_max+2) :: density0
REAL(KIND=8), DIMENSION(x_min-2:x_max+2,y_min-2:y_max+2) :: viscosity_a
REAL(KIND=8), DIMENSION(x_min-2:x_max+2,y_min-2:y_max+2) :: soundspeed
REAL(KIND=8), DIMENSION(x_min-2:x_max+3,y_min-2:y_max+3) :: xvel0,yvel0 !4p

INTEGER          :: small

INTEGER          :: j,k

REAL(KIND=8)     :: div,dsx,dsy,dtut,dtvt,dtct,dtdivt,cc,dv1,dv2,jk_control

small=0
jk_control=1.1
g_small = 1.0
g_big = 1.0
dtmin = 1.0

dtc_safe = 0.5
dtu_safe = 0.5
dtv_safe = 0.5
dtdiv_safe = 0.5

!$OMP PARALLEL

!$OMP DO PRIVATE(dsx,dsy,cc,dv1,dv2,div,dtct,dtut,dtvt,dtdivt)
DO k=y_min,y_max
    DO j=x_min,x_max

    dsx=celldx(j)
    dsy=celldy(k)

    cc=soundspeed(j,k)*soundspeed(j,k)
    cc=cc+2.0_8*viscosity_a(j,k)/density0(j,k)
    cc=MAX(SQRT(cc),g_small)

    dtct=dtc_safe*MIN(dsx,dsy)/cc

    div=0.0

    dv1=(xvel0(j  ,k)+xvel0(j  ,k+1))*xarea(j  ,k)
    dv2=(xvel0(j+1,k)+xvel0(j+1,k+1))*xarea(j+1,k)

    div=div+dv2-dv1

    dtut=dtu_safe*2.0_8*volume(j,k)/MAX(ABS(dv1),ABS(dv2),g_small*volume(j,k))

    dv1=(yvel0(j,k  )+yvel0(j+1,k  ))*yarea(j,k  )
    dv2=(yvel0(j,k+1)+yvel0(j+1,k+1))*yarea(j,k+1)

    div=div+dv2-dv1

    dtvt=dtv_safe*2.0_8*volume(j,k)/MAX(ABS(dv1),ABS(dv2),g_small*volume(j,k))

    div=div/(2.0_8*volume(j,k))

    IF(div.LT.-g_small)THEN
        dtdivt=dtdiv_safe*(-1.0_8/div)
    ELSE
        dtdivt=g_big
    ENDIF

    dt_min_val(j,k) = MIN(g_big, dtct,dtut,dtvt,dtdivt)

    ENDDO
ENDDO
!$OMP END DO

!$OMP END PARALLEL

END SUBROUTINE
