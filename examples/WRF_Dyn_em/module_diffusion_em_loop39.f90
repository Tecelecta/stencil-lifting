SUBROUTINE vertical_diffusion_implicit(tao_yz,v_2,u_2,rho,ust)

!------下面三个数组需要初始化---------------------------------------------
REAL(kind=8), DIMENSION( 100, 100, 100), INTENT(IN) :: v_2, u_2
REAL(kind=8), DIMENSION( 100, 100, 100), INTENT(IN) :: rho 
REAL(kind=8), DIMENSION( 100, 100), INTENT(IN) :: ust

integer :: i, j
REAL(kind=8), DIMENSION( 100, 100 ) :: tao_yz
REAL(kind=8), PARAMETER :: epsilon = 1.e-10
REAL(kind=8) :: V0_v, ustar

DO j = 1, 100
    DO i = 1, 100
       V0_v = 0.
       V0_v =       sqrt((v_2(i,1,j)**2) +          &
                        (((u_2(i  ,1,j  )+          &
                           u_2(i  ,1,j-1)+          &
                           u_2(i+1,1,j  )+          &
                           u_2(i+1,1,j-1))/4)**2))+epsilon
       ustar = 0.5*(ust(i,j)+ust(i,j-1))
       tao_yz(i,j) = ustar*ustar*(rho(i,1,j)+rho(i,1,j-1))/(2.*V0_v)
    ENDDO
    ENDDO
END SUBROUTINE
