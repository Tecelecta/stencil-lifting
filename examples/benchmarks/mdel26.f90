SUBROUTINE calc_l_scale( l_scale, &
  tke, BN2, rdzw, msftx, msfty,   &
  its, ite, jts, jte, kts, kte)
! History:     Sep 2003   Written by Bryan and Knievel, NCAR
! Purpose:     This routine calculates the length scale, based on stability,
!              for TKE parameterization of subgrid-scale turbulence.
!-----------------------------------------------------------------------
! Begin declarations.
IMPLICIT NONE
INTEGER :: its, ite, jts, jte, kts, kte

REAL(kind=8), DIMENSION( its:ite, kts:kte, jts:jte ) :: BN2, tke, rdzw ! IN 1p*3
REAL(kind=8), DIMENSION( its:ite, jts:jte) :: msftx, msfty !IN 1p*2
REAL(kind=8), DIMENSION( its:ite, kts:kte, jts:jte ) :: l_scale ! OUT

! param
REAL(kind=8) :: dx = 0.8
REAL(kind=8) :: dy = 0.8

! Local variables.
INTEGER :: i, j, k
REAL(kind=8) :: deltas, tmp
! End declarations.
!-----------------------------------------------------------------------

DO j = jts, jte
DO k = kts, kte
DO i = its, ite
  deltas         = ( dx/msftx(i,j) * dy/msfty(i,j) / rdzw(i,k,j) )**0.33333333
  l_scale(i,k,j) = deltas
  IF ( BN2(i,k,j) .gt. 1.0e-6 ) THEN
    tmp            = SQRT( MAX( tke(i,k,j), 1.0e-6 ) )
    l_scale(i,k,j) = 0.76 * tmp / SQRT( BN2(i,k,j) )
    l_scale(i,k,j) = MIN( l_scale(i,k,j), deltas)
    l_scale(i,k,j) = MAX( l_scale(i,k,j), 0.001 * deltas )
  END IF
END DO
END DO
END DO
END SUBROUTINE calc_l_scale
