SUBROUTINE cal_helicity ( uh, ph, phb, ht, wavg, rvort, &
  its, ite, kts, kte, jts, jte )

  IMPLICIT NONE
  INTEGER :: its, ite, jts, jte, kts, kte 
!------------------------------------
REAL(kind=8), DIMENSION( its-1:ite, kts:kte+1, jts-1:jte ) :: ph, phb ! in 8p*2
REAL(kind=8), DIMENSION( its-1:ite, jts-1:jte ) :: ht ! in 4P
REAL(kind=8), DIMENSION( its:ite, kts:kte+1, jts:jte ) :: wavg, rvort ! in 2p*2

REAL(kind=8), DIMENSION( its:ite, kts:kte, jts:jte ) :: uh ! out
!-------------------------------------------------------------------

REAL(kind=8) :: g=9.81

INTEGER :: i, k, j
REAL(kind=8) :: zl, zu

DO j = jts, jte
  DO k = kts, kte
    DO i = its, ite
      zl = ( 0.25 * (  &
           (( ph(i  ,k  ,j  ) + phb(i  ,k  ,j  ) ) / g - ht(i  ,j  ) ) +  &
           (( ph(i-1,k  ,j  ) + phb(i-1,k  ,j  ) ) / g - ht(i-1,j  ) ) +  &
           (( ph(i  ,k  ,j-1) + phb(i  ,k  ,j-1) ) / g - ht(i  ,j-1) ) +  &
           (( ph(i-1,k  ,j-1) + phb(i-1,k  ,j-1) ) / g - ht(i-1,j-1) ) ) )

      zu = ( 0.25 * (  &
           (( ph(i  ,k+1,j  ) + phb(i  ,k+1,j  ) ) / g - ht(i  ,j  ) ) +  &
           (( ph(i-1,k+1,j  ) + phb(i-1,k+1,j  ) ) / g - ht(i-1,j  ) ) +  &
           (( ph(i  ,k+1,j-1) + phb(i  ,k+1,j-1) ) / g - ht(i  ,j-1) ) +  &
           (( ph(i-1,k+1,j-1) + phb(i-1,k+1,j-1) ) / g - ht(i-1,j-1) ) ) )

      IF ( zl .GE. 2. .AND. zu .LE. 5000. ) THEN
        IF ( wavg(i,k,j) .GT. 0. .AND. wavg(i,k+1,j) .GT. 0. ) THEN
          uh(i,k,j) = ( ( wavg(i,k,j) * rvort(i,k,j) + &
                    wavg(i,k+1,j) * rvort(i,k+1,j) ) * 0.5 ) &
                    * ( zu - zl )
        ELSE
          uh(i,k,j) = 0.
        ENDIF
      ELSE
        uh(i,k,j) = 0.
      ENDIF
    END DO
  END DO
END DO
END SUBROUTINE    