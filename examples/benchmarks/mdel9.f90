SUBROUTINE cal_deform_and_div( hat, &
  msfvx, v,                         &
  its, ite, jts, jte, kts, kte )

  IMPLICIT NONE

  REAL(kind=8), DIMENSION( its:ite, jts:jte ) :: msfvx
  REAL(kind=8), DIMENSION( its:ite, kts:kte, jts:jte ) :: v
  REAL(kind=8), DIMENSION( its:ite, kts:kte, jts:jte ) :: hat
  INTEGER :: its, ite, jts, jte, kts, kte
  INTEGER :: i, j, k
  ! Calculate dv/dy.

! Apply a coordinate transformation to meridional velocity, v.

  DO j = jts, jte
    DO k = kts, kte
      DO i = its, ite
        ! Because msfvx at the poles will be undefined (1./0.), we will have
        ! trouble.  But we are OK since v at the poles is 0., and that takes
        ! precedence in this case.
        IF((j == jts) .OR. (j == jte)) THEN
          hat(i,k,j) = 0.
        ELSE ! normal code
          hat(i,k,j) = v(i,k,j) / msfvx(i,j)
        ENDIF
      END DO
    END DO
  END DO
END SUBROUTINE 