SUBROUTINE meso_length_scale(els, &
    rmol, za,    &
    its, ite, jts, jte, kts, kte )

    IMPLICIT NONE
!
! The mesoscale length scale based on Nakanishi and Niino (2009)
! and modified by X. Zhang 
    INTEGER :: its, ite, jts, jte, kts, kte
    REAL(kind=8), DIMENSION( its:ite, jts:jte ) :: rmol !1p
    ! REAL(kind=8), DIMENSION( its:ite, kts:kte, jts:jte )  :: rdzw
    REAL(kind=8), DIMENSION( its:ite, kts:kte, jts:jte ) :: za !1p

    REAL(kind=8), DIMENSION( its:ite, kts:kte, jts:jte ) :: els !out
    ! REAL(kind=8), DIMENSION( its:ite, kts:kte+1, jts:jte )  :: zfull

    REAL(kind=8) :: alp4 = 100.0
    REAL(kind=8) :: karman=0.4               ! von karman constant

    ! local vars
    REAL(kind=8) :: coe
    INTEGER :: i, k, j

    ! 局部变量的初始赋值
    ! DO  j = jts, jte
    !   DO  i = its, ite
    !       zfull(i,kts,j) = 0.
    !   ENDDO
    ! ENDDO
   
    ! DO  j = jts, jte
    !   DO  k = kts, kte
    !     DO  i = its, ite
    !         zfull(i,k+1,j) = 1.0/rdzw(i,k,j) + zfull(i,k,j)
    !     ENDDO
    !   ENDDO
    ! ENDDO

    ! DO  j = jts, jte
    !   DO  k = kts, kte
    !     DO  i = its, ite
    !       za(i,k,j) = (zfull(i,k,j) + zfull(i,k+1,j))/2.0
    !     ENDDO
    !   ENDDO
    ! ENDDO

      !赋值结束，kernel开始
    DO j = jts, jte
      DO k = kts, kte
        DO i = its,ite
          IF (rmol(i,j) .GT. 0.0) THEN
             els(i,k,j) = karman*za(i,k,j)/(1.0+2.7*min(za(i,k,j)*rmol(i,j),1.0))
          ELSE
            coe = (1.0 - alp4*za(i,k,j)*rmol(i,j))**0.2
            els(i,k,j) = 1.0*karman*za(i,k,j)*coe
          ENDIF
        ENDDO
      ENDDO
    ENDDO
        
END SUBROUTINE 