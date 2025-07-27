SUBROUTINE meso_length_scale( elb, elf, &
    dthrdn, qtke, sflux, elt,           &
    its, ite, jts, jte, kts, kte )

    ! in grid
    REAL(kind=8), DIMENSION( its:ite, kts:kte, jts:jte )  :: dthrdn, qtke !1p*2
    REAL(kind=8), DIMENSION( its:ite,          jts:jte )  :: sflux, elt !1p*2

    REAL(kind=8), DIMENSION( its:ite, kts:kte, jts:jte )  :: elb, elf !out*2


    ! local vars
    REAL(kind=8) :: qcv, N2, gtr
    REAL(kind=8) :: alp2 = 0.5, alp3 = 0.5
    REAL(kind=8) :: g = 9.81  ! acceleration due to gravity (m {s}^-2)

    gtr  = g/300.  
! -----------------------------------赋初值
    ! DO j = jts, jte
    !     DO k = kts+1, kte-1
    !     DO i = its, ite
    !       tmpdz         = 1.0 / rdz(i,k+1,j) + 1.0 / rdz(i,k,j)
    !       dthrdn(i,k,j) = ( theta(i,k+1,j) - theta(i,k-1,j) ) / tmpdz
    !     END DO
    !     END DO
    !     END DO
     
    !     k = kts
    !     DO j = jts, jte
    !     DO i = its, ite
    !       tmpdz         = 1.0 / rdzw(i,k+1,j) + 1.0 / rdzw(i,k,j)
    !       thetasfc      = T8w(i,kts,j) / ( p8w(i,k,j) / p1000mb )**( R_d / Cp )
    !       dthrdn(i,k,j) = ( theta(i,k+1,j) - thetasfc ) / tmpdz
    !     END DO
    !     END DO
    !     k = kte
    !     DO j = jts, jte
    !     DO i = its, ite
    !       tmpdz         = 1.0 / rdz(i,k,j) + 0.5 / rdzw(i,k,j)
    !       thetatop      = T8w(i,kde,j) / ( p8w(i,kde,j) / p1000mb )**( R_d / Cp )
    !       dthrdn(i,k,j) = ( thetatop - theta(i,k-1,j) ) / tmpdz
    !     END DO
    !     END DO

    !     DO j = jts, jte
    !         DO i = its, ite
    !           elt(i,j) = 1.0e-5
    !         ENDDO
    !     ENDDO

    !     DO j = jts,jte
    !             DO i = its,ite
    !                cpm        = cp  * (1. + 0.8*moist(i,1,j,P_QV)) 
    !                sflux(i,j) = (hfx(i,j)/cpm)/rho(i,1,j)
    !             ENDDO
    !    ENDDO        
       
!--------赋值结束，下面是kernel-----------------------------------

!-------Length scale limited by bouyancy effect-----    
DO j = jts, jte
  DO k = kts, kte
    DO i = its, ite
      IF( dthrdn(i,k,j).GT.0.0 ) THEN
        N2 = gtr*dthrdn(i,k,j)
        qcv = (gtr*MAX(sflux(i,j),0.0)*elt(i,j))**(1.0/3.0)
        elb(i,k,j) = qtke(i,k,j)/sqrt(N2)*(alp2 + alp3*sqrt(qcv/(elt(i,j)*sqrt(N2))))
        elf(i,k,j) = alp2*qtke(i,k,j)/sqrt(N2)
      ELSE
        elb(i,k,j) = 1.0e10
        elf(i,k,j) = elb(i,k,j)
      ENDIF
    ENDDO
  ENDDO
ENDDO
END SUBROUTINE 