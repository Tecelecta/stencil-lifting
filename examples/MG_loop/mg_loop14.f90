MODULE mg_loop14_mod
CONTAINS
SUBROUTINE mg_loop14(i,j1,j2,j3,mm,ten)
INTEGER :: i
INTEGER :: mm
INTEGER, DIMENSION(100,0:1) :: j1
INTEGER, DIMENSION(100,0:1) :: j2
INTEGER, DIMENSION(100,0:1) :: j3
DOUBLE PRECISION, DIMENSION(100,0:1) :: ten
!---------------------------------------------------------------------
!       call comm3(z,n1,n2,n3)
!       call showall(z,n1,n2,n3)
!---------------------------------------------------------------------
!---------------------------------------------------------------------
!     each processor looks for twenty candidates
!---------------------------------------------------------------------
DO i = 1, 100
ten(i,1) = 0.0D0
j1(i,1) = 0
j2(i,1) = 0
j3(i,1) = 0
ten(i,0) = 1.0D0
j1(i,0) = 0
j2(i,0) = 0
j3(i,0) = 0
END DO
END SUBROUTINE 

END MODULE mg_loop14_mod

