MODULE mg_loop15_mod
CONTAINS
SUBROUTINE mg_loop15(i1,i2,i3,n1,n2,n3,z)
INTEGER :: i1
INTEGER :: i2
INTEGER :: i3
INTEGER :: n1
INTEGER :: n2
INTEGER :: n3
DOUBLE PRECISION, DIMENSION(100,100,100) :: z
!     write(*,*)' '
!     write(*,*)' negative charges at'
!     write(*,9)(jg(1,i,0),jg(2,i,0),jg(3,i,0),i=1,mm)
!     write(*,*)' positive charges at'
!     write(*,9)(jg(1,i,1),jg(2,i,1),jg(3,i,1),i=1,mm)
!     write(*,*)' small random numbers were'
!     write(*,8)(ten( i,0),i=mm,1,-1)
!     write(*,*)' and they were found on processor number'
!     write(*,7)(jg(0,i,0),i=mm,1,-1)
!     write(*,*)' large random numbers were'
!     write(*,8)(ten( i,1),i=mm,1,-1)
!     write(*,*)' and they were found on processor number'
!     write(*,7)(jg(0,i,1),i=mm,1,-1)
! 9    format(5(' (',i3,2(',',i3),')'))
! 8    format(5D15.8)
! 7    format(10i4)
DO i3 = 1, 100
DO i2 = 1, 100
DO i1 = 1, 100
z(i1,i2,i3) = 0.0D0
END DO
END DO
END DO
END SUBROUTINE 

END MODULE mg_loop15_mod

