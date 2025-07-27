SUBROUTINE stencil(ain, aout1, aout2, aout3)
      IMPLICIT NONE
      real(kind=8) :: aout1(258,258,258)
      real(kind=8) :: aout2(258,258,258)
      real(kind=8) :: aout3(258,258,258)
      real(kind=8) :: ain(258,258,258)

      integer :: i,j,k

      do i=2,258-1
        do j=2,258-1
          do k=2,258-1
            aout1(i,j,k) = ain(i,j,k) + (1.0_8/6.0_8) * ( ain(i+1,j,k) + &
                                         ain(i-1,j,k) )
            aout2(i,j,k) = ain(i,j,k) + (2.0_8/6.0_8) * ( ain(i,j+1,k) + &
                                         ain(i,j-1,k) )
            aout3(i,j,k) = ain(i,j,k) + (3.0_8/6.0_8) * ( ain(i,j,k+1) + &
                                         ain(i,j,k-1) )
          enddo
        enddo
      enddo


END SUBROUTINE
