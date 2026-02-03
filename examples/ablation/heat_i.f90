SUBROUTINE stencil(ain, aout)
      IMPLICIT NONE
      real(kind=8) :: ain(258,258,258)
      real(kind=8) :: aout(258,258,258)

      integer :: i,j,k

      real(kind=8), dimension(258) :: tmp

      do i=2,258-1
        do j=2,258-1
          do k=2,258-1
            tmp(k) = ain(i+1,j,k) + ain(i-1,j,k) + ain(i,j+1,k) + ain(i,j-1,k)
          enddo
          do k=2,258-1
            aout(i,j,k) = ain(i,j,k) + (1.0_8/6.0_8) * ( tmp(k) + &
                                         ain(i,j,k+1) + &
                                         ain(i,j,k-1) ) 
          enddo
        enddo
      enddo
END SUBROUTINE
