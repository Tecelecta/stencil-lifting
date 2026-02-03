SUBROUTINE stencil(ain, aout)
      IMPLICIT NONE
      real(kind=8) :: ain(258,258,258)
      real(kind=8) :: aout(258,258,258)

      integer :: i,j,k

      real(kind=8), dimension(258) :: tmp

      do k=2,258-1
        do j=2,258-1
          do i=2,258-1
            tmp(i) = ain(i  ,j+1,k  ) + ain(i  ,j+1,k-1) + ain(i  ,j+1,k+1) + &
                     ain(i-1,j+1,k  ) + ain(i-1,j+1,k-1) + ain(i-1,j+1,k+1) + &
                     ain(i+1,j+1,k  ) + ain(i+1,j+1,k-1) + ain(i+1,j+1,k+1)
          enddo
          do i=2,258-1
            aout(i,j,k) = ain(i,j,k) + (1.0_8/26.0_8) * ( tmp(i) + &
                                             ain(i  ,j  ,k-1) + ain(i  ,j  ,k+1) + &
                          ain(i  ,j-1,k  ) + ain(i  ,j-1,k-1) + ain(i  ,j-1,k+1) + &
                          ain(i-1,j  ,k  ) + ain(i-1,j  ,k-1) + ain(i-1,j  ,k+1) + &
                          ain(i-1,j-1,k  ) + ain(i-1,j-1,k-1) + ain(i-1,j-1,k+1) + &
                          ain(i+1,j  ,k  ) + ain(i+1,j  ,k-1) + ain(i+1,j  ,k+1) + &
                          ain(i+1,j-1,k  ) + ain(i+1,j-1,k-1) + ain(i+1,j-1,k+1))
          enddo
        enddo
      enddo


END SUBROUTINE
