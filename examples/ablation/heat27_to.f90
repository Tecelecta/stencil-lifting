SUBROUTINE stencil(ain, aout)
      IMPLICIT NONE
      real(kind=8) :: ain(258,258,258)
      real(kind=8) :: aout(258,258,258)

      integer :: i,j,k
      integer :: ti,si

      do ti=2,258-1,16
        do k=2,258-1
          do j=2,258-1
            do si=0,15
              i = ti + si
              aout(i,j,k) = ain(i,j,k) + (1.0_8/26.0_8) * ( &
                                                ain(i  ,j  ,k-1) + ain(i  ,j  ,k+1) + &
                            ain(i  ,j-1,k  ) + ain(i  ,j-1,k-1) + ain(i  ,j-1,k+1) + &
                            ain(i  ,j+1,k  ) + ain(i  ,j+1,k-1) + ain(i  ,j+1,k+1) + &
                            ain(i-1,j  ,k  ) + ain(i-1,j  ,k-1) + ain(i-1,j  ,k+1) + &
                            ain(i-1,j-1,k  ) + ain(i-1,j-1,k-1) + ain(i-1,j-1,k+1) + &
                            ain(i-1,j+1,k  ) + ain(i-1,j+1,k-1) + ain(i-1,j+1,k+1) + &
                            ain(i+1,j  ,k  ) + ain(i+1,j  ,k-1) + ain(i+1,j  ,k+1) + &
                            ain(i+1,j-1,k  ) + ain(i+1,j-1,k-1) + ain(i+1,j-1,k+1) + &
                            ain(i+1,j+1,k  ) + ain(i+1,j+1,k-1) + ain(i+1,j+1,k+1)) 
            enddo
          enddo
        enddo
      enddo


END SUBROUTINE
