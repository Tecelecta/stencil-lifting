SUBROUTINE conv2d(ain, aout)
      IMPLICIT NONE
      real(kind=8) :: ain(258,258)
      real(kind=8) :: aout(258,258)

      integer :: i,j

      do j=2,258-1
        do i=2,258-1
          if (j <= 4) then
            if (i <= 4) then
              aout(i,j) = (1.0_8/4.0_8) * ( ain(i+1,j  ) + ain(i, j  ) + &
                                            ain(i+1,j+1) + ain(i, j+1) )
            else
              aout(i,j) = (1.0_8/6.0_8) * ( ain(i+1,j  ) + ain(i, j  ) + ain(i-1,j  ) + &
                                            ain(i+1,j+1) + ain(i, j+1) + ain(i-1,j+1) )
            endif
          else
            if (i <= 4) then
              aout(i,j) = (1.0_8/6.0_8) * ( ain(i+1,j  ) + ain(i, j  ) + &
                                            ain(i+1,j-1) + ain(i, j-1) + &
                                            ain(i+1,j+1) + ain(i, j+1) )
            else            
              aout(i,j) = (1.0_8/9.0_8) * ( ain(i+1,j  ) + ain(i, j  ) + ain(i-1,j  ) + &
                                            ain(i+1,j-1) + ain(i, j-1) + ain(i-1,j-1) + &
                                            ain(i+1,j+1) + ain(i, j+1) + ain(i-1,j+1) )
            endif
          endif
        enddo
      enddo

END SUBROUTINE