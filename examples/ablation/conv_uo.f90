SUBROUTINE conv2d(ain, aout)
      IMPLICIT NONE
      real(kind=8) :: ain(258,258)
      real(kind=8) :: aout(258,258)

      integer :: i,j
      integer :: i1, i2, i3

      do j=2,258-1
        do i=2,258-1
          i1 = i+1
          i2 = i+2
          i3 = i+3
          aout(i,j) = (1.0_8/9.0_8) * ( ain(i+1,j  ) + ain(i, j  ) + ain(i-1,j  ) + &
                                        ain(i+1,j-1) + ain(i, j-1) + ain(i-1,j-1) + &
                                        ain(i+1,j+1) + ain(i, j+1) + ain(i-1,j+1) )
          aout(i1,j) = (1.0_8/9.0_8) * ( ain(i1+1,j  ) + ain(i1, j  ) + ain(i1-1,j  ) + &
                                         ain(i1+1,j-1) + ain(i1, j-1) + ain(i1-1,j-1) + &
                                         ain(i1+1,j+1) + ain(i1, j+1) + ain(i1-1,j+1) )
          aout(i2,j) = (1.0_8/9.0_8) * ( ain(i2+1,j  ) + ain(i2, j  ) + ain(i2-1,j  ) + &
                                         ain(i2+1,j-1) + ain(i2, j-1) + ain(i2-1,j-1) + &
                                         ain(i2+1,j+1) + ain(i2, j+1) + ain(i2-1,j+1) )
          aout(i3,j) = (1.0_8/9.0_8) * ( ain(i3+1,j  ) + ain(i3, j  ) + ain(i3-1,j  ) + &
                                         ain(i3+1,j-1) + ain(i3, j-1) + ain(i3-1,j-1) + &
                                         ain(i3+1,j+1) + ain(i3, j+1) + ain(i3-1,j+1) )
        enddo
      enddo

END SUBROUTINE