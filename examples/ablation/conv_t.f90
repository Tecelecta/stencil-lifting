SUBROUTINE conv2d(ain, aout)
      IMPLICIT NONE
      real(kind=8) :: ain(258,258)
      real(kind=8) :: aout(258,258)

      integer :: i,j
      integer :: ti,tj
      integer :: si,sj

      do ti=2,258-1,16
        do tj=2,258-1,16
          do sj=0,15
            do si=0,15
              i = ti + si
              j = tj + sj
              aout(i,j) = (1.0_8/9.0_8) * ( ain(i+1,j  ) + ain(i, j  ) + ain(i-1,j  ) + &
                                            ain(i+1,j-1) + ain(i, j-1) + ain(i-1,j-1) + &
                                            ain(i+1,j+1) + ain(i, j+1) + ain(i-1,j+1) )
            enddo
          enddo
        enddo
      enddo

END SUBROUTINE