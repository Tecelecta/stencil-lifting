SUBROUTINE conv2d(ain, aout)
      IMPLICIT NONE
      real(kind=8) :: ain(258,258)
      real(kind=8) :: aout(258,258)

      integer :: i,j

      real(kind=8) :: tmp(258)

      do j=2,258-1
        do i=2,258-1
          tmp(i) = ain(i+1,j+1) + ain(i+1,j-1) + ain(i+1,j  ) 
        enddo
        do i=2,258-1
            aout(i,j) = (1.0_8/9.0_8) * ( ain(i, j  ) + ain(i-1,j  ) + &
                                          ain(i, j-1) + ain(i-1,j-1) + &
                                          ain(i, j+1) + ain(i-1,j ) + tmp(i))
        enddo
      enddo

END SUBROUTINE