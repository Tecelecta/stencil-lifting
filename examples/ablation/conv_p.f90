SUBROUTINE conv2d(ain, aout)
      IMPLICIT NONE
      real(kind=8) :: ain(258,258)
      real(kind=8) :: aout(258,258)

      integer :: i,j
      !$OMP PARALLEL
      !$OMP DO
      do j=2,258-1
        do i=2,258-1
            aout(i,j) = (1.0_8/9.0_8) * ( ain(i+1,j  ) + ain(i, j  ) + ain(i-1,j  ) + &
                                          ain(i+1,j-1) + ain(i, j-1) + ain(i-1,j-1) + &
                                          ain(i+1,j+1) + ain(i, j  ) + ain(i-1,j-1) )
        enddo
      enddo
      !$OMP END DO
      !$OMP END PARALLEL
END SUBROUTINE