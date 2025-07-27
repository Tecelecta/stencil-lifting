SUBROUTINE stencil(ain, aout)
      IMPLICIT NONE
      real(kind=8) :: ain(258,258,258)
      real(kind=8) :: aout(258,258,258)

      integer :: i,j,k
      integer :: tk, sk

      do tk = 2, 258-1, 16

      do i=2,258-1
        do j=2,258-1
          do sk=0,15
            k = tk + sk
            aout(i,j,k) = ain(i,j,k) + (1.0_8/6.0_8) * ( ain(i+1,j,k) + &
                                         ain(i-1,j,k) + &
                                         ain(i,j+1,k) + &
                                         ain(i,j-1,k) + &
                                         ain(i,j,k+1) + &
                                         ain(i,j,k-1) ) 
          enddo
        enddo
      enddo

      enddo


END SUBROUTINE
