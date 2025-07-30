SUBROUTINE jacobi_2d_5pt(ain, aout)
      IMPLICIT NONE
      real(kind=8) :: ain(258,258)
      real(kind=8) :: aout(258,258)

      integer :: i,j

      do i=2,258-1
        do j=2,258-1,4
            aout(i,j) = 0.25_8 * ( ain(i+1,j) + ain(i-1,j) + &
                                  ain(i,j+1) + ain(i,j-1) )
            aout(i,j+1) = 0.25_8 * ( ain(i+1,j+1) + ain(i-1,j+1) + &
                                    ain(i,j+2) + ain(i,j) )
            aout(i,j+2) = 0.25_8 * ( ain(i+1,j+2) + ain(i-1,j+2) + &
                                    ain(i,j+3) + ain(i,j+1) )
            aout(i,j+3) = 0.25_8 * ( ain(i+1,j+3) + ain(i-1,j+3) + &
                                    ain(i,j+4) + ain(i,j+2) )
        enddo
      enddo

END SUBROUTINE