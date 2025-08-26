SUBROUTINE jacobi_2d_5pt(ain, aout)
      IMPLICIT NONE
      real(kind=8) :: ain(258,258)
      real(kind=8) :: aout(258,258)

      integer :: i,j

      do i=2,258-1
        do j=2,258-1
          if( i>=4 ) then
            if( j >= 4 ) then
                aout(i,j) = 0.25_8 * ( ain(i+1,j) + ain(i-1,j) + &
                                       ain(i,j+1) + ain(i,j-1) )
            else
                aout(i,j) = 0.25_8 * ( ain(i+1,j) + ain(i-1,j) + &
                                       ain(i,j+1) )
            endif
          else
            if( j >= 4 ) then
                aout(i,j) = 0.25_8 * ( ain(i+1,j) + ain(i-1,j) + &
                                       ain(i,j+1) )
            else
                aout(i,j) = 0.25_8 * ( ain(i+1,j) + ain(i-1,j) )
            endif
          endif 
        enddo
      enddo

END SUBROUTINE