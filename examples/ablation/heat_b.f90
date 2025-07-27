! heat with 1 boundary cond
SUBROUTINE stencil(ain, aout)
      IMPLICIT NONE
      real(kind=8) :: ain(258,258,258)
      real(kind=8) :: aout(258,258,258)

      integer :: i,j,k

      do i=2,258-1
        do j=2,258-1
          do k=2,258-1
            if ( i > 4  ) then
              if ( j > 4  ) then
                if ( k > 4 .and. k < 256-1 ) then
                    aout(i,j,k) = ain(i,j,k) + (1.0_8/6.0_8) * ( ain(i+1,j,k) + &
                                                ain(i-1,j,k) + &
                                                ain(i,j+1,k) + &
                                                ain(i,j-1,k) + &
                                                ain(i,j,k+1) + &
                                                ain(i,j,k-1) ) 
                else
                    aout(i,j,k) = ain(i,j,k) + (1.0_8/6.0_8) * ( ain(i+1,j,k) + &
                                                ain(i-1,j,k) + &
                                                ain(i,j+1,k) + &
                                                ain(i,j-1,k))
                endif
              else
                if ( k > 4 .and. k < 256-1 ) then
                    aout(i,j,k) = ain(i,j,k) + (1.0_8/6.0_8) * ( ain(i+1,j,k) + &
                                                ain(i-1,j,k) + &
                                                ain(i,j,k+1) + &
                                                ain(i,j,k-1) ) 
                else
                    aout(i,j,k) = ain(i,j,k) + (1.0_8/6.0_8) * ( ain(i+1,j,k) + &
                                                ain(i-1,j,k))
                endif
              endif
            else
              if ( j > 4  ) then
                if ( k > 4 .and. k < 256-1 ) then
                    aout(i,j,k) = ain(i,j,k) + (1.0_8/6.0_8) * ( &
                                                ain(i,j+1,k) + &
                                                ain(i,j-1,k) + &
                                                ain(i,j,k+1) + &
                                                ain(i,j,k-1) ) 
                else
                    aout(i,j,k) = ain(i,j,k) + (1.0_8/6.0_8) * (&
                                                ain(i,j+1,k) + &
                                                ain(i,j-1,k))
                endif
              else
                if ( k > 4 .and. k < 256-1 ) then
                    aout(i,j,k) = ain(i,j,k) + (1.0_8/6.0_8) * ( &
                                                ain(i,j,k+1) + &
                                                ain(i,j,k-1) ) 
                else
                    aout(i,j,k) = ain(i,j,k)
                endif
              endif
            endif  
          enddo
        enddo
      enddo

END SUBROUTINE
