SUBROUTINE stencil(ain, aout)
      IMPLICIT NONE
      real(kind=8) :: ain(258,258,258)
      real(kind=8) :: aout(258,258,258)

      integer :: i,j,k

      do k=2,258-1
        do j=2,258-1
          do i=2,258-1
            if ( k <= 4) then
              if ( j <= 4) then
                if (i <= 4) then
                  aout(i,j,k) = ain(i,j,k) 
                else
                  aout(i,j,k) = ain(i,j,k) + (1.0_8/2.0_8) * ( &
                          ain(i-1,j  ,k  ) + &
                          ain(i+1,j  ,k  )) 
                endif
              else
                if (i <= 4) then
                  aout(i,j,k) = ain(i,j,k) + (1.0_8/26.0_8) * ( &
                          ain(i  ,j-1,k  )+ &
                          ain(i  ,j+1,k  ) ) 
                else
                  aout(i,j,k) = ain(i,j,k) + (1.0_8/8.0_8) * ( &
                          ain(i  ,j-1,k  ) + &
                          ain(i  ,j+1,k  ) + &
                          ain(i-1,j  ,k  ) + &
                          ain(i-1,j-1,k  ) + &
                          ain(i-1,j+1,k  ) + &
                          ain(i+1,j  ,k  ) + &
                          ain(i+1,j-1,k  ) + &
                          ain(i+1,j+1,k  )) 
                endif
              endif
            else
              if ( j <= 4) then
                if (i <= 4) then
                  aout(i,j,k) = ain(i,j,k) + (1.0_8/2.0_8) * ( &
                                             ain(i  ,j  ,k-1) + ain(i  ,j  ,k+1)) 
                else
                  aout(i,j,k) = ain(i,j,k) + (1.0_8/8.0_8) * ( &
                                             ain(i  ,j  ,k-1) + ain(i  ,j  ,k+1) + &
                          ain(i-1,j  ,k  ) + ain(i-1,j  ,k-1) + ain(i-1,j  ,k+1) + &
                          ain(i+1,j  ,k  ) + ain(i+1,j  ,k-1) + ain(i+1,j  ,k+1))
                endif
              else
                if (i <= 4) then
                  aout(i,j,k) = ain(i,j,k) + (1.0_8/8.0_8) * ( &
                                             ain(i  ,j  ,k-1) + ain(i  ,j  ,k+1) + &
                          ain(i  ,j-1,k  ) + ain(i  ,j-1,k-1) + ain(i  ,j-1,k+1) + &
                          ain(i  ,j+1,k  ) + ain(i  ,j+1,k-1) + ain(i  ,j+1,k+1))
                else
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
                endif
              endif
            endif
          enddo
        enddo
      enddo


END SUBROUTINE
