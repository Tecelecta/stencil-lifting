SUBROUTINE stencil(ain, aout)
      IMPLICIT NONE
      real(kind=8) :: ain(258,258,258)
      real(kind=8) :: aout(258,258,258)

      integer :: i,j,k
      integer :: i1, i2, i3

      do k=2,258-1
        do j=2,258-1
          do i=2,258-1
            i1 = i+1
            i2 = i+2
            i3 = i+3
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

            aout(i1,j,k) = ain(i1,j,k) + (1.0_8/26.0_8) * ( &
                                              ain(i1  ,j  ,k-1) + ain(i1  ,j  ,k+1) + &
                          ain(i1  ,j-1,k  ) + ain(i1  ,j-1,k-1) + ain(i1  ,j-1,k+1) + &
                          ain(i1  ,j+1,k  ) + ain(i1  ,j+1,k-1) + ain(i1  ,j+1,k+1) + &
                          ain(i1-1,j  ,k  ) + ain(i1-1,j  ,k-1) + ain(i1-1,j  ,k+1) + &
                          ain(i1-1,j-1,k  ) + ain(i1-1,j-1,k-1) + ain(i1-1,j-1,k+1) + &
                          ain(i1-1,j+1,k  ) + ain(i1-1,j+1,k-1) + ain(i1-1,j+1,k+1) + &
                          ain(i1+1,j  ,k  ) + ain(i1+1,j  ,k-1) + ain(i1+1,j  ,k+1) + &
                          ain(i1+1,j-1,k  ) + ain(i1+1,j-1,k-1) + ain(i1+1,j-1,k+1) + &
                          ain(i1+1,j+1,k  ) + ain(i1+1,j+1,k-1) + ain(i1+1,j+1,k+1)) 

            aout(i2,j,k) = ain(i2,j,k) + (1.0_8/26.0_8) * ( &
                                              ain(i2  ,j  ,k-1) + ain(i2  ,j  ,k+1) + &
                          ain(i2  ,j-1,k  ) + ain(i2  ,j-1,k-1) + ain(i2  ,j-1,k+1) + &
                          ain(i2  ,j+1,k  ) + ain(i2  ,j+1,k-1) + ain(i2  ,j+1,k+1) + &
                          ain(i2-1,j  ,k  ) + ain(i2-1,j  ,k-1) + ain(i2-1,j  ,k+1) + &
                          ain(i2-1,j-1,k  ) + ain(i2-1,j-1,k-1) + ain(i2-1,j-1,k+1) + &
                          ain(i2-1,j+1,k  ) + ain(i2-1,j+1,k-1) + ain(i2-1,j+1,k+1) + &
                          ain(i2+1,j  ,k  ) + ain(i2+1,j  ,k-1) + ain(i2+1,j  ,k+1) + &
                          ain(i2+1,j-1,k  ) + ain(i2+1,j-1,k-1) + ain(i2+1,j-1,k+1) + &
                          ain(i2+1,j+1,k  ) + ain(i2+1,j+1,k-1) + ain(i2+1,j+1,k+1)) 

            aout(i3,j,k) = ain(i3,j,k) + (1.0_8/26.0_8) * ( &
                                              ain(i3  ,j  ,k-1) + ain(i3  ,j  ,k+1) + &
                          ain(i3  ,j-1,k  ) + ain(i3  ,j-1,k-1) + ain(i3  ,j-1,k+1) + &
                          ain(i3  ,j+1,k  ) + ain(i3  ,j+1,k-1) + ain(i3  ,j+1,k+1) + &
                          ain(i3-1,j  ,k  ) + ain(i3-1,j  ,k-1) + ain(i3-1,j  ,k+1) + &
                          ain(i3-1,j-1,k  ) + ain(i3-1,j-1,k-1) + ain(i3-1,j-1,k+1) + &
                          ain(i3-1,j+1,k  ) + ain(i3-1,j+1,k-1) + ain(i3-1,j+1,k+1) + &
                          ain(i3+1,j  ,k  ) + ain(i3+1,j  ,k-1) + ain(i3+1,j  ,k+1) + &
                          ain(i3+1,j-1,k  ) + ain(i3+1,j-1,k-1) + ain(i3+1,j-1,k+1) + &
                          ain(i3+1,j+1,k  ) + ain(i3+1,j+1,k-1) + ain(i3+1,j+1,k+1)) 
          enddo
        enddo
      enddo


END SUBROUTINE
