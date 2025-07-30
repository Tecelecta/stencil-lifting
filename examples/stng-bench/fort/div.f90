SUBROUTINE stencil(ain1, ain2, ain3, aout)
      IMPLICIT NONE
      real(kind=8) :: ain1(258,258,258)
      real(kind=8) :: ain2(258,258,258)
      real(kind=8) :: ain3(258,258,258)
      real(kind=8) :: aout(258,258,258)

      integer :: i,j,k

      do k=2,258-1
        do j=2,258-1
          do i=2,258-1
            aout(i,j,k) = ain1(i,j,k) + (1.0_8/6.0_8) * ( ain1(i+1,j,k) + &
                                         ain1(i-1,j,k) ) + &
                                       (2.0_8/6.0_8) * ( ain2(i,j+1,k) + &
                                         ain2(i,j-1,k) ) + &
                                       (3.0_8/6.0_8) * ( ain3(i,j,k+1) + &
                                         ain3(i,j,k-1) )
          enddo
        enddo
      enddo

END SUBROUTINE
