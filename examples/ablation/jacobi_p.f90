SUBROUTINE jacobi_2d_5pt(ain, aout)
      IMPLICIT NONE
      real(kind=8) :: ain(258,258)
      real(kind=8) :: aout(258,258)

      integer :: i,j
      !$OMP PARALLEL
      !$OMP DO
      do i=2,258-1
        do j=2,258-1
            aout(i,j) = 0.25_8 * ( ain(i+1,j) + ain(i-1,j) + &
                                  ain(i,j+1) + ain(i,j-1) )
        enddo
      enddo
      !$OMP END DO
      !$OMP END PARALLEL
END SUBROUTINE