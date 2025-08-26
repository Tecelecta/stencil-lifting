SUBROUTINE jacobi_2d_5pt(ain, aout)
      IMPLICIT NONE
      real(kind=8) :: ain(258,258)
      real(kind=8) :: aout(258,258)

      integer :: i,j
      integer :: tj, sj
      integer :: ti, si

      do ti = 2, 258-1, 16
        do tj = 2, 258-1, 16
            do si = 0, 15
              i = ti + si
              do sj = 0, 15
                j = tj + sj
                aout(i,j) = 0.25_8 * ( ain(i+1,j) + ain(i-1,j) + &
                                      ain(i,j+1) + ain(i,j-1) )
              enddo
            enddo
        enddo
      enddo

END SUBROUTINE