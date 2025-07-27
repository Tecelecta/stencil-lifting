SUBROUTINE generate_chunk_kernel_loop80(number_of_states,state_yvel,x_max,x_min,y_max,y_min,yvel0)
INTEGER :: j
INTEGER :: k
INTEGER :: number_of_states
INTEGER :: x_max
INTEGER :: x_min
INTEGER :: y_max
INTEGER :: y_min
REAL(kind=8), DIMENSION(number_of_states) :: state_yvel
REAL(kind=8), DIMENSION((x_min - 2):(x_max + 3),(y_min - 2):(y_max + 3)) :: yvel0
DO k = y_min - 2, y_max + 2
DO j = x_min - 2, x_max + 2
yvel0(j,k) = state_yvel(1)
END DO
END DO
END SUBROUTINE 