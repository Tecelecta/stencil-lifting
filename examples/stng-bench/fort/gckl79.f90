SUBROUTINE generate_chunk_kernel_loop79(number_of_states,state_xvel,x_max,x_min,xvel0,y_max,y_min)
INTEGER :: j
INTEGER :: k
INTEGER :: number_of_states
INTEGER :: x_max
INTEGER :: x_min
INTEGER :: y_max
INTEGER :: y_min
REAL(kind=8), DIMENSION(number_of_states) :: state_xvel
REAL(kind=8), DIMENSION((x_min - 2):(x_max + 3),(y_min - 2):(y_max + 3)) :: xvel0
DO k = y_min - 2, y_max + 2
DO j = x_min - 2, x_max + 2
xvel0(j,k) = state_xvel(1)
END DO
END DO
END SUBROUTINE 
