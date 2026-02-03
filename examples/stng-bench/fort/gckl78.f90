SUBROUTINE generate_chunk_kernel_loop78(density0,number_of_states,state_density,x_max,x_min,y_max,y_min)
INTEGER :: j
INTEGER :: k
INTEGER :: number_of_states
INTEGER :: x_max
INTEGER :: x_min
INTEGER :: y_max
INTEGER :: y_min
REAL(kind=8), DIMENSION((x_min - 2):(x_max + 2),(y_min - 2):(y_max + 2)) :: density0
REAL(kind=8), DIMENSION(number_of_states) :: state_density
DO k = y_min - 2, y_max + 2
DO j = x_min - 2, x_max + 2
density0(j,k) = state_density(1)
END DO
END DO
END SUBROUTINE 