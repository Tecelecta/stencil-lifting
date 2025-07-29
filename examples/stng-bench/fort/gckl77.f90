SUBROUTINE generate_chunk_kernel_loop77(j,k,energy0,number_of_states,state_energy,x_max,x_min,y_max,y_min)
  INTEGER :: j
  INTEGER :: k
  INTEGER :: number_of_states
  INTEGER :: x_max
  INTEGER :: x_min
  INTEGER :: y_max
  INTEGER :: y_min
  REAL(kind=8), DIMENSION((x_min - 2):(x_max + 2),(y_min - 2):(y_max + 2)) :: energy0
  REAL(kind=8), DIMENSION(number_of_states) :: state_energy
    ! State 1 is always the background state
  DO k = y_min - 2, y_max + 2
    DO j = x_min - 2, x_max + 2
      energy0(j,k) = state_energy(1)
    END DO
  END DO
END SUBROUTINE 

