SUBROUTINE accelerate_kernel_loop81(density0,dt,nodal_mass,stepbymass,volume,x_max,x_min,y_max,y_min)
    INTEGER :: j
    INTEGER :: k
    REAL(kind=8) :: dt
    REAL(kind=8) :: nodal_mass
    INTEGER :: x_max
    INTEGER :: x_min
    INTEGER :: y_max
    INTEGER :: y_min
    ! Arrays use 0-based indexing to match C/Halide layout
    REAL(kind=8), DIMENSION(0:(x_max - x_min + 4),0:(y_max - y_min + 4)) :: density0
    REAL(kind=8), DIMENSION(0:(x_max - x_min + 4),0:(y_max - y_min + 5)) :: stepbymass
    REAL(kind=8), DIMENSION(0:(x_max - x_min + 4),0:(y_max - y_min + 4)) :: volume

    DO k = y_min, y_max + 1
       DO j = x_min, x_max + 1
          ! Access with offset to match 0-based array indexing
          nodal_mass = (density0(j - 1 + 2,k - 1 + 2) * volume(j - 1 + 2,k - 1 + 2) &
             + density0(j + 2,k - 1 + 2) * volume(j + 2,k - 1 + 2) + &
             density0(j + 2,k + 2) * volume(j + 2,k + 2) + density0(j - 1 + 2,k + 2) * volume(j - 1 + 2,k + 2)) * 0.25_8
          stepbymass(j + 2,k + 2) = 0.5_8 * dt / nodal_mass
       END DO
    END DO
END SUBROUTINE 