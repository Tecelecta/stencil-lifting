MODULE mg_loop0_mod
CONTAINS
SUBROUTINE mg_loop0(i,debug_default,debug_vec)
INTEGER :: i
INTEGER :: debug_default
INTEGER, DIMENSION(0:7) :: debug_vec
DO i = 0, 7
debug_vec(i) = debug_default
END DO
END SUBROUTINE 

END MODULE mg_loop0_mod

