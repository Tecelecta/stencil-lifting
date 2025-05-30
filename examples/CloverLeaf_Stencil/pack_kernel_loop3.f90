MODULE pack_kernel_loop3_mod
CONTAINS
SUBROUTINE pack_kernel_loop3(j,k,depth,field,index,right_rcv_buffer,x_inc,x_max,y_inc,y_max,y_min)
INTEGER :: j
INTEGER :: k
INTEGER :: depth
INTEGER :: index
INTEGER :: x_inc
INTEGER :: x_max
INTEGER :: y_inc
INTEGER :: y_max
INTEGER :: y_min
REAL(kind=8), DIMENSION((-1):,(-1):) :: field
REAL(kind=8), DIMENSION(:) :: right_rcv_buffer
DO k = y_min - depth, y_max + y_inc + depth
DO j = 1, depth
index = j + (k + depth - 1) * depth
field(x_max + x_inc + j,k) = right_rcv_buffer(index)
END DO
END DO
END SUBROUTINE 

END MODULE pack_kernel_loop3_mod

