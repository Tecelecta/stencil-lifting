MODULE pack_kernel_loop7_mod
CONTAINS
SUBROUTINE pack_kernel_loop7(j,k,depth,field,index,top_rcv_buffer,x_inc,x_max,x_min,y_inc,y_max)
INTEGER :: j
INTEGER :: k
INTEGER :: depth
INTEGER :: index
INTEGER :: x_inc
INTEGER :: x_max
INTEGER :: x_min
INTEGER :: y_inc
INTEGER :: y_max
REAL(kind=8), DIMENSION((-1):,(-1):) :: field
REAL(kind=8), DIMENSION(:) :: top_rcv_buffer
DO k = 1, depth
DO j = x_min - depth, x_max + x_inc + depth
index = j + depth + (k - 1) * (x_max + x_inc + (2 * depth))
field(j,y_max + y_inc + k) = top_rcv_buffer(index)
END DO
END DO
END SUBROUTINE 

END MODULE pack_kernel_loop7_mod

