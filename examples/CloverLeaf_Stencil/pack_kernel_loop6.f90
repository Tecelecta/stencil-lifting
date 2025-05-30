MODULE pack_kernel_loop6_mod
CONTAINS
SUBROUTINE pack_kernel_loop6(j,k,bottom_rcv_buffer,depth,field,index,x_inc,x_max,x_min,y_min)
INTEGER :: j
INTEGER :: k
INTEGER :: depth
INTEGER :: index
INTEGER :: x_inc
INTEGER :: x_max
INTEGER :: x_min
INTEGER :: y_min
REAL(kind=8), DIMENSION(:) :: bottom_rcv_buffer
REAL(kind=8), DIMENSION((-1):,(-1):) :: field
DO k = 1, depth
DO j = x_min - depth, x_max + x_inc + depth
index = j + depth + (k - 1) * (x_max + x_inc + (2 * depth))
field(j,y_min - k) = bottom_rcv_buffer(index)
END DO
END DO
END SUBROUTINE 

END MODULE pack_kernel_loop6_mod

