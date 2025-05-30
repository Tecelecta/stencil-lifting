MODULE mg_loop2_mod
CONTAINS
SUBROUTINE mg_loop2(k,lt,maxlevel,ng,nx,ny,nz)
INTEGER :: k
INTEGER :: lt
INTEGER :: maxlevel
INTEGER, DIMENSION(3,10) :: ng
INTEGER, DIMENSION(100) :: nx
INTEGER, DIMENSION(100) :: ny
INTEGER, DIMENSION(100) :: nz
DO k = 0, 1, -1
nx(k) = ng(1,k)
ny(k) = ng(2,k)
nz(k) = ng(3,k)
END DO
END SUBROUTINE 

END MODULE mg_loop2_mod

