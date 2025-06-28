SUBROUTINE mg_loop3(m1,m2,m3,mi,ng,lt)
INTEGER :: ax
INTEGER :: k
INTEGER :: lt
REAL(kind=8), DIMENSION(lt) :: m1
REAL(kind=8), DIMENSION(lt) :: m2
REAL(kind=8), DIMENSION(lt) :: m3
REAL(kind=8), DIMENSION(3,lt) :: mi
REAL(kind=8), DIMENSION(3,lt) :: ng
DO k = lt, 1, -1
    DO ax = 1, 3
        mi(ax,k) = 2 + ng(ax,k)
    END DO
    m1(k) = mi(1,k)
    m2(k) = mi(2,k)
    m3(k) = mi(3,k)
END DO
END SUBROUTINE 