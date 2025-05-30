MODULE field_summary_kernel_loop81_mod
CONTAINS
SUBROUTINE field_summary_kernel_loop81(j,jv,k,kv,cell_mass,cell_vol,density0,energy0,ie,ke,mass,press,pressure,vol,volume,vsqrd,x_max,x_min,xvel0,y_max,y_min,yvel0)
INTEGER :: j
INTEGER :: jv
INTEGER :: k
INTEGER :: kv
REAL(kind=8) :: cell_mass
REAL(kind=8) :: cell_vol
REAL(kind=8) :: ie
REAL(kind=8) :: ke
REAL(kind=8) :: mass
REAL(kind=8) :: press
REAL(kind=8) :: vol
REAL(kind=8) :: vsqrd
INTEGER :: x_max
INTEGER :: x_min
INTEGER :: y_max
INTEGER :: y_min
REAL(kind=8), DIMENSION((x_min - 2):(x_max + 2),(y_min - 2):(y_max + 2)) :: density0
REAL(kind=8), DIMENSION((x_min - 2):(x_max + 2),(y_min - 2):(y_max + 2)) :: energy0
REAL(kind=8), DIMENSION((x_min - 2):(x_max + 2),(y_min - 2):(y_max + 2)) :: pressure
REAL(kind=8), DIMENSION((x_min - 2):(x_max + 2),(y_min - 2):(y_max + 2)) :: volume
REAL(kind=8), DIMENSION((x_min - 2):(x_max + 3),(y_min - 2):(y_max + 3)) :: xvel0
REAL(kind=8), DIMENSION((x_min - 2):(x_max + 3),(y_min - 2):(y_max + 3)) :: yvel0
DO k = y_min, y_max
DO j = x_min, x_max
vsqrd = 0.0
DO kv = k, k + 1
DO jv = j, j + 1
vsqrd = vsqrd + 0.25 * (xvel0(jv,kv) ** 2 + yvel0(jv,kv) ** 2)
END DO
END DO
cell_vol = volume(j,k)
cell_mass = cell_vol * density0(j,k)
vol = vol + cell_vol
mass = mass + cell_mass
ie = ie + cell_mass * energy0(j,k)
ke = ke + cell_mass * 0.5 * vsqrd
press = press + cell_vol * pressure(j,k)
END DO
END DO
END SUBROUTINE 

END MODULE field_summary_kernel_loop81_mod

