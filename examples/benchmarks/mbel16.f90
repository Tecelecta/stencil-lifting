SUBROUTINE theta_and_thetam_lbc_only (new_t_bdy_xe, new_t_bdy_tend_xe,&
                              theta_to_thetam,                       &
                              mub,                                   &
                              mu_bdy_xe,                  &
                              mu_bdy_tend_xe,        &
                              moist_bdy_xe,            &
                              moist_bdy_tend_xe,  &
                              t_bdy_xe, t_bdy_tend_xe, &
                              its,ite, jts,jte, kts,kte              )

   IMPLICIT NONE

   !  This routine is called from the solve_em routine.  The purpose is to
   !  convert the thermal lateral boundary conditions between dry potential
   !  temperature and moist potential temperature.  The first argument is a
   !  flag telling us the direction of the conversion:
   !     True  = convert dry to moist potential temp
   !     False = convert moist to dry potential temp

   LOGICAL,      INTENT(IN   )    :: theta_to_thetam

   INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte

   REAL,  DIMENSION( its:ite ,           jts:jte        ), INTENT(IN   ) :: mub

   REAL,  DIMENSION( jts:jte , 1       , its:ite ), INTENT(IN   ) :: mu_bdy_xe
   REAL,  DIMENSION( jts:jte , 1       , its:ite ), INTENT(IN   ) :: mu_bdy_tend_xe

   REAL,  DIMENSION( jts:jte , kts:kte , its:ite ), INTENT(IN   ) :: moist_bdy_xe
   REAL,  DIMENSION( jts:jte , kts:kte , its:ite ), INTENT(IN   ) :: moist_bdy_tend_xe

   REAL,  DIMENSION( jts:jte , kts:kte , its:ite ) ::  t_bdy_xe
   REAL,  DIMENSION( jts:jte , kts:kte , its:ite ) ::  t_bdy_tend_xe

   REAL,  DIMENSION( jts:jte , kts:kte , its:ite ), INTENT(OUT) ::  new_t_bdy_xe
   REAL,  DIMENSION( jts:jte , kts:kte , its:ite ), INTENT(OUT) ::  new_t_bdy_tend_xe

   ! Param
   REAl :: T0 = 0.5
   REAL :: R_v = 0.25, R_d = 0.25
   REAL :: dt_interval = 1120.0

   !  Local variables


   INTEGER    :: i, j, k 

   REAL ::  mu_old_bdy_xe
   REAL ::  mu_new_bdy_xe

   REAL :: t_old_bdy_xe
   REAL :: t_new_bdy_xe

   REAL :: moist_old_bdy_xe
   REAL :: moist_new_bdy_xe

   !  IF      (       theta_to_thetam ) THEN
   !     Convert dry potential temperature to theta_m
   !     Defined as: theta_m = ( theta   + T0 ) * ( 1. + (R_v/R_d) Qv ) - T0
   !  ELSE
   !     Convert dry potential temperature to theta_m
   !     Defined as: theta   = ( theta_m + T0 ) / ( 1. + (R_v/R_d) Qv ) - T0
   !  END IF

   !  We want the current value and the tendency, using information mostly
   !  from the lateral boundary file.  In that file, the thermal variable
   !  is a potential temperature with the T0 offset removed (theta-300).  Both
   !  the moisture variable and the potential temperature are coupled
   !  (multiplied by total dry column pressure).  And to add one more complication,
   !  the MU variable in the lateral boundary array is perturbation only.

   !  Since we need to end up with lateral boundary values that are coupled,
   !  we need to first DECOUPLE T and Qv, compute Tm, and then couple that.  As
   !  there is a need for the lateral tendency also, we compute the T and Qv
   !  values at the two boundary times (previous/current and next).  These two
   !  times are adequate to get us a tendency.  For the tendency, we need to have
   !  coupled values for the T (or Tm) at both times, which gives us a coupled
   !  tendency.  We cannot have an uncoupled tendency and somehow multiply that
   !  by some intermediate/average column pressure.

   !  This routine's purpose is to manufacture a lateral boundary set of arrays
   !  (all eight of them) for the thermal field.  Depending on the logical flag
   !  passed in, this will either be dry potential temperature or moist potential
   !  temperature.

   !  The i_min, i_max for the south and north boundaries depends on if we are doing
   !  serial, OpenMP, or MPI.  For OpenMP, we do not want any overlap between tiles that
   !  are on the same task (either OpenMP only, or OpenMP+MPI).

   ! LOOP 16
   DO j = jts , jte
      DO k = kts , kte
         DO i = its, ite
            mu_old_bdy_xe         =   mu_bdy_xe(i,1,j)    + mub(i,j)
            t_old_bdy_xe          = ( t_bdy_xe(i,k,j)                                            ) / mu_old_bdy_xe
            moist_old_bdy_xe      = ( moist_bdy_xe(i,k,j)                                        ) / mu_old_bdy_xe
            mu_new_bdy_xe         =   mu_old_bdy_xe       + mu_bdy_tend_xe(i,1,j)   *dt_interval
            t_new_bdy_xe          = ( t_bdy_xe(i,k,j)     + t_bdy_tend_xe(i,k,j)    *dt_interval ) / mu_new_bdy_xe
            moist_new_bdy_xe      = ( moist_bdy_xe(i,k,j) + moist_bdy_tend_xe(i,k,j)*dt_interval ) / mu_new_bdy_xe
            IF ( theta_to_thetam ) THEN
               new_t_bdy_xe(i,k,j) = ( ( ( t_old_bdy_xe + T0 ) * ( 1. + (R_v/R_d) * moist_old_bdy_xe ) ) - T0 ) * mu_old_bdy_xe
               new_t_bdy_tend_xe(i,k,j) = ( ( mu_new_bdy_xe * ( ( t_new_bdy_xe + T0 ) * &
                                          ( 1. + (R_v/R_d) * moist_new_bdy_xe ) - T0 ) ) - &
                                          ( mu_old_bdy_xe * ( ( t_old_bdy_xe + T0 ) * &
                                          ( 1. + (R_v/R_d) * moist_old_bdy_xe ) - T0 ) ) ) / dt_interval
            ELSE
               new_t_bdy_xe(i,k,j) = ( ( ( t_old_bdy_xe + T0 ) / ( 1. + (R_v/R_d) * moist_old_bdy_xe ) ) - T0 ) * mu_old_bdy_xe
               new_t_bdy_tend_xe(i,k,j) = ( ( mu_new_bdy_xe * ( ( t_new_bdy_xe + T0 ) / &
                                          ( 1. + (R_v/R_d) * moist_new_bdy_xe ) - T0 ) ) - &
                                          ( mu_old_bdy_xe * ( ( t_old_bdy_xe + T0 ) / &
                                          ( 1. + (R_v/R_d) * moist_old_bdy_xe ) - T0 ) ) ) / dt_interval
            END IF
         END DO
      END DO
   END DO

END SUBROUTINE theta_and_thetam_lbc_only