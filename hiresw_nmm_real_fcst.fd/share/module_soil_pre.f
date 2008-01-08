MODULE module_soil_pre

CONTAINS

   SUBROUTINE process_percent_cat ( xland , &
                                landuse_frac_input , soil_top_cat_input , soil_bot_cat_input , &
                                isltyp_input , ivgtyp_input , &
                                num_veg_cat , num_soil_top_cat , num_soil_bot_cat , &
                                ids , ide , jds , jde , kds , kde , &
                                ims , ime , jms , jme , kms , kme , &
                                its , ite , jts , jte , kts , kte , &
                                iswater )

      IMPLICIT NONE


      INTEGER , INTENT(IN) :: ids , ide , jds , jde , kds , kde , &
                              ims , ime , jms , jme , kms , kme , &
                              its , ite , jts , jte , kts , kte , &
                              iswater
      INTEGER , INTENT(IN) :: num_veg_cat , num_soil_top_cat , num_soil_bot_cat
      REAL , DIMENSION(its:ite,jts:jte,1:num_veg_cat) , INTENT(IN):: landuse_frac_input 
      REAL , DIMENSION(its:ite,jts:jte,1:num_soil_top_cat) , INTENT(IN):: soil_top_cat_input
      REAL , DIMENSION(its:ite,jts:jte,1:num_soil_bot_cat) , INTENT(IN):: soil_bot_cat_input
      INTEGER , DIMENSION(its:ite,jts:jte), INTENT(OUT) :: isltyp_input , ivgtyp_input
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(OUT) :: xland

      INTEGER :: i , j , l , dominant_index
      REAL :: dominant_value

      INTEGER , PARAMETER :: iswater_soil = 14
      INTEGER :: iforce

      !  Compute the dominant VEGETATION INDEX.

      DO i = its , MIN(ide-1,ite)
         DO j = jts , MIN(jde-1,jte)
            dominant_value = landuse_frac_input(i,j,1)
            dominant_index = 1
            DO l = 2 , num_veg_cat
               IF      ( ( l .EQ. iswater ) .AND. ( landuse_frac_input(i,j,l) .GT.            0.5 ) ) THEN
                  dominant_value = soil_top_cat_input(i,j,l)
                  dominant_index = l
               ELSE IF ( ( l .NE. iswater ) .AND. ( landuse_frac_input(i,j,l) .GT. dominant_value ) ) THEN
                  dominant_value = landuse_frac_input(i,j,l)
                  dominant_index = l
               END IF
            END DO
            IF      ( dominant_index .EQ. iswater ) THEN
               xland(i,j) = 2.
            ELSE IF ( dominant_index .NE. iswater ) THEN
               xland(i,j) = 1.
            END IF
            ivgtyp_input(i,j) = dominant_index
         END DO
      END DO

      !  Compute the dominant SOIL TEXTURE INDEX, TOP.

      iforce = 0
      DO i = its , MIN(ide-1,ite)
         DO j = jts , MIN(jde-1,jte)
            dominant_value = soil_top_cat_input(i,j,1)
            dominant_index = 1
            IF ( xland(i,j) .LT. 1.5 ) THEN
               DO l = 2 , num_soil_top_cat
                  IF ( ( l .NE. iswater_soil ) .AND. ( soil_top_cat_input(i,j,l) .GT. dominant_value ) ) THEN
                     dominant_value = soil_top_cat_input(i,j,l)
                     dominant_index = l
                  END IF
               END DO
               IF ( dominant_value .LT. 0.01 ) THEN
                  iforce = iforce + 1
!print *,forcing a soil value over land
!print *,iforce,NINT(soil_top_cat_input(i,j,:))
                  dominant_index = 8
               END IF
            ELSE
               dominant_index = iswater_soil
            END IF
            isltyp_input(i,j) = dominant_index
         END DO
      END DO
if(iforce.ne.0)then
print *,'forcing artificial silty clay loam at ',iforce,' points, out of ',(MIN(ide-1,ite)-its+1)*(MIN(jde-1,jte)-jts+1)
endif

   END SUBROUTINE process_percent_cat

   SUBROUTINE process_percent_cat_new ( xland , &
                                landuse_frac , soil_top_cat , soil_bot_cat , &
                                isltyp , ivgtyp , &
                                num_veg_cat , num_soil_top_cat , num_soil_bot_cat , &
                                ids , ide , jds , jde , kds , kde , &
                                ims , ime , jms , jme , kms , kme , &
                                its , ite , jts , jte , kts , kte , &
                                iswater )

      IMPLICIT NONE

      INTEGER , INTENT(IN) :: ids , ide , jds , jde , kds , kde , &
                              ims , ime , jms , jme , kms , kme , &
                              its , ite , jts , jte , kts , kte , &
                              iswater
      INTEGER , INTENT(IN) :: num_veg_cat , num_soil_top_cat , num_soil_bot_cat
      REAL , DIMENSION(ims:ime,1:num_veg_cat,jms:jme) , INTENT(IN):: landuse_frac
      REAL , DIMENSION(ims:ime,1:num_soil_top_cat,jms:jme) , INTENT(IN):: soil_top_cat
      REAL , DIMENSION(ims:ime,1:num_soil_bot_cat,jms:jme) , INTENT(IN):: soil_bot_cat
      INTEGER , DIMENSION(ims:ime,jms:jme), INTENT(OUT) :: isltyp , ivgtyp
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(OUT) :: xland

      INTEGER :: i , j , l , dominant_index
      REAL :: dominant_value

      INTEGER , PARAMETER :: iswater_soil = 14
      INTEGER :: iforce

      !  Compute the dominant VEGETATION INDEX.

      DO j = jts , MIN(jde-1,jte)
         DO i = its , MIN(ide-1,ite)
            dominant_value = landuse_frac(i,1,j)
            dominant_index = 1

            DO l = 2 , num_veg_cat
               IF      ( ( l .EQ. iswater ) .AND. ( landuse_frac(i,l,j) .GT.            0.5 ) ) THEN


!dontunderstand                  dominant_value = soil_top_cat(i,l,j)
                  dominant_value = landuse_frac(i,l,j)
                  dominant_index = l
               ELSE IF ( ( l .NE. iswater ) .AND. ( landuse_frac(i,l,j) .GT. dominant_value ) ) THEN
                  dominant_value = landuse_frac(i,l,j)
                  dominant_index = l
               END IF
            END DO

            IF      ( dominant_index .EQ. iswater ) THEN
               xland(i,j) = 2.
            ELSE IF ( dominant_index .NE. iswater ) THEN
               xland(i,j) = 1.
            END IF
            ivgtyp(i,j) = dominant_index
         END DO
      END DO

      !  Compute the dominant SOIL TEXTURE INDEX, TOP.

      iforce = 0
      DO i = its , MIN(ide-1,ite)
         DO j = jts , MIN(jde-1,jte)
            dominant_value = soil_top_cat(i,1,j)
            dominant_index = 1
            IF ( xland(i,j) .LT. 1.5 ) THEN
               DO l = 2 , num_soil_top_cat
                  IF ( ( l .NE. iswater_soil ) .AND. ( soil_top_cat(i,l,j) .GT. dominant_value ) ) THEN
                     dominant_value = soil_top_cat(i,l,j)
                     dominant_index = l
                  END IF
               END DO
               IF ( dominant_value .LT. 0.01 ) THEN
                  iforce = iforce + 1
!print *,forcing a soil value over land
!print *,iforce,NINT(soil_top_cat(i,:,j))
                  dominant_index = 8
               END IF
            ELSE
               dominant_index = iswater_soil
            END IF
            isltyp(i,j) = dominant_index
         END DO
      END DO
if(iforce.ne.0)then
print *,'forcing artificial silty clay loam at ',iforce,' points, out of ',(MIN(ide-1,ite)-its+1)*(MIN(jde-1,jte)-jts+1)
endif

   END SUBROUTINE process_percent_cat_new

   SUBROUTINE process_soil_real ( tsk , tmn , xland , &
                                landmask , sst , &
                                st_input , sm_input , sw_input , st_levels_input , sm_levels_input , sw_levels_input , &
                                zs , dzs , tslb , smois , &
                                flag_sst , &
                                ids , ide , jds , jde , kds , kde , &
                                ims , ime , jms , jme , kms , kme , &
                                its , ite , jts , jte , kts , kte , &
                                bl_surface_physics , num_soil_layers , real_data_init_type , &
                                num_st_levels_input , num_sm_levels_input , num_sw_levels_input , &
                                num_st_levels_alloc , num_sm_levels_alloc , num_sw_levels_alloc )

      IMPLICIT NONE

      INTEGER , INTENT(IN) :: ids , ide , jds , jde , kds , kde , &
                              ims , ime , jms , jme , kms , kme , &
                              its , ite , jts , jte , kts , kte , &
                              bl_surface_physics , num_soil_layers , real_data_init_type , &
                              num_st_levels_input , num_sm_levels_input , num_sw_levels_input , &
                              num_st_levels_alloc , num_sm_levels_alloc , num_sw_levels_alloc 

      INTEGER , INTENT(IN) :: flag_sst

      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN) :: landmask , sst

      INTEGER , DIMENSION(1:num_st_levels_input) , INTENT(INOUT) :: st_levels_input
      INTEGER , DIMENSION(1:num_sm_levels_input) , INTENT(INOUT) :: sm_levels_input
      INTEGER , DIMENSION(1:num_sw_levels_input) , INTENT(INOUT) :: sw_levels_input
      REAL , DIMENSION(ims:ime,1:num_st_levels_alloc,jms:jme) , INTENT(INOUT) :: st_input
      REAL , DIMENSION(ims:ime,1:num_sm_levels_alloc,jms:jme) , INTENT(INOUT) :: sm_input
      REAL , DIMENSION(ims:ime,1:num_sw_levels_alloc,jms:jme) , INTENT(INOUT) :: sw_input

      REAL, DIMENSION(1:num_soil_layers), INTENT(OUT)  ::  zs,dzs
      REAL , DIMENSION(ims:ime,num_soil_layers,jms:jme) , INTENT(OUT) :: tslb , smois
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN) :: tmn , xland
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(INOUT) :: tsk

      INTEGER :: i , j , l , dominant_index , num_soil_cat , num_veg_cat
      REAL :: dominant_value

      !  Initialize the soil depth, and the soil temperature and moisture.
   
      IF      ( ( bl_surface_physics .EQ. 1 ) .AND. ( num_soil_layers .GT. 1 ) ) THEN
         CALL init_soil_depth_1 ( zs , dzs , num_soil_layers )
         CALL init_soil_1_real ( tsk , tmn , tslb , zs , dzs , num_soil_layers , real_data_init_type , &
                                 landmask , sst , flag_sst , &
                                 ids , ide , jds , jde , kds , kde , &
                                 ims , ime , jms , jme , kms , kme , &
                                 its , ite , jts , jte , kts , kte )

      ELSE IF ( ( bl_surface_physics .EQ. 2 .OR. bl_surface_physics .EQ. 99 ) &
               .AND. ( num_soil_layers .GT. 1 ) ) THEN

         CALL init_soil_depth_2 ( zs , dzs , num_soil_layers )
	write(6,*) 'calling with st_levels_input: ', st_levels_input
         CALL init_soil_2_real ( tsk , tmn , smois , tslb , &
                                 st_input , sm_input , sw_input , landmask , sst , &
                                 zs , dzs , &
                                 st_levels_input , sm_levels_input , sw_levels_input , &
                                 num_soil_layers , num_st_levels_input , num_sm_levels_input , num_sw_levels_input , &
                                 num_st_levels_alloc , num_sm_levels_alloc , num_sw_levels_alloc , &
                                 flag_sst , &
                                 ids , ide , jds , jde , kds , kde , &
                                 ims , ime , jms , jme , kms , kme , &
                                 its , ite , jts , jte , kts , kte )
!        CALL init_soil_old ( tsk , tmn , &
!                             smois , tslb , zs , dzs , num_soil_layers , &
!                             st000010_input , st010040_input , st040100_input , st100200_input , &
!                             st010200_input , &
!                             sm000010_input , sm010040_input , sm040100_input , sm100200_input , &
!                             sm010200_input , &
!                             landmask_input , sst_input , &
!                             ids , ide , jds , jde , kds , kde , &
!                             ims , ime , jms , jme , kms , kme , &
!                             its , ite , jts , jte , kts , kte )
      ELSE IF ( ( bl_surface_physics .EQ. 3 ) .AND. ( num_soil_layers .GT. 1 ) ) THEN
print *,"Tanya's stuff is not fixed yet for parallel real, still domain sized, etc."
stop
         CALL init_soil_depth_3 ( zs , dzs , num_soil_layers )
         CALL init_soil_3_real ( tsk , tmn , smois , tslb , &
                                 st_input , sm_input , landmask , sst , &
                                 zs , dzs , &
                                 st_levels_input , sm_levels_input , &
                                 num_soil_layers , num_st_levels_input , num_sm_levels_input , &
                                 flag_sst , &
                                 ids , ide , jds , jde , kds , kde , &
                                 ims , ime , jms , jme , kms , kme , &
                                 its , ite , jts , jte , kts , kte )
      END IF

   END SUBROUTINE process_soil_real

   SUBROUTINE process_soil_ideal ( xland,xice,vegfra,snow,canwat,  &
                                   ivgtyp,isltyp,tslb,smois, &
                                   tsk,tmn,zs,dzs,           &
                                   num_soil_layers,          &
                                   bl_surface_physics ,      &
                                   ids,ide, jds,jde, kds,kde,&
                                   ims,ime, jms,jme, kms,kme,&
                                   its,ite, jts,jte, kts,kte )

      IMPLICIT NONE

      INTEGER, INTENT(IN) ::ids,ide, jds,jde, kds,kde,  &
                            ims,ime, jms,jme, kms,kme,  &
                            its,ite, jts,jte, kts,kte
  
      INTEGER, INTENT(IN) :: num_soil_layers , bl_surface_physics

      REAL, DIMENSION( ims:ime, num_soil_layers, jms:jme ) , INTENT(INOUT) :: smois, tslb

      REAL, DIMENSION(num_soil_layers), INTENT(OUT) :: dzs,zs

      REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT) :: tsk, tmn
      REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(OUT) :: xland, snow, canwat, xice, vegfra
      INTEGER, DIMENSION( ims:ime, jms:jme ) , INTENT(OUT) :: ivgtyp, isltyp

      !  Local variables.

      INTEGER :: itf,jtf

      itf=MIN(ite,ide-1)
      jtf=MIN(jte,jde-1)

      IF      ( ( bl_surface_physics .EQ. 1 ) .AND. ( num_soil_layers .GT. 1 ) ) THEN
         CALL init_soil_depth_1 ( zs , dzs , num_soil_layers )
         CALL init_soil_1_ideal(tsk,tmn,tslb,xland,ivgtyp,zs,dzs,num_soil_layers,     &
                                ids,ide, jds,jde, kds,kde,               &
                                ims,ime, jms,jme, kms,kme,               &
                                its,ite, jts,jte, kts,kte                )
      ELSE IF ( ( bl_surface_physics .EQ. 2 ) .AND. ( num_soil_layers .GT. 1 ) ) THEN
         CALL init_soil_depth_2 ( zs , dzs , num_soil_layers )
         CALL init_soil_2_ideal ( xland,xice,vegfra,snow,canwat,         &
                                  ivgtyp,isltyp,tslb,smois,tmn,          &
                                  num_soil_layers,                       &
                                  ids,ide, jds,jde, kds,kde,             &
                                  ims,ime, jms,jme, kms,kme,             &
                                  its,ite, jts,jte, kts,kte              )
      END IF

   END SUBROUTINE process_soil_ideal

   SUBROUTINE adjust_soil_temp_new ( tmn , bl_surface_physics , &
                                 tsk , ter , toposoil , landmask , flag_toposoil , &
                                      st000010 ,      st010040 ,      st040100 ,      st100200 ,      st010200 , &
                                 flag_st000010 , flag_st010040 , flag_st040100 , flag_st100200 , flag_st010200 , & 
                                      soilt000 ,      soilt005 ,      soilt020 ,      soilt040 ,      soilt160 ,      soilt300 , &
                                 flag_soilt000 , flag_soilt005 , flag_soilt020 , flag_soilt040 , flag_soilt160 , flag_soilt300 , &
                                 ids , ide , jds , jde , kds , kde , &
                                 ims , ime , jms , jme , kms , kme , &
                                 its , ite , jts , jte , kts , kte )

      IMPLICIT NONE
   
      INTEGER , INTENT(IN) :: ids , ide , jds , jde , kds , kde , &
                              ims , ime , jms , jme , kms , kme , &
                              its , ite , jts , jte , kts , kte 

      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN)    :: ter , toposoil , landmask
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(INOUT) :: tmn , tsk
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(INOUT) :: st000010 , st010040 , st040100 , st100200 , st010200
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(INOUT) :: soilt000 , soilt005 , soilt020 , soilt040 , soilt160 , soilt300

      INTEGER , INTENT(IN) :: flag_st000010 , flag_st010040 , flag_st040100 , flag_st100200 , flag_st010200
      INTEGER , INTENT(IN) :: flag_soilt000 , flag_soilt005 , flag_soilt020 , flag_soilt040 , flag_soilt160 , flag_soilt300
      INTEGER , INTENT(IN) :: bl_surface_physics , flag_toposoil
 
      INTEGER :: i , j

      IF ( flag_toposoil .EQ. 1 ) THEN

         IF ( bl_surface_physics .EQ. 1 ) THEN
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  IF (landmask(i,j) .GT. 0.5 ) THEN
                     tmn(i,j) = tmn(i,j) - 0.0065 * ( ter(i,j) - toposoil(i,j) )
                  END IF
               END DO
            END DO
         END IF
   
         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               IF (landmask(i,j) .GT. 0.5 ) THEN
   
                  tsk(i,j) = tsk(i,j) - 0.0065 * ( ter(i,j) - toposoil(i,j) )
      
                  IF ( flag_st000010 .EQ. 1 ) THEN
                     st000010(i,j) = st000010(i,j) - 0.0065 * ( ter(i,j) - toposoil(i,j) )
                  END IF
                  IF ( flag_st010040 .EQ. 1 ) THEN
                     st010040(i,j) = st010040(i,j) - 0.0065 * ( ter(i,j) - toposoil(i,j) )
                  END IF
                  IF ( flag_st040100 .EQ. 1 ) THEN
                     st040100(i,j) = st040100(i,j) - 0.0065 * ( ter(i,j) - toposoil(i,j) )
                  END IF
                  IF ( flag_st100200 .EQ. 1 ) THEN
                     st100200(i,j) = st100200(i,j) - 0.0065 * ( ter(i,j) - toposoil(i,j) )
                  END IF
                  IF ( flag_st010200 .EQ. 1 ) THEN
                     st010200(i,j) = st010200(i,j) - 0.0065 * ( ter(i,j) - toposoil(i,j) )
                  END IF
      
                  IF ( flag_soilt000 .EQ. 1 ) THEN
                     soilt000(i,j) = soilt000(i,j) - 0.0065 * ( ter(i,j) - toposoil(i,j) )
                  END IF
                  IF ( flag_soilt005 .EQ. 1 ) THEN
                     soilt005(i,j) = soilt005(i,j) - 0.0065 * ( ter(i,j) - toposoil(i,j) )
                  END IF
                  IF ( flag_soilt020 .EQ. 1 ) THEN
                     soilt020(i,j) = soilt020(i,j) - 0.0065 * ( ter(i,j) - toposoil(i,j) )
                  END IF
                  IF ( flag_soilt040 .EQ. 1 ) THEN
                     soilt040(i,j) = soilt040(i,j) - 0.0065 * ( ter(i,j) - toposoil(i,j) )
                  END IF
                  IF ( flag_soilt160 .EQ. 1 ) THEN
                     soilt160(i,j) = soilt160(i,j) - 0.0065 * ( ter(i,j) - toposoil(i,j) )
                  END IF
                  IF ( flag_soilt300 .EQ. 1 ) THEN
                     soilt300(i,j) = soilt300(i,j) - 0.0065 * ( ter(i,j) - toposoil(i,j) )
                  END IF
   
               END IF
            END DO
         END DO

      END IF

   END SUBROUTINE adjust_soil_temp_new

   SUBROUTINE adjust_soil_temp ( tmn , bl_surface_physics , &
                                 tsk , t_annual_avg_input , ter_input , toposoil_input , &
                                 st000010_input , st010040_input , st040100_input , st100200_input , st010200_input , &
                                 flag_st000010 , flag_st010040 , flag_st040100 , flag_st100200 , flag_st010200 , & 
                                 ids , ide , jds , jde , kds , kde , &
                                 ims , ime , jms , jme , kms , kme , &
                                 its , ite , jts , jte , kts , kte )

      IMPLICIT NONE
   
      INTEGER , INTENT(IN) :: ids , ide , jds , jde , kds , kde , &
                              ims , ime , jms , jme , kms , kme , &
                              its , ite , jts , jte , kts , kte 

      REAL , DIMENSION(its:ite,jts:jte) , INTENT(IN) :: ter_input , toposoil_input
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(INOUT) :: tmn , tsk
      REAL , DIMENSION(its:ite,jts:jte) , INTENT(INOUT) :: t_annual_avg_input , &
                              st000010_input , st010040_input , st040100_input , st100200_input , st010200_input
      LOGICAL , INTENT(IN) :: flag_st000010 , flag_st010040 , flag_st040100 , flag_st100200 , flag_st010200
      INTEGER , INTENT(IN) :: bl_surface_physics
 
      INTEGER :: i , j

      IF ( bl_surface_physics .EQ. 1 ) THEN
         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               tmn(i,j) = tmn(i,j) - 0.0065 * ( ter_input(i,j) - toposoil_input(i,j) )
            END DO
         END DO
      END IF

      DO j = jts , MIN(jde-1,jte)
         DO i = its , MIN(ide-1,ite)
            tsk(i,j) = tsk(i,j) - 0.0065 * ( ter_input(i,j) - toposoil_input(i,j) )
!           t_annual_avg_input(i,j) = t_annual_avg_input(i,j) - 0.0065 * ter_input(i,j) ! handled by SI
            IF ( flag_st000010 ) THEN
               st000010_input(i,j) = st000010_input(i,j) - 0.0065 * ( ter_input(i,j) - toposoil_input(i,j) )
            END IF
            IF ( flag_st010040 ) THEN
               st010040_input(i,j) = st010040_input(i,j) - 0.0065 * ( ter_input(i,j) - toposoil_input(i,j) )
            END IF
            IF ( flag_st040100 ) THEN
               st040100_input(i,j) = st040100_input(i,j) - 0.0065 * ( ter_input(i,j) - toposoil_input(i,j) )
            END IF
            IF ( flag_st100200 ) THEN
               st100200_input(i,j) = st100200_input(i,j) - 0.0065 * ( ter_input(i,j) - toposoil_input(i,j) )
            END IF
            IF ( flag_st010200 ) THEN
               st010200_input(i,j) = st010200_input(i,j) - 0.0065 * ( ter_input(i,j) - toposoil_input(i,j) )
            END IF
         END DO
      END DO

   END SUBROUTINE adjust_soil_temp

   SUBROUTINE adjust_soil_temp_3 ( tmn , bl_surface_physics , &
                                 tsk , t_annual_avg_input , ter_input , toposoil_input , &
      soilt000_input , soilt005_input , soilt020_input , soilt040_input , soilt160_input , soilt300_input , &
      flag_soilt000 , flag_soilt005 , flag_soilt020 , flag_soilt040 , flag_soilt160 , flag_soilt300 , &
                                 ids , ide , jds , jde , kds , kde , &
                                 ims , ime , jms , jme , kms , kme , &
                                 its , ite , jts , jte , kts , kte )

      IMPLICIT NONE
   
      INTEGER , INTENT(IN) :: ids , ide , jds , jde , kds , kde , &
                              ims , ime , jms , jme , kms , kme , &
                              its , ite , jts , jte , kts , kte 

      REAL , DIMENSION(its:ite,jts:jte) , INTENT(IN) :: ter_input , toposoil_input
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(INOUT) :: tmn , tsk
      REAL , DIMENSION(its:ite,jts:jte) , INTENT(INOUT) :: t_annual_avg_input , &
      soilt000_input , soilt005_input , soilt020_input , soilt040_input , soilt160_input , soilt300_input
      LOGICAL , INTENT(IN) :: flag_soilt000 , flag_soilt005 , flag_soilt020 , flag_soilt040 , flag_soilt160 , flag_soilt300
      INTEGER , INTENT(IN) :: bl_surface_physics
 
      INTEGER :: i , j

      DO j = jts , MIN(jde-1,jte)
         DO i = its , MIN(ide-1,ite)
            tsk(i,j) = tsk(i,j) - 0.0065 * ( ter_input(i,j) - toposoil_input(i,j) )
!           t_annual_avg_input(i,j) = t_annual_avg_input(i,j) - 0.0065 * ter_input(i,j) ! handled by SI
            IF ( flag_soilt000 ) THEN
               soilt000_input(i,j) = soilt000_input(i,j) - 0.0065 * ( ter_input(i,j) - toposoil_input(i,j) )
            END IF
            IF ( flag_soilt005 ) THEN
               soilt005_input(i,j) = soilt005_input(i,j) - 0.0065 * ( ter_input(i,j) - toposoil_input(i,j) )
            END IF
            IF ( flag_soilt020 ) THEN
               soilt020_input(i,j) = soilt020_input(i,j) - 0.0065 * ( ter_input(i,j) - toposoil_input(i,j) )
            END IF
            IF ( flag_soilt040 ) THEN
               soilt040_input(i,j) = soilt040_input(i,j) - 0.0065 * ( ter_input(i,j) - toposoil_input(i,j) )
            END IF
            IF ( flag_soilt160 ) THEN
               soilt160_input(i,j) = soilt160_input(i,j) - 0.0065 * ( ter_input(i,j) - toposoil_input(i,j) )
            END IF
            IF ( flag_soilt300 ) THEN
               soilt300_input(i,j) = soilt300_input(i,j) - 0.0065 * ( ter_input(i,j) - toposoil_input(i,j) )
            END IF
         END DO
      END DO

   END SUBROUTINE adjust_soil_temp_3

   SUBROUTINE init_soil_depth_1 ( zs , dzs , num_soil_layers )

      IMPLICIT NONE
   
      INTEGER, INTENT(IN) :: num_soil_layers
   
      REAL, DIMENSION(1:num_soil_layers), INTENT(OUT)  ::  zs,dzs
   
      INTEGER                   ::      l

      !  Define layers (top layer = 0.01 m).  Double the thicknesses at each step (dzs values).
      !  The distance from the ground level to the midpoint of the layer is given by zs.

      !    -------   Ground Level   ----------      ||      ||   ||  || 
      !                                             ||      ||   ||  || zs(1) = 0.005 m
      !    --  --  --  --  --  --  --  --  --       ||      ||   ||  \/
      !                                             ||      ||   ||
      !    -----------------------------------      ||  ||  ||   \/   dzs(1) = 0.01 m
      !                                             ||  ||  || 
      !                                             ||  ||  || zs(2) = 0.02
      !    --  --  --  --  --  --  --  --  --       ||  ||  \/
      !                                             ||  ||
      !                                             ||  ||
      !    -----------------------------------  ||  ||  \/   dzs(2) = 0.02 m
      !                                         ||  || 
      !                                         ||  ||
      !                                         ||  || 
      !                                         ||  || zs(3) = 0.05
      !    --  --  --  --  --  --  --  --  --   ||  \/
      !                                         ||
      !                                         ||
      !                                         ||
      !                                         ||
      !    -----------------------------------  \/   dzs(3) = 0.04 m

      IF ( num_soil_layers .NE. 5 ) THEN
         PRINT '(A)','Usually, the 5-layer diffusion uses 5 layers.  Change this in the namelist.'
         STOP '5-layer_diffusion_uses_5_layers'
      END IF
   
      dzs(1)=.01
      zs(1)=.5*dzs(1)
   
      DO l=2,num_soil_layers
         dzs(l)=2*dzs(l-1)
         zs(l)=zs(l-1)+.5*dzs(l-1)+.5*dzs(l)
      ENDDO

   END SUBROUTINE init_soil_depth_1

   SUBROUTINE init_soil_depth_2 ( zs , dzs , num_soil_layers )

      IMPLICIT NONE
   
      INTEGER, INTENT(IN) :: num_soil_layers
   
      REAL, DIMENSION(1:num_soil_layers), INTENT(OUT)  ::  zs,dzs
   
      INTEGER                   ::      l

      dzs = (/ 0.1 , 0.3 , 0.6 , 1.0 /)

      IF ( num_soil_layers .NE. 4 ) THEN
         PRINT '(A)','Usually, the LSM uses 4 layers.  Change this in the namelist.'
         STOP 'LSM_uses_4_layers'
      END IF

      zs(1)=.5*dzs(1)
   
      DO l=2,num_soil_layers
         zs(l)=zs(l-1)+.5*dzs(l-1)+.5*dzs(l)
      ENDDO

   END SUBROUTINE init_soil_depth_2

   SUBROUTINE init_soil_depth_3 ( zs , dzs , num_soil_layers )

      IMPLICIT NONE
   
      INTEGER, INTENT(IN) :: num_soil_layers
   
      REAL, DIMENSION(1:num_soil_layers), INTENT(OUT)  ::  zs,dzs
   
      INTEGER                   ::      l

      zs  = (/ 0.00 , 0.05 , 0.20 , 0.40 , 1.60 , 3.00 /)
      dzs = (/ 0.00 , 0.125, 0.175 , 0.70 , 1.30 , 1.40 /)

      IF ( num_soil_layers .NE. 6 ) THEN
         PRINT '(A)','Usually, the RUC LSM uses 6 layers.  Change this in the namelist.'
         STOP 'LSM_uses_6_layers'
      END IF

   END SUBROUTINE init_soil_depth_3

   SUBROUTINE init_soil_1_real ( tsk , tmn , tslb , zs , dzs , &
                                 num_soil_layers , real_data_init_type , &
                                 landmask , sst , flag_sst , &
                                 ids , ide , jds , jde , kds , kde , &
                                 ims , ime , jms , jme , kms , kme , &
                                 its , ite , jts , jte , kts , kte )

      IMPLICIT NONE

      INTEGER , INTENT(IN) :: num_soil_layers , real_data_init_type , &
                              ids , ide , jds , jde , kds , kde , &
                              ims , ime , jms , jme , kms , kme , &
                              its , ite , jts , jte , kts , kte 

      INTEGER , INTENT(IN) :: flag_sst

      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN) :: landmask , sst
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN) :: tsk , tmn

      REAL , DIMENSION(num_soil_layers) :: zs , dzs

      REAL , DIMENSION(ims:ime,num_soil_layers,jms:jme) , INTENT(OUT) :: tslb

      INTEGER :: i , j , l

      !  Soil temperature is linearly interpolated between the skin temperature (taken to be at a
      !  depth of 0.5 cm) and the deep soil, annual temperature (taken to be at a depth of 23 cm).
      !  The tslb(i,1,j) is the skin temperature, and the tslb(i,num_soil_layers,j) level is the 
      !  annual mean temperature.

      DO j = jts , MIN(jde-1,jte)
         DO i = its , MIN(ide-1,ite)
            IF ( landmask(i,j) .GT. 0.5 ) THEN
               DO l = 1 , num_soil_layers
                  tslb(i,l,j)= ( tsk(i,j) * ( zs(num_soil_layers) - zs(l) )   + &
                                 tmn(i,j) * ( zs(              l) - zs(1) ) ) / &
                                            ( zs(num_soil_layers) - zs(1) )
               END DO
            ELSE
               IF ( ( real_data_init_type .EQ. 1 ) .AND. ( flag_sst .EQ. 1 ) ) THEN
                  DO l = 1 , num_soil_layers
                     tslb(i,l,j)= sst(i,j)
                  END DO
               ELSE
                  DO l = 1 , num_soil_layers
                     tslb(i,l,j)= tsk(i,j)
                  END DO
               END IF
            END IF
         END DO
      END DO

   END SUBROUTINE init_soil_1_real

   SUBROUTINE init_soil_1_ideal(tsk,tmn,tslb,xland,             &
                       ivgtyp,zs,dzs,num_soil_layers,           &
                       ids,ide, jds,jde, kds,kde,               &
                       ims,ime, jms,jme, kms,kme,               &
                       its,ite, jts,jte, kts,kte                )

      IMPLICIT NONE
   
      INTEGER, INTENT(IN   )    ::      ids,ide, jds,jde, kds,kde, &
                                        ims,ime, jms,jme, kms,kme, &
                                        its,ite, jts,jte, kts,kte
   
      INTEGER, INTENT(IN   )    ::      num_soil_layers
   
      REAL, DIMENSION( ims: , 1: , jms: ), INTENT(OUT) :: tslb
      REAL, DIMENSION( ims:ime , jms:jme ), INTENT(OUT) :: xland
      INTEGER, DIMENSION( ims:ime , jms:jme ), INTENT(OUT) :: ivgtyp
   
      REAL, DIMENSION(1:), INTENT(IN) :: dzs,zs
   
      REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(IN) :: tsk, tmn

      !  Lcal variables.
   
      INTEGER :: l,j,i,itf,jtf
   
      itf=MIN(ite,ide-1)
      jtf=MIN(jte,jde-1)
   
      IF (num_soil_layers.NE.1)THEN
         DO j=jts,jtf
            DO l=1,num_soil_layers
               DO i=its,itf
                 tslb(i,l,j)=( tsk(i,j)*(zs(num_soil_layers)-zs(l)) + tmn(i,j)*(zs(l)-zs(1)) ) / &
                             ( zs(num_soil_layers)-zs(1) )
               ENDDO
            ENDDO
         ENDDO
      ENDIF
      DO j=jts,jtf
         DO i=its,itf
           xland(i,j)  = 2
           ivgtyp(i,j) = 7
         ENDDO
      ENDDO

   END SUBROUTINE init_soil_1_ideal


   SUBROUTINE init_soil_2_real ( tsk , tmn , smois , tslb , &
                                 st_input , sm_input , sw_input , landmask , sst , &
                                 zs , dzs , &
                                 st_levels_input , sm_levels_input , sw_levels_input , &
                                 num_soil_layers , num_st_levels_input , num_sm_levels_input ,  num_sw_levels_input , &
                                 num_st_levels_alloc , num_sm_levels_alloc ,  num_sw_levels_alloc , &
                                 flag_sst , &
                                 ids , ide , jds , jde , kds , kde , &
                                 ims , ime , jms , jme , kms , kme , &
                                 its , ite , jts , jte , kts , kte )

      IMPLICIT NONE

      INTEGER , INTENT(IN) :: num_soil_layers , &
                              num_st_levels_input , num_sm_levels_input , num_sw_levels_input , &
                              num_st_levels_alloc , num_sm_levels_alloc , num_sw_levels_alloc , &
                              ids , ide , jds , jde , kds , kde , &
                              ims , ime , jms , jme , kms , kme , &
                              its , ite , jts , jte , kts , kte 

      INTEGER , INTENT(IN) :: flag_sst

      INTEGER , DIMENSION(1:num_st_levels_input) , INTENT(INOUT) :: st_levels_input
      INTEGER , DIMENSION(1:num_sm_levels_input) , INTENT(INOUT) :: sm_levels_input
      INTEGER , DIMENSION(1:num_sw_levels_input) , INTENT(INOUT) :: sw_levels_input

      REAL , DIMENSION(ims:ime,1:num_st_levels_alloc,jms:jme) , INTENT(INOUT) :: st_input
      REAL , DIMENSION(ims:ime,1:num_sm_levels_alloc,jms:jme) , INTENT(INOUT) :: sm_input
      REAL , DIMENSION(ims:ime,1:num_sw_levels_alloc,jms:jme) , INTENT(INOUT) :: sw_input
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN) :: landmask , sst

      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN) :: tmn
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(INOUT) :: tsk
      REAL , DIMENSION(num_soil_layers) :: zs , dzs

      REAL , DIMENSION(ims:ime,num_soil_layers,jms:jme) , INTENT(OUT) :: tslb , smois

      REAL , ALLOCATABLE , DIMENSION(:) :: zhave

      INTEGER :: i , j , l , lout , lin , lwant , lhave
      REAL :: temp

      !  Allocate the soil layer array used for interpolating.

	write(6,*) 'num_st_levels_input: ', num_st_levels_input
	write(6,*) 'num_sm_levels_input: ', num_sm_levels_input

      IF ( ( num_st_levels_input .LE. 0 ) .OR. & 
           ( num_sm_levels_input .LE. 0 ) ) THEN
         PRINT '(A)','No input soil level data (either temperature or moisture, or both are missing).  Required for LSM.'
         STOP 'no soil data'
      ELSE
         ALLOCATE ( zhave( MAX(num_st_levels_input,num_sm_levels_input,num_sw_levels_input) +2) )
      END IF

      !  Sort the levels for temperature.

      outert : DO lout = 1 , num_st_levels_input-1
         innert : DO lin = lout+1 , num_st_levels_input
            IF ( st_levels_input(lout) .GT. st_levels_input(lin) ) THEN
               temp = st_levels_input(lout) 
               st_levels_input(lout) = st_levels_input(lin)
               st_levels_input(lin) = NINT(temp)
               DO j = jts , MIN(jde-1,jte)
                  DO i = its , MIN(ide-1,ite)
                     temp = st_input(i,lout+1,j)
                     st_input(i,lout+1,j) = st_input(i,lin+1,j)
                     st_input(i,lin+1,j) = temp
                  END DO
               END DO
            END IF
         END DO innert
      END DO outert

      DO j = jts , MIN(jde-1,jte)
         DO i = its , MIN(ide-1,ite)
            st_input(i,1,j) = tsk(i,j)
            st_input(i,num_st_levels_input+2,j) = tmn(i,j)
         END DO
      END DO

      !  Sort the levels for moisture.

      outerm: DO lout = 1 , num_sm_levels_input-1
         innerm : DO lin = lout+1 , num_sm_levels_input
            IF ( sm_levels_input(lout) .GT. sm_levels_input(lin) ) THEN
               temp = sm_levels_input(lout) 
               sm_levels_input(lout) = sm_levels_input(lin)
               sm_levels_input(lin) = NINT(temp)
               DO j = jts , MIN(jde-1,jte)
                  DO i = its , MIN(ide-1,ite)
                     temp = sm_input(i,lout+1,j)
                     sm_input(i,lout+1,j) = sm_input(i,lin+1,j)
                     sm_input(i,lin+1,j) = temp
                  END DO
               END DO
            END IF
         END DO innerm
      END DO outerm
      DO j = jts , MIN(jde-1,jte)
         DO i = its , MIN(ide-1,ite)
            sm_input(i,1,j) = sm_input(i,2,j)
            sm_input(i,num_sm_levels_input+2,j) = sm_input(i,num_sm_levels_input+1,j)
         END DO
      END DO

      !  Sort the levels for liquid moisture.

      outerw: DO lout = 1 , num_sw_levels_input-1
         innerw : DO lin = lout+1 , num_sw_levels_input
            IF ( sw_levels_input(lout) .GT. sw_levels_input(lin) ) THEN
               temp = sw_levels_input(lout) 
               sw_levels_input(lout) = sw_levels_input(lin)
               sw_levels_input(lin) = NINT(temp)
               DO j = jts , MIN(jde-1,jte)
                  DO i = its , MIN(ide-1,ite)
                     temp = sw_input(i,lout+1,j)
                     sw_input(i,lout+1,j) = sw_input(i,lin+1,j)
                     sw_input(i,lin+1,j) = temp
                  END DO
               END DO
            END IF
         END DO innerw
      END DO outerw
      DO j = jts , MIN(jde-1,jte)
         DO i = its , MIN(ide-1,ite)
            sw_input(i,1,j) = sw_input(i,2,j)
            sw_input(i,num_sw_levels_input+2,j) = sw_input(i,num_sw_levels_input+1,j)
         END DO
      END DO

      !  Here are the levels that we have from the input for temperature.  The input levels plus
      !  two more: the skin temperature at 0 cm, and the annual mean temperature at 300 cm.

      zhave(1) = 0.
      DO l = 1 , num_st_levels_input
         zhave(l+1) = st_levels_input(l) / 100.
      END DO
      zhave(num_st_levels_input+2) = 300. / 100.

      !  Interpolate between the layers we have (zhave) and those that we want (zs).

      z_wantt : DO lwant = 1 , num_soil_layers
         z_havet : DO lhave = 1 , num_st_levels_input +2 -1
            IF ( ( zs(lwant) .GE. zhave(lhave  ) ) .AND. &
                 ( zs(lwant) .LE. zhave(lhave+1) ) ) THEN
               DO j = jts , MIN(jde-1,jte)
                  DO i = its , MIN(ide-1,ite)
                     tslb(i,lwant,j)= ( st_input(i,lhave  ,j) * ( zhave(lhave+1) - zs   (lwant) ) + &
                                        st_input(i,lhave+1,j) * ( zs   (lwant  ) - zhave(lhave) ) ) / &
                                                                ( zhave(lhave+1) - zhave(lhave) )

                  END DO
               END DO
               EXIT z_havet
            END IF
         END DO z_havet
      END DO z_wantt

      !  Here are the levels that we have from the input for moisture.  The input levels plus
      !  two more: a value at 0 cm and one at 300 cm.  The 0 cm value is taken to be identical
      !  to the most shallow layers value.  Similarly, the 300 cm value is taken to be the same
      !  as the most deep layers value.

      zhave(1) = 0.
      DO l = 1 , num_sm_levels_input
         zhave(l+1) = sm_levels_input(l) / 100.
      END DO
      zhave(num_sm_levels_input+2) = 300. / 100.

      !  Interpolate between the layers we have (zhave) and those that we want (zs).

      z_wantm : DO lwant = 1 , num_soil_layers
         z_havem : DO lhave = 1 , num_sm_levels_input +2 -1
            IF ( ( zs(lwant) .GE. zhave(lhave  ) ) .AND. &
                 ( zs(lwant) .LE. zhave(lhave+1) ) ) THEN
               DO j = jts , MIN(jde-1,jte)
                  DO i = its , MIN(ide-1,ite)
                     smois(i,lwant,j)= ( sm_input(i,lhave  ,j) * ( zhave(lhave+1) - zs   (lwant) ) + &
                                         sm_input(i,lhave+1,j) * ( zs   (lwant  ) - zhave(lhave) ) ) / &
                                                                 ( zhave(lhave+1) - zhave(lhave) )
                  END DO
               END DO
               EXIT z_havem
            END IF
         END DO z_havem
      END DO z_wantm

      !  Any liquid soil moisture to worry about?

      IF ( num_sw_levels_input .GT. 1 ) THEN

         zhave(1) = 0.
         DO l = 1 , num_sw_levels_input
            zhave(l+1) = sw_levels_input(l) / 100.
         END DO
         zhave(num_sw_levels_input+2) = 300. / 100.
   
         !  Interpolate between the layers we have (zhave) and those that we want (zs).
   
         z_wantw : DO lwant = 1 , num_soil_layers
            z_havew : DO lhave = 1 , num_sw_levels_input +2 -1
               IF ( ( zs(lwant) .GE. zhave(lhave  ) ) .AND. &
                    ( zs(lwant) .LE. zhave(lhave+1) ) ) THEN
                  DO j = jts , MIN(jde-1,jte)
                     DO i = its , MIN(ide-1,ite)
!hmm, what is this variable called
!                       sXXXX(i,lwant,j)= ( sw_input(i,lhave  ,j) * ( zhave(lhave+1) - zs   (lwant) ) + &
!                                           sw_input(i,lhave+1,j) * ( zs   (lwant  ) - zhave(lhave) ) ) / &
!                                                                   ( zhave(lhave+1) - zhave(lhave) )
                     END DO
                  END DO
                  EXIT z_havew
               END IF
            END DO z_havew
         END DO z_wantw

      END IF


      !  Over water, put in reasonable values for soil temperature and moisture.  These wont be
      !  used, but they will make a more continuous plot.

      IF ( flag_sst .EQ. 1 ) THEN
         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               IF ( landmask(i,j) .LT. 0.5 ) THEN
                  DO l = 1 , num_soil_layers
                     tslb(i,l,j)= sst(i,j)
                     smois(i,l,j)= 1.0
!                    sXXXX(i,l,j)= 1.0
                  END DO
               END IF
            END DO
         END DO
      ELSE
         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               IF ( landmask(i,j) .LT. 0.5 ) THEN
                  DO l = 1 , num_soil_layers
                     tslb(i,l,j)= tsk(i,j)
                     smois(i,l,j)= 1.0
!                    sXXXX(i,l,j)= 1.0
                  END DO
               END IF
            END DO
         END DO
      END IF

      DEALLOCATE (zhave)

   END SUBROUTINE init_soil_2_real

   SUBROUTINE init_soil_2_ideal ( xland,xice,vegfra,snow,canwat,     &
                     ivgtyp,isltyp,tslb,smois,tmn,                  &
                     num_soil_layers,                               &
                     ids,ide, jds,jde, kds,kde,                     &
                     ims,ime, jms,jme, kms,kme,                     &
                     its,ite, jts,jte, kts,kte                      )

      IMPLICIT NONE 
   
      INTEGER, INTENT(IN) ::ids,ide, jds,jde, kds,kde,  &
                            ims,ime, jms,jme, kms,kme,  &
                            its,ite, jts,jte, kts,kte
   
      INTEGER, INTENT(IN) ::num_soil_layers
   
      REAL, DIMENSION( ims:ime, num_soil_layers, jms:jme ) , INTENT(OUT) :: smois, tslb 
   
      REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(OUT)  :: xland, snow, canwat, xice, vegfra, tmn
   
      INTEGER, DIMENSION( ims:ime, jms:jme ) , INTENT(OUT) :: ivgtyp, isltyp
   
      INTEGER :: icm,jcm,itf,jtf
      INTEGER ::  i,j,l
   
      itf=min0(ite,ide-1)
      jtf=min0(jte,jde-1)
   
      icm = ide/2
      jcm = jde/2
   
      DO j=jts,jtf
         DO l=1,num_soil_layers
            DO i=its,itf
   
               smois(i,1,j)=0.10
               smois(i,2,j)=0.10
               smois(i,3,j)=0.10
               smois(i,4,j)=0.10
      
               tslb(i,1,j)=295. 	  
               tslb(i,2,j)=297. 	 
               tslb(i,3,j)=293. 	
               tslb(i,4,j)=293. 

            ENDDO
         ENDDO
      ENDDO                                 

      DO j=jts,jtf
         DO i=its,itf
            xland(i,j)  =   2
            tmn(i,j)    = 294. 
            xice(i,j)   =   0.
            vegfra(i,j) =   0. 
            snow(i,j)   =   0.
            canwat(i,j) =   0.
            ivgtyp(i,j) =   7
            isltyp(i,j) =   8
         ENDDO
      ENDDO

   END SUBROUTINE init_soil_2_ideal

   SUBROUTINE init_soil_3_real ( tsk , tmn , smois , tslb , &
                                 st_input , sm_input , landmask_input , sst_input , &
                                 zs , dzs , &
                                 st_levels_input , sm_levels_input , &
                                 num_soil_layers , num_st_levels_input , num_sm_levels_input ,  &
                                 flag_sst , &
                                 ids , ide , jds , jde , kds , kde , &
                                 ims , ime , jms , jme , kms , kme , &
                                 its , ite , jts , jte , kts , kte )

      IMPLICIT NONE

      INTEGER , INTENT(IN) :: num_soil_layers , num_st_levels_input , num_sm_levels_input , &
                              ids , ide , jds , jde , kds , kde , &
                              ims , ime , jms , jme , kms , kme , &
                              its , ite , jts , jte , kts , kte 

      INTEGER , INTENT(IN) :: flag_sst

      INTEGER , DIMENSION(1:num_st_levels_input) , INTENT(INOUT) :: st_levels_input
      INTEGER , DIMENSION(1:num_sm_levels_input) , INTENT(INOUT) :: sm_levels_input

      REAL , DIMENSION(its:ite,jts:jte,1:num_st_levels_input) , INTENT(INOUT) :: st_input
      REAL , DIMENSION(its:ite,jts:jte,1:num_sm_levels_input) , INTENT(INOUT) :: sm_input
      REAL , DIMENSION(its:ite,jts:jte) , INTENT(IN) :: landmask_input , sst_input 

      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN) :: tmn
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(INOUT) :: tsk
      REAL , DIMENSION(num_soil_layers) :: zs , dzs

      REAL , DIMENSION(ims:ime,num_soil_layers,jms:jme) , INTENT(OUT) :: tslb , smois

      REAL , ALLOCATABLE , DIMENSION(:) :: zhave

      INTEGER :: i , j , l , lout , lin , lwant , lhave
      REAL :: temp

      !  Allocate the soil layer array used for interpolating.

      IF ( ( num_st_levels_input .LE. 0 ) .OR. & 
           ( num_sm_levels_input .LE. 0 ) ) THEN
         PRINT '(A)','No input soil level data (either temperature or moisture, or both are missing).  Required for RUC LSM.'
         STOP 'no soil data'
      ELSE
         ALLOCATE ( zhave( MAX(num_st_levels_input,num_sm_levels_input) ) )
      END IF

      !  Sort the levels for temperature.

      outert : DO lout = 1 , num_st_levels_input-1
         innert : DO lin = lout+1 , num_st_levels_input
            IF ( st_levels_input(lout) .GT. st_levels_input(lin) ) THEN
               temp = st_levels_input(lout) 
               st_levels_input(lout) = st_levels_input(lin)
               st_levels_input(lin) = NINT(temp)
               DO j = jts , MIN(jde-1,jte)
                  DO i = its , MIN(ide-1,ite)
                     temp = st_input(i,j,lout)
                     st_input(i,j,lout) = st_input(i,j,lin)
                     st_input(i,j,lin) = temp
                  END DO
               END DO
            END IF
         END DO innert
      END DO outert

      !  Sort the levels for moisture.

      outerm: DO lout = 1 , num_sm_levels_input-1
         innerm : DO lin = lout+1 , num_sm_levels_input
            IF ( sm_levels_input(lout) .GT. sm_levels_input(lin) ) THEN
               temp = sm_levels_input(lout) 
               sm_levels_input(lout) = sm_levels_input(lin)
               sm_levels_input(lin) = NINT(temp)
               DO j = jts , MIN(jde-1,jte)
                  DO i = its , MIN(ide-1,ite)
                     temp = sm_input(i,j,lout)
                     sm_input(i,j,lout) = sm_input(i,j,lin)
                     sm_input(i,j,lin) = temp
                  END DO
               END DO
            END IF
         END DO innerm
      END DO outerm

      !  Here are the levels that we have from the input for temperature.

      DO l = 1 , num_st_levels_input
         zhave(l) = st_levels_input(l) / 100.
      END DO

      !  Interpolate between the layers we have (zhave) and those that we want (zs).

      z_wantt : DO lwant = 1 , num_soil_layers
         z_havet : DO lhave = 1 , num_st_levels_input -1
            IF ( ( zs(lwant) .GE. zhave(lhave  ) ) .AND. &
                 ( zs(lwant) .LE. zhave(lhave+1) ) ) THEN
               DO j = jts , MIN(jde-1,jte)
                  DO i = its , MIN(ide-1,ite)
                     tslb(i,lwant,j)= ( st_input(i,j,lhave  ) * ( zhave(lhave+1) - zs   (lwant) ) + &
                                        st_input(i,j,lhave+1) * ( zs   (lwant  ) - zhave(lhave) ) ) / &
                                                                ( zhave(lhave+1) - zhave(lhave) )
                  END DO
               END DO
               EXIT z_havet
            END IF
         END DO z_havet
      END DO z_wantt

      !  Here are the levels that we have from the input for moisture.

      DO l = 1 , num_sm_levels_input
         zhave(l) = sm_levels_input(l) / 100.
      END DO

      !  Interpolate between the layers we have (zhave) and those that we want (zs).

      z_wantm : DO lwant = 1 , num_soil_layers
         z_havem : DO lhave = 1 , num_sm_levels_input -1
            IF ( ( zs(lwant) .GE. zhave(lhave  ) ) .AND. &
                 ( zs(lwant) .LE. zhave(lhave+1) ) ) THEN
               DO j = jts , MIN(jde-1,jte)
                  DO i = its , MIN(ide-1,ite)
                     smois(i,lwant,j)= ( sm_input(i,j,lhave  ) * ( zhave(lhave+1) - zs   (lwant) ) + &
                                         sm_input(i,j,lhave+1) * ( zs   (lwant  ) - zhave(lhave) ) ) / &
                                                                 ( zhave(lhave+1) - zhave(lhave) )
                  END DO
               END DO
               EXIT z_havem
            END IF
         END DO z_havem
      END DO z_wantm

      !  Over water, put in reasonable values for soil temperature and moisture.  These wont be
      !  used, but they will make a more continuous plot.

      IF ( flag_sst .EQ. 1 ) THEN
         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               IF ( landmask_input(i,j) .LT. 0.5 ) THEN
                  DO l = 1 , num_soil_layers
                     tslb(i,l,j) = sst_input(i,j)
                     tsk(i,j)    = sst_input(i,j)
                     smois(i,l,j)= 1.0
                  END DO
               END IF
            END DO
         END DO
      ELSE
         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               IF ( landmask_input(i,j) .LT. 0.5 ) THEN
                  DO l = 1 , num_soil_layers
                     tslb(i,l,j)= tsk(i,j)
                     smois(i,l,j)= 1.0
                  END DO
               END IF
            END DO
         END DO
      END IF

      DEALLOCATE (zhave)

   END SUBROUTINE init_soil_3_real

   SUBROUTINE init_soil_old_real ( tsk , tmn , &
                                 smois , tslb , zs , dzs , num_soil_layers , &
                                 st000010_input , st010040_input , st040100_input , st100200_input , &
                                 st010200_input , &
                                 sm000010_input , sm010040_input , sm040100_input , sm100200_input , &
                                 sm010200_input , &
                                 landmask_input , sst_input , &
                                 ids , ide , jds , jde , kds , kde , &
                                 ims , ime , jms , jme , kms , kme , &
                                 its , ite , jts , jte , kts , kte )

      !  This is the old version of init_soil_temp_2.  Here we directly assign the
      !  soil t and moisture levels to WRF levels - no interpolation.

      IMPLICIT NONE

      INTEGER , INTENT(IN) :: num_soil_layers , &
                              ids , ide , jds , jde , kds , kde , &
                              ims , ime , jms , jme , kms , kme , &
                              its , ite , jts , jte , kts , kte 

      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN) :: tsk , tmn
      REAL , DIMENSION(num_soil_layers) :: zs , dzs

      REAL , DIMENSION(:,:) , INTENT(IN) :: st000010_input , st010040_input , st040100_input , st100200_input , &
                                            st010200_input , &
                                            sm000010_input , sm010040_input , sm040100_input , sm100200_input , &
                                            sm010200_input , &
                                            landmask_input , sst_input

      REAL , DIMENSION(ims:ime,num_soil_layers,jms:jme) , INTENT(OUT) :: tslb , smois

      INTEGER :: i , j , l

      !  Soil temperature is linearly interpolated between the skin temperature (taken to be at a
      !  depth of 0 cm) and the various input temperature levels.

      DO j = jts , MIN(jde-1,jte)
         DO i = its , MIN(ide-1,ite)
            IF ( landmask_input(i,j) .EQ. 1 ) THEN
               tslb(i,1,j)= st000010_input(i,j)
               tslb(i,2,j)= st010040_input(i,j)
               tslb(i,3,j)= st040100_input(i,j)
               tslb(i,4,j)= st100200_input(i,j)
!tslb(i,4,j)= st010200_input(i,j)
               smois(i,1,j)= sm000010_input(i,j)
               smois(i,2,j)= sm010040_input(i,j)
               smois(i,3,j)= sm040100_input(i,j)
               smois(i,4,j)= sm100200_input(i,j)
!smois(i,4,j)= sm010200_input(i,j)
            ELSE
               DO l = 1 , num_soil_layers
                  tslb(i,l,j)= sst_input(i,j)
                  smois(i,l,j)= 1.0
               END DO
            END IF
         END DO
      END DO

   END SUBROUTINE init_soil_old_real

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!  all of this following "_old" stuff is to allow eh cores to use the
!!!!  SI binary input, which has input data still as domain sized,
!!!!  not memory sized, note that FLAG_SST is a logical in the following
!!!!  routines

   SUBROUTINE process_soil_real_old ( tsk , tmn , xland , &
                                landmask_input , sst_input , &
                                st_input , sm_input , st_levels_input , sm_levels_input , &
                                zs , dzs , tslb , smois , &
                                FLAG_SST , &
                                st000010_input , st010040_input , st040100_input , st100200_input , &
                                st010200_input , &
                                sm000010_input , sm010040_input , sm040100_input , sm100200_input , &
                                sm010200_input , &
                                ids , ide , jds , jde , kds , kde , &
                                ims , ime , jms , jme , kms , kme , &
                                its , ite , jts , jte , kts , kte , &
                                bl_surface_physics , num_soil_layers , real_data_init_type , &
                                num_st_levels_input , num_sm_levels_input )

      IMPLICIT NONE

      INTEGER , INTENT(IN) :: ids , ide , jds , jde , kds , kde , &
                              ims , ime , jms , jme , kms , kme , &
                              its , ite , jts , jte , kts , kte , &
                              bl_surface_physics , num_soil_layers , real_data_init_type , &
                              num_st_levels_input , num_sm_levels_input

      LOGICAL , INTENT(IN) :: FLAG_SST

      REAL , DIMENSION(its:ite,jts:jte) , INTENT(IN) :: landmask_input , sst_input

      INTEGER , DIMENSION(1:num_st_levels_input) , INTENT(INOUT) :: st_levels_input
      INTEGER , DIMENSION(1:num_sm_levels_input) , INTENT(INOUT) :: sm_levels_input
      REAL , DIMENSION(its:ite,jts:jte,1:num_st_levels_input) , INTENT(INOUT) :: st_input
      REAL , DIMENSION(its:ite,jts:jte,1:num_sm_levels_input) , INTENT(INOUT) :: sm_input

      REAL, DIMENSION(1:num_soil_layers), INTENT(OUT)  ::  zs,dzs
      REAL , DIMENSION(ims:ime,num_soil_layers,jms:jme) , INTENT(OUT) :: tslb , smois

      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN) :: tmn , xland
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(INOUT) :: tsk

      REAL , DIMENSION(its:ite,jts:jte) , INTENT(IN) :: st000010_input , st010040_input , st040100_input , st100200_input , &
                                            st010200_input , &
                                            sm000010_input , sm010040_input , sm040100_input , sm100200_input , &
                                            sm010200_input 

      INTEGER :: i , j , l , dominant_index , num_soil_cat , num_veg_cat
      REAL :: dominant_value

      !  Initialize the soil depth, and the soil temperature and moisture.
   
      IF      ( ( bl_surface_physics .EQ. 1 ) .AND. ( num_soil_layers .GT. 1 ) ) THEN
         CALL init_soil_depth_1 ( zs , dzs , num_soil_layers )
         CALL init_soil_1_real_old ( tsk , tmn , tslb , zs , dzs , num_soil_layers , real_data_init_type , &
                                 landmask_input , sst_input , FLAG_SST , &
                                 ids , ide , jds , jde , kds , kde , &
                                 ims , ime , jms , jme , kms , kme , &
                                 its , ite , jts , jte , kts , kte )

      ELSE IF ( ( bl_surface_physics .EQ. 2 .or. bl_surface_physics .EQ. 99 ) &
                            .AND. ( num_soil_layers .GT. 1 ) ) THEN
         CALL init_soil_depth_2 ( zs , dzs , num_soil_layers )
         CALL init_soil_2_real_old ( tsk , tmn , smois , tslb , &
                                 st_input , sm_input , landmask_input , sst_input , &
                                 zs , dzs , &
                                 st_levels_input , sm_levels_input , &
                                 num_soil_layers , num_st_levels_input , num_sm_levels_input , &
                                 FLAG_SST , &
                                 ids , ide , jds , jde , kds , kde , &
                                 ims , ime , jms , jme , kms , kme , &
                                 its , ite , jts , jte , kts , kte )
      ELSE IF ( ( bl_surface_physics .EQ. 3 ) .AND. ( num_soil_layers .GT. 1 ) ) THEN
         CALL init_soil_depth_3 ( zs , dzs , num_soil_layers )
         CALL init_soil_3_real_old ( tsk , tmn , smois , tslb , &
                                 st_input , sm_input , landmask_input , sst_input , &
                                 zs , dzs , &
                                 st_levels_input , sm_levels_input , &
                                 num_soil_layers , num_st_levels_input , num_sm_levels_input , &
                                 FLAG_SST , &
                                 ids , ide , jds , jde , kds , kde , &
                                 ims , ime , jms , jme , kms , kme , &
                                 its , ite , jts , jte , kts , kte )
      END IF

   END SUBROUTINE process_soil_real_old

   SUBROUTINE init_soil_1_real_old ( tsk , tmn , tslb , zs , dzs , &
                                 num_soil_layers , real_data_init_type , &
                                 landmask_input , sst_input , FLAG_SST , &
                                 ids , ide , jds , jde , kds , kde , &
                                 ims , ime , jms , jme , kms , kme , &
                                 its , ite , jts , jte , kts , kte )

      IMPLICIT NONE

      INTEGER , INTENT(IN) :: num_soil_layers , real_data_init_type , &
                              ids , ide , jds , jde , kds , kde , &
                              ims , ime , jms , jme , kms , kme , &
                              its , ite , jts , jte , kts , kte 

      LOGICAL , INTENT(IN) :: FLAG_SST

      REAL , DIMENSION(its:ite,jts:jte) , INTENT(IN) :: landmask_input , sst_input
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN) :: tmn
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(INOUT) :: tsk
      REAL , DIMENSION(num_soil_layers) :: zs , dzs

      REAL , DIMENSION(ims:ime,num_soil_layers,jms:jme) , INTENT(OUT) :: tslb

      INTEGER :: i , j , l

      !  Soil temperature is linearly interpolated between the skin temperature (taken to be at a
      !  depth of 0.5 cm) and the deep soil, annual temperature (taken to be at a depth of 23 cm).
      !  The tslb(i,1,j) is the skin temperature, and the tslb(i,num_soil_layers,j) level is the 
      !  annual mean temperature.

      DO j = jts , MIN(jde-1,jte)
         DO i = its , MIN(ide-1,ite)
            IF ( landmask_input(i,j) .GT. 0.5 ) THEN
               DO l = 1 , num_soil_layers
                  tslb(i,l,j)= ( tsk(i,j) * ( zs(num_soil_layers) - zs(l) )   + &
                                 tmn(i,j) * ( zs(              l) - zs(1) ) ) / &
                                            ( zs(num_soil_layers) - zs(1) )
               END DO
            ELSE
               IF ( ( real_data_init_type .EQ. 1 ) .AND. ( FLAG_SST ) ) THEN
                  DO l = 1 , num_soil_layers
                     tslb(i,l,j) = sst_input(i,j)
                     tsk (i,j)   = sst_input(i,j)
                  END DO
               ELSE
                  DO l = 1 , num_soil_layers
                     tslb(i,l,j)= tsk(i,j)
                  END DO
               END IF
            END IF
         END DO
      END DO

   END SUBROUTINE init_soil_1_real_old

   SUBROUTINE init_soil_2_real_old ( tsk , tmn , smois , tslb , &
                                 st_input , sm_input , landmask_input , sst_input , &
                                 zs , dzs , &
                                 st_levels_input , sm_levels_input , &
                                 num_soil_layers , num_st_levels_input , num_sm_levels_input ,  &
                                 FLAG_SST , &
                                 ids , ide , jds , jde , kds , kde , &
                                 ims , ime , jms , jme , kms , kme , &
                                 its , ite , jts , jte , kts , kte )

      IMPLICIT NONE

      INTEGER , INTENT(IN) :: num_soil_layers , num_st_levels_input , num_sm_levels_input , &
                              ids , ide , jds , jde , kds , kde , &
                              ims , ime , jms , jme , kms , kme , &
                              its , ite , jts , jte , kts , kte 

      LOGICAL , INTENT(IN) :: FLAG_SST

      INTEGER , DIMENSION(1:num_st_levels_input) , INTENT(INOUT) :: st_levels_input
      INTEGER , DIMENSION(1:num_sm_levels_input) , INTENT(INOUT) :: sm_levels_input

      REAL , DIMENSION(its:ite,jts:jte,1:num_st_levels_input) , INTENT(INOUT) :: st_input
      REAL , DIMENSION(its:ite,jts:jte,1:num_sm_levels_input) , INTENT(INOUT) :: sm_input
      REAL , DIMENSION(its:ite,jts:jte) , INTENT(IN) :: landmask_input , sst_input 

      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN) :: tmn
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(INOUT) :: tsk
      REAL , DIMENSION(num_soil_layers) :: zs , dzs

      REAL , DIMENSION(ims:ime,num_soil_layers,jms:jme) , INTENT(OUT) :: tslb , smois

      REAL , ALLOCATABLE , DIMENSION(:) :: zhave

      INTEGER :: i , j , l , lout , lin , lwant , lhave
      REAL :: temp

      !  Allocate the soil layer array used for interpolating.

      IF ( ( num_st_levels_input .LE. 0 ) .OR. & 
           ( num_sm_levels_input .LE. 0 ) ) THEN
         PRINT '(A)','No input soil level data (either temperature or moisture, or both are missing).  Required for LSM.'
         STOP 'no soil data'
      ELSE
         ALLOCATE ( zhave( MAX(num_st_levels_input,num_sm_levels_input) +2) )
      END IF

      !  Sort the levels for temperature.

      outert : DO lout = 1 , num_st_levels_input-1
         innert : DO lin = lout+1 , num_st_levels_input
            IF ( st_levels_input(lout) .GT. st_levels_input(lin) ) THEN
               temp = st_levels_input(lout) 
               st_levels_input(lout) = st_levels_input(lin)
               st_levels_input(lin) = NINT(temp)
               DO j = jts , MIN(jde-1,jte)
                  DO i = its , MIN(ide-1,ite)
                     temp = st_input(i,j,lout+1)
                     st_input(i,j,lout+1) = st_input(i,j,lin+1)
                     st_input(i,j,lin+1) = temp
                  END DO
               END DO
            END IF
         END DO innert
      END DO outert
      DO j = jts , MIN(jde-1,jte)
         DO i = its , MIN(ide-1,ite)
            st_input(i,j,1) = tsk(i,j)
            st_input(i,j,num_st_levels_input+2) = tmn(i,j)
         END DO
      END DO

      !  Sort the levels for moisture.

      outerm: DO lout = 1 , num_sm_levels_input-1
         innerm : DO lin = lout+1 , num_sm_levels_input
            IF ( sm_levels_input(lout) .GT. sm_levels_input(lin) ) THEN
               temp = sm_levels_input(lout) 
               sm_levels_input(lout) = sm_levels_input(lin)
               sm_levels_input(lin) = NINT(temp)
               DO j = jts , MIN(jde-1,jte)
                  DO i = its , MIN(ide-1,ite)
                     temp = sm_input(i,j,lout+1)
                     sm_input(i,j,lout+1) = sm_input(i,j,lin+1)
                     sm_input(i,j,lin+1) = temp
                  END DO
               END DO
            END IF
         END DO innerm
      END DO outerm
      DO j = jts , MIN(jde-1,jte)
         DO i = its , MIN(ide-1,ite)
            sm_input(i,j,1) = sm_input(i,j,2)
            sm_input(i,j,num_sm_levels_input+2) = sm_input(i,j,num_sm_levels_input+1)
         END DO
      END DO

      !  Here are the levels that we have from the input for temperature.  The input levels plus
      !  two more: the skin temperature at 0 cm, and the annual mean temperature at 300 cm.

      zhave(1) = 0.
      DO l = 1 , num_st_levels_input
         zhave(l+1) = st_levels_input(l) / 100.
      END DO
      zhave(num_st_levels_input+2) = 300. / 100.

      !  Interpolate between the layers we have (zhave) and those that we want (zs).

      z_wantt : DO lwant = 1 , num_soil_layers
         z_havet : DO lhave = 1 , num_st_levels_input +2 -1
            IF ( ( zs(lwant) .GE. zhave(lhave  ) ) .AND. &
                 ( zs(lwant) .LE. zhave(lhave+1) ) ) THEN
               DO j = jts , MIN(jde-1,jte)
                  DO i = its , MIN(ide-1,ite)
                     tslb(i,lwant,j)= ( st_input(i,j,lhave  ) * ( zhave(lhave+1) - zs   (lwant) ) + &
                                        st_input(i,j,lhave+1) * ( zs   (lwant  ) - zhave(lhave) ) ) / &
                                                                ( zhave(lhave+1) - zhave(lhave) )
                  END DO
               END DO
               EXIT z_havet
            END IF
         END DO z_havet
      END DO z_wantt

      !  Here are the levels that we have from the input for moisture.  The input levels plus
      !  two more: a value at 0 cm and one at 300 cm.  The 0 cm value is taken to be identical
      !  to the most shallow layers value.  Similarly, the 300 cm value is taken to be the same
      !  as the most deep layers value.

      zhave(1) = 0.
      DO l = 1 , num_sm_levels_input
         zhave(l+1) = sm_levels_input(l) / 100.
      END DO
      zhave(num_sm_levels_input+2) = 300. / 100.

      !  Interpolate between the layers we have (zhave) and those that we want (zs).

      z_wantm : DO lwant = 1 , num_soil_layers
         z_havem : DO lhave = 1 , num_sm_levels_input +2 -1
            IF ( ( zs(lwant) .GE. zhave(lhave  ) ) .AND. &
                 ( zs(lwant) .LE. zhave(lhave+1) ) ) THEN
               DO j = jts , MIN(jde-1,jte)
                  DO i = its , MIN(ide-1,ite)
                     smois(i,lwant,j)= ( sm_input(i,j,lhave  ) * ( zhave(lhave+1) - zs   (lwant) ) + &
                                         sm_input(i,j,lhave+1) * ( zs   (lwant  ) - zhave(lhave) ) ) / &
                                                                 ( zhave(lhave+1) - zhave(lhave) )
                  END DO
               END DO
               EXIT z_havem
            END IF
         END DO z_havem
      END DO z_wantm

      !  Over water, put in reasonable values for soil temperature and moisture.  These wont be
      !  used, but they will make a more continuous plot.

      IF ( FLAG_SST ) THEN
         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               IF ( landmask_input(i,j) .LT. 0.5 ) THEN
                  DO l = 1 , num_soil_layers
                     tslb(i,l,j) = sst_input(i,j)
                     tsk(i,j)    = sst_input(i,j)
                     smois(i,l,j)= 1.0
                  END DO
               END IF
            END DO
         END DO
      ELSE
         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               IF ( landmask_input(i,j) .LT. 0.5 ) THEN
                  DO l = 1 , num_soil_layers
                     tslb(i,l,j)= tsk(i,j)
                     smois(i,l,j)= 1.0
                  END DO
               END IF
            END DO
         END DO
      END IF

      DEALLOCATE (zhave)

   END SUBROUTINE init_soil_2_real_old

   SUBROUTINE init_soil_3_real_old ( tsk , tmn , smois , tslb , &
                                 st_input , sm_input , landmask_input , sst_input , &
                                 zs , dzs , &
                                 st_levels_input , sm_levels_input , &
                                 num_soil_layers , num_st_levels_input , num_sm_levels_input ,  &
                                 FLAG_SST , &
                                 ids , ide , jds , jde , kds , kde , &
                                 ims , ime , jms , jme , kms , kme , &
                                 its , ite , jts , jte , kts , kte )

      IMPLICIT NONE

      INTEGER , INTENT(IN) :: num_soil_layers , num_st_levels_input , num_sm_levels_input , &
                              ids , ide , jds , jde , kds , kde , &
                              ims , ime , jms , jme , kms , kme , &
                              its , ite , jts , jte , kts , kte 

      LOGICAL , INTENT(IN) :: FLAG_SST

      INTEGER , DIMENSION(1:num_st_levels_input) , INTENT(INOUT) :: st_levels_input
      INTEGER , DIMENSION(1:num_sm_levels_input) , INTENT(INOUT) :: sm_levels_input

      REAL , DIMENSION(its:ite,jts:jte,1:num_st_levels_input) , INTENT(INOUT) :: st_input
      REAL , DIMENSION(its:ite,jts:jte,1:num_sm_levels_input) , INTENT(INOUT) :: sm_input
      REAL , DIMENSION(its:ite,jts:jte) , INTENT(IN) :: landmask_input , sst_input 

      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN) :: tmn
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(INOUT) :: tsk
      REAL , DIMENSION(num_soil_layers) :: zs , dzs

      REAL , DIMENSION(ims:ime,num_soil_layers,jms:jme) , INTENT(OUT) :: tslb , smois

      REAL , ALLOCATABLE , DIMENSION(:) :: zhave

      INTEGER :: i , j , l , lout , lin , lwant , lhave
      REAL :: temp

      !  Allocate the soil layer array used for interpolating.

      IF ( ( num_st_levels_input .LE. 0 ) .OR. & 
           ( num_sm_levels_input .LE. 0 ) ) THEN
         PRINT '(A)','No input soil level data (either temperature or moisture, or both are missing).  Required for RUC LSM.'
         STOP 'no soil data'
      ELSE
         ALLOCATE ( zhave( MAX(num_st_levels_input,num_sm_levels_input) ) )
      END IF

      !  Sort the levels for temperature.

      outert : DO lout = 1 , num_st_levels_input-1
         innert : DO lin = lout+1 , num_st_levels_input
            IF ( st_levels_input(lout) .GT. st_levels_input(lin) ) THEN
               temp = st_levels_input(lout) 
               st_levels_input(lout) = st_levels_input(lin)
               st_levels_input(lin) = NINT(temp)
               DO j = jts , MIN(jde-1,jte)
                  DO i = its , MIN(ide-1,ite)
                     temp = st_input(i,j,lout)
                     st_input(i,j,lout) = st_input(i,j,lin)
                     st_input(i,j,lin) = temp
                  END DO
               END DO
            END IF
         END DO innert
      END DO outert

      !  Sort the levels for moisture.

      outerm: DO lout = 1 , num_sm_levels_input-1
         innerm : DO lin = lout+1 , num_sm_levels_input
            IF ( sm_levels_input(lout) .GT. sm_levels_input(lin) ) THEN
               temp = sm_levels_input(lout) 
               sm_levels_input(lout) = sm_levels_input(lin)
               sm_levels_input(lin) = NINT(temp)
               DO j = jts , MIN(jde-1,jte)
                  DO i = its , MIN(ide-1,ite)
                     temp = sm_input(i,j,lout)
                     sm_input(i,j,lout) = sm_input(i,j,lin)
                     sm_input(i,j,lin) = temp
                  END DO
               END DO
            END IF
         END DO innerm
      END DO outerm

      !  Here are the levels that we have from the input for temperature.

      DO l = 1 , num_st_levels_input
         zhave(l) = st_levels_input(l) / 100.
      END DO

      !  Interpolate between the layers we have (zhave) and those that we want (zs).

      z_wantt : DO lwant = 1 , num_soil_layers
         z_havet : DO lhave = 1 , num_st_levels_input -1
            IF ( ( zs(lwant) .GE. zhave(lhave  ) ) .AND. &
                 ( zs(lwant) .LE. zhave(lhave+1) ) ) THEN
               DO j = jts , MIN(jde-1,jte)
                  DO i = its , MIN(ide-1,ite)
                     tslb(i,lwant,j)= ( st_input(i,j,lhave  ) * ( zhave(lhave+1) - zs   (lwant) ) + &
                                        st_input(i,j,lhave+1) * ( zs   (lwant  ) - zhave(lhave) ) ) / &
                                                                ( zhave(lhave+1) - zhave(lhave) )
                  END DO
               END DO
               EXIT z_havet
            END IF
         END DO z_havet
      END DO z_wantt

      !  Here are the levels that we have from the input for moisture.

      DO l = 1 , num_sm_levels_input
         zhave(l) = sm_levels_input(l) / 100.
      END DO

      !  Interpolate between the layers we have (zhave) and those that we want (zs).

      z_wantm : DO lwant = 1 , num_soil_layers
         z_havem : DO lhave = 1 , num_sm_levels_input -1
            IF ( ( zs(lwant) .GE. zhave(lhave  ) ) .AND. &
                 ( zs(lwant) .LE. zhave(lhave+1) ) ) THEN
               DO j = jts , MIN(jde-1,jte)
                  DO i = its , MIN(ide-1,ite)
                     smois(i,lwant,j)= ( sm_input(i,j,lhave  ) * ( zhave(lhave+1) - zs   (lwant) ) + &
                                         sm_input(i,j,lhave+1) * ( zs   (lwant  ) - zhave(lhave) ) ) / &
                                                                 ( zhave(lhave+1) - zhave(lhave) )
                  END DO
               END DO
               EXIT z_havem
            END IF
         END DO z_havem
      END DO z_wantm

      !  Over water, put in reasonable values for soil temperature and moisture.  These wont be
      !  used, but they will make a more continuous plot.

      IF ( FLAG_SST ) THEN
         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               IF ( landmask_input(i,j) .LT. 0.5 ) THEN
                  DO l = 1 , num_soil_layers
                     tslb(i,l,j) = sst_input(i,j)
                     tsk(i,j)    = sst_input(i,j)
                     smois(i,l,j)= 1.0
                  END DO
               END IF
            END DO
         END DO
      ELSE
         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               IF ( landmask_input(i,j) .LT. 0.5 ) THEN
                  DO l = 1 , num_soil_layers
                     tslb(i,l,j)= tsk(i,j)
                     smois(i,l,j)= 1.0
                  END DO
               END IF
            END DO
         END DO
      END IF

      DEALLOCATE (zhave)

   END SUBROUTINE init_soil_3_real_old

END MODULE module_soil_pre
