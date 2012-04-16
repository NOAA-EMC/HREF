
MODULE module_soil_pre

   USE module_date_time
   USE module_state_description

CONTAINS

   SUBROUTINE adjust_for_seaice_pre ( xice , landmask , tsk , ivgtyp , vegcat , lu_index , &
                                      xland , landusef , isltyp , soilcat , soilctop , &
                                      soilcbot , tmn , &
                                      seaice_threshold , &
                                      fractional_seaice, &
                                      num_veg_cat , num_soil_top_cat , num_soil_bot_cat , &
                                      iswater , isice , &
                                      scheme , &
                                      ids , ide , jds , jde , kds , kde , &
                                      ims , ime , jms , jme , kms , kme , &
                                      its , ite , jts , jte , kts , kte )

      IMPLICIT NONE

      INTEGER , INTENT(IN) :: ids , ide , jds , jde , kds , kde , &
                              ims , ime , jms , jme , kms , kme , &
                              its , ite , jts , jte , kts , kte , &
                              iswater , isice
      INTEGER , INTENT(IN) :: num_veg_cat , num_soil_top_cat , num_soil_bot_cat , scheme

      REAL , DIMENSION(ims:ime,1:num_veg_cat,jms:jme) , INTENT(INOUT):: landusef
      REAL , DIMENSION(ims:ime,1:num_soil_top_cat,jms:jme) , INTENT(INOUT):: soilctop
      REAL , DIMENSION(ims:ime,1:num_soil_bot_cat,jms:jme) , INTENT(INOUT):: soilcbot
      INTEGER , DIMENSION(ims:ime,jms:jme), INTENT(OUT) :: isltyp , ivgtyp
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(INOUT) :: landmask , xice , tsk , lu_index , &
                                                           vegcat, xland , soilcat , tmn
      REAL , INTENT(IN) :: seaice_threshold

      INTEGER :: i , j , num_seaice_changes , loop
      CHARACTER (LEN=132) :: message


      integer, intent(in) :: fractional_seaice
      real                :: xice_threshold

      IF ( FRACTIONAL_SEAICE == 0 ) THEN
         xice_threshold = 0.5
      ELSEIF ( FRACTIONAL_SEAICE == 1 ) THEN
         CALL wrf_error_fatal3("",47,&
"NMM cannot use FRACTIONAL_SEAICE = 1")
         xice_threshold = 0.02
      ENDIF

      fix_seaice : SELECT CASE ( scheme )

         CASE ( SLABSCHEME )
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  IF ( xice(i,j) .GT. 200.0 ) THEN
                     xice(i,j) = 0.
                     num_seaice_changes = num_seaice_changes + 1
                  END IF
               END DO
            END DO
            IF ( num_seaice_changes .GT. 0 ) THEN
               WRITE ( message , FMT='(A,I6)' ) &
               'Total pre number of sea ice locations removed (due to FLAG values) = ', &
               num_seaice_changes
               CALL wrf_debug ( 0 , message )
            END IF
            num_seaice_changes = 0
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  IF ( ( xice(i,j) .GE. xice_threshold ) .OR. &
                       ( ( landmask(i,j) .LT. 0.5 ) .AND. ( tsk(i,j) .LT. seaice_threshold ) ) ) THEN
                     IF ( FRACTIONAL_SEAICE == 0 ) THEN
                        xice(i,j) = 1.0
                     ENDIF
                     num_seaice_changes = num_seaice_changes + 1
                     tmn(i,j) = 271.4
                     vegcat(i,j)=isice
                     lu_index(i,j)=ivgtyp(i,j)
                     landmask(i,j)=1.
                     xland(i,j)=1.
                     DO loop=1,num_veg_cat
                        landusef(i,loop,j)=0.
                     END DO
                     landusef(i,ivgtyp(i,j),j)=1.

                     isltyp(i,j) = 16
                     soilcat(i,j)=isltyp(i,j)
                     DO loop=1,num_soil_top_cat
                        soilctop(i,loop,j)=0
                     END DO
                     DO loop=1,num_soil_bot_cat
                        soilcbot(i,loop,j)=0
                     END DO
                     soilctop(i,isltyp(i,j),j)=1.
                     soilcbot(i,isltyp(i,j),j)=1.
                  ELSE
                     xice(i,j) = 0.0
                  END IF
               END DO
            END DO
            IF ( num_seaice_changes .GT. 0 ) THEN
               WRITE ( message , FMT='(A,I6)' ) &
               'Total pre number of sea ice location changes (water to land) = ', num_seaice_changes
               CALL wrf_debug ( 0 , message )
            END IF

         CASE ( LSMSCHEME )
         CASE ( RUCLSMSCHEME )

      END SELECT fix_seaice

   END SUBROUTINE adjust_for_seaice_pre

   SUBROUTINE adjust_for_seaice_post ( xice , landmask , tsk , ivgtyp , vegcat , lu_index , &
                                      xland , landusef , isltyp , soilcat , soilctop , &
                                      soilcbot , tmn , &
                                      tslb , smois , sh2o , &
                                      seaice_threshold , &
                                      fractional_seaice, &
                                      num_veg_cat , num_soil_top_cat , num_soil_bot_cat , &
                                      num_soil_layers , &
                                      iswater , isice , &
                                      scheme , &
                                      ids , ide , jds , jde , kds , kde , &
                                      ims , ime , jms , jme , kms , kme , &
                                      its , ite , jts , jte , kts , kte )

      IMPLICIT NONE

      INTEGER , INTENT(IN) :: ids , ide , jds , jde , kds , kde , &
                              ims , ime , jms , jme , kms , kme , &
                              its , ite , jts , jte , kts , kte , &
                              iswater , isice
      INTEGER , INTENT(IN) :: num_veg_cat , num_soil_top_cat , num_soil_bot_cat , scheme
      INTEGER , INTENT(IN) :: num_soil_layers

      REAL , DIMENSION(ims:ime,1:num_veg_cat,jms:jme) , INTENT(INOUT):: landusef
      REAL , DIMENSION(ims:ime,1:num_soil_top_cat,jms:jme) , INTENT(INOUT):: soilctop
      REAL , DIMENSION(ims:ime,1:num_soil_bot_cat,jms:jme) , INTENT(INOUT):: soilcbot
      REAL , DIMENSION(ims:ime,1:num_soil_layers,jms:jme) , INTENT(INOUT):: tslb , smois , sh2o
      INTEGER , DIMENSION(ims:ime,jms:jme), INTENT(OUT) :: isltyp , ivgtyp
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(INOUT) :: landmask , xice , tsk , lu_index , &
                                                           vegcat, xland , soilcat , tmn
      REAL , INTENT(IN) :: seaice_threshold
      REAL :: total_depth , mid_point_depth

      INTEGER :: i , j , num_seaice_changes , loop
      CHARACTER (LEN=132) :: message

      integer, intent(in) :: fractional_seaice
      real                :: xice_threshold
      IF ( FRACTIONAL_SEAICE == 0 ) THEN
         xice_threshold = 0.5
      ELSEIF ( FRACTIONAL_SEAICE == 1 ) THEN
         CALL wrf_error_fatal3("",157,&
"NMM cannot use FRACTIONAL_SEAICE = 1")
         xice_threshold = 0.02
      ENDIF
      fix_seaice : SELECT CASE ( scheme )

         CASE ( SLABSCHEME )


         CASE ( LSMSCHEME, GFDLSLAB )
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  IF ( xice(i,j) .GT. 200.0 ) THEN
                     xice(i,j) = 0.
                     num_seaice_changes = num_seaice_changes + 1
                  END IF
               END DO
            END DO
            IF ( num_seaice_changes .GT. 0 ) THEN
               WRITE ( message , FMT='(A,I6)' ) &
               'Total post number of sea ice locations removed (due to FLAG values) = ', &
               num_seaice_changes
               CALL wrf_debug ( 0 , message )
            END IF
            num_seaice_changes = 0
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  IF ( ( xice(i,j) .GE. xice_threshold ) .OR. &
                       ( ( landmask(i,j) .LT. 0.5 ) .AND. ( tsk(i,j) .LT. seaice_threshold ) ) ) THEN
                     IF ( FRACTIONAL_SEAICE == 0 ) THEN
                        xice(i,j) = 1.0
                     ENDIF
                     num_seaice_changes = num_seaice_changes + 1
                     tmn(i,j) = 271.16
                     vegcat(i,j)=isice
                     lu_index(i,j)=ivgtyp(i,j)
                     landmask(i,j)=1.
                     xland(i,j)=1.
                     DO loop=1,num_veg_cat
                        landusef(i,loop,j)=0.
                     END DO
                     landusef(i,ivgtyp(i,j),j)=1.

                     isltyp(i,j) = 16
                     soilcat(i,j)=isltyp(i,j)
                     DO loop=1,num_soil_top_cat
                        soilctop(i,loop,j)=0
                     END DO
                     DO loop=1,num_soil_bot_cat
                        soilcbot(i,loop,j)=0
                     END DO
                     soilctop(i,isltyp(i,j),j)=1.
                     soilcbot(i,isltyp(i,j),j)=1.

                     total_depth = 3. 
                     DO loop = 1,num_soil_layers
                        mid_point_depth=(total_depth/num_soil_layers)/2. + &
                                        (loop-1)*(total_depth/num_soil_layers)
                        tslb(i,loop,j) = ( (total_depth-mid_point_depth)*tsk(i,j) + &
                                            mid_point_depth*tmn(i,j) ) / total_depth
                     END DO

                     DO loop=1,num_soil_layers
                        smois(i,loop,j) = 1.0
                        sh2o(i,loop,j)  = 0.0
                     END DO
                  ELSE
                     xice(i,j) = 0.0
                  END IF
               END DO
            END DO
            IF ( num_seaice_changes .GT. 0 ) THEN
               WRITE ( message , FMT='(A,I6)' ) &
               'Total post number of sea ice location changes (water to land) = ', num_seaice_changes
               CALL wrf_debug ( 0 , message )
            END IF

         CASE ( RUCLSMSCHEME )

      END SELECT fix_seaice

   END SUBROUTINE adjust_for_seaice_post

   SUBROUTINE process_percent_cat_new ( landmask ,  &
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
      REAL , DIMENSION(ims:ime,1:num_veg_cat,jms:jme) , INTENT(INOUT):: landuse_frac
      REAL , DIMENSION(ims:ime,1:num_soil_top_cat,jms:jme) , INTENT(IN):: soil_top_cat
      REAL , DIMENSION(ims:ime,1:num_soil_bot_cat,jms:jme) , INTENT(IN):: soil_bot_cat
      INTEGER , DIMENSION(ims:ime,jms:jme), INTENT(OUT) :: isltyp , ivgtyp
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(INOUT) :: landmask

      INTEGER :: i , j , l , ll, dominant_index
      REAL :: dominant_value

      REAL :: lwthresh = .50

      INTEGER , PARAMETER :: iswater_soil = 14
      INTEGER :: iforce
      CHARACTER (LEN=1024) :: message

      

      DO j = jts , MIN(jde-1,jte)
         DO i = its , MIN(ide-1,ite)
            dominant_value = landuse_frac(i,iswater,j)
            IF ( dominant_value .EQ. lwthresh ) THEN
               DO l = 1 , num_veg_cat
                  IF ( l .EQ. iswater ) CYCLE
                  IF ( landuse_frac(i,l,j) .EQ. lwthresh ) THEN
                write(message, FMT='(I4,I4,A,I4,A,F12.4)') i,j,' water and category ',l,' both at 50%, landmask is ',landmask(i,j)
                call wrf_message ( message )
                     landuse_frac(i,l,j) = lwthresh - .01
                     landuse_frac(i,iswater,j) = 1. - (lwthresh + 0.01)
                  END IF
               END DO
            END IF
         END DO
      END DO

      

      DO j = jts , MIN(jde-1,jte)
         DO i = its , MIN(ide-1,ite)
            dominant_value = landuse_frac(i,1,j)
            dominant_index = 1
            DO l = 2 , num_veg_cat
               IF        ( l .EQ. iswater ) THEN
                  
               ELSE IF ( ( l .NE. iswater ) .AND. ( landuse_frac(i,l,j) .GT. dominant_value ) ) THEN
                  dominant_value = landuse_frac(i,l,j)
                  dominant_index = l
               END IF
            END DO
            IF ( landuse_frac(i,iswater,j) .GT. lwthresh ) THEN
               dominant_value = landuse_frac(i,iswater,j)
               dominant_index = iswater
ELSEIF((landuse_frac(i,iswater,j).EQ.lwthresh).AND.(dominant_value.EQ.lwthresh).and.(dominant_index.LT.iswater))THEN
write(message,*)'temporary SI landmask fix'
call wrf_message(trim(message))

ELSEIF((landuse_frac(i,iswater,j).EQ.lwthresh).AND.(dominant_value.EQ.lwthresh).and.(dominant_index.GT.iswater))THEN
write(message,*)'temporary SI landmask fix'
call wrf_message(trim(message))
dominant_value = landuse_frac(i,iswater,j)
dominant_index = iswater
            ELSE IF ( ( landuse_frac(i,iswater,j) .EQ. lwthresh ) .AND. &
                      ( dominant_value .LT. lwthresh ) ) THEN
               dominant_value = landuse_frac(i,iswater,j)
               dominant_index = iswater
            END IF
            IF      ( dominant_index .EQ. iswater ) THEN
if(landmask(i,j).gt.lwthresh) then
write(message,*) 'changing to water at point ',i,j
call wrf_message(trim(message))
write(message,*)  nint(landuse_frac(i,:,j)*100)
call wrf_message(trim(message))
endif
               landmask(i,j) = 0
            ELSE IF ( dominant_index .NE. iswater ) THEN
if(landmask(i,j).lt.lwthresh) then
write(message,*) 'changing to land at point ',i,j
call wrf_message(trim(message))
write(message,*) nint(landuse_frac(i,:,j)*100)
call wrf_message(trim(message))
endif
               landmask(i,j) = 1
            END IF
            ivgtyp(i,j) = dominant_index
         END DO
      END DO

      

      iforce = 0
      DO i = its , MIN(ide-1,ite)
         DO j = jts , MIN(jde-1,jte)
            dominant_value = soil_top_cat(i,1,j)
            dominant_index = 1
            IF ( landmask(i,j) .GT. lwthresh ) THEN
               DO l = 2 , num_soil_top_cat
                  IF ( ( l .NE. iswater_soil ) .AND. ( soil_top_cat(i,l,j) .GT. dominant_value ) ) THEN
                     dominant_value = soil_top_cat(i,l,j)
                     dominant_index = l
                  END IF
               END DO
               IF ( dominant_value .LT. 0.01 ) THEN
                  iforce = iforce + 1
                  WRITE ( message , FMT = '(A,I4,I4)' ) &
                  'based on landuse, changing soil to land at point ',i,j
                  CALL wrf_debug(1,message)
                  WRITE ( message , FMT = '(16(i3,1x))' ) &
                  1, 2, 3, 4, 5, 6, 7, 8, 9, 10,11,12, 13, 14, 15, 16
                  CALL wrf_debug(1,message)
                  WRITE ( message , FMT = '(16(i3,1x))' ) &
                  nint(soil_top_cat(i,:,j)*100)
                  CALL wrf_debug(1,message)
                  dominant_index = 8
               END IF
            ELSE
               dominant_index = iswater_soil
            END IF
            isltyp(i,j) = dominant_index
         END DO
      END DO

if(iforce.ne.0)then
WRITE(message,FMT='(A,I4,A,I6)' ) &
'forcing artificial silty clay loam at ',iforce,' points, out of ',&
(MIN(ide-1,ite)-its+1)*(MIN(jde-1,jte)-jts+1)
CALL wrf_debug(0,message)
endif

   END SUBROUTINE process_percent_cat_new

   SUBROUTINE process_soil_real ( tsk , tmn , &
                                landmask , sst , &
                                st_input , sm_input , sw_input , st_levels_input , sm_levels_input , sw_levels_input , &
                                zs , dzs , tslb , smois , sh2o , &
                                flag_sst , flag_soilt000, flag_soilm000, &
                                ids , ide , jds , jde , kds , kde , &
                                ims , ime , jms , jme , kms , kme , &
                                its , ite , jts , jte , kts , kte , &
                                sf_surface_physics , num_soil_layers , real_data_init_type , &
                                num_st_levels_input , num_sm_levels_input , num_sw_levels_input , &
                                num_st_levels_alloc , num_sm_levels_alloc , num_sw_levels_alloc )

      IMPLICIT NONE

      INTEGER , INTENT(IN) :: ids , ide , jds , jde , kds , kde , &
                              ims , ime , jms , jme , kms , kme , &
                              its , ite , jts , jte , kts , kte , &
                              sf_surface_physics , num_soil_layers , real_data_init_type , &
                              num_st_levels_input , num_sm_levels_input , num_sw_levels_input , &
                              num_st_levels_alloc , num_sm_levels_alloc , num_sw_levels_alloc

      INTEGER , INTENT(IN) :: flag_sst, flag_soilt000, flag_soilm000

      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN) :: landmask , sst

      INTEGER , DIMENSION(1:num_st_levels_input) , INTENT(INOUT) :: st_levels_input
      INTEGER , DIMENSION(1:num_sm_levels_input) , INTENT(INOUT) :: sm_levels_input
      INTEGER , DIMENSION(1:num_sw_levels_input) , INTENT(INOUT) :: sw_levels_input
      REAL , DIMENSION(ims:ime,1:num_st_levels_alloc,jms:jme) , INTENT(INOUT) :: st_input
      REAL , DIMENSION(ims:ime,1:num_sm_levels_alloc,jms:jme) , INTENT(INOUT) :: sm_input
      REAL , DIMENSION(ims:ime,1:num_sw_levels_alloc,jms:jme) , INTENT(INOUT) :: sw_input

      REAL, DIMENSION(1:num_soil_layers), INTENT(OUT)  ::  zs,dzs
      REAL , DIMENSION(ims:ime,num_soil_layers,jms:jme) , INTENT(OUT) :: tslb , smois , sh2o
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN) :: tmn
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(INOUT) :: tsk

      INTEGER :: i , j , l , dominant_index , num_soil_cat , num_veg_cat
      REAL :: dominant_value

      

      IF      ( ( sf_surface_physics .EQ. 1 ) .AND. ( num_soil_layers .GT. 1 ) ) THEN
         CALL init_soil_depth_1 ( zs , dzs , num_soil_layers )
         CALL init_soil_1_real ( tsk , tmn , tslb , zs , dzs , num_soil_layers , real_data_init_type , &
                                 landmask , sst , flag_sst , &
                                 ids , ide , jds , jde , kds , kde , &
                                 ims , ime , jms , jme , kms , kme , &
                                 its , ite , jts , jte , kts , kte )

      ELSE IF ( ( sf_surface_physics .EQ. 2 .or. sf_surface_physics .EQ. 99 .or. sf_surface_physics .eq. 88 ) .AND. &
                ( num_soil_layers .GT. 1 ) ) THEN
         CALL init_soil_depth_2 ( zs , dzs , num_soil_layers )
         CALL init_soil_2_real ( tsk , tmn , smois , sh2o , tslb , &
                                 st_input , sm_input , sw_input , landmask , sst , &
                                 zs , dzs , &
                                 st_levels_input , sm_levels_input , sw_levels_input , &
                                 num_soil_layers , num_st_levels_input , num_sm_levels_input , num_sw_levels_input , &
                                 num_st_levels_alloc , num_sm_levels_alloc , num_sw_levels_alloc , &
                                 flag_sst , flag_soilt000 , flag_soilm000 , &
                                 ids , ide , jds , jde , kds , kde , &
                                 ims , ime , jms , jme , kms , kme , &
                                 its , ite , jts , jte , kts , kte )










      ELSE IF ( ( sf_surface_physics .EQ. 3 ) .AND. ( num_soil_layers .GT. 1 ) ) THEN
         CALL init_soil_depth_3 ( zs , dzs , num_soil_layers )
         CALL init_soil_3_real ( tsk , tmn , smois , tslb , &
                                 st_input , sm_input , landmask , sst , &
                                 zs , dzs , &
                                 st_levels_input , sm_levels_input , &
                                 num_soil_layers , num_st_levels_input , num_sm_levels_input , &
                                 num_st_levels_alloc , num_sm_levels_alloc , &
                                 flag_sst , flag_soilt000 , flag_soilm000 , &
                                 ids , ide , jds , jde , kds , kde , &
                                 ims , ime , jms , jme , kms , kme , &
                                 its , ite , jts , jte , kts , kte )
      END IF

   END SUBROUTINE process_soil_real

   SUBROUTINE process_soil_ideal ( xland,xice,vegfra,snow,canwat,  &
                                   ivgtyp,isltyp,tslb,smois, &
                                   tsk,tmn,zs,dzs,           &
                                   num_soil_layers,          &
                                   sf_surface_physics ,      &
                                   ids,ide, jds,jde, kds,kde,&
                                   ims,ime, jms,jme, kms,kme,&
                                   its,ite, jts,jte, kts,kte )

      IMPLICIT NONE

      INTEGER, INTENT(IN) ::ids,ide, jds,jde, kds,kde,  &
                            ims,ime, jms,jme, kms,kme,  &
                            its,ite, jts,jte, kts,kte

      INTEGER, INTENT(IN) :: num_soil_layers , sf_surface_physics

      REAL, DIMENSION( ims:ime, num_soil_layers, jms:jme ) , INTENT(INOUT) :: smois, tslb

      REAL, DIMENSION(num_soil_layers), INTENT(OUT) :: dzs,zs

      REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT) :: tsk, tmn
      REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(OUT) :: xland, snow, canwat, xice, vegfra
      INTEGER, DIMENSION( ims:ime, jms:jme ) , INTENT(OUT) :: ivgtyp, isltyp

      

      INTEGER :: itf,jtf

      itf=MIN(ite,ide-1)
      jtf=MIN(jte,jde-1)

      IF      ( ( sf_surface_physics .EQ. 1 ) .AND. ( num_soil_layers .GT. 1 ) ) THEN
         CALL init_soil_depth_1 ( zs , dzs , num_soil_layers )
         CALL init_soil_1_ideal(tsk,tmn,tslb,xland,                      &
                                ivgtyp,zs,dzs,num_soil_layers,           &
                                ids,ide, jds,jde, kds,kde,               &
                                ims,ime, jms,jme, kms,kme,               &
                                its,ite, jts,jte, kts,kte                )
      ELSE IF ( ( sf_surface_physics .EQ. 2 ) .AND. ( num_soil_layers .GT. 1 ) ) THEN
         CALL init_soil_depth_2 ( zs , dzs , num_soil_layers )
         CALL init_soil_2_ideal ( xland,xice,vegfra,snow,canwat,         &
                                  ivgtyp,isltyp,tslb,smois,tmn,          &
                                  num_soil_layers,                       &
                                  ids,ide, jds,jde, kds,kde,             &
                                  ims,ime, jms,jme, kms,kme,             &
                                  its,ite, jts,jte, kts,kte              )
      ELSE IF ( ( sf_surface_physics .EQ. 3 ) .AND. ( num_soil_layers .GT. 1 ) ) THEN
         CALL init_soil_depth_3 ( zs , dzs , num_soil_layers )

      END IF

   END SUBROUTINE process_soil_ideal

   SUBROUTINE adjust_soil_temp_new ( tmn , sf_surface_physics , &
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
      INTEGER , INTENT(IN) :: sf_surface_physics , flag_toposoil

      INTEGER :: i , j

      REAL :: soil_elev_min_val ,  soil_elev_max_val , soil_elev_min_dif , soil_elev_max_dif

      

      IF ( flag_toposoil .EQ. 1 ) THEN

         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)

               
               
               
               
               
               
               

               soil_elev_min_val = toposoil(i,j)
               soil_elev_max_val = toposoil(i,j)
               soil_elev_min_dif = ter(i,j) - toposoil(i,j)
               soil_elev_max_dif = ter(i,j) - toposoil(i,j)

               IF      ( ( soil_elev_min_val .LT. -1000 ) .AND. ( landmask(i,j) .LT. 0.5 ) ) THEN
                  CYCLE
               ELSE IF ( ( soil_elev_min_val .LT. -1000 ) .AND. ( landmask(i,j) .GT. 0.5 ) ) THEN

cycle

               ENDIF

               IF      ( ( soil_elev_max_val .GT. 10000 ) .AND. ( landmask(i,j) .LT. 0.5 ) ) THEN
                  CYCLE
               ELSE IF ( ( soil_elev_max_val .GT. 10000 ) .AND. ( landmask(i,j) .GT. 0.5 ) ) THEN
print *,'no soil temperature elevation adjustment, soil height too high = ',toposoil(i,j)
cycle
                  CALL wrf_error_fatal3("",590,&
'TOPOSOIL values have large positive values > 10,000 m , unrealistic.' )
               ENDIF

               IF      ( ( ( soil_elev_min_dif .LT. -3000 ) .OR. ( soil_elev_max_dif .GT. 3000 ) ) .AND. &
                           ( landmask(i,j) .LT. 0.5 ) ) THEN
                  CYCLE
               ELSE IF ( ( ( soil_elev_min_dif .LT. -3000 ) .OR. ( soil_elev_max_dif .GT. 3000 ) ) .AND. &
                           ( landmask(i,j) .GT. 0.5 ) ) THEN
print *,'no soil temperature elevation adjustment, diff of soil height and terrain = ',ter(i,j) - toposoil(i,j)
cycle
                  CALL wrf_error_fatal3("",601,&
'TOPOSOIL difference with terrain elevation differs by more than 3000 m, unrealistic' )
               ENDIF

               
               
               

               IF (landmask(i,j) .GT. 0.5 ) THEN

                  IF ( sf_surface_physics .EQ. 1 .OR. sf_surface_physics .EQ. 7) THEN
                     tmn(i,j) = tmn(i,j) - 0.0065 * ( ter(i,j) - toposoil(i,j) )
                  END IF

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


   SUBROUTINE init_soil_depth_1 ( zs , dzs , num_soil_layers )

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: num_soil_layers

      REAL, DIMENSION(1:num_soil_layers), INTENT(OUT)  ::  zs,dzs

      INTEGER                   ::      l

      CHARACTER (LEN=132) :: message

      
      

      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      

      IF ( num_soil_layers .NE. 5 ) THEN
         write(message,FMT= '(A)') 'Usually, the 5-layer diffusion uses 5 layers.  Change this in the namelist.'
         CALL wrf_error_fatal3("",700,&
message )
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

      CHARACTER (LEN=132) :: message

      dzs = (/ 0.1 , 0.3 , 0.6 , 1.0 /)

      IF ( num_soil_layers .NE. 4 ) THEN
         write(message,FMT='(A)') 'Usually, the LSM uses 4 layers.  Change this in the namelist.'
         CALL wrf_error_fatal3("",730,&
message )
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

      CHARACTER (LEN=132) :: message







     IF ( num_soil_layers .EQ. 6) THEN
      zs  = (/ 0.00 , 0.05 , 0.20 , 0.40 , 1.60 , 3.00 /)

     ELSEIF ( num_soil_layers .EQ. 9) THEN
      zs  = (/ 0.00 , 0.05 , 0.20 , 0.40 , 0.60, 1.00, 1.60 , 2.20, 3.00 /)

     ENDIF

      IF ( num_soil_layers .EQ. 4 .OR. num_soil_layers .EQ. 5 ) THEN
         WRITE(message,FMT= '(A)')'Usually, the RUC LSM uses 6, 9 or more levels.  Change this in the namelist.'
         CALL wrf_error_fatal3("",770,&
message )
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
                       ivgtyp,ZS,DZS,num_soil_layers,           &
                       ids,ide, jds,jde, kds,kde,               &
                       ims,ime, jms,jme, kms,kme,               &
                       its,ite, jts,jte, kts,kte                )

      IMPLICIT NONE

      INTEGER, INTENT(IN   )    ::      ids,ide, jds,jde, kds,kde, &
                                        ims,ime, jms,jme, kms,kme, &
                                        its,ite, jts,jte, kts,kte

      INTEGER, INTENT(IN   )    ::      num_soil_layers

      REAL, DIMENSION( ims:ime , 1 , jms:jme ), INTENT(OUT) :: tslb
      REAL, DIMENSION( ims:ime , jms:jme ), INTENT(OUT) :: xland
      INTEGER, DIMENSION( ims:ime , jms:jme ), INTENT(OUT) :: ivgtyp

      REAL, DIMENSION(1:), INTENT(IN) :: dzs,zs

      REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(IN) :: tsk, tmn

      

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

   SUBROUTINE init_soil_2_real ( tsk , tmn , smois , sh2o , tslb , &
                                 st_input , sm_input , sw_input , landmask , sst , &
                                 zs , dzs , &
                                 st_levels_input , sm_levels_input , sw_levels_input , &
                                 num_soil_layers , num_st_levels_input , num_sm_levels_input ,  num_sw_levels_input , &
                                 num_st_levels_alloc , num_sm_levels_alloc ,  num_sw_levels_alloc , &
                                 flag_sst , flag_soilt000 , flag_soilm000 , &
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

      INTEGER , INTENT(IN) :: flag_sst, flag_soilt000, flag_soilm000

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

      REAL , DIMENSION(ims:ime,num_soil_layers,jms:jme) , INTENT(OUT) :: tslb , smois , sh2o

      REAL , ALLOCATABLE , DIMENSION(:) :: zhave

      INTEGER :: i , j , l , lout , lin , lwant , lhave , num
      REAL :: temp
      LOGICAL :: found_levels

      CHARACTER (LEN=132) :: message

      

      num = num_st_levels_input * num_sm_levels_input

      IF ( num .GE. 1 ) THEN

         
         IF ( flag_soilt000 .eq. 1 ) THEN
           write(message, FMT='(A)') ' Assume RUC LSM 6-level input'
           CALL wrf_message ( message )
           ALLOCATE ( zhave( MAX(num_st_levels_input,num_sm_levels_input,num_sw_levels_input)  ) )
         ELSE
           write(message, FMT='(A)') ' Assume Noah LSM input'
           CALL wrf_message ( message )
         ALLOCATE ( zhave( MAX(num_st_levels_input,num_sm_levels_input,num_sw_levels_input) +2) )
         END IF


         



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

      IF ( flag_soilt000 .NE. 1 ) THEN
         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               st_input(i,1,j) = tsk(i,j)
               st_input(i,num_st_levels_input+2,j) = tmn(i,j)
            END DO
         END DO
      ENDIF













  616	format(20(f4.0,1x))

         

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

      IF ( flag_soilm000 .NE. 1 ) THEN
         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               sm_input(i,1,j) = sm_input(i,2,j)
               sm_input(i,num_sm_levels_input+2,j) = sm_input(i,num_sm_levels_input+1,j)
            END DO
         END DO
      ENDIF

         

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
         IF ( num_sw_levels_input .GT. 1 ) THEN
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  sw_input(i,1,j) = sw_input(i,2,j)
                  sw_input(i,num_sw_levels_input+2,j) = sw_input(i,num_sw_levels_input+1,j)
               END DO
            END DO
         END IF

         found_levels = .TRUE.

      ELSE IF ( ( num .LE. 0 ) .AND. (  start_date .NE. current_date ) ) THEN

         found_levels = .FALSE.

      ELSE
         CALL wrf_error_fatal3("",1047,&
         'No input soil level data (temperature, moisture or liquid, or all are missing). Required for LSM.' )
      END IF

      

      IF ( found_levels ) THEN


      IF ( flag_soilt000 .NE.1) THEN

         
         

         zhave(1) = 0.
         DO l = 1 , num_st_levels_input
            zhave(l+1) = st_levels_input(l) / 100.
         END DO
         zhave(num_st_levels_input+2) = 300. / 100.

         

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

     ELSE


         DO l = 1 , num_st_levels_input
            zhave(l) = st_levels_input(l) / 100.
         END DO

      


      z_wantt_2 : DO lwant = 1 , num_soil_layers
         z_havet_2 : DO lhave = 1 , num_st_levels_input -1
            IF ( ( zs(lwant) .GE. zhave(lhave  ) ) .AND. &
                 ( zs(lwant) .LE. zhave(lhave+1) ) ) THEN
               DO j = jts , MIN(jde-1,jte)
                  DO i = its , MIN(ide-1,ite)
                     tslb(i,lwant,j)= ( st_input(i,lhave,j ) * ( zhave(lhave+1) - zs   (lwant) ) + &
                                        st_input(i,lhave+1,j) * ( zs   (lwant  ) - zhave(lhave) ) ) / &
                                                                ( zhave(lhave+1) - zhave(lhave) )
                  END DO
               END DO
               EXIT z_havet_2
            END IF
         END DO z_havet_2
      END DO z_wantt_2

      ENDIF


      IF ( flag_soilm000 .NE. 1 ) THEN

         
         
         
         

         zhave(1) = 0.
         DO l = 1 , num_sm_levels_input
            zhave(l+1) = sm_levels_input(l) / 100.
         END DO
         zhave(num_sm_levels_input+2) = 300. / 100.

         

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

     ELSE


         DO l = 1 , num_sm_levels_input
            zhave(l) = sm_levels_input(l) / 100.
         END DO

      

      z_wantm_2 : DO lwant = 1 , num_soil_layers
         z_havem_2 : DO lhave = 1 , num_sm_levels_input -1
            IF ( ( zs(lwant) .GE. zhave(lhave  ) ) .AND. &
                 ( zs(lwant) .LE. zhave(lhave+1) ) ) THEN
               DO j = jts , MIN(jde-1,jte)
                  DO i = its , MIN(ide-1,ite)
                     smois(i,lwant,j)= ( sm_input(i,lhave,j ) * ( zhave(lhave+1) - zs   (lwant) ) + &
                                         sm_input(i,lhave+1,j) * ( zs   (lwant  ) - zhave(lhave) ) ) / &
                                                                 ( zhave(lhave+1) - zhave(lhave) )
                  END DO
               END DO
               EXIT z_havem_2
             END IF
         END DO z_havem_2
      END DO z_wantm_2

     ENDIF
         

         IF ( num_sw_levels_input .GT. 1 ) THEN

            zhave(1) = 0.
            DO l = 1 , num_sw_levels_input
               zhave(l+1) = sw_levels_input(l) / 100.
            END DO
            zhave(num_sw_levels_input+2) = 300. / 100.

            

            z_wantw : DO lwant = 1 , num_soil_layers
               z_havew : DO lhave = 1 , num_sw_levels_input +2 -1
                  IF ( ( zs(lwant) .GE. zhave(lhave  ) ) .AND. &
                       ( zs(lwant) .LE. zhave(lhave+1) ) ) THEN
                     DO j = jts , MIN(jde-1,jte)
                        DO i = its , MIN(ide-1,ite)
                           sh2o(i,lwant,j)= ( sw_input(i,lhave  ,j) * ( zhave(lhave+1) - zs   (lwant) ) + &
                                               sw_input(i,lhave+1,j) * ( zs   (lwant  ) - zhave(lhave) ) ) / &
                                                                       ( zhave(lhave+1) - zhave(lhave) )
                        END DO
                     END DO
                     EXIT z_havew
                  END IF
               END DO z_havew
            END DO z_wantw

         END IF


         
         

         IF ( flag_sst .EQ. 1 ) THEN
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  IF ( landmask(i,j) .LT. 0.5 ) THEN
                     DO l = 1 , num_soil_layers
                        tslb(i,l,j)= sst(i,j)

                        tsk(i,j)    = sst(i,j)
                        smois(i,l,j)= 1.0
                        sh2o (i,l,j)= 1.0
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
                        sh2o (i,l,j)= 1.0
                     END DO
                  END IF
               END DO
            END DO
         END IF

         DEALLOCATE (zhave)

      END IF

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
                                 st_input , sm_input , landmask, sst, &
                                 zs , dzs , &
                                 st_levels_input , sm_levels_input , &
                                 num_soil_layers , num_st_levels_input , num_sm_levels_input ,  &
                                 num_st_levels_alloc , num_sm_levels_alloc , &
                                 flag_sst , flag_soilt000 , flag_soilm000 , &
                                 ids , ide , jds , jde , kds , kde , &
                                 ims , ime , jms , jme , kms , kme , &
                                 its , ite , jts , jte , kts , kte )

      IMPLICIT NONE

      INTEGER , INTENT(IN) :: num_soil_layers , &
                              num_st_levels_input , num_sm_levels_input , &
                              num_st_levels_alloc , num_sm_levels_alloc , &
                              ids , ide , jds , jde , kds , kde , &
                              ims , ime , jms , jme , kms , kme , &
                              its , ite , jts , jte , kts , kte

      INTEGER , INTENT(IN) :: flag_sst, flag_soilt000, flag_soilm000

      INTEGER , DIMENSION(1:num_st_levels_input) , INTENT(INOUT) :: st_levels_input
      INTEGER , DIMENSION(1:num_sm_levels_input) , INTENT(INOUT) :: sm_levels_input

      REAL , DIMENSION(ims:ime,1:num_st_levels_alloc,jms:jme) , INTENT(INOUT) :: st_input
      REAL , DIMENSION(ims:ime,1:num_sm_levels_alloc,jms:jme) , INTENT(INOUT) :: sm_input
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN) :: landmask , sst

      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN) :: tmn
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(INOUT) :: tsk
      REAL , DIMENSION(num_soil_layers) :: zs , dzs

      REAL , DIMENSION(ims:ime,num_soil_layers,jms:jme) , INTENT(OUT) :: tslb , smois

      REAL , ALLOCATABLE , DIMENSION(:) :: zhave

      INTEGER :: i , j , l , lout , lin , lwant , lhave
      REAL :: temp

      

      IF ( ( num_st_levels_input .LE. 0 ) .OR. &
           ( num_sm_levels_input .LE. 0 ) ) THEN
         PRINT '(A)','No input soil level data (either temperature or moisture, or both are missing).  Required for RUC LSM.'
         CALL wrf_error_fatal3("",1347,&
'no soil data' )
      ELSE
         IF ( flag_soilt000 .eq. 1 ) THEN
           PRINT '(A)',' Assume RUC LSM 6-level input'
           ALLOCATE ( zhave( MAX(num_st_levels_input,num_sm_levels_input)  ) )
         ELSE
           PRINT '(A)',' Assume non-RUC LSM input'
           ALLOCATE ( zhave( MAX(num_st_levels_input,num_soil_layers)  ) )
         END IF
      END IF

      

      outert : DO lout = 1 , num_st_levels_input-1
         innert : DO lin = lout+1 , num_st_levels_input
            IF ( st_levels_input(lout) .GT. st_levels_input(lin) ) THEN
               temp = st_levels_input(lout)
               st_levels_input(lout) = st_levels_input(lin)
               st_levels_input(lin) = NINT(temp)
               DO j = jts , MIN(jde-1,jte)
                  DO i = its , MIN(ide-1,ite)
                     temp = st_input(i,lout,j)
                     st_input(i,lout,j) = st_input(i,lin,j)
                     st_input(i,lin,j) = temp
                  END DO
               END DO
            END IF
         END DO innert
      END DO outert

      IF ( flag_soilt000 .NE. 1 ) THEN
      DO j = jts , MIN(jde-1,jte)
         DO i = its , MIN(ide-1,ite)
            st_input(i,1,j) = tsk(i,j)
            st_input(i,num_st_levels_input+2,j) = tmn(i,j)
         END DO
      END DO
      END IF

      

      outerm: DO lout = 1 , num_sm_levels_input-1
         innerm : DO lin = lout+1 , num_sm_levels_input
            IF ( sm_levels_input(lout) .GT. sm_levels_input(lin) ) THEN
               temp = sm_levels_input(lout)
               sm_levels_input(lout) = sm_levels_input(lin)
               sm_levels_input(lin) = NINT(temp)
               DO j = jts , MIN(jde-1,jte)
                  DO i = its , MIN(ide-1,ite)
                     temp = sm_input(i,lout,j)
                     sm_input(i,lout,j) = sm_input(i,lin,j)
                     sm_input(i,lin,j) = temp
                  END DO
               END DO
            END IF
         END DO innerm
      END DO outerm

      IF ( flag_soilm000 .NE. 1 ) THEN
      DO j = jts , MIN(jde-1,jte)
         DO i = its , MIN(ide-1,ite)
            sm_input(i,1,j) = sm_input(i,2,j)
            sm_input(i,num_sm_levels_input+2,j) = sm_input(i,num_sm_levels_input+1,j)
         END DO
      END DO
      END IF

      

      IF ( flag_soilt000 .EQ. 1 ) THEN
         DO l = 1 , num_st_levels_input
            zhave(l) = st_levels_input(l) / 100.
         END DO

      

      z_wantt : DO lwant = 1 , num_soil_layers
         z_havet : DO lhave = 1 , num_st_levels_input -1
            IF ( ( zs(lwant) .GE. zhave(lhave  ) ) .AND. &
                 ( zs(lwant) .LE. zhave(lhave+1) ) ) THEN
               DO j = jts , MIN(jde-1,jte)
                  DO i = its , MIN(ide-1,ite)
                     tslb(i,lwant,j)= ( st_input(i,lhave,j ) * ( zhave(lhave+1) - zs   (lwant) ) + &
                                        st_input(i,lhave+1,j) * ( zs   (lwant  ) - zhave(lhave) ) ) / &
                                                                ( zhave(lhave+1) - zhave(lhave) )
                  END DO
               END DO
               EXIT z_havet
            END IF
         END DO z_havet
      END DO z_wantt

      ELSE

         zhave(1) = 0.
         DO l = 1 , num_st_levels_input
            zhave(l+1) = st_levels_input(l) / 100.
         END DO
         zhave(num_st_levels_input+2) = 300. / 100.

      

      z_wantt_2 : DO lwant = 1 , num_soil_layers
         z_havet_2 : DO lhave = 1 , num_st_levels_input +2
            IF ( ( zs(lwant) .GE. zhave(lhave  ) ) .AND. &
                 ( zs(lwant) .LE. zhave(lhave+1) ) ) THEN
               DO j = jts , MIN(jde-1,jte)
                  DO i = its , MIN(ide-1,ite)
                     tslb(i,lwant,j)= ( st_input(i,lhave,j ) * ( zhave(lhave+1) - zs   (lwant) ) + &
                                        st_input(i,lhave+1,j) * ( zs   (lwant  ) - zhave(lhave) ) ) / &
                                                                ( zhave(lhave+1) - zhave(lhave) )
                  END DO
               END DO
               EXIT z_havet_2
            END IF
         END DO z_havet_2
      END DO z_wantt_2

      END IF

      

      IF ( flag_soilm000 .EQ. 1 ) THEN
         DO l = 1 , num_sm_levels_input
            zhave(l) = sm_levels_input(l) / 100.
         END DO

      

      z_wantm : DO lwant = 1 , num_soil_layers
         z_havem : DO lhave = 1 , num_sm_levels_input -1
            IF ( ( zs(lwant) .GE. zhave(lhave  ) ) .AND. &
                 ( zs(lwant) .LE. zhave(lhave+1) ) ) THEN
               DO j = jts , MIN(jde-1,jte)
                  DO i = its , MIN(ide-1,ite)
                     smois(i,lwant,j)= ( sm_input(i,lhave,j ) * ( zhave(lhave+1) - zs   (lwant) ) + &
                                         sm_input(i,lhave+1,j) * ( zs   (lwant  ) - zhave(lhave) ) ) / &
                                                                 ( zhave(lhave+1) - zhave(lhave) )
                  END DO
               END DO
               EXIT z_havem
            END IF
         END DO z_havem
      END DO z_wantm

      ELSE

         zhave(1) = 0.
         DO l = 1 , num_sm_levels_input
            zhave(l+1) = sm_levels_input(l) / 100.
         END DO
         zhave(num_sm_levels_input+2) = 300. / 100.

      z_wantm_2 : DO lwant = 1 , num_soil_layers
         z_havem_2 : DO lhave = 1 , num_sm_levels_input +2
            IF ( ( zs(lwant) .GE. zhave(lhave  ) ) .AND. &
                 ( zs(lwant) .LE. zhave(lhave+1) ) ) THEN
               DO j = jts , MIN(jde-1,jte)
                  DO i = its , MIN(ide-1,ite)
                     smois(i,lwant,j)= ( sm_input(i,lhave,j ) * ( zhave(lhave+1) - zs   (lwant) ) + &
                                         sm_input(i,lhave+1,j) * ( zs   (lwant  ) - zhave(lhave) ) ) / &
                                                                 ( zhave(lhave+1) - zhave(lhave) )
                  END DO
               END DO
               EXIT z_havem_2
            END IF
         END DO z_havem_2
      END DO z_wantm_2

      END IF
      
      

      IF ( flag_sst .EQ. 1 ) THEN
         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               IF ( landmask(i,j) .LT. 0.5 ) THEN
                  DO l = 1 , num_soil_layers
                     tslb(i,l,j) = sst(i,j)
                     tsk(i,j)    = sst(i,j)
                     smois(i,l,j)= 1.0
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
                  END DO
               END IF
            END DO
         END DO
      END IF

      DEALLOCATE (zhave)

   END SUBROUTINE init_soil_3_real

END MODULE module_soil_pre

