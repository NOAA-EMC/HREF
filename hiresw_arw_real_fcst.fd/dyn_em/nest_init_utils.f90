SUBROUTINE init_domain_constants_em ( parent , nest )
   USE module_domain
   USE module_configure
   IMPLICIT NONE
   TYPE(domain)  :: parent , nest

   INTEGER iswater , map_proj, julyr, julday
   REAL    cen_lat, cen_lon, truelat1 , truelat2 , gmt , moad_cen_lat , stand_lon
   CHARACTER (LEN=4) :: char_junk

! single-value constants

   nest%p_top   = parent%p_top
   nest%cfn     = parent%cfn
   nest%cfn1    = parent%cfn1
   nest%rdx     = 1./nest%dx
   nest%rdy     = 1./nest%dy
!  nest%dts     = nest%dt/float(nest%time_step_sound)
   nest%dtseps  = parent%dtseps  ! used in height model only?
   nest%resm    = parent%resm    ! used in height model only?
   nest%zetatop = parent%zetatop ! used in height model only?
   nest%cf1     = parent%cf1
   nest%cf2     = parent%cf2
   nest%cf3     = parent%cf3
   nest%gmt     = parent%gmt
   nest%julyr   = parent%julyr
   nest%julday  = parent%julday

   CALL nl_get_mminlu ( 1,char_junk(1:4) )
   CALL nl_get_iswater (1, iswater )
   CALL nl_get_truelat1 ( 1 , truelat1 )
   CALL nl_get_truelat2 ( 1 , truelat2 )
   CALL nl_get_moad_cen_lat ( 1 , moad_cen_lat )
   CALL nl_get_stand_lon ( 1 , stand_lon )
   CALL nl_get_map_proj ( 1 , map_proj )
   CALL nl_get_gmt ( 1 , gmt)
   CALL nl_get_julyr ( 1 , julyr)
   CALL nl_get_julday ( 1 , julday)
   IF ( nest%id .NE. 1 ) THEN
     CALL nl_set_gmt (nest%id, gmt)
     CALL nl_set_julyr (nest%id, julyr)
     CALL nl_set_julday (nest%id, julday)
     CALL nl_set_iswater (nest%id, iswater )
     CALL nl_set_truelat1 ( nest%id , truelat1 )
     CALL nl_set_truelat2 ( nest%id , truelat2 )
     CALL nl_set_moad_cen_lat ( nest%id , moad_cen_lat )
     CALL nl_set_stand_lon ( nest%id , stand_lon )
     CALL nl_set_map_proj ( nest%id , map_proj )
   END IF
   nest%gmt     = gmt 
   nest%julday  = julday
   nest%julyr   = julyr
   nest%iswater = iswater
   nest%cen_lat = cen_lat
   nest%cen_lon = cen_lon
   nest%truelat1= truelat1
   nest%truelat2= truelat2
   nest%moad_cen_lat= moad_cen_lat
   nest%stand_lon= stand_lon
   nest%map_proj= map_proj

   nest%step_number  = parent%step_number

! 1D constants (Z)

   nest%em_fnm    = parent%em_fnm
   nest%em_fnp    = parent%em_fnp
   nest%em_rdnw   = parent%em_rdnw
   nest%em_rdn    = parent%em_rdn
   nest%em_dnw    = parent%em_dnw
   nest%em_dn     = parent%em_dn
   nest%em_znu    = parent%em_znu
   nest%em_znw    = parent%em_znw
   nest%em_t_base = parent%em_t_base
   nest%u_base    = parent%u_base
   nest%v_base    = parent%v_base
   nest%qv_base   = parent%qv_base
   nest%z_base    = parent%z_base
   nest%dzs       = parent%dzs
   nest%zs        = parent%zs

END SUBROUTINE init_domain_constants_em

SUBROUTINE blend_terrain ( ter_interpolated , ter_input , &
                           ids , ide , jds , jde , kds , kde , & 
                           ims , ime , jms , jme , kms , kme , & 
                           ips , ipe , jps , jpe , kps , kpe )

   USE module_configure
   IMPLICIT NONE

   INTEGER , INTENT(IN)                       :: ids , ide , jds , jde , kds , kde , & 
                                                 ims , ime , jms , jme , kms , kme , & 
                                                 ips , ipe , jps , jpe , kps , kpe
   REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(IN)    :: ter_interpolated
   REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(INOUT) :: ter_input

   REAL , DIMENSION(ims:ime,kms:kme,jms:jme) :: ter_temp
   INTEGER :: i , j , k , spec_bdy_width
   REAL    :: r_blend_zones
   INTEGER blend_cell, blend_width

   !  The fine grid elevation comes from the horizontally interpolated
   !  parent elevation for the first spec_bdy_width row/columns, so we need
   !  to get that value.  We blend the coarse and fine in the next blend_width
   !  rows and columns.  After that, in the interior, it is 100% fine grid.

   CALL nl_get_spec_bdy_width ( 1, spec_bdy_width) 
   CALL nl_get_blend_width ( 1, blend_width)

   !  Initialize temp values to the nest ter elevation.  This fills in the values
   !  that will not be modified below.  

   DO j = jps , MIN(jpe, jde-1)
      DO k = kps , kpe
         DO i = ips , MIN(ipe, ide-1)
            ter_temp(i,k,j) = ter_input(i,k,j)
         END DO 
      END DO 
   END DO 

   !  To avoid some tricky indexing, we fill in the values inside out.  This allows
   !  us to overwrite incorrect assignments.  There are replicated assignments, and
   !  there is much unnecessary "IF test inside of a loop" stuff.  For a large
   !  domain, this is only a patch; for a small domain, this is not a biggy.

   r_blend_zones = 1./(blend_width+1)
   DO j = jps , MIN(jpe, jde-1)
      DO k = kps , kpe
         DO i = ips , MIN(ipe, ide-1)
            DO blend_cell = blend_width,1,-1
               IF   ( ( i .EQ.       spec_bdy_width + blend_cell ) .OR.  ( j .EQ.       spec_bdy_width + blend_cell ) .OR. &
                      ( i .EQ. ide - spec_bdy_width - blend_cell ) .OR.  ( j .EQ. jde - spec_bdy_width - blend_cell ) ) THEN
                  ter_temp(i,k,j) = ( (blend_cell)*ter_input(i,k,j) + (blend_width+1-blend_cell)*ter_interpolated(i,k,j) ) &
                                    * r_blend_zones
               END IF
            ENDDO
            IF      ( ( i .LE.       spec_bdy_width     ) .OR.  ( j .LE.       spec_bdy_width     ) .OR. &
                      ( i .GE. ide - spec_bdy_width     ) .OR.  ( j .GE. jde - spec_bdy_width     ) ) THEN
               ter_temp(i,k,j) =      ter_interpolated(i,k,j)
            END IF
         END DO 
      END DO 
   END DO 

   !  Set nest elevation with temp values.  All values not overwritten in the above
   !  loops have been previously set in the initial assignment.

   DO j = jps , MIN(jpe, jde-1)
      DO k = kps , kpe
         DO i = ips , MIN(ipe, ide-1)
            ter_input(i,k,j) = ter_temp(i,k,j)
         END DO 
      END DO 
   END DO 

END SUBROUTINE blend_terrain

SUBROUTINE store_terrain ( ter_interpolated , ter_input , &
                           ids , ide , jds , jde , kds , kde , & 
                           ims , ime , jms , jme , kms , kme , & 
                           ips , ipe , jps , jpe , kps , kpe )

   IMPLICIT NONE

   INTEGER , INTENT(IN)                       :: ids , ide , jds , jde , kds , kde , & 
                                                 ims , ime , jms , jme , kms , kme , & 
                                                 ips , ipe , jps , jpe , kps , kpe
   REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(OUT) :: ter_interpolated
   REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(IN)  :: ter_input

   INTEGER :: i , j , k

   DO j = jps , MIN(jpe, jde-1)
      DO k = kps , kpe
         DO i = ips , MIN(ipe, ide-1)
            ter_interpolated(i,k,j) = ter_input(i,k,j)
         END DO 
      END DO 
   END DO 

END SUBROUTINE store_terrain


SUBROUTINE input_terrain_rsmas ( grid ,                        &
                           ids , ide , jds , jde , kds , kde , &
                           ims , ime , jms , jme , kms , kme , &
                           ips , ipe , jps , jpe , kps , kpe )

   USE module_domain
   IMPLICIT NONE
   TYPE ( domain ) :: grid

   INTEGER , INTENT(IN)                       :: ids , ide , jds , jde , kds , kde , &
                                                 ims , ime , jms , jme , kms , kme , &
                                                 ips , ipe , jps , jpe , kps , kpe

   LOGICAL, EXTERNAL ::  wrf_dm_on_monitor

   INTEGER :: i , j , k , myproc
   INTEGER, DIMENSION(256) :: ipath  ! array for integer coded ascii for passing path down to get_terrain
   CHARACTER*256 :: message, message2
   CHARACTER*256 :: rsmas_data_path

! Local globally sized arrays
   REAL , DIMENSION(ids:ide,jds:jde) :: ht_g, xlat_g, xlon_g

   CALL wrf_get_myproc ( myproc ) 


   CALL nl_get_rsmas_data_path(1,rsmas_data_path)
   do i = 1, LEN(TRIM(rsmas_data_path))
      ipath(i) = ICHAR(rsmas_data_path(i:i))
   enddo


   CALL wrf_patch_to_global_real ( grid%xlat , xlat_g , grid%domdesc, ' ' , 'xy' ,       &
                                   ids, ide-1 , jds , jde-1 , 1 , 1 , &
                                   ims, ime   , jms , jme   , 1 , 1 , &
                                   ips, ipe   , jps , jpe   , 1 , 1   ) 
   CALL wrf_patch_to_global_real ( grid%xlong , xlon_g , grid%domdesc, ' ' , 'xy' ,       &
                                   ids, ide-1 , jds , jde-1 , 1 , 1 , &
                                   ims, ime   , jms , jme   , 1 , 1 , &
                                   ips, ipe   , jps , jpe   , 1 , 1   ) 

   IF ( wrf_dm_on_monitor() ) THEN
     CALL get_terrain ( grid%dx/1000., xlat_g(ids:ide,jds:jde), xlon_g(ids:ide,jds:jde), ht_g(ids:ide,jds:jde), &
                        ide-ids+1,jde-jds+1,ide-ids+1,jde-jds+1, ipath, LEN(TRIM(rsmas_data_path)) )
     WHERE ( ht_g(ids:ide,jds:jde) < -1000. ) ht_g(ids:ide,jds:jde) = 0.
   ENDIF

   CALL wrf_global_to_patch_real ( ht_g , grid%ht , grid%domdesc, ' ' , 'xy' ,         &
                                   ids, ide-1 , jds , jde-1 , 1 , 1 , &
                                   ims, ime   , jms , jme   , 1 , 1 , &
                                   ips, ipe   , jps , jpe   , 1 , 1   ) 

                       
END SUBROUTINE input_terrain_rsmas

SUBROUTINE update_after_feedback_em ( grid  &
!
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/em_dummy_new_args.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
,u_b,u_bt,v_b,v_bt,w_b,w_bt,ph_b,ph_bt,t_b,t_bt,mu_b,mu_bt,moist,moist_b,moist_bt,chem,scalar,scalar_b,scalar_bt,ozmixm, &
aerosolc_1,aerosolc_2,fdda3d,fdda2d &
!ENDOFREGISTRYGENERATEDINCLUDE
!
                 )
!
! perform core specific updates, exchanges after
! model feedback  (called from med_feedback_domain) -John
!

! Driver layer modules
   USE module_domain
   USE module_configure
   USE module_driver_constants
   USE module_machine
   USE module_tiles
   USE module_dm
   USE module_bc
! Mediation layer modules
! Registry generated module
   USE module_state_description

   IMPLICIT NONE

   !  Subroutine interface block.

   TYPE(domain) , TARGET         :: grid

   !  Definitions of dummy arguments
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/em_dummy_new_decl.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: u_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: u_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: v_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: v_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: w_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: w_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: ph_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: ph_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: t_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4)           :: t_bt
real      ,DIMENSION(max(grid%ed31,grid%ed33),1,grid%spec_bdy_width,4)           :: mu_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),1,grid%spec_bdy_width,4)           :: mu_bt
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_moist)           :: moist
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4,num_moist)           :: moist_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4,num_moist)           :: moist_bt
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_chem)           :: chem
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_scalar)           :: scalar
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4,num_scalar)           :: scalar_b
real      ,DIMENSION(max(grid%ed31,grid%ed33),grid%sd32:grid%ed32,grid%spec_bdy_width,4,num_scalar)           :: scalar_bt
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%levsiz,grid%sm33:grid%em33,num_ozmixm)           :: ozmixm
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%paerlev,grid%sm33:grid%em33,num_aerosolc)           :: aerosolc_1
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%paerlev,grid%sm33:grid%em33,num_aerosolc)           :: aerosolc_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_fdda3d)           :: fdda3d
real      ,DIMENSION(grid%sm31:grid%em31,1:1,grid%sm33:grid%em33,num_fdda2d)           :: fdda2d
!ENDOFREGISTRYGENERATEDINCLUDE

   INTEGER                         :: ids , ide , jds , jde , kds , kde , &
                                      ims , ime , jms , jme , kms , kme , &
                                      ips , ipe , jps , jpe , kps , kpe

  CALL wrf_debug( 500, "entering update_after_feedback_em" )

!STARTOFREGISTRYGENERATEDINCLUDE 'inc/em_data_calls.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
!ENDOFREGISTRYGENERATEDINCLUDE

!  Obtain dimension information stored in the grid data structure.
  CALL get_ijk_from_grid (  grid ,                   &
                            ids, ide, jds, jde, kds, kde,    &
                            ims, ime, jms, jme, kms, kme,    &
                            ips, ipe, jps, jpe, kps, kpe    )

  CALL wrf_debug( 500, "before HALO_EM_FEEDBACK.inc in update_after_feedback_em" )
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/HALO_EM_FEEDBACK.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_debug(2,'calling inc/HALO_EM_FEEDBACK.inc')
CALL RSL_LITE_INIT_EXCH ( local_communicator, 3, &
     0, 1, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
))
CALL RSL_LITE_PACK ( local_communicator, grid%ht, 3, 4, 0, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
CALL RSL_LITE_PACK ( local_communicator, grid%ht, 3, 4, 0, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
CALL RSL_LITE_INIT_EXCH ( local_communicator, 3 , &
     0, 1, 4, &
     0, 0, 4, &
     0, 0, 8, &
      0,  0, 4, &
      mytask, ntasks, ntasks_x, ntasks_y,   &
      ips, ipe, jps, jpe, kps, MAX(1,1&
))
CALL RSL_LITE_PACK ( local_communicator, grid%ht, 3, 4, 1, 0, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )
CALL RSL_LITE_PACK ( local_communicator, grid%ht, 3, 4, 1, 1, 'XY', 0, &
mytask, ntasks, ntasks_x, ntasks_y,       &
ids, ide, jds, jde, 1  , 1  ,             &
ims, ime, jms, jme, 1  , 1  ,             &
ips, ipe, jps, jpe, 1  , 1                )
!ENDOFREGISTRYGENERATEDINCLUDE
  CALL wrf_debug( 500, "leaving update_after_feedback_em" )

END SUBROUTINE update_after_feedback_em

