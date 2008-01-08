MODULE module_si_io_em

   !  Input 3D meteorological fields.

   REAL , DIMENSION(:,:,:) , ALLOCATABLE :: u_input , v_input , q_input , theta_input

   !  Input 3D LSM fields.

   REAL , DIMENSION(:,:,:) , ALLOCATABLE :: landuse_frac_input , soil_top_cat_input , soil_bot_cat_input

   !  Input 2D surface fields.

   REAL , DIMENSION(:,:)   , ALLOCATABLE :: tg_input , res_input , t_annual_avg_input , mu_input , &
                                            st000010_input , st010040_input , st040100_input , st100200_input , &
                                            sm000010_input , sm010040_input , sm040100_input , sm100200_input , &
                                            st010200_input , &
                                            sm010200_input , &
                                            soilt010_input , soilt040_input , soilt100_input , soilt200_input , &
                                            soilm010_input , soilm040_input , soilm100_input , soilm200_input , &
                                            seaice_input , snow_input , canwat_input , &
                                            landuse_input , landmask_input , &
                                            skintemp_input , sst_input , green_frac_input , &
                                            ter_input , ter_std_input , ter_slpx_input , ter_slpy_input , toposoil_input

   INTEGER , DIMENSION(:,:), ALLOCATABLE :: isltyp_input , ivgtyp_input

   !  Input 2D map information.

   REAL , DIMENSION(:,:)   , ALLOCATABLE :: msft_input , msfu_input , msfv_input , sina_input , cosa_input , &
                                            e_input , f_input , lat_input , lon_input

   REAL , DIMENSION(:)     , ALLOCATABLE :: etafull , etahalf

   LOGICAL , SAVE :: first_time_in = .TRUE.

   LOGICAL :: flag_st000010 , flag_st010040 , flag_st040100 , flag_st100200 , &
              flag_sm000010 , flag_sm010040 , flag_sm040100 , flag_sm100200 , &
              flag_st010200 , &
              flag_sm010200
   LOGICAL :: flag_soilt010 , flag_soilt040 , flag_soilt100 , flag_soilt200 , &
              flag_soilm010 , flag_soilm040 , flag_soilm100 , flag_soilm200

   LOGICAL :: flag_sst , flag_toposoil

   INTEGER                  :: num_st_levels_input , num_sm_levels_input
   INTEGER , DIMENSION(100) ::     st_levels_input ,     sm_levels_input
   REAL , ALLOCATABLE , DIMENSION(:,:,:) :: st_input , sm_input

!   Some constants to allow simple dimensions in the defined types
!   given below.

   INTEGER, PARAMETER          :: max_domains = 10
   INTEGER, PARAMETER          :: var_maxdims = 5
   INTEGER, PARAMETER          :: max_staggers_xy_new = 4
   INTEGER, PARAMETER          :: max_staggers_xy_old = 3
   INTEGER, PARAMETER          :: max_staggers_z = 2
   INTEGER, PARAMETER          :: max_standard_lats = 4
   INTEGER, PARAMETER          :: max_standard_lons = 4  
   INTEGER, PARAMETER          :: max_fg_variables = 200
   INTEGER, PARAMETER          :: max_vertical_levels = 2000

!   This module defines the items needed for the WRF metadata
!   which is broken up into three levels:  
!      Global metadata:  Those things which apply to the
!                        entire simulation that are 
!                        independent of time, domain, or
!                        variable
!
!      Domain metadata:  Those things which apply to 
!                        a single domain (this may
!                        or may not be time dependent)
!
!      Variable metadata: Those things which apply to 
!                        a specific variable at a 
!                        specific time
!
!      The variable names and definitions can be 
!      found in the wrf_metadata spec, which is still
!      a living document as coding goes on.   The names
!      may not match exactly, but you should be able 
!      to figure things out.  
!

   TYPE wrf_var_metadata
      CHARACTER (LEN=8)         :: name 
      CHARACTER (LEN=16)        :: units
      CHARACTER (LEN=80)        :: description
      INTEGER                   :: domain_id
      INTEGER                   :: ndim
      INTEGER                   :: dim_val (var_maxdims)
      CHARACTER(LEN=4)          :: dim_desc (var_maxdims)
      INTEGER                   :: start_index(var_maxdims)
      INTEGER                   :: stop_index(var_maxdims)
      INTEGER                   :: h_stagger_index
      INTEGER                   :: v_stagger_index
      CHARACTER(LEN=8)          :: array_order
      CHARACTER(LEN=4)          :: field_type
      CHARACTER(LEN=8)          :: field_source_prog
      CHARACTER(LEN=80)         :: source_desc
      CHARACTER(LEN=8)          :: field_time_type
      INTEGER                   :: vt_date_start
      REAL                      :: vt_time_start
      INTEGER                   :: vt_date_stop
      REAL                      :: vt_time_stop
   END TYPE wrf_var_metadata

   TYPE(wrf_var_metadata)  :: var_meta , var_info

   TYPE wrf_domain_metadata
      INTEGER                   :: id
      INTEGER                   :: parent_id
      CHARACTER(LEN=8)          :: dyn_init_src
      CHARACTER(LEN=8)          :: static_init_src 
      INTEGER                   :: vt_date
      REAL                      :: vt_time
      INTEGER                   :: origin_parent_x
      INTEGER                   :: origin_parent_y
      INTEGER                   :: ratio_to_parent
      REAL                      :: delta_x
      REAL                      :: delta_y
      REAL                      :: top_level
      INTEGER                   :: origin_parent_z
      REAL                      :: corner_lats_new(4,max_staggers_xy_new)
      REAL                      :: corner_lons_new(4,max_staggers_xy_new)
      REAL                      :: corner_lats_old(4,max_staggers_xy_old)
      REAL                      :: corner_lons_old(4,max_staggers_xy_old)
      INTEGER                   :: xdim
      INTEGER                   :: ydim
      INTEGER                   :: zdim
   END TYPE wrf_domain_metadata
   TYPE(wrf_domain_metadata) :: dom_meta

   TYPE wrf_global_metadata
      CHARACTER(LEN=80)         :: simulation_name
      CHARACTER(LEN=80)         :: user_desc
      INTEGER                   :: si_version
      INTEGER                   :: analysis_version  
      INTEGER                   :: wrf_version
      INTEGER                   :: post_version
      CHARACTER(LEN=32)         :: map_projection
      REAL                      :: moad_known_lat
      REAL                      :: moad_known_lon
      CHARACTER(LEN=8)          :: moad_known_loc
      REAL                      :: moad_stand_lats(max_standard_lats)
      REAL                      :: moad_stand_lons(max_standard_lons)
      REAL                      :: moad_delta_x
      REAL                      :: moad_delta_y
      CHARACTER(LEN=4)          :: horiz_stagger_type
      INTEGER                   :: num_stagger_xy
      REAL                      :: stagger_dir_x_new(max_staggers_xy_new)
      REAL                      :: stagger_dir_y_new(max_staggers_xy_new)
      REAL                      :: stagger_dir_x_old(max_staggers_xy_old)
      REAL                      :: stagger_dir_y_old(max_staggers_xy_old)
      INTEGER                   :: num_stagger_z    
      REAL                      :: stagger_dir_z(max_staggers_z)
      CHARACTER(LEN=8)          :: vertical_coord
      INTEGER                   :: num_domains
      INTEGER                   :: init_date
      REAL                      :: init_time
      INTEGER                   :: end_date
      REAL                      :: end_time
      CHARACTER(LEN=4)          :: lu_source
      INTEGER                   :: lu_water
      INTEGER                   :: lu_ice  
   END TYPE wrf_global_metadata
   TYPE(wrf_global_metadata)   :: global_meta

CONTAINS

   SUBROUTINE read_si ( grid , ix , jx , kx , dx , p_top , file_date_string )

      USE module_soil_pre
      USE module_domain

      IMPLICIT NONE

      TYPE(domain) , INTENT(INOUT)  :: grid
      INTEGER , INTENT(IN) :: ix , jx , kx
      REAL , INTENT(IN) :: dx
      REAL , INTENT(OUT) :: p_top
      CHARACTER (LEN=19) , INTENT(IN) :: file_date_string

      REAL , DIMENSION(jx,ix) :: dum2d
     
      INTEGER :: i , j , k , loop

      REAL :: dummy

      CHARACTER (LEN= 8) :: dummy_char

      INTEGER :: ok , map_proj , ok_open

      !  Initialize what soil temperature and moisture is available.

      flag_st000010 = .FALSE. 
      flag_st010040 = .FALSE.
      flag_st040100 = .FALSE.
      flag_st100200 = .FALSE.
      flag_sm000010 = .FALSE.
      flag_sm010040 = .FALSE.
      flag_sm040100 = .FALSE.
      flag_sm100200 = .FALSE.
      flag_st010200 = .FALSE.
      flag_sm010200 = .FALSE.

      flag_soilt010 = .FALSE. 
      flag_soilt040 = .FALSE. 
      flag_soilt100 = .FALSE. 
      flag_soilt200 = .FALSE. 
      flag_soilm010 = .FALSE. 
      flag_soilm040 = .FALSE. 
      flag_soilm100 = .FALSE. 
      flag_soilm200 = .FALSE. 

      flag_sst      = .FALSE.
      flag_toposoil = .FALSE.

      !  How many soil levels have we found?  Well, right now, none.

      num_st_levels_input = 0
      num_sm_levels_input = 0
      st_levels_input = -1
      sm_levels_input = -1

      !  Get the space for the data if this is the first time here.

      IF ( first_time_in ) THEN

         CLOSE(12)
         OPEN ( FILE   = 'real_input_em.global.metadata' , &
                UNIT   = 12                              , &
                STATUS = 'OLD'                           , &
                ACCESS = 'SEQUENTIAL'                    , &
                FORM   = 'UNFORMATTED'                   , &
                IOSTAT = ok_open                           )

         IF ( ok_open .NE. 0 ) THEN
            PRINT '(A)','You asked for WRF SI data, but no real_input_em.global.metadata file exists.'
            STOP 'No_real_input_em.global.metadata_exists'
         END IF

         READ(12) global_meta%simulation_name, global_meta%user_desc, &
                  global_meta%si_version, global_meta%analysis_version, &
                  global_meta%wrf_version, global_meta%post_version
   
         REWIND (12)

         IF      ( global_meta%si_version .EQ. 1 ) THEN
            READ(12) global_meta%simulation_name, global_meta%user_desc, &
                     global_meta%si_version, global_meta%analysis_version, &
                     global_meta%wrf_version, global_meta%post_version, &
                     global_meta%map_projection, global_meta%moad_known_lat, &
                     global_meta%moad_known_lon, global_meta%moad_known_loc, &
                     global_meta%moad_stand_lats, global_meta%moad_stand_lons, &
                     global_meta%moad_delta_x, global_meta%moad_delta_y, &
                     global_meta%horiz_stagger_type, global_meta%num_stagger_xy, &
                     global_meta%stagger_dir_x_old, global_meta%stagger_dir_y_old, &
                     global_meta%num_stagger_z, global_meta%stagger_dir_z, &
                     global_meta%vertical_coord, global_meta%num_domains, &
                     global_meta%init_date, global_meta%init_time, &
                     global_meta%end_date, global_meta%end_time
         ELSE IF ( global_meta%si_version .EQ. 2 ) THEN
            READ(12) global_meta%simulation_name, global_meta%user_desc, &
                     global_meta%si_version, global_meta%analysis_version, &
                     global_meta%wrf_version, global_meta%post_version, &
                     global_meta%map_projection, global_meta%moad_known_lat, &
                     global_meta%moad_known_lon, global_meta%moad_known_loc, &
                     global_meta%moad_stand_lats, global_meta%moad_stand_lons, &
                     global_meta%moad_delta_x, global_meta%moad_delta_y, &
                     global_meta%horiz_stagger_type, global_meta%num_stagger_xy, &
                     global_meta%stagger_dir_x_new, global_meta%stagger_dir_y_new, &
                     global_meta%num_stagger_z, global_meta%stagger_dir_z, &
                     global_meta%vertical_coord, global_meta%num_domains, &
                     global_meta%init_date, global_meta%init_time, &
                     global_meta%end_date, global_meta%end_time , &
                     global_meta%lu_source, global_meta%lu_water, global_meta%lu_ice
         END IF
         CLOSE (12)
   
         print *,'GLOBAL METADATA'
         print *,'global_meta%simulation_name', global_meta%simulation_name
         print *,'global_meta%user_desc', global_meta%user_desc
         print *,'global_meta%user_desc', global_meta%user_desc
         print *,'global_meta%si_version', global_meta%si_version
         print *,'global_meta%analysis_version', global_meta%analysis_version
         print *,'global_meta%wrf_version', global_meta%wrf_version
         print *,'global_meta%post_version', global_meta%post_version
         print *,'global_meta%map_projection', global_meta%map_projection
         print *,'global_meta%moad_known_lat', global_meta%moad_known_lat
         print *,'global_meta%moad_known_lon', global_meta%moad_known_lon
         print *,'global_meta%moad_known_loc', global_meta%moad_known_loc
         print *,'global_meta%moad_stand_lats', global_meta%moad_stand_lats
         print *,'global_meta%moad_stand_lons', global_meta%moad_stand_lons
         print *,'global_meta%moad_delta_x', global_meta%moad_delta_x
         print *,'global_meta%moad_delta_y', global_meta%moad_delta_y
         print *,'global_meta%horiz_stagger_type', global_meta%horiz_stagger_type
         print *,'global_meta%num_stagger_xy', global_meta%num_stagger_xy
         IF      ( global_meta%si_version .EQ. 1 ) THEN
            print *,'global_meta%stagger_dir_x', global_meta%stagger_dir_x_old
            print *,'global_meta%stagger_dir_y', global_meta%stagger_dir_y_old
         ELSE IF ( global_meta%si_version .EQ. 2 ) THEN
            print *,'global_meta%stagger_dir_x', global_meta%stagger_dir_x_new
            print *,'global_meta%stagger_dir_y', global_meta%stagger_dir_y_new
         END IF
         print *,'global_meta%num_stagger_z', global_meta%num_stagger_z
         print *,'global_meta%stagger_dir_z', global_meta%stagger_dir_z
         print *,'global_meta%vertical_coord', global_meta%vertical_coord
         print *,'global_meta%num_domains', global_meta%num_domains
         print *,'global_meta%init_date', global_meta%init_date
         print *,'global_meta%init_time', global_meta%init_time
         print *,'global_meta%end_date', global_meta%end_date
         print *,'global_meta%end_time', global_meta%end_time
         IF ( global_meta%si_version .EQ. 2 ) THEN
            print *,'global_meta%lu_source', global_meta%lu_source
            print *,'global_meta%lu_water', global_meta%lu_water
            print *,'global_meta%lu_ice', global_meta%lu_ice
         END IF
         print *,' '
         
         !  1D - this is the definition of the vertical coordinate.

         IF ( .NOT. ALLOCATED ( etafull)            ) ALLOCATE ( etafull(kx) )
         IF ( .NOT. ALLOCATED ( etahalf)            ) ALLOCATE ( etahalf(kx) )

         !  3D met

         IF ( .NOT. ALLOCATED ( u_input)            ) ALLOCATE ( u_input(ix,jx,kx) )
         IF ( .NOT. ALLOCATED ( v_input)            ) ALLOCATE ( v_input(ix,jx,kx) )
         IF ( .NOT. ALLOCATED ( q_input)            ) ALLOCATE ( q_input(ix,jx,kx) )
         IF ( .NOT. ALLOCATED ( theta_input)        ) ALLOCATE ( theta_input(ix,jx,kx) )

         !  2D dry pressure - ptop, this is for the vertical coordinate

         IF ( .NOT. ALLOCATED ( mu_input)           ) ALLOCATE ( mu_input(ix,jx) )

         !  2D - skin temperature and some sort of soil temp, usually 0-10 cm

         IF ( .NOT. ALLOCATED ( tg_input)           ) ALLOCATE ( tg_input(ix,jx) )
         IF ( .NOT. ALLOCATED ( res_input)          ) ALLOCATE ( res_input(ix,jx) )

         !  2D - for LSM, these are computed from the categorical precentage values.

         IF ( .NOT. ALLOCATED ( isltyp_input)       ) ALLOCATE ( isltyp_input(ix,jx) )
         IF ( .NOT. ALLOCATED ( ivgtyp_input)       ) ALLOCATE ( ivgtyp_input(ix,jx) )

         !  2D - for LSM, this is a new field from the SI

         IF ( .NOT. ALLOCATED ( t_annual_avg_input) ) ALLOCATE ( t_annual_avg_input(ix,jx) )

         !  2D - for LSM, the various soil temperature and moisture levels that are available.

         IF ( .NOT. ALLOCATED ( st000010_input)     ) ALLOCATE ( st000010_input(ix,jx) )
         IF ( .NOT. ALLOCATED ( st010040_input)     ) ALLOCATE ( st010040_input(ix,jx) )
         IF ( .NOT. ALLOCATED ( st040100_input)     ) ALLOCATE ( st040100_input(ix,jx) )
         IF ( .NOT. ALLOCATED ( st100200_input)     ) ALLOCATE ( st100200_input(ix,jx) )
         IF ( .NOT. ALLOCATED ( st010200_input)     ) ALLOCATE ( st010200_input(ix,jx) )
         IF ( .NOT. ALLOCATED ( st_input      )     ) ALLOCATE ( st_input      (ix,jx,10) )
         IF ( .NOT. ALLOCATED ( sm000010_input)     ) ALLOCATE ( sm000010_input(ix,jx) )
         IF ( .NOT. ALLOCATED ( sm010040_input)     ) ALLOCATE ( sm010040_input(ix,jx) )
         IF ( .NOT. ALLOCATED ( sm040100_input)     ) ALLOCATE ( sm040100_input(ix,jx) )
         IF ( .NOT. ALLOCATED ( sm100200_input)     ) ALLOCATE ( sm100200_input(ix,jx) )
         IF ( .NOT. ALLOCATED ( sm010200_input)     ) ALLOCATE ( sm010200_input(ix,jx) )
         IF ( .NOT. ALLOCATED ( sm_input      )     ) ALLOCATE ( sm_input      (ix,jx,10) )

         IF ( .NOT. ALLOCATED ( soilt010_input)     ) ALLOCATE ( soilt010_input(ix,jx) )
         IF ( .NOT. ALLOCATED ( soilt040_input)     ) ALLOCATE ( soilt040_input(ix,jx) )
         IF ( .NOT. ALLOCATED ( soilt100_input)     ) ALLOCATE ( soilt100_input(ix,jx) )
         IF ( .NOT. ALLOCATED ( soilt200_input)     ) ALLOCATE ( soilt200_input(ix,jx) )
         IF ( .NOT. ALLOCATED ( soilm010_input)     ) ALLOCATE ( soilm010_input(ix,jx) )
         IF ( .NOT. ALLOCATED ( soilm040_input)     ) ALLOCATE ( soilm040_input(ix,jx) )
         IF ( .NOT. ALLOCATED ( soilm100_input)     ) ALLOCATE ( soilm100_input(ix,jx) )
         IF ( .NOT. ALLOCATED ( soilm200_input)     ) ALLOCATE ( soilm200_input(ix,jx) )

         ! 2D - for LSM, seaice and snow, maybe runoff is later.

         IF ( .NOT. ALLOCATED ( seaice_input)       ) ALLOCATE ( seaice_input(ix,jx) )
         IF ( .NOT. ALLOCATED ( snow_input)         ) ALLOCATE ( snow_input(ix,jx) )
         IF ( .NOT. ALLOCATED ( canwat_input)       ) ALLOCATE ( canwat_input(ix,jx) )

         IF ( .NOT. ALLOCATED ( landuse_input)      ) ALLOCATE ( landuse_input(ix,jx) )
         IF ( .NOT. ALLOCATED ( landmask_input)     ) ALLOCATE ( landmask_input(ix,jx) )

         IF ( .NOT. ALLOCATED ( skintemp_input)     ) ALLOCATE ( skintemp_input(ix,jx)  )
         IF ( .NOT. ALLOCATED ( sst_input)          ) ALLOCATE ( sst_input(ix,jx)  )
         IF ( .NOT. ALLOCATED ( green_frac_input)   ) ALLOCATE ( green_frac_input(ix,jx)  )

         IF ( .NOT. ALLOCATED ( ter_input)          ) ALLOCATE ( ter_input(ix,jx) )
         IF ( .NOT. ALLOCATED ( ter_std_input)      ) ALLOCATE ( ter_std_input(ix,jx) )
         IF ( .NOT. ALLOCATED ( ter_slpx_input)     ) ALLOCATE ( ter_slpx_input(ix,jx) )
         IF ( .NOT. ALLOCATED ( ter_slpy_input)     ) ALLOCATE ( ter_slpy_input(ix,jx) )
         IF ( .NOT. ALLOCATED ( toposoil_input)     ) ALLOCATE ( toposoil_input(ix,jx) )

         !  2D terrestrial fields other than elevation stuff.

         IF ( .NOT. ALLOCATED ( msft_input)         ) ALLOCATE ( msft_input(ix,jx) )
         IF ( .NOT. ALLOCATED ( msfu_input)         ) ALLOCATE ( msfu_input(ix,jx) )
         IF ( .NOT. ALLOCATED ( msfv_input)         ) ALLOCATE ( msfv_input(ix,jx) )
         IF ( .NOT. ALLOCATED ( sina_input)         ) ALLOCATE ( sina_input(ix,jx) )
         IF ( .NOT. ALLOCATED ( cosa_input)         ) ALLOCATE ( cosa_input(ix,jx) )
         IF ( .NOT. ALLOCATED ( e_input)            ) ALLOCATE ( e_input(ix,jx) )
         IF ( .NOT. ALLOCATED ( f_input)            ) ALLOCATE ( f_input(ix,jx) )
         IF ( .NOT. ALLOCATED ( lat_input)          ) ALLOCATE ( lat_input(ix,jx) )
         IF ( .NOT. ALLOCATED ( lon_input)          ) ALLOCATE ( lon_input(ix,jx) )
      END IF

      CLOSE(13)
      OPEN ( FILE   = 'real_input_em.d01.'//file_date_string , &
             UNIT   = 13                                     , &
             STATUS = 'OLD'                                  , &
             ACCESS = 'SEQUENTIAL'                           , &
             FORM   = 'UNFORMATTED'                            )

      IF      ( global_meta%si_version .EQ. 1 ) THEN
         READ (13) dom_meta%id,dom_meta%parent_id,dom_meta%dyn_init_src,&
                   dom_meta%static_init_src, dom_meta%vt_date, dom_meta%vt_time, &
                   dom_meta%origin_parent_x, dom_meta%origin_parent_y, &
                   dom_meta%ratio_to_parent, dom_meta%delta_x, dom_meta%delta_y, &
                   dom_meta%top_level, dom_meta%origin_parent_z, &
                   dom_meta%corner_lats_old, dom_meta%corner_lons_old, dom_meta%xdim, &
                   dom_meta%ydim, dom_meta%zdim
      ELSE IF ( global_meta%si_version .EQ. 2 ) THEN
         READ (13) dom_meta%id,dom_meta%parent_id,dom_meta%dyn_init_src,&
                   dom_meta%static_init_src, dom_meta%vt_date, dom_meta%vt_time, &
                   dom_meta%origin_parent_x, dom_meta%origin_parent_y, &
                   dom_meta%ratio_to_parent, dom_meta%delta_x, dom_meta%delta_y, &
                   dom_meta%top_level, dom_meta%origin_parent_z, &
                   dom_meta%corner_lats_new, dom_meta%corner_lons_new, dom_meta%xdim, &
                   dom_meta%ydim, dom_meta%zdim
      END IF

      print *,'DOMAIN METADATA'
      print *,'dom_meta%id=', dom_meta%id
      print *,'dom_meta%parent_id=', dom_meta%parent_id
      print *,'dom_meta%dyn_init_src=', dom_meta%dyn_init_src
      print *,'dom_meta%static_init_src=', dom_meta%static_init_src
      print *,'dom_meta%vt_date=', dom_meta%vt_date
      print *,'dom_meta%vt_time=', dom_meta%vt_time
      print *,'dom_meta%origin_parent_x=', dom_meta%origin_parent_x
      print *,'dom_meta%origin_parent_y=', dom_meta%origin_parent_y
      print *,'dom_meta%ratio_to_parent=', dom_meta%ratio_to_parent
      print *,'dom_meta%delta_x=', dom_meta%delta_x
      print *,'dom_meta%delta_y=', dom_meta%delta_y
      print *,'dom_meta%top_level=', dom_meta%top_level
      print *,'dom_meta%origin_parent_z=', dom_meta%origin_parent_z
      IF      ( global_meta%si_version .EQ. 1 ) THEN
         print *,'dom_meta%corner_lats=', dom_meta%corner_lats_old
         print *,'dom_meta%corner_lons=', dom_meta%corner_lons_old
      ELSE IF ( global_meta%si_version .EQ. 2 ) THEN
         print *,'dom_meta%corner_lats=', dom_meta%corner_lats_new
         print *,'dom_meta%corner_lons=', dom_meta%corner_lons_new
      END IF
      print *,'dom_meta%xdim=', dom_meta%xdim
      print *,'dom_meta%ydim=', dom_meta%ydim
      print *,'dom_meta%zdim=', dom_meta%zdim
      print *,' '

      !  A simple domain size test.
    
      IF ( ( dom_meta%xdim .NE. ix ) .OR. ( dom_meta%ydim .NE. jx ) .OR. ( dom_meta%zdim .NE. kx ) ) THEN
         PRINT '(A)','Namelist does not match the input data.'
         PRINT '(A,3I5,A)','Namelist dimensions =',ix,jx,kx,'.'
         PRINT '(A,3I5,A)','Input data dimensions =',dom_meta%xdim,dom_meta%ydim,dom_meta%zdim,'.'
         STOP 'Wrong_data_size'
      END IF

      ! How about the grid distance?  Is it the same as in the namelist?

      IF ( ABS ( dom_meta%delta_x - dx ) .GT. 1 ) THEN
         PRINT '(A)','Grid distance differs between input file and namelist.'
         PRINT '(A,F8.1,A)','Grid distance in input file = ',dom_meta%delta_x,' m.'
         PRINT '(A,F8.1,A)','Grid distance in namelist = ',dx,' m.'
         STOP 'Wrong_grid_distance'
      END IF

      IF        ( global_meta%si_version .EQ. 1 ) THEN
         CALL set_cen_lat ( grid%id , ( dom_meta%corner_lats_old(1,1) + dom_meta%corner_lats_old(2,1) +        &
                                        dom_meta%corner_lats_old(3,1) + dom_meta%corner_lats_old(4,1) ) * 0.25 ) 
      ELSE IF ( ( global_meta%si_version .EQ. 2 ) .AND. ( global_meta%moad_known_loc(1:6) .EQ. 'CENTER' ) ) THEN
         CALL set_cen_lat ( grid%id , global_meta%moad_known_lat )
      ELSE IF   ( global_meta%si_version .EQ. 2 ) THEN
         CALL set_cen_lat ( grid%id , ( dom_meta%corner_lats_new(1,1) + dom_meta%corner_lats_new(2,1) +        &
                                        dom_meta%corner_lats_new(3,1) + dom_meta%corner_lats_new(4,1) ) * 0.25 ) 
      END IF

      CALL set_cen_lon ( grid%id , global_meta%moad_stand_lons(1) )
      CALL set_truelat1 ( grid%id , global_meta%moad_stand_lats(1) )
      CALL set_truelat2 ( grid%id , global_meta%moad_stand_lats(2) )

      p_top = dom_meta%top_level

      IF      ( global_meta%map_projection(1:17) .EQ. 'LAMBERT CONFORMAL'   ) THEN
         map_proj = 1
      ELSE IF ( global_meta%map_projection(1:19) .EQ. 'POLAR STEREOGRAPHIC' ) THEN
         map_proj = 2
      ELSE IF ( global_meta%map_projection(1: 8) .EQ. 'MERCATOR'            ) THEN
         map_proj = 3
      ELSE
         PRINT '(A,A,A)','Undefined map projection: ',TRIM(global_meta%map_projection(1:20)),'.'
         STOP 'Undefined_map_proj_si'
      END IF
      CALL set_map_proj ( grid%id , map_proj ) 
     
      IF      ( global_meta%si_version .EQ. 1 ) THEN
         CALL set_mminlu ( 'USGS' )
         CALL set_iswater (grid%id, 16 )
      ELSE IF ( global_meta%si_version .EQ. 2 ) THEN
         CALL set_mminlu ( global_meta%lu_source )
         CALL set_iswater (grid%id, global_meta%lu_water )
         CALL set_isice (grid%id, global_meta%lu_ice )
      END IF

      CALL set_gmt (grid%id, dom_meta%vt_time / 3600. )
      CALL set_julyr (grid%id, dom_meta%vt_date / 1000 )
      CALL set_julday (grid%id, dom_meta%vt_date - ( dom_meta%vt_date / 1000 ) * 1000 )

      read_all_the_data : DO

         READ (13,IOSTAT=OK) var_info%name, var_info%units, &
                             var_info%description, var_info%domain_id, var_info%ndim, &
                             var_info%dim_val, var_info%dim_desc, var_info%start_index, &
                             var_info%stop_index, var_info%h_stagger_index, var_info%v_stagger_index,&
                             var_info%array_order, var_info%field_type, var_info%field_source_prog, &
                             var_info%source_desc, var_info%field_time_type, var_info%vt_date_start, &
                             var_info%vt_time_start, var_info%vt_date_stop, var_info%vt_time_stop

         IF ( OK .NE. 0 ) THEN
            PRINT '(A,A,A)','End of file found for real_input_em.d01.',file_date_string,'.'
            EXIT read_all_the_data
         END IF

!        print *,'VARIABLE METADATA'
         PRINT '(A,A)','var_info%name=', var_info%name 
!        print *,'var_info%units=', var_info%units 
!        print *,'var_info%description=', var_info%description 
!        print *,'var_info%domain_id=', var_info%domain_id 
!        print *,'var_info%ndim=', var_info%ndim 
!        print *,'var_info%dim_val=', var_info%dim_val 
!        print *,'var_info%dim_desc=', var_info%dim_desc 
!        print *,'var_info%start_index=', var_info%start_index 
!        print *,'var_info%stop_index=', var_info%stop_index 
!        print *,'var_info%h_stagger_index=', var_info%h_stagger_index 
!        print *,'var_info%v_stagger_index=', var_info%v_stagger_index
!        print *,'var_info%array_order=', var_info%array_order 
!        print *,'var_info%field_type=', var_info%field_type 
!        print *,'var_info%field_source_prog=', var_info%field_source_prog 
!        print *,'var_info%source_desc=', var_info%source_desc 
!        print *,'var_info%field_time_type=', var_info%field_time_type 
!        print *,'var_info%vt_date_start=', var_info%vt_date_start 
!        print *,'var_info%vt_time_start=', var_info%vt_time_start 
!        print *,'var_info%vt_date_stop=', var_info%vt_date_stop 
!        print *,'var_info%vt_time_stop=', var_info%vt_time_stop

         !  3D meteorological fields.

         IF      ( var_info%name(1:8) .EQ. 'U       ' ) THEN
            READ (13) u_input
         ELSE IF ( var_info%name(1:8) .EQ. 'V       ' ) THEN
            READ (13) v_input
         ELSE IF ( var_info%name(1:8) .EQ. 'QVAPOR  ' ) THEN
            READ (13) q_input
         ELSE IF ( var_info%name(1:8) .EQ. 'THETA   ' ) THEN
            READ (13) theta_input

         !  3D LSM fields.  Don't know the 3rd dimension until we read it in.

         ELSE IF ( var_info%name(1:8) .EQ. 'LANDUSEF' ) THEN
            IF ( ( first_time_in ) .AND. ( .NOT. ALLOCATED ( landuse_frac_input) ) ) THEN
               ALLOCATE (landuse_frac_input(ix,jx,var_info%dim_val(3)) )
            END IF
            READ (13) landuse_frac_input
         ELSE IF ( var_info%name(1:8) .EQ. 'SOILCTOP' ) THEN
            IF ( ( first_time_in ) .AND. ( .NOT. ALLOCATED ( soil_top_cat_input) ) ) THEN
               ALLOCATE (soil_top_cat_input(ix,jx,var_info%dim_val(3)) )
            END IF
            READ (13) soil_top_cat_input
         ELSE IF ( var_info%name(1:8) .EQ. 'SOILCBOT' ) THEN
            IF ( ( first_time_in ) .AND. ( .NOT. ALLOCATED ( soil_bot_cat_input) ) ) THEN
               ALLOCATE (soil_bot_cat_input(ix,jx,var_info%dim_val(3)) )
            END IF
            READ (13) soil_bot_cat_input

         !  2D dry pressure minus ptop.

         ELSE IF ( var_info%name(1:8) .EQ. 'MU_M    ' ) THEN
            READ (13) mu_input

         !  2D surface fields.

         ELSE IF ( var_info%name(1:8) .EQ. 'T_AVGANN' ) THEN
            READ (13) t_annual_avg_input

         ELSE IF ( var_info%name(1:8) .EQ. 'ST000010' ) THEN
            READ (13) st000010_input
            res_input = st000010_input
            flag_st000010 = .TRUE.
            num_st_levels_input = num_st_levels_input + 1
            st_levels_input(num_st_levels_input) = char2int2(var_info%name(3:8))
            st_input(:,:,num_st_levels_input + 1) = st000010_input
         ELSE IF ( var_info%name(1:8) .EQ. 'ST010040' ) THEN
            READ (13) st010040_input
            flag_st010040 = .TRUE.
            num_st_levels_input = num_st_levels_input + 1
            st_levels_input(num_st_levels_input) = char2int2(var_info%name(3:8))
            st_input(:,:,num_st_levels_input + 1) = st010040_input
         ELSE IF ( var_info%name(1:8) .EQ. 'ST040100' ) THEN
            READ (13) st040100_input
            flag_st040100 = .TRUE.
            num_st_levels_input = num_st_levels_input + 1
            st_levels_input(num_st_levels_input) = char2int2(var_info%name(3:8))
            st_input(:,:,num_st_levels_input + 1) = st040100_input
         ELSE IF ( var_info%name(1:8) .EQ. 'ST100200' ) THEN
            READ (13) st100200_input
            flag_st100200 = .TRUE.
            num_st_levels_input = num_st_levels_input + 1
            st_levels_input(num_st_levels_input) = char2int2(var_info%name(3:8))
            st_input(:,:,num_st_levels_input + 1) = st100200_input
         ELSE IF ( var_info%name(1:8) .EQ. 'ST010200' ) THEN
            READ (13) st010200_input
            flag_st010200 = .TRUE.
            num_st_levels_input = num_st_levels_input + 1
            st_levels_input(num_st_levels_input) = char2int2(var_info%name(3:8))
            st_input(:,:,num_st_levels_input + 1) = st010200_input
         ELSE IF ( var_info%name(1:8) .EQ. 'SM000010' ) THEN
            READ (13) sm000010_input
            flag_sm000010 = .TRUE.
            num_sm_levels_input = num_sm_levels_input + 1
            sm_levels_input(num_sm_levels_input) = char2int2(var_info%name(3:8))
            sm_input(:,:,num_sm_levels_input + 1) = sm000010_input
         ELSE IF ( var_info%name(1:8) .EQ. 'SM010040' ) THEN
            READ (13) sm010040_input
            flag_sm010040 = .TRUE.
            num_sm_levels_input = num_sm_levels_input + 1
            sm_levels_input(num_sm_levels_input) = char2int2(var_info%name(3:8))
            sm_input(:,:,num_sm_levels_input + 1) = sm010040_input
         ELSE IF ( var_info%name(1:8) .EQ. 'SM040100' ) THEN
            READ (13) sm040100_input
            flag_sm040100 = .TRUE.
            num_sm_levels_input = num_sm_levels_input + 1
            sm_levels_input(num_sm_levels_input) = char2int2(var_info%name(3:8))
            sm_input(:,:,num_sm_levels_input + 1) = sm040100_input
         ELSE IF ( var_info%name(1:8) .EQ. 'SM100200' ) THEN
            READ (13) sm100200_input
            flag_sm100200 = .TRUE.
            num_sm_levels_input = num_sm_levels_input + 1
            sm_levels_input(num_sm_levels_input) = char2int2(var_info%name(3:8))
            sm_input(:,:,num_sm_levels_input + 1) = sm100200_input
         ELSE IF ( var_info%name(1:8) .EQ. 'SM010200' ) THEN
            READ (13) sm010200_input
            flag_sm010200 = .TRUE.
            num_sm_levels_input = num_sm_levels_input + 1
            sm_levels_input(num_sm_levels_input) = char2int2(var_info%name(3:8))
            sm_input(:,:,num_sm_levels_input + 1) = sm010200_input

         ELSE IF ( var_info%name(1:8) .EQ. 'SOILT010' ) THEN
            READ (13) soilt010_input
            res_input = soilt010_input
            flag_soilt010 = .TRUE.
            num_st_levels_input = num_st_levels_input + 1
            st_levels_input(num_st_levels_input) = char2int1(var_info%name(6:8))
            st_input(:,:,num_st_levels_input + 1) = soilt010_input
         ELSE IF ( var_info%name(1:8) .EQ. 'SOILT040' ) THEN
            READ (13) soilt040_input
            flag_soilt040 = .TRUE.
            num_st_levels_input = num_st_levels_input + 1
            st_levels_input(num_st_levels_input) = char2int1(var_info%name(6:8))
            st_input(:,:,num_st_levels_input + 1) = soilt040_input
         ELSE IF ( var_info%name(1:8) .EQ. 'SOILT100' ) THEN
            READ (13) soilt100_input
            flag_soilt100 = .TRUE.
            num_st_levels_input = num_st_levels_input + 1
            st_levels_input(num_st_levels_input) = char2int1(var_info%name(6:8))
            st_input(:,:,num_st_levels_input + 1) = soilt100_input
         ELSE IF ( var_info%name(1:8) .EQ. 'SOILT200' ) THEN
            READ (13) soilt200_input
            flag_soilt200 = .TRUE.
            num_st_levels_input = num_st_levels_input + 1
            st_levels_input(num_st_levels_input) = char2int1(var_info%name(6:8))
            st_input(:,:,num_st_levels_input + 1) = soilt200_input
         ELSE IF ( var_info%name(1:8) .EQ. 'SOILM010' ) THEN
            READ (13) soilm010_input
            flag_soilm010 = .TRUE.
            num_sm_levels_input = num_sm_levels_input + 1
            sm_levels_input(num_sm_levels_input) = char2int1(var_info%name(6:8))
            sm_input(:,:,num_sm_levels_input + 1) = soilm010_input
         ELSE IF ( var_info%name(1:8) .EQ. 'SOILM040' ) THEN
            READ (13) soilm040_input
            flag_soilm040 = .TRUE.
            num_sm_levels_input = num_sm_levels_input + 1
            sm_levels_input(num_sm_levels_input) = char2int1(var_info%name(6:8))
            sm_input(:,:,num_sm_levels_input + 1) = soilm040_input
         ELSE IF ( var_info%name(1:8) .EQ. 'SOILM100' ) THEN
            READ (13) soilm100_input
            flag_soilm100 = .TRUE.
            num_sm_levels_input = num_sm_levels_input + 1
            sm_levels_input(num_sm_levels_input) = char2int1(var_info%name(6:8))
            sm_input(:,:,num_sm_levels_input + 1) = soilm100_input
         ELSE IF ( var_info%name(1:8) .EQ. 'SOILM200' ) THEN
            READ (13) soilm200_input
            flag_soilm200 = .TRUE.
            num_sm_levels_input = num_sm_levels_input + 1
            sm_levels_input(num_sm_levels_input) = char2int1(var_info%name(6:8))
            sm_input(:,:,num_sm_levels_input + 1) = soilm200_input

         ELSE IF ( var_info%name(1:8) .EQ. 'SEAICE  ' ) THEN
            READ (13) seaice_input
         ELSE IF ( var_info%name(1:8) .EQ. 'WEASD   ' ) THEN
            READ (13) snow_input

	do J=1,JX
	do I=1,IX
	if (mod(J,20) .eq. 0 .and. mod(I,20) .eq. 0) then
	write(6,*) 'read snow_input: ', i,j,snow_input(i,j)
	endif
	enddo
	enddo

         ELSE IF ( var_info%name(1:8) .EQ. 'CANWAT  ' ) THEN
            READ (13) canwat_input
         ELSE IF ( var_info%name(1:8) .EQ. 'LANDMASK' ) THEN
            READ (13) landmask_input
         ELSE IF ( var_info%name(1:8) .EQ. 'SKINTEMP' ) THEN
            READ (13) skintemp_input
            tg_input = skintemp_input
         ELSE IF ( var_info%name(1:8) .EQ. 'SST     ' ) THEN
            READ (13) sst_input
            flag_sst = .TRUE.
         ELSE IF ( var_info%name(1:8) .EQ. 'GREENFRC' ) THEN
            READ (13) green_frac_input
         ELSE IF(( var_info%name(1:8) .EQ. 'USGSLAND' ) .AND. ( global_meta%si_version .EQ. 1 ) ) THEN
            READ (13) landuse_input
         ELSE IF(( var_info%name(1:8) .EQ. 'TERRAIN ' ) .AND. ( global_meta%si_version .EQ. 1 ) ) THEN 
            READ (13) ter_input
         ELSE IF(( var_info%name(1:8) .EQ. 'TOPO_M  ' ) .AND. ( global_meta%si_version .EQ. 2 ) ) THEN
            READ (13) ter_input
         ELSE IF ( var_info%name(1:8) .EQ. 'TOPOSTDV' ) THEN
            READ (13) ter_std_input
         ELSE IF ( var_info%name(1:8) .EQ. 'TOPOSLPX' ) THEN
            READ (13) ter_slpx_input
         ELSE IF ( var_info%name(1:8) .EQ. 'TOPOSLPY' ) THEN
            READ (13) ter_slpy_input
         ELSE IF ( var_info%name(1:8) .EQ. 'SOILGHT ' ) THEN
            READ (13) toposoil_input
            flag_toposoil = .TRUE.

         !  2D map input fields.

         ELSE IF ( var_info%name(1:8) .EQ. 'MAPFAC_M' ) THEN
            READ (13) msft_input
         ELSE IF ( var_info%name(1:8) .EQ. 'MAPFAC_U' ) THEN
            READ (13) msfu_input
         ELSE IF ( var_info%name(1:8) .EQ. 'MAPFAC_V' ) THEN
            READ (13) msfv_input
         ELSE IF ( var_info%name(1:8) .EQ. 'SINALPHA' ) THEN
            READ (13) sina_input
         ELSE IF ( var_info%name(1:8) .EQ. 'COSALPHA' ) THEN
            READ (13) cosa_input
         ELSE IF ( var_info%name(1:8) .EQ. 'H_CORIOL' ) THEN
            READ (13) f_input
         ELSE IF ( var_info%name(1:8) .EQ. 'V_CORIOL' ) THEN
            READ (13) e_input
         ELSE IF(( var_info%name(1:8) .EQ. 'LATITUDE' ) .AND. ( global_meta%si_version .EQ. 1 ) ) THEN
            READ (13) lat_input
         ELSE IF(( var_info%name(1:8) .EQ. 'LONGITUD' ) .AND. ( global_meta%si_version .EQ. 1 ) ) THEN
            READ (13) lon_input
         ELSE IF(( var_info%name(1:8) .EQ. 'LAT_M   ' ) .AND. ( global_meta%si_version .EQ. 2 ) ) THEN
            READ (13) lat_input
         ELSE IF(( var_info%name(1:8) .EQ. 'LON_M   ' ) .AND. ( global_meta%si_version .EQ. 2 ) ) THEN
            READ (13) lon_input

         !  1D vertical coordinate.

         ELSE IF ( var_info%name(1:8) .EQ. 'ETAPHALF' ) THEN
            READ (13) etahalf
         ELSE IF ( var_info%name(1:8) .EQ. 'ETAPFULL' ) THEN
            READ (13) etafull

         !  wrong input data.

         ELSE IF ( var_info%name(1:8) .EQ. 'ZETAFULL' ) THEN
            PRINT '(A)','Oops, you put in the height data.'
            STOP 'this_is_mass_not_height'
 

         !  Stuff that we do not want or need is just skipped over.

         ELSE
print *,'------------------> skipping ', var_info%name(1:8)
            READ (13) dummy
         END IF

      END DO read_all_the_data

      CLOSE (13)

      first_time_in = .FALSE.

   END SUBROUTINE read_si

   SUBROUTINE read_mm5 ( grid , ix , jx , kx , dx , p_top , file_date_string )

      USE module_domain

      IMPLICIT NONE

      TYPE(domain) , INTENT(INOUT)  :: grid
      INTEGER , INTENT(IN) :: ix , jx , kx
      REAL , INTENT(IN) :: dx 
      CHARACTER (LEN=19) , INTENT(IN) :: file_date_string
      REAL , INTENT(OUT) ::  p_top

      REAL , DIMENSION(jx,ix) :: dum2d
     
      INTEGER :: i , j , k , loop

      REAL :: dummy

      CHARACTER (LEN= 8) :: dummy_char

      INTEGER :: ok , ix_input,jx_input,kx_input,iswater,julyr,julday,map_proj
      CHARACTER (LEN=4) :: mminlu
      REAL :: gmt , cen_lat , dx_input , truelat1 , truelat2 , cen_lon , p_top_input
 
      LOGICAL :: wrfsi

      !  Get the space for the data if this is the first time here.

      IF ( first_time_in ) THEN

         INQUIRE ( FILE  = 'real_input_em.global.metadata' , &
                   EXIST = wrfsi                          )

         IF ( wrfsi ) THEN
            PRINT '(A)', 'You have asked to use converted MM5, but the input looks like WRF SI.'
            STOP
         END IF
         
         IF ( .NOT. ALLOCATED ( etafull) ) ALLOCATE ( etafull(kx) )
         IF ( .NOT. ALLOCATED ( etahalf) ) ALLOCATE ( etahalf(kx) )

         IF ( .NOT. ALLOCATED ( u_input) ) ALLOCATE ( u_input(ix,jx,kx) )
         IF ( .NOT. ALLOCATED ( v_input) ) ALLOCATE ( v_input(ix,jx,kx) )
         IF ( .NOT. ALLOCATED ( q_input) ) ALLOCATE ( q_input(ix,jx,kx) )
         IF ( .NOT. ALLOCATED ( theta_input) ) ALLOCATE ( theta_input(ix,jx,kx) )

         IF ( .NOT. ALLOCATED ( tg_input) ) ALLOCATE ( tg_input(ix,jx) )
         IF ( .NOT. ALLOCATED ( res_input) ) ALLOCATE ( res_input(ix,jx) )
         IF ( .NOT. ALLOCATED ( st000010_input) ) ALLOCATE ( st000010_input(ix,jx) )
         IF ( .NOT. ALLOCATED ( st010040_input) ) ALLOCATE ( st010040_input(ix,jx) )
         IF ( .NOT. ALLOCATED ( st040100_input) ) ALLOCATE ( st040100_input(ix,jx) )
         IF ( .NOT. ALLOCATED ( st100200_input) ) ALLOCATE ( st100200_input(ix,jx) )
         IF ( .NOT. ALLOCATED ( st010200_input) ) ALLOCATE ( st010200_input(ix,jx) )
         IF ( .NOT. ALLOCATED ( sm000010_input) ) ALLOCATE ( sm000010_input(ix,jx) )
         IF ( .NOT. ALLOCATED ( sm010040_input) ) ALLOCATE ( sm010040_input(ix,jx) )
         IF ( .NOT. ALLOCATED ( sm040100_input) ) ALLOCATE ( sm040100_input(ix,jx) )
         IF ( .NOT. ALLOCATED ( sm100200_input) ) ALLOCATE ( sm100200_input(ix,jx) )
         IF ( .NOT. ALLOCATED ( sm010200_input) ) ALLOCATE ( sm010200_input(ix,jx) )
         IF ( .NOT. ALLOCATED ( seaice_input) ) ALLOCATE ( seaice_input(ix,jx) )
         IF ( .NOT. ALLOCATED ( ter_input) ) ALLOCATE ( ter_input(ix,jx) )
         IF ( .NOT. ALLOCATED ( landuse_input) ) ALLOCATE ( landuse_input(ix,jx) )
         IF ( .NOT. ALLOCATED ( landmask_input) ) ALLOCATE ( landmask_input(ix,jx) )
         IF ( .NOT. ALLOCATED ( snow_input) ) ALLOCATE ( snow_input(ix,jx) )
         IF ( .NOT. ALLOCATED ( lat_input) ) ALLOCATE ( lat_input(ix,jx) )
         IF ( .NOT. ALLOCATED ( lon_input) ) ALLOCATE ( lon_input(ix,jx) )

         IF ( .NOT. ALLOCATED ( msft_input) ) ALLOCATE ( msft_input(ix,jx) )
         IF ( .NOT. ALLOCATED ( msfu_input) ) ALLOCATE ( msfu_input(ix,jx) )
         IF ( .NOT. ALLOCATED ( msfv_input) ) ALLOCATE ( msfv_input(ix,jx) )
         IF ( .NOT. ALLOCATED ( sina_input) ) ALLOCATE ( sina_input(ix,jx) )
         IF ( .NOT. ALLOCATED ( cosa_input) ) ALLOCATE ( cosa_input(ix,jx) )
         IF ( .NOT. ALLOCATED ( e_input) ) ALLOCATE ( e_input(ix,jx) )
         IF ( .NOT. ALLOCATED ( f_input) ) ALLOCATE ( f_input(ix,jx) )
         IF ( .NOT. ALLOCATED ( mu_input)  ) ALLOCATE ( mu_input(ix,jx)  )

      END IF

      CLOSE(13)
      OPEN ( FILE   = 'real_input_em.d01.'//file_date_string , &
             UNIT   = 13                                     , &
             STATUS = 'OLD'                                  , &
             ACCESS = 'SEQUENTIAL'                           , &
             FORM   = 'UNFORMATTED'                            )

      READ (13) ix_input,jx_input,kx_input,mminlu,iswater,gmt,julday,julyr,cen_lat,dx_input,&
                map_proj , truelat1 , truelat2 , cen_lon , p_top_input

      !  A simple domain size test.
    
      IF ( ( ix_input .NE. ix ) .OR. ( jx_input .NE. jx ) .OR. ( kx_input .NE. kx ) ) THEN
         PRINT '(A)','Namelist does not match the input data.'
         PRINT '(A,3I5,A)','Namelist dimensions =',ix,jx,kx,'.'
         PRINT '(A,3I5,A)','Input data dimensions =',ix_input,jx_input,kx_input,'.'
         STOP 'Wrong_data_size'
      END IF

      !  How about the grid distance?  Is it the same as in the namelist?

      IF ( ABS ( dx_input - dx ) .GT. 1 ) THEN
         PRINT '(A)','Grid distance differs between input file and namelist.'
         PRINT '(A,F8.1,A)','Grid distance in input file = ',dx_input,' m.'
         PRINT '(A,F8.1,A)','Grid distance in namelist = ',dx,' m.'
         STOP 'Wrong_grid_distance'
      END IF

      p_top = p_top_input

      CALL set_cen_lat ( grid%id , cen_lat )
      CALL set_cen_lon ( grid%id , cen_lon )
      CALL set_truelat1 ( grid%id , truelat1 )
      CALL set_truelat2 ( grid%id , truelat2 )
      CALL set_map_proj ( grid%id , map_proj ) 
      CALL set_mminlu ( mminlu(1:4) )
      CALL set_iswater (grid%id, iswater )
      CALL set_gmt (grid%id, gmt ) 
      CALL set_julyr (grid%id, julyr )
      CALL set_julday (grid%id, julday )

      read_all_the_data : DO

         READ (13,IOSTAT=OK) dummy_char

         IF ( OK .NE. 0 ) THEN
            PRINT '(A,A,A)','End of file found for real_input_em.d01.',file_date_string,'.'
            EXIT read_all_the_data
         END IF

!print *,'name = ',dummy_char(1:8)

         IF      ( dummy_char(1:8) .EQ. 'ETAHALF ' ) THEN
            READ (13) etahalf
         ELSE IF ( dummy_char(1:8) .EQ. 'ETAFULL ' ) THEN
            READ (13) etafull
!print *,'etafull=',etafull
         ELSE IF ( dummy_char(1:8) .EQ. 'LATITCRS' ) THEN
            READ (13) lat_input
         ELSE IF ( dummy_char(1:8) .EQ. 'LONGICRS' ) THEN
            READ (13) lon_input
         ELSE IF ( dummy_char(1:8) .EQ. 'TERRAIN ' ) THEN 
            READ (13) ter_input
         ELSE IF ( dummy_char(1:8) .EQ. 'MSFT    ' ) THEN
            READ (13) msft_input
         ELSE IF ( dummy_char(1:8) .EQ. 'MSFU    ' ) THEN
            READ (13) msfu_input
         ELSE IF ( dummy_char(1:8) .EQ. 'MSFV    ' ) THEN
            READ (13) msfv_input
         ELSE IF ( dummy_char(1:8) .EQ. 'U       ' ) THEN
            READ (13) u_input
         ELSE IF ( dummy_char(1:8) .EQ. 'V       ' ) THEN
            READ (13) v_input
         ELSE IF ( dummy_char(1:8) .EQ. 'THETA   ' ) THEN
            READ (13) theta_input
         ELSE IF ( dummy_char(1:8) .EQ. 'Q       ' ) THEN
            READ (13) q_input
         ELSE IF ( dummy_char(1:8) .EQ. 'MU      ' ) THEN
            READ (13) mu_input 
         ELSE IF ( dummy_char(1:8) .EQ. 'WEASD   ' ) THEN
            READ (13) dummy
         ELSE IF ( dummy_char(1:8) .EQ. 'GROUND T' ) THEN
            READ (13) tg_input
         ELSE IF ( dummy_char(1:8) .EQ. 'RES TEMP' ) THEN
            READ (13) res_input
         ELSE IF ( dummy_char(1:8) .EQ. 'SOILT010' ) THEN
            READ (13) st000010_input
         ELSE IF ( dummy_char(1:8) .EQ. 'SOILT200' ) THEN
            READ (13) st010200_input
         ELSE IF ( dummy_char(1:8) .EQ. 'SOILT400' ) THEN
            READ (13) dummy
         ELSE IF ( dummy_char(1:8) .EQ. 'SOILM010' ) THEN
            READ (13) sm000010_input
         ELSE IF ( dummy_char(1:8) .EQ. 'SOILM200' ) THEN
            READ (13) sm010200_input
         ELSE IF ( dummy_char(1:8) .EQ. 'SEAICE  ' ) THEN
            READ (13) seaice_input
         ELSE IF ( dummy_char(1:8) .EQ. 'LANDSEA ' ) THEN
            READ (13) dummy
         ELSE IF ( dummy_char(1:8) .EQ. 'LANDMASK' ) THEN
            READ (13) landmask_input
         ELSE IF ( dummy_char(1:8) .EQ. 'LAND USE' ) THEN
            READ (13) landuse_input
         ELSE IF ( dummy_char(1:8) .EQ. 'COSA    ' ) THEN
            READ (13) cosa_input
         ELSE IF ( dummy_char(1:8) .EQ. 'SINA    ' ) THEN
            READ (13) sina_input
         ELSE IF ( dummy_char(1:8) .EQ. 'F       ' ) THEN
            READ (13) f_input
         ELSE IF ( dummy_char(1:8) .EQ. 'E       ' ) THEN
            READ (13) e_input
         ELSE IF ( dummy_char(1:8) .EQ. 'SNOWCOVR' ) THEN
            READ (13) snow_input
         ELSE
            READ (13) dummy
         END IF
 
!        print *,' '

      END DO read_all_the_data

      CLOSE (13)

      first_time_in = .FALSE.

   END SUBROUTINE read_mm5

END MODULE module_si_io_em
