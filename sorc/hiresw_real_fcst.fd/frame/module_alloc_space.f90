MODULE module_alloc_space
CONTAINS
   SUBROUTINE alloc_space_field_core ( grid,   id, setinitval_in ,  tl_in , inter_domain_in ,   &
                                  sd31, ed31, sd32, ed32, sd33, ed33, &
                                  sm31 , em31 , sm32 , em32 , sm33 , em33 , &
                                  sm31x, em31x, sm32x, em32x, sm33x, em33x, &
                                  sm31y, em31y, sm32y, em32y, sm33y, em33y )

      USE module_domain_type
      USE module_configure, ONLY : model_config_rec, in_use_for_config
      USE module_state_description

      IMPLICIT NONE

      

      TYPE(domain)               , POINTER          :: grid
      INTEGER , INTENT(IN)            :: id
      INTEGER , INTENT(IN)            :: setinitval_in   
      INTEGER , INTENT(IN)            :: sd31, ed31, sd32, ed32, sd33, ed33
      INTEGER , INTENT(IN)            :: sm31, em31, sm32, em32, sm33, em33
      INTEGER , INTENT(IN)            :: sm31x, em31x, sm32x, em32x, sm33x, em33x
      INTEGER , INTENT(IN)            :: sm31y, em31y, sm32y, em32y, sm33y, em33y

      
      
      
      
      INTEGER , INTENT(IN)            :: tl_in
 
      
      
      LOGICAL , INTENT(IN)            :: inter_domain_in

      
      INTEGER idum1, idum2, spec_bdy_width
      INTEGER num_bytes_allocated
      REAL    initial_data_value
      CHARACTER (LEN=256) message
      INTEGER tl
      LOGICAL inter_domain
      INTEGER setinitval
      INTEGER sr_x, sr_y

      
      INTEGER ierr

      INTEGER                              :: loop

      CALL nl_get_sr_x( id , sr_x )
      CALL nl_get_sr_x( id , sr_y )

      tl = tl_in
      inter_domain = inter_domain_in

      CALL get_initial_data_value ( initial_data_value )

      setinitval = setinitval_in

      CALL nl_get_spec_bdy_width( 1, spec_bdy_width )

      CALL set_scalar_indices_from_config( id , idum1 , idum2 )

      num_bytes_allocated = 0 

      IF ( grid%id .EQ. 1 ) &
          CALL wrf_message ( 'DYNAMICS OPTION: nmm dyncore' )







IF(in_use_for_config(id,'x_1').AND.(.NOT.inter_domain).AND.(IAND(1,tl).NE.0))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%x_1(sm31:em31,sm33:em33,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",80,&
    'frame/module_domain.f: Failed to allocate grid%x_1(sm31:em31,sm33:em33,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%x_1=initial_data_value
ELSE
  ALLOCATE(grid%x_1(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",87,&
    'frame/module_domain.f: Failed to allocate grid%x_1(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'x_2').AND.(.NOT.inter_domain).AND.(IAND(2,tl).NE.0))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%x_2(sm31:em31,sm33:em33,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",96,&
    'frame/module_domain.f: Failed to allocate grid%x_2(sm31:em31,sm33:em33,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%x_2=initial_data_value
ELSE
  ALLOCATE(grid%x_2(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",103,&
    'frame/module_domain.f: Failed to allocate grid%x_2(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'lu_index'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%lu_index(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",112,&
    'frame/module_domain.f: Failed to allocate grid%lu_index(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%lu_index=initial_data_value
ELSE
  ALLOCATE(grid%lu_index(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",119,&
    'frame/module_domain.f: Failed to allocate grid%lu_index(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'lu_mask').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%lu_mask(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",128,&
    'frame/module_domain.f: Failed to allocate grid%lu_mask(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%lu_mask=initial_data_value
ELSE
  ALLOCATE(grid%lu_mask(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",135,&
    'frame/module_domain.f: Failed to allocate grid%lu_mask(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'p_gc').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((model_config_rec%num_metgrid_levels)-(1)+1))) * 4
  ALLOCATE(grid%p_gc(sm31:em31,sm32:em32,1:model_config_rec%num_metgrid_levels),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",144,&
    'frame/module_domain.f: Failed to allocate grid%p_gc(sm31:em31,sm32:em32,1:model_config_rec%num_metgrid_levels). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%p_gc=initial_data_value
ELSE
  ALLOCATE(grid%p_gc(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",151,&
    'frame/module_domain.f: Failed to allocate grid%p_gc(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'vegcat').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%vegcat(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",160,&
    'frame/module_domain.f: Failed to allocate grid%vegcat(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%vegcat=initial_data_value
ELSE
  ALLOCATE(grid%vegcat(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",167,&
    'frame/module_domain.f: Failed to allocate grid%vegcat(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'soilcat').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%soilcat(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",176,&
    'frame/module_domain.f: Failed to allocate grid%soilcat(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilcat=initial_data_value
ELSE
  ALLOCATE(grid%soilcat(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",183,&
    'frame/module_domain.f: Failed to allocate grid%soilcat(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'input_soil_cat').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%input_soil_cat(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",192,&
    'frame/module_domain.f: Failed to allocate grid%input_soil_cat(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%input_soil_cat=initial_data_value
ELSE
  ALLOCATE(grid%input_soil_cat(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",199,&
    'frame/module_domain.f: Failed to allocate grid%input_soil_cat(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'tsk_gc').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%tsk_gc(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",208,&
    'frame/module_domain.f: Failed to allocate grid%tsk_gc(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tsk_gc=initial_data_value
ELSE
  ALLOCATE(grid%tsk_gc(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",215,&
    'frame/module_domain.f: Failed to allocate grid%tsk_gc(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'xice_gc').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%xice_gc(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",224,&
    'frame/module_domain.f: Failed to allocate grid%xice_gc(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%xice_gc=initial_data_value
ELSE
  ALLOCATE(grid%xice_gc(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",231,&
    'frame/module_domain.f: Failed to allocate grid%xice_gc(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'ght_gc').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((model_config_rec%num_metgrid_levels)-(1)+1))) * 4
  ALLOCATE(grid%ght_gc(sm31:em31,sm32:em32,1:model_config_rec%num_metgrid_levels),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",240,&
    'frame/module_domain.f: Failed to allocate grid%ght_gc(sm31:em31,sm32:em32,1:model_config_rec%num_metgrid_levels). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ght_gc=initial_data_value
ELSE
  ALLOCATE(grid%ght_gc(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",247,&
    'frame/module_domain.f: Failed to allocate grid%ght_gc(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'rh_gc').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((model_config_rec%num_metgrid_levels)-(1)+1))) * 4
  ALLOCATE(grid%rh_gc(sm31:em31,sm32:em32,1:model_config_rec%num_metgrid_levels),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",256,&
    'frame/module_domain.f: Failed to allocate grid%rh_gc(sm31:em31,sm32:em32,1:model_config_rec%num_metgrid_levels). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rh_gc=initial_data_value
ELSE
  ALLOCATE(grid%rh_gc(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",263,&
    'frame/module_domain.f: Failed to allocate grid%rh_gc(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'v_gc').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((model_config_rec%num_metgrid_levels)-(1)+1))) * 4
  ALLOCATE(grid%v_gc(sm31:em31,sm32:em32,1:model_config_rec%num_metgrid_levels),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",272,&
    'frame/module_domain.f: Failed to allocate grid%v_gc(sm31:em31,sm32:em32,1:model_config_rec%num_metgrid_levels). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%v_gc=initial_data_value
ELSE
  ALLOCATE(grid%v_gc(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",279,&
    'frame/module_domain.f: Failed to allocate grid%v_gc(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'u_gc').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((model_config_rec%num_metgrid_levels)-(1)+1))) * 4
  ALLOCATE(grid%u_gc(sm31:em31,sm32:em32,1:model_config_rec%num_metgrid_levels),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",288,&
    'frame/module_domain.f: Failed to allocate grid%u_gc(sm31:em31,sm32:em32,1:model_config_rec%num_metgrid_levels). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%u_gc=initial_data_value
ELSE
  ALLOCATE(grid%u_gc(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",295,&
    'frame/module_domain.f: Failed to allocate grid%u_gc(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'t_gc').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((model_config_rec%num_metgrid_levels)-(1)+1))) * 4
  ALLOCATE(grid%t_gc(sm31:em31,sm32:em32,1:model_config_rec%num_metgrid_levels),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",304,&
    'frame/module_domain.f: Failed to allocate grid%t_gc(sm31:em31,sm32:em32,1:model_config_rec%num_metgrid_levels). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%t_gc=initial_data_value
ELSE
  ALLOCATE(grid%t_gc(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",311,&
    'frame/module_domain.f: Failed to allocate grid%t_gc(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'snoalb').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%snoalb(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",320,&
    'frame/module_domain.f: Failed to allocate grid%snoalb(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%snoalb=initial_data_value
ELSE
  ALLOCATE(grid%snoalb(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",327,&
    'frame/module_domain.f: Failed to allocate grid%snoalb(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'greenfrac_gc').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((12)-(1)+1))) * 4
  ALLOCATE(grid%greenfrac_gc(sm31:em31,sm32:em32,1:12),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",336,&
    'frame/module_domain.f: Failed to allocate grid%greenfrac_gc(sm31:em31,sm32:em32,1:12). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%greenfrac_gc=initial_data_value
ELSE
  ALLOCATE(grid%greenfrac_gc(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",343,&
    'frame/module_domain.f: Failed to allocate grid%greenfrac_gc(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'albedo12m_gc').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((12)-(1)+1))) * 4
  ALLOCATE(grid%albedo12m_gc(sm31:em31,sm32:em32,1:12),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",352,&
    'frame/module_domain.f: Failed to allocate grid%albedo12m_gc(sm31:em31,sm32:em32,1:12). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%albedo12m_gc=initial_data_value
ELSE
  ALLOCATE(grid%albedo12m_gc(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",359,&
    'frame/module_domain.f: Failed to allocate grid%albedo12m_gc(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'soilcbot_gc').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((model_config_rec%num_soil_cat)-(1)+1))) * 4
  ALLOCATE(grid%soilcbot_gc(sm31:em31,sm32:em32,1:model_config_rec%num_soil_cat),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",368,&
    'frame/module_domain.f: Failed to allocate grid%soilcbot_gc(sm31:em31,sm32:em32,1:model_config_rec%num_soil_cat). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilcbot_gc=initial_data_value
ELSE
  ALLOCATE(grid%soilcbot_gc(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",375,&
    'frame/module_domain.f: Failed to allocate grid%soilcbot_gc(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'soilctop_gc').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((model_config_rec%num_soil_cat)-(1)+1))) * 4
  ALLOCATE(grid%soilctop_gc(sm31:em31,sm32:em32,1:model_config_rec%num_soil_cat),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",384,&
    'frame/module_domain.f: Failed to allocate grid%soilctop_gc(sm31:em31,sm32:em32,1:model_config_rec%num_soil_cat). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilctop_gc=initial_data_value
ELSE
  ALLOCATE(grid%soilctop_gc(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",391,&
    'frame/module_domain.f: Failed to allocate grid%soilctop_gc(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'tmn_gc').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%tmn_gc(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",400,&
    'frame/module_domain.f: Failed to allocate grid%tmn_gc(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tmn_gc=initial_data_value
ELSE
  ALLOCATE(grid%tmn_gc(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",407,&
    'frame/module_domain.f: Failed to allocate grid%tmn_gc(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'htv_gc').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%htv_gc(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",416,&
    'frame/module_domain.f: Failed to allocate grid%htv_gc(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%htv_gc=initial_data_value
ELSE
  ALLOCATE(grid%htv_gc(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",423,&
    'frame/module_domain.f: Failed to allocate grid%htv_gc(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'ht_gc').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%ht_gc(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",432,&
    'frame/module_domain.f: Failed to allocate grid%ht_gc(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ht_gc=initial_data_value
ELSE
  ALLOCATE(grid%ht_gc(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",439,&
    'frame/module_domain.f: Failed to allocate grid%ht_gc(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'landusef_gc').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((model_config_rec%num_land_cat)-(1)+1))) * 4
  ALLOCATE(grid%landusef_gc(sm31:em31,sm32:em32,1:model_config_rec%num_land_cat),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",448,&
    'frame/module_domain.f: Failed to allocate grid%landusef_gc(sm31:em31,sm32:em32,1:model_config_rec%num_land_cat). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%landusef_gc=initial_data_value
ELSE
  ALLOCATE(grid%landusef_gc(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",455,&
    'frame/module_domain.f: Failed to allocate grid%landusef_gc(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'vlon_gc').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%vlon_gc(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",464,&
    'frame/module_domain.f: Failed to allocate grid%vlon_gc(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%vlon_gc=initial_data_value
ELSE
  ALLOCATE(grid%vlon_gc(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",471,&
    'frame/module_domain.f: Failed to allocate grid%vlon_gc(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'vlat_gc').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%vlat_gc(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",480,&
    'frame/module_domain.f: Failed to allocate grid%vlat_gc(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%vlat_gc=initial_data_value
ELSE
  ALLOCATE(grid%vlat_gc(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",487,&
    'frame/module_domain.f: Failed to allocate grid%vlat_gc(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'hlon_gc').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%hlon_gc(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",496,&
    'frame/module_domain.f: Failed to allocate grid%hlon_gc(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%hlon_gc=initial_data_value
ELSE
  ALLOCATE(grid%hlon_gc(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",503,&
    'frame/module_domain.f: Failed to allocate grid%hlon_gc(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'hlat_gc').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%hlat_gc(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",512,&
    'frame/module_domain.f: Failed to allocate grid%hlat_gc(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%hlat_gc=initial_data_value
ELSE
  ALLOCATE(grid%hlat_gc(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",519,&
    'frame/module_domain.f: Failed to allocate grid%hlat_gc(1,1).  ')
  endif
ENDIF
IF ( setinitval .EQ. 3 ) grid%moved=.FALSE.
IF(in_use_for_config(id,'hbm2').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%hbm2(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",529,&
    'frame/module_domain.f: Failed to allocate grid%hbm2(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%hbm2=initial_data_value
ELSE
  ALLOCATE(grid%hbm2(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",536,&
    'frame/module_domain.f: Failed to allocate grid%hbm2(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'hbm3').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%hbm3(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",545,&
    'frame/module_domain.f: Failed to allocate grid%hbm3(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%hbm3=initial_data_value
ELSE
  ALLOCATE(grid%hbm3(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",552,&
    'frame/module_domain.f: Failed to allocate grid%hbm3(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'vbm2').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%vbm2(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",561,&
    'frame/module_domain.f: Failed to allocate grid%vbm2(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%vbm2=initial_data_value
ELSE
  ALLOCATE(grid%vbm2(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",568,&
    'frame/module_domain.f: Failed to allocate grid%vbm2(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'vbm3').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%vbm3(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",577,&
    'frame/module_domain.f: Failed to allocate grid%vbm3(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%vbm3=initial_data_value
ELSE
  ALLOCATE(grid%vbm3(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",584,&
    'frame/module_domain.f: Failed to allocate grid%vbm3(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'sm').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%sm(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",593,&
    'frame/module_domain.f: Failed to allocate grid%sm(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sm=initial_data_value
ELSE
  ALLOCATE(grid%sm(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",600,&
    'frame/module_domain.f: Failed to allocate grid%sm(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'sice').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%sice(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",609,&
    'frame/module_domain.f: Failed to allocate grid%sice(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sice=initial_data_value
ELSE
  ALLOCATE(grid%sice(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",616,&
    'frame/module_domain.f: Failed to allocate grid%sice(1,1).  ')
  endif
ENDIF
IF ( setinitval .EQ. 3 ) grid%ntsd=0
IF ( setinitval .EQ. 3 ) grid%nstart_hour=0
IF(in_use_for_config(id,'pd').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%pd(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",627,&
    'frame/module_domain.f: Failed to allocate grid%pd(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%pd=initial_data_value
ELSE
  ALLOCATE(grid%pd(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",634,&
    'frame/module_domain.f: Failed to allocate grid%pd(1,1).  ')
  endif
ENDIF
IF(.TRUE..AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
(((em32-sm32+1)*(spec_bdy_width))) * 4
  ALLOCATE(grid%pd_bxs(sm32:em32,1,spec_bdy_width),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",643,&
    'frame/module_domain.f: Failed to allocate grid%pd_bxs(sm32:em32,1,spec_bdy_width). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%pd_bxs=initial_data_value
  num_bytes_allocated = num_bytes_allocated + &
(((em32-sm32+1)*(spec_bdy_width))) * 4
  ALLOCATE(grid%pd_bxe(sm32:em32,1,spec_bdy_width),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",651,&
    'frame/module_domain.f: Failed to allocate grid%pd_bxe(sm32:em32,1,spec_bdy_width). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%pd_bxe=initial_data_value
  num_bytes_allocated = num_bytes_allocated + &
(((em31-sm31+1)*(spec_bdy_width))) * 4
  ALLOCATE(grid%pd_bys(sm31:em31,1,spec_bdy_width),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",659,&
    'frame/module_domain.f: Failed to allocate grid%pd_bys(sm31:em31,1,spec_bdy_width). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%pd_bys=initial_data_value
  num_bytes_allocated = num_bytes_allocated + &
(((em31-sm31+1)*(spec_bdy_width))) * 4
  ALLOCATE(grid%pd_bye(sm31:em31,1,spec_bdy_width),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",667,&
    'frame/module_domain.f: Failed to allocate grid%pd_bye(sm31:em31,1,spec_bdy_width). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%pd_bye=initial_data_value
ELSE
  ALLOCATE(grid%pd_bxs(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",674,&
    'frame/module_domain.f: Failed to allocate grid%pd_bxs(1,1,1).  ')
  endif
  ALLOCATE(grid%pd_bxe(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",679,&
    'frame/module_domain.f: Failed to allocate grid%pd_bxe(1,1,1).  ')
  endif
  ALLOCATE(grid%pd_bys(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",684,&
    'frame/module_domain.f: Failed to allocate grid%pd_bys(1,1,1).  ')
  endif
  ALLOCATE(grid%pd_bye(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",689,&
    'frame/module_domain.f: Failed to allocate grid%pd_bye(1,1,1).  ')
  endif
ENDIF
IF(.TRUE..AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
(((em32-sm32+1)*(spec_bdy_width))) * 4
  ALLOCATE(grid%pd_btxs(sm32:em32,1,spec_bdy_width),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",698,&
    'frame/module_domain.f: Failed to allocate grid%pd_btxs(sm32:em32,1,spec_bdy_width). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%pd_btxs=initial_data_value
  num_bytes_allocated = num_bytes_allocated + &
(((em32-sm32+1)*(spec_bdy_width))) * 4
  ALLOCATE(grid%pd_btxe(sm32:em32,1,spec_bdy_width),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",706,&
    'frame/module_domain.f: Failed to allocate grid%pd_btxe(sm32:em32,1,spec_bdy_width). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%pd_btxe=initial_data_value
  num_bytes_allocated = num_bytes_allocated + &
(((em31-sm31+1)*(spec_bdy_width))) * 4
  ALLOCATE(grid%pd_btys(sm31:em31,1,spec_bdy_width),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",714,&
    'frame/module_domain.f: Failed to allocate grid%pd_btys(sm31:em31,1,spec_bdy_width). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%pd_btys=initial_data_value
  num_bytes_allocated = num_bytes_allocated + &
(((em31-sm31+1)*(spec_bdy_width))) * 4
  ALLOCATE(grid%pd_btye(sm31:em31,1,spec_bdy_width),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",722,&
    'frame/module_domain.f: Failed to allocate grid%pd_btye(sm31:em31,1,spec_bdy_width). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%pd_btye=initial_data_value
ELSE
  ALLOCATE(grid%pd_btxs(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",729,&
    'frame/module_domain.f: Failed to allocate grid%pd_btxs(1,1,1).  ')
  endif
  ALLOCATE(grid%pd_btxe(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",734,&
    'frame/module_domain.f: Failed to allocate grid%pd_btxe(1,1,1).  ')
  endif
  ALLOCATE(grid%pd_btys(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",739,&
    'frame/module_domain.f: Failed to allocate grid%pd_btys(1,1,1).  ')
  endif
  ALLOCATE(grid%pd_btye(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",744,&
    'frame/module_domain.f: Failed to allocate grid%pd_btye(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'fis').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%fis(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",753,&
    'frame/module_domain.f: Failed to allocate grid%fis(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%fis=initial_data_value
ELSE
  ALLOCATE(grid%fis(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",760,&
    'frame/module_domain.f: Failed to allocate grid%fis(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'res').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%res(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",769,&
    'frame/module_domain.f: Failed to allocate grid%res(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%res=initial_data_value
ELSE
  ALLOCATE(grid%res(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",776,&
    'frame/module_domain.f: Failed to allocate grid%res(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'t').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%t(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",785,&
    'frame/module_domain.f: Failed to allocate grid%t(sm31:em31,sm32:em32,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%t=initial_data_value
ELSE
  ALLOCATE(grid%t(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",792,&
    'frame/module_domain.f: Failed to allocate grid%t(1,1,1).  ')
  endif
ENDIF
IF(.TRUE..AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
(((em32-sm32+1)*(em33-sm33+1)*(spec_bdy_width))) * 4
  ALLOCATE(grid%t_bxs(sm32:em32,sm33:em33,spec_bdy_width),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",801,&
    'frame/module_domain.f: Failed to allocate grid%t_bxs(sm32:em32,sm33:em33,spec_bdy_width). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%t_bxs=initial_data_value
  num_bytes_allocated = num_bytes_allocated + &
(((em32-sm32+1)*(em33-sm33+1)*(spec_bdy_width))) * 4
  ALLOCATE(grid%t_bxe(sm32:em32,sm33:em33,spec_bdy_width),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",809,&
    'frame/module_domain.f: Failed to allocate grid%t_bxe(sm32:em32,sm33:em33,spec_bdy_width). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%t_bxe=initial_data_value
  num_bytes_allocated = num_bytes_allocated + &
(((em31-sm31+1)*(em33-sm33+1)*(spec_bdy_width))) * 4
  ALLOCATE(grid%t_bys(sm31:em31,sm33:em33,spec_bdy_width),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",817,&
    'frame/module_domain.f: Failed to allocate grid%t_bys(sm31:em31,sm33:em33,spec_bdy_width). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%t_bys=initial_data_value
  num_bytes_allocated = num_bytes_allocated + &
(((em31-sm31+1)*(em33-sm33+1)*(spec_bdy_width))) * 4
  ALLOCATE(grid%t_bye(sm31:em31,sm33:em33,spec_bdy_width),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",825,&
    'frame/module_domain.f: Failed to allocate grid%t_bye(sm31:em31,sm33:em33,spec_bdy_width). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%t_bye=initial_data_value
ELSE
  ALLOCATE(grid%t_bxs(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",832,&
    'frame/module_domain.f: Failed to allocate grid%t_bxs(1,1,1).  ')
  endif
  ALLOCATE(grid%t_bxe(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",837,&
    'frame/module_domain.f: Failed to allocate grid%t_bxe(1,1,1).  ')
  endif
  ALLOCATE(grid%t_bys(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",842,&
    'frame/module_domain.f: Failed to allocate grid%t_bys(1,1,1).  ')
  endif
  ALLOCATE(grid%t_bye(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",847,&
    'frame/module_domain.f: Failed to allocate grid%t_bye(1,1,1).  ')
  endif
ENDIF
IF(.TRUE..AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
(((em32-sm32+1)*(em33-sm33+1)*(spec_bdy_width))) * 4
  ALLOCATE(grid%t_btxs(sm32:em32,sm33:em33,spec_bdy_width),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",856,&
    'frame/module_domain.f: Failed to allocate grid%t_btxs(sm32:em32,sm33:em33,spec_bdy_width). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%t_btxs=initial_data_value
  num_bytes_allocated = num_bytes_allocated + &
(((em32-sm32+1)*(em33-sm33+1)*(spec_bdy_width))) * 4
  ALLOCATE(grid%t_btxe(sm32:em32,sm33:em33,spec_bdy_width),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",864,&
    'frame/module_domain.f: Failed to allocate grid%t_btxe(sm32:em32,sm33:em33,spec_bdy_width). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%t_btxe=initial_data_value
  num_bytes_allocated = num_bytes_allocated + &
(((em31-sm31+1)*(em33-sm33+1)*(spec_bdy_width))) * 4
  ALLOCATE(grid%t_btys(sm31:em31,sm33:em33,spec_bdy_width),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",872,&
    'frame/module_domain.f: Failed to allocate grid%t_btys(sm31:em31,sm33:em33,spec_bdy_width). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%t_btys=initial_data_value
  num_bytes_allocated = num_bytes_allocated + &
(((em31-sm31+1)*(em33-sm33+1)*(spec_bdy_width))) * 4
  ALLOCATE(grid%t_btye(sm31:em31,sm33:em33,spec_bdy_width),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",880,&
    'frame/module_domain.f: Failed to allocate grid%t_btye(sm31:em31,sm33:em33,spec_bdy_width). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%t_btye=initial_data_value
ELSE
  ALLOCATE(grid%t_btxs(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",887,&
    'frame/module_domain.f: Failed to allocate grid%t_btxs(1,1,1).  ')
  endif
  ALLOCATE(grid%t_btxe(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",892,&
    'frame/module_domain.f: Failed to allocate grid%t_btxe(1,1,1).  ')
  endif
  ALLOCATE(grid%t_btys(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",897,&
    'frame/module_domain.f: Failed to allocate grid%t_btys(1,1,1).  ')
  endif
  ALLOCATE(grid%t_btye(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",902,&
    'frame/module_domain.f: Failed to allocate grid%t_btye(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'q').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%q(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",911,&
    'frame/module_domain.f: Failed to allocate grid%q(sm31:em31,sm32:em32,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%q=initial_data_value
ELSE
  ALLOCATE(grid%q(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",918,&
    'frame/module_domain.f: Failed to allocate grid%q(1,1,1).  ')
  endif
ENDIF
IF(.TRUE..AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
(((em32-sm32+1)*(em33-sm33+1)*(spec_bdy_width))) * 4
  ALLOCATE(grid%q_bxs(sm32:em32,sm33:em33,spec_bdy_width),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",927,&
    'frame/module_domain.f: Failed to allocate grid%q_bxs(sm32:em32,sm33:em33,spec_bdy_width). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%q_bxs=initial_data_value
  num_bytes_allocated = num_bytes_allocated + &
(((em32-sm32+1)*(em33-sm33+1)*(spec_bdy_width))) * 4
  ALLOCATE(grid%q_bxe(sm32:em32,sm33:em33,spec_bdy_width),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",935,&
    'frame/module_domain.f: Failed to allocate grid%q_bxe(sm32:em32,sm33:em33,spec_bdy_width). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%q_bxe=initial_data_value
  num_bytes_allocated = num_bytes_allocated + &
(((em31-sm31+1)*(em33-sm33+1)*(spec_bdy_width))) * 4
  ALLOCATE(grid%q_bys(sm31:em31,sm33:em33,spec_bdy_width),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",943,&
    'frame/module_domain.f: Failed to allocate grid%q_bys(sm31:em31,sm33:em33,spec_bdy_width). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%q_bys=initial_data_value
  num_bytes_allocated = num_bytes_allocated + &
(((em31-sm31+1)*(em33-sm33+1)*(spec_bdy_width))) * 4
  ALLOCATE(grid%q_bye(sm31:em31,sm33:em33,spec_bdy_width),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",951,&
    'frame/module_domain.f: Failed to allocate grid%q_bye(sm31:em31,sm33:em33,spec_bdy_width). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%q_bye=initial_data_value
ELSE
  ALLOCATE(grid%q_bxs(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",958,&
    'frame/module_domain.f: Failed to allocate grid%q_bxs(1,1,1).  ')
  endif
  ALLOCATE(grid%q_bxe(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",963,&
    'frame/module_domain.f: Failed to allocate grid%q_bxe(1,1,1).  ')
  endif
  ALLOCATE(grid%q_bys(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",968,&
    'frame/module_domain.f: Failed to allocate grid%q_bys(1,1,1).  ')
  endif
  ALLOCATE(grid%q_bye(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",973,&
    'frame/module_domain.f: Failed to allocate grid%q_bye(1,1,1).  ')
  endif
ENDIF
IF(.TRUE..AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
(((em32-sm32+1)*(em33-sm33+1)*(spec_bdy_width))) * 4
  ALLOCATE(grid%q_btxs(sm32:em32,sm33:em33,spec_bdy_width),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",982,&
    'frame/module_domain.f: Failed to allocate grid%q_btxs(sm32:em32,sm33:em33,spec_bdy_width). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%q_btxs=initial_data_value
  num_bytes_allocated = num_bytes_allocated + &
(((em32-sm32+1)*(em33-sm33+1)*(spec_bdy_width))) * 4
  ALLOCATE(grid%q_btxe(sm32:em32,sm33:em33,spec_bdy_width),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",990,&
    'frame/module_domain.f: Failed to allocate grid%q_btxe(sm32:em32,sm33:em33,spec_bdy_width). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%q_btxe=initial_data_value
  num_bytes_allocated = num_bytes_allocated + &
(((em31-sm31+1)*(em33-sm33+1)*(spec_bdy_width))) * 4
  ALLOCATE(grid%q_btys(sm31:em31,sm33:em33,spec_bdy_width),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",998,&
    'frame/module_domain.f: Failed to allocate grid%q_btys(sm31:em31,sm33:em33,spec_bdy_width). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%q_btys=initial_data_value
  num_bytes_allocated = num_bytes_allocated + &
(((em31-sm31+1)*(em33-sm33+1)*(spec_bdy_width))) * 4
  ALLOCATE(grid%q_btye(sm31:em31,sm33:em33,spec_bdy_width),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1006,&
    'frame/module_domain.f: Failed to allocate grid%q_btye(sm31:em31,sm33:em33,spec_bdy_width). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%q_btye=initial_data_value
ELSE
  ALLOCATE(grid%q_btxs(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1013,&
    'frame/module_domain.f: Failed to allocate grid%q_btxs(1,1,1).  ')
  endif
  ALLOCATE(grid%q_btxe(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1018,&
    'frame/module_domain.f: Failed to allocate grid%q_btxe(1,1,1).  ')
  endif
  ALLOCATE(grid%q_btys(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1023,&
    'frame/module_domain.f: Failed to allocate grid%q_btys(1,1,1).  ')
  endif
  ALLOCATE(grid%q_btye(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1028,&
    'frame/module_domain.f: Failed to allocate grid%q_btye(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'u').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%u(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1037,&
    'frame/module_domain.f: Failed to allocate grid%u(sm31:em31,sm32:em32,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%u=initial_data_value
ELSE
  ALLOCATE(grid%u(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1044,&
    'frame/module_domain.f: Failed to allocate grid%u(1,1,1).  ')
  endif
ENDIF
IF(.TRUE..AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
(((em32-sm32+1)*(em33-sm33+1)*(spec_bdy_width))) * 4
  ALLOCATE(grid%u_bxs(sm32:em32,sm33:em33,spec_bdy_width),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1053,&
    'frame/module_domain.f: Failed to allocate grid%u_bxs(sm32:em32,sm33:em33,spec_bdy_width). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%u_bxs=initial_data_value
  num_bytes_allocated = num_bytes_allocated + &
(((em32-sm32+1)*(em33-sm33+1)*(spec_bdy_width))) * 4
  ALLOCATE(grid%u_bxe(sm32:em32,sm33:em33,spec_bdy_width),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1061,&
    'frame/module_domain.f: Failed to allocate grid%u_bxe(sm32:em32,sm33:em33,spec_bdy_width). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%u_bxe=initial_data_value
  num_bytes_allocated = num_bytes_allocated + &
(((em31-sm31+1)*(em33-sm33+1)*(spec_bdy_width))) * 4
  ALLOCATE(grid%u_bys(sm31:em31,sm33:em33,spec_bdy_width),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1069,&
    'frame/module_domain.f: Failed to allocate grid%u_bys(sm31:em31,sm33:em33,spec_bdy_width). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%u_bys=initial_data_value
  num_bytes_allocated = num_bytes_allocated + &
(((em31-sm31+1)*(em33-sm33+1)*(spec_bdy_width))) * 4
  ALLOCATE(grid%u_bye(sm31:em31,sm33:em33,spec_bdy_width),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1077,&
    'frame/module_domain.f: Failed to allocate grid%u_bye(sm31:em31,sm33:em33,spec_bdy_width). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%u_bye=initial_data_value
ELSE
  ALLOCATE(grid%u_bxs(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1084,&
    'frame/module_domain.f: Failed to allocate grid%u_bxs(1,1,1).  ')
  endif
  ALLOCATE(grid%u_bxe(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1089,&
    'frame/module_domain.f: Failed to allocate grid%u_bxe(1,1,1).  ')
  endif
  ALLOCATE(grid%u_bys(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1094,&
    'frame/module_domain.f: Failed to allocate grid%u_bys(1,1,1).  ')
  endif
  ALLOCATE(grid%u_bye(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1099,&
    'frame/module_domain.f: Failed to allocate grid%u_bye(1,1,1).  ')
  endif
ENDIF
IF(.TRUE..AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
(((em32-sm32+1)*(em33-sm33+1)*(spec_bdy_width))) * 4
  ALLOCATE(grid%u_btxs(sm32:em32,sm33:em33,spec_bdy_width),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1108,&
    'frame/module_domain.f: Failed to allocate grid%u_btxs(sm32:em32,sm33:em33,spec_bdy_width). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%u_btxs=initial_data_value
  num_bytes_allocated = num_bytes_allocated + &
(((em32-sm32+1)*(em33-sm33+1)*(spec_bdy_width))) * 4
  ALLOCATE(grid%u_btxe(sm32:em32,sm33:em33,spec_bdy_width),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1116,&
    'frame/module_domain.f: Failed to allocate grid%u_btxe(sm32:em32,sm33:em33,spec_bdy_width). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%u_btxe=initial_data_value
  num_bytes_allocated = num_bytes_allocated + &
(((em31-sm31+1)*(em33-sm33+1)*(spec_bdy_width))) * 4
  ALLOCATE(grid%u_btys(sm31:em31,sm33:em33,spec_bdy_width),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1124,&
    'frame/module_domain.f: Failed to allocate grid%u_btys(sm31:em31,sm33:em33,spec_bdy_width). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%u_btys=initial_data_value
  num_bytes_allocated = num_bytes_allocated + &
(((em31-sm31+1)*(em33-sm33+1)*(spec_bdy_width))) * 4
  ALLOCATE(grid%u_btye(sm31:em31,sm33:em33,spec_bdy_width),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1132,&
    'frame/module_domain.f: Failed to allocate grid%u_btye(sm31:em31,sm33:em33,spec_bdy_width). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%u_btye=initial_data_value
ELSE
  ALLOCATE(grid%u_btxs(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1139,&
    'frame/module_domain.f: Failed to allocate grid%u_btxs(1,1,1).  ')
  endif
  ALLOCATE(grid%u_btxe(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1144,&
    'frame/module_domain.f: Failed to allocate grid%u_btxe(1,1,1).  ')
  endif
  ALLOCATE(grid%u_btys(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1149,&
    'frame/module_domain.f: Failed to allocate grid%u_btys(1,1,1).  ')
  endif
  ALLOCATE(grid%u_btye(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1154,&
    'frame/module_domain.f: Failed to allocate grid%u_btye(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'v').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%v(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1163,&
    'frame/module_domain.f: Failed to allocate grid%v(sm31:em31,sm32:em32,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%v=initial_data_value
ELSE
  ALLOCATE(grid%v(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1170,&
    'frame/module_domain.f: Failed to allocate grid%v(1,1,1).  ')
  endif
ENDIF
IF(.TRUE..AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
(((em32-sm32+1)*(em33-sm33+1)*(spec_bdy_width))) * 4
  ALLOCATE(grid%v_bxs(sm32:em32,sm33:em33,spec_bdy_width),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1179,&
    'frame/module_domain.f: Failed to allocate grid%v_bxs(sm32:em32,sm33:em33,spec_bdy_width). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%v_bxs=initial_data_value
  num_bytes_allocated = num_bytes_allocated + &
(((em32-sm32+1)*(em33-sm33+1)*(spec_bdy_width))) * 4
  ALLOCATE(grid%v_bxe(sm32:em32,sm33:em33,spec_bdy_width),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1187,&
    'frame/module_domain.f: Failed to allocate grid%v_bxe(sm32:em32,sm33:em33,spec_bdy_width). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%v_bxe=initial_data_value
  num_bytes_allocated = num_bytes_allocated + &
(((em31-sm31+1)*(em33-sm33+1)*(spec_bdy_width))) * 4
  ALLOCATE(grid%v_bys(sm31:em31,sm33:em33,spec_bdy_width),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1195,&
    'frame/module_domain.f: Failed to allocate grid%v_bys(sm31:em31,sm33:em33,spec_bdy_width). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%v_bys=initial_data_value
  num_bytes_allocated = num_bytes_allocated + &
(((em31-sm31+1)*(em33-sm33+1)*(spec_bdy_width))) * 4
  ALLOCATE(grid%v_bye(sm31:em31,sm33:em33,spec_bdy_width),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1203,&
    'frame/module_domain.f: Failed to allocate grid%v_bye(sm31:em31,sm33:em33,spec_bdy_width). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%v_bye=initial_data_value
ELSE
  ALLOCATE(grid%v_bxs(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1210,&
    'frame/module_domain.f: Failed to allocate grid%v_bxs(1,1,1).  ')
  endif
  ALLOCATE(grid%v_bxe(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1215,&
    'frame/module_domain.f: Failed to allocate grid%v_bxe(1,1,1).  ')
  endif
  ALLOCATE(grid%v_bys(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1220,&
    'frame/module_domain.f: Failed to allocate grid%v_bys(1,1,1).  ')
  endif
  ALLOCATE(grid%v_bye(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1225,&
    'frame/module_domain.f: Failed to allocate grid%v_bye(1,1,1).  ')
  endif
ENDIF
IF(.TRUE..AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
(((em32-sm32+1)*(em33-sm33+1)*(spec_bdy_width))) * 4
  ALLOCATE(grid%v_btxs(sm32:em32,sm33:em33,spec_bdy_width),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1234,&
    'frame/module_domain.f: Failed to allocate grid%v_btxs(sm32:em32,sm33:em33,spec_bdy_width). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%v_btxs=initial_data_value
  num_bytes_allocated = num_bytes_allocated + &
(((em32-sm32+1)*(em33-sm33+1)*(spec_bdy_width))) * 4
  ALLOCATE(grid%v_btxe(sm32:em32,sm33:em33,spec_bdy_width),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1242,&
    'frame/module_domain.f: Failed to allocate grid%v_btxe(sm32:em32,sm33:em33,spec_bdy_width). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%v_btxe=initial_data_value
  num_bytes_allocated = num_bytes_allocated + &
(((em31-sm31+1)*(em33-sm33+1)*(spec_bdy_width))) * 4
  ALLOCATE(grid%v_btys(sm31:em31,sm33:em33,spec_bdy_width),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1250,&
    'frame/module_domain.f: Failed to allocate grid%v_btys(sm31:em31,sm33:em33,spec_bdy_width). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%v_btys=initial_data_value
  num_bytes_allocated = num_bytes_allocated + &
(((em31-sm31+1)*(em33-sm33+1)*(spec_bdy_width))) * 4
  ALLOCATE(grid%v_btye(sm31:em31,sm33:em33,spec_bdy_width),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1258,&
    'frame/module_domain.f: Failed to allocate grid%v_btye(sm31:em31,sm33:em33,spec_bdy_width). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%v_btye=initial_data_value
ELSE
  ALLOCATE(grid%v_btxs(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1265,&
    'frame/module_domain.f: Failed to allocate grid%v_btxs(1,1,1).  ')
  endif
  ALLOCATE(grid%v_btxe(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1270,&
    'frame/module_domain.f: Failed to allocate grid%v_btxe(1,1,1).  ')
  endif
  ALLOCATE(grid%v_btys(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1275,&
    'frame/module_domain.f: Failed to allocate grid%v_btys(1,1,1).  ')
  endif
  ALLOCATE(grid%v_btye(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1280,&
    'frame/module_domain.f: Failed to allocate grid%v_btye(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'told').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%told(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1289,&
    'frame/module_domain.f: Failed to allocate grid%told(sm31:em31,sm32:em32,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%told=initial_data_value
ELSE
  ALLOCATE(grid%told(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1296,&
    'frame/module_domain.f: Failed to allocate grid%told(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'uold').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%uold(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1305,&
    'frame/module_domain.f: Failed to allocate grid%uold(sm31:em31,sm32:em32,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%uold=initial_data_value
ELSE
  ALLOCATE(grid%uold(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1312,&
    'frame/module_domain.f: Failed to allocate grid%uold(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'vold').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%vold(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1321,&
    'frame/module_domain.f: Failed to allocate grid%vold(sm31:em31,sm32:em32,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%vold=initial_data_value
ELSE
  ALLOCATE(grid%vold(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1328,&
    'frame/module_domain.f: Failed to allocate grid%vold(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'hcoeff').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((model_config_rec%dfi_time_dim)-(1)+1))) * 4
  ALLOCATE(grid%hcoeff(1:model_config_rec%dfi_time_dim),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1337,&
    'frame/module_domain.f: Failed to allocate grid%hcoeff(1:model_config_rec%dfi_time_dim). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%hcoeff=initial_data_value
ELSE
  ALLOCATE(grid%hcoeff(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1344,&
    'frame/module_domain.f: Failed to allocate grid%hcoeff(1).  ')
  endif
ENDIF
IF ( setinitval .EQ. 3 ) grid%hcoeff_tot=initial_data_value
IF(in_use_for_config(id,'dfi_pd').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%dfi_pd(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1354,&
    'frame/module_domain.f: Failed to allocate grid%dfi_pd(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%dfi_pd=initial_data_value
ELSE
  ALLOCATE(grid%dfi_pd(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1361,&
    'frame/module_domain.f: Failed to allocate grid%dfi_pd(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'dfi_pint').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%dfi_pint(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1370,&
    'frame/module_domain.f: Failed to allocate grid%dfi_pint(sm31:em31,sm32:em32,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%dfi_pint=initial_data_value
ELSE
  ALLOCATE(grid%dfi_pint(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1377,&
    'frame/module_domain.f: Failed to allocate grid%dfi_pint(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'dfi_dwdt').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%dfi_dwdt(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1386,&
    'frame/module_domain.f: Failed to allocate grid%dfi_dwdt(sm31:em31,sm32:em32,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%dfi_dwdt=initial_data_value
ELSE
  ALLOCATE(grid%dfi_dwdt(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1393,&
    'frame/module_domain.f: Failed to allocate grid%dfi_dwdt(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'dfi_t').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%dfi_t(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1402,&
    'frame/module_domain.f: Failed to allocate grid%dfi_t(sm31:em31,sm32:em32,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%dfi_t=initial_data_value
ELSE
  ALLOCATE(grid%dfi_t(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1409,&
    'frame/module_domain.f: Failed to allocate grid%dfi_t(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'dfi_q').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%dfi_q(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1418,&
    'frame/module_domain.f: Failed to allocate grid%dfi_q(sm31:em31,sm32:em32,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%dfi_q=initial_data_value
ELSE
  ALLOCATE(grid%dfi_q(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1425,&
    'frame/module_domain.f: Failed to allocate grid%dfi_q(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'dfi_u').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%dfi_u(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1434,&
    'frame/module_domain.f: Failed to allocate grid%dfi_u(sm31:em31,sm32:em32,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%dfi_u=initial_data_value
ELSE
  ALLOCATE(grid%dfi_u(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1441,&
    'frame/module_domain.f: Failed to allocate grid%dfi_u(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'dfi_v').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%dfi_v(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1450,&
    'frame/module_domain.f: Failed to allocate grid%dfi_v(sm31:em31,sm32:em32,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%dfi_v=initial_data_value
ELSE
  ALLOCATE(grid%dfi_v(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1457,&
    'frame/module_domain.f: Failed to allocate grid%dfi_v(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'dfi_q2').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%dfi_q2(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1466,&
    'frame/module_domain.f: Failed to allocate grid%dfi_q2(sm31:em31,sm32:em32,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%dfi_q2=initial_data_value
ELSE
  ALLOCATE(grid%dfi_q2(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1473,&
    'frame/module_domain.f: Failed to allocate grid%dfi_q2(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'dfi_cwm').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%dfi_cwm(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1482,&
    'frame/module_domain.f: Failed to allocate grid%dfi_cwm(sm31:em31,sm32:em32,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%dfi_cwm=initial_data_value
ELSE
  ALLOCATE(grid%dfi_cwm(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1489,&
    'frame/module_domain.f: Failed to allocate grid%dfi_cwm(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'dfi_rrw').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%dfi_rrw(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1498,&
    'frame/module_domain.f: Failed to allocate grid%dfi_rrw(sm31:em31,sm32:em32,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%dfi_rrw=initial_data_value
ELSE
  ALLOCATE(grid%dfi_rrw(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1505,&
    'frame/module_domain.f: Failed to allocate grid%dfi_rrw(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'dfi_stc').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((model_config_rec%num_soil_layers)-(1)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%dfi_stc(sm31:em31,1:model_config_rec%num_soil_layers,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1514,&
    'frame/module_domain.f: Failed to allocate grid%dfi_stc(sm31:em31,1:model_config_rec%num_soil_layers,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%dfi_stc=initial_data_value
ELSE
  ALLOCATE(grid%dfi_stc(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1521,&
    'frame/module_domain.f: Failed to allocate grid%dfi_stc(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'dfi_smc').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((model_config_rec%num_soil_layers)-(1)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%dfi_smc(sm31:em31,1:model_config_rec%num_soil_layers,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1530,&
    'frame/module_domain.f: Failed to allocate grid%dfi_smc(sm31:em31,1:model_config_rec%num_soil_layers,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%dfi_smc=initial_data_value
ELSE
  ALLOCATE(grid%dfi_smc(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1537,&
    'frame/module_domain.f: Failed to allocate grid%dfi_smc(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'dfi_sh2o').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((model_config_rec%num_soil_layers)-(1)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%dfi_sh2o(sm31:em31,1:model_config_rec%num_soil_layers,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1546,&
    'frame/module_domain.f: Failed to allocate grid%dfi_sh2o(sm31:em31,1:model_config_rec%num_soil_layers,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%dfi_sh2o=initial_data_value
ELSE
  ALLOCATE(grid%dfi_sh2o(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1553,&
    'frame/module_domain.f: Failed to allocate grid%dfi_sh2o(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'dfi_snow').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%dfi_snow(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1562,&
    'frame/module_domain.f: Failed to allocate grid%dfi_snow(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%dfi_snow=initial_data_value
ELSE
  ALLOCATE(grid%dfi_snow(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1569,&
    'frame/module_domain.f: Failed to allocate grid%dfi_snow(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'dfi_snowh').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%dfi_snowh(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1578,&
    'frame/module_domain.f: Failed to allocate grid%dfi_snowh(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%dfi_snowh=initial_data_value
ELSE
  ALLOCATE(grid%dfi_snowh(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1585,&
    'frame/module_domain.f: Failed to allocate grid%dfi_snowh(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'dfi_canwat').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%dfi_canwat(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1594,&
    'frame/module_domain.f: Failed to allocate grid%dfi_canwat(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%dfi_canwat=initial_data_value
ELSE
  ALLOCATE(grid%dfi_canwat(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1601,&
    'frame/module_domain.f: Failed to allocate grid%dfi_canwat(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'dfi_nmm_tsk').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%dfi_nmm_tsk(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1610,&
    'frame/module_domain.f: Failed to allocate grid%dfi_nmm_tsk(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%dfi_nmm_tsk=initial_data_value
ELSE
  ALLOCATE(grid%dfi_nmm_tsk(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1617,&
    'frame/module_domain.f: Failed to allocate grid%dfi_nmm_tsk(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'dfi_snowc').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%dfi_snowc(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1626,&
    'frame/module_domain.f: Failed to allocate grid%dfi_snowc(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%dfi_snowc=initial_data_value
ELSE
  ALLOCATE(grid%dfi_snowc(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1633,&
    'frame/module_domain.f: Failed to allocate grid%dfi_snowc(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'dx_nmm').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%dx_nmm(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1642,&
    'frame/module_domain.f: Failed to allocate grid%dx_nmm(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%dx_nmm=initial_data_value
ELSE
  ALLOCATE(grid%dx_nmm(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1649,&
    'frame/module_domain.f: Failed to allocate grid%dx_nmm(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'wpdar').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%wpdar(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1658,&
    'frame/module_domain.f: Failed to allocate grid%wpdar(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%wpdar=initial_data_value
ELSE
  ALLOCATE(grid%wpdar(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1665,&
    'frame/module_domain.f: Failed to allocate grid%wpdar(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'cpgfu').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%cpgfu(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1674,&
    'frame/module_domain.f: Failed to allocate grid%cpgfu(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%cpgfu=initial_data_value
ELSE
  ALLOCATE(grid%cpgfu(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1681,&
    'frame/module_domain.f: Failed to allocate grid%cpgfu(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'curv').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%curv(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1690,&
    'frame/module_domain.f: Failed to allocate grid%curv(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%curv=initial_data_value
ELSE
  ALLOCATE(grid%curv(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1697,&
    'frame/module_domain.f: Failed to allocate grid%curv(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'fcp').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%fcp(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1706,&
    'frame/module_domain.f: Failed to allocate grid%fcp(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%fcp=initial_data_value
ELSE
  ALLOCATE(grid%fcp(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1713,&
    'frame/module_domain.f: Failed to allocate grid%fcp(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'fdiv').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%fdiv(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1722,&
    'frame/module_domain.f: Failed to allocate grid%fdiv(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%fdiv=initial_data_value
ELSE
  ALLOCATE(grid%fdiv(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1729,&
    'frame/module_domain.f: Failed to allocate grid%fdiv(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'f').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%f(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1738,&
    'frame/module_domain.f: Failed to allocate grid%f(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%f=initial_data_value
ELSE
  ALLOCATE(grid%f(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1745,&
    'frame/module_domain.f: Failed to allocate grid%f(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'fad').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%fad(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1754,&
    'frame/module_domain.f: Failed to allocate grid%fad(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%fad=initial_data_value
ELSE
  ALLOCATE(grid%fad(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1761,&
    'frame/module_domain.f: Failed to allocate grid%fad(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'ddmpu').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%ddmpu(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1770,&
    'frame/module_domain.f: Failed to allocate grid%ddmpu(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ddmpu=initial_data_value
ELSE
  ALLOCATE(grid%ddmpu(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1777,&
    'frame/module_domain.f: Failed to allocate grid%ddmpu(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'ddmpv').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%ddmpv(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1786,&
    'frame/module_domain.f: Failed to allocate grid%ddmpv(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ddmpv=initial_data_value
ELSE
  ALLOCATE(grid%ddmpv(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1793,&
    'frame/module_domain.f: Failed to allocate grid%ddmpv(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'deta').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%deta(sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1802,&
    'frame/module_domain.f: Failed to allocate grid%deta(sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%deta=initial_data_value
ELSE
  ALLOCATE(grid%deta(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1809,&
    'frame/module_domain.f: Failed to allocate grid%deta(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'rdeta').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%rdeta(sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1818,&
    'frame/module_domain.f: Failed to allocate grid%rdeta(sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rdeta=initial_data_value
ELSE
  ALLOCATE(grid%rdeta(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1825,&
    'frame/module_domain.f: Failed to allocate grid%rdeta(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'aeta').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%aeta(sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1834,&
    'frame/module_domain.f: Failed to allocate grid%aeta(sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%aeta=initial_data_value
ELSE
  ALLOCATE(grid%aeta(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1841,&
    'frame/module_domain.f: Failed to allocate grid%aeta(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'f4q2').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%f4q2(sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1850,&
    'frame/module_domain.f: Failed to allocate grid%f4q2(sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%f4q2=initial_data_value
ELSE
  ALLOCATE(grid%f4q2(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1857,&
    'frame/module_domain.f: Failed to allocate grid%f4q2(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'etax').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%etax(sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1866,&
    'frame/module_domain.f: Failed to allocate grid%etax(sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%etax=initial_data_value
ELSE
  ALLOCATE(grid%etax(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1873,&
    'frame/module_domain.f: Failed to allocate grid%etax(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'dfl').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%dfl(sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1882,&
    'frame/module_domain.f: Failed to allocate grid%dfl(sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%dfl=initial_data_value
ELSE
  ALLOCATE(grid%dfl(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1889,&
    'frame/module_domain.f: Failed to allocate grid%dfl(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'deta1').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%deta1(sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1898,&
    'frame/module_domain.f: Failed to allocate grid%deta1(sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%deta1=initial_data_value
ELSE
  ALLOCATE(grid%deta1(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1905,&
    'frame/module_domain.f: Failed to allocate grid%deta1(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'aeta1').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%aeta1(sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1914,&
    'frame/module_domain.f: Failed to allocate grid%aeta1(sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%aeta1=initial_data_value
ELSE
  ALLOCATE(grid%aeta1(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1921,&
    'frame/module_domain.f: Failed to allocate grid%aeta1(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'eta1').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%eta1(sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1930,&
    'frame/module_domain.f: Failed to allocate grid%eta1(sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%eta1=initial_data_value
ELSE
  ALLOCATE(grid%eta1(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1937,&
    'frame/module_domain.f: Failed to allocate grid%eta1(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'deta2').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%deta2(sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1946,&
    'frame/module_domain.f: Failed to allocate grid%deta2(sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%deta2=initial_data_value
ELSE
  ALLOCATE(grid%deta2(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1953,&
    'frame/module_domain.f: Failed to allocate grid%deta2(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'aeta2').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%aeta2(sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1962,&
    'frame/module_domain.f: Failed to allocate grid%aeta2(sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%aeta2=initial_data_value
ELSE
  ALLOCATE(grid%aeta2(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1969,&
    'frame/module_domain.f: Failed to allocate grid%aeta2(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'eta2').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%eta2(sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1978,&
    'frame/module_domain.f: Failed to allocate grid%eta2(sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%eta2=initial_data_value
ELSE
  ALLOCATE(grid%eta2(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1985,&
    'frame/module_domain.f: Failed to allocate grid%eta2(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'em').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((2600)-(1)+1))) * 4
  ALLOCATE(grid%em(1:2600),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",1994,&
    'frame/module_domain.f: Failed to allocate grid%em(1:2600). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em=initial_data_value
ELSE
  ALLOCATE(grid%em(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2001,&
    'frame/module_domain.f: Failed to allocate grid%em(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'emt').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((2600)-(1)+1))) * 4
  ALLOCATE(grid%emt(1:2600),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2010,&
    'frame/module_domain.f: Failed to allocate grid%emt(1:2600). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%emt=initial_data_value
ELSE
  ALLOCATE(grid%emt(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2017,&
    'frame/module_domain.f: Failed to allocate grid%emt(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'adt').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%adt(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2026,&
    'frame/module_domain.f: Failed to allocate grid%adt(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%adt=initial_data_value
ELSE
  ALLOCATE(grid%adt(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2033,&
    'frame/module_domain.f: Failed to allocate grid%adt(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'adu').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%adu(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2042,&
    'frame/module_domain.f: Failed to allocate grid%adu(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%adu=initial_data_value
ELSE
  ALLOCATE(grid%adu(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2049,&
    'frame/module_domain.f: Failed to allocate grid%adu(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'adv').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%adv(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2058,&
    'frame/module_domain.f: Failed to allocate grid%adv(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%adv=initial_data_value
ELSE
  ALLOCATE(grid%adv(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2065,&
    'frame/module_domain.f: Failed to allocate grid%adv(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'em_loc').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((2600)-(1)+1))) * 4
  ALLOCATE(grid%em_loc(1:2600),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2074,&
    'frame/module_domain.f: Failed to allocate grid%em_loc(1:2600). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%em_loc=initial_data_value
ELSE
  ALLOCATE(grid%em_loc(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2081,&
    'frame/module_domain.f: Failed to allocate grid%em_loc(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'emt_loc').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((2600)-(1)+1))) * 4
  ALLOCATE(grid%emt_loc(1:2600),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2090,&
    'frame/module_domain.f: Failed to allocate grid%emt_loc(1:2600). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%emt_loc=initial_data_value
ELSE
  ALLOCATE(grid%emt_loc(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2097,&
    'frame/module_domain.f: Failed to allocate grid%emt_loc(1).  ')
  endif
ENDIF
IF ( setinitval .EQ. 3 ) grid%dy_nmm=initial_data_value
IF ( setinitval .EQ. 3 ) grid%cpgfv=initial_data_value
IF ( setinitval .EQ. 3 ) grid%en=initial_data_value
IF ( setinitval .EQ. 3 ) grid%ent=initial_data_value
IF ( setinitval .EQ. 3 ) grid%f4d=initial_data_value
IF ( setinitval .EQ. 3 ) grid%f4q=initial_data_value
IF ( setinitval .EQ. 3 ) grid%ef4t=initial_data_value
IF ( setinitval .EQ. 3 ) grid%upstrm=.FALSE.
IF ( setinitval .EQ. 3 ) grid%dlmd=initial_data_value
IF ( setinitval .EQ. 3 ) grid%dphd=initial_data_value
IF ( setinitval .EQ. 3 ) grid%pdtop=initial_data_value
IF ( setinitval .EQ. 3 ) grid%pt=initial_data_value
IF(in_use_for_config(id,'pdsl').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%pdsl(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2118,&
    'frame/module_domain.f: Failed to allocate grid%pdsl(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%pdsl=initial_data_value
ELSE
  ALLOCATE(grid%pdsl(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2125,&
    'frame/module_domain.f: Failed to allocate grid%pdsl(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'pdslo').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%pdslo(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2134,&
    'frame/module_domain.f: Failed to allocate grid%pdslo(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%pdslo=initial_data_value
ELSE
  ALLOCATE(grid%pdslo(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2141,&
    'frame/module_domain.f: Failed to allocate grid%pdslo(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'psdt').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%psdt(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2150,&
    'frame/module_domain.f: Failed to allocate grid%psdt(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%psdt=initial_data_value
ELSE
  ALLOCATE(grid%psdt(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2157,&
    'frame/module_domain.f: Failed to allocate grid%psdt(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'div').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%div(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2166,&
    'frame/module_domain.f: Failed to allocate grid%div(sm31:em31,sm32:em32,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%div=initial_data_value
ELSE
  ALLOCATE(grid%div(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2173,&
    'frame/module_domain.f: Failed to allocate grid%div(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'few').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%few(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2182,&
    'frame/module_domain.f: Failed to allocate grid%few(sm31:em31,sm32:em32,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%few=initial_data_value
ELSE
  ALLOCATE(grid%few(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2189,&
    'frame/module_domain.f: Failed to allocate grid%few(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'fne').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%fne(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2198,&
    'frame/module_domain.f: Failed to allocate grid%fne(sm31:em31,sm32:em32,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%fne=initial_data_value
ELSE
  ALLOCATE(grid%fne(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2205,&
    'frame/module_domain.f: Failed to allocate grid%fne(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'fns').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%fns(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2214,&
    'frame/module_domain.f: Failed to allocate grid%fns(sm31:em31,sm32:em32,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%fns=initial_data_value
ELSE
  ALLOCATE(grid%fns(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2221,&
    'frame/module_domain.f: Failed to allocate grid%fns(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'fse').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%fse(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2230,&
    'frame/module_domain.f: Failed to allocate grid%fse(sm31:em31,sm32:em32,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%fse=initial_data_value
ELSE
  ALLOCATE(grid%fse(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2237,&
    'frame/module_domain.f: Failed to allocate grid%fse(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'omgalf').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%omgalf(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2246,&
    'frame/module_domain.f: Failed to allocate grid%omgalf(sm31:em31,sm32:em32,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%omgalf=initial_data_value
ELSE
  ALLOCATE(grid%omgalf(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2253,&
    'frame/module_domain.f: Failed to allocate grid%omgalf(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'petdt').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%petdt(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2262,&
    'frame/module_domain.f: Failed to allocate grid%petdt(sm31:em31,sm32:em32,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%petdt=initial_data_value
ELSE
  ALLOCATE(grid%petdt(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2269,&
    'frame/module_domain.f: Failed to allocate grid%petdt(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'rtop').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%rtop(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2278,&
    'frame/module_domain.f: Failed to allocate grid%rtop(sm31:em31,sm32:em32,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rtop=initial_data_value
ELSE
  ALLOCATE(grid%rtop(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2285,&
    'frame/module_domain.f: Failed to allocate grid%rtop(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'pblh').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%pblh(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2294,&
    'frame/module_domain.f: Failed to allocate grid%pblh(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%pblh=initial_data_value
ELSE
  ALLOCATE(grid%pblh(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2301,&
    'frame/module_domain.f: Failed to allocate grid%pblh(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'lpbl').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%lpbl(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2310,&
    'frame/module_domain.f: Failed to allocate grid%lpbl(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%lpbl=0
ELSE
  ALLOCATE(grid%lpbl(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2317,&
    'frame/module_domain.f: Failed to allocate grid%lpbl(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'mixht').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%mixht(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2326,&
    'frame/module_domain.f: Failed to allocate grid%mixht(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%mixht=initial_data_value
ELSE
  ALLOCATE(grid%mixht(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2333,&
    'frame/module_domain.f: Failed to allocate grid%mixht(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'ustar').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%ustar(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2342,&
    'frame/module_domain.f: Failed to allocate grid%ustar(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ustar=initial_data_value
ELSE
  ALLOCATE(grid%ustar(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2349,&
    'frame/module_domain.f: Failed to allocate grid%ustar(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'z0').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%z0(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2358,&
    'frame/module_domain.f: Failed to allocate grid%z0(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%z0=initial_data_value
ELSE
  ALLOCATE(grid%z0(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2365,&
    'frame/module_domain.f: Failed to allocate grid%z0(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'z0base').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%z0base(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2374,&
    'frame/module_domain.f: Failed to allocate grid%z0base(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%z0base=initial_data_value
ELSE
  ALLOCATE(grid%z0base(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2381,&
    'frame/module_domain.f: Failed to allocate grid%z0base(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'ths').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%ths(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2390,&
    'frame/module_domain.f: Failed to allocate grid%ths(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ths=initial_data_value
ELSE
  ALLOCATE(grid%ths(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2397,&
    'frame/module_domain.f: Failed to allocate grid%ths(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'mavail').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%mavail(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2406,&
    'frame/module_domain.f: Failed to allocate grid%mavail(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%mavail=initial_data_value
ELSE
  ALLOCATE(grid%mavail(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2413,&
    'frame/module_domain.f: Failed to allocate grid%mavail(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'qsh').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%qsh(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2422,&
    'frame/module_domain.f: Failed to allocate grid%qsh(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%qsh=initial_data_value
ELSE
  ALLOCATE(grid%qsh(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2429,&
    'frame/module_domain.f: Failed to allocate grid%qsh(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'twbs').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%twbs(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2438,&
    'frame/module_domain.f: Failed to allocate grid%twbs(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%twbs=initial_data_value
ELSE
  ALLOCATE(grid%twbs(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2445,&
    'frame/module_domain.f: Failed to allocate grid%twbs(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'qwbs').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%qwbs(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2454,&
    'frame/module_domain.f: Failed to allocate grid%qwbs(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%qwbs=initial_data_value
ELSE
  ALLOCATE(grid%qwbs(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2461,&
    'frame/module_domain.f: Failed to allocate grid%qwbs(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'taux').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%taux(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2470,&
    'frame/module_domain.f: Failed to allocate grid%taux(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%taux=initial_data_value
ELSE
  ALLOCATE(grid%taux(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2477,&
    'frame/module_domain.f: Failed to allocate grid%taux(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'tauy').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%tauy(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2486,&
    'frame/module_domain.f: Failed to allocate grid%tauy(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tauy=initial_data_value
ELSE
  ALLOCATE(grid%tauy(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2493,&
    'frame/module_domain.f: Failed to allocate grid%tauy(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'prec').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%prec(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2502,&
    'frame/module_domain.f: Failed to allocate grid%prec(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%prec=initial_data_value
ELSE
  ALLOCATE(grid%prec(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2509,&
    'frame/module_domain.f: Failed to allocate grid%prec(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'aprec').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%aprec(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2518,&
    'frame/module_domain.f: Failed to allocate grid%aprec(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%aprec=initial_data_value
ELSE
  ALLOCATE(grid%aprec(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2525,&
    'frame/module_domain.f: Failed to allocate grid%aprec(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'acprec').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%acprec(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2534,&
    'frame/module_domain.f: Failed to allocate grid%acprec(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%acprec=initial_data_value
ELSE
  ALLOCATE(grid%acprec(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2541,&
    'frame/module_domain.f: Failed to allocate grid%acprec(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'cuprec').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%cuprec(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2550,&
    'frame/module_domain.f: Failed to allocate grid%cuprec(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%cuprec=initial_data_value
ELSE
  ALLOCATE(grid%cuprec(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2557,&
    'frame/module_domain.f: Failed to allocate grid%cuprec(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'lspa').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%lspa(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2566,&
    'frame/module_domain.f: Failed to allocate grid%lspa(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%lspa=initial_data_value
ELSE
  ALLOCATE(grid%lspa(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2573,&
    'frame/module_domain.f: Failed to allocate grid%lspa(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'ddata').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%ddata(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2582,&
    'frame/module_domain.f: Failed to allocate grid%ddata(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ddata=initial_data_value
ELSE
  ALLOCATE(grid%ddata(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2589,&
    'frame/module_domain.f: Failed to allocate grid%ddata(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'accliq').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%accliq(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2598,&
    'frame/module_domain.f: Failed to allocate grid%accliq(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%accliq=initial_data_value
ELSE
  ALLOCATE(grid%accliq(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2605,&
    'frame/module_domain.f: Failed to allocate grid%accliq(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'sno').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%sno(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2614,&
    'frame/module_domain.f: Failed to allocate grid%sno(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sno=initial_data_value
ELSE
  ALLOCATE(grid%sno(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2621,&
    'frame/module_domain.f: Failed to allocate grid%sno(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'si').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%si(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2630,&
    'frame/module_domain.f: Failed to allocate grid%si(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%si=initial_data_value
ELSE
  ALLOCATE(grid%si(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2637,&
    'frame/module_domain.f: Failed to allocate grid%si(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'cldefi').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%cldefi(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2646,&
    'frame/module_domain.f: Failed to allocate grid%cldefi(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%cldefi=initial_data_value
ELSE
  ALLOCATE(grid%cldefi(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2653,&
    'frame/module_domain.f: Failed to allocate grid%cldefi(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'deep').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%deep(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2662,&
    'frame/module_domain.f: Failed to allocate grid%deep(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%deep=initial_data_value
ELSE
  ALLOCATE(grid%deep(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2669,&
    'frame/module_domain.f: Failed to allocate grid%deep(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'rf').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%rf(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2678,&
    'frame/module_domain.f: Failed to allocate grid%rf(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rf=initial_data_value
ELSE
  ALLOCATE(grid%rf(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2685,&
    'frame/module_domain.f: Failed to allocate grid%rf(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'th10').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%th10(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2694,&
    'frame/module_domain.f: Failed to allocate grid%th10(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%th10=initial_data_value
ELSE
  ALLOCATE(grid%th10(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2701,&
    'frame/module_domain.f: Failed to allocate grid%th10(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'q10').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%q10(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2710,&
    'frame/module_domain.f: Failed to allocate grid%q10(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%q10=initial_data_value
ELSE
  ALLOCATE(grid%q10(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2717,&
    'frame/module_domain.f: Failed to allocate grid%q10(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'pshltr').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%pshltr(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2726,&
    'frame/module_domain.f: Failed to allocate grid%pshltr(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%pshltr=initial_data_value
ELSE
  ALLOCATE(grid%pshltr(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2733,&
    'frame/module_domain.f: Failed to allocate grid%pshltr(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'tshltr').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%tshltr(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2742,&
    'frame/module_domain.f: Failed to allocate grid%tshltr(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tshltr=initial_data_value
ELSE
  ALLOCATE(grid%tshltr(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2749,&
    'frame/module_domain.f: Failed to allocate grid%tshltr(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'qshltr').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%qshltr(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2758,&
    'frame/module_domain.f: Failed to allocate grid%qshltr(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%qshltr=initial_data_value
ELSE
  ALLOCATE(grid%qshltr(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2765,&
    'frame/module_domain.f: Failed to allocate grid%qshltr(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'q2').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%q2(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2774,&
    'frame/module_domain.f: Failed to allocate grid%q2(sm31:em31,sm32:em32,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%q2=initial_data_value
ELSE
  ALLOCATE(grid%q2(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2781,&
    'frame/module_domain.f: Failed to allocate grid%q2(1,1,1).  ')
  endif
ENDIF
IF(.TRUE..AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
(((em32-sm32+1)*(em33-sm33+1)*(spec_bdy_width))) * 4
  ALLOCATE(grid%q2_bxs(sm32:em32,sm33:em33,spec_bdy_width),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2790,&
    'frame/module_domain.f: Failed to allocate grid%q2_bxs(sm32:em32,sm33:em33,spec_bdy_width). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%q2_bxs=initial_data_value
  num_bytes_allocated = num_bytes_allocated + &
(((em32-sm32+1)*(em33-sm33+1)*(spec_bdy_width))) * 4
  ALLOCATE(grid%q2_bxe(sm32:em32,sm33:em33,spec_bdy_width),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2798,&
    'frame/module_domain.f: Failed to allocate grid%q2_bxe(sm32:em32,sm33:em33,spec_bdy_width). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%q2_bxe=initial_data_value
  num_bytes_allocated = num_bytes_allocated + &
(((em31-sm31+1)*(em33-sm33+1)*(spec_bdy_width))) * 4
  ALLOCATE(grid%q2_bys(sm31:em31,sm33:em33,spec_bdy_width),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2806,&
    'frame/module_domain.f: Failed to allocate grid%q2_bys(sm31:em31,sm33:em33,spec_bdy_width). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%q2_bys=initial_data_value
  num_bytes_allocated = num_bytes_allocated + &
(((em31-sm31+1)*(em33-sm33+1)*(spec_bdy_width))) * 4
  ALLOCATE(grid%q2_bye(sm31:em31,sm33:em33,spec_bdy_width),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2814,&
    'frame/module_domain.f: Failed to allocate grid%q2_bye(sm31:em31,sm33:em33,spec_bdy_width). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%q2_bye=initial_data_value
ELSE
  ALLOCATE(grid%q2_bxs(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2821,&
    'frame/module_domain.f: Failed to allocate grid%q2_bxs(1,1,1).  ')
  endif
  ALLOCATE(grid%q2_bxe(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2826,&
    'frame/module_domain.f: Failed to allocate grid%q2_bxe(1,1,1).  ')
  endif
  ALLOCATE(grid%q2_bys(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2831,&
    'frame/module_domain.f: Failed to allocate grid%q2_bys(1,1,1).  ')
  endif
  ALLOCATE(grid%q2_bye(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2836,&
    'frame/module_domain.f: Failed to allocate grid%q2_bye(1,1,1).  ')
  endif
ENDIF
IF(.TRUE..AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
(((em32-sm32+1)*(em33-sm33+1)*(spec_bdy_width))) * 4
  ALLOCATE(grid%q2_btxs(sm32:em32,sm33:em33,spec_bdy_width),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2845,&
    'frame/module_domain.f: Failed to allocate grid%q2_btxs(sm32:em32,sm33:em33,spec_bdy_width). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%q2_btxs=initial_data_value
  num_bytes_allocated = num_bytes_allocated + &
(((em32-sm32+1)*(em33-sm33+1)*(spec_bdy_width))) * 4
  ALLOCATE(grid%q2_btxe(sm32:em32,sm33:em33,spec_bdy_width),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2853,&
    'frame/module_domain.f: Failed to allocate grid%q2_btxe(sm32:em32,sm33:em33,spec_bdy_width). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%q2_btxe=initial_data_value
  num_bytes_allocated = num_bytes_allocated + &
(((em31-sm31+1)*(em33-sm33+1)*(spec_bdy_width))) * 4
  ALLOCATE(grid%q2_btys(sm31:em31,sm33:em33,spec_bdy_width),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2861,&
    'frame/module_domain.f: Failed to allocate grid%q2_btys(sm31:em31,sm33:em33,spec_bdy_width). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%q2_btys=initial_data_value
  num_bytes_allocated = num_bytes_allocated + &
(((em31-sm31+1)*(em33-sm33+1)*(spec_bdy_width))) * 4
  ALLOCATE(grid%q2_btye(sm31:em31,sm33:em33,spec_bdy_width),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2869,&
    'frame/module_domain.f: Failed to allocate grid%q2_btye(sm31:em31,sm33:em33,spec_bdy_width). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%q2_btye=initial_data_value
ELSE
  ALLOCATE(grid%q2_btxs(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2876,&
    'frame/module_domain.f: Failed to allocate grid%q2_btxs(1,1,1).  ')
  endif
  ALLOCATE(grid%q2_btxe(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2881,&
    'frame/module_domain.f: Failed to allocate grid%q2_btxe(1,1,1).  ')
  endif
  ALLOCATE(grid%q2_btys(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2886,&
    'frame/module_domain.f: Failed to allocate grid%q2_btys(1,1,1).  ')
  endif
  ALLOCATE(grid%q2_btye(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2891,&
    'frame/module_domain.f: Failed to allocate grid%q2_btye(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'t_adj').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%t_adj(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2900,&
    'frame/module_domain.f: Failed to allocate grid%t_adj(sm31:em31,sm32:em32,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%t_adj=initial_data_value
ELSE
  ALLOCATE(grid%t_adj(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2907,&
    'frame/module_domain.f: Failed to allocate grid%t_adj(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'t_old').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%t_old(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2916,&
    'frame/module_domain.f: Failed to allocate grid%t_old(sm31:em31,sm32:em32,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%t_old=initial_data_value
ELSE
  ALLOCATE(grid%t_old(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2923,&
    'frame/module_domain.f: Failed to allocate grid%t_old(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'zero_3d').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%zero_3d(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2932,&
    'frame/module_domain.f: Failed to allocate grid%zero_3d(sm31:em31,sm32:em32,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%zero_3d=initial_data_value
ELSE
  ALLOCATE(grid%zero_3d(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2939,&
    'frame/module_domain.f: Failed to allocate grid%zero_3d(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'w0avg').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%w0avg(sm31:em31,sm33:em33,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2948,&
    'frame/module_domain.f: Failed to allocate grid%w0avg(sm31:em31,sm33:em33,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%w0avg=initial_data_value
ELSE
  ALLOCATE(grid%w0avg(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2955,&
    'frame/module_domain.f: Failed to allocate grid%w0avg(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'akhs_out').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%akhs_out(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2964,&
    'frame/module_domain.f: Failed to allocate grid%akhs_out(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%akhs_out=initial_data_value
ELSE
  ALLOCATE(grid%akhs_out(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2971,&
    'frame/module_domain.f: Failed to allocate grid%akhs_out(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'akms_out').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%akms_out(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2980,&
    'frame/module_domain.f: Failed to allocate grid%akms_out(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%akms_out=initial_data_value
ELSE
  ALLOCATE(grid%akms_out(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2987,&
    'frame/module_domain.f: Failed to allocate grid%akms_out(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'albase').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%albase(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",2996,&
    'frame/module_domain.f: Failed to allocate grid%albase(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%albase=initial_data_value
ELSE
  ALLOCATE(grid%albase(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3003,&
    'frame/module_domain.f: Failed to allocate grid%albase(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'albedo').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%albedo(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3012,&
    'frame/module_domain.f: Failed to allocate grid%albedo(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%albedo=initial_data_value
ELSE
  ALLOCATE(grid%albedo(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3019,&
    'frame/module_domain.f: Failed to allocate grid%albedo(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'cnvbot').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%cnvbot(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3028,&
    'frame/module_domain.f: Failed to allocate grid%cnvbot(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%cnvbot=initial_data_value
ELSE
  ALLOCATE(grid%cnvbot(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3035,&
    'frame/module_domain.f: Failed to allocate grid%cnvbot(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'cnvtop').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%cnvtop(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3044,&
    'frame/module_domain.f: Failed to allocate grid%cnvtop(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%cnvtop=initial_data_value
ELSE
  ALLOCATE(grid%cnvtop(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3051,&
    'frame/module_domain.f: Failed to allocate grid%cnvtop(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'czen').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%czen(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3060,&
    'frame/module_domain.f: Failed to allocate grid%czen(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%czen=initial_data_value
ELSE
  ALLOCATE(grid%czen(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3067,&
    'frame/module_domain.f: Failed to allocate grid%czen(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'czmean').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%czmean(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3076,&
    'frame/module_domain.f: Failed to allocate grid%czmean(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%czmean=initial_data_value
ELSE
  ALLOCATE(grid%czmean(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3083,&
    'frame/module_domain.f: Failed to allocate grid%czmean(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'embck').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%embck(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3092,&
    'frame/module_domain.f: Failed to allocate grid%embck(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%embck=initial_data_value
ELSE
  ALLOCATE(grid%embck(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3099,&
    'frame/module_domain.f: Failed to allocate grid%embck(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'epsr').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%epsr(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3108,&
    'frame/module_domain.f: Failed to allocate grid%epsr(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%epsr=initial_data_value
ELSE
  ALLOCATE(grid%epsr(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3115,&
    'frame/module_domain.f: Failed to allocate grid%epsr(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'gffc').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%gffc(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3124,&
    'frame/module_domain.f: Failed to allocate grid%gffc(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%gffc=initial_data_value
ELSE
  ALLOCATE(grid%gffc(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3131,&
    'frame/module_domain.f: Failed to allocate grid%gffc(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'glat').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%glat(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3140,&
    'frame/module_domain.f: Failed to allocate grid%glat(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%glat=initial_data_value
ELSE
  ALLOCATE(grid%glat(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3147,&
    'frame/module_domain.f: Failed to allocate grid%glat(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'glon').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%glon(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3156,&
    'frame/module_domain.f: Failed to allocate grid%glon(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%glon=initial_data_value
ELSE
  ALLOCATE(grid%glon(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3163,&
    'frame/module_domain.f: Failed to allocate grid%glon(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'nmm_tsk').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%nmm_tsk(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3172,&
    'frame/module_domain.f: Failed to allocate grid%nmm_tsk(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nmm_tsk=initial_data_value
ELSE
  ALLOCATE(grid%nmm_tsk(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3179,&
    'frame/module_domain.f: Failed to allocate grid%nmm_tsk(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'hdac').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%hdac(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3188,&
    'frame/module_domain.f: Failed to allocate grid%hdac(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%hdac=initial_data_value
ELSE
  ALLOCATE(grid%hdac(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3195,&
    'frame/module_domain.f: Failed to allocate grid%hdac(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'hdacv').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%hdacv(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3204,&
    'frame/module_domain.f: Failed to allocate grid%hdacv(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%hdacv=initial_data_value
ELSE
  ALLOCATE(grid%hdacv(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3211,&
    'frame/module_domain.f: Failed to allocate grid%hdacv(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'mxsnal').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%mxsnal(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3220,&
    'frame/module_domain.f: Failed to allocate grid%mxsnal(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%mxsnal=initial_data_value
ELSE
  ALLOCATE(grid%mxsnal(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3227,&
    'frame/module_domain.f: Failed to allocate grid%mxsnal(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'radin').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%radin(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3236,&
    'frame/module_domain.f: Failed to allocate grid%radin(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%radin=initial_data_value
ELSE
  ALLOCATE(grid%radin(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3243,&
    'frame/module_domain.f: Failed to allocate grid%radin(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'radot').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%radot(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3252,&
    'frame/module_domain.f: Failed to allocate grid%radot(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%radot=initial_data_value
ELSE
  ALLOCATE(grid%radot(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3259,&
    'frame/module_domain.f: Failed to allocate grid%radot(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'sigt4').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%sigt4(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3268,&
    'frame/module_domain.f: Failed to allocate grid%sigt4(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sigt4=initial_data_value
ELSE
  ALLOCATE(grid%sigt4(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3275,&
    'frame/module_domain.f: Failed to allocate grid%sigt4(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'tg').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%tg(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3284,&
    'frame/module_domain.f: Failed to allocate grid%tg(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tg=initial_data_value
ELSE
  ALLOCATE(grid%tg(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3291,&
    'frame/module_domain.f: Failed to allocate grid%tg(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'dfrlg').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%dfrlg(sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3300,&
    'frame/module_domain.f: Failed to allocate grid%dfrlg(sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%dfrlg=initial_data_value
ELSE
  ALLOCATE(grid%dfrlg(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3307,&
    'frame/module_domain.f: Failed to allocate grid%dfrlg(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'lvl').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%lvl(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3316,&
    'frame/module_domain.f: Failed to allocate grid%lvl(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%lvl=0
ELSE
  ALLOCATE(grid%lvl(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3323,&
    'frame/module_domain.f: Failed to allocate grid%lvl(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'cwm').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%cwm(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3332,&
    'frame/module_domain.f: Failed to allocate grid%cwm(sm31:em31,sm32:em32,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%cwm=initial_data_value
ELSE
  ALLOCATE(grid%cwm(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3339,&
    'frame/module_domain.f: Failed to allocate grid%cwm(1,1,1).  ')
  endif
ENDIF
IF(.TRUE..AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
(((em32-sm32+1)*(em33-sm33+1)*(spec_bdy_width))) * 4
  ALLOCATE(grid%cwm_bxs(sm32:em32,sm33:em33,spec_bdy_width),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3348,&
    'frame/module_domain.f: Failed to allocate grid%cwm_bxs(sm32:em32,sm33:em33,spec_bdy_width). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%cwm_bxs=initial_data_value
  num_bytes_allocated = num_bytes_allocated + &
(((em32-sm32+1)*(em33-sm33+1)*(spec_bdy_width))) * 4
  ALLOCATE(grid%cwm_bxe(sm32:em32,sm33:em33,spec_bdy_width),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3356,&
    'frame/module_domain.f: Failed to allocate grid%cwm_bxe(sm32:em32,sm33:em33,spec_bdy_width). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%cwm_bxe=initial_data_value
  num_bytes_allocated = num_bytes_allocated + &
(((em31-sm31+1)*(em33-sm33+1)*(spec_bdy_width))) * 4
  ALLOCATE(grid%cwm_bys(sm31:em31,sm33:em33,spec_bdy_width),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3364,&
    'frame/module_domain.f: Failed to allocate grid%cwm_bys(sm31:em31,sm33:em33,spec_bdy_width). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%cwm_bys=initial_data_value
  num_bytes_allocated = num_bytes_allocated + &
(((em31-sm31+1)*(em33-sm33+1)*(spec_bdy_width))) * 4
  ALLOCATE(grid%cwm_bye(sm31:em31,sm33:em33,spec_bdy_width),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3372,&
    'frame/module_domain.f: Failed to allocate grid%cwm_bye(sm31:em31,sm33:em33,spec_bdy_width). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%cwm_bye=initial_data_value
ELSE
  ALLOCATE(grid%cwm_bxs(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3379,&
    'frame/module_domain.f: Failed to allocate grid%cwm_bxs(1,1,1).  ')
  endif
  ALLOCATE(grid%cwm_bxe(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3384,&
    'frame/module_domain.f: Failed to allocate grid%cwm_bxe(1,1,1).  ')
  endif
  ALLOCATE(grid%cwm_bys(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3389,&
    'frame/module_domain.f: Failed to allocate grid%cwm_bys(1,1,1).  ')
  endif
  ALLOCATE(grid%cwm_bye(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3394,&
    'frame/module_domain.f: Failed to allocate grid%cwm_bye(1,1,1).  ')
  endif
ENDIF
IF(.TRUE..AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
(((em32-sm32+1)*(em33-sm33+1)*(spec_bdy_width))) * 4
  ALLOCATE(grid%cwm_btxs(sm32:em32,sm33:em33,spec_bdy_width),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3403,&
    'frame/module_domain.f: Failed to allocate grid%cwm_btxs(sm32:em32,sm33:em33,spec_bdy_width). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%cwm_btxs=initial_data_value
  num_bytes_allocated = num_bytes_allocated + &
(((em32-sm32+1)*(em33-sm33+1)*(spec_bdy_width))) * 4
  ALLOCATE(grid%cwm_btxe(sm32:em32,sm33:em33,spec_bdy_width),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3411,&
    'frame/module_domain.f: Failed to allocate grid%cwm_btxe(sm32:em32,sm33:em33,spec_bdy_width). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%cwm_btxe=initial_data_value
  num_bytes_allocated = num_bytes_allocated + &
(((em31-sm31+1)*(em33-sm33+1)*(spec_bdy_width))) * 4
  ALLOCATE(grid%cwm_btys(sm31:em31,sm33:em33,spec_bdy_width),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3419,&
    'frame/module_domain.f: Failed to allocate grid%cwm_btys(sm31:em31,sm33:em33,spec_bdy_width). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%cwm_btys=initial_data_value
  num_bytes_allocated = num_bytes_allocated + &
(((em31-sm31+1)*(em33-sm33+1)*(spec_bdy_width))) * 4
  ALLOCATE(grid%cwm_btye(sm31:em31,sm33:em33,spec_bdy_width),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3427,&
    'frame/module_domain.f: Failed to allocate grid%cwm_btye(sm31:em31,sm33:em33,spec_bdy_width). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%cwm_btye=initial_data_value
ELSE
  ALLOCATE(grid%cwm_btxs(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3434,&
    'frame/module_domain.f: Failed to allocate grid%cwm_btxs(1,1,1).  ')
  endif
  ALLOCATE(grid%cwm_btxe(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3439,&
    'frame/module_domain.f: Failed to allocate grid%cwm_btxe(1,1,1).  ')
  endif
  ALLOCATE(grid%cwm_btys(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3444,&
    'frame/module_domain.f: Failed to allocate grid%cwm_btys(1,1,1).  ')
  endif
  ALLOCATE(grid%cwm_btye(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3449,&
    'frame/module_domain.f: Failed to allocate grid%cwm_btye(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'rrw').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%rrw(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3458,&
    'frame/module_domain.f: Failed to allocate grid%rrw(sm31:em31,sm32:em32,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rrw=initial_data_value
ELSE
  ALLOCATE(grid%rrw(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3465,&
    'frame/module_domain.f: Failed to allocate grid%rrw(1,1,1).  ')
  endif
ENDIF
IF(.TRUE..AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
(((em32-sm32+1)*(em33-sm33+1)*(spec_bdy_width))) * 4
  ALLOCATE(grid%rrw_bxs(sm32:em32,sm33:em33,spec_bdy_width),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3474,&
    'frame/module_domain.f: Failed to allocate grid%rrw_bxs(sm32:em32,sm33:em33,spec_bdy_width). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rrw_bxs=initial_data_value
  num_bytes_allocated = num_bytes_allocated + &
(((em32-sm32+1)*(em33-sm33+1)*(spec_bdy_width))) * 4
  ALLOCATE(grid%rrw_bxe(sm32:em32,sm33:em33,spec_bdy_width),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3482,&
    'frame/module_domain.f: Failed to allocate grid%rrw_bxe(sm32:em32,sm33:em33,spec_bdy_width). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rrw_bxe=initial_data_value
  num_bytes_allocated = num_bytes_allocated + &
(((em31-sm31+1)*(em33-sm33+1)*(spec_bdy_width))) * 4
  ALLOCATE(grid%rrw_bys(sm31:em31,sm33:em33,spec_bdy_width),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3490,&
    'frame/module_domain.f: Failed to allocate grid%rrw_bys(sm31:em31,sm33:em33,spec_bdy_width). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rrw_bys=initial_data_value
  num_bytes_allocated = num_bytes_allocated + &
(((em31-sm31+1)*(em33-sm33+1)*(spec_bdy_width))) * 4
  ALLOCATE(grid%rrw_bye(sm31:em31,sm33:em33,spec_bdy_width),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3498,&
    'frame/module_domain.f: Failed to allocate grid%rrw_bye(sm31:em31,sm33:em33,spec_bdy_width). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rrw_bye=initial_data_value
ELSE
  ALLOCATE(grid%rrw_bxs(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3505,&
    'frame/module_domain.f: Failed to allocate grid%rrw_bxs(1,1,1).  ')
  endif
  ALLOCATE(grid%rrw_bxe(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3510,&
    'frame/module_domain.f: Failed to allocate grid%rrw_bxe(1,1,1).  ')
  endif
  ALLOCATE(grid%rrw_bys(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3515,&
    'frame/module_domain.f: Failed to allocate grid%rrw_bys(1,1,1).  ')
  endif
  ALLOCATE(grid%rrw_bye(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3520,&
    'frame/module_domain.f: Failed to allocate grid%rrw_bye(1,1,1).  ')
  endif
ENDIF
IF(.TRUE..AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
(((em32-sm32+1)*(em33-sm33+1)*(spec_bdy_width))) * 4
  ALLOCATE(grid%rrw_btxs(sm32:em32,sm33:em33,spec_bdy_width),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3529,&
    'frame/module_domain.f: Failed to allocate grid%rrw_btxs(sm32:em32,sm33:em33,spec_bdy_width). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rrw_btxs=initial_data_value
  num_bytes_allocated = num_bytes_allocated + &
(((em32-sm32+1)*(em33-sm33+1)*(spec_bdy_width))) * 4
  ALLOCATE(grid%rrw_btxe(sm32:em32,sm33:em33,spec_bdy_width),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3537,&
    'frame/module_domain.f: Failed to allocate grid%rrw_btxe(sm32:em32,sm33:em33,spec_bdy_width). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rrw_btxe=initial_data_value
  num_bytes_allocated = num_bytes_allocated + &
(((em31-sm31+1)*(em33-sm33+1)*(spec_bdy_width))) * 4
  ALLOCATE(grid%rrw_btys(sm31:em31,sm33:em33,spec_bdy_width),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3545,&
    'frame/module_domain.f: Failed to allocate grid%rrw_btys(sm31:em31,sm33:em33,spec_bdy_width). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rrw_btys=initial_data_value
  num_bytes_allocated = num_bytes_allocated + &
(((em31-sm31+1)*(em33-sm33+1)*(spec_bdy_width))) * 4
  ALLOCATE(grid%rrw_btye(sm31:em31,sm33:em33,spec_bdy_width),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3553,&
    'frame/module_domain.f: Failed to allocate grid%rrw_btye(sm31:em31,sm33:em33,spec_bdy_width). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rrw_btye=initial_data_value
ELSE
  ALLOCATE(grid%rrw_btxs(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3560,&
    'frame/module_domain.f: Failed to allocate grid%rrw_btxs(1,1,1).  ')
  endif
  ALLOCATE(grid%rrw_btxe(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3565,&
    'frame/module_domain.f: Failed to allocate grid%rrw_btxe(1,1,1).  ')
  endif
  ALLOCATE(grid%rrw_btys(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3570,&
    'frame/module_domain.f: Failed to allocate grid%rrw_btys(1,1,1).  ')
  endif
  ALLOCATE(grid%rrw_btye(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3575,&
    'frame/module_domain.f: Failed to allocate grid%rrw_btye(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'f_ice').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%f_ice(sm31:em31,sm33:em33,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3584,&
    'frame/module_domain.f: Failed to allocate grid%f_ice(sm31:em31,sm33:em33,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%f_ice=initial_data_value
ELSE
  ALLOCATE(grid%f_ice(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3591,&
    'frame/module_domain.f: Failed to allocate grid%f_ice(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'f_rain').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%f_rain(sm31:em31,sm33:em33,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3600,&
    'frame/module_domain.f: Failed to allocate grid%f_rain(sm31:em31,sm33:em33,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%f_rain=initial_data_value
ELSE
  ALLOCATE(grid%f_rain(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3607,&
    'frame/module_domain.f: Failed to allocate grid%f_rain(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'f_rimef').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%f_rimef(sm31:em31,sm33:em33,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3616,&
    'frame/module_domain.f: Failed to allocate grid%f_rimef(sm31:em31,sm33:em33,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%f_rimef=initial_data_value
ELSE
  ALLOCATE(grid%f_rimef(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3623,&
    'frame/module_domain.f: Failed to allocate grid%f_rimef(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'cldfra').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%cldfra(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3632,&
    'frame/module_domain.f: Failed to allocate grid%cldfra(sm31:em31,sm32:em32,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%cldfra=initial_data_value
ELSE
  ALLOCATE(grid%cldfra(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3639,&
    'frame/module_domain.f: Failed to allocate grid%cldfra(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'sr').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%sr(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3648,&
    'frame/module_domain.f: Failed to allocate grid%sr(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sr=initial_data_value
ELSE
  ALLOCATE(grid%sr(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3655,&
    'frame/module_domain.f: Failed to allocate grid%sr(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'cfrach').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%cfrach(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3664,&
    'frame/module_domain.f: Failed to allocate grid%cfrach(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%cfrach=initial_data_value
ELSE
  ALLOCATE(grid%cfrach(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3671,&
    'frame/module_domain.f: Failed to allocate grid%cfrach(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'cfracl').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%cfracl(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3680,&
    'frame/module_domain.f: Failed to allocate grid%cfracl(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%cfracl=initial_data_value
ELSE
  ALLOCATE(grid%cfracl(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3687,&
    'frame/module_domain.f: Failed to allocate grid%cfracl(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'cfracm').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%cfracm(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3696,&
    'frame/module_domain.f: Failed to allocate grid%cfracm(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%cfracm=initial_data_value
ELSE
  ALLOCATE(grid%cfracm(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3703,&
    'frame/module_domain.f: Failed to allocate grid%cfracm(1,1).  ')
  endif
ENDIF
IF ( setinitval .EQ. 3 ) grid%micro_start=.FALSE.
IF(in_use_for_config(id,'islope').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%islope(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3713,&
    'frame/module_domain.f: Failed to allocate grid%islope(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%islope=0
ELSE
  ALLOCATE(grid%islope(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3720,&
    'frame/module_domain.f: Failed to allocate grid%islope(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'dzsoil').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%dzsoil(sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3729,&
    'frame/module_domain.f: Failed to allocate grid%dzsoil(sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%dzsoil=initial_data_value
ELSE
  ALLOCATE(grid%dzsoil(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3736,&
    'frame/module_domain.f: Failed to allocate grid%dzsoil(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'rtdpth').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%rtdpth(sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3745,&
    'frame/module_domain.f: Failed to allocate grid%rtdpth(sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rtdpth=initial_data_value
ELSE
  ALLOCATE(grid%rtdpth(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3752,&
    'frame/module_domain.f: Failed to allocate grid%rtdpth(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'sldpth').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%sldpth(sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3761,&
    'frame/module_domain.f: Failed to allocate grid%sldpth(sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sldpth=initial_data_value
ELSE
  ALLOCATE(grid%sldpth(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3768,&
    'frame/module_domain.f: Failed to allocate grid%sldpth(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'cmc').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%cmc(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3777,&
    'frame/module_domain.f: Failed to allocate grid%cmc(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%cmc=initial_data_value
ELSE
  ALLOCATE(grid%cmc(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3784,&
    'frame/module_domain.f: Failed to allocate grid%cmc(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'grnflx').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%grnflx(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3793,&
    'frame/module_domain.f: Failed to allocate grid%grnflx(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%grnflx=initial_data_value
ELSE
  ALLOCATE(grid%grnflx(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3800,&
    'frame/module_domain.f: Failed to allocate grid%grnflx(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'pctsno').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%pctsno(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3809,&
    'frame/module_domain.f: Failed to allocate grid%pctsno(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%pctsno=initial_data_value
ELSE
  ALLOCATE(grid%pctsno(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3816,&
    'frame/module_domain.f: Failed to allocate grid%pctsno(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'soiltb').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%soiltb(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3825,&
    'frame/module_domain.f: Failed to allocate grid%soiltb(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soiltb=initial_data_value
ELSE
  ALLOCATE(grid%soiltb(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3832,&
    'frame/module_domain.f: Failed to allocate grid%soiltb(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'vegfrc').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%vegfrc(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3841,&
    'frame/module_domain.f: Failed to allocate grid%vegfrc(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%vegfrc=initial_data_value
ELSE
  ALLOCATE(grid%vegfrc(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3848,&
    'frame/module_domain.f: Failed to allocate grid%vegfrc(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'shdmin').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%shdmin(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3857,&
    'frame/module_domain.f: Failed to allocate grid%shdmin(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%shdmin=initial_data_value
ELSE
  ALLOCATE(grid%shdmin(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3864,&
    'frame/module_domain.f: Failed to allocate grid%shdmin(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'shdmax').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%shdmax(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3873,&
    'frame/module_domain.f: Failed to allocate grid%shdmax(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%shdmax=initial_data_value
ELSE
  ALLOCATE(grid%shdmax(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3880,&
    'frame/module_domain.f: Failed to allocate grid%shdmax(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'sh2o').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((model_config_rec%num_soil_layers)-(1)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%sh2o(sm31:em31,1:model_config_rec%num_soil_layers,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3889,&
    'frame/module_domain.f: Failed to allocate grid%sh2o(sm31:em31,1:model_config_rec%num_soil_layers,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sh2o=initial_data_value
ELSE
  ALLOCATE(grid%sh2o(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3896,&
    'frame/module_domain.f: Failed to allocate grid%sh2o(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'smc').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((model_config_rec%num_soil_layers)-(1)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%smc(sm31:em31,1:model_config_rec%num_soil_layers,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3905,&
    'frame/module_domain.f: Failed to allocate grid%smc(sm31:em31,1:model_config_rec%num_soil_layers,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%smc=initial_data_value
ELSE
  ALLOCATE(grid%smc(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3912,&
    'frame/module_domain.f: Failed to allocate grid%smc(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'stc').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((model_config_rec%num_soil_layers)-(1)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%stc(sm31:em31,1:model_config_rec%num_soil_layers,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3921,&
    'frame/module_domain.f: Failed to allocate grid%stc(sm31:em31,1:model_config_rec%num_soil_layers,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%stc=initial_data_value
ELSE
  ALLOCATE(grid%stc(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3928,&
    'frame/module_domain.f: Failed to allocate grid%stc(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'hstdv').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%hstdv(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3937,&
    'frame/module_domain.f: Failed to allocate grid%hstdv(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%hstdv=initial_data_value
ELSE
  ALLOCATE(grid%hstdv(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3944,&
    'frame/module_domain.f: Failed to allocate grid%hstdv(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'hcnvx').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%hcnvx(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3953,&
    'frame/module_domain.f: Failed to allocate grid%hcnvx(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%hcnvx=initial_data_value
ELSE
  ALLOCATE(grid%hcnvx(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3960,&
    'frame/module_domain.f: Failed to allocate grid%hcnvx(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'hasyw').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%hasyw(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3969,&
    'frame/module_domain.f: Failed to allocate grid%hasyw(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%hasyw=initial_data_value
ELSE
  ALLOCATE(grid%hasyw(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3976,&
    'frame/module_domain.f: Failed to allocate grid%hasyw(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'hasys').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%hasys(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3985,&
    'frame/module_domain.f: Failed to allocate grid%hasys(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%hasys=initial_data_value
ELSE
  ALLOCATE(grid%hasys(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",3992,&
    'frame/module_domain.f: Failed to allocate grid%hasys(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'hasysw').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%hasysw(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4001,&
    'frame/module_domain.f: Failed to allocate grid%hasysw(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%hasysw=initial_data_value
ELSE
  ALLOCATE(grid%hasysw(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4008,&
    'frame/module_domain.f: Failed to allocate grid%hasysw(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'hasynw').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%hasynw(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4017,&
    'frame/module_domain.f: Failed to allocate grid%hasynw(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%hasynw=initial_data_value
ELSE
  ALLOCATE(grid%hasynw(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4024,&
    'frame/module_domain.f: Failed to allocate grid%hasynw(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'hlenw').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%hlenw(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4033,&
    'frame/module_domain.f: Failed to allocate grid%hlenw(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%hlenw=initial_data_value
ELSE
  ALLOCATE(grid%hlenw(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4040,&
    'frame/module_domain.f: Failed to allocate grid%hlenw(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'hlens').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%hlens(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4049,&
    'frame/module_domain.f: Failed to allocate grid%hlens(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%hlens=initial_data_value
ELSE
  ALLOCATE(grid%hlens(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4056,&
    'frame/module_domain.f: Failed to allocate grid%hlens(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'hlensw').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%hlensw(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4065,&
    'frame/module_domain.f: Failed to allocate grid%hlensw(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%hlensw=initial_data_value
ELSE
  ALLOCATE(grid%hlensw(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4072,&
    'frame/module_domain.f: Failed to allocate grid%hlensw(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'hlennw').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%hlennw(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4081,&
    'frame/module_domain.f: Failed to allocate grid%hlennw(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%hlennw=initial_data_value
ELSE
  ALLOCATE(grid%hlennw(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4088,&
    'frame/module_domain.f: Failed to allocate grid%hlennw(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'hangl').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%hangl(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4097,&
    'frame/module_domain.f: Failed to allocate grid%hangl(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%hangl=initial_data_value
ELSE
  ALLOCATE(grid%hangl(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4104,&
    'frame/module_domain.f: Failed to allocate grid%hangl(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'hanis').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%hanis(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4113,&
    'frame/module_domain.f: Failed to allocate grid%hanis(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%hanis=initial_data_value
ELSE
  ALLOCATE(grid%hanis(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4120,&
    'frame/module_domain.f: Failed to allocate grid%hanis(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'hslop').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%hslop(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4129,&
    'frame/module_domain.f: Failed to allocate grid%hslop(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%hslop=initial_data_value
ELSE
  ALLOCATE(grid%hslop(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4136,&
    'frame/module_domain.f: Failed to allocate grid%hslop(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'hzmax').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%hzmax(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4145,&
    'frame/module_domain.f: Failed to allocate grid%hzmax(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%hzmax=initial_data_value
ELSE
  ALLOCATE(grid%hzmax(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4152,&
    'frame/module_domain.f: Failed to allocate grid%hzmax(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'crot').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%crot(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4161,&
    'frame/module_domain.f: Failed to allocate grid%crot(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%crot=initial_data_value
ELSE
  ALLOCATE(grid%crot(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4168,&
    'frame/module_domain.f: Failed to allocate grid%crot(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'srot').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%srot(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4177,&
    'frame/module_domain.f: Failed to allocate grid%srot(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%srot=initial_data_value
ELSE
  ALLOCATE(grid%srot(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4184,&
    'frame/module_domain.f: Failed to allocate grid%srot(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'ugwdsfc').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%ugwdsfc(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4193,&
    'frame/module_domain.f: Failed to allocate grid%ugwdsfc(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ugwdsfc=initial_data_value
ELSE
  ALLOCATE(grid%ugwdsfc(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4200,&
    'frame/module_domain.f: Failed to allocate grid%ugwdsfc(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'vgwdsfc').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%vgwdsfc(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4209,&
    'frame/module_domain.f: Failed to allocate grid%vgwdsfc(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%vgwdsfc=initial_data_value
ELSE
  ALLOCATE(grid%vgwdsfc(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4216,&
    'frame/module_domain.f: Failed to allocate grid%vgwdsfc(1,1).  ')
  endif
ENDIF
IF ( setinitval .EQ. 3 ) grid%hydro=.FALSE.
IF(in_use_for_config(id,'dwdtmn').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%dwdtmn(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4226,&
    'frame/module_domain.f: Failed to allocate grid%dwdtmn(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%dwdtmn=initial_data_value
ELSE
  ALLOCATE(grid%dwdtmn(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4233,&
    'frame/module_domain.f: Failed to allocate grid%dwdtmn(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'dwdtmx').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%dwdtmx(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4242,&
    'frame/module_domain.f: Failed to allocate grid%dwdtmx(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%dwdtmx=initial_data_value
ELSE
  ALLOCATE(grid%dwdtmx(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4249,&
    'frame/module_domain.f: Failed to allocate grid%dwdtmx(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'dwdt').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%dwdt(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4258,&
    'frame/module_domain.f: Failed to allocate grid%dwdt(sm31:em31,sm32:em32,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%dwdt=initial_data_value
ELSE
  ALLOCATE(grid%dwdt(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4265,&
    'frame/module_domain.f: Failed to allocate grid%dwdt(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'pdwdt').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%pdwdt(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4274,&
    'frame/module_domain.f: Failed to allocate grid%pdwdt(sm31:em31,sm32:em32,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%pdwdt=initial_data_value
ELSE
  ALLOCATE(grid%pdwdt(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4281,&
    'frame/module_domain.f: Failed to allocate grid%pdwdt(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'pint').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%pint(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4290,&
    'frame/module_domain.f: Failed to allocate grid%pint(sm31:em31,sm32:em32,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%pint=initial_data_value
ELSE
  ALLOCATE(grid%pint(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4297,&
    'frame/module_domain.f: Failed to allocate grid%pint(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'w').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%w(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4306,&
    'frame/module_domain.f: Failed to allocate grid%w(sm31:em31,sm32:em32,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%w=initial_data_value
ELSE
  ALLOCATE(grid%w(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4313,&
    'frame/module_domain.f: Failed to allocate grid%w(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'z').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%z(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4322,&
    'frame/module_domain.f: Failed to allocate grid%z(sm31:em31,sm32:em32,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%z=initial_data_value
ELSE
  ALLOCATE(grid%z(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4329,&
    'frame/module_domain.f: Failed to allocate grid%z(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'acfrcv').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%acfrcv(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4338,&
    'frame/module_domain.f: Failed to allocate grid%acfrcv(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%acfrcv=initial_data_value
ELSE
  ALLOCATE(grid%acfrcv(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4345,&
    'frame/module_domain.f: Failed to allocate grid%acfrcv(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'acfrst').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%acfrst(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4354,&
    'frame/module_domain.f: Failed to allocate grid%acfrst(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%acfrst=initial_data_value
ELSE
  ALLOCATE(grid%acfrst(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4361,&
    'frame/module_domain.f: Failed to allocate grid%acfrst(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'ssroff').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%ssroff(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4370,&
    'frame/module_domain.f: Failed to allocate grid%ssroff(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ssroff=initial_data_value
ELSE
  ALLOCATE(grid%ssroff(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4377,&
    'frame/module_domain.f: Failed to allocate grid%ssroff(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'bgroff').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%bgroff(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4386,&
    'frame/module_domain.f: Failed to allocate grid%bgroff(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%bgroff=initial_data_value
ELSE
  ALLOCATE(grid%bgroff(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4393,&
    'frame/module_domain.f: Failed to allocate grid%bgroff(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'rlwin').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%rlwin(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4402,&
    'frame/module_domain.f: Failed to allocate grid%rlwin(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rlwin=initial_data_value
ELSE
  ALLOCATE(grid%rlwin(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4409,&
    'frame/module_domain.f: Failed to allocate grid%rlwin(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'rlwout').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%rlwout(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4418,&
    'frame/module_domain.f: Failed to allocate grid%rlwout(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rlwout=initial_data_value
ELSE
  ALLOCATE(grid%rlwout(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4425,&
    'frame/module_domain.f: Failed to allocate grid%rlwout(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'rlwtoa').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%rlwtoa(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4434,&
    'frame/module_domain.f: Failed to allocate grid%rlwtoa(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rlwtoa=initial_data_value
ELSE
  ALLOCATE(grid%rlwtoa(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4441,&
    'frame/module_domain.f: Failed to allocate grid%rlwtoa(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'alwin').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%alwin(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4450,&
    'frame/module_domain.f: Failed to allocate grid%alwin(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%alwin=initial_data_value
ELSE
  ALLOCATE(grid%alwin(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4457,&
    'frame/module_domain.f: Failed to allocate grid%alwin(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'alwout').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%alwout(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4466,&
    'frame/module_domain.f: Failed to allocate grid%alwout(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%alwout=initial_data_value
ELSE
  ALLOCATE(grid%alwout(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4473,&
    'frame/module_domain.f: Failed to allocate grid%alwout(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'alwtoa').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%alwtoa(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4482,&
    'frame/module_domain.f: Failed to allocate grid%alwtoa(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%alwtoa=initial_data_value
ELSE
  ALLOCATE(grid%alwtoa(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4489,&
    'frame/module_domain.f: Failed to allocate grid%alwtoa(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'rswin').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%rswin(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4498,&
    'frame/module_domain.f: Failed to allocate grid%rswin(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rswin=initial_data_value
ELSE
  ALLOCATE(grid%rswin(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4505,&
    'frame/module_domain.f: Failed to allocate grid%rswin(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'rswinc').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%rswinc(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4514,&
    'frame/module_domain.f: Failed to allocate grid%rswinc(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rswinc=initial_data_value
ELSE
  ALLOCATE(grid%rswinc(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4521,&
    'frame/module_domain.f: Failed to allocate grid%rswinc(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'rswout').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%rswout(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4530,&
    'frame/module_domain.f: Failed to allocate grid%rswout(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rswout=initial_data_value
ELSE
  ALLOCATE(grid%rswout(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4537,&
    'frame/module_domain.f: Failed to allocate grid%rswout(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'rswtoa').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%rswtoa(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4546,&
    'frame/module_domain.f: Failed to allocate grid%rswtoa(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rswtoa=initial_data_value
ELSE
  ALLOCATE(grid%rswtoa(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4553,&
    'frame/module_domain.f: Failed to allocate grid%rswtoa(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'aswin').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%aswin(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4562,&
    'frame/module_domain.f: Failed to allocate grid%aswin(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%aswin=initial_data_value
ELSE
  ALLOCATE(grid%aswin(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4569,&
    'frame/module_domain.f: Failed to allocate grid%aswin(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'aswout').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%aswout(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4578,&
    'frame/module_domain.f: Failed to allocate grid%aswout(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%aswout=initial_data_value
ELSE
  ALLOCATE(grid%aswout(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4585,&
    'frame/module_domain.f: Failed to allocate grid%aswout(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'aswtoa').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%aswtoa(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4594,&
    'frame/module_domain.f: Failed to allocate grid%aswtoa(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%aswtoa=initial_data_value
ELSE
  ALLOCATE(grid%aswtoa(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4601,&
    'frame/module_domain.f: Failed to allocate grid%aswtoa(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'sfcshx').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%sfcshx(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4610,&
    'frame/module_domain.f: Failed to allocate grid%sfcshx(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sfcshx=initial_data_value
ELSE
  ALLOCATE(grid%sfcshx(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4617,&
    'frame/module_domain.f: Failed to allocate grid%sfcshx(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'sfclhx').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%sfclhx(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4626,&
    'frame/module_domain.f: Failed to allocate grid%sfclhx(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sfclhx=initial_data_value
ELSE
  ALLOCATE(grid%sfclhx(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4633,&
    'frame/module_domain.f: Failed to allocate grid%sfclhx(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'subshx').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%subshx(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4642,&
    'frame/module_domain.f: Failed to allocate grid%subshx(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%subshx=initial_data_value
ELSE
  ALLOCATE(grid%subshx(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4649,&
    'frame/module_domain.f: Failed to allocate grid%subshx(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'snopcx').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%snopcx(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4658,&
    'frame/module_domain.f: Failed to allocate grid%snopcx(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%snopcx=initial_data_value
ELSE
  ALLOCATE(grid%snopcx(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4665,&
    'frame/module_domain.f: Failed to allocate grid%snopcx(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'sfcuvx').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%sfcuvx(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4674,&
    'frame/module_domain.f: Failed to allocate grid%sfcuvx(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sfcuvx=initial_data_value
ELSE
  ALLOCATE(grid%sfcuvx(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4681,&
    'frame/module_domain.f: Failed to allocate grid%sfcuvx(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'potevp').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%potevp(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4690,&
    'frame/module_domain.f: Failed to allocate grid%potevp(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%potevp=initial_data_value
ELSE
  ALLOCATE(grid%potevp(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4697,&
    'frame/module_domain.f: Failed to allocate grid%potevp(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'potflx').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%potflx(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4706,&
    'frame/module_domain.f: Failed to allocate grid%potflx(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%potflx=initial_data_value
ELSE
  ALLOCATE(grid%potflx(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4713,&
    'frame/module_domain.f: Failed to allocate grid%potflx(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'tlmin').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%tlmin(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4722,&
    'frame/module_domain.f: Failed to allocate grid%tlmin(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tlmin=initial_data_value
ELSE
  ALLOCATE(grid%tlmin(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4729,&
    'frame/module_domain.f: Failed to allocate grid%tlmin(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'tlmax').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%tlmax(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4738,&
    'frame/module_domain.f: Failed to allocate grid%tlmax(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tlmax=initial_data_value
ELSE
  ALLOCATE(grid%tlmax(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4745,&
    'frame/module_domain.f: Failed to allocate grid%tlmax(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'t02_min').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%t02_min(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4754,&
    'frame/module_domain.f: Failed to allocate grid%t02_min(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%t02_min=initial_data_value
ELSE
  ALLOCATE(grid%t02_min(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4761,&
    'frame/module_domain.f: Failed to allocate grid%t02_min(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'t02_max').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%t02_max(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4770,&
    'frame/module_domain.f: Failed to allocate grid%t02_max(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%t02_max=initial_data_value
ELSE
  ALLOCATE(grid%t02_max(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4777,&
    'frame/module_domain.f: Failed to allocate grid%t02_max(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'rh02_min').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%rh02_min(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4786,&
    'frame/module_domain.f: Failed to allocate grid%rh02_min(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rh02_min=initial_data_value
ELSE
  ALLOCATE(grid%rh02_min(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4793,&
    'frame/module_domain.f: Failed to allocate grid%rh02_min(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'rh02_max').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%rh02_max(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4802,&
    'frame/module_domain.f: Failed to allocate grid%rh02_max(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rh02_max=initial_data_value
ELSE
  ALLOCATE(grid%rh02_max(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4809,&
    'frame/module_domain.f: Failed to allocate grid%rh02_max(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'rlwtt').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%rlwtt(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4818,&
    'frame/module_domain.f: Failed to allocate grid%rlwtt(sm31:em31,sm32:em32,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rlwtt=initial_data_value
ELSE
  ALLOCATE(grid%rlwtt(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4825,&
    'frame/module_domain.f: Failed to allocate grid%rlwtt(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'rswtt').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%rswtt(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4834,&
    'frame/module_domain.f: Failed to allocate grid%rswtt(sm31:em31,sm32:em32,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rswtt=initial_data_value
ELSE
  ALLOCATE(grid%rswtt(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4841,&
    'frame/module_domain.f: Failed to allocate grid%rswtt(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'tcucn').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%tcucn(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4850,&
    'frame/module_domain.f: Failed to allocate grid%tcucn(sm31:em31,sm32:em32,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tcucn=initial_data_value
ELSE
  ALLOCATE(grid%tcucn(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4857,&
    'frame/module_domain.f: Failed to allocate grid%tcucn(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'train').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%train(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4866,&
    'frame/module_domain.f: Failed to allocate grid%train(sm31:em31,sm32:em32,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%train=initial_data_value
ELSE
  ALLOCATE(grid%train(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4873,&
    'frame/module_domain.f: Failed to allocate grid%train(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'ncfrcv').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%ncfrcv(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4882,&
    'frame/module_domain.f: Failed to allocate grid%ncfrcv(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ncfrcv=0
ELSE
  ALLOCATE(grid%ncfrcv(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4889,&
    'frame/module_domain.f: Failed to allocate grid%ncfrcv(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'ncfrst').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%ncfrst(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4898,&
    'frame/module_domain.f: Failed to allocate grid%ncfrst(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ncfrst=0
ELSE
  ALLOCATE(grid%ncfrst(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4905,&
    'frame/module_domain.f: Failed to allocate grid%ncfrst(1,1).  ')
  endif
ENDIF
IF ( setinitval .EQ. 3 ) grid%nphs0=0
IF ( setinitval .EQ. 3 ) grid%nprec=0
IF ( setinitval .EQ. 3 ) grid%nclod=0
IF ( setinitval .EQ. 3 ) grid%nheat=0
IF ( setinitval .EQ. 3 ) grid%nrdlw=0
IF ( setinitval .EQ. 3 ) grid%nrdsw=0
IF ( setinitval .EQ. 3 ) grid%nsrfc=0
IF ( setinitval .EQ. 3 ) grid%avrain=initial_data_value
IF ( setinitval .EQ. 3 ) grid%avcnvc=initial_data_value
IF ( setinitval .EQ. 3 ) grid%aratim=initial_data_value
IF ( setinitval .EQ. 3 ) grid%acutim=initial_data_value
IF ( setinitval .EQ. 3 ) grid%ardlw=initial_data_value
IF ( setinitval .EQ. 3 ) grid%ardsw=initial_data_value
IF ( setinitval .EQ. 3 ) grid%asrfc=initial_data_value
IF ( setinitval .EQ. 3 ) grid%aphtim=initial_data_value
IF(in_use_for_config(id,'max10mw').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%max10mw(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4929,&
    'frame/module_domain.f: Failed to allocate grid%max10mw(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%max10mw=initial_data_value
ELSE
  ALLOCATE(grid%max10mw(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4936,&
    'frame/module_domain.f: Failed to allocate grid%max10mw(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'max10u').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%max10u(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4945,&
    'frame/module_domain.f: Failed to allocate grid%max10u(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%max10u=initial_data_value
ELSE
  ALLOCATE(grid%max10u(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4952,&
    'frame/module_domain.f: Failed to allocate grid%max10u(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'max10v').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%max10v(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4961,&
    'frame/module_domain.f: Failed to allocate grid%max10v(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%max10v=initial_data_value
ELSE
  ALLOCATE(grid%max10v(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4968,&
    'frame/module_domain.f: Failed to allocate grid%max10v(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'maxupdr').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%maxupdr(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4977,&
    'frame/module_domain.f: Failed to allocate grid%maxupdr(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%maxupdr=initial_data_value
ELSE
  ALLOCATE(grid%maxupdr(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4984,&
    'frame/module_domain.f: Failed to allocate grid%maxupdr(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'maxdndr').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%maxdndr(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",4993,&
    'frame/module_domain.f: Failed to allocate grid%maxdndr(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%maxdndr=initial_data_value
ELSE
  ALLOCATE(grid%maxdndr(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5000,&
    'frame/module_domain.f: Failed to allocate grid%maxdndr(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'maxhlcy').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%maxhlcy(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5009,&
    'frame/module_domain.f: Failed to allocate grid%maxhlcy(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%maxhlcy=initial_data_value
ELSE
  ALLOCATE(grid%maxhlcy(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5016,&
    'frame/module_domain.f: Failed to allocate grid%maxhlcy(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'maxdbz').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%maxdbz(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5025,&
    'frame/module_domain.f: Failed to allocate grid%maxdbz(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%maxdbz=initial_data_value
ELSE
  ALLOCATE(grid%maxdbz(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5032,&
    'frame/module_domain.f: Failed to allocate grid%maxdbz(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'ihe').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%ihe(sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5041,&
    'frame/module_domain.f: Failed to allocate grid%ihe(sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ihe=0
ELSE
  ALLOCATE(grid%ihe(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5048,&
    'frame/module_domain.f: Failed to allocate grid%ihe(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'ihw').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%ihw(sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5057,&
    'frame/module_domain.f: Failed to allocate grid%ihw(sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ihw=0
ELSE
  ALLOCATE(grid%ihw(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5064,&
    'frame/module_domain.f: Failed to allocate grid%ihw(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'ive').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%ive(sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5073,&
    'frame/module_domain.f: Failed to allocate grid%ive(sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ive=0
ELSE
  ALLOCATE(grid%ive(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5080,&
    'frame/module_domain.f: Failed to allocate grid%ive(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'ivw').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%ivw(sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5089,&
    'frame/module_domain.f: Failed to allocate grid%ivw(sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ivw=0
ELSE
  ALLOCATE(grid%ivw(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5096,&
    'frame/module_domain.f: Failed to allocate grid%ivw(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'irad').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))) * 4
  ALLOCATE(grid%irad(sm31:em31),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5105,&
    'frame/module_domain.f: Failed to allocate grid%irad(sm31:em31). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%irad=0
ELSE
  ALLOCATE(grid%irad(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5112,&
    'frame/module_domain.f: Failed to allocate grid%irad(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'iheg').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((2600)-(1)+1))) * 4
  ALLOCATE(grid%iheg(1:2600),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5121,&
    'frame/module_domain.f: Failed to allocate grid%iheg(1:2600). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%iheg=0
ELSE
  ALLOCATE(grid%iheg(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5128,&
    'frame/module_domain.f: Failed to allocate grid%iheg(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'ihwg').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((2600)-(1)+1))) * 4
  ALLOCATE(grid%ihwg(1:2600),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5137,&
    'frame/module_domain.f: Failed to allocate grid%ihwg(1:2600). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ihwg=0
ELSE
  ALLOCATE(grid%ihwg(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5144,&
    'frame/module_domain.f: Failed to allocate grid%ihwg(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'iveg').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((2600)-(1)+1))) * 4
  ALLOCATE(grid%iveg(1:2600),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5153,&
    'frame/module_domain.f: Failed to allocate grid%iveg(1:2600). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%iveg=0
ELSE
  ALLOCATE(grid%iveg(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5160,&
    'frame/module_domain.f: Failed to allocate grid%iveg(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'ivwg').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((2600)-(1)+1))) * 4
  ALLOCATE(grid%ivwg(1:2600),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5169,&
    'frame/module_domain.f: Failed to allocate grid%ivwg(1:2600). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ivwg=0
ELSE
  ALLOCATE(grid%ivwg(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5176,&
    'frame/module_domain.f: Failed to allocate grid%ivwg(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'iradg').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((2000)-(1)+1))) * 4
  ALLOCATE(grid%iradg(1:2000),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5185,&
    'frame/module_domain.f: Failed to allocate grid%iradg(1:2000). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%iradg=0
ELSE
  ALLOCATE(grid%iradg(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5192,&
    'frame/module_domain.f: Failed to allocate grid%iradg(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'n_iup_h').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%n_iup_h(sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5201,&
    'frame/module_domain.f: Failed to allocate grid%n_iup_h(sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%n_iup_h=0
ELSE
  ALLOCATE(grid%n_iup_h(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5208,&
    'frame/module_domain.f: Failed to allocate grid%n_iup_h(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'n_iup_v').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%n_iup_v(sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5217,&
    'frame/module_domain.f: Failed to allocate grid%n_iup_v(sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%n_iup_v=0
ELSE
  ALLOCATE(grid%n_iup_v(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5224,&
    'frame/module_domain.f: Failed to allocate grid%n_iup_v(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'n_iup_adh').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%n_iup_adh(sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5233,&
    'frame/module_domain.f: Failed to allocate grid%n_iup_adh(sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%n_iup_adh=0
ELSE
  ALLOCATE(grid%n_iup_adh(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5240,&
    'frame/module_domain.f: Failed to allocate grid%n_iup_adh(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'n_iup_adv').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%n_iup_adv(sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5249,&
    'frame/module_domain.f: Failed to allocate grid%n_iup_adv(sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%n_iup_adv=0
ELSE
  ALLOCATE(grid%n_iup_adv(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5256,&
    'frame/module_domain.f: Failed to allocate grid%n_iup_adv(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'iup_h').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%iup_h(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5265,&
    'frame/module_domain.f: Failed to allocate grid%iup_h(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%iup_h=0
ELSE
  ALLOCATE(grid%iup_h(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5272,&
    'frame/module_domain.f: Failed to allocate grid%iup_h(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'iup_v').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%iup_v(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5281,&
    'frame/module_domain.f: Failed to allocate grid%iup_v(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%iup_v=0
ELSE
  ALLOCATE(grid%iup_v(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5288,&
    'frame/module_domain.f: Failed to allocate grid%iup_v(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'iup_adh').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%iup_adh(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5297,&
    'frame/module_domain.f: Failed to allocate grid%iup_adh(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%iup_adh=0
ELSE
  ALLOCATE(grid%iup_adh(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5304,&
    'frame/module_domain.f: Failed to allocate grid%iup_adh(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'iup_adv').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%iup_adv(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5313,&
    'frame/module_domain.f: Failed to allocate grid%iup_adv(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%iup_adv=0
ELSE
  ALLOCATE(grid%iup_adv(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5320,&
    'frame/module_domain.f: Failed to allocate grid%iup_adv(1,1).  ')
  endif
ENDIF
IF ( setinitval .EQ. 3 ) grid%imicrogram=0
IF(in_use_for_config(id,'imask_nostag').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%imask_nostag(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5330,&
    'frame/module_domain.f: Failed to allocate grid%imask_nostag(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%imask_nostag=0
ELSE
  ALLOCATE(grid%imask_nostag(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5337,&
    'frame/module_domain.f: Failed to allocate grid%imask_nostag(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'imask_xstag').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%imask_xstag(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5346,&
    'frame/module_domain.f: Failed to allocate grid%imask_xstag(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%imask_xstag=0
ELSE
  ALLOCATE(grid%imask_xstag(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5353,&
    'frame/module_domain.f: Failed to allocate grid%imask_xstag(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'imask_ystag').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%imask_ystag(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5362,&
    'frame/module_domain.f: Failed to allocate grid%imask_ystag(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%imask_ystag=0
ELSE
  ALLOCATE(grid%imask_ystag(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5369,&
    'frame/module_domain.f: Failed to allocate grid%imask_ystag(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'imask_xystag').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%imask_xystag(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5378,&
    'frame/module_domain.f: Failed to allocate grid%imask_xystag(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%imask_xystag=0
ELSE
  ALLOCATE(grid%imask_xystag(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5385,&
    'frame/module_domain.f: Failed to allocate grid%imask_xystag(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'sm000007').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%sm000007(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5394,&
    'frame/module_domain.f: Failed to allocate grid%sm000007(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sm000007=initial_data_value
ELSE
  ALLOCATE(grid%sm000007(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5401,&
    'frame/module_domain.f: Failed to allocate grid%sm000007(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'sm007028').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%sm007028(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5410,&
    'frame/module_domain.f: Failed to allocate grid%sm007028(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sm007028=initial_data_value
ELSE
  ALLOCATE(grid%sm007028(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5417,&
    'frame/module_domain.f: Failed to allocate grid%sm007028(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'sm028100').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%sm028100(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5426,&
    'frame/module_domain.f: Failed to allocate grid%sm028100(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sm028100=initial_data_value
ELSE
  ALLOCATE(grid%sm028100(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5433,&
    'frame/module_domain.f: Failed to allocate grid%sm028100(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'sm100255').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%sm100255(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5442,&
    'frame/module_domain.f: Failed to allocate grid%sm100255(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sm100255=initial_data_value
ELSE
  ALLOCATE(grid%sm100255(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5449,&
    'frame/module_domain.f: Failed to allocate grid%sm100255(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'st000007').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%st000007(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5458,&
    'frame/module_domain.f: Failed to allocate grid%st000007(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%st000007=initial_data_value
ELSE
  ALLOCATE(grid%st000007(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5465,&
    'frame/module_domain.f: Failed to allocate grid%st000007(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'st007028').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%st007028(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5474,&
    'frame/module_domain.f: Failed to allocate grid%st007028(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%st007028=initial_data_value
ELSE
  ALLOCATE(grid%st007028(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5481,&
    'frame/module_domain.f: Failed to allocate grid%st007028(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'st028100').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%st028100(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5490,&
    'frame/module_domain.f: Failed to allocate grid%st028100(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%st028100=initial_data_value
ELSE
  ALLOCATE(grid%st028100(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5497,&
    'frame/module_domain.f: Failed to allocate grid%st028100(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'st100255').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%st100255(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5506,&
    'frame/module_domain.f: Failed to allocate grid%st100255(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%st100255=initial_data_value
ELSE
  ALLOCATE(grid%st100255(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5513,&
    'frame/module_domain.f: Failed to allocate grid%st100255(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'sm000010').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%sm000010(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5522,&
    'frame/module_domain.f: Failed to allocate grid%sm000010(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sm000010=initial_data_value
ELSE
  ALLOCATE(grid%sm000010(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5529,&
    'frame/module_domain.f: Failed to allocate grid%sm000010(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'sm010040').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%sm010040(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5538,&
    'frame/module_domain.f: Failed to allocate grid%sm010040(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sm010040=initial_data_value
ELSE
  ALLOCATE(grid%sm010040(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5545,&
    'frame/module_domain.f: Failed to allocate grid%sm010040(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'sm040100').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%sm040100(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5554,&
    'frame/module_domain.f: Failed to allocate grid%sm040100(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sm040100=initial_data_value
ELSE
  ALLOCATE(grid%sm040100(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5561,&
    'frame/module_domain.f: Failed to allocate grid%sm040100(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'sm100200').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%sm100200(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5570,&
    'frame/module_domain.f: Failed to allocate grid%sm100200(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sm100200=initial_data_value
ELSE
  ALLOCATE(grid%sm100200(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5577,&
    'frame/module_domain.f: Failed to allocate grid%sm100200(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'sm010200').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%sm010200(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5586,&
    'frame/module_domain.f: Failed to allocate grid%sm010200(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sm010200=initial_data_value
ELSE
  ALLOCATE(grid%sm010200(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5593,&
    'frame/module_domain.f: Failed to allocate grid%sm010200(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'soilm000').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%soilm000(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5602,&
    'frame/module_domain.f: Failed to allocate grid%soilm000(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilm000=initial_data_value
ELSE
  ALLOCATE(grid%soilm000(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5609,&
    'frame/module_domain.f: Failed to allocate grid%soilm000(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'soilm005').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%soilm005(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5618,&
    'frame/module_domain.f: Failed to allocate grid%soilm005(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilm005=initial_data_value
ELSE
  ALLOCATE(grid%soilm005(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5625,&
    'frame/module_domain.f: Failed to allocate grid%soilm005(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'soilm020').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%soilm020(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5634,&
    'frame/module_domain.f: Failed to allocate grid%soilm020(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilm020=initial_data_value
ELSE
  ALLOCATE(grid%soilm020(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5641,&
    'frame/module_domain.f: Failed to allocate grid%soilm020(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'soilm040').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%soilm040(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5650,&
    'frame/module_domain.f: Failed to allocate grid%soilm040(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilm040=initial_data_value
ELSE
  ALLOCATE(grid%soilm040(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5657,&
    'frame/module_domain.f: Failed to allocate grid%soilm040(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'soilm160').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%soilm160(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5666,&
    'frame/module_domain.f: Failed to allocate grid%soilm160(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilm160=initial_data_value
ELSE
  ALLOCATE(grid%soilm160(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5673,&
    'frame/module_domain.f: Failed to allocate grid%soilm160(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'soilm300').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%soilm300(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5682,&
    'frame/module_domain.f: Failed to allocate grid%soilm300(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilm300=initial_data_value
ELSE
  ALLOCATE(grid%soilm300(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5689,&
    'frame/module_domain.f: Failed to allocate grid%soilm300(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'sw000010').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%sw000010(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5698,&
    'frame/module_domain.f: Failed to allocate grid%sw000010(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sw000010=initial_data_value
ELSE
  ALLOCATE(grid%sw000010(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5705,&
    'frame/module_domain.f: Failed to allocate grid%sw000010(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'sw010040').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%sw010040(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5714,&
    'frame/module_domain.f: Failed to allocate grid%sw010040(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sw010040=initial_data_value
ELSE
  ALLOCATE(grid%sw010040(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5721,&
    'frame/module_domain.f: Failed to allocate grid%sw010040(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'sw040100').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%sw040100(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5730,&
    'frame/module_domain.f: Failed to allocate grid%sw040100(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sw040100=initial_data_value
ELSE
  ALLOCATE(grid%sw040100(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5737,&
    'frame/module_domain.f: Failed to allocate grid%sw040100(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'sw100200').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%sw100200(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5746,&
    'frame/module_domain.f: Failed to allocate grid%sw100200(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sw100200=initial_data_value
ELSE
  ALLOCATE(grid%sw100200(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5753,&
    'frame/module_domain.f: Failed to allocate grid%sw100200(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'sw010200').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%sw010200(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5762,&
    'frame/module_domain.f: Failed to allocate grid%sw010200(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sw010200=initial_data_value
ELSE
  ALLOCATE(grid%sw010200(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5769,&
    'frame/module_domain.f: Failed to allocate grid%sw010200(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'soilw000').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%soilw000(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5778,&
    'frame/module_domain.f: Failed to allocate grid%soilw000(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilw000=initial_data_value
ELSE
  ALLOCATE(grid%soilw000(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5785,&
    'frame/module_domain.f: Failed to allocate grid%soilw000(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'soilw005').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%soilw005(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5794,&
    'frame/module_domain.f: Failed to allocate grid%soilw005(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilw005=initial_data_value
ELSE
  ALLOCATE(grid%soilw005(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5801,&
    'frame/module_domain.f: Failed to allocate grid%soilw005(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'soilw020').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%soilw020(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5810,&
    'frame/module_domain.f: Failed to allocate grid%soilw020(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilw020=initial_data_value
ELSE
  ALLOCATE(grid%soilw020(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5817,&
    'frame/module_domain.f: Failed to allocate grid%soilw020(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'soilw040').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%soilw040(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5826,&
    'frame/module_domain.f: Failed to allocate grid%soilw040(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilw040=initial_data_value
ELSE
  ALLOCATE(grid%soilw040(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5833,&
    'frame/module_domain.f: Failed to allocate grid%soilw040(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'soilw160').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%soilw160(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5842,&
    'frame/module_domain.f: Failed to allocate grid%soilw160(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilw160=initial_data_value
ELSE
  ALLOCATE(grid%soilw160(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5849,&
    'frame/module_domain.f: Failed to allocate grid%soilw160(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'soilw300').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%soilw300(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5858,&
    'frame/module_domain.f: Failed to allocate grid%soilw300(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilw300=initial_data_value
ELSE
  ALLOCATE(grid%soilw300(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5865,&
    'frame/module_domain.f: Failed to allocate grid%soilw300(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'st000010').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%st000010(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5874,&
    'frame/module_domain.f: Failed to allocate grid%st000010(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%st000010=initial_data_value
ELSE
  ALLOCATE(grid%st000010(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5881,&
    'frame/module_domain.f: Failed to allocate grid%st000010(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'st010040').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%st010040(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5890,&
    'frame/module_domain.f: Failed to allocate grid%st010040(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%st010040=initial_data_value
ELSE
  ALLOCATE(grid%st010040(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5897,&
    'frame/module_domain.f: Failed to allocate grid%st010040(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'st040100').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%st040100(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5906,&
    'frame/module_domain.f: Failed to allocate grid%st040100(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%st040100=initial_data_value
ELSE
  ALLOCATE(grid%st040100(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5913,&
    'frame/module_domain.f: Failed to allocate grid%st040100(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'st100200').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%st100200(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5922,&
    'frame/module_domain.f: Failed to allocate grid%st100200(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%st100200=initial_data_value
ELSE
  ALLOCATE(grid%st100200(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5929,&
    'frame/module_domain.f: Failed to allocate grid%st100200(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'st010200').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%st010200(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5938,&
    'frame/module_domain.f: Failed to allocate grid%st010200(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%st010200=initial_data_value
ELSE
  ALLOCATE(grid%st010200(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5945,&
    'frame/module_domain.f: Failed to allocate grid%st010200(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'soilt000').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%soilt000(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5954,&
    'frame/module_domain.f: Failed to allocate grid%soilt000(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilt000=initial_data_value
ELSE
  ALLOCATE(grid%soilt000(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5961,&
    'frame/module_domain.f: Failed to allocate grid%soilt000(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'soilt005').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%soilt005(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5970,&
    'frame/module_domain.f: Failed to allocate grid%soilt005(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilt005=initial_data_value
ELSE
  ALLOCATE(grid%soilt005(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5977,&
    'frame/module_domain.f: Failed to allocate grid%soilt005(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'soilt020').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%soilt020(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5986,&
    'frame/module_domain.f: Failed to allocate grid%soilt020(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilt020=initial_data_value
ELSE
  ALLOCATE(grid%soilt020(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",5993,&
    'frame/module_domain.f: Failed to allocate grid%soilt020(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'soilt040').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%soilt040(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6002,&
    'frame/module_domain.f: Failed to allocate grid%soilt040(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilt040=initial_data_value
ELSE
  ALLOCATE(grid%soilt040(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6009,&
    'frame/module_domain.f: Failed to allocate grid%soilt040(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'soilt160').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%soilt160(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6018,&
    'frame/module_domain.f: Failed to allocate grid%soilt160(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilt160=initial_data_value
ELSE
  ALLOCATE(grid%soilt160(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6025,&
    'frame/module_domain.f: Failed to allocate grid%soilt160(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'soilt300').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%soilt300(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6034,&
    'frame/module_domain.f: Failed to allocate grid%soilt300(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilt300=initial_data_value
ELSE
  ALLOCATE(grid%soilt300(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6041,&
    'frame/module_domain.f: Failed to allocate grid%soilt300(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'landmask').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%landmask(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6050,&
    'frame/module_domain.f: Failed to allocate grid%landmask(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%landmask=initial_data_value
ELSE
  ALLOCATE(grid%landmask(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6057,&
    'frame/module_domain.f: Failed to allocate grid%landmask(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'topostdv').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%topostdv(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6066,&
    'frame/module_domain.f: Failed to allocate grid%topostdv(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%topostdv=initial_data_value
ELSE
  ALLOCATE(grid%topostdv(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6073,&
    'frame/module_domain.f: Failed to allocate grid%topostdv(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'toposlpx').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%toposlpx(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6082,&
    'frame/module_domain.f: Failed to allocate grid%toposlpx(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%toposlpx=initial_data_value
ELSE
  ALLOCATE(grid%toposlpx(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6089,&
    'frame/module_domain.f: Failed to allocate grid%toposlpx(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'toposlpy').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%toposlpy(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6098,&
    'frame/module_domain.f: Failed to allocate grid%toposlpy(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%toposlpy=initial_data_value
ELSE
  ALLOCATE(grid%toposlpy(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6105,&
    'frame/module_domain.f: Failed to allocate grid%toposlpy(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'greenmax').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%greenmax(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6114,&
    'frame/module_domain.f: Failed to allocate grid%greenmax(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%greenmax=initial_data_value
ELSE
  ALLOCATE(grid%greenmax(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6121,&
    'frame/module_domain.f: Failed to allocate grid%greenmax(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'greenmin').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%greenmin(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6130,&
    'frame/module_domain.f: Failed to allocate grid%greenmin(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%greenmin=initial_data_value
ELSE
  ALLOCATE(grid%greenmin(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6137,&
    'frame/module_domain.f: Failed to allocate grid%greenmin(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'albedomx').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%albedomx(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6146,&
    'frame/module_domain.f: Failed to allocate grid%albedomx(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%albedomx=initial_data_value
ELSE
  ALLOCATE(grid%albedomx(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6153,&
    'frame/module_domain.f: Failed to allocate grid%albedomx(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'slopecat').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%slopecat(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6162,&
    'frame/module_domain.f: Failed to allocate grid%slopecat(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%slopecat=initial_data_value
ELSE
  ALLOCATE(grid%slopecat(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6169,&
    'frame/module_domain.f: Failed to allocate grid%slopecat(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'toposoil').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%toposoil(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6178,&
    'frame/module_domain.f: Failed to allocate grid%toposoil(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%toposoil=initial_data_value
ELSE
  ALLOCATE(grid%toposoil(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6185,&
    'frame/module_domain.f: Failed to allocate grid%toposoil(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'landusef').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((model_config_rec%num_land_cat)-(1)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%landusef(sm31:em31,1:model_config_rec%num_land_cat,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6194,&
    'frame/module_domain.f: Failed to allocate grid%landusef(sm31:em31,1:model_config_rec%num_land_cat,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%landusef=initial_data_value
ELSE
  ALLOCATE(grid%landusef(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6201,&
    'frame/module_domain.f: Failed to allocate grid%landusef(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'soilctop').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((model_config_rec%num_soil_cat)-(1)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%soilctop(sm31:em31,1:model_config_rec%num_soil_cat,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6210,&
    'frame/module_domain.f: Failed to allocate grid%soilctop(sm31:em31,1:model_config_rec%num_soil_cat,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilctop=initial_data_value
ELSE
  ALLOCATE(grid%soilctop(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6217,&
    'frame/module_domain.f: Failed to allocate grid%soilctop(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'soilcbot').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((model_config_rec%num_soil_cat)-(1)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%soilcbot(sm31:em31,1:model_config_rec%num_soil_cat,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6226,&
    'frame/module_domain.f: Failed to allocate grid%soilcbot(sm31:em31,1:model_config_rec%num_soil_cat,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilcbot=initial_data_value
ELSE
  ALLOCATE(grid%soilcbot(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6233,&
    'frame/module_domain.f: Failed to allocate grid%soilcbot(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'ts_hour').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((model_config_rec%ts_buf_size)-(1)+1))*(((model_config_rec%max_ts_locs)-(1)+1))) * 4
  ALLOCATE(grid%ts_hour(1:model_config_rec%ts_buf_size,1:model_config_rec%max_ts_locs),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6242,&
    'frame/module_domain.f: Failed to allocate grid%ts_hour(1:model_config_rec%ts_buf_size,1:model_config_rec%max_ts_locs). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ts_hour=initial_data_value
ELSE
  ALLOCATE(grid%ts_hour(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6249,&
    'frame/module_domain.f: Failed to allocate grid%ts_hour(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'ts_u').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((model_config_rec%ts_buf_size)-(1)+1))*(((model_config_rec%max_ts_locs)-(1)+1))) * 4
  ALLOCATE(grid%ts_u(1:model_config_rec%ts_buf_size,1:model_config_rec%max_ts_locs),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6258,&
    'frame/module_domain.f: Failed to allocate grid%ts_u(1:model_config_rec%ts_buf_size,1:model_config_rec%max_ts_locs). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ts_u=initial_data_value
ELSE
  ALLOCATE(grid%ts_u(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6265,&
    'frame/module_domain.f: Failed to allocate grid%ts_u(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'ts_v').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((model_config_rec%ts_buf_size)-(1)+1))*(((model_config_rec%max_ts_locs)-(1)+1))) * 4
  ALLOCATE(grid%ts_v(1:model_config_rec%ts_buf_size,1:model_config_rec%max_ts_locs),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6274,&
    'frame/module_domain.f: Failed to allocate grid%ts_v(1:model_config_rec%ts_buf_size,1:model_config_rec%max_ts_locs). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ts_v=initial_data_value
ELSE
  ALLOCATE(grid%ts_v(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6281,&
    'frame/module_domain.f: Failed to allocate grid%ts_v(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'ts_q').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((model_config_rec%ts_buf_size)-(1)+1))*(((model_config_rec%max_ts_locs)-(1)+1))) * 4
  ALLOCATE(grid%ts_q(1:model_config_rec%ts_buf_size,1:model_config_rec%max_ts_locs),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6290,&
    'frame/module_domain.f: Failed to allocate grid%ts_q(1:model_config_rec%ts_buf_size,1:model_config_rec%max_ts_locs). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ts_q=initial_data_value
ELSE
  ALLOCATE(grid%ts_q(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6297,&
    'frame/module_domain.f: Failed to allocate grid%ts_q(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'ts_t').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((model_config_rec%ts_buf_size)-(1)+1))*(((model_config_rec%max_ts_locs)-(1)+1))) * 4
  ALLOCATE(grid%ts_t(1:model_config_rec%ts_buf_size,1:model_config_rec%max_ts_locs),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6306,&
    'frame/module_domain.f: Failed to allocate grid%ts_t(1:model_config_rec%ts_buf_size,1:model_config_rec%max_ts_locs). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ts_t=initial_data_value
ELSE
  ALLOCATE(grid%ts_t(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6313,&
    'frame/module_domain.f: Failed to allocate grid%ts_t(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'ts_psfc').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((model_config_rec%ts_buf_size)-(1)+1))*(((model_config_rec%max_ts_locs)-(1)+1))) * 4
  ALLOCATE(grid%ts_psfc(1:model_config_rec%ts_buf_size,1:model_config_rec%max_ts_locs),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6322,&
    'frame/module_domain.f: Failed to allocate grid%ts_psfc(1:model_config_rec%ts_buf_size,1:model_config_rec%max_ts_locs). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ts_psfc=initial_data_value
ELSE
  ALLOCATE(grid%ts_psfc(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6329,&
    'frame/module_domain.f: Failed to allocate grid%ts_psfc(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'ts_tsk').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((model_config_rec%ts_buf_size)-(1)+1))*(((model_config_rec%max_ts_locs)-(1)+1))) * 4
  ALLOCATE(grid%ts_tsk(1:model_config_rec%ts_buf_size,1:model_config_rec%max_ts_locs),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6338,&
    'frame/module_domain.f: Failed to allocate grid%ts_tsk(1:model_config_rec%ts_buf_size,1:model_config_rec%max_ts_locs). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ts_tsk=initial_data_value
ELSE
  ALLOCATE(grid%ts_tsk(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6345,&
    'frame/module_domain.f: Failed to allocate grid%ts_tsk(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'ts_tslb').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((model_config_rec%ts_buf_size)-(1)+1))*(((model_config_rec%max_ts_locs)-(1)+1))) * 4
  ALLOCATE(grid%ts_tslb(1:model_config_rec%ts_buf_size,1:model_config_rec%max_ts_locs),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6354,&
    'frame/module_domain.f: Failed to allocate grid%ts_tslb(1:model_config_rec%ts_buf_size,1:model_config_rec%max_ts_locs). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ts_tslb=initial_data_value
ELSE
  ALLOCATE(grid%ts_tslb(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6361,&
    'frame/module_domain.f: Failed to allocate grid%ts_tslb(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'ts_clw').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((model_config_rec%ts_buf_size)-(1)+1))*(((model_config_rec%max_ts_locs)-(1)+1))) * 4
  ALLOCATE(grid%ts_clw(1:model_config_rec%ts_buf_size,1:model_config_rec%max_ts_locs),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6370,&
    'frame/module_domain.f: Failed to allocate grid%ts_clw(1:model_config_rec%ts_buf_size,1:model_config_rec%max_ts_locs). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ts_clw=initial_data_value
ELSE
  ALLOCATE(grid%ts_clw(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6377,&
    'frame/module_domain.f: Failed to allocate grid%ts_clw(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'moist'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1)*num_moist)) * 4
  ALLOCATE(grid%moist(sm31:em31,sm32:em32,sm33:em33,num_moist),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6386,&
    'frame/module_domain.f: Failed to allocate grid%moist(sm31:em31,sm32:em32,sm33:em33,num_moist). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%moist=initial_data_value
ELSE
  ALLOCATE(grid%moist(1,1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6393,&
    'frame/module_domain.f: Failed to allocate grid%moist(1,1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'dfi_moist'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1)*num_dfi_moist)) * 4
  ALLOCATE(grid%dfi_moist(sm31:em31,sm32:em32,sm33:em33,num_dfi_moist),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6402,&
    'frame/module_domain.f: Failed to allocate grid%dfi_moist(sm31:em31,sm32:em32,sm33:em33,num_dfi_moist). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%dfi_moist=initial_data_value
ELSE
  ALLOCATE(grid%dfi_moist(1,1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6409,&
    'frame/module_domain.f: Failed to allocate grid%dfi_moist(1,1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'scalar'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1)*num_scalar)) * 4
  ALLOCATE(grid%scalar(sm31:em31,sm32:em32,sm33:em33,num_scalar),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6418,&
    'frame/module_domain.f: Failed to allocate grid%scalar(sm31:em31,sm32:em32,sm33:em33,num_scalar). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%scalar=initial_data_value
ELSE
  ALLOCATE(grid%scalar(1,1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6425,&
    'frame/module_domain.f: Failed to allocate grid%scalar(1,1,1,1).  ')
  endif
ENDIF
IF(.TRUE..AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
(((em32-sm32+1)*(em33-sm33+1)*(spec_bdy_width)*num_scalar)) * 4
  ALLOCATE(grid%scalar_bxs(sm32:em32,sm33:em33,spec_bdy_width,num_scalar),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6434,&
    'frame/module_domain.f: Failed to allocate grid%scalar_bxs(sm32:em32,sm33:em33,spec_bdy_width,num_scalar). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%scalar_bxs=initial_data_value
  num_bytes_allocated = num_bytes_allocated + &
(((em32-sm32+1)*(em33-sm33+1)*(spec_bdy_width)*num_scalar)) * 4
  ALLOCATE(grid%scalar_bxe(sm32:em32,sm33:em33,spec_bdy_width,num_scalar),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6442,&
    'frame/module_domain.f: Failed to allocate grid%scalar_bxe(sm32:em32,sm33:em33,spec_bdy_width,num_scalar). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%scalar_bxe=initial_data_value
  num_bytes_allocated = num_bytes_allocated + &
(((em31-sm31+1)*(em33-sm33+1)*(spec_bdy_width)*num_scalar)) * 4
  ALLOCATE(grid%scalar_bys(sm31:em31,sm33:em33,spec_bdy_width,num_scalar),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6450,&
    'frame/module_domain.f: Failed to allocate grid%scalar_bys(sm31:em31,sm33:em33,spec_bdy_width,num_scalar). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%scalar_bys=initial_data_value
  num_bytes_allocated = num_bytes_allocated + &
(((em31-sm31+1)*(em33-sm33+1)*(spec_bdy_width)*num_scalar)) * 4
  ALLOCATE(grid%scalar_bye(sm31:em31,sm33:em33,spec_bdy_width,num_scalar),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6458,&
    'frame/module_domain.f: Failed to allocate grid%scalar_bye(sm31:em31,sm33:em33,spec_bdy_width,num_scalar). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%scalar_bye=initial_data_value
ELSE
  ALLOCATE(grid%scalar_bxs(1,1,1,num_scalar),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6465,&
    'frame/module_domain.f: Failed to allocate grid%scalar_bxs(1,1,1,num_scalar).  ')
  endif
  ALLOCATE(grid%scalar_bxe(1,1,1,num_scalar),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6470,&
    'frame/module_domain.f: Failed to allocate grid%scalar_bxe(1,1,1,num_scalar).  ')
  endif
  ALLOCATE(grid%scalar_bys(1,1,1,num_scalar),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6475,&
    'frame/module_domain.f: Failed to allocate grid%scalar_bys(1,1,1,num_scalar).  ')
  endif
  ALLOCATE(grid%scalar_bye(1,1,1,num_scalar),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6480,&
    'frame/module_domain.f: Failed to allocate grid%scalar_bye(1,1,1,num_scalar).  ')
  endif
ENDIF
IF(.TRUE..AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
(((em32-sm32+1)*(em33-sm33+1)*(spec_bdy_width)*num_scalar)) * 4
  ALLOCATE(grid%scalar_btxs(sm32:em32,sm33:em33,spec_bdy_width,num_scalar),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6489,&
    'frame/module_domain.f: Failed to allocate grid%scalar_btxs(sm32:em32,sm33:em33,spec_bdy_width,num_scalar). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%scalar_btxs=initial_data_value
  num_bytes_allocated = num_bytes_allocated + &
(((em32-sm32+1)*(em33-sm33+1)*(spec_bdy_width)*num_scalar)) * 4
  ALLOCATE(grid%scalar_btxe(sm32:em32,sm33:em33,spec_bdy_width,num_scalar),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6497,&
    'frame/module_domain.f: Failed to allocate grid%scalar_btxe(sm32:em32,sm33:em33,spec_bdy_width,num_scalar). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%scalar_btxe=initial_data_value
  num_bytes_allocated = num_bytes_allocated + &
(((em31-sm31+1)*(em33-sm33+1)*(spec_bdy_width)*num_scalar)) * 4
  ALLOCATE(grid%scalar_btys(sm31:em31,sm33:em33,spec_bdy_width,num_scalar),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6505,&
    'frame/module_domain.f: Failed to allocate grid%scalar_btys(sm31:em31,sm33:em33,spec_bdy_width,num_scalar). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%scalar_btys=initial_data_value
  num_bytes_allocated = num_bytes_allocated + &
(((em31-sm31+1)*(em33-sm33+1)*(spec_bdy_width)*num_scalar)) * 4
  ALLOCATE(grid%scalar_btye(sm31:em31,sm33:em33,spec_bdy_width,num_scalar),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6513,&
    'frame/module_domain.f: Failed to allocate grid%scalar_btye(sm31:em31,sm33:em33,spec_bdy_width,num_scalar). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%scalar_btye=initial_data_value
ELSE
  ALLOCATE(grid%scalar_btxs(1,1,1,num_scalar),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6520,&
    'frame/module_domain.f: Failed to allocate grid%scalar_btxs(1,1,1,num_scalar).  ')
  endif
  ALLOCATE(grid%scalar_btxe(1,1,1,num_scalar),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6525,&
    'frame/module_domain.f: Failed to allocate grid%scalar_btxe(1,1,1,num_scalar).  ')
  endif
  ALLOCATE(grid%scalar_btys(1,1,1,num_scalar),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6530,&
    'frame/module_domain.f: Failed to allocate grid%scalar_btys(1,1,1,num_scalar).  ')
  endif
  ALLOCATE(grid%scalar_btye(1,1,1,num_scalar),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6535,&
    'frame/module_domain.f: Failed to allocate grid%scalar_btye(1,1,1,num_scalar).  ')
  endif
ENDIF
IF(in_use_for_config(id,'dfi_scalar'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))*(((em32)-(sm32)+1)*num_dfi_scalar)) * 4
  ALLOCATE(grid%dfi_scalar(sm31:em31,sm33:em33,sm32:em32,num_dfi_scalar),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6544,&
    'frame/module_domain.f: Failed to allocate grid%dfi_scalar(sm31:em31,sm33:em33,sm32:em32,num_dfi_scalar). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%dfi_scalar=initial_data_value
ELSE
  ALLOCATE(grid%dfi_scalar(1,1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6551,&
    'frame/module_domain.f: Failed to allocate grid%dfi_scalar(1,1,1,1).  ')
  endif
ENDIF
IF(.TRUE..AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
(((em32-sm32+1)*(em33-sm33+1)*(spec_bdy_width)*num_dfi_scalar)) * 4
  ALLOCATE(grid%dfi_scalar_bxs(sm32:em32,sm33:em33,spec_bdy_width,num_dfi_scalar),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6560,&
    'frame/module_domain.f: Failed to allocate grid%dfi_scalar_bxs(sm32:em32,sm33:em33,spec_bdy_width,num_dfi_scalar). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%dfi_scalar_bxs=initial_data_value
  num_bytes_allocated = num_bytes_allocated + &
(((em32-sm32+1)*(em33-sm33+1)*(spec_bdy_width)*num_dfi_scalar)) * 4
  ALLOCATE(grid%dfi_scalar_bxe(sm32:em32,sm33:em33,spec_bdy_width,num_dfi_scalar),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6568,&
    'frame/module_domain.f: Failed to allocate grid%dfi_scalar_bxe(sm32:em32,sm33:em33,spec_bdy_width,num_dfi_scalar). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%dfi_scalar_bxe=initial_data_value
  num_bytes_allocated = num_bytes_allocated + &
(((em31-sm31+1)*(em33-sm33+1)*(spec_bdy_width)*num_dfi_scalar)) * 4
  ALLOCATE(grid%dfi_scalar_bys(sm31:em31,sm33:em33,spec_bdy_width,num_dfi_scalar),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6576,&
    'frame/module_domain.f: Failed to allocate grid%dfi_scalar_bys(sm31:em31,sm33:em33,spec_bdy_width,num_dfi_scalar). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%dfi_scalar_bys=initial_data_value
  num_bytes_allocated = num_bytes_allocated + &
(((em31-sm31+1)*(em33-sm33+1)*(spec_bdy_width)*num_dfi_scalar)) * 4
  ALLOCATE(grid%dfi_scalar_bye(sm31:em31,sm33:em33,spec_bdy_width,num_dfi_scalar),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6584,&
    'frame/module_domain.f: Failed to allocate grid%dfi_scalar_bye(sm31:em31,sm33:em33,spec_bdy_width,num_dfi_scalar). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%dfi_scalar_bye=initial_data_value
ELSE
  ALLOCATE(grid%dfi_scalar_bxs(1,1,1,num_dfi_scalar),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6591,&
    'frame/module_domain.f: Failed to allocate grid%dfi_scalar_bxs(1,1,1,num_dfi_scalar).  ')
  endif
  ALLOCATE(grid%dfi_scalar_bxe(1,1,1,num_dfi_scalar),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6596,&
    'frame/module_domain.f: Failed to allocate grid%dfi_scalar_bxe(1,1,1,num_dfi_scalar).  ')
  endif
  ALLOCATE(grid%dfi_scalar_bys(1,1,1,num_dfi_scalar),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6601,&
    'frame/module_domain.f: Failed to allocate grid%dfi_scalar_bys(1,1,1,num_dfi_scalar).  ')
  endif
  ALLOCATE(grid%dfi_scalar_bye(1,1,1,num_dfi_scalar),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6606,&
    'frame/module_domain.f: Failed to allocate grid%dfi_scalar_bye(1,1,1,num_dfi_scalar).  ')
  endif
ENDIF
IF(.TRUE..AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
(((em32-sm32+1)*(em33-sm33+1)*(spec_bdy_width)*num_dfi_scalar)) * 4
  ALLOCATE(grid%dfi_scalar_btxs(sm32:em32,sm33:em33,spec_bdy_width,num_dfi_scalar),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6615,&
    'frame/module_domain.f: Failed to allocate grid%dfi_scalar_btxs(sm32:em32,sm33:em33,spec_bdy_width,num_dfi_scalar). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%dfi_scalar_btxs=initial_data_value
  num_bytes_allocated = num_bytes_allocated + &
(((em32-sm32+1)*(em33-sm33+1)*(spec_bdy_width)*num_dfi_scalar)) * 4
  ALLOCATE(grid%dfi_scalar_btxe(sm32:em32,sm33:em33,spec_bdy_width,num_dfi_scalar),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6623,&
    'frame/module_domain.f: Failed to allocate grid%dfi_scalar_btxe(sm32:em32,sm33:em33,spec_bdy_width,num_dfi_scalar). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%dfi_scalar_btxe=initial_data_value
  num_bytes_allocated = num_bytes_allocated + &
(((em31-sm31+1)*(em33-sm33+1)*(spec_bdy_width)*num_dfi_scalar)) * 4
  ALLOCATE(grid%dfi_scalar_btys(sm31:em31,sm33:em33,spec_bdy_width,num_dfi_scalar),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6631,&
    'frame/module_domain.f: Failed to allocate grid%dfi_scalar_btys(sm31:em31,sm33:em33,spec_bdy_width,num_dfi_scalar). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%dfi_scalar_btys=initial_data_value
  num_bytes_allocated = num_bytes_allocated + &
(((em31-sm31+1)*(em33-sm33+1)*(spec_bdy_width)*num_dfi_scalar)) * 4
  ALLOCATE(grid%dfi_scalar_btye(sm31:em31,sm33:em33,spec_bdy_width,num_dfi_scalar),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6639,&
    'frame/module_domain.f: Failed to allocate grid%dfi_scalar_btye(sm31:em31,sm33:em33,spec_bdy_width,num_dfi_scalar). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%dfi_scalar_btye=initial_data_value
ELSE
  ALLOCATE(grid%dfi_scalar_btxs(1,1,1,num_dfi_scalar),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6646,&
    'frame/module_domain.f: Failed to allocate grid%dfi_scalar_btxs(1,1,1,num_dfi_scalar).  ')
  endif
  ALLOCATE(grid%dfi_scalar_btxe(1,1,1,num_dfi_scalar),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6651,&
    'frame/module_domain.f: Failed to allocate grid%dfi_scalar_btxe(1,1,1,num_dfi_scalar).  ')
  endif
  ALLOCATE(grid%dfi_scalar_btys(1,1,1,num_dfi_scalar),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6656,&
    'frame/module_domain.f: Failed to allocate grid%dfi_scalar_btys(1,1,1,num_dfi_scalar).  ')
  endif
  ALLOCATE(grid%dfi_scalar_btye(1,1,1,num_dfi_scalar),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6661,&
    'frame/module_domain.f: Failed to allocate grid%dfi_scalar_btye(1,1,1,num_dfi_scalar).  ')
  endif
ENDIF
IF(in_use_for_config(id,'chem'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))*(((em32)-(sm32)+1)*num_chem)) * 4
  ALLOCATE(grid%chem(sm31:em31,sm33:em33,sm32:em32,num_chem),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6670,&
    'frame/module_domain.f: Failed to allocate grid%chem(sm31:em31,sm33:em33,sm32:em32,num_chem). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%chem=initial_data_value
ELSE
  ALLOCATE(grid%chem(1,1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6677,&
    'frame/module_domain.f: Failed to allocate grid%chem(1,1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'smois').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((model_config_rec%num_soil_layers)-(1)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%smois(sm31:em31,1:model_config_rec%num_soil_layers,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6686,&
    'frame/module_domain.f: Failed to allocate grid%smois(sm31:em31,1:model_config_rec%num_soil_layers,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%smois=initial_data_value
ELSE
  ALLOCATE(grid%smois(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6693,&
    'frame/module_domain.f: Failed to allocate grid%smois(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'tslb').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((model_config_rec%num_soil_layers)-(1)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%tslb(sm31:em31,1:model_config_rec%num_soil_layers,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6702,&
    'frame/module_domain.f: Failed to allocate grid%tslb(sm31:em31,1:model_config_rec%num_soil_layers,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tslb=initial_data_value
ELSE
  ALLOCATE(grid%tslb(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6709,&
    'frame/module_domain.f: Failed to allocate grid%tslb(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'gsw').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%gsw(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6718,&
    'frame/module_domain.f: Failed to allocate grid%gsw(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%gsw=initial_data_value
ELSE
  ALLOCATE(grid%gsw(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6725,&
    'frame/module_domain.f: Failed to allocate grid%gsw(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'xlat').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%xlat(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6734,&
    'frame/module_domain.f: Failed to allocate grid%xlat(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%xlat=initial_data_value
ELSE
  ALLOCATE(grid%xlat(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6741,&
    'frame/module_domain.f: Failed to allocate grid%xlat(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'xlong').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%xlong(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6750,&
    'frame/module_domain.f: Failed to allocate grid%xlong(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%xlong=initial_data_value
ELSE
  ALLOCATE(grid%xlong(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6757,&
    'frame/module_domain.f: Failed to allocate grid%xlong(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'xland').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%xland(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6766,&
    'frame/module_domain.f: Failed to allocate grid%xland(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%xland=initial_data_value
ELSE
  ALLOCATE(grid%xland(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6773,&
    'frame/module_domain.f: Failed to allocate grid%xland(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'raincv').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%raincv(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6782,&
    'frame/module_domain.f: Failed to allocate grid%raincv(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%raincv=initial_data_value
ELSE
  ALLOCATE(grid%raincv(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6789,&
    'frame/module_domain.f: Failed to allocate grid%raincv(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'psfc').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%psfc(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6798,&
    'frame/module_domain.f: Failed to allocate grid%psfc(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%psfc=initial_data_value
ELSE
  ALLOCATE(grid%psfc(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6805,&
    'frame/module_domain.f: Failed to allocate grid%psfc(1,1).  ')
  endif
ENDIF
IF ( setinitval .EQ. 3 ) grid%dtbc=initial_data_value
IF(in_use_for_config(id,'th2').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%th2(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6815,&
    'frame/module_domain.f: Failed to allocate grid%th2(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%th2=initial_data_value
ELSE
  ALLOCATE(grid%th2(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6822,&
    'frame/module_domain.f: Failed to allocate grid%th2(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'t2').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%t2(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6831,&
    'frame/module_domain.f: Failed to allocate grid%t2(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%t2=initial_data_value
ELSE
  ALLOCATE(grid%t2(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6838,&
    'frame/module_domain.f: Failed to allocate grid%t2(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'u10').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%u10(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6847,&
    'frame/module_domain.f: Failed to allocate grid%u10(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%u10=initial_data_value
ELSE
  ALLOCATE(grid%u10(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6854,&
    'frame/module_domain.f: Failed to allocate grid%u10(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'v10').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%v10(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6863,&
    'frame/module_domain.f: Failed to allocate grid%v10(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%v10=initial_data_value
ELSE
  ALLOCATE(grid%v10(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6870,&
    'frame/module_domain.f: Failed to allocate grid%v10(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'xice').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%xice(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6879,&
    'frame/module_domain.f: Failed to allocate grid%xice(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%xice=initial_data_value
ELSE
  ALLOCATE(grid%xice(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6886,&
    'frame/module_domain.f: Failed to allocate grid%xice(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'lai').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%lai(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6895,&
    'frame/module_domain.f: Failed to allocate grid%lai(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%lai=initial_data_value
ELSE
  ALLOCATE(grid%lai(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6902,&
    'frame/module_domain.f: Failed to allocate grid%lai(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'smstav').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%smstav(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6911,&
    'frame/module_domain.f: Failed to allocate grid%smstav(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%smstav=initial_data_value
ELSE
  ALLOCATE(grid%smstav(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6918,&
    'frame/module_domain.f: Failed to allocate grid%smstav(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'smstot').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%smstot(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6927,&
    'frame/module_domain.f: Failed to allocate grid%smstot(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%smstot=initial_data_value
ELSE
  ALLOCATE(grid%smstot(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6934,&
    'frame/module_domain.f: Failed to allocate grid%smstot(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'sfcrunoff').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%sfcrunoff(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6943,&
    'frame/module_domain.f: Failed to allocate grid%sfcrunoff(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sfcrunoff=initial_data_value
ELSE
  ALLOCATE(grid%sfcrunoff(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6950,&
    'frame/module_domain.f: Failed to allocate grid%sfcrunoff(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'udrunoff').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%udrunoff(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6959,&
    'frame/module_domain.f: Failed to allocate grid%udrunoff(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%udrunoff=initial_data_value
ELSE
  ALLOCATE(grid%udrunoff(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6966,&
    'frame/module_domain.f: Failed to allocate grid%udrunoff(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'ivgtyp').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%ivgtyp(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6975,&
    'frame/module_domain.f: Failed to allocate grid%ivgtyp(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ivgtyp=0
ELSE
  ALLOCATE(grid%ivgtyp(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6982,&
    'frame/module_domain.f: Failed to allocate grid%ivgtyp(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'isltyp').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%isltyp(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6991,&
    'frame/module_domain.f: Failed to allocate grid%isltyp(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%isltyp=0
ELSE
  ALLOCATE(grid%isltyp(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",6998,&
    'frame/module_domain.f: Failed to allocate grid%isltyp(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'vegfra').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%vegfra(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7007,&
    'frame/module_domain.f: Failed to allocate grid%vegfra(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%vegfra=initial_data_value
ELSE
  ALLOCATE(grid%vegfra(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7014,&
    'frame/module_domain.f: Failed to allocate grid%vegfra(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'sfcevp').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%sfcevp(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7023,&
    'frame/module_domain.f: Failed to allocate grid%sfcevp(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sfcevp=initial_data_value
ELSE
  ALLOCATE(grid%sfcevp(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7030,&
    'frame/module_domain.f: Failed to allocate grid%sfcevp(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'grdflx').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%grdflx(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7039,&
    'frame/module_domain.f: Failed to allocate grid%grdflx(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%grdflx=initial_data_value
ELSE
  ALLOCATE(grid%grdflx(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7046,&
    'frame/module_domain.f: Failed to allocate grid%grdflx(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'albbck').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%albbck(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7055,&
    'frame/module_domain.f: Failed to allocate grid%albbck(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%albbck=initial_data_value
ELSE
  ALLOCATE(grid%albbck(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7062,&
    'frame/module_domain.f: Failed to allocate grid%albbck(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'sfcexc').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%sfcexc(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7071,&
    'frame/module_domain.f: Failed to allocate grid%sfcexc(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sfcexc=initial_data_value
ELSE
  ALLOCATE(grid%sfcexc(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7078,&
    'frame/module_domain.f: Failed to allocate grid%sfcexc(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'snotime').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%snotime(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7087,&
    'frame/module_domain.f: Failed to allocate grid%snotime(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%snotime=initial_data_value
ELSE
  ALLOCATE(grid%snotime(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7094,&
    'frame/module_domain.f: Failed to allocate grid%snotime(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'acsnow').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%acsnow(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7103,&
    'frame/module_domain.f: Failed to allocate grid%acsnow(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%acsnow=initial_data_value
ELSE
  ALLOCATE(grid%acsnow(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7110,&
    'frame/module_domain.f: Failed to allocate grid%acsnow(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'acsnom').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%acsnom(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7119,&
    'frame/module_domain.f: Failed to allocate grid%acsnom(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%acsnom=initial_data_value
ELSE
  ALLOCATE(grid%acsnom(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7126,&
    'frame/module_domain.f: Failed to allocate grid%acsnom(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'rmol').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%rmol(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7135,&
    'frame/module_domain.f: Failed to allocate grid%rmol(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rmol=initial_data_value
ELSE
  ALLOCATE(grid%rmol(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7142,&
    'frame/module_domain.f: Failed to allocate grid%rmol(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'snow').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%snow(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7151,&
    'frame/module_domain.f: Failed to allocate grid%snow(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%snow=initial_data_value
ELSE
  ALLOCATE(grid%snow(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7158,&
    'frame/module_domain.f: Failed to allocate grid%snow(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'canwat').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%canwat(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7167,&
    'frame/module_domain.f: Failed to allocate grid%canwat(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%canwat=initial_data_value
ELSE
  ALLOCATE(grid%canwat(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7174,&
    'frame/module_domain.f: Failed to allocate grid%canwat(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'sst').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%sst(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7183,&
    'frame/module_domain.f: Failed to allocate grid%sst(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sst=initial_data_value
ELSE
  ALLOCATE(grid%sst(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7190,&
    'frame/module_domain.f: Failed to allocate grid%sst(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'weasd').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%weasd(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7199,&
    'frame/module_domain.f: Failed to allocate grid%weasd(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%weasd=initial_data_value
ELSE
  ALLOCATE(grid%weasd(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7206,&
    'frame/module_domain.f: Failed to allocate grid%weasd(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'znt').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%znt(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7215,&
    'frame/module_domain.f: Failed to allocate grid%znt(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%znt=initial_data_value
ELSE
  ALLOCATE(grid%znt(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7222,&
    'frame/module_domain.f: Failed to allocate grid%znt(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'mol').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%mol(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7231,&
    'frame/module_domain.f: Failed to allocate grid%mol(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%mol=initial_data_value
ELSE
  ALLOCATE(grid%mol(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7238,&
    'frame/module_domain.f: Failed to allocate grid%mol(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'noahres').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%noahres(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7247,&
    'frame/module_domain.f: Failed to allocate grid%noahres(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%noahres=initial_data_value
ELSE
  ALLOCATE(grid%noahres(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7254,&
    'frame/module_domain.f: Failed to allocate grid%noahres(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'tke_myj').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%tke_myj(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7263,&
    'frame/module_domain.f: Failed to allocate grid%tke_myj(sm31:em31,sm32:em32,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tke_myj=initial_data_value
ELSE
  ALLOCATE(grid%tke_myj(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7270,&
    'frame/module_domain.f: Failed to allocate grid%tke_myj(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'el_myj').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%el_myj(sm31:em31,sm33:em33,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7279,&
    'frame/module_domain.f: Failed to allocate grid%el_myj(sm31:em31,sm33:em33,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%el_myj=initial_data_value
ELSE
  ALLOCATE(grid%el_myj(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7286,&
    'frame/module_domain.f: Failed to allocate grid%el_myj(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'exch_h').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%exch_h(sm31:em31,sm33:em33,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7295,&
    'frame/module_domain.f: Failed to allocate grid%exch_h(sm31:em31,sm33:em33,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%exch_h=initial_data_value
ELSE
  ALLOCATE(grid%exch_h(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7302,&
    'frame/module_domain.f: Failed to allocate grid%exch_h(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'exch_m').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%exch_m(sm31:em31,sm33:em33,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7311,&
    'frame/module_domain.f: Failed to allocate grid%exch_m(sm31:em31,sm33:em33,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%exch_m=initial_data_value
ELSE
  ALLOCATE(grid%exch_m(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7318,&
    'frame/module_domain.f: Failed to allocate grid%exch_m(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'thz0').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%thz0(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7327,&
    'frame/module_domain.f: Failed to allocate grid%thz0(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%thz0=initial_data_value
ELSE
  ALLOCATE(grid%thz0(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7334,&
    'frame/module_domain.f: Failed to allocate grid%thz0(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'qz0').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%qz0(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7343,&
    'frame/module_domain.f: Failed to allocate grid%qz0(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%qz0=initial_data_value
ELSE
  ALLOCATE(grid%qz0(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7350,&
    'frame/module_domain.f: Failed to allocate grid%qz0(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'uz0').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%uz0(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7359,&
    'frame/module_domain.f: Failed to allocate grid%uz0(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%uz0=initial_data_value
ELSE
  ALLOCATE(grid%uz0(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7366,&
    'frame/module_domain.f: Failed to allocate grid%uz0(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'vz0').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%vz0(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7375,&
    'frame/module_domain.f: Failed to allocate grid%vz0(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%vz0=initial_data_value
ELSE
  ALLOCATE(grid%vz0(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7382,&
    'frame/module_domain.f: Failed to allocate grid%vz0(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'flhc').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%flhc(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7391,&
    'frame/module_domain.f: Failed to allocate grid%flhc(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%flhc=initial_data_value
ELSE
  ALLOCATE(grid%flhc(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7398,&
    'frame/module_domain.f: Failed to allocate grid%flhc(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'flqc').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%flqc(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7407,&
    'frame/module_domain.f: Failed to allocate grid%flqc(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%flqc=initial_data_value
ELSE
  ALLOCATE(grid%flqc(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7414,&
    'frame/module_domain.f: Failed to allocate grid%flqc(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'qsg').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%qsg(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7423,&
    'frame/module_domain.f: Failed to allocate grid%qsg(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%qsg=initial_data_value
ELSE
  ALLOCATE(grid%qsg(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7430,&
    'frame/module_domain.f: Failed to allocate grid%qsg(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'qvg').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%qvg(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7439,&
    'frame/module_domain.f: Failed to allocate grid%qvg(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%qvg=initial_data_value
ELSE
  ALLOCATE(grid%qvg(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7446,&
    'frame/module_domain.f: Failed to allocate grid%qvg(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'qcg').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%qcg(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7455,&
    'frame/module_domain.f: Failed to allocate grid%qcg(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%qcg=initial_data_value
ELSE
  ALLOCATE(grid%qcg(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7462,&
    'frame/module_domain.f: Failed to allocate grid%qcg(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'soilt1').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%soilt1(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7471,&
    'frame/module_domain.f: Failed to allocate grid%soilt1(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilt1=initial_data_value
ELSE
  ALLOCATE(grid%soilt1(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7478,&
    'frame/module_domain.f: Failed to allocate grid%soilt1(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'tsnav').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%tsnav(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7487,&
    'frame/module_domain.f: Failed to allocate grid%tsnav(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tsnav=initial_data_value
ELSE
  ALLOCATE(grid%tsnav(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7494,&
    'frame/module_domain.f: Failed to allocate grid%tsnav(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'psfc_out').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%psfc_out(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7503,&
    'frame/module_domain.f: Failed to allocate grid%psfc_out(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%psfc_out=initial_data_value
ELSE
  ALLOCATE(grid%psfc_out(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7510,&
    'frame/module_domain.f: Failed to allocate grid%psfc_out(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'uz0h').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%uz0h(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7519,&
    'frame/module_domain.f: Failed to allocate grid%uz0h(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%uz0h=initial_data_value
ELSE
  ALLOCATE(grid%uz0h(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7526,&
    'frame/module_domain.f: Failed to allocate grid%uz0h(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'vz0h').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%vz0h(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7535,&
    'frame/module_domain.f: Failed to allocate grid%vz0h(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%vz0h=initial_data_value
ELSE
  ALLOCATE(grid%vz0h(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7542,&
    'frame/module_domain.f: Failed to allocate grid%vz0h(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'dudt').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%dudt(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7551,&
    'frame/module_domain.f: Failed to allocate grid%dudt(sm31:em31,sm32:em32,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%dudt=initial_data_value
ELSE
  ALLOCATE(grid%dudt(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7558,&
    'frame/module_domain.f: Failed to allocate grid%dudt(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'dvdt').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%dvdt(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7567,&
    'frame/module_domain.f: Failed to allocate grid%dvdt(sm31:em31,sm32:em32,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%dvdt=initial_data_value
ELSE
  ALLOCATE(grid%dvdt(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7574,&
    'frame/module_domain.f: Failed to allocate grid%dvdt(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'qsfc').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%qsfc(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7583,&
    'frame/module_domain.f: Failed to allocate grid%qsfc(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%qsfc=initial_data_value
ELSE
  ALLOCATE(grid%qsfc(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7590,&
    'frame/module_domain.f: Failed to allocate grid%qsfc(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'akhs').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%akhs(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7599,&
    'frame/module_domain.f: Failed to allocate grid%akhs(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%akhs=initial_data_value
ELSE
  ALLOCATE(grid%akhs(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7606,&
    'frame/module_domain.f: Failed to allocate grid%akhs(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'akms').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%akms(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7615,&
    'frame/module_domain.f: Failed to allocate grid%akms(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%akms=initial_data_value
ELSE
  ALLOCATE(grid%akms(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7622,&
    'frame/module_domain.f: Failed to allocate grid%akms(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'htop').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%htop(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7631,&
    'frame/module_domain.f: Failed to allocate grid%htop(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%htop=initial_data_value
ELSE
  ALLOCATE(grid%htop(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7638,&
    'frame/module_domain.f: Failed to allocate grid%htop(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'hbot').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%hbot(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7647,&
    'frame/module_domain.f: Failed to allocate grid%hbot(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%hbot=initial_data_value
ELSE
  ALLOCATE(grid%hbot(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7654,&
    'frame/module_domain.f: Failed to allocate grid%hbot(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'htopr').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%htopr(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7663,&
    'frame/module_domain.f: Failed to allocate grid%htopr(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%htopr=initial_data_value
ELSE
  ALLOCATE(grid%htopr(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7670,&
    'frame/module_domain.f: Failed to allocate grid%htopr(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'hbotr').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%hbotr(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7679,&
    'frame/module_domain.f: Failed to allocate grid%hbotr(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%hbotr=initial_data_value
ELSE
  ALLOCATE(grid%hbotr(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7686,&
    'frame/module_domain.f: Failed to allocate grid%hbotr(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'htopd').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%htopd(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7695,&
    'frame/module_domain.f: Failed to allocate grid%htopd(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%htopd=initial_data_value
ELSE
  ALLOCATE(grid%htopd(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7702,&
    'frame/module_domain.f: Failed to allocate grid%htopd(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'hbotd').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%hbotd(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7711,&
    'frame/module_domain.f: Failed to allocate grid%hbotd(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%hbotd=initial_data_value
ELSE
  ALLOCATE(grid%hbotd(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7718,&
    'frame/module_domain.f: Failed to allocate grid%hbotd(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'htops').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%htops(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7727,&
    'frame/module_domain.f: Failed to allocate grid%htops(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%htops=initial_data_value
ELSE
  ALLOCATE(grid%htops(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7734,&
    'frame/module_domain.f: Failed to allocate grid%htops(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'hbots').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%hbots(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7743,&
    'frame/module_domain.f: Failed to allocate grid%hbots(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%hbots=initial_data_value
ELSE
  ALLOCATE(grid%hbots(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7750,&
    'frame/module_domain.f: Failed to allocate grid%hbots(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'cuppt').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%cuppt(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7759,&
    'frame/module_domain.f: Failed to allocate grid%cuppt(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%cuppt=initial_data_value
ELSE
  ALLOCATE(grid%cuppt(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7766,&
    'frame/module_domain.f: Failed to allocate grid%cuppt(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'cprate').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%cprate(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7775,&
    'frame/module_domain.f: Failed to allocate grid%cprate(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%cprate=initial_data_value
ELSE
  ALLOCATE(grid%cprate(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7782,&
    'frame/module_domain.f: Failed to allocate grid%cprate(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'f_ice_phy').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%f_ice_phy(sm31:em31,sm33:em33,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7791,&
    'frame/module_domain.f: Failed to allocate grid%f_ice_phy(sm31:em31,sm33:em33,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%f_ice_phy=initial_data_value
ELSE
  ALLOCATE(grid%f_ice_phy(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7798,&
    'frame/module_domain.f: Failed to allocate grid%f_ice_phy(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'f_rain_phy').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%f_rain_phy(sm31:em31,sm33:em33,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7807,&
    'frame/module_domain.f: Failed to allocate grid%f_rain_phy(sm31:em31,sm33:em33,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%f_rain_phy=initial_data_value
ELSE
  ALLOCATE(grid%f_rain_phy(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7814,&
    'frame/module_domain.f: Failed to allocate grid%f_rain_phy(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'f_rimef_phy').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%f_rimef_phy(sm31:em31,sm33:em33,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7823,&
    'frame/module_domain.f: Failed to allocate grid%f_rimef_phy(sm31:em31,sm33:em33,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%f_rimef_phy=initial_data_value
ELSE
  ALLOCATE(grid%f_rimef_phy(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7830,&
    'frame/module_domain.f: Failed to allocate grid%f_rimef_phy(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'mass_flux').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%mass_flux(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7839,&
    'frame/module_domain.f: Failed to allocate grid%mass_flux(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%mass_flux=initial_data_value
ELSE
  ALLOCATE(grid%mass_flux(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7846,&
    'frame/module_domain.f: Failed to allocate grid%mass_flux(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'apr_gr').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%apr_gr(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7855,&
    'frame/module_domain.f: Failed to allocate grid%apr_gr(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%apr_gr=initial_data_value
ELSE
  ALLOCATE(grid%apr_gr(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7862,&
    'frame/module_domain.f: Failed to allocate grid%apr_gr(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'apr_w').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%apr_w(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7871,&
    'frame/module_domain.f: Failed to allocate grid%apr_w(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%apr_w=initial_data_value
ELSE
  ALLOCATE(grid%apr_w(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7878,&
    'frame/module_domain.f: Failed to allocate grid%apr_w(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'apr_mc').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%apr_mc(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7887,&
    'frame/module_domain.f: Failed to allocate grid%apr_mc(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%apr_mc=initial_data_value
ELSE
  ALLOCATE(grid%apr_mc(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7894,&
    'frame/module_domain.f: Failed to allocate grid%apr_mc(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'apr_st').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%apr_st(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7903,&
    'frame/module_domain.f: Failed to allocate grid%apr_st(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%apr_st=initial_data_value
ELSE
  ALLOCATE(grid%apr_st(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7910,&
    'frame/module_domain.f: Failed to allocate grid%apr_st(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'apr_as').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%apr_as(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7919,&
    'frame/module_domain.f: Failed to allocate grid%apr_as(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%apr_as=initial_data_value
ELSE
  ALLOCATE(grid%apr_as(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7926,&
    'frame/module_domain.f: Failed to allocate grid%apr_as(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'apr_capma').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%apr_capma(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7935,&
    'frame/module_domain.f: Failed to allocate grid%apr_capma(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%apr_capma=initial_data_value
ELSE
  ALLOCATE(grid%apr_capma(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7942,&
    'frame/module_domain.f: Failed to allocate grid%apr_capma(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'apr_capme').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%apr_capme(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7951,&
    'frame/module_domain.f: Failed to allocate grid%apr_capme(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%apr_capme=initial_data_value
ELSE
  ALLOCATE(grid%apr_capme(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7958,&
    'frame/module_domain.f: Failed to allocate grid%apr_capme(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'apr_capmi').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%apr_capmi(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7967,&
    'frame/module_domain.f: Failed to allocate grid%apr_capmi(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%apr_capmi=initial_data_value
ELSE
  ALLOCATE(grid%apr_capmi(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7974,&
    'frame/module_domain.f: Failed to allocate grid%apr_capmi(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'xf_ens').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((model_config_rec%ensdim)-(1)+1))) * 4
  ALLOCATE(grid%xf_ens(sm31:em31,sm32:em32,1:model_config_rec%ensdim),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7983,&
    'frame/module_domain.f: Failed to allocate grid%xf_ens(sm31:em31,sm32:em32,1:model_config_rec%ensdim). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%xf_ens=initial_data_value
ELSE
  ALLOCATE(grid%xf_ens(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7990,&
    'frame/module_domain.f: Failed to allocate grid%xf_ens(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'pr_ens').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((model_config_rec%ensdim)-(1)+1))) * 4
  ALLOCATE(grid%pr_ens(sm31:em31,sm32:em32,1:model_config_rec%ensdim),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",7999,&
    'frame/module_domain.f: Failed to allocate grid%pr_ens(sm31:em31,sm32:em32,1:model_config_rec%ensdim). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%pr_ens=initial_data_value
ELSE
  ALLOCATE(grid%pr_ens(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",8006,&
    'frame/module_domain.f: Failed to allocate grid%pr_ens(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'rthften').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%rthften(sm31:em31,sm33:em33,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",8015,&
    'frame/module_domain.f: Failed to allocate grid%rthften(sm31:em31,sm33:em33,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rthften=initial_data_value
ELSE
  ALLOCATE(grid%rthften(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",8022,&
    'frame/module_domain.f: Failed to allocate grid%rthften(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'rqvften').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%rqvften(sm31:em31,sm33:em33,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",8031,&
    'frame/module_domain.f: Failed to allocate grid%rqvften(sm31:em31,sm33:em33,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rqvften=initial_data_value
ELSE
  ALLOCATE(grid%rqvften(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",8038,&
    'frame/module_domain.f: Failed to allocate grid%rqvften(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'snowh'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%snowh(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",8047,&
    'frame/module_domain.f: Failed to allocate grid%snowh(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%snowh=initial_data_value
ELSE
  ALLOCATE(grid%snowh(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",8054,&
    'frame/module_domain.f: Failed to allocate grid%snowh(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'rhosn'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%rhosn(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",8063,&
    'frame/module_domain.f: Failed to allocate grid%rhosn(sm31:em31,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rhosn=initial_data_value
ELSE
  ALLOCATE(grid%rhosn(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",8070,&
    'frame/module_domain.f: Failed to allocate grid%rhosn(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'smfr3d').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((model_config_rec%num_soil_layers)-(1)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%smfr3d(sm31:em31,1:model_config_rec%num_soil_layers,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",8079,&
    'frame/module_domain.f: Failed to allocate grid%smfr3d(sm31:em31,1:model_config_rec%num_soil_layers,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%smfr3d=initial_data_value
ELSE
  ALLOCATE(grid%smfr3d(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",8086,&
    'frame/module_domain.f: Failed to allocate grid%smfr3d(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'keepfr3dflag').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((model_config_rec%num_soil_layers)-(1)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%keepfr3dflag(sm31:em31,1:model_config_rec%num_soil_layers,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",8095,&
    'frame/module_domain.f: Failed to allocate grid%keepfr3dflag(sm31:em31,1:model_config_rec%num_soil_layers,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%keepfr3dflag=initial_data_value
ELSE
  ALLOCATE(grid%keepfr3dflag(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",8102,&
    'frame/module_domain.f: Failed to allocate grid%keepfr3dflag(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'mp_restart_state').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((7501)-(1)+1))) * 4
  ALLOCATE(grid%mp_restart_state(1:7501),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",8111,&
    'frame/module_domain.f: Failed to allocate grid%mp_restart_state(1:7501). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%mp_restart_state=initial_data_value
ELSE
  ALLOCATE(grid%mp_restart_state(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",8118,&
    'frame/module_domain.f: Failed to allocate grid%mp_restart_state(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'tbpvs_state').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((7501)-(1)+1))) * 4
  ALLOCATE(grid%tbpvs_state(1:7501),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",8127,&
    'frame/module_domain.f: Failed to allocate grid%tbpvs_state(1:7501). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tbpvs_state=initial_data_value
ELSE
  ALLOCATE(grid%tbpvs_state(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",8134,&
    'frame/module_domain.f: Failed to allocate grid%tbpvs_state(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'tbpvs0_state').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((7501)-(1)+1))) * 4
  ALLOCATE(grid%tbpvs0_state(1:7501),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",8143,&
    'frame/module_domain.f: Failed to allocate grid%tbpvs0_state(1:7501). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tbpvs0_state=initial_data_value
ELSE
  ALLOCATE(grid%tbpvs0_state(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",8150,&
    'frame/module_domain.f: Failed to allocate grid%tbpvs0_state(1).  ')
  endif
ENDIF
IF ( setinitval .EQ. 3 ) grid%landuse_isice=0
IF ( setinitval .EQ. 3 ) grid%landuse_lucats=0
IF ( setinitval .EQ. 3 ) grid%landuse_luseas=0
IF ( setinitval .EQ. 3 ) grid%landuse_isn=0
IF(in_use_for_config(id,'lu_state').AND.(.NOT.inter_domain))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((7501)-(1)+1))) * 4
  ALLOCATE(grid%lu_state(1:7501),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",8163,&
    'frame/module_domain.f: Failed to allocate grid%lu_state(1:7501). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%lu_state=initial_data_value
ELSE
  ALLOCATE(grid%lu_state(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("",8170,&
    'frame/module_domain.f: Failed to allocate grid%lu_state(1).  ')
  endif
ENDIF
IF ( setinitval .EQ. 3 ) grid%number_at_same_level=0
IF ( setinitval .EQ. 3 ) grid%itimestep=0
IF ( setinitval .EQ. 3 ) grid%xtime=initial_data_value
IF ( setinitval .EQ. 3 ) grid%julian=initial_data_value
IF ( setinitval .EQ. 3 ) grid%lbc_fid=0
IF ( setinitval .EQ. 3 ) grid%tiled=.FALSE.
IF ( setinitval .EQ. 3 ) grid%patched=.FALSE.
IF ( setinitval .EQ. 3 ) grid%run_days=0
IF ( setinitval .EQ. 3 ) grid%run_hours=0
IF ( setinitval .EQ. 3 ) grid%run_minutes=0
IF ( setinitval .EQ. 3 ) grid%run_seconds=0
IF ( setinitval .EQ. 3 ) grid%start_year=0
IF ( setinitval .EQ. 3 ) grid%start_month=0
IF ( setinitval .EQ. 3 ) grid%start_day=0
IF ( setinitval .EQ. 3 ) grid%start_hour=0
IF ( setinitval .EQ. 3 ) grid%start_minute=0
IF ( setinitval .EQ. 3 ) grid%start_second=0
IF ( setinitval .EQ. 3 ) grid%end_year=0
IF ( setinitval .EQ. 3 ) grid%end_month=0
IF ( setinitval .EQ. 3 ) grid%end_day=0
IF ( setinitval .EQ. 3 ) grid%end_hour=0
IF ( setinitval .EQ. 3 ) grid%end_minute=0
IF ( setinitval .EQ. 3 ) grid%end_second=0
IF ( setinitval .EQ. 3 ) grid%interval_seconds=0
IF ( setinitval .EQ. 3 ) grid%input_from_file=.FALSE.
IF ( setinitval .EQ. 3 ) grid%fine_input_stream=0
IF ( setinitval .EQ. 3 ) grid%oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_oid=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_oid=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_oid=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_oid=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_oid=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_oid=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_oid=0
IF ( setinitval .EQ. 3 ) grid%auxinput6_oid=0
IF ( setinitval .EQ. 3 ) grid%auxinput7_oid=0
IF ( setinitval .EQ. 3 ) grid%auxinput8_oid=0
IF ( setinitval .EQ. 3 ) grid%auxinput9_oid=0
IF ( setinitval .EQ. 3 ) grid%auxinput10_oid=0
IF ( setinitval .EQ. 3 ) grid%auxinput11_oid=0
IF ( setinitval .EQ. 3 ) grid%history_interval=0
IF ( setinitval .EQ. 3 ) grid%frames_per_outfile=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist1=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist2=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist3=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist4=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist5=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist6=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist7=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist8=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist9=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist10=0
IF ( setinitval .EQ. 3 ) grid%frames_per_auxhist11=0
IF ( setinitval .EQ. 3 ) grid%restart=.FALSE.
IF ( setinitval .EQ. 3 ) grid%restart_interval=0
IF ( setinitval .EQ. 3 ) grid%io_form_input=0
IF ( setinitval .EQ. 3 ) grid%io_form_history=0
IF ( setinitval .EQ. 3 ) grid%io_form_restart=0
IF ( setinitval .EQ. 3 ) grid%io_form_boundary=0
IF ( setinitval .EQ. 3 ) grid%debug_level=0
IF ( setinitval .EQ. 3 ) grid%self_test_domain=.FALSE.
IF ( setinitval .EQ. 3 ) grid%history_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%history_interval_d=0
IF ( setinitval .EQ. 3 ) grid%history_interval_h=0
IF ( setinitval .EQ. 3 ) grid%history_interval_m=0
IF ( setinitval .EQ. 3 ) grid%history_interval_s=0
IF ( setinitval .EQ. 3 ) grid%inputout_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%inputout_interval_d=0
IF ( setinitval .EQ. 3 ) grid%inputout_interval_h=0
IF ( setinitval .EQ. 3 ) grid%inputout_interval_m=0
IF ( setinitval .EQ. 3 ) grid%inputout_interval_s=0
IF ( setinitval .EQ. 3 ) grid%inputout_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_interval=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_interval=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_interval=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_interval=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_interval=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_interval=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_interval=0
IF ( setinitval .EQ. 3 ) grid%auxinput6_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput6_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput6_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput6_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput6_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput6_interval=0
IF ( setinitval .EQ. 3 ) grid%auxinput7_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput7_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput7_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput7_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput7_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput7_interval=0
IF ( setinitval .EQ. 3 ) grid%auxinput8_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput8_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput8_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput8_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput8_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput8_interval=0
IF ( setinitval .EQ. 3 ) grid%sgfdda_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%sgfdda_interval_d=0
IF ( setinitval .EQ. 3 ) grid%sgfdda_interval_h=0
IF ( setinitval .EQ. 3 ) grid%sgfdda_interval_m=0
IF ( setinitval .EQ. 3 ) grid%sgfdda_interval_s=0
IF ( setinitval .EQ. 3 ) grid%sgfdda_interval=0
IF ( setinitval .EQ. 3 ) grid%gfdda_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%gfdda_interval_d=0
IF ( setinitval .EQ. 3 ) grid%gfdda_interval_h=0
IF ( setinitval .EQ. 3 ) grid%gfdda_interval_m=0
IF ( setinitval .EQ. 3 ) grid%gfdda_interval_s=0
IF ( setinitval .EQ. 3 ) grid%gfdda_interval=0
IF ( setinitval .EQ. 3 ) grid%auxinput11_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput11_interval_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput11_interval_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput11_interval_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput11_interval_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput11_interval=0
IF ( setinitval .EQ. 3 ) grid%restart_interval_mo=0
IF ( setinitval .EQ. 3 ) grid%restart_interval_d=0
IF ( setinitval .EQ. 3 ) grid%restart_interval_h=0
IF ( setinitval .EQ. 3 ) grid%restart_interval_m=0
IF ( setinitval .EQ. 3 ) grid%restart_interval_s=0
IF ( setinitval .EQ. 3 ) grid%history_begin_y=0
IF ( setinitval .EQ. 3 ) grid%history_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%history_begin_d=0
IF ( setinitval .EQ. 3 ) grid%history_begin_h=0
IF ( setinitval .EQ. 3 ) grid%history_begin_m=0
IF ( setinitval .EQ. 3 ) grid%history_begin_s=0
IF ( setinitval .EQ. 3 ) grid%inputout_begin_y=0
IF ( setinitval .EQ. 3 ) grid%inputout_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%inputout_begin_d=0
IF ( setinitval .EQ. 3 ) grid%inputout_begin_h=0
IF ( setinitval .EQ. 3 ) grid%inputout_begin_m=0
IF ( setinitval .EQ. 3 ) grid%inputout_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput6_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput6_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput6_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput6_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput6_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput6_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput7_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput7_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput7_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput7_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput7_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput7_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput8_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput8_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput8_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput8_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput8_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput8_begin_s=0
IF ( setinitval .EQ. 3 ) grid%sgfdda_begin_y=0
IF ( setinitval .EQ. 3 ) grid%sgfdda_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%sgfdda_begin_d=0
IF ( setinitval .EQ. 3 ) grid%sgfdda_begin_h=0
IF ( setinitval .EQ. 3 ) grid%sgfdda_begin_m=0
IF ( setinitval .EQ. 3 ) grid%sgfdda_begin_s=0
IF ( setinitval .EQ. 3 ) grid%gfdda_begin_y=0
IF ( setinitval .EQ. 3 ) grid%gfdda_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%gfdda_begin_d=0
IF ( setinitval .EQ. 3 ) grid%gfdda_begin_h=0
IF ( setinitval .EQ. 3 ) grid%gfdda_begin_m=0
IF ( setinitval .EQ. 3 ) grid%gfdda_begin_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput11_begin_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput11_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput11_begin_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput11_begin_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput11_begin_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput11_begin_s=0
IF ( setinitval .EQ. 3 ) grid%restart_begin_y=0
IF ( setinitval .EQ. 3 ) grid%restart_begin_mo=0
IF ( setinitval .EQ. 3 ) grid%restart_begin_d=0
IF ( setinitval .EQ. 3 ) grid%restart_begin_h=0
IF ( setinitval .EQ. 3 ) grid%restart_begin_m=0
IF ( setinitval .EQ. 3 ) grid%restart_begin_s=0
IF ( setinitval .EQ. 3 ) grid%history_end_y=0
IF ( setinitval .EQ. 3 ) grid%history_end_mo=0
IF ( setinitval .EQ. 3 ) grid%history_end_d=0
IF ( setinitval .EQ. 3 ) grid%history_end_h=0
IF ( setinitval .EQ. 3 ) grid%history_end_m=0
IF ( setinitval .EQ. 3 ) grid%history_end_s=0
IF ( setinitval .EQ. 3 ) grid%inputout_end_y=0
IF ( setinitval .EQ. 3 ) grid%inputout_end_mo=0
IF ( setinitval .EQ. 3 ) grid%inputout_end_d=0
IF ( setinitval .EQ. 3 ) grid%inputout_end_h=0
IF ( setinitval .EQ. 3 ) grid%inputout_end_m=0
IF ( setinitval .EQ. 3 ) grid%inputout_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist1_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist2_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist3_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist4_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist5_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist6_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist7_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist8_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist9_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist10_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxhist11_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput1_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput2_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput3_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput4_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput5_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput6_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput6_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput6_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput6_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput6_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput6_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput7_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput7_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput7_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput7_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput7_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput7_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput8_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput8_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput8_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput8_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput8_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput8_end_s=0
IF ( setinitval .EQ. 3 ) grid%sgfdda_end_y=0
IF ( setinitval .EQ. 3 ) grid%sgfdda_end_mo=0
IF ( setinitval .EQ. 3 ) grid%sgfdda_end_d=0
IF ( setinitval .EQ. 3 ) grid%sgfdda_end_h=0
IF ( setinitval .EQ. 3 ) grid%sgfdda_end_m=0
IF ( setinitval .EQ. 3 ) grid%sgfdda_end_s=0
IF ( setinitval .EQ. 3 ) grid%gfdda_end_y=0
IF ( setinitval .EQ. 3 ) grid%gfdda_end_mo=0
IF ( setinitval .EQ. 3 ) grid%gfdda_end_d=0
IF ( setinitval .EQ. 3 ) grid%gfdda_end_h=0
IF ( setinitval .EQ. 3 ) grid%gfdda_end_m=0
IF ( setinitval .EQ. 3 ) grid%gfdda_end_s=0
IF ( setinitval .EQ. 3 ) grid%auxinput11_end_y=0
IF ( setinitval .EQ. 3 ) grid%auxinput11_end_mo=0
IF ( setinitval .EQ. 3 ) grid%auxinput11_end_d=0
IF ( setinitval .EQ. 3 ) grid%auxinput11_end_h=0
IF ( setinitval .EQ. 3 ) grid%auxinput11_end_m=0
IF ( setinitval .EQ. 3 ) grid%auxinput11_end_s=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxinput1=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxinput2=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxinput3=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxinput4=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxinput5=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxinput6=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxinput7=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxinput8=0
IF ( setinitval .EQ. 3 ) grid%io_form_sgfdda=0
IF ( setinitval .EQ. 3 ) grid%io_form_gfdda=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxinput11=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist1=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist2=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist3=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist4=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist5=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist6=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist7=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist8=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist9=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist10=0
IF ( setinitval .EQ. 3 ) grid%io_form_auxhist11=0
IF ( setinitval .EQ. 3 ) grid%simulation_start_year=0
IF ( setinitval .EQ. 3 ) grid%simulation_start_month=0
IF ( setinitval .EQ. 3 ) grid%simulation_start_day=0
IF ( setinitval .EQ. 3 ) grid%simulation_start_hour=0
IF ( setinitval .EQ. 3 ) grid%simulation_start_minute=0
IF ( setinitval .EQ. 3 ) grid%simulation_start_second=0
IF ( setinitval .EQ. 3 ) grid%reset_simulation_start=.FALSE.
IF ( setinitval .EQ. 3 ) grid%sr_x=0
IF ( setinitval .EQ. 3 ) grid%sr_y=0
IF ( setinitval .EQ. 3 ) grid%julyr=0
IF ( setinitval .EQ. 3 ) grid%julday=0
IF ( setinitval .EQ. 3 ) grid%gmt=initial_data_value
IF ( setinitval .EQ. 3 ) grid%write_input=.FALSE.
IF ( setinitval .EQ. 3 ) grid%write_restart_at_0h=.FALSE.
IF ( setinitval .EQ. 3 ) grid%adjust_output_times=.FALSE.
IF ( setinitval .EQ. 3 ) grid%adjust_input_times=.FALSE.
IF ( setinitval .EQ. 3 ) grid%tstart=initial_data_value
IF ( setinitval .EQ. 3 ) grid%nocolons=.FALSE.
IF ( setinitval .EQ. 3 ) grid%cycling=.FALSE.
IF ( setinitval .EQ. 3 ) grid%dfi_opt=0
IF ( setinitval .EQ. 3 ) grid%dfi_nfilter=0
IF ( setinitval .EQ. 3 ) grid%dfi_write_filtered_input=.FALSE.
IF ( setinitval .EQ. 3 ) grid%dfi_write_dfi_history=.FALSE.
IF ( setinitval .EQ. 3 ) grid%dfi_cutoff_seconds=0
IF ( setinitval .EQ. 3 ) grid%dfi_time_dim=0
IF ( setinitval .EQ. 3 ) grid%dfi_fwdstop_year=0
IF ( setinitval .EQ. 3 ) grid%dfi_fwdstop_month=0
IF ( setinitval .EQ. 3 ) grid%dfi_fwdstop_day=0
IF ( setinitval .EQ. 3 ) grid%dfi_fwdstop_hour=0
IF ( setinitval .EQ. 3 ) grid%dfi_fwdstop_minute=0
IF ( setinitval .EQ. 3 ) grid%dfi_fwdstop_second=0
IF ( setinitval .EQ. 3 ) grid%dfi_bckstop_year=0
IF ( setinitval .EQ. 3 ) grid%dfi_bckstop_month=0
IF ( setinitval .EQ. 3 ) grid%dfi_bckstop_day=0
IF ( setinitval .EQ. 3 ) grid%dfi_bckstop_hour=0
IF ( setinitval .EQ. 3 ) grid%dfi_bckstop_minute=0
IF ( setinitval .EQ. 3 ) grid%dfi_bckstop_second=0
IF ( setinitval .EQ. 3 ) grid%time_step=0
IF ( setinitval .EQ. 3 ) grid%time_step_fract_num=0
IF ( setinitval .EQ. 3 ) grid%time_step_fract_den=0
IF ( setinitval .EQ. 3 ) grid%max_dom=0
IF ( setinitval .EQ. 3 ) grid%s_we=0
IF ( setinitval .EQ. 3 ) grid%e_we=0
IF ( setinitval .EQ. 3 ) grid%s_sn=0
IF ( setinitval .EQ. 3 ) grid%e_sn=0
IF ( setinitval .EQ. 3 ) grid%s_vert=0
IF ( setinitval .EQ. 3 ) grid%e_vert=0
IF ( setinitval .EQ. 3 ) grid%dx=initial_data_value
IF ( setinitval .EQ. 3 ) grid%dy=initial_data_value
IF ( setinitval .EQ. 3 ) grid%grid_id=0
IF ( setinitval .EQ. 3 ) grid%grid_allowed=.FALSE.
IF ( setinitval .EQ. 3 ) grid%parent_id=0
IF ( setinitval .EQ. 3 ) grid%i_parent_start=0
IF ( setinitval .EQ. 3 ) grid%j_parent_start=0
IF ( setinitval .EQ. 3 ) grid%parent_grid_ratio=0
IF ( setinitval .EQ. 3 ) grid%parent_time_step_ratio=0
IF ( setinitval .EQ. 3 ) grid%feedback=0
IF ( setinitval .EQ. 3 ) grid%smooth_option=0
IF ( setinitval .EQ. 3 ) grid%ztop=initial_data_value
IF ( setinitval .EQ. 3 ) grid%moad_grid_ratio=0
IF ( setinitval .EQ. 3 ) grid%moad_time_step_ratio=0
IF ( setinitval .EQ. 3 ) grid%shw=0
IF ( setinitval .EQ. 3 ) grid%tile_sz_x=0
IF ( setinitval .EQ. 3 ) grid%tile_sz_y=0
IF ( setinitval .EQ. 3 ) grid%numtiles=0
IF ( setinitval .EQ. 3 ) grid%nproc_x=0
IF ( setinitval .EQ. 3 ) grid%nproc_y=0
IF ( setinitval .EQ. 3 ) grid%irand=0
IF ( setinitval .EQ. 3 ) grid%dt=initial_data_value
IF ( setinitval .EQ. 3 ) grid%ts_buf_size=0
IF ( setinitval .EQ. 3 ) grid%max_ts_locs=0
IF ( setinitval .EQ. 3 ) grid%num_moves=0
IF ( setinitval .EQ. 3 ) grid%move_id=0
IF ( setinitval .EQ. 3 ) grid%move_interval=0
IF ( setinitval .EQ. 3 ) grid%move_cd_x=0
IF ( setinitval .EQ. 3 ) grid%move_cd_y=0
IF ( setinitval .EQ. 3 ) grid%swap_x=.FALSE.
IF ( setinitval .EQ. 3 ) grid%swap_y=.FALSE.
IF ( setinitval .EQ. 3 ) grid%cycle_x=.FALSE.
IF ( setinitval .EQ. 3 ) grid%cycle_y=.FALSE.
IF ( setinitval .EQ. 3 ) grid%reorder_mesh=.FALSE.
IF ( setinitval .EQ. 3 ) grid%perturb_input=.FALSE.
IF ( setinitval .EQ. 3 ) grid%eta_levels=initial_data_value
IF ( setinitval .EQ. 3 ) grid%ptsgm=initial_data_value
IF ( setinitval .EQ. 3 ) grid%num_metgrid_levels=0
IF ( setinitval .EQ. 3 ) grid%p_top_requested=initial_data_value
IF ( setinitval .EQ. 3 ) grid%mp_physics=0
IF ( setinitval .EQ. 3 ) grid%ra_lw_physics=0
IF ( setinitval .EQ. 3 ) grid%ra_sw_physics=0
IF ( setinitval .EQ. 3 ) grid%radt=initial_data_value
IF ( setinitval .EQ. 3 ) grid%sf_sfclay_physics=0
IF ( setinitval .EQ. 3 ) grid%sf_surface_physics=0
IF ( setinitval .EQ. 3 ) grid%bl_pbl_physics=0
IF ( setinitval .EQ. 3 ) grid%sf_urban_physics=0
IF ( setinitval .EQ. 3 ) grid%bldt=initial_data_value
IF ( setinitval .EQ. 3 ) grid%cu_physics=0
IF ( setinitval .EQ. 3 ) grid%cudt=initial_data_value
IF ( setinitval .EQ. 3 ) grid%gsmdt=initial_data_value
IF ( setinitval .EQ. 3 ) grid%isfflx=0
IF ( setinitval .EQ. 3 ) grid%ifsnow=0
IF ( setinitval .EQ. 3 ) grid%icloud=0
IF ( setinitval .EQ. 3 ) grid%swrad_scat=initial_data_value
IF ( setinitval .EQ. 3 ) grid%surface_input_source=0
IF ( setinitval .EQ. 3 ) grid%num_soil_layers=0
IF ( setinitval .EQ. 3 ) grid%num_urban_layers=0
IF ( setinitval .EQ. 3 ) grid%maxiens=0
IF ( setinitval .EQ. 3 ) grid%maxens=0
IF ( setinitval .EQ. 3 ) grid%maxens2=0
IF ( setinitval .EQ. 3 ) grid%maxens3=0
IF ( setinitval .EQ. 3 ) grid%ensdim=0
IF ( setinitval .EQ. 3 ) grid%chem_opt=0
IF ( setinitval .EQ. 3 ) grid%num_land_cat=0
IF ( setinitval .EQ. 3 ) grid%num_soil_cat=0
IF ( setinitval .EQ. 3 ) grid%mp_zero_out=0
IF ( setinitval .EQ. 3 ) grid%mp_zero_out_thresh=initial_data_value
IF ( setinitval .EQ. 3 ) grid%seaice_threshold=initial_data_value
IF ( setinitval .EQ. 3 ) grid%fractional_seaice=0
IF ( setinitval .EQ. 3 ) grid%sst_update=0
IF ( setinitval .EQ. 3 ) grid%usemonalb=.FALSE.
IF ( setinitval .EQ. 3 ) grid%rdmaxalb=.FALSE.
IF ( setinitval .EQ. 3 ) grid%rdlai2d=.FALSE.
IF ( setinitval .EQ. 3 ) grid%gwd_opt=0
IF ( setinitval .EQ. 3 ) grid%idtad=0
IF ( setinitval .EQ. 3 ) grid%nsoil=0
IF ( setinitval .EQ. 3 ) grid%nphs=0
IF ( setinitval .EQ. 3 ) grid%ncnvc=0
IF ( setinitval .EQ. 3 ) grid%nrads=0
IF ( setinitval .EQ. 3 ) grid%nradl=0
IF ( setinitval .EQ. 3 ) grid%tprec=initial_data_value
IF ( setinitval .EQ. 3 ) grid%theat=initial_data_value
IF ( setinitval .EQ. 3 ) grid%tclod=initial_data_value
IF ( setinitval .EQ. 3 ) grid%trdsw=initial_data_value
IF ( setinitval .EQ. 3 ) grid%trdlw=initial_data_value
IF ( setinitval .EQ. 3 ) grid%tsrfc=initial_data_value
IF ( setinitval .EQ. 3 ) grid%pcpflg=.FALSE.
IF ( setinitval .EQ. 3 ) grid%sigma=0
IF ( setinitval .EQ. 3 ) grid%sfenth=initial_data_value
IF ( setinitval .EQ. 3 ) grid%co2tf=0
IF ( setinitval .EQ. 3 ) grid%ra_call_offset=0
IF ( setinitval .EQ. 3 ) grid%cam_abs_freq_s=initial_data_value
IF ( setinitval .EQ. 3 ) grid%levsiz=0
IF ( setinitval .EQ. 3 ) grid%paerlev=0
IF ( setinitval .EQ. 3 ) grid%cam_abs_dim1=0
IF ( setinitval .EQ. 3 ) grid%cam_abs_dim2=0
IF ( setinitval .EQ. 3 ) grid%cu_rad_feedback=.FALSE.
IF ( setinitval .EQ. 3 ) grid%dyn_opt=0
IF ( setinitval .EQ. 3 ) grid%rk_ord=0
IF ( setinitval .EQ. 3 ) grid%w_damping=0
IF ( setinitval .EQ. 3 ) grid%diff_opt=0
IF ( setinitval .EQ. 3 ) grid%km_opt=0
IF ( setinitval .EQ. 3 ) grid%damp_opt=0
IF ( setinitval .EQ. 3 ) grid%zdamp=initial_data_value
IF ( setinitval .EQ. 3 ) grid%base_pres=initial_data_value
IF ( setinitval .EQ. 3 ) grid%base_temp=initial_data_value
IF ( setinitval .EQ. 3 ) grid%base_lapse=initial_data_value
IF ( setinitval .EQ. 3 ) grid%iso_temp=initial_data_value
IF ( setinitval .EQ. 3 ) grid%dampcoef=initial_data_value
IF ( setinitval .EQ. 3 ) grid%khdif=initial_data_value
IF ( setinitval .EQ. 3 ) grid%kvdif=initial_data_value
IF ( setinitval .EQ. 3 ) grid%c_s=initial_data_value
IF ( setinitval .EQ. 3 ) grid%c_k=initial_data_value
IF ( setinitval .EQ. 3 ) grid%smdiv=initial_data_value
IF ( setinitval .EQ. 3 ) grid%emdiv=initial_data_value
IF ( setinitval .EQ. 3 ) grid%epssm=initial_data_value
IF ( setinitval .EQ. 3 ) grid%non_hydrostatic=.FALSE.
IF ( setinitval .EQ. 3 ) grid%time_step_sound=0
IF ( setinitval .EQ. 3 ) grid%h_mom_adv_order=0
IF ( setinitval .EQ. 3 ) grid%v_mom_adv_order=0
IF ( setinitval .EQ. 3 ) grid%h_sca_adv_order=0
IF ( setinitval .EQ. 3 ) grid%v_sca_adv_order=0
IF ( setinitval .EQ. 3 ) grid%top_radiation=.FALSE.
IF ( setinitval .EQ. 3 ) grid%tke_upper_bound=initial_data_value
IF ( setinitval .EQ. 3 ) grid%tke_drag_coefficient=initial_data_value
IF ( setinitval .EQ. 3 ) grid%tke_heat_flux=initial_data_value
IF ( setinitval .EQ. 3 ) grid%pert_coriolis=.FALSE.
IF ( setinitval .EQ. 3 ) grid%euler_adv=.FALSE.
IF ( setinitval .EQ. 3 ) grid%idtadt=0
IF ( setinitval .EQ. 3 ) grid%idtadc=0
IF ( setinitval .EQ. 3 ) grid%boundary_flux=.FALSE.
IF ( setinitval .EQ. 3 ) grid%spec_bdy_width=0
IF ( setinitval .EQ. 3 ) grid%spec_zone=0
IF ( setinitval .EQ. 3 ) grid%relax_zone=0
IF ( setinitval .EQ. 3 ) grid%specified=.FALSE.
IF ( setinitval .EQ. 3 ) grid%periodic_x=.FALSE.
IF ( setinitval .EQ. 3 ) grid%symmetric_xs=.FALSE.
IF ( setinitval .EQ. 3 ) grid%symmetric_xe=.FALSE.
IF ( setinitval .EQ. 3 ) grid%open_xs=.FALSE.
IF ( setinitval .EQ. 3 ) grid%open_xe=.FALSE.
IF ( setinitval .EQ. 3 ) grid%periodic_y=.FALSE.
IF ( setinitval .EQ. 3 ) grid%symmetric_ys=.FALSE.
IF ( setinitval .EQ. 3 ) grid%symmetric_ye=.FALSE.
IF ( setinitval .EQ. 3 ) grid%open_ys=.FALSE.
IF ( setinitval .EQ. 3 ) grid%open_ye=.FALSE.
IF ( setinitval .EQ. 3 ) grid%polar=.FALSE.
IF ( setinitval .EQ. 3 ) grid%nested=.FALSE.
IF ( setinitval .EQ. 3 ) grid%real_data_init_type=0
IF ( setinitval .EQ. 3 ) grid%background_proc_id=0
IF ( setinitval .EQ. 3 ) grid%forecast_proc_id=0
IF ( setinitval .EQ. 3 ) grid%production_status=0
IF ( setinitval .EQ. 3 ) grid%compression=0
IF ( setinitval .EQ. 3 ) grid%cen_lat=initial_data_value
IF ( setinitval .EQ. 3 ) grid%cen_lon=initial_data_value
IF ( setinitval .EQ. 3 ) grid%truelat1=initial_data_value
IF ( setinitval .EQ. 3 ) grid%truelat2=initial_data_value
IF ( setinitval .EQ. 3 ) grid%moad_cen_lat=initial_data_value
IF ( setinitval .EQ. 3 ) grid%stand_lon=initial_data_value
IF ( setinitval .EQ. 3 ) grid%flag_metgrid=0
IF ( setinitval .EQ. 3 ) grid%flag_snow=0
IF ( setinitval .EQ. 3 ) grid%flag_psfc=0
IF ( setinitval .EQ. 3 ) grid%flag_sm000010=0
IF ( setinitval .EQ. 3 ) grid%flag_sm010040=0
IF ( setinitval .EQ. 3 ) grid%flag_sm040100=0
IF ( setinitval .EQ. 3 ) grid%flag_sm100200=0
IF ( setinitval .EQ. 3 ) grid%flag_st000010=0
IF ( setinitval .EQ. 3 ) grid%flag_st010040=0
IF ( setinitval .EQ. 3 ) grid%flag_st040100=0
IF ( setinitval .EQ. 3 ) grid%flag_st100200=0
IF ( setinitval .EQ. 3 ) grid%flag_slp=0
IF ( setinitval .EQ. 3 ) grid%flag_soilhgt=0
IF ( setinitval .EQ. 3 ) grid%flag_mf_xy=0
IF ( setinitval .EQ. 3 ) grid%bdyfrq=initial_data_value
IF ( setinitval .EQ. 3 ) grid%iswater=0
IF ( setinitval .EQ. 3 ) grid%islake=0
IF ( setinitval .EQ. 3 ) grid%isice=0
IF ( setinitval .EQ. 3 ) grid%isurban=0
IF ( setinitval .EQ. 3 ) grid%isoilwater=0
IF ( setinitval .EQ. 3 ) grid%map_proj=0
IF ( setinitval .EQ. 3 ) grid%dfi_stage=0
IF ( setinitval .EQ. 3 ) grid%mp_physics_dfi=0
IF ( setinitval .EQ. 3 ) grid%nodyn_dummy=0


      WRITE(message,*)&
          'alloc_space_field: domain ',id,', ',num_bytes_allocated,' bytes allocated'
      CALL  wrf_debug( 1, message )

   END SUBROUTINE alloc_space_field_core
END MODULE module_alloc_space

