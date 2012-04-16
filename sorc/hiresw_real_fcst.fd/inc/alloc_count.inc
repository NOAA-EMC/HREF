!STARTOFREGISTRYGENERATEDINCLUDE 'inc/alloc_count.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
IF(in_use_for_config(id,'x_1'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%x_1(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%x_1(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'x_2'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%x_2(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%x_2(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'lu_index'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%lu_index(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%lu_index(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'lu_mask'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%lu_mask(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%lu_mask(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'p_gc'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((model_config_rec%num_metgrid_levels)-(1)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%p_gc(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%p_gc(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'vegcat'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%vegcat(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%vegcat(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'soilcat'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%soilcat(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%soilcat(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'input_soil_cat'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%input_soil_cat(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%input_soil_cat(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'tsk_gc'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%tsk_gc(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%tsk_gc(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'xice_gc'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%xice_gc(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%xice_gc(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'ght_gc'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((model_config_rec%num_metgrid_levels)-(1)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%ght_gc(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%ght_gc(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'rh_gc'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((model_config_rec%num_metgrid_levels)-(1)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%rh_gc(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%rh_gc(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'v_gc'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((model_config_rec%num_metgrid_levels)-(1)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%v_gc(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%v_gc(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'u_gc'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((model_config_rec%num_metgrid_levels)-(1)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%u_gc(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%u_gc(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'t_gc'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((model_config_rec%num_metgrid_levels)-(1)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%t_gc(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%t_gc(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'snoalb'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%snoalb(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%snoalb(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'greenfrac_gc'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((12)-(1)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%greenfrac_gc(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%greenfrac_gc(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'albedo12m_gc'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((12)-(1)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%albedo12m_gc(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%albedo12m_gc(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'soilcbot_gc'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((model_config_rec%num_soil_cat)-(1)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%soilcbot_gc(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%soilcbot_gc(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'soilctop_gc'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((model_config_rec%num_soil_cat)-(1)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%soilctop_gc(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%soilctop_gc(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'tmn_gc'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%tmn_gc(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%tmn_gc(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'htv_gc'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%htv_gc(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%htv_gc(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'ht_gc'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%ht_gc(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%ht_gc(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'landusef_gc'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((model_config_rec%num_land_cat)-(1)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%landusef_gc(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%landusef_gc(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'vlon_gc'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%vlon_gc(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%vlon_gc(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'vlat_gc'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%vlat_gc(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%vlat_gc(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'hlon_gc'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%hlon_gc(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%hlon_gc(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'hlat_gc'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%hlat_gc(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%hlat_gc(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'hbm2'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%hbm2(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%hbm2(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'hbm3'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%hbm3(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%hbm3(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'vbm2'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%vbm2(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%vbm2(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'vbm3'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%vbm3(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%vbm3(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'sm'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%sm(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%sm(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'sice'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%sice(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%sice(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'pd'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%pd(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%pd(1,1).  ')
  endif
ENDIF
IF(.TRUE.)THEN
  num_bytes_allocated = num_bytes_allocated + &
(((em32-sm32+1)*(spec_bdy_width))) * RWORDSIZE
  num_bytes_allocated = num_bytes_allocated + &
(((em32-sm32+1)*(spec_bdy_width))) * RWORDSIZE
  num_bytes_allocated = num_bytes_allocated + &
(((em31-sm31+1)*(spec_bdy_width))) * RWORDSIZE
  num_bytes_allocated = num_bytes_allocated + &
(((em31-sm31+1)*(spec_bdy_width))) * RWORDSIZE
ELSE
  ALLOCATE(grid%pd_bxs(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%pd_bxs(1,1,1).  ')
  endif
  ALLOCATE(grid%pd_bxe(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%pd_bxe(1,1,1).  ')
  endif
  ALLOCATE(grid%pd_bys(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%pd_bys(1,1,1).  ')
  endif
  ALLOCATE(grid%pd_bye(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%pd_bye(1,1,1).  ')
  endif
ENDIF
IF(.TRUE.)THEN
  num_bytes_allocated = num_bytes_allocated + &
(((em32-sm32+1)*(spec_bdy_width))) * RWORDSIZE
  num_bytes_allocated = num_bytes_allocated + &
(((em32-sm32+1)*(spec_bdy_width))) * RWORDSIZE
  num_bytes_allocated = num_bytes_allocated + &
(((em31-sm31+1)*(spec_bdy_width))) * RWORDSIZE
  num_bytes_allocated = num_bytes_allocated + &
(((em31-sm31+1)*(spec_bdy_width))) * RWORDSIZE
ELSE
  ALLOCATE(grid%pd_btxs(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%pd_btxs(1,1,1).  ')
  endif
  ALLOCATE(grid%pd_btxe(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%pd_btxe(1,1,1).  ')
  endif
  ALLOCATE(grid%pd_btys(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%pd_btys(1,1,1).  ')
  endif
  ALLOCATE(grid%pd_btye(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%pd_btye(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'fis'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%fis(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%fis(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'res'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%res(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%res(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'t'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%t(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%t(1,1,1).  ')
  endif
ENDIF
IF(.TRUE.)THEN
  num_bytes_allocated = num_bytes_allocated + &
(((em32-sm32+1)*(em33-sm33+1)*(spec_bdy_width))) * RWORDSIZE
  num_bytes_allocated = num_bytes_allocated + &
(((em32-sm32+1)*(em33-sm33+1)*(spec_bdy_width))) * RWORDSIZE
  num_bytes_allocated = num_bytes_allocated + &
(((em31-sm31+1)*(em33-sm33+1)*(spec_bdy_width))) * RWORDSIZE
  num_bytes_allocated = num_bytes_allocated + &
(((em31-sm31+1)*(em33-sm33+1)*(spec_bdy_width))) * RWORDSIZE
ELSE
  ALLOCATE(grid%t_bxs(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%t_bxs(1,1,1).  ')
  endif
  ALLOCATE(grid%t_bxe(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%t_bxe(1,1,1).  ')
  endif
  ALLOCATE(grid%t_bys(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%t_bys(1,1,1).  ')
  endif
  ALLOCATE(grid%t_bye(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%t_bye(1,1,1).  ')
  endif
ENDIF
IF(.TRUE.)THEN
  num_bytes_allocated = num_bytes_allocated + &
(((em32-sm32+1)*(em33-sm33+1)*(spec_bdy_width))) * RWORDSIZE
  num_bytes_allocated = num_bytes_allocated + &
(((em32-sm32+1)*(em33-sm33+1)*(spec_bdy_width))) * RWORDSIZE
  num_bytes_allocated = num_bytes_allocated + &
(((em31-sm31+1)*(em33-sm33+1)*(spec_bdy_width))) * RWORDSIZE
  num_bytes_allocated = num_bytes_allocated + &
(((em31-sm31+1)*(em33-sm33+1)*(spec_bdy_width))) * RWORDSIZE
ELSE
  ALLOCATE(grid%t_btxs(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%t_btxs(1,1,1).  ')
  endif
  ALLOCATE(grid%t_btxe(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%t_btxe(1,1,1).  ')
  endif
  ALLOCATE(grid%t_btys(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%t_btys(1,1,1).  ')
  endif
  ALLOCATE(grid%t_btye(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%t_btye(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'q'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%q(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%q(1,1,1).  ')
  endif
ENDIF
IF(.TRUE.)THEN
  num_bytes_allocated = num_bytes_allocated + &
(((em32-sm32+1)*(em33-sm33+1)*(spec_bdy_width))) * RWORDSIZE
  num_bytes_allocated = num_bytes_allocated + &
(((em32-sm32+1)*(em33-sm33+1)*(spec_bdy_width))) * RWORDSIZE
  num_bytes_allocated = num_bytes_allocated + &
(((em31-sm31+1)*(em33-sm33+1)*(spec_bdy_width))) * RWORDSIZE
  num_bytes_allocated = num_bytes_allocated + &
(((em31-sm31+1)*(em33-sm33+1)*(spec_bdy_width))) * RWORDSIZE
ELSE
  ALLOCATE(grid%q_bxs(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%q_bxs(1,1,1).  ')
  endif
  ALLOCATE(grid%q_bxe(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%q_bxe(1,1,1).  ')
  endif
  ALLOCATE(grid%q_bys(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%q_bys(1,1,1).  ')
  endif
  ALLOCATE(grid%q_bye(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%q_bye(1,1,1).  ')
  endif
ENDIF
IF(.TRUE.)THEN
  num_bytes_allocated = num_bytes_allocated + &
(((em32-sm32+1)*(em33-sm33+1)*(spec_bdy_width))) * RWORDSIZE
  num_bytes_allocated = num_bytes_allocated + &
(((em32-sm32+1)*(em33-sm33+1)*(spec_bdy_width))) * RWORDSIZE
  num_bytes_allocated = num_bytes_allocated + &
(((em31-sm31+1)*(em33-sm33+1)*(spec_bdy_width))) * RWORDSIZE
  num_bytes_allocated = num_bytes_allocated + &
(((em31-sm31+1)*(em33-sm33+1)*(spec_bdy_width))) * RWORDSIZE
ELSE
  ALLOCATE(grid%q_btxs(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%q_btxs(1,1,1).  ')
  endif
  ALLOCATE(grid%q_btxe(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%q_btxe(1,1,1).  ')
  endif
  ALLOCATE(grid%q_btys(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%q_btys(1,1,1).  ')
  endif
  ALLOCATE(grid%q_btye(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%q_btye(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'u'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%u(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%u(1,1,1).  ')
  endif
ENDIF
IF(.TRUE.)THEN
  num_bytes_allocated = num_bytes_allocated + &
(((em32-sm32+1)*(em33-sm33+1)*(spec_bdy_width))) * RWORDSIZE
  num_bytes_allocated = num_bytes_allocated + &
(((em32-sm32+1)*(em33-sm33+1)*(spec_bdy_width))) * RWORDSIZE
  num_bytes_allocated = num_bytes_allocated + &
(((em31-sm31+1)*(em33-sm33+1)*(spec_bdy_width))) * RWORDSIZE
  num_bytes_allocated = num_bytes_allocated + &
(((em31-sm31+1)*(em33-sm33+1)*(spec_bdy_width))) * RWORDSIZE
ELSE
  ALLOCATE(grid%u_bxs(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%u_bxs(1,1,1).  ')
  endif
  ALLOCATE(grid%u_bxe(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%u_bxe(1,1,1).  ')
  endif
  ALLOCATE(grid%u_bys(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%u_bys(1,1,1).  ')
  endif
  ALLOCATE(grid%u_bye(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%u_bye(1,1,1).  ')
  endif
ENDIF
IF(.TRUE.)THEN
  num_bytes_allocated = num_bytes_allocated + &
(((em32-sm32+1)*(em33-sm33+1)*(spec_bdy_width))) * RWORDSIZE
  num_bytes_allocated = num_bytes_allocated + &
(((em32-sm32+1)*(em33-sm33+1)*(spec_bdy_width))) * RWORDSIZE
  num_bytes_allocated = num_bytes_allocated + &
(((em31-sm31+1)*(em33-sm33+1)*(spec_bdy_width))) * RWORDSIZE
  num_bytes_allocated = num_bytes_allocated + &
(((em31-sm31+1)*(em33-sm33+1)*(spec_bdy_width))) * RWORDSIZE
ELSE
  ALLOCATE(grid%u_btxs(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%u_btxs(1,1,1).  ')
  endif
  ALLOCATE(grid%u_btxe(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%u_btxe(1,1,1).  ')
  endif
  ALLOCATE(grid%u_btys(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%u_btys(1,1,1).  ')
  endif
  ALLOCATE(grid%u_btye(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%u_btye(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'v'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%v(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%v(1,1,1).  ')
  endif
ENDIF
IF(.TRUE.)THEN
  num_bytes_allocated = num_bytes_allocated + &
(((em32-sm32+1)*(em33-sm33+1)*(spec_bdy_width))) * RWORDSIZE
  num_bytes_allocated = num_bytes_allocated + &
(((em32-sm32+1)*(em33-sm33+1)*(spec_bdy_width))) * RWORDSIZE
  num_bytes_allocated = num_bytes_allocated + &
(((em31-sm31+1)*(em33-sm33+1)*(spec_bdy_width))) * RWORDSIZE
  num_bytes_allocated = num_bytes_allocated + &
(((em31-sm31+1)*(em33-sm33+1)*(spec_bdy_width))) * RWORDSIZE
ELSE
  ALLOCATE(grid%v_bxs(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%v_bxs(1,1,1).  ')
  endif
  ALLOCATE(grid%v_bxe(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%v_bxe(1,1,1).  ')
  endif
  ALLOCATE(grid%v_bys(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%v_bys(1,1,1).  ')
  endif
  ALLOCATE(grid%v_bye(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%v_bye(1,1,1).  ')
  endif
ENDIF
IF(.TRUE.)THEN
  num_bytes_allocated = num_bytes_allocated + &
(((em32-sm32+1)*(em33-sm33+1)*(spec_bdy_width))) * RWORDSIZE
  num_bytes_allocated = num_bytes_allocated + &
(((em32-sm32+1)*(em33-sm33+1)*(spec_bdy_width))) * RWORDSIZE
  num_bytes_allocated = num_bytes_allocated + &
(((em31-sm31+1)*(em33-sm33+1)*(spec_bdy_width))) * RWORDSIZE
  num_bytes_allocated = num_bytes_allocated + &
(((em31-sm31+1)*(em33-sm33+1)*(spec_bdy_width))) * RWORDSIZE
ELSE
  ALLOCATE(grid%v_btxs(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%v_btxs(1,1,1).  ')
  endif
  ALLOCATE(grid%v_btxe(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%v_btxe(1,1,1).  ')
  endif
  ALLOCATE(grid%v_btys(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%v_btys(1,1,1).  ')
  endif
  ALLOCATE(grid%v_btye(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%v_btye(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'told'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%told(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%told(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'uold'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%uold(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%uold(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'vold'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%vold(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%vold(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'hcoeff'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((model_config_rec%dfi_time_dim)-(1)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%hcoeff(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%hcoeff(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'dfi_pd'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%dfi_pd(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%dfi_pd(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'dfi_pint'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%dfi_pint(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%dfi_pint(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'dfi_dwdt'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%dfi_dwdt(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%dfi_dwdt(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'dfi_t'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%dfi_t(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%dfi_t(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'dfi_q'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%dfi_q(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%dfi_q(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'dfi_u'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%dfi_u(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%dfi_u(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'dfi_v'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%dfi_v(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%dfi_v(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'dfi_q2'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%dfi_q2(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%dfi_q2(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'dfi_cwm'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%dfi_cwm(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%dfi_cwm(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'dfi_rrw'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%dfi_rrw(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%dfi_rrw(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'dfi_stc'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((model_config_rec%num_soil_layers)-(1)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%dfi_stc(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%dfi_stc(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'dfi_smc'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((model_config_rec%num_soil_layers)-(1)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%dfi_smc(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%dfi_smc(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'dfi_sh2o'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((model_config_rec%num_soil_layers)-(1)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%dfi_sh2o(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%dfi_sh2o(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'dfi_snow'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%dfi_snow(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%dfi_snow(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'dfi_snowh'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%dfi_snowh(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%dfi_snowh(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'dfi_canwat'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%dfi_canwat(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%dfi_canwat(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'dfi_nmm_tsk'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%dfi_nmm_tsk(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%dfi_nmm_tsk(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'dfi_snowc'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%dfi_snowc(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%dfi_snowc(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'dx_nmm'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%dx_nmm(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%dx_nmm(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'wpdar'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%wpdar(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%wpdar(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'cpgfu'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%cpgfu(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%cpgfu(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'curv'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%curv(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%curv(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'fcp'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%fcp(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%fcp(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'fdiv'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%fdiv(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%fdiv(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'f'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%f(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%f(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'fad'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%fad(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%fad(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'ddmpu'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%ddmpu(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%ddmpu(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'ddmpv'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%ddmpv(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%ddmpv(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'deta'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em33)-(sm33)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%deta(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%deta(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'rdeta'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em33)-(sm33)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%rdeta(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%rdeta(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'aeta'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em33)-(sm33)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%aeta(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%aeta(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'f4q2'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em33)-(sm33)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%f4q2(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%f4q2(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'etax'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em33)-(sm33)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%etax(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%etax(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'dfl'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em33)-(sm33)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%dfl(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%dfl(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'deta1'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em33)-(sm33)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%deta1(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%deta1(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'aeta1'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em33)-(sm33)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%aeta1(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%aeta1(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'eta1'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em33)-(sm33)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%eta1(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%eta1(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'deta2'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em33)-(sm33)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%deta2(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%deta2(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'aeta2'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em33)-(sm33)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%aeta2(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%aeta2(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'eta2'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em33)-(sm33)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%eta2(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%eta2(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'em'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((2600)-(1)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%em(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%em(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'emt'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((2600)-(1)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%emt(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%emt(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'adt'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%adt(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%adt(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'adu'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%adu(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%adu(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'adv'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%adv(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%adv(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'em_loc'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((2600)-(1)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%em_loc(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%em_loc(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'emt_loc'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((2600)-(1)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%emt_loc(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%emt_loc(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'pdsl'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%pdsl(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%pdsl(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'pdslo'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%pdslo(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%pdslo(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'psdt'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%psdt(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%psdt(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'div'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%div(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%div(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'few'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%few(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%few(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'fne'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%fne(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%fne(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'fns'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%fns(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%fns(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'fse'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%fse(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%fse(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'omgalf'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%omgalf(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%omgalf(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'petdt'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%petdt(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%petdt(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'rtop'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%rtop(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%rtop(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'pblh'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%pblh(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%pblh(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'lpbl'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * IWORDSIZE
ELSE
  ALLOCATE(grid%lpbl(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%lpbl(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'mixht'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%mixht(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%mixht(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'ustar'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%ustar(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%ustar(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'z0'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%z0(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%z0(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'z0base'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%z0base(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%z0base(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'ths'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%ths(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%ths(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'mavail'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%mavail(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%mavail(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'qsh'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%qsh(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%qsh(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'twbs'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%twbs(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%twbs(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'qwbs'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%qwbs(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%qwbs(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'taux'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%taux(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%taux(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'tauy'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%tauy(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%tauy(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'prec'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%prec(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%prec(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'aprec'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%aprec(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%aprec(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'acprec'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%acprec(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%acprec(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'cuprec'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%cuprec(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%cuprec(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'lspa'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%lspa(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%lspa(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'ddata'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%ddata(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%ddata(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'accliq'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%accliq(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%accliq(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'sno'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%sno(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%sno(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'si'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%si(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%si(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'cldefi'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%cldefi(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%cldefi(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'deep'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%deep(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%deep(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'rf'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%rf(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%rf(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'th10'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%th10(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%th10(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'q10'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%q10(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%q10(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'pshltr'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%pshltr(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%pshltr(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'tshltr'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%tshltr(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%tshltr(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'qshltr'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%qshltr(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%qshltr(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'q2'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%q2(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%q2(1,1,1).  ')
  endif
ENDIF
IF(.TRUE.)THEN
  num_bytes_allocated = num_bytes_allocated + &
(((em32-sm32+1)*(em33-sm33+1)*(spec_bdy_width))) * RWORDSIZE
  num_bytes_allocated = num_bytes_allocated + &
(((em32-sm32+1)*(em33-sm33+1)*(spec_bdy_width))) * RWORDSIZE
  num_bytes_allocated = num_bytes_allocated + &
(((em31-sm31+1)*(em33-sm33+1)*(spec_bdy_width))) * RWORDSIZE
  num_bytes_allocated = num_bytes_allocated + &
(((em31-sm31+1)*(em33-sm33+1)*(spec_bdy_width))) * RWORDSIZE
ELSE
  ALLOCATE(grid%q2_bxs(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%q2_bxs(1,1,1).  ')
  endif
  ALLOCATE(grid%q2_bxe(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%q2_bxe(1,1,1).  ')
  endif
  ALLOCATE(grid%q2_bys(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%q2_bys(1,1,1).  ')
  endif
  ALLOCATE(grid%q2_bye(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%q2_bye(1,1,1).  ')
  endif
ENDIF
IF(.TRUE.)THEN
  num_bytes_allocated = num_bytes_allocated + &
(((em32-sm32+1)*(em33-sm33+1)*(spec_bdy_width))) * RWORDSIZE
  num_bytes_allocated = num_bytes_allocated + &
(((em32-sm32+1)*(em33-sm33+1)*(spec_bdy_width))) * RWORDSIZE
  num_bytes_allocated = num_bytes_allocated + &
(((em31-sm31+1)*(em33-sm33+1)*(spec_bdy_width))) * RWORDSIZE
  num_bytes_allocated = num_bytes_allocated + &
(((em31-sm31+1)*(em33-sm33+1)*(spec_bdy_width))) * RWORDSIZE
ELSE
  ALLOCATE(grid%q2_btxs(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%q2_btxs(1,1,1).  ')
  endif
  ALLOCATE(grid%q2_btxe(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%q2_btxe(1,1,1).  ')
  endif
  ALLOCATE(grid%q2_btys(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%q2_btys(1,1,1).  ')
  endif
  ALLOCATE(grid%q2_btye(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%q2_btye(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'t_adj'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%t_adj(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%t_adj(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'t_old'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%t_old(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%t_old(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'zero_3d'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%zero_3d(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%zero_3d(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'w0avg'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%w0avg(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%w0avg(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'akhs_out'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%akhs_out(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%akhs_out(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'akms_out'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%akms_out(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%akms_out(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'albase'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%albase(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%albase(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'albedo'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%albedo(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%albedo(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'cnvbot'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%cnvbot(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%cnvbot(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'cnvtop'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%cnvtop(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%cnvtop(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'czen'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%czen(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%czen(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'czmean'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%czmean(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%czmean(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'embck'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%embck(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%embck(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'epsr'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%epsr(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%epsr(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'gffc'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%gffc(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%gffc(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'glat'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%glat(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%glat(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'glon'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%glon(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%glon(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'nmm_tsk'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%nmm_tsk(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%nmm_tsk(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'hdac'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%hdac(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%hdac(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'hdacv'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%hdacv(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%hdacv(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'mxsnal'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%mxsnal(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%mxsnal(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'radin'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%radin(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%radin(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'radot'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%radot(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%radot(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'sigt4'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%sigt4(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%sigt4(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'tg'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%tg(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%tg(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'dfrlg'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em33)-(sm33)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%dfrlg(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%dfrlg(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'lvl'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * IWORDSIZE
ELSE
  ALLOCATE(grid%lvl(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%lvl(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'cwm'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%cwm(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%cwm(1,1,1).  ')
  endif
ENDIF
IF(.TRUE.)THEN
  num_bytes_allocated = num_bytes_allocated + &
(((em32-sm32+1)*(em33-sm33+1)*(spec_bdy_width))) * RWORDSIZE
  num_bytes_allocated = num_bytes_allocated + &
(((em32-sm32+1)*(em33-sm33+1)*(spec_bdy_width))) * RWORDSIZE
  num_bytes_allocated = num_bytes_allocated + &
(((em31-sm31+1)*(em33-sm33+1)*(spec_bdy_width))) * RWORDSIZE
  num_bytes_allocated = num_bytes_allocated + &
(((em31-sm31+1)*(em33-sm33+1)*(spec_bdy_width))) * RWORDSIZE
ELSE
  ALLOCATE(grid%cwm_bxs(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%cwm_bxs(1,1,1).  ')
  endif
  ALLOCATE(grid%cwm_bxe(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%cwm_bxe(1,1,1).  ')
  endif
  ALLOCATE(grid%cwm_bys(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%cwm_bys(1,1,1).  ')
  endif
  ALLOCATE(grid%cwm_bye(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%cwm_bye(1,1,1).  ')
  endif
ENDIF
IF(.TRUE.)THEN
  num_bytes_allocated = num_bytes_allocated + &
(((em32-sm32+1)*(em33-sm33+1)*(spec_bdy_width))) * RWORDSIZE
  num_bytes_allocated = num_bytes_allocated + &
(((em32-sm32+1)*(em33-sm33+1)*(spec_bdy_width))) * RWORDSIZE
  num_bytes_allocated = num_bytes_allocated + &
(((em31-sm31+1)*(em33-sm33+1)*(spec_bdy_width))) * RWORDSIZE
  num_bytes_allocated = num_bytes_allocated + &
(((em31-sm31+1)*(em33-sm33+1)*(spec_bdy_width))) * RWORDSIZE
ELSE
  ALLOCATE(grid%cwm_btxs(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%cwm_btxs(1,1,1).  ')
  endif
  ALLOCATE(grid%cwm_btxe(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%cwm_btxe(1,1,1).  ')
  endif
  ALLOCATE(grid%cwm_btys(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%cwm_btys(1,1,1).  ')
  endif
  ALLOCATE(grid%cwm_btye(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%cwm_btye(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'rrw'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%rrw(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%rrw(1,1,1).  ')
  endif
ENDIF
IF(.TRUE.)THEN
  num_bytes_allocated = num_bytes_allocated + &
(((em32-sm32+1)*(em33-sm33+1)*(spec_bdy_width))) * RWORDSIZE
  num_bytes_allocated = num_bytes_allocated + &
(((em32-sm32+1)*(em33-sm33+1)*(spec_bdy_width))) * RWORDSIZE
  num_bytes_allocated = num_bytes_allocated + &
(((em31-sm31+1)*(em33-sm33+1)*(spec_bdy_width))) * RWORDSIZE
  num_bytes_allocated = num_bytes_allocated + &
(((em31-sm31+1)*(em33-sm33+1)*(spec_bdy_width))) * RWORDSIZE
ELSE
  ALLOCATE(grid%rrw_bxs(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%rrw_bxs(1,1,1).  ')
  endif
  ALLOCATE(grid%rrw_bxe(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%rrw_bxe(1,1,1).  ')
  endif
  ALLOCATE(grid%rrw_bys(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%rrw_bys(1,1,1).  ')
  endif
  ALLOCATE(grid%rrw_bye(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%rrw_bye(1,1,1).  ')
  endif
ENDIF
IF(.TRUE.)THEN
  num_bytes_allocated = num_bytes_allocated + &
(((em32-sm32+1)*(em33-sm33+1)*(spec_bdy_width))) * RWORDSIZE
  num_bytes_allocated = num_bytes_allocated + &
(((em32-sm32+1)*(em33-sm33+1)*(spec_bdy_width))) * RWORDSIZE
  num_bytes_allocated = num_bytes_allocated + &
(((em31-sm31+1)*(em33-sm33+1)*(spec_bdy_width))) * RWORDSIZE
  num_bytes_allocated = num_bytes_allocated + &
(((em31-sm31+1)*(em33-sm33+1)*(spec_bdy_width))) * RWORDSIZE
ELSE
  ALLOCATE(grid%rrw_btxs(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%rrw_btxs(1,1,1).  ')
  endif
  ALLOCATE(grid%rrw_btxe(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%rrw_btxe(1,1,1).  ')
  endif
  ALLOCATE(grid%rrw_btys(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%rrw_btys(1,1,1).  ')
  endif
  ALLOCATE(grid%rrw_btye(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%rrw_btye(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'f_ice'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%f_ice(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%f_ice(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'f_rain'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%f_rain(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%f_rain(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'f_rimef'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%f_rimef(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%f_rimef(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'cldfra'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%cldfra(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%cldfra(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'sr'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%sr(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%sr(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'cfrach'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%cfrach(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%cfrach(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'cfracl'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%cfracl(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%cfracl(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'cfracm'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%cfracm(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%cfracm(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'islope'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * IWORDSIZE
ELSE
  ALLOCATE(grid%islope(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%islope(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'dzsoil'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em33)-(sm33)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%dzsoil(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%dzsoil(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'rtdpth'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em33)-(sm33)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%rtdpth(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%rtdpth(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'sldpth'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em33)-(sm33)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%sldpth(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%sldpth(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'cmc'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%cmc(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%cmc(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'grnflx'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%grnflx(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%grnflx(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'pctsno'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%pctsno(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%pctsno(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'soiltb'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%soiltb(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%soiltb(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'vegfrc'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%vegfrc(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%vegfrc(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'shdmin'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%shdmin(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%shdmin(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'shdmax'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%shdmax(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%shdmax(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'sh2o'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((model_config_rec%num_soil_layers)-(1)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%sh2o(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%sh2o(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'smc'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((model_config_rec%num_soil_layers)-(1)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%smc(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%smc(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'stc'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((model_config_rec%num_soil_layers)-(1)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%stc(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%stc(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'hstdv'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%hstdv(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%hstdv(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'hcnvx'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%hcnvx(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%hcnvx(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'hasyw'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%hasyw(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%hasyw(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'hasys'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%hasys(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%hasys(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'hasysw'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%hasysw(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%hasysw(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'hasynw'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%hasynw(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%hasynw(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'hlenw'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%hlenw(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%hlenw(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'hlens'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%hlens(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%hlens(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'hlensw'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%hlensw(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%hlensw(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'hlennw'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%hlennw(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%hlennw(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'hangl'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%hangl(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%hangl(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'hanis'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%hanis(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%hanis(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'hslop'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%hslop(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%hslop(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'hzmax'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%hzmax(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%hzmax(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'crot'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%crot(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%crot(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'srot'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%srot(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%srot(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'ugwdsfc'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%ugwdsfc(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%ugwdsfc(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'vgwdsfc'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%vgwdsfc(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%vgwdsfc(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'dwdtmn'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%dwdtmn(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%dwdtmn(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'dwdtmx'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%dwdtmx(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%dwdtmx(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'dwdt'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%dwdt(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%dwdt(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'pdwdt'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%pdwdt(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%pdwdt(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'pint'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%pint(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%pint(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'w'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%w(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%w(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'z'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%z(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%z(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'acfrcv'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%acfrcv(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%acfrcv(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'acfrst'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%acfrst(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%acfrst(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'ssroff'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%ssroff(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%ssroff(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'bgroff'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%bgroff(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%bgroff(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'rlwin'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%rlwin(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%rlwin(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'rlwout'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%rlwout(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%rlwout(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'rlwtoa'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%rlwtoa(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%rlwtoa(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'alwin'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%alwin(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%alwin(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'alwout'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%alwout(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%alwout(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'alwtoa'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%alwtoa(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%alwtoa(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'rswin'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%rswin(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%rswin(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'rswinc'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%rswinc(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%rswinc(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'rswout'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%rswout(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%rswout(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'rswtoa'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%rswtoa(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%rswtoa(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'aswin'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%aswin(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%aswin(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'aswout'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%aswout(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%aswout(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'aswtoa'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%aswtoa(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%aswtoa(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'sfcshx'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%sfcshx(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%sfcshx(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'sfclhx'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%sfclhx(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%sfclhx(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'subshx'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%subshx(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%subshx(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'snopcx'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%snopcx(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%snopcx(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'sfcuvx'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%sfcuvx(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%sfcuvx(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'potevp'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%potevp(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%potevp(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'potflx'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%potflx(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%potflx(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'tlmin'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%tlmin(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%tlmin(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'tlmax'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%tlmax(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%tlmax(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'t02_min'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%t02_min(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%t02_min(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'t02_max'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%t02_max(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%t02_max(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'rh02_min'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%rh02_min(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%rh02_min(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'rh02_max'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%rh02_max(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%rh02_max(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'rlwtt'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%rlwtt(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%rlwtt(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'rswtt'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%rswtt(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%rswtt(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'tcucn'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%tcucn(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%tcucn(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'train'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%train(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%train(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'ncfrcv'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * IWORDSIZE
ELSE
  ALLOCATE(grid%ncfrcv(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%ncfrcv(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'ncfrst'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * IWORDSIZE
ELSE
  ALLOCATE(grid%ncfrst(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%ncfrst(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'max10mw'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%max10mw(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%max10mw(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'max10u'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%max10u(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%max10u(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'max10v'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%max10v(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%max10v(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'maxupdr'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%maxupdr(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%maxupdr(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'maxdndr'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%maxdndr(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%maxdndr(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'maxhlcy'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%maxhlcy(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%maxhlcy(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'maxdbz'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%maxdbz(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%maxdbz(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'ihe'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em32)-(sm32)+1))) * IWORDSIZE
ELSE
  ALLOCATE(grid%ihe(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%ihe(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'ihw'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em32)-(sm32)+1))) * IWORDSIZE
ELSE
  ALLOCATE(grid%ihw(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%ihw(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'ive'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em32)-(sm32)+1))) * IWORDSIZE
ELSE
  ALLOCATE(grid%ive(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%ive(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'ivw'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em32)-(sm32)+1))) * IWORDSIZE
ELSE
  ALLOCATE(grid%ivw(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%ivw(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'irad'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))) * IWORDSIZE
ELSE
  ALLOCATE(grid%irad(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%irad(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'iheg'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((2600)-(1)+1))) * IWORDSIZE
ELSE
  ALLOCATE(grid%iheg(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%iheg(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'ihwg'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((2600)-(1)+1))) * IWORDSIZE
ELSE
  ALLOCATE(grid%ihwg(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%ihwg(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'iveg'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((2600)-(1)+1))) * IWORDSIZE
ELSE
  ALLOCATE(grid%iveg(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%iveg(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'ivwg'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((2600)-(1)+1))) * IWORDSIZE
ELSE
  ALLOCATE(grid%ivwg(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%ivwg(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'iradg'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((2000)-(1)+1))) * IWORDSIZE
ELSE
  ALLOCATE(grid%iradg(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%iradg(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'n_iup_h'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em32)-(sm32)+1))) * IWORDSIZE
ELSE
  ALLOCATE(grid%n_iup_h(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%n_iup_h(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'n_iup_v'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em32)-(sm32)+1))) * IWORDSIZE
ELSE
  ALLOCATE(grid%n_iup_v(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%n_iup_v(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'n_iup_adh'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em32)-(sm32)+1))) * IWORDSIZE
ELSE
  ALLOCATE(grid%n_iup_adh(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%n_iup_adh(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'n_iup_adv'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em32)-(sm32)+1))) * IWORDSIZE
ELSE
  ALLOCATE(grid%n_iup_adv(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%n_iup_adv(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'iup_h'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * IWORDSIZE
ELSE
  ALLOCATE(grid%iup_h(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%iup_h(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'iup_v'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * IWORDSIZE
ELSE
  ALLOCATE(grid%iup_v(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%iup_v(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'iup_adh'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * IWORDSIZE
ELSE
  ALLOCATE(grid%iup_adh(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%iup_adh(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'iup_adv'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * IWORDSIZE
ELSE
  ALLOCATE(grid%iup_adv(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%iup_adv(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'imask_nostag'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * IWORDSIZE
ELSE
  ALLOCATE(grid%imask_nostag(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%imask_nostag(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'imask_xstag'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * IWORDSIZE
ELSE
  ALLOCATE(grid%imask_xstag(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%imask_xstag(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'imask_ystag'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * IWORDSIZE
ELSE
  ALLOCATE(grid%imask_ystag(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%imask_ystag(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'imask_xystag'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * IWORDSIZE
ELSE
  ALLOCATE(grid%imask_xystag(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%imask_xystag(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'sm000007'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%sm000007(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%sm000007(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'sm007028'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%sm007028(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%sm007028(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'sm028100'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%sm028100(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%sm028100(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'sm100255'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%sm100255(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%sm100255(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'st000007'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%st000007(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%st000007(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'st007028'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%st007028(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%st007028(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'st028100'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%st028100(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%st028100(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'st100255'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%st100255(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%st100255(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'sm000010'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%sm000010(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%sm000010(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'sm010040'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%sm010040(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%sm010040(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'sm040100'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%sm040100(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%sm040100(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'sm100200'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%sm100200(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%sm100200(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'sm010200'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%sm010200(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%sm010200(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'soilm000'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%soilm000(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%soilm000(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'soilm005'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%soilm005(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%soilm005(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'soilm020'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%soilm020(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%soilm020(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'soilm040'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%soilm040(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%soilm040(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'soilm160'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%soilm160(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%soilm160(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'soilm300'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%soilm300(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%soilm300(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'sw000010'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%sw000010(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%sw000010(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'sw010040'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%sw010040(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%sw010040(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'sw040100'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%sw040100(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%sw040100(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'sw100200'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%sw100200(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%sw100200(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'sw010200'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%sw010200(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%sw010200(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'soilw000'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%soilw000(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%soilw000(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'soilw005'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%soilw005(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%soilw005(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'soilw020'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%soilw020(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%soilw020(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'soilw040'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%soilw040(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%soilw040(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'soilw160'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%soilw160(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%soilw160(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'soilw300'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%soilw300(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%soilw300(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'st000010'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%st000010(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%st000010(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'st010040'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%st010040(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%st010040(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'st040100'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%st040100(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%st040100(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'st100200'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%st100200(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%st100200(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'st010200'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%st010200(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%st010200(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'soilt000'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%soilt000(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%soilt000(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'soilt005'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%soilt005(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%soilt005(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'soilt020'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%soilt020(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%soilt020(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'soilt040'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%soilt040(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%soilt040(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'soilt160'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%soilt160(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%soilt160(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'soilt300'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%soilt300(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%soilt300(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'landmask'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%landmask(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%landmask(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'topostdv'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%topostdv(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%topostdv(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'toposlpx'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%toposlpx(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%toposlpx(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'toposlpy'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%toposlpy(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%toposlpy(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'greenmax'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%greenmax(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%greenmax(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'greenmin'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%greenmin(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%greenmin(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'albedomx'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%albedomx(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%albedomx(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'slopecat'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%slopecat(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%slopecat(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'toposoil'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%toposoil(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%toposoil(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'landusef'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((model_config_rec%num_land_cat)-(1)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%landusef(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%landusef(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'soilctop'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((model_config_rec%num_soil_cat)-(1)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%soilctop(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%soilctop(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'soilcbot'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((model_config_rec%num_soil_cat)-(1)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%soilcbot(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%soilcbot(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'ts_hour'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((model_config_rec%ts_buf_size)-(1)+1))*(((model_config_rec%max_ts_locs)-(1)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%ts_hour(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%ts_hour(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'ts_u'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((model_config_rec%ts_buf_size)-(1)+1))*(((model_config_rec%max_ts_locs)-(1)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%ts_u(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%ts_u(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'ts_v'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((model_config_rec%ts_buf_size)-(1)+1))*(((model_config_rec%max_ts_locs)-(1)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%ts_v(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%ts_v(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'ts_q'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((model_config_rec%ts_buf_size)-(1)+1))*(((model_config_rec%max_ts_locs)-(1)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%ts_q(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%ts_q(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'ts_t'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((model_config_rec%ts_buf_size)-(1)+1))*(((model_config_rec%max_ts_locs)-(1)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%ts_t(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%ts_t(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'ts_psfc'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((model_config_rec%ts_buf_size)-(1)+1))*(((model_config_rec%max_ts_locs)-(1)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%ts_psfc(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%ts_psfc(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'ts_tsk'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((model_config_rec%ts_buf_size)-(1)+1))*(((model_config_rec%max_ts_locs)-(1)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%ts_tsk(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%ts_tsk(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'ts_tslb'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((model_config_rec%ts_buf_size)-(1)+1))*(((model_config_rec%max_ts_locs)-(1)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%ts_tslb(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%ts_tslb(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'ts_clw'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((model_config_rec%ts_buf_size)-(1)+1))*(((model_config_rec%max_ts_locs)-(1)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%ts_clw(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%ts_clw(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'moist'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1)*num_moist)) * RWORDSIZE
ELSE
  ALLOCATE(grid%moist(1,1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%moist(1,1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'dfi_moist'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1)*num_dfi_moist)) * RWORDSIZE
ELSE
  ALLOCATE(grid%dfi_moist(1,1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%dfi_moist(1,1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'scalar'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1)*num_scalar)) * RWORDSIZE
ELSE
  ALLOCATE(grid%scalar(1,1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%scalar(1,1,1,1).  ')
  endif
ENDIF
IF(.TRUE.)THEN
  num_bytes_allocated = num_bytes_allocated + &
(((em32-sm32+1)*(em33-sm33+1)*(spec_bdy_width)*num_scalar)) * RWORDSIZE
  num_bytes_allocated = num_bytes_allocated + &
(((em32-sm32+1)*(em33-sm33+1)*(spec_bdy_width)*num_scalar)) * RWORDSIZE
  num_bytes_allocated = num_bytes_allocated + &
(((em31-sm31+1)*(em33-sm33+1)*(spec_bdy_width)*num_scalar)) * RWORDSIZE
  num_bytes_allocated = num_bytes_allocated + &
(((em31-sm31+1)*(em33-sm33+1)*(spec_bdy_width)*num_scalar)) * RWORDSIZE
ELSE
  ALLOCATE(grid%scalar_bxs(1,1,1,num_scalar),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%scalar_bxs(1,1,1,num_scalar).  ')
  endif
  ALLOCATE(grid%scalar_bxe(1,1,1,num_scalar),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%scalar_bxe(1,1,1,num_scalar).  ')
  endif
  ALLOCATE(grid%scalar_bys(1,1,1,num_scalar),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%scalar_bys(1,1,1,num_scalar).  ')
  endif
  ALLOCATE(grid%scalar_bye(1,1,1,num_scalar),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%scalar_bye(1,1,1,num_scalar).  ')
  endif
ENDIF
IF(.TRUE.)THEN
  num_bytes_allocated = num_bytes_allocated + &
(((em32-sm32+1)*(em33-sm33+1)*(spec_bdy_width)*num_scalar)) * RWORDSIZE
  num_bytes_allocated = num_bytes_allocated + &
(((em32-sm32+1)*(em33-sm33+1)*(spec_bdy_width)*num_scalar)) * RWORDSIZE
  num_bytes_allocated = num_bytes_allocated + &
(((em31-sm31+1)*(em33-sm33+1)*(spec_bdy_width)*num_scalar)) * RWORDSIZE
  num_bytes_allocated = num_bytes_allocated + &
(((em31-sm31+1)*(em33-sm33+1)*(spec_bdy_width)*num_scalar)) * RWORDSIZE
ELSE
  ALLOCATE(grid%scalar_btxs(1,1,1,num_scalar),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%scalar_btxs(1,1,1,num_scalar).  ')
  endif
  ALLOCATE(grid%scalar_btxe(1,1,1,num_scalar),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%scalar_btxe(1,1,1,num_scalar).  ')
  endif
  ALLOCATE(grid%scalar_btys(1,1,1,num_scalar),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%scalar_btys(1,1,1,num_scalar).  ')
  endif
  ALLOCATE(grid%scalar_btye(1,1,1,num_scalar),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%scalar_btye(1,1,1,num_scalar).  ')
  endif
ENDIF
IF(in_use_for_config(id,'dfi_scalar'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))*(((em32)-(sm32)+1)*num_dfi_scalar)) * RWORDSIZE
ELSE
  ALLOCATE(grid%dfi_scalar(1,1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%dfi_scalar(1,1,1,1).  ')
  endif
ENDIF
IF(.TRUE.)THEN
  num_bytes_allocated = num_bytes_allocated + &
(((em32-sm32+1)*(em33-sm33+1)*(spec_bdy_width)*num_dfi_scalar)) * RWORDSIZE
  num_bytes_allocated = num_bytes_allocated + &
(((em32-sm32+1)*(em33-sm33+1)*(spec_bdy_width)*num_dfi_scalar)) * RWORDSIZE
  num_bytes_allocated = num_bytes_allocated + &
(((em31-sm31+1)*(em33-sm33+1)*(spec_bdy_width)*num_dfi_scalar)) * RWORDSIZE
  num_bytes_allocated = num_bytes_allocated + &
(((em31-sm31+1)*(em33-sm33+1)*(spec_bdy_width)*num_dfi_scalar)) * RWORDSIZE
ELSE
  ALLOCATE(grid%dfi_scalar_bxs(1,1,1,num_dfi_scalar),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%dfi_scalar_bxs(1,1,1,num_dfi_scalar).  ')
  endif
  ALLOCATE(grid%dfi_scalar_bxe(1,1,1,num_dfi_scalar),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%dfi_scalar_bxe(1,1,1,num_dfi_scalar).  ')
  endif
  ALLOCATE(grid%dfi_scalar_bys(1,1,1,num_dfi_scalar),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%dfi_scalar_bys(1,1,1,num_dfi_scalar).  ')
  endif
  ALLOCATE(grid%dfi_scalar_bye(1,1,1,num_dfi_scalar),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%dfi_scalar_bye(1,1,1,num_dfi_scalar).  ')
  endif
ENDIF
IF(.TRUE.)THEN
  num_bytes_allocated = num_bytes_allocated + &
(((em32-sm32+1)*(em33-sm33+1)*(spec_bdy_width)*num_dfi_scalar)) * RWORDSIZE
  num_bytes_allocated = num_bytes_allocated + &
(((em32-sm32+1)*(em33-sm33+1)*(spec_bdy_width)*num_dfi_scalar)) * RWORDSIZE
  num_bytes_allocated = num_bytes_allocated + &
(((em31-sm31+1)*(em33-sm33+1)*(spec_bdy_width)*num_dfi_scalar)) * RWORDSIZE
  num_bytes_allocated = num_bytes_allocated + &
(((em31-sm31+1)*(em33-sm33+1)*(spec_bdy_width)*num_dfi_scalar)) * RWORDSIZE
ELSE
  ALLOCATE(grid%dfi_scalar_btxs(1,1,1,num_dfi_scalar),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%dfi_scalar_btxs(1,1,1,num_dfi_scalar).  ')
  endif
  ALLOCATE(grid%dfi_scalar_btxe(1,1,1,num_dfi_scalar),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%dfi_scalar_btxe(1,1,1,num_dfi_scalar).  ')
  endif
  ALLOCATE(grid%dfi_scalar_btys(1,1,1,num_dfi_scalar),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%dfi_scalar_btys(1,1,1,num_dfi_scalar).  ')
  endif
  ALLOCATE(grid%dfi_scalar_btye(1,1,1,num_dfi_scalar),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%dfi_scalar_btye(1,1,1,num_dfi_scalar).  ')
  endif
ENDIF
IF(in_use_for_config(id,'chem'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))*(((em32)-(sm32)+1)*num_chem)) * RWORDSIZE
ELSE
  ALLOCATE(grid%chem(1,1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%chem(1,1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'smois'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((model_config_rec%num_soil_layers)-(1)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%smois(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%smois(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'tslb'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((model_config_rec%num_soil_layers)-(1)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%tslb(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%tslb(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'gsw'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%gsw(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%gsw(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'xlat'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%xlat(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%xlat(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'xlong'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%xlong(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%xlong(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'xland'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%xland(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%xland(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'raincv'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%raincv(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%raincv(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'psfc'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%psfc(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%psfc(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'th2'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%th2(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%th2(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'t2'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%t2(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%t2(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'u10'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%u10(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%u10(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'v10'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%v10(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%v10(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'xice'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%xice(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%xice(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'lai'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%lai(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%lai(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'smstav'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%smstav(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%smstav(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'smstot'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%smstot(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%smstot(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'sfcrunoff'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%sfcrunoff(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%sfcrunoff(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'udrunoff'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%udrunoff(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%udrunoff(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'ivgtyp'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * IWORDSIZE
ELSE
  ALLOCATE(grid%ivgtyp(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%ivgtyp(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'isltyp'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * IWORDSIZE
ELSE
  ALLOCATE(grid%isltyp(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%isltyp(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'vegfra'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%vegfra(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%vegfra(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'sfcevp'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%sfcevp(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%sfcevp(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'grdflx'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%grdflx(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%grdflx(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'albbck'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%albbck(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%albbck(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'sfcexc'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%sfcexc(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%sfcexc(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'snotime'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%snotime(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%snotime(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'acsnow'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%acsnow(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%acsnow(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'acsnom'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%acsnom(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%acsnom(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'rmol'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%rmol(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%rmol(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'snow'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%snow(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%snow(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'canwat'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%canwat(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%canwat(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'sst'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%sst(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%sst(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'weasd'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%weasd(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%weasd(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'znt'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%znt(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%znt(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'mol'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%mol(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%mol(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'noahres'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%noahres(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%noahres(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'tke_myj'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%tke_myj(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%tke_myj(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'el_myj'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%el_myj(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%el_myj(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'exch_h'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%exch_h(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%exch_h(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'exch_m'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%exch_m(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%exch_m(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'thz0'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%thz0(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%thz0(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'qz0'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%qz0(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%qz0(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'uz0'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%uz0(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%uz0(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'vz0'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%vz0(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%vz0(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'flhc'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%flhc(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%flhc(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'flqc'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%flqc(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%flqc(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'qsg'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%qsg(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%qsg(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'qvg'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%qvg(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%qvg(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'qcg'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%qcg(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%qcg(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'soilt1'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%soilt1(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%soilt1(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'tsnav'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%tsnav(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%tsnav(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'psfc_out'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%psfc_out(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%psfc_out(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'uz0h'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%uz0h(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%uz0h(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'vz0h'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%vz0h(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%vz0h(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'dudt'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%dudt(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%dudt(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'dvdt'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%dvdt(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%dvdt(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'qsfc'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%qsfc(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%qsfc(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'akhs'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%akhs(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%akhs(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'akms'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%akms(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%akms(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'htop'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%htop(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%htop(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'hbot'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%hbot(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%hbot(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'htopr'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%htopr(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%htopr(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'hbotr'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%hbotr(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%hbotr(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'htopd'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%htopd(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%htopd(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'hbotd'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%hbotd(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%hbotd(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'htops'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%htops(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%htops(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'hbots'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%hbots(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%hbots(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'cuppt'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%cuppt(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%cuppt(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'cprate'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%cprate(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%cprate(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'f_ice_phy'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%f_ice_phy(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%f_ice_phy(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'f_rain_phy'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%f_rain_phy(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%f_rain_phy(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'f_rimef_phy'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%f_rimef_phy(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%f_rimef_phy(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'mass_flux'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%mass_flux(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%mass_flux(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'apr_gr'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%apr_gr(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%apr_gr(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'apr_w'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%apr_w(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%apr_w(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'apr_mc'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%apr_mc(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%apr_mc(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'apr_st'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%apr_st(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%apr_st(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'apr_as'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%apr_as(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%apr_as(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'apr_capma'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%apr_capma(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%apr_capma(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'apr_capme'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%apr_capme(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%apr_capme(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'apr_capmi'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%apr_capmi(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%apr_capmi(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'xf_ens'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((model_config_rec%ensdim)-(1)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%xf_ens(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%xf_ens(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'pr_ens'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((model_config_rec%ensdim)-(1)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%pr_ens(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%pr_ens(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'rthften'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%rthften(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%rthften(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'rqvften'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%rqvften(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%rqvften(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'snowh'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%snowh(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%snowh(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'rhosn'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%rhosn(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%rhosn(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'smfr3d'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((model_config_rec%num_soil_layers)-(1)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%smfr3d(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%smfr3d(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'keepfr3dflag'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((model_config_rec%num_soil_layers)-(1)+1))*(((em32)-(sm32)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%keepfr3dflag(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%keepfr3dflag(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'mp_restart_state'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((7501)-(1)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%mp_restart_state(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%mp_restart_state(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'tbpvs_state'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((7501)-(1)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%tbpvs_state(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%tbpvs_state(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'tbpvs0_state'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((7501)-(1)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%tbpvs0_state(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%tbpvs0_state(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'lu_state'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((7501)-(1)+1))) * RWORDSIZE
ELSE
  ALLOCATE(grid%lu_state(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal ( &
    'frame/module_domain.f: Failed to allocate grid%lu_state(1).  ')
  endif
ENDIF
!ENDOFREGISTRYGENERATEDINCLUDE
