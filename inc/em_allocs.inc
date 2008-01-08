!STARTOFREGISTRYGENERATEDINCLUDE 'inc/em_allocs.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
ALLOCATE(grid%em_u_1(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_u_1(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%em_u_1=initial_data_value
ALLOCATE(grid%em_u_2(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_u_2(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%em_u_2=initial_data_value
ALLOCATE(grid%em_ru(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_ru(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%em_ru=initial_data_value
ALLOCATE(grid%em_v_1(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_v_1(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%em_v_1=initial_data_value
ALLOCATE(grid%em_v_2(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_v_2(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%em_v_2=initial_data_value
ALLOCATE(grid%em_rv(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_rv(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%em_rv=initial_data_value
ALLOCATE(grid%em_w_1(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_w_1(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%em_w_1=initial_data_value
ALLOCATE(grid%em_w_2(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_w_2(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%em_w_2=initial_data_value
ALLOCATE(grid%em_ww(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_ww(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%em_ww=initial_data_value
ALLOCATE(grid%em_ph_1(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_ph_1(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%em_ph_1=initial_data_value
ALLOCATE(grid%em_ph_2(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_ph_2(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%em_ph_2=initial_data_value
ALLOCATE(grid%em_phb(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_phb(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%em_phb=initial_data_value
ALLOCATE(grid%em_ph0(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_ph0(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%em_ph0=initial_data_value
ALLOCATE(grid%em_php(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_php(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%em_php=initial_data_value
ALLOCATE(grid%em_t_1(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_t_1(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%em_t_1=initial_data_value
ALLOCATE(grid%em_t_2(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_t_2(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%em_t_2=initial_data_value
ALLOCATE(grid%em_t_init(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_t_init(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%em_t_init=initial_data_value
ALLOCATE(grid%em_tp_1(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_tp_1(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%em_tp_1=initial_data_value
ALLOCATE(grid%em_tp_2(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_tp_2(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%em_tp_2=initial_data_value
ALLOCATE(grid%em_mu_1(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_mu_1(sm31:em31,sm33:em33). ')
 endif
  grid%em_mu_1=initial_data_value
ALLOCATE(grid%em_mu_2(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_mu_2(sm31:em31,sm33:em33). ')
 endif
  grid%em_mu_2=initial_data_value
ALLOCATE(grid%em_mub(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_mub(sm31:em31,sm33:em33). ')
 endif
  grid%em_mub=initial_data_value
ALLOCATE(grid%em_mu0(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_mu0(sm31:em31,sm33:em33). ')
 endif
  grid%em_mu0=initial_data_value
ALLOCATE(grid%em_mudf(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_mudf(sm31:em31,sm33:em33). ')
 endif
  grid%em_mudf=initial_data_value
ALLOCATE(grid%em_tke_1(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_tke_1(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%em_tke_1=initial_data_value
ALLOCATE(grid%em_tke_2(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_tke_2(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%em_tke_2=initial_data_value
ALLOCATE(grid%em_p(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_p(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%em_p=initial_data_value
ALLOCATE(grid%em_al(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_al(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%em_al=initial_data_value
ALLOCATE(grid%em_alt(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_alt(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%em_alt=initial_data_value
ALLOCATE(grid%em_alb(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_alb(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%em_alb=initial_data_value
ALLOCATE(grid%em_zx(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_zx(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%em_zx=initial_data_value
ALLOCATE(grid%em_zy(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_zy(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%em_zy=initial_data_value
ALLOCATE(grid%em_rdz(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_rdz(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%em_rdz=initial_data_value
ALLOCATE(grid%em_rdzw(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_rdzw(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%em_rdzw=initial_data_value
ALLOCATE(grid%em_q2(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_q2(sm31:em31,sm33:em33). ')
 endif
  grid%em_q2=initial_data_value
ALLOCATE(grid%sm000010(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%sm000010(sm31:em31,sm33:em33). ')
 endif
  grid%sm000010=initial_data_value
ALLOCATE(grid%sm010040(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%sm010040(sm31:em31,sm33:em33). ')
 endif
  grid%sm010040=initial_data_value
ALLOCATE(grid%sm040100(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%sm040100(sm31:em31,sm33:em33). ')
 endif
  grid%sm040100=initial_data_value
ALLOCATE(grid%sm100200(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%sm100200(sm31:em31,sm33:em33). ')
 endif
  grid%sm100200=initial_data_value
ALLOCATE(grid%sm010200(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%sm010200(sm31:em31,sm33:em33). ')
 endif
  grid%sm010200=initial_data_value
ALLOCATE(grid%soilm000(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%soilm000(sm31:em31,sm33:em33). ')
 endif
  grid%soilm000=initial_data_value
ALLOCATE(grid%soilm005(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%soilm005(sm31:em31,sm33:em33). ')
 endif
  grid%soilm005=initial_data_value
ALLOCATE(grid%soilm020(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%soilm020(sm31:em31,sm33:em33). ')
 endif
  grid%soilm020=initial_data_value
ALLOCATE(grid%soilm040(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%soilm040(sm31:em31,sm33:em33). ')
 endif
  grid%soilm040=initial_data_value
ALLOCATE(grid%soilm160(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%soilm160(sm31:em31,sm33:em33). ')
 endif
  grid%soilm160=initial_data_value
ALLOCATE(grid%soilm300(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%soilm300(sm31:em31,sm33:em33). ')
 endif
  grid%soilm300=initial_data_value
ALLOCATE(grid%sw000010(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%sw000010(sm31:em31,sm33:em33). ')
 endif
  grid%sw000010=initial_data_value
ALLOCATE(grid%sw010040(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%sw010040(sm31:em31,sm33:em33). ')
 endif
  grid%sw010040=initial_data_value
ALLOCATE(grid%sw040100(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%sw040100(sm31:em31,sm33:em33). ')
 endif
  grid%sw040100=initial_data_value
ALLOCATE(grid%sw100200(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%sw100200(sm31:em31,sm33:em33). ')
 endif
  grid%sw100200=initial_data_value
ALLOCATE(grid%sw010200(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%sw010200(sm31:em31,sm33:em33). ')
 endif
  grid%sw010200=initial_data_value
ALLOCATE(grid%soilw000(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%soilw000(sm31:em31,sm33:em33). ')
 endif
  grid%soilw000=initial_data_value
ALLOCATE(grid%soilw005(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%soilw005(sm31:em31,sm33:em33). ')
 endif
  grid%soilw005=initial_data_value
ALLOCATE(grid%soilw020(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%soilw020(sm31:em31,sm33:em33). ')
 endif
  grid%soilw020=initial_data_value
ALLOCATE(grid%soilw040(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%soilw040(sm31:em31,sm33:em33). ')
 endif
  grid%soilw040=initial_data_value
ALLOCATE(grid%soilw160(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%soilw160(sm31:em31,sm33:em33). ')
 endif
  grid%soilw160=initial_data_value
ALLOCATE(grid%soilw300(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%soilw300(sm31:em31,sm33:em33). ')
 endif
  grid%soilw300=initial_data_value
ALLOCATE(grid%st000010(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%st000010(sm31:em31,sm33:em33). ')
 endif
  grid%st000010=initial_data_value
ALLOCATE(grid%st010040(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%st010040(sm31:em31,sm33:em33). ')
 endif
  grid%st010040=initial_data_value
ALLOCATE(grid%st040100(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%st040100(sm31:em31,sm33:em33). ')
 endif
  grid%st040100=initial_data_value
ALLOCATE(grid%st100200(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%st100200(sm31:em31,sm33:em33). ')
 endif
  grid%st100200=initial_data_value
ALLOCATE(grid%st010200(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%st010200(sm31:em31,sm33:em33). ')
 endif
  grid%st010200=initial_data_value
ALLOCATE(grid%soilt000(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%soilt000(sm31:em31,sm33:em33). ')
 endif
  grid%soilt000=initial_data_value
ALLOCATE(grid%soilt005(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%soilt005(sm31:em31,sm33:em33). ')
 endif
  grid%soilt005=initial_data_value
ALLOCATE(grid%soilt020(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%soilt020(sm31:em31,sm33:em33). ')
 endif
  grid%soilt020=initial_data_value
ALLOCATE(grid%soilt040(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%soilt040(sm31:em31,sm33:em33). ')
 endif
  grid%soilt040=initial_data_value
ALLOCATE(grid%soilt160(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%soilt160(sm31:em31,sm33:em33). ')
 endif
  grid%soilt160=initial_data_value
ALLOCATE(grid%soilt300(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%soilt300(sm31:em31,sm33:em33). ')
 endif
  grid%soilt300=initial_data_value
ALLOCATE(grid%landmask(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%landmask(sm31:em31,sm33:em33). ')
 endif
  grid%landmask=initial_data_value
ALLOCATE(grid%topostdv(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%topostdv(sm31:em31,sm33:em33). ')
 endif
  grid%topostdv=initial_data_value
ALLOCATE(grid%toposlpx(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%toposlpx(sm31:em31,sm33:em33). ')
 endif
  grid%toposlpx=initial_data_value
ALLOCATE(grid%toposlpy(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%toposlpy(sm31:em31,sm33:em33). ')
 endif
  grid%toposlpy=initial_data_value
ALLOCATE(grid%greenmax(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%greenmax(sm31:em31,sm33:em33). ')
 endif
  grid%greenmax=initial_data_value
ALLOCATE(grid%greenmin(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%greenmin(sm31:em31,sm33:em33). ')
 endif
  grid%greenmin=initial_data_value
ALLOCATE(grid%albedomx(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%albedomx(sm31:em31,sm33:em33). ')
 endif
  grid%albedomx=initial_data_value
ALLOCATE(grid%slopecat(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%slopecat(sm31:em31,sm33:em33). ')
 endif
  grid%slopecat=initial_data_value
ALLOCATE(grid%toposoil(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%toposoil(sm31:em31,sm33:em33). ')
 endif
  grid%toposoil=initial_data_value
ALLOCATE(grid%landusef(sm31:em31,model_config_rec%num_land_cat,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%landusef(sm31:em31,model_config_rec%num_land_cat,sm33:em33). ')
 endif
  grid%landusef=initial_data_value
ALLOCATE(grid%soilctop(sm31:em31,model_config_rec%num_soil_cat,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%soilctop(sm31:em31,model_config_rec%num_soil_cat,sm33:em33). ')
 endif
  grid%soilctop=initial_data_value
ALLOCATE(grid%soilcbot(sm31:em31,model_config_rec%num_soil_cat,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%soilcbot(sm31:em31,model_config_rec%num_soil_cat,sm33:em33). ')
 endif
  grid%soilcbot=initial_data_value
ALLOCATE(grid%moist_1(sm31:em31,sm32:em32,sm33:em33,num_moist),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%moist_1(sm31:em31,sm32:em32,sm33:em33,num_moist). ')
 endif
  grid%moist_1=initial_data_value
ALLOCATE(grid%moist_2(sm31:em31,sm32:em32,sm33:em33,num_moist),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%moist_2(sm31:em31,sm32:em32,sm33:em33,num_moist). ')
 endif
  grid%moist_2=initial_data_value
ALLOCATE(grid%chem_1(sm31:em31,sm32:em32,sm33:em33,num_chem),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%chem_1(sm31:em31,sm32:em32,sm33:em33,num_chem). ')
 endif
  grid%chem_1=initial_data_value
ALLOCATE(grid%chem_2(sm31:em31,sm32:em32,sm33:em33,num_chem),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%chem_2(sm31:em31,sm32:em32,sm33:em33,num_chem). ')
 endif
  grid%chem_2=initial_data_value
ALLOCATE(grid%em_u_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_u_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  grid%em_u_b=initial_data_value
ALLOCATE(grid%em_u_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_u_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  grid%em_u_bt=initial_data_value
ALLOCATE(grid%em_v_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_v_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  grid%em_v_b=initial_data_value
ALLOCATE(grid%em_v_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_v_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  grid%em_v_bt=initial_data_value
ALLOCATE(grid%em_ph_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_ph_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  grid%em_ph_b=initial_data_value
ALLOCATE(grid%em_ph_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_ph_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  grid%em_ph_bt=initial_data_value
ALLOCATE(grid%em_t_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_t_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  grid%em_t_b=initial_data_value
ALLOCATE(grid%em_t_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_t_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  grid%em_t_bt=initial_data_value
ALLOCATE(grid%em_mu_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_mu_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  grid%em_mu_b=initial_data_value
ALLOCATE(grid%em_mu_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_mu_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  grid%em_mu_bt=initial_data_value
ALLOCATE(grid%em_rqv_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_rqv_b(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  grid%em_rqv_b=initial_data_value
ALLOCATE(grid%em_rqv_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_rqv_bt(max(ed31,ed33),sd32:ed32,spec_bdy_width,4). ')
 endif
  grid%em_rqv_bt=initial_data_value
ALLOCATE(grid%smois(sm31:em31,model_config_rec%num_soil_layers,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%smois(sm31:em31,model_config_rec%num_soil_layers,sm33:em33). ')
 endif
  grid%smois=initial_data_value
ALLOCATE(grid%em_pb(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_pb(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%em_pb=initial_data_value
ALLOCATE(grid%em_fnm(sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_fnm(sm32:em32). ')
 endif
  grid%em_fnm=initial_data_value
ALLOCATE(grid%em_fnp(sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_fnp(sm32:em32). ')
 endif
  grid%em_fnp=initial_data_value
ALLOCATE(grid%em_rdnw(sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_rdnw(sm32:em32). ')
 endif
  grid%em_rdnw=initial_data_value
ALLOCATE(grid%em_rdn(sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_rdn(sm32:em32). ')
 endif
  grid%em_rdn=initial_data_value
ALLOCATE(grid%em_dnw(sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_dnw(sm32:em32). ')
 endif
  grid%em_dnw=initial_data_value
ALLOCATE(grid%em_dn(sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_dn(sm32:em32). ')
 endif
  grid%em_dn=initial_data_value
ALLOCATE(grid%em_znu(sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_znu(sm32:em32). ')
 endif
  grid%em_znu=initial_data_value
ALLOCATE(grid%em_znw(sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_znw(sm32:em32). ')
 endif
  grid%em_znw=initial_data_value
ALLOCATE(grid%em_t_base(sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_t_base(sm32:em32). ')
 endif
  grid%em_t_base=initial_data_value
ALLOCATE(grid%em_z(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_z(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%em_z=initial_data_value
grid%em_cfn=initial_data_value
grid%em_cfn1=initial_data_value
grid%em_epsts=initial_data_value
grid%em_step_number=0
ALLOCATE(grid%em_fcx(model_config_rec%spec_bdy_width),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_fcx(model_config_rec%spec_bdy_width). ')
 endif
  grid%em_fcx=initial_data_value
ALLOCATE(grid%em_gcx(model_config_rec%spec_bdy_width),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_gcx(model_config_rec%spec_bdy_width). ')
 endif
  grid%em_gcx=initial_data_value
ALLOCATE(grid%em_tslb(sm31:em31,model_config_rec%num_soil_layers,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_tslb(sm31:em31,model_config_rec%num_soil_layers,sm33:em33). ')
 endif
  grid%em_tslb=initial_data_value
ALLOCATE(grid%em_zs(model_config_rec%num_soil_layers),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_zs(model_config_rec%num_soil_layers). ')
 endif
  grid%em_zs=initial_data_value
ALLOCATE(grid%em_dzs(model_config_rec%num_soil_layers),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_dzs(model_config_rec%num_soil_layers). ')
 endif
  grid%em_dzs=initial_data_value
ALLOCATE(grid%em_h_diabatic(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_h_diabatic(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%em_h_diabatic=initial_data_value
ALLOCATE(grid%em_msft(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_msft(sm31:em31,sm33:em33). ')
 endif
  grid%em_msft=initial_data_value
ALLOCATE(grid%em_msfu(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_msfu(sm31:em31,sm33:em33). ')
 endif
  grid%em_msfu=initial_data_value
ALLOCATE(grid%em_msfv(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_msfv(sm31:em31,sm33:em33). ')
 endif
  grid%em_msfv=initial_data_value
ALLOCATE(grid%em_f(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_f(sm31:em31,sm33:em33). ')
 endif
  grid%em_f=initial_data_value
ALLOCATE(grid%em_e(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_e(sm31:em31,sm33:em33). ')
 endif
  grid%em_e=initial_data_value
ALLOCATE(grid%em_sina(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_sina(sm31:em31,sm33:em33). ')
 endif
  grid%em_sina=initial_data_value
ALLOCATE(grid%em_cosa(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_cosa(sm31:em31,sm33:em33). ')
 endif
  grid%em_cosa=initial_data_value
ALLOCATE(grid%em_ht(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_ht(sm31:em31,sm33:em33). ')
 endif
  grid%em_ht=initial_data_value
ALLOCATE(grid%em_tsk(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_tsk(sm31:em31,sm33:em33). ')
 endif
  grid%em_tsk=initial_data_value
ALLOCATE(grid%em_u_base(sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_u_base(sm32:em32). ')
 endif
  grid%em_u_base=initial_data_value
ALLOCATE(grid%em_v_base(sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_v_base(sm32:em32). ')
 endif
  grid%em_v_base=initial_data_value
ALLOCATE(grid%em_qv_base(sm32:em32),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_qv_base(sm32:em32). ')
 endif
  grid%em_qv_base=initial_data_value
grid%em_u_frame=initial_data_value
grid%em_v_frame=initial_data_value
grid%em_p_top=initial_data_value
ALLOCATE(grid%em_rthcuten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_rthcuten(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%em_rthcuten=initial_data_value
ALLOCATE(grid%em_rqvcuten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_rqvcuten(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%em_rqvcuten=initial_data_value
ALLOCATE(grid%em_rqrcuten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_rqrcuten(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%em_rqrcuten=initial_data_value
ALLOCATE(grid%em_rqccuten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_rqccuten(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%em_rqccuten=initial_data_value
ALLOCATE(grid%em_rqscuten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_rqscuten(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%em_rqscuten=initial_data_value
ALLOCATE(grid%em_rqicuten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_rqicuten(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%em_rqicuten=initial_data_value
ALLOCATE(grid%em_w0avg(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_w0avg(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%em_w0avg=initial_data_value
ALLOCATE(grid%em_rainc(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_rainc(sm31:em31,sm33:em33). ')
 endif
  grid%em_rainc=initial_data_value
ALLOCATE(grid%em_rainnc(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_rainnc(sm31:em31,sm33:em33). ')
 endif
  grid%em_rainnc=initial_data_value
ALLOCATE(grid%em_raincv(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_raincv(sm31:em31,sm33:em33). ')
 endif
  grid%em_raincv=initial_data_value
ALLOCATE(grid%em_rainncv(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_rainncv(sm31:em31,sm33:em33). ')
 endif
  grid%em_rainncv=initial_data_value
ALLOCATE(grid%em_rainbl(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_rainbl(sm31:em31,sm33:em33). ')
 endif
  grid%em_rainbl=initial_data_value
ALLOCATE(grid%em_nca(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_nca(sm31:em31,sm33:em33). ')
 endif
  grid%em_nca=initial_data_value
ALLOCATE(grid%em_lowlyr(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_lowlyr(sm31:em31,sm33:em33). ')
 endif
  grid%em_lowlyr=0
grid%em_stepcu=0
ALLOCATE(grid%em_rthraten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_rthraten(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%em_rthraten=initial_data_value
ALLOCATE(grid%em_rthratenlw(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_rthratenlw(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%em_rthratenlw=initial_data_value
ALLOCATE(grid%em_rthratensw(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_rthratensw(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%em_rthratensw=initial_data_value
ALLOCATE(grid%em_cldfra(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_cldfra(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%em_cldfra=initial_data_value
ALLOCATE(grid%em_gsw(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_gsw(sm31:em31,sm33:em33). ')
 endif
  grid%em_gsw=initial_data_value
ALLOCATE(grid%em_glw(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_glw(sm31:em31,sm33:em33). ')
 endif
  grid%em_glw=initial_data_value
ALLOCATE(grid%em_xlat(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_xlat(sm31:em31,sm33:em33). ')
 endif
  grid%em_xlat=initial_data_value
ALLOCATE(grid%em_xlong(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_xlong(sm31:em31,sm33:em33). ')
 endif
  grid%em_xlong=initial_data_value
ALLOCATE(grid%em_albedo(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_albedo(sm31:em31,sm33:em33). ')
 endif
  grid%em_albedo=initial_data_value
ALLOCATE(grid%em_emiss(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_emiss(sm31:em31,sm33:em33). ')
 endif
  grid%em_emiss=initial_data_value
ALLOCATE(grid%em_lu_index(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_lu_index(sm31:em31,sm33:em33). ')
 endif
  grid%em_lu_index=initial_data_value
ALLOCATE(grid%em_cldefi(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_cldefi(sm31:em31,sm33:em33). ')
 endif
  grid%em_cldefi=initial_data_value
grid%em_stepra=0
ALLOCATE(grid%em_rublten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_rublten(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%em_rublten=initial_data_value
ALLOCATE(grid%em_rvblten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_rvblten(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%em_rvblten=initial_data_value
ALLOCATE(grid%em_rthblten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_rthblten(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%em_rthblten=initial_data_value
ALLOCATE(grid%em_rqvblten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_rqvblten(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%em_rqvblten=initial_data_value
ALLOCATE(grid%em_rqcblten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_rqcblten(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%em_rqcblten=initial_data_value
ALLOCATE(grid%em_rqiblten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_rqiblten(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%em_rqiblten=initial_data_value
ALLOCATE(grid%em_tmn(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_tmn(sm31:em31,sm33:em33). ')
 endif
  grid%em_tmn=initial_data_value
ALLOCATE(grid%em_xland(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_xland(sm31:em31,sm33:em33). ')
 endif
  grid%em_xland=initial_data_value
ALLOCATE(grid%em_ust(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_ust(sm31:em31,sm33:em33). ')
 endif
  grid%em_ust=initial_data_value
ALLOCATE(grid%em_pblh(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_pblh(sm31:em31,sm33:em33). ')
 endif
  grid%em_pblh=initial_data_value
ALLOCATE(grid%em_capg(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_capg(sm31:em31,sm33:em33). ')
 endif
  grid%em_capg=initial_data_value
ALLOCATE(grid%em_thc(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_thc(sm31:em31,sm33:em33). ')
 endif
  grid%em_thc=initial_data_value
ALLOCATE(grid%em_hfx(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_hfx(sm31:em31,sm33:em33). ')
 endif
  grid%em_hfx=initial_data_value
ALLOCATE(grid%em_qfx(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_qfx(sm31:em31,sm33:em33). ')
 endif
  grid%em_qfx=initial_data_value
ALLOCATE(grid%em_snowc(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_snowc(sm31:em31,sm33:em33). ')
 endif
  grid%em_snowc=initial_data_value
ALLOCATE(grid%em_mavail(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_mavail(sm31:em31,sm33:em33). ')
 endif
  grid%em_mavail=initial_data_value
ALLOCATE(grid%em_tkesfcf(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_tkesfcf(sm31:em31,sm33:em33). ')
 endif
  grid%em_tkesfcf=initial_data_value
grid%em_stepbl=0
ALLOCATE(grid%em_taucldi(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_taucldi(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%em_taucldi=initial_data_value
ALLOCATE(grid%em_taucldc(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_taucldc(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%em_taucldc=initial_data_value
ALLOCATE(grid%em_defor11(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_defor11(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%em_defor11=initial_data_value
ALLOCATE(grid%em_defor22(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_defor22(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%em_defor22=initial_data_value
ALLOCATE(grid%em_defor12(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_defor12(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%em_defor12=initial_data_value
ALLOCATE(grid%em_defor33(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_defor33(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%em_defor33=initial_data_value
ALLOCATE(grid%em_defor13(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_defor13(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%em_defor13=initial_data_value
ALLOCATE(grid%em_defor23(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_defor23(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%em_defor23=initial_data_value
ALLOCATE(grid%em_xkmv(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_xkmv(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%em_xkmv=initial_data_value
ALLOCATE(grid%em_xkmh(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_xkmh(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%em_xkmh=initial_data_value
ALLOCATE(grid%em_xkmhd(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_xkmhd(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%em_xkmhd=initial_data_value
ALLOCATE(grid%em_xkhv(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_xkhv(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%em_xkhv=initial_data_value
ALLOCATE(grid%em_xkhh(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_xkhh(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%em_xkhh=initial_data_value
ALLOCATE(grid%em_div(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_div(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%em_div=initial_data_value
ALLOCATE(grid%em_bn2(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%em_bn2(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%em_bn2=initial_data_value
grid%em_rdx=initial_data_value
grid%em_rdy=initial_data_value
grid%em_dts=initial_data_value
grid%em_dtseps=initial_data_value
grid%em_resm=initial_data_value
grid%em_zetatop=initial_data_value
grid%em_cf1=initial_data_value
grid%em_cf2=initial_data_value
grid%em_cf3=initial_data_value
grid%dtbc=initial_data_value
ALLOCATE(grid%th2(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%th2(sm31:em31,sm33:em33). ')
 endif
  grid%th2=initial_data_value
ALLOCATE(grid%u10(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%u10(sm31:em31,sm33:em33). ')
 endif
  grid%u10=initial_data_value
ALLOCATE(grid%v10(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%v10(sm31:em31,sm33:em33). ')
 endif
  grid%v10=initial_data_value
ALLOCATE(grid%xice(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%xice(sm31:em31,sm33:em33). ')
 endif
  grid%xice=initial_data_value
ALLOCATE(grid%smstav(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%smstav(sm31:em31,sm33:em33). ')
 endif
  grid%smstav=initial_data_value
ALLOCATE(grid%smstot(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%smstot(sm31:em31,sm33:em33). ')
 endif
  grid%smstot=initial_data_value
ALLOCATE(grid%sfcrunoff(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%sfcrunoff(sm31:em31,sm33:em33). ')
 endif
  grid%sfcrunoff=initial_data_value
ALLOCATE(grid%udrunoff(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%udrunoff(sm31:em31,sm33:em33). ')
 endif
  grid%udrunoff=initial_data_value
ALLOCATE(grid%ivgtyp(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%ivgtyp(sm31:em31,sm33:em33). ')
 endif
  grid%ivgtyp=0
ALLOCATE(grid%isltyp(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%isltyp(sm31:em31,sm33:em33). ')
 endif
  grid%isltyp=0
ALLOCATE(grid%vegfra(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%vegfra(sm31:em31,sm33:em33). ')
 endif
  grid%vegfra=initial_data_value
ALLOCATE(grid%sfcevp(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%sfcevp(sm31:em31,sm33:em33). ')
 endif
  grid%sfcevp=initial_data_value
ALLOCATE(grid%grdflx(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%grdflx(sm31:em31,sm33:em33). ')
 endif
  grid%grdflx=initial_data_value
ALLOCATE(grid%albbck(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%albbck(sm31:em31,sm33:em33). ')
 endif
  grid%albbck=initial_data_value
ALLOCATE(grid%sfcexc(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%sfcexc(sm31:em31,sm33:em33). ')
 endif
  grid%sfcexc=initial_data_value
ALLOCATE(grid%acsnow(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%acsnow(sm31:em31,sm33:em33). ')
 endif
  grid%acsnow=initial_data_value
ALLOCATE(grid%acsnom(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%acsnom(sm31:em31,sm33:em33). ')
 endif
  grid%acsnom=initial_data_value
ALLOCATE(grid%snow(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%snow(sm31:em31,sm33:em33). ')
 endif
  grid%snow=initial_data_value
ALLOCATE(grid%canwat(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%canwat(sm31:em31,sm33:em33). ')
 endif
  grid%canwat=initial_data_value
ALLOCATE(grid%sst(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%sst(sm31:em31,sm33:em33). ')
 endif
  grid%sst=initial_data_value
ALLOCATE(grid%weasd(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%weasd(sm31:em31,sm33:em33). ')
 endif
  grid%weasd=initial_data_value
ALLOCATE(grid%mol(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%mol(sm31:em31,sm33:em33). ')
 endif
  grid%mol=initial_data_value
ALLOCATE(grid%znt(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%znt(sm31:em31,sm33:em33). ')
 endif
  grid%znt=initial_data_value
ALLOCATE(grid%tke_myj(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%tke_myj(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%tke_myj=initial_data_value
ALLOCATE(grid%thz0(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%thz0(sm31:em31,sm33:em33). ')
 endif
  grid%thz0=initial_data_value
ALLOCATE(grid%qz0(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%qz0(sm31:em31,sm33:em33). ')
 endif
  grid%qz0=initial_data_value
ALLOCATE(grid%uz0(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%uz0(sm31:em31,sm33:em33). ')
 endif
  grid%uz0=initial_data_value
ALLOCATE(grid%vz0(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%vz0(sm31:em31,sm33:em33). ')
 endif
  grid%vz0=initial_data_value
ALLOCATE(grid%uz0h(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%uz0h(sm31:em31,sm33:em33). ')
 endif
  grid%uz0h=initial_data_value
ALLOCATE(grid%vz0h(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%vz0h(sm31:em31,sm33:em33). ')
 endif
  grid%vz0h=initial_data_value
ALLOCATE(grid%dudt(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%dudt(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%dudt=initial_data_value
ALLOCATE(grid%dvdt(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%dvdt(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%dvdt=initial_data_value
ALLOCATE(grid%qsfc(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%qsfc(sm31:em31,sm33:em33). ')
 endif
  grid%qsfc=initial_data_value
ALLOCATE(grid%akhs(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%akhs(sm31:em31,sm33:em33). ')
 endif
  grid%akhs=initial_data_value
ALLOCATE(grid%akms(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%akms(sm31:em31,sm33:em33). ')
 endif
  grid%akms=initial_data_value
ALLOCATE(grid%htop(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%htop(sm31:em31,sm33:em33). ')
 endif
  grid%htop=initial_data_value
ALLOCATE(grid%hbot(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%hbot(sm31:em31,sm33:em33). ')
 endif
  grid%hbot=initial_data_value
ALLOCATE(grid%cuppt(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%cuppt(sm31:em31,sm33:em33). ')
 endif
  grid%cuppt=initial_data_value
ALLOCATE(grid%t0eta(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%t0eta(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%t0eta=initial_data_value
ALLOCATE(grid%q0eta(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%q0eta(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%q0eta=initial_data_value
ALLOCATE(grid%p0eta(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%p0eta(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%p0eta=initial_data_value
ALLOCATE(grid%f_ice_phy(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%f_ice_phy(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%f_ice_phy=initial_data_value
ALLOCATE(grid%f_rain_phy(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%f_rain_phy(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%f_rain_phy=initial_data_value
ALLOCATE(grid%f_rimef_phy(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%f_rimef_phy(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%f_rimef_phy=initial_data_value
ALLOCATE(grid%mass_flux(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%mass_flux(sm31:em31,sm33:em33). ')
 endif
  grid%mass_flux=initial_data_value
ALLOCATE(grid%apr_gr(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%apr_gr(sm31:em31,sm33:em33). ')
 endif
  grid%apr_gr=initial_data_value
ALLOCATE(grid%apr_w(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%apr_w(sm31:em31,sm33:em33). ')
 endif
  grid%apr_w=initial_data_value
ALLOCATE(grid%apr_mc(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%apr_mc(sm31:em31,sm33:em33). ')
 endif
  grid%apr_mc=initial_data_value
ALLOCATE(grid%apr_st(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%apr_st(sm31:em31,sm33:em33). ')
 endif
  grid%apr_st=initial_data_value
ALLOCATE(grid%apr_as(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%apr_as(sm31:em31,sm33:em33). ')
 endif
  grid%apr_as=initial_data_value
ALLOCATE(grid%apr_capma(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%apr_capma(sm31:em31,sm33:em33). ')
 endif
  grid%apr_capma=initial_data_value
ALLOCATE(grid%apr_capme(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%apr_capme(sm31:em31,sm33:em33). ')
 endif
  grid%apr_capme=initial_data_value
ALLOCATE(grid%apr_capmi(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%apr_capmi(sm31:em31,sm33:em33). ')
 endif
  grid%apr_capmi=initial_data_value
ALLOCATE(grid%xf_ens(sm31:em31,sm33:em33,model_config_rec%ensdim),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%xf_ens(sm31:em31,sm33:em33,model_config_rec%ensdim). ')
 endif
  grid%xf_ens=initial_data_value
ALLOCATE(grid%pr_ens(sm31:em31,sm33:em33,model_config_rec%ensdim),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%pr_ens(sm31:em31,sm33:em33,model_config_rec%ensdim). ')
 endif
  grid%pr_ens=initial_data_value
ALLOCATE(grid%rthften(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%rthften(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%rthften=initial_data_value
ALLOCATE(grid%rqvften(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%rqvften(sm31:em31,sm32:em32,sm33:em33). ')
 endif
  grid%rqvften=initial_data_value
ALLOCATE(grid%snowh(sm31:em31,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%snowh(sm31:em31,sm33:em33). ')
 endif
  grid%snowh=initial_data_value
ALLOCATE(grid%smfr3d(sm31:em31,model_config_rec%num_soil_layers,sm33:em33),STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( 'frame/module_domain.f: Failed to allocate grid%smfr3d(sm31:em31,model_config_rec%num_soil_layers,sm33:em33). ')
 endif
  grid%smfr3d=initial_data_value
grid%number_at_same_level=0
grid%itimestep=0
grid%oid=0
grid%auxhist1_oid=0
grid%auxhist2_oid=0
grid%auxhist3_oid=0
grid%auxhist4_oid=0
grid%auxhist5_oid=0
grid%auxinput1_oid=0
grid%auxinput2_oid=0
grid%auxinput3_oid=0
grid%auxinput4_oid=0
grid%auxinput5_oid=0
grid%nframes=0
grid%lbc_fid=0
grid%time_step=0
grid%time_step_fract_num=0
grid%time_step_fract_den=0
grid%max_dom=0
grid%dyn_opt=0
grid%rk_ord=0
grid%diff_opt=0
grid%km_opt=0
grid%damp_opt=0
grid%isfflx=0
grid%ifsnow=0
grid%icloud=0
grid%num_soil_layers=0
grid%num_land_cat=0
grid%num_soil_cat=0
grid%spec_bdy_width=0
grid%spec_zone=0
grid%relax_zone=0
grid%ensdim=0
grid%tile_sz_x=0
grid%tile_sz_y=0
grid%numtiles=0
grid%debug_level=0
grid%irand=0
grid%run_days=0
grid%run_hours=0
grid%run_minutes=0
grid%run_seconds=0
grid%start_year=0
grid%start_month=0
grid%start_day=0
grid%start_hour=0
grid%start_minute=0
grid%start_second=0
grid%end_year=0
grid%end_month=0
grid%end_day=0
grid%end_hour=0
grid%end_minute=0
grid%end_second=0
grid%grid_id=0
grid%level=0
grid%s_we=0
grid%e_we=0
grid%s_sn=0
grid%e_sn=0
grid%s_vert=0
grid%e_vert=0
grid%history_interval=0
grid%auxhist1_interval=0
grid%auxhist2_interval=0
grid%auxhist3_interval=0
grid%auxhist4_interval=0
grid%auxhist5_interval=0
grid%auxinput1_interval=0
grid%auxinput2_interval=0
grid%auxinput3_interval=0
grid%auxinput4_interval=0
grid%auxinput5_interval=0
grid%restart_interval=0
grid%frames_per_outfile=0
grid%time_step_sound=0
grid%parent_id=0
grid%i_parent_start=0
grid%j_parent_start=0
grid%shw=0
grid%parent_grid_ratio=0
grid%parent_time_step_ratio=0
grid%moad_grid_ratio=0
grid%moad_time_step_ratio=0
grid%dx=initial_data_value
grid%dy=initial_data_value
grid%dt=initial_data_value
grid%ztop=initial_data_value
grid%zdamp=initial_data_value
grid%dampcoef=initial_data_value
grid%smdiv=initial_data_value
grid%emdiv=initial_data_value
grid%epssm=initial_data_value
grid%khdif=initial_data_value
grid%kvdif=initial_data_value
grid%mix_cr_len=initial_data_value
grid%tke_upper_bound=initial_data_value
grid%kh_tke_upper_bound=initial_data_value
grid%kv_tke_upper_bound=initial_data_value
grid%radt=initial_data_value
grid%bldt=initial_data_value
grid%cudt=initial_data_value
grid%gsmdt=initial_data_value
grid%julyr=0
grid%julday=0
grid%gmt=initial_data_value
grid%idtad=0
grid%nsoil=0
grid%nphs=0
grid%ncnvc=0
grid%nrads=0
grid%nradl=0
grid%sigma=0
grid%chem_opt=0
grid%mp_physics=0
grid%ra_lw_physics=0
grid%ra_sw_physics=0
grid%bl_sfclay_physics=0
grid%bl_surface_physics=0
grid%bl_pbl_physics=0
grid%cu_physics=0
grid%h_mom_adv_order=0
grid%v_mom_adv_order=0
grid%h_sca_adv_order=0
grid%v_sca_adv_order=0
grid%io_form_input=0
grid%io_form_auxinput1=0
grid%io_form_auxinput2=0
grid%io_form_auxinput3=0
grid%io_form_auxinput4=0
grid%io_form_auxinput5=0
grid%io_form_history=0
grid%io_form_auxhist1=0
grid%io_form_auxhist2=0
grid%io_form_auxhist3=0
grid%io_form_auxhist4=0
grid%io_form_auxhist5=0
grid%io_form_restart=0
grid%io_form_boundary=0
grid%interval_seconds=0
grid%real_data_init_type=0
grid%cen_lat=initial_data_value
grid%cen_lon=initial_data_value
grid%truelat1=initial_data_value
grid%truelat2=initial_data_value
grid%bdyfrq=initial_data_value
grid%iswater=0
grid%isice=0
grid%map_proj=0
!ENDOFREGISTRYGENERATEDINCLUDE
