!STARTOFREGISTRYGENERATEDINCLUDE 'inc/nodyn_deallocs.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
IF ( ASSOCIATED( grid%lu_index ) ) THEN 
  DEALLOCATE(grid%lu_index,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%lu_index. ')
 endif
  NULLIFY(grid%lu_index)
ENDIF
IF ( ASSOCIATED( grid%lu_mask ) ) THEN 
  DEALLOCATE(grid%lu_mask,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%lu_mask. ')
 endif
  NULLIFY(grid%lu_mask)
ENDIF
IF ( ASSOCIATED( grid%vegcat ) ) THEN 
  DEALLOCATE(grid%vegcat,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%vegcat. ')
 endif
  NULLIFY(grid%vegcat)
ENDIF
IF ( ASSOCIATED( grid%soilcat ) ) THEN 
  DEALLOCATE(grid%soilcat,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%soilcat. ')
 endif
  NULLIFY(grid%soilcat)
ENDIF
IF ( ASSOCIATED( grid%input_soil_cat ) ) THEN 
  DEALLOCATE(grid%input_soil_cat,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%input_soil_cat. ')
 endif
  NULLIFY(grid%input_soil_cat)
ENDIF
IF ( ASSOCIATED( grid%xice_gc ) ) THEN 
  DEALLOCATE(grid%xice_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%xice_gc. ')
 endif
  NULLIFY(grid%xice_gc)
ENDIF
IF ( ASSOCIATED( grid%snoalb ) ) THEN 
  DEALLOCATE(grid%snoalb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%snoalb. ')
 endif
  NULLIFY(grid%snoalb)
ENDIF
IF ( ASSOCIATED( grid%soilcbot_gc ) ) THEN 
  DEALLOCATE(grid%soilcbot_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%soilcbot_gc. ')
 endif
  NULLIFY(grid%soilcbot_gc)
ENDIF
IF ( ASSOCIATED( grid%soilctop_gc ) ) THEN 
  DEALLOCATE(grid%soilctop_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%soilctop_gc. ')
 endif
  NULLIFY(grid%soilctop_gc)
ENDIF
IF ( ASSOCIATED( grid%landusef_gc ) ) THEN 
  DEALLOCATE(grid%landusef_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%landusef_gc. ')
 endif
  NULLIFY(grid%landusef_gc)
ENDIF
IF ( ASSOCIATED( grid%imask_nostag ) ) THEN 
  DEALLOCATE(grid%imask_nostag,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%imask_nostag. ')
 endif
  NULLIFY(grid%imask_nostag)
ENDIF
IF ( ASSOCIATED( grid%imask_xstag ) ) THEN 
  DEALLOCATE(grid%imask_xstag,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%imask_xstag. ')
 endif
  NULLIFY(grid%imask_xstag)
ENDIF
IF ( ASSOCIATED( grid%imask_ystag ) ) THEN 
  DEALLOCATE(grid%imask_ystag,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%imask_ystag. ')
 endif
  NULLIFY(grid%imask_ystag)
ENDIF
IF ( ASSOCIATED( grid%imask_xystag ) ) THEN 
  DEALLOCATE(grid%imask_xystag,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%imask_xystag. ')
 endif
  NULLIFY(grid%imask_xystag)
ENDIF
IF ( ASSOCIATED( grid%sm000007 ) ) THEN 
  DEALLOCATE(grid%sm000007,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%sm000007. ')
 endif
  NULLIFY(grid%sm000007)
ENDIF
IF ( ASSOCIATED( grid%sm007028 ) ) THEN 
  DEALLOCATE(grid%sm007028,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%sm007028. ')
 endif
  NULLIFY(grid%sm007028)
ENDIF
IF ( ASSOCIATED( grid%sm028100 ) ) THEN 
  DEALLOCATE(grid%sm028100,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%sm028100. ')
 endif
  NULLIFY(grid%sm028100)
ENDIF
IF ( ASSOCIATED( grid%sm100255 ) ) THEN 
  DEALLOCATE(grid%sm100255,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%sm100255. ')
 endif
  NULLIFY(grid%sm100255)
ENDIF
IF ( ASSOCIATED( grid%st000007 ) ) THEN 
  DEALLOCATE(grid%st000007,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%st000007. ')
 endif
  NULLIFY(grid%st000007)
ENDIF
IF ( ASSOCIATED( grid%st007028 ) ) THEN 
  DEALLOCATE(grid%st007028,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%st007028. ')
 endif
  NULLIFY(grid%st007028)
ENDIF
IF ( ASSOCIATED( grid%st028100 ) ) THEN 
  DEALLOCATE(grid%st028100,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%st028100. ')
 endif
  NULLIFY(grid%st028100)
ENDIF
IF ( ASSOCIATED( grid%st100255 ) ) THEN 
  DEALLOCATE(grid%st100255,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%st100255. ')
 endif
  NULLIFY(grid%st100255)
ENDIF
IF ( ASSOCIATED( grid%sm000010 ) ) THEN 
  DEALLOCATE(grid%sm000010,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%sm000010. ')
 endif
  NULLIFY(grid%sm000010)
ENDIF
IF ( ASSOCIATED( grid%sm010040 ) ) THEN 
  DEALLOCATE(grid%sm010040,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%sm010040. ')
 endif
  NULLIFY(grid%sm010040)
ENDIF
IF ( ASSOCIATED( grid%sm040100 ) ) THEN 
  DEALLOCATE(grid%sm040100,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%sm040100. ')
 endif
  NULLIFY(grid%sm040100)
ENDIF
IF ( ASSOCIATED( grid%sm100200 ) ) THEN 
  DEALLOCATE(grid%sm100200,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%sm100200. ')
 endif
  NULLIFY(grid%sm100200)
ENDIF
IF ( ASSOCIATED( grid%sm010200 ) ) THEN 
  DEALLOCATE(grid%sm010200,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%sm010200. ')
 endif
  NULLIFY(grid%sm010200)
ENDIF
IF ( ASSOCIATED( grid%soilm000 ) ) THEN 
  DEALLOCATE(grid%soilm000,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%soilm000. ')
 endif
  NULLIFY(grid%soilm000)
ENDIF
IF ( ASSOCIATED( grid%soilm005 ) ) THEN 
  DEALLOCATE(grid%soilm005,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%soilm005. ')
 endif
  NULLIFY(grid%soilm005)
ENDIF
IF ( ASSOCIATED( grid%soilm020 ) ) THEN 
  DEALLOCATE(grid%soilm020,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%soilm020. ')
 endif
  NULLIFY(grid%soilm020)
ENDIF
IF ( ASSOCIATED( grid%soilm040 ) ) THEN 
  DEALLOCATE(grid%soilm040,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%soilm040. ')
 endif
  NULLIFY(grid%soilm040)
ENDIF
IF ( ASSOCIATED( grid%soilm160 ) ) THEN 
  DEALLOCATE(grid%soilm160,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%soilm160. ')
 endif
  NULLIFY(grid%soilm160)
ENDIF
IF ( ASSOCIATED( grid%soilm300 ) ) THEN 
  DEALLOCATE(grid%soilm300,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%soilm300. ')
 endif
  NULLIFY(grid%soilm300)
ENDIF
IF ( ASSOCIATED( grid%sw000010 ) ) THEN 
  DEALLOCATE(grid%sw000010,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%sw000010. ')
 endif
  NULLIFY(grid%sw000010)
ENDIF
IF ( ASSOCIATED( grid%sw010040 ) ) THEN 
  DEALLOCATE(grid%sw010040,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%sw010040. ')
 endif
  NULLIFY(grid%sw010040)
ENDIF
IF ( ASSOCIATED( grid%sw040100 ) ) THEN 
  DEALLOCATE(grid%sw040100,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%sw040100. ')
 endif
  NULLIFY(grid%sw040100)
ENDIF
IF ( ASSOCIATED( grid%sw100200 ) ) THEN 
  DEALLOCATE(grid%sw100200,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%sw100200. ')
 endif
  NULLIFY(grid%sw100200)
ENDIF
IF ( ASSOCIATED( grid%sw010200 ) ) THEN 
  DEALLOCATE(grid%sw010200,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%sw010200. ')
 endif
  NULLIFY(grid%sw010200)
ENDIF
IF ( ASSOCIATED( grid%soilw000 ) ) THEN 
  DEALLOCATE(grid%soilw000,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%soilw000. ')
 endif
  NULLIFY(grid%soilw000)
ENDIF
IF ( ASSOCIATED( grid%soilw005 ) ) THEN 
  DEALLOCATE(grid%soilw005,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%soilw005. ')
 endif
  NULLIFY(grid%soilw005)
ENDIF
IF ( ASSOCIATED( grid%soilw020 ) ) THEN 
  DEALLOCATE(grid%soilw020,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%soilw020. ')
 endif
  NULLIFY(grid%soilw020)
ENDIF
IF ( ASSOCIATED( grid%soilw040 ) ) THEN 
  DEALLOCATE(grid%soilw040,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%soilw040. ')
 endif
  NULLIFY(grid%soilw040)
ENDIF
IF ( ASSOCIATED( grid%soilw160 ) ) THEN 
  DEALLOCATE(grid%soilw160,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%soilw160. ')
 endif
  NULLIFY(grid%soilw160)
ENDIF
IF ( ASSOCIATED( grid%soilw300 ) ) THEN 
  DEALLOCATE(grid%soilw300,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%soilw300. ')
 endif
  NULLIFY(grid%soilw300)
ENDIF
IF ( ASSOCIATED( grid%st000010 ) ) THEN 
  DEALLOCATE(grid%st000010,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%st000010. ')
 endif
  NULLIFY(grid%st000010)
ENDIF
IF ( ASSOCIATED( grid%st010040 ) ) THEN 
  DEALLOCATE(grid%st010040,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%st010040. ')
 endif
  NULLIFY(grid%st010040)
ENDIF
IF ( ASSOCIATED( grid%st040100 ) ) THEN 
  DEALLOCATE(grid%st040100,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%st040100. ')
 endif
  NULLIFY(grid%st040100)
ENDIF
IF ( ASSOCIATED( grid%st100200 ) ) THEN 
  DEALLOCATE(grid%st100200,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%st100200. ')
 endif
  NULLIFY(grid%st100200)
ENDIF
IF ( ASSOCIATED( grid%st010200 ) ) THEN 
  DEALLOCATE(grid%st010200,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%st010200. ')
 endif
  NULLIFY(grid%st010200)
ENDIF
IF ( ASSOCIATED( grid%soilt000 ) ) THEN 
  DEALLOCATE(grid%soilt000,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%soilt000. ')
 endif
  NULLIFY(grid%soilt000)
ENDIF
IF ( ASSOCIATED( grid%soilt005 ) ) THEN 
  DEALLOCATE(grid%soilt005,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%soilt005. ')
 endif
  NULLIFY(grid%soilt005)
ENDIF
IF ( ASSOCIATED( grid%soilt020 ) ) THEN 
  DEALLOCATE(grid%soilt020,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%soilt020. ')
 endif
  NULLIFY(grid%soilt020)
ENDIF
IF ( ASSOCIATED( grid%soilt040 ) ) THEN 
  DEALLOCATE(grid%soilt040,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%soilt040. ')
 endif
  NULLIFY(grid%soilt040)
ENDIF
IF ( ASSOCIATED( grid%soilt160 ) ) THEN 
  DEALLOCATE(grid%soilt160,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%soilt160. ')
 endif
  NULLIFY(grid%soilt160)
ENDIF
IF ( ASSOCIATED( grid%soilt300 ) ) THEN 
  DEALLOCATE(grid%soilt300,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%soilt300. ')
 endif
  NULLIFY(grid%soilt300)
ENDIF
IF ( ASSOCIATED( grid%landmask ) ) THEN 
  DEALLOCATE(grid%landmask,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%landmask. ')
 endif
  NULLIFY(grid%landmask)
ENDIF
IF ( ASSOCIATED( grid%topostdv ) ) THEN 
  DEALLOCATE(grid%topostdv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%topostdv. ')
 endif
  NULLIFY(grid%topostdv)
ENDIF
IF ( ASSOCIATED( grid%toposlpx ) ) THEN 
  DEALLOCATE(grid%toposlpx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%toposlpx. ')
 endif
  NULLIFY(grid%toposlpx)
ENDIF
IF ( ASSOCIATED( grid%toposlpy ) ) THEN 
  DEALLOCATE(grid%toposlpy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%toposlpy. ')
 endif
  NULLIFY(grid%toposlpy)
ENDIF
IF ( ASSOCIATED( grid%greenmax ) ) THEN 
  DEALLOCATE(grid%greenmax,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%greenmax. ')
 endif
  NULLIFY(grid%greenmax)
ENDIF
IF ( ASSOCIATED( grid%greenmin ) ) THEN 
  DEALLOCATE(grid%greenmin,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%greenmin. ')
 endif
  NULLIFY(grid%greenmin)
ENDIF
IF ( ASSOCIATED( grid%albedomx ) ) THEN 
  DEALLOCATE(grid%albedomx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%albedomx. ')
 endif
  NULLIFY(grid%albedomx)
ENDIF
IF ( ASSOCIATED( grid%slopecat ) ) THEN 
  DEALLOCATE(grid%slopecat,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%slopecat. ')
 endif
  NULLIFY(grid%slopecat)
ENDIF
IF ( ASSOCIATED( grid%toposoil ) ) THEN 
  DEALLOCATE(grid%toposoil,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%toposoil. ')
 endif
  NULLIFY(grid%toposoil)
ENDIF
IF ( ASSOCIATED( grid%landusef ) ) THEN 
  DEALLOCATE(grid%landusef,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%landusef. ')
 endif
  NULLIFY(grid%landusef)
ENDIF
IF ( ASSOCIATED( grid%soilctop ) ) THEN 
  DEALLOCATE(grid%soilctop,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%soilctop. ')
 endif
  NULLIFY(grid%soilctop)
ENDIF
IF ( ASSOCIATED( grid%soilcbot ) ) THEN 
  DEALLOCATE(grid%soilcbot,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%soilcbot. ')
 endif
  NULLIFY(grid%soilcbot)
ENDIF
IF ( ASSOCIATED( grid%moist ) ) THEN 
  DEALLOCATE(grid%moist,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%moist. ')
 endif
  NULLIFY(grid%moist)
ENDIF
IF ( ASSOCIATED( grid%scalar ) ) THEN 
  DEALLOCATE(grid%scalar,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%scalar. ')
 endif
  NULLIFY(grid%scalar)
ENDIF
IF ( ASSOCIATED( grid%scalar_b ) ) THEN 
  DEALLOCATE(grid%scalar_b,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%scalar_b. ')
 endif
  NULLIFY(grid%scalar_b)
ENDIF
IF ( ASSOCIATED( grid%scalar_bt ) ) THEN 
  DEALLOCATE(grid%scalar_bt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%scalar_bt. ')
 endif
  NULLIFY(grid%scalar_bt)
ENDIF
IF ( ASSOCIATED( grid%chem ) ) THEN 
  DEALLOCATE(grid%chem,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%chem. ')
 endif
  NULLIFY(grid%chem)
ENDIF
IF ( ASSOCIATED( grid%smois ) ) THEN 
  DEALLOCATE(grid%smois,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%smois. ')
 endif
  NULLIFY(grid%smois)
ENDIF
IF ( ASSOCIATED( grid%tslb ) ) THEN 
  DEALLOCATE(grid%tslb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%tslb. ')
 endif
  NULLIFY(grid%tslb)
ENDIF
IF ( ASSOCIATED( grid%gsw ) ) THEN 
  DEALLOCATE(grid%gsw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%gsw. ')
 endif
  NULLIFY(grid%gsw)
ENDIF
IF ( ASSOCIATED( grid%xlat ) ) THEN 
  DEALLOCATE(grid%xlat,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%xlat. ')
 endif
  NULLIFY(grid%xlat)
ENDIF
IF ( ASSOCIATED( grid%xlong ) ) THEN 
  DEALLOCATE(grid%xlong,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%xlong. ')
 endif
  NULLIFY(grid%xlong)
ENDIF
IF ( ASSOCIATED( grid%xland ) ) THEN 
  DEALLOCATE(grid%xland,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%xland. ')
 endif
  NULLIFY(grid%xland)
ENDIF
IF ( ASSOCIATED( grid%raincv ) ) THEN 
  DEALLOCATE(grid%raincv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%raincv. ')
 endif
  NULLIFY(grid%raincv)
ENDIF
IF ( ASSOCIATED( grid%psfc ) ) THEN 
  DEALLOCATE(grid%psfc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%psfc. ')
 endif
  NULLIFY(grid%psfc)
ENDIF
IF ( ASSOCIATED( grid%th2 ) ) THEN 
  DEALLOCATE(grid%th2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%th2. ')
 endif
  NULLIFY(grid%th2)
ENDIF
IF ( ASSOCIATED( grid%t2 ) ) THEN 
  DEALLOCATE(grid%t2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%t2. ')
 endif
  NULLIFY(grid%t2)
ENDIF
IF ( ASSOCIATED( grid%u10 ) ) THEN 
  DEALLOCATE(grid%u10,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%u10. ')
 endif
  NULLIFY(grid%u10)
ENDIF
IF ( ASSOCIATED( grid%v10 ) ) THEN 
  DEALLOCATE(grid%v10,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%v10. ')
 endif
  NULLIFY(grid%v10)
ENDIF
IF ( ASSOCIATED( grid%xice ) ) THEN 
  DEALLOCATE(grid%xice,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%xice. ')
 endif
  NULLIFY(grid%xice)
ENDIF
IF ( ASSOCIATED( grid%smstav ) ) THEN 
  DEALLOCATE(grid%smstav,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%smstav. ')
 endif
  NULLIFY(grid%smstav)
ENDIF
IF ( ASSOCIATED( grid%smstot ) ) THEN 
  DEALLOCATE(grid%smstot,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%smstot. ')
 endif
  NULLIFY(grid%smstot)
ENDIF
IF ( ASSOCIATED( grid%sfcrunoff ) ) THEN 
  DEALLOCATE(grid%sfcrunoff,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%sfcrunoff. ')
 endif
  NULLIFY(grid%sfcrunoff)
ENDIF
IF ( ASSOCIATED( grid%udrunoff ) ) THEN 
  DEALLOCATE(grid%udrunoff,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%udrunoff. ')
 endif
  NULLIFY(grid%udrunoff)
ENDIF
IF ( ASSOCIATED( grid%ivgtyp ) ) THEN 
  DEALLOCATE(grid%ivgtyp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%ivgtyp. ')
 endif
  NULLIFY(grid%ivgtyp)
ENDIF
IF ( ASSOCIATED( grid%isltyp ) ) THEN 
  DEALLOCATE(grid%isltyp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%isltyp. ')
 endif
  NULLIFY(grid%isltyp)
ENDIF
IF ( ASSOCIATED( grid%vegfra ) ) THEN 
  DEALLOCATE(grid%vegfra,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%vegfra. ')
 endif
  NULLIFY(grid%vegfra)
ENDIF
IF ( ASSOCIATED( grid%sfcevp ) ) THEN 
  DEALLOCATE(grid%sfcevp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%sfcevp. ')
 endif
  NULLIFY(grid%sfcevp)
ENDIF
IF ( ASSOCIATED( grid%grdflx ) ) THEN 
  DEALLOCATE(grid%grdflx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%grdflx. ')
 endif
  NULLIFY(grid%grdflx)
ENDIF
IF ( ASSOCIATED( grid%albbck ) ) THEN 
  DEALLOCATE(grid%albbck,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%albbck. ')
 endif
  NULLIFY(grid%albbck)
ENDIF
IF ( ASSOCIATED( grid%sfcexc ) ) THEN 
  DEALLOCATE(grid%sfcexc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%sfcexc. ')
 endif
  NULLIFY(grid%sfcexc)
ENDIF
IF ( ASSOCIATED( grid%acsnow ) ) THEN 
  DEALLOCATE(grid%acsnow,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%acsnow. ')
 endif
  NULLIFY(grid%acsnow)
ENDIF
IF ( ASSOCIATED( grid%acsnom ) ) THEN 
  DEALLOCATE(grid%acsnom,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%acsnom. ')
 endif
  NULLIFY(grid%acsnom)
ENDIF
IF ( ASSOCIATED( grid%rmol ) ) THEN 
  DEALLOCATE(grid%rmol,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%rmol. ')
 endif
  NULLIFY(grid%rmol)
ENDIF
IF ( ASSOCIATED( grid%snow ) ) THEN 
  DEALLOCATE(grid%snow,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%snow. ')
 endif
  NULLIFY(grid%snow)
ENDIF
IF ( ASSOCIATED( grid%canwat ) ) THEN 
  DEALLOCATE(grid%canwat,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%canwat. ')
 endif
  NULLIFY(grid%canwat)
ENDIF
IF ( ASSOCIATED( grid%sst ) ) THEN 
  DEALLOCATE(grid%sst,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%sst. ')
 endif
  NULLIFY(grid%sst)
ENDIF
IF ( ASSOCIATED( grid%weasd ) ) THEN 
  DEALLOCATE(grid%weasd,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%weasd. ')
 endif
  NULLIFY(grid%weasd)
ENDIF
IF ( ASSOCIATED( grid%znt ) ) THEN 
  DEALLOCATE(grid%znt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%znt. ')
 endif
  NULLIFY(grid%znt)
ENDIF
IF ( ASSOCIATED( grid%mol ) ) THEN 
  DEALLOCATE(grid%mol,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%mol. ')
 endif
  NULLIFY(grid%mol)
ENDIF
IF ( ASSOCIATED( grid%tke_myj ) ) THEN 
  DEALLOCATE(grid%tke_myj,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%tke_myj. ')
 endif
  NULLIFY(grid%tke_myj)
ENDIF
IF ( ASSOCIATED( grid%el_myj ) ) THEN 
  DEALLOCATE(grid%el_myj,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%el_myj. ')
 endif
  NULLIFY(grid%el_myj)
ENDIF
IF ( ASSOCIATED( grid%exch_h ) ) THEN 
  DEALLOCATE(grid%exch_h,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%exch_h. ')
 endif
  NULLIFY(grid%exch_h)
ENDIF
IF ( ASSOCIATED( grid%thz0 ) ) THEN 
  DEALLOCATE(grid%thz0,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%thz0. ')
 endif
  NULLIFY(grid%thz0)
ENDIF
IF ( ASSOCIATED( grid%qz0 ) ) THEN 
  DEALLOCATE(grid%qz0,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%qz0. ')
 endif
  NULLIFY(grid%qz0)
ENDIF
IF ( ASSOCIATED( grid%uz0 ) ) THEN 
  DEALLOCATE(grid%uz0,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%uz0. ')
 endif
  NULLIFY(grid%uz0)
ENDIF
IF ( ASSOCIATED( grid%vz0 ) ) THEN 
  DEALLOCATE(grid%vz0,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%vz0. ')
 endif
  NULLIFY(grid%vz0)
ENDIF
IF ( ASSOCIATED( grid%flhc ) ) THEN 
  DEALLOCATE(grid%flhc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%flhc. ')
 endif
  NULLIFY(grid%flhc)
ENDIF
IF ( ASSOCIATED( grid%flqc ) ) THEN 
  DEALLOCATE(grid%flqc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%flqc. ')
 endif
  NULLIFY(grid%flqc)
ENDIF
IF ( ASSOCIATED( grid%qsg ) ) THEN 
  DEALLOCATE(grid%qsg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%qsg. ')
 endif
  NULLIFY(grid%qsg)
ENDIF
IF ( ASSOCIATED( grid%qvg ) ) THEN 
  DEALLOCATE(grid%qvg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%qvg. ')
 endif
  NULLIFY(grid%qvg)
ENDIF
IF ( ASSOCIATED( grid%qcg ) ) THEN 
  DEALLOCATE(grid%qcg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%qcg. ')
 endif
  NULLIFY(grid%qcg)
ENDIF
IF ( ASSOCIATED( grid%soilt1 ) ) THEN 
  DEALLOCATE(grid%soilt1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%soilt1. ')
 endif
  NULLIFY(grid%soilt1)
ENDIF
IF ( ASSOCIATED( grid%tsnav ) ) THEN 
  DEALLOCATE(grid%tsnav,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%tsnav. ')
 endif
  NULLIFY(grid%tsnav)
ENDIF
IF ( ASSOCIATED( grid%uz0h ) ) THEN 
  DEALLOCATE(grid%uz0h,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%uz0h. ')
 endif
  NULLIFY(grid%uz0h)
ENDIF
IF ( ASSOCIATED( grid%vz0h ) ) THEN 
  DEALLOCATE(grid%vz0h,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%vz0h. ')
 endif
  NULLIFY(grid%vz0h)
ENDIF
IF ( ASSOCIATED( grid%dudt ) ) THEN 
  DEALLOCATE(grid%dudt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%dudt. ')
 endif
  NULLIFY(grid%dudt)
ENDIF
IF ( ASSOCIATED( grid%dvdt ) ) THEN 
  DEALLOCATE(grid%dvdt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%dvdt. ')
 endif
  NULLIFY(grid%dvdt)
ENDIF
IF ( ASSOCIATED( grid%qsfc ) ) THEN 
  DEALLOCATE(grid%qsfc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%qsfc. ')
 endif
  NULLIFY(grid%qsfc)
ENDIF
IF ( ASSOCIATED( grid%akhs ) ) THEN 
  DEALLOCATE(grid%akhs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%akhs. ')
 endif
  NULLIFY(grid%akhs)
ENDIF
IF ( ASSOCIATED( grid%akms ) ) THEN 
  DEALLOCATE(grid%akms,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%akms. ')
 endif
  NULLIFY(grid%akms)
ENDIF
IF ( ASSOCIATED( grid%htop ) ) THEN 
  DEALLOCATE(grid%htop,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%htop. ')
 endif
  NULLIFY(grid%htop)
ENDIF
IF ( ASSOCIATED( grid%hbot ) ) THEN 
  DEALLOCATE(grid%hbot,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%hbot. ')
 endif
  NULLIFY(grid%hbot)
ENDIF
IF ( ASSOCIATED( grid%htopr ) ) THEN 
  DEALLOCATE(grid%htopr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%htopr. ')
 endif
  NULLIFY(grid%htopr)
ENDIF
IF ( ASSOCIATED( grid%hbotr ) ) THEN 
  DEALLOCATE(grid%hbotr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%hbotr. ')
 endif
  NULLIFY(grid%hbotr)
ENDIF
IF ( ASSOCIATED( grid%htopd ) ) THEN 
  DEALLOCATE(grid%htopd,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%htopd. ')
 endif
  NULLIFY(grid%htopd)
ENDIF
IF ( ASSOCIATED( grid%hbotd ) ) THEN 
  DEALLOCATE(grid%hbotd,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%hbotd. ')
 endif
  NULLIFY(grid%hbotd)
ENDIF
IF ( ASSOCIATED( grid%htops ) ) THEN 
  DEALLOCATE(grid%htops,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%htops. ')
 endif
  NULLIFY(grid%htops)
ENDIF
IF ( ASSOCIATED( grid%hbots ) ) THEN 
  DEALLOCATE(grid%hbots,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%hbots. ')
 endif
  NULLIFY(grid%hbots)
ENDIF
IF ( ASSOCIATED( grid%cuppt ) ) THEN 
  DEALLOCATE(grid%cuppt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%cuppt. ')
 endif
  NULLIFY(grid%cuppt)
ENDIF
IF ( ASSOCIATED( grid%cprate ) ) THEN 
  DEALLOCATE(grid%cprate,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%cprate. ')
 endif
  NULLIFY(grid%cprate)
ENDIF
IF ( ASSOCIATED( grid%f_ice_phy ) ) THEN 
  DEALLOCATE(grid%f_ice_phy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%f_ice_phy. ')
 endif
  NULLIFY(grid%f_ice_phy)
ENDIF
IF ( ASSOCIATED( grid%f_rain_phy ) ) THEN 
  DEALLOCATE(grid%f_rain_phy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%f_rain_phy. ')
 endif
  NULLIFY(grid%f_rain_phy)
ENDIF
IF ( ASSOCIATED( grid%f_rimef_phy ) ) THEN 
  DEALLOCATE(grid%f_rimef_phy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%f_rimef_phy. ')
 endif
  NULLIFY(grid%f_rimef_phy)
ENDIF
IF ( ASSOCIATED( grid%mass_flux ) ) THEN 
  DEALLOCATE(grid%mass_flux,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%mass_flux. ')
 endif
  NULLIFY(grid%mass_flux)
ENDIF
IF ( ASSOCIATED( grid%apr_gr ) ) THEN 
  DEALLOCATE(grid%apr_gr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%apr_gr. ')
 endif
  NULLIFY(grid%apr_gr)
ENDIF
IF ( ASSOCIATED( grid%apr_w ) ) THEN 
  DEALLOCATE(grid%apr_w,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%apr_w. ')
 endif
  NULLIFY(grid%apr_w)
ENDIF
IF ( ASSOCIATED( grid%apr_mc ) ) THEN 
  DEALLOCATE(grid%apr_mc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%apr_mc. ')
 endif
  NULLIFY(grid%apr_mc)
ENDIF
IF ( ASSOCIATED( grid%apr_st ) ) THEN 
  DEALLOCATE(grid%apr_st,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%apr_st. ')
 endif
  NULLIFY(grid%apr_st)
ENDIF
IF ( ASSOCIATED( grid%apr_as ) ) THEN 
  DEALLOCATE(grid%apr_as,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%apr_as. ')
 endif
  NULLIFY(grid%apr_as)
ENDIF
IF ( ASSOCIATED( grid%apr_capma ) ) THEN 
  DEALLOCATE(grid%apr_capma,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%apr_capma. ')
 endif
  NULLIFY(grid%apr_capma)
ENDIF
IF ( ASSOCIATED( grid%apr_capme ) ) THEN 
  DEALLOCATE(grid%apr_capme,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%apr_capme. ')
 endif
  NULLIFY(grid%apr_capme)
ENDIF
IF ( ASSOCIATED( grid%apr_capmi ) ) THEN 
  DEALLOCATE(grid%apr_capmi,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%apr_capmi. ')
 endif
  NULLIFY(grid%apr_capmi)
ENDIF
IF ( ASSOCIATED( grid%xf_ens ) ) THEN 
  DEALLOCATE(grid%xf_ens,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%xf_ens. ')
 endif
  NULLIFY(grid%xf_ens)
ENDIF
IF ( ASSOCIATED( grid%pr_ens ) ) THEN 
  DEALLOCATE(grid%pr_ens,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%pr_ens. ')
 endif
  NULLIFY(grid%pr_ens)
ENDIF
IF ( ASSOCIATED( grid%rthften ) ) THEN 
  DEALLOCATE(grid%rthften,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%rthften. ')
 endif
  NULLIFY(grid%rthften)
ENDIF
IF ( ASSOCIATED( grid%rqvften ) ) THEN 
  DEALLOCATE(grid%rqvften,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%rqvften. ')
 endif
  NULLIFY(grid%rqvften)
ENDIF
IF ( ASSOCIATED( grid%snowh ) ) THEN 
  DEALLOCATE(grid%snowh,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%snowh. ')
 endif
  NULLIFY(grid%snowh)
ENDIF
IF ( ASSOCIATED( grid%rhosn ) ) THEN 
  DEALLOCATE(grid%rhosn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%rhosn. ')
 endif
  NULLIFY(grid%rhosn)
ENDIF
IF ( ASSOCIATED( grid%smfr3d ) ) THEN 
  DEALLOCATE(grid%smfr3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%smfr3d. ')
 endif
  NULLIFY(grid%smfr3d)
ENDIF
IF ( ASSOCIATED( grid%keepfr3dflag ) ) THEN 
  DEALLOCATE(grid%keepfr3dflag,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%keepfr3dflag. ')
 endif
  NULLIFY(grid%keepfr3dflag)
ENDIF
IF ( ASSOCIATED( grid%mp_restart_state ) ) THEN 
  DEALLOCATE(grid%mp_restart_state,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%mp_restart_state. ')
 endif
  NULLIFY(grid%mp_restart_state)
ENDIF
IF ( ASSOCIATED( grid%tbpvs_state ) ) THEN 
  DEALLOCATE(grid%tbpvs_state,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%tbpvs_state. ')
 endif
  NULLIFY(grid%tbpvs_state)
ENDIF
IF ( ASSOCIATED( grid%tbpvs0_state ) ) THEN 
  DEALLOCATE(grid%tbpvs0_state,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%tbpvs0_state. ')
 endif
  NULLIFY(grid%tbpvs0_state)
ENDIF
IF ( ASSOCIATED( grid%lu_state ) ) THEN 
  DEALLOCATE(grid%lu_state,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal ( &
'frame/module_domain.f: Failed to dallocate grid%lu_state. ')
 endif
  NULLIFY(grid%lu_state)
ENDIF
!ENDOFREGISTRYGENERATEDINCLUDE
