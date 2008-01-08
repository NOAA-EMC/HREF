!WRF:MEDIATION_LAYER:ADT_BARRIER
!

SUBROUTINE start_domain ( grid , allowed_to_read )

   USE module_domain
   USE module_configure

   IMPLICIT NONE

   !  Input Arguments.
   TYPE (domain)          :: grid
   LOGICAL, INTENT(IN)    :: allowed_to_read
   !  Local data.
   INTEGER                :: dyn_opt
   INTEGER :: idum1, idum2




   CALL nl_get_dyn_opt( 1, dyn_opt )
  
   CALL set_scalar_indices_from_config ( head_grid%id , idum1, idum2 )

   IF (      dyn_opt .eq. DYN_NODYN ) THEN

   ELSE IF (      dyn_opt .eq. DYN_NMM ) THEN
     CALL start_domain_nmm( grid, allowed_to_read   &
!
!STARTOFREGISTRYGENERATEDINCLUDE 'inc/nmm_actual_args.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
,grid%lu_index,grid%lu_mask,grid%nmm_p_gc,grid%vegcat,grid%soilcat,grid%input_soil_cat,grid%nmm_tsk_gc,grid%xice_gc, &
grid%nmm_ght_gc,grid%nmm_rh_gc,grid%nmm_v_gc,grid%nmm_u_gc,grid%nmm_t_gc,grid%nmm_rwmr_gc,grid%nmm_snmr_gc,grid%nmm_clwmr_gc, &
grid%nmm_cice_gc,grid%nmm_rimef_gc,grid%snoalb,grid%nmm_greenfrac_gc,grid%nmm_albedo12m_gc,grid%soilcbot_gc,grid%soilctop_gc, &
grid%nmm_tmn_gc,grid%nmm_htv_gc,grid%nmm_ht_gc,grid%landusef_gc,grid%nmm_vlon_gc,grid%nmm_vlat_gc,grid%nmm_hlon_gc, &
grid%nmm_hlat_gc,grid%nmm_hbm2,grid%nmm_hbm3,grid%nmm_vbm2,grid%nmm_vbm3,grid%nmm_sm,grid%nmm_sice,grid%nmm_pd,grid%nmm_pd_b, &
grid%nmm_pd_bt,grid%nmm_fis,grid%nmm_res,grid%nmm_t,grid%nmm_t_b,grid%nmm_t_bt,grid%nmm_q,grid%nmm_q_b,grid%nmm_q_bt,grid%nmm_u, &
grid%nmm_u_b,grid%nmm_u_bt,grid%nmm_v,grid%nmm_v_b,grid%nmm_v_bt,grid%nmm_told,grid%nmm_uold,grid%nmm_vold,grid%nmm_dx_nmm, &
grid%nmm_wpdar,grid%nmm_cpgfu,grid%nmm_curv,grid%nmm_fcp,grid%nmm_fdiv,grid%nmm_f,grid%nmm_fad,grid%nmm_ddmpu,grid%nmm_ddmpv, &
grid%nmm_deta,grid%nmm_rdeta,grid%nmm_aeta,grid%nmm_f4q2,grid%nmm_etax,grid%nmm_dfl,grid%nmm_deta1,grid%nmm_aeta1,grid%nmm_eta1, &
grid%nmm_deta2,grid%nmm_aeta2,grid%nmm_eta2,grid%nmm_em,grid%nmm_emt,grid%nmm_adt,grid%nmm_adu,grid%nmm_adv,grid%nmm_em_loc, &
grid%nmm_emt_loc,grid%nmm_pdsl,grid%nmm_pdslo,grid%nmm_psdt,grid%nmm_div,grid%nmm_few,grid%nmm_fne,grid%nmm_fns,grid%nmm_fse, &
grid%nmm_omgalf,grid%nmm_petdt,grid%nmm_rtop,grid%nmm_pblh,grid%nmm_lpbl,grid%nmm_ustar,grid%nmm_z0,grid%nmm_z0base, &
grid%nmm_ths,grid%nmm_mavail,grid%nmm_qsh,grid%nmm_twbs,grid%nmm_qwbs,grid%nmm_prec,grid%nmm_aprec,grid%nmm_acprec, &
grid%nmm_cuprec,grid%nmm_lspa,grid%nmm_ddata,grid%nmm_accliq,grid%nmm_sno,grid%nmm_si,grid%nmm_cldefi,grid%nmm_deep,grid%nmm_rf, &
grid%nmm_th10,grid%nmm_q10,grid%nmm_pshltr,grid%nmm_tshltr,grid%nmm_qshltr,grid%nmm_q2,grid%nmm_q2_b,grid%nmm_q2_bt, &
grid%nmm_t_adj,grid%nmm_t_old,grid%nmm_zero_3d,grid%nmm_w0avg,grid%nmm_akhs_out,grid%nmm_akms_out,grid%nmm_albase, &
grid%nmm_albedo,grid%nmm_cnvbot,grid%nmm_cnvtop,grid%nmm_czen,grid%nmm_czmean,grid%nmm_epsr,grid%nmm_gffc,grid%nmm_glat, &
grid%nmm_glon,grid%nmm_nmm_tsk,grid%nmm_hdac,grid%nmm_hdacv,grid%nmm_mxsnal,grid%nmm_radin,grid%nmm_radot,grid%nmm_sigt4, &
grid%nmm_tg,grid%nmm_dfrlg,grid%nmm_lvl,grid%nmm_cwm,grid%nmm_cwm_b,grid%nmm_cwm_bt,grid%nmm_f_ice,grid%nmm_f_rain, &
grid%nmm_f_rimef,grid%nmm_cldfra,grid%nmm_sr,grid%nmm_cfrach,grid%nmm_cfracl,grid%nmm_cfracm,grid%nmm_islope,grid%nmm_dzsoil, &
grid%nmm_rtdpth,grid%nmm_sldpth,grid%nmm_cmc,grid%nmm_grnflx,grid%nmm_pctsno,grid%nmm_soiltb,grid%nmm_vegfrc,grid%nmm_shdmin, &
grid%nmm_shdmax,grid%nmm_sh2o,grid%nmm_smc,grid%nmm_stc,grid%nmm_dwdtmn,grid%nmm_dwdtmx,grid%nmm_dwdt,grid%nmm_pdwdt, &
grid%nmm_pint,grid%nmm_w,grid%nmm_z,grid%nmm_acfrcv,grid%nmm_acfrst,grid%nmm_ssroff,grid%nmm_bgroff,grid%nmm_rlwin, &
grid%nmm_rlwout,grid%nmm_rlwtoa,grid%nmm_alwin,grid%nmm_alwout,grid%nmm_alwtoa,grid%nmm_rswin,grid%nmm_rswinc,grid%nmm_rswout, &
grid%nmm_rswtoa,grid%nmm_aswin,grid%nmm_aswout,grid%nmm_aswtoa,grid%nmm_sfcshx,grid%nmm_sfclhx,grid%nmm_subshx,grid%nmm_snopcx, &
grid%nmm_sfcuvx,grid%nmm_potevp,grid%nmm_potflx,grid%nmm_tlmin,grid%nmm_tlmax,grid%nmm_rlwtt,grid%nmm_rswtt,grid%nmm_tcucn, &
grid%nmm_train,grid%nmm_ncfrcv,grid%nmm_ncfrst,grid%nmm_ihe,grid%nmm_ihw,grid%nmm_ive,grid%nmm_ivw,grid%nmm_irad,grid%nmm_iheg, &
grid%nmm_ihwg,grid%nmm_iveg,grid%nmm_ivwg,grid%nmm_iradg,grid%nmm_n_iup_h,grid%nmm_n_iup_v,grid%nmm_n_iup_adh, &
grid%nmm_n_iup_adv,grid%nmm_iup_h,grid%nmm_iup_v,grid%nmm_iup_adh,grid%nmm_iup_adv,grid%imask_nostag,grid%imask_xstag, &
grid%imask_ystag,grid%imask_xystag,grid%sm000007,grid%sm007028,grid%sm028100,grid%sm100255,grid%st000007,grid%st007028, &
grid%st028100,grid%st100255,grid%sm000010,grid%sm010040,grid%sm040100,grid%sm100200,grid%sm010200,grid%soilm000,grid%soilm005, &
grid%soilm020,grid%soilm040,grid%soilm160,grid%soilm300,grid%sw000010,grid%sw010040,grid%sw040100,grid%sw100200,grid%sw010200, &
grid%soilw000,grid%soilw005,grid%soilw020,grid%soilw040,grid%soilw160,grid%soilw300,grid%st000010,grid%st010040,grid%st040100, &
grid%st100200,grid%st010200,grid%soilt000,grid%soilt005,grid%soilt020,grid%soilt040,grid%soilt160,grid%soilt300,grid%landmask, &
grid%topostdv,grid%toposlpx,grid%toposlpy,grid%greenmax,grid%greenmin,grid%albedomx,grid%slopecat,grid%toposoil,grid%landusef, &
grid%soilctop,grid%soilcbot,grid%moist,grid%scalar,grid%scalar_b,grid%scalar_bt,grid%chem,grid%smois,grid%tslb,grid%gsw, &
grid%xlat,grid%xlong,grid%xland,grid%raincv,grid%psfc,grid%th2,grid%t2,grid%u10,grid%v10,grid%xice,grid%smstav,grid%smstot, &
grid%sfcrunoff,grid%udrunoff,grid%ivgtyp,grid%isltyp,grid%vegfra,grid%sfcevp,grid%grdflx,grid%albbck,grid%sfcexc,grid%acsnow, &
grid%acsnom,grid%rmol,grid%snow,grid%canwat,grid%sst,grid%weasd,grid%znt,grid%mol,grid%tke_myj,grid%el_myj,grid%exch_h, &
grid%thz0,grid%qz0,grid%uz0,grid%vz0,grid%flhc,grid%flqc,grid%qsg,grid%qvg,grid%qcg,grid%soilt1,grid%tsnav,grid%nmm_psfc_out, &
grid%uz0h,grid%vz0h,grid%dudt,grid%dvdt,grid%qsfc,grid%akhs,grid%akms,grid%htop,grid%hbot,grid%htopr,grid%hbotr,grid%htopd, &
grid%hbotd,grid%htops,grid%hbots,grid%cuppt,grid%cprate,grid%f_ice_phy,grid%f_rain_phy,grid%f_rimef_phy,grid%mass_flux, &
grid%apr_gr,grid%apr_w,grid%apr_mc,grid%apr_st,grid%apr_as,grid%apr_capma,grid%apr_capme,grid%apr_capmi,grid%xf_ens,grid%pr_ens, &
grid%rthften,grid%rqvften,grid%snowh,grid%rhosn,grid%smfr3d,grid%keepfr3dflag,grid%mp_restart_state,grid%tbpvs_state, &
grid%tbpvs0_state,grid%lu_state &
!ENDOFREGISTRYGENERATEDINCLUDE
!
                         )

!### 4a. edit share/start_domain.F to call domain inits for core if any


   ELSE

     WRITE(0,*)' start_domain: unknown or unimplemented dyn_opt = ',dyn_opt
     CALL wrf_error_fatal3 ( "start_domain.b" , 68 ,  ' start_domain: unknown or unimplemented dyn_opt ' ) 
   ENDIF


END SUBROUTINE start_domain

