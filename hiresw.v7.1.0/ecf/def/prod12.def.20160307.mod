# 4.0.3
extern /hourly/ecmwf/jecmwf_12
extern /prod06/gdas/enkf
extern /prod06/gdas/enkf/forecast
extern /prod06/gfs/mos/forecast/jgfsmos_tstms_fcst
extern /prod06/gfs/mos/prdgen/jgfsmos_station_prdgen
extern /prod06/gfs/sminit_guam/jgfs_sminit_guam
extern /prod06/nam/mos/jnam_mos
extern /prod06/nam/sminit_ak/jnam_sminit_f09
extern /prod06/nam/sminit_hi/jnam_sminit_f09
extern /prod06/nam/sminit_pr/jnam_sminit_f09
extern /prod06/ndas12/time_minus_03hr/jndas_forecast
extern /prod06/rap/rap_11/prdgen/jrap_prdgen_f01
extern /prod06/rtofs/jrtofs_global_forecast_step1
extern /prod06/sref09/enspost/jsref_enspost
extern /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready
extern /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready
extern /prod06/sref09/enspost/jsref_enspost:ensgrd_243_ready
extern /prod06/sref09/jsref_gefs2sref
extern /prod18/cmcens/jcmc_ens_post_12
suite prod12
  repeat day 1
  edit ECF_TRIES '1'
  edit ECF_JOB_CMD '/ecf/ecfutils/unixsubmit %ECF_JOB% %ECF_JOBOUT% ibmsp'
  edit ECF_KILL_CMD '/ecf/ecfutils/unixkill %ECF_NAME% %ECF_JOBOUT%'
  edit CYC '12'
  edit ENVIR 'prod'
  edit PROJ 'OPS'
  edit E 'j'
  edit QUEUE 'prod'
  edit PROJENVIR 'OPS'
  family prod_filemanager
    edit W2RUN 'prod'
    edit PROJ 'HOURLY-OPS'
    task jprod_setup
      time 11:31
    task jdate_setup_xc40
      edit PROJ 'HOURLY'
      edit ECF_JOB_CMD '/ecf/ecfutils/unixsubmit %ECF_JOB% %ECF_JOBOUT% xc40p'
      time 11:31
    task jrzdm_cleanup
      trigger jprod_setup eq complete
    task jnwprod_bkup
      trigger jprod_setup eq complete
      time 12:00
    task jnwprod2_backup
      trigger jnwprod_bkup == complete
      edit PROJ 'HOURLY'
      edit QUEUE 'prod2'
    task jnwprod_backup_xc40
      trigger jdate_setup_xc40 == complete
      edit PROJ 'HOURLY'
      edit ECF_JOB_CMD '/ecf/ecfutils/unixsubmit %ECF_JOB% %ECF_JOBOUT% xc40p'
      time 12:00
    task jprod_cleanup
      trigger jprod_setup eq complete
  endfamily
  family arkv12
    family arkv_ncar
      edit PROJ 'VERF-OPS'
      task jarkv_ncar_12
        trigger /prod12/prod_filemanager/jprod_setup == complete
    endfamily
    family run_history
      task jdecode_bkup
        trigger /prod12/prod_filemanager/jprod_setup eq complete
        edit ECF_JOB_CMD '/ecf/ecfutils/unixsubmit %ECF_JOB% %ECF_JOBOUT% ibmsp'
        time 12:00
    endfamily
  endfamily
  family blend
    edit ECF_FILES '/ecf/ecfnets/scripts'
    edit COM 'com2'
    edit QUEUE 'prod2'
    edit QUEUESERV 'prod2_serv'
    edit PROJ 'BLEND-OPS'
    task jblend_prep
      time 21:55
    task jblend_update
      trigger jblend_prep == complete
    task jblend_bcfcst
      trigger jblend_update == complete
    task jblend_maewfcst
      trigger jblend_bcfcst == complete
    task jblend_prdgen
      trigger jblend_maewfcst == complete
    task jblend_gempak
      trigger jblend_prdgen == complete
  endfamily
  family real12
    task jthanks_12
      time 12:45 14:00 00:15
  endfamily
  family href
    edit PROJ 'HRW-OPS'
    edit QUEUE 'prod2'
    edit COM 'com2'
    task jhref_ensprod
      trigger /prod12/nam/nest_conus/prdgen/jnam_prdgen_f36 == complete and ../hiresw/nmmb/conus/jhiresw_prdgen == complete and ../hiresw/arw/conus/jhiresw_prdgen == complete
    task jhref_gempak
      trigger jhref_ensprod == complete
  endfamily
  family ekdmos
    edit ECF_FILES '/ecf/ecfnets/scripts/ekdmos'
    edit COM 'com2'
    edit PROJ 'MDL-OPS'
    edit QUEUE 'prod2'
    edit QUEUESERV 'prod2_serv'
    task jekdmos_warning
      defstatus complete
    task jekdmos_stnfcst
      trigger /prod12/gefs/prdgen_low == complete and /prod12/gefs/post_processing/jgefs_prdgen_gfs == complete
      time 20:05
    task jekdmos_stnpst
      trigger jekdmos_stnfcst == complete
    family conus
      task jekdmos_gridded
        trigger ../jekdmos_stnpst == complete
      task jekdmos_catgb2
        trigger jekdmos_gridded == complete
    endfamily
    family ak
      task jekdmos_akprep
        trigger ../jekdmos_stnfcst == complete
      task jekdmos_akgridded
        trigger ../jekdmos_stnpst == complete and jekdmos_akprep == complete
      task jekdmos_catakgb2
        trigger jekdmos_akgridded == complete
    endfamily
    task jekdmos_gempak
      trigger conus == complete and ak == complete
  endfamily
  family gefs_legacy12
    edit PROJ 'GEN-OPS'
    edit QUEUESERV 'prod_serv2'
    family init
      edit ECF_FILES '/ecf/ecfnets/scripts/gefs_legacy/init'
      task jgefs_init_separate
        trigger /prod12/gfs/prdgen/jgfs_pgrb2_f00 eq complete
        edit ECF_PASS 'FREE'
        time 19:10
      task jgefs_init_et
        trigger /prod12/gfs/prdgen/jgfs_pgrb2_f00 eq complete and /prod12/gefs_legacy12/init/jgefs_init_separate eq complete
        edit ECF_PASS 'FREE'
      task jgefs_init_combine
        trigger /prod12/gfs/prdgen/jgfs_pgrb2_f00 eq complete and /prod12/gefs_legacy12/init/jgefs_init_separate eq complete and /prod12/gefs_legacy12/init/jgefs_init_et eq complete
        edit ECF_PASS 'FREE'
    endfamily
    family gefs12
      task jgefs_forecast_high_12
        trigger /prod12/gefs_legacy12/init eq complete and ( /prod12/gfs/forecast/jgfs_forecast_low == active or /prod12/gfs/forecast/jgfs_forecast_low == complete )
        edit ECF_PASS 'FREE'
      task jgefs_restart_copy_high_12
        trigger /prod12/gefs_legacy12/gefs12/jgefs_forecast_high_12 eq active or /prod12/gefs_legacy12/gefs12/jgefs_forecast_high_12 eq complete
        edit ECF_PASS 'FREE'
      family post_high
        task jgefs_c00_post_high_12
          trigger /prod12/gefs_legacy12/gefs12/jgefs_forecast_high_12 eq active
          edit ECF_PASS 'FREE'
          event 1 post_126_ready
        task jgefs_p01_post_high_12
          trigger /prod12/gefs_legacy12/gefs12/jgefs_forecast_high_12 eq active
          edit ECF_PASS 'FREE'
          event 1 post_126_ready
        task jgefs_p02_post_high_12
          trigger /prod12/gefs_legacy12/gefs12/jgefs_forecast_high_12 eq active
          edit ECF_PASS 'FREE'
          event 1 post_126_ready
        task jgefs_p03_post_high_12
          trigger /prod12/gefs_legacy12/gefs12/jgefs_forecast_high_12 eq active
          edit ECF_PASS 'FREE'
          event 1 post_126_ready
        task jgefs_p04_post_high_12
          trigger /prod12/gefs_legacy12/gefs12/jgefs_forecast_high_12 eq active
          edit ECF_PASS 'FREE'
          event 1 post_126_ready
        task jgefs_p05_post_high_12
          trigger /prod12/gefs_legacy12/gefs12/jgefs_forecast_high_12 eq active
          edit ECF_PASS 'FREE'
          event 1 post_126_ready
        task jgefs_p06_post_high_12
          trigger /prod12/gefs_legacy12/gefs12/jgefs_forecast_high_12 eq active
          edit ECF_PASS 'FREE'
          event 1 post_126_ready
        task jgefs_p07_post_high_12
          trigger /prod12/gefs_legacy12/gefs12/jgefs_forecast_high_12 eq active
          edit ECF_PASS 'FREE'
          event 1 post_126_ready
        task jgefs_p08_post_high_12
          trigger /prod12/gefs_legacy12/gefs12/jgefs_forecast_high_12 eq active
          edit ECF_PASS 'FREE'
          event 1 post_126_ready
        task jgefs_p09_post_high_12
          trigger /prod12/gefs_legacy12/gefs12/jgefs_forecast_high_12 eq active
          edit ECF_PASS 'FREE'
          event 1 post_126_ready
        task jgefs_p10_post_high_12
          trigger /prod12/gefs_legacy12/gefs12/jgefs_forecast_high_12 eq active
          edit ECF_PASS 'FREE'
          event 1 post_126_ready
        task jgefs_p11_post_high_12
          trigger /prod12/gefs_legacy12/gefs12/jgefs_forecast_high_12 eq active
          edit ECF_PASS 'FREE'
          event 1 post_126_ready
        task jgefs_p12_post_high_12
          trigger /prod12/gefs_legacy12/gefs12/jgefs_forecast_high_12 eq active
          edit ECF_PASS 'FREE'
          event 1 post_126_ready
        task jgefs_p13_post_high_12
          trigger /prod12/gefs_legacy12/gefs12/jgefs_forecast_high_12 eq active
          edit ECF_PASS 'FREE'
          event 1 post_126_ready
        task jgefs_p14_post_high_12
          trigger /prod12/gefs_legacy12/gefs12/jgefs_forecast_high_12 eq active
          edit ECF_PASS 'FREE'
          event 1 post_126_ready
        task jgefs_p15_post_high_12
          trigger /prod12/gefs_legacy12/gefs12/jgefs_forecast_high_12 eq active
          edit ECF_PASS 'FREE'
          event 1 post_126_ready
        task jgefs_p16_post_high_12
          trigger /prod12/gefs_legacy12/gefs12/jgefs_forecast_high_12 eq active
          edit ECF_PASS 'FREE'
          event 1 post_126_ready
        task jgefs_p17_post_high_12
          trigger /prod12/gefs_legacy12/gefs12/jgefs_forecast_high_12 eq active
          edit ECF_PASS 'FREE'
          event 1 post_126_ready
        task jgefs_p18_post_high_12
          trigger /prod12/gefs_legacy12/gefs12/jgefs_forecast_high_12 eq active
          edit ECF_PASS 'FREE'
          event 1 post_126_ready
        task jgefs_p19_post_high_12
          trigger /prod12/gefs_legacy12/gefs12/jgefs_forecast_high_12 eq active
          edit ECF_PASS 'FREE'
          event 1 post_126_ready
        task jgefs_p20_post_high_12
          trigger /prod12/gefs_legacy12/gefs12/jgefs_forecast_high_12 eq active
          edit ECF_PASS 'FREE'
          event 1 post_126_ready
      endfamily
      family prdgen_high
        task jgefs_c00_prdgen_high_12
          trigger /prod12/gefs_legacy12/gefs12/post_high/jgefs_c00_post_high_12 eq active or /prod12/gefs_legacy12/gefs12/post_high/jgefs_c00_post_high_12 eq complete
          edit ECF_PASS 'FREE'
        task jgefs_p01_prdgen_high_12
          trigger /prod12/gefs_legacy12/gefs12/post_high/jgefs_p01_post_high_12 eq active or /prod12/gefs_legacy12/gefs12/post_high/jgefs_p01_post_high_12 eq complete
          edit ECF_PASS 'FREE'
        task jgefs_p02_prdgen_high_12
          trigger /prod12/gefs_legacy12/gefs12/post_high/jgefs_p02_post_high_12 eq active or /prod12/gefs_legacy12/gefs12/post_high/jgefs_p02_post_high_12 eq complete
          edit ECF_PASS 'FREE'
        task jgefs_p03_prdgen_high_12
          trigger /prod12/gefs_legacy12/gefs12/post_high/jgefs_p03_post_high_12 eq active or /prod12/gefs_legacy12/gefs12/post_high/jgefs_p03_post_high_12 eq complete
          edit ECF_PASS 'FREE'
        task jgefs_p04_prdgen_high_12
          trigger /prod12/gefs_legacy12/gefs12/post_high/jgefs_p04_post_high_12 eq active or /prod12/gefs_legacy12/gefs12/post_high/jgefs_p04_post_high_12 eq complete
          edit ECF_PASS 'FREE'
        task jgefs_p05_prdgen_high_12
          trigger /prod12/gefs_legacy12/gefs12/post_high/jgefs_p05_post_high_12 eq active or /prod12/gefs_legacy12/gefs12/post_high/jgefs_p05_post_high_12 eq complete
          edit ECF_PASS 'FREE'
        task jgefs_p06_prdgen_high_12
          trigger /prod12/gefs_legacy12/gefs12/post_high/jgefs_p06_post_high_12 eq active or /prod12/gefs_legacy12/gefs12/post_high/jgefs_p06_post_high_12 eq complete
          edit ECF_PASS 'FREE'
        task jgefs_p07_prdgen_high_12
          trigger /prod12/gefs_legacy12/gefs12/post_high/jgefs_p07_post_high_12 eq active or /prod12/gefs_legacy12/gefs12/post_high/jgefs_p07_post_high_12 eq complete
          edit ECF_PASS 'FREE'
        task jgefs_p08_prdgen_high_12
          trigger /prod12/gefs_legacy12/gefs12/post_high/jgefs_p08_post_high_12 eq active or /prod12/gefs_legacy12/gefs12/post_high/jgefs_p08_post_high_12 eq complete
          edit ECF_PASS 'FREE'
        task jgefs_p09_prdgen_high_12
          trigger /prod12/gefs_legacy12/gefs12/post_high/jgefs_p09_post_high_12 eq active or /prod12/gefs_legacy12/gefs12/post_high/jgefs_p09_post_high_12 eq complete
          edit ECF_PASS 'FREE'
        task jgefs_p10_prdgen_high_12
          trigger /prod12/gefs_legacy12/gefs12/post_high/jgefs_p10_post_high_12 eq active or /prod12/gefs_legacy12/gefs12/post_high/jgefs_p10_post_high_12 eq complete
          edit ECF_PASS 'FREE'
        task jgefs_p11_prdgen_high_12
          trigger /prod12/gefs_legacy12/gefs12/post_high/jgefs_p11_post_high_12 eq active or /prod12/gefs_legacy12/gefs12/post_high/jgefs_p11_post_high_12 eq complete
          edit ECF_PASS 'FREE'
        task jgefs_p12_prdgen_high_12
          trigger /prod12/gefs_legacy12/gefs12/post_high/jgefs_p12_post_high_12 eq active or /prod12/gefs_legacy12/gefs12/post_high/jgefs_p12_post_high_12 eq complete
          edit ECF_PASS 'FREE'
        task jgefs_p13_prdgen_high_12
          trigger /prod12/gefs_legacy12/gefs12/post_high/jgefs_p13_post_high_12 eq active or /prod12/gefs_legacy12/gefs12/post_high/jgefs_p13_post_high_12 eq complete
          edit ECF_PASS 'FREE'
        task jgefs_p14_prdgen_high_12
          trigger /prod12/gefs_legacy12/gefs12/post_high/jgefs_p14_post_high_12 eq active or /prod12/gefs_legacy12/gefs12/post_high/jgefs_p14_post_high_12 eq complete
          edit ECF_PASS 'FREE'
        task jgefs_p15_prdgen_high_12
          trigger /prod12/gefs_legacy12/gefs12/post_high/jgefs_p15_post_high_12 eq active or /prod12/gefs_legacy12/gefs12/post_high/jgefs_p15_post_high_12 eq complete
          edit ECF_PASS 'FREE'
        task jgefs_p16_prdgen_high_12
          trigger /prod12/gefs_legacy12/gefs12/post_high/jgefs_p16_post_high_12 eq active or /prod12/gefs_legacy12/gefs12/post_high/jgefs_p16_post_high_12 eq complete
          edit ECF_PASS 'FREE'
        task jgefs_p17_prdgen_high_12
          trigger /prod12/gefs_legacy12/gefs12/post_high/jgefs_p17_post_high_12 eq active or /prod12/gefs_legacy12/gefs12/post_high/jgefs_p17_post_high_12 eq complete
          edit ECF_PASS 'FREE'
        task jgefs_p18_prdgen_high_12
          trigger /prod12/gefs_legacy12/gefs12/post_high/jgefs_p18_post_high_12 eq active or /prod12/gefs_legacy12/gefs12/post_high/jgefs_p18_post_high_12 eq complete
          edit ECF_PASS 'FREE'
        task jgefs_p19_prdgen_high_12
          trigger /prod12/gefs_legacy12/gefs12/post_high/jgefs_p19_post_high_12 eq active or /prod12/gefs_legacy12/gefs12/post_high/jgefs_p19_post_high_12 eq complete
          edit ECF_PASS 'FREE'
        task jgefs_p20_prdgen_high_12
          trigger /prod12/gefs_legacy12/gefs12/post_high/jgefs_p20_post_high_12 eq active or /prod12/gefs_legacy12/gefs12/post_high/jgefs_p20_post_high_12 eq complete
          edit ECF_PASS 'FREE'
      endfamily
      task jgefs_post_track_12
        trigger /prod12/gefs_legacy12/gefs12/prdgen_high eq complete
        edit ECF_PASS 'FREE'
    endfamily
    family gefs12_cf18
      task jgefs_forecast_cf18_12
        trigger /prod12/gefs_legacy12/init eq complete and /prod12/gefs_legacy12/gefs12/jgefs_forecast_high_12 eq complete
        edit ECF_PASS 'FREE'
      family post
        task jgefs_p01_post_cf18_12
          trigger /prod12/gefs_legacy12/gefs12_cf18/jgefs_forecast_cf18_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p02_post_cf18_12
          trigger /prod12/gefs_legacy12/gefs12_cf18/jgefs_forecast_cf18_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p03_post_cf18_12
          trigger /prod12/gefs_legacy12/gefs12_cf18/jgefs_forecast_cf18_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p04_post_cf18_12
          trigger /prod12/gefs_legacy12/gefs12_cf18/jgefs_forecast_cf18_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p05_post_cf18_12
          trigger /prod12/gefs_legacy12/gefs12_cf18/jgefs_forecast_cf18_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p06_post_cf18_12
          trigger /prod12/gefs_legacy12/gefs12_cf18/jgefs_forecast_cf18_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p07_post_cf18_12
          trigger /prod12/gefs_legacy12/gefs12_cf18/jgefs_forecast_cf18_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p08_post_cf18_12
          trigger /prod12/gefs_legacy12/gefs12_cf18/jgefs_forecast_cf18_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p09_post_cf18_12
          trigger /prod12/gefs_legacy12/gefs12_cf18/jgefs_forecast_cf18_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p10_post_cf18_12
          trigger /prod12/gefs_legacy12/gefs12_cf18/jgefs_forecast_cf18_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p11_post_cf18_12
          trigger /prod12/gefs_legacy12/gefs12_cf18/jgefs_forecast_cf18_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p12_post_cf18_12
          trigger /prod12/gefs_legacy12/gefs12_cf18/jgefs_forecast_cf18_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p13_post_cf18_12
          trigger /prod12/gefs_legacy12/gefs12_cf18/jgefs_forecast_cf18_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p14_post_cf18_12
          trigger /prod12/gefs_legacy12/gefs12_cf18/jgefs_forecast_cf18_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p15_post_cf18_12
          trigger /prod12/gefs_legacy12/gefs12_cf18/jgefs_forecast_cf18_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p16_post_cf18_12
          trigger /prod12/gefs_legacy12/gefs12_cf18/jgefs_forecast_cf18_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p17_post_cf18_12
          trigger /prod12/gefs_legacy12/gefs12_cf18/jgefs_forecast_cf18_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p18_post_cf18_12
          trigger /prod12/gefs_legacy12/gefs12_cf18/jgefs_forecast_cf18_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p19_post_cf18_12
          trigger /prod12/gefs_legacy12/gefs12_cf18/jgefs_forecast_cf18_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p20_post_cf18_12
          trigger /prod12/gefs_legacy12/gefs12_cf18/jgefs_forecast_cf18_12 eq active
          edit ECF_PASS 'FREE'
      endfamily
      family prdgen
        task jgefs_p01_prdgen_cf18_12
          trigger /prod12/gefs_legacy12/gefs12_cf18/jgefs_forecast_cf18_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p02_prdgen_cf18_12
          trigger /prod12/gefs_legacy12/gefs12_cf18/jgefs_forecast_cf18_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p03_prdgen_cf18_12
          trigger /prod12/gefs_legacy12/gefs12_cf18/jgefs_forecast_cf18_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p04_prdgen_cf18_12
          trigger /prod12/gefs_legacy12/gefs12_cf18/jgefs_forecast_cf18_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p05_prdgen_cf18_12
          trigger /prod12/gefs_legacy12/gefs12_cf18/jgefs_forecast_cf18_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p06_prdgen_cf18_12
          trigger /prod12/gefs_legacy12/gefs12_cf18/jgefs_forecast_cf18_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p07_prdgen_cf18_12
          trigger /prod12/gefs_legacy12/gefs12_cf18/jgefs_forecast_cf18_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p08_prdgen_cf18_12
          trigger /prod12/gefs_legacy12/gefs12_cf18/jgefs_forecast_cf18_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p09_prdgen_cf18_12
          trigger /prod12/gefs_legacy12/gefs12_cf18/jgefs_forecast_cf18_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p10_prdgen_cf18_12
          trigger /prod12/gefs_legacy12/gefs12_cf18/jgefs_forecast_cf18_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p11_prdgen_cf18_12
          trigger /prod12/gefs_legacy12/gefs12_cf18/jgefs_forecast_cf18_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p12_prdgen_cf18_12
          trigger /prod12/gefs_legacy12/gefs12_cf18/jgefs_forecast_cf18_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p13_prdgen_cf18_12
          trigger /prod12/gefs_legacy12/gefs12_cf18/jgefs_forecast_cf18_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p14_prdgen_cf18_12
          trigger /prod12/gefs_legacy12/gefs12_cf18/jgefs_forecast_cf18_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p15_prdgen_cf18_12
          trigger /prod12/gefs_legacy12/gefs12_cf18/jgefs_forecast_cf18_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p16_prdgen_cf18_12
          trigger /prod12/gefs_legacy12/gefs12_cf18/jgefs_forecast_cf18_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p17_prdgen_cf18_12
          trigger /prod12/gefs_legacy12/gefs12_cf18/jgefs_forecast_cf18_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p18_prdgen_cf18_12
          trigger /prod12/gefs_legacy12/gefs12_cf18/jgefs_forecast_cf18_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p19_prdgen_cf18_12
          trigger /prod12/gefs_legacy12/gefs12_cf18/jgefs_forecast_cf18_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p20_prdgen_cf18_12
          trigger /prod12/gefs_legacy12/gefs12_cf18/jgefs_forecast_cf18_12 eq active
          edit ECF_PASS 'FREE'
      endfamily
      task jgefs_post_track_cf18_12
        trigger /prod12/gefs_legacy12/gefs12_cf18/prdgen eq complete
        edit ECF_PASS 'FREE'
    endfamily
    family gefs12_cf00
      task jgefs_forecast_cf00_12
        trigger /prod12/gefs_legacy12/init eq complete and /prod12/gefs_legacy12/gefs12_cf18/jgefs_forecast_cf18_12 eq complete
        edit ECF_PASS 'FREE'
      family post
        task jgefs_p01_post_cf00_12
          trigger /prod12/gefs_legacy12/gefs12_cf00/jgefs_forecast_cf00_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p02_post_cf00_12
          trigger /prod12/gefs_legacy12/gefs12_cf00/jgefs_forecast_cf00_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p03_post_cf00_12
          trigger /prod12/gefs_legacy12/gefs12_cf00/jgefs_forecast_cf00_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p04_post_cf00_12
          trigger /prod12/gefs_legacy12/gefs12_cf00/jgefs_forecast_cf00_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p05_post_cf00_12
          trigger /prod12/gefs_legacy12/gefs12_cf00/jgefs_forecast_cf00_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p06_post_cf00_12
          trigger /prod12/gefs_legacy12/gefs12_cf00/jgefs_forecast_cf00_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p07_post_cf00_12
          trigger /prod12/gefs_legacy12/gefs12_cf00/jgefs_forecast_cf00_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p08_post_cf00_12
          trigger /prod12/gefs_legacy12/gefs12_cf00/jgefs_forecast_cf00_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p09_post_cf00_12
          trigger /prod12/gefs_legacy12/gefs12_cf00/jgefs_forecast_cf00_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p10_post_cf00_12
          trigger /prod12/gefs_legacy12/gefs12_cf00/jgefs_forecast_cf00_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p11_post_cf00_12
          trigger /prod12/gefs_legacy12/gefs12_cf00/jgefs_forecast_cf00_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p12_post_cf00_12
          trigger /prod12/gefs_legacy12/gefs12_cf00/jgefs_forecast_cf00_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p13_post_cf00_12
          trigger /prod12/gefs_legacy12/gefs12_cf00/jgefs_forecast_cf00_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p14_post_cf00_12
          trigger /prod12/gefs_legacy12/gefs12_cf00/jgefs_forecast_cf00_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p15_post_cf00_12
          trigger /prod12/gefs_legacy12/gefs12_cf00/jgefs_forecast_cf00_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p16_post_cf00_12
          trigger /prod12/gefs_legacy12/gefs12_cf00/jgefs_forecast_cf00_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p17_post_cf00_12
          trigger /prod12/gefs_legacy12/gefs12_cf00/jgefs_forecast_cf00_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p18_post_cf00_12
          trigger /prod12/gefs_legacy12/gefs12_cf00/jgefs_forecast_cf00_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p19_post_cf00_12
          trigger /prod12/gefs_legacy12/gefs12_cf00/jgefs_forecast_cf00_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p20_post_cf00_12
          trigger /prod12/gefs_legacy12/gefs12_cf00/jgefs_forecast_cf00_12 eq active
          edit ECF_PASS 'FREE'
      endfamily
      family prdgen
        task jgefs_p01_prdgen_cf00_12
          trigger /prod12/gefs_legacy12/gefs12_cf00/jgefs_forecast_cf00_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p02_prdgen_cf00_12
          trigger /prod12/gefs_legacy12/gefs12_cf00/jgefs_forecast_cf00_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p03_prdgen_cf00_12
          trigger /prod12/gefs_legacy12/gefs12_cf00/jgefs_forecast_cf00_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p04_prdgen_cf00_12
          trigger /prod12/gefs_legacy12/gefs12_cf00/jgefs_forecast_cf00_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p05_prdgen_cf00_12
          trigger /prod12/gefs_legacy12/gefs12_cf00/jgefs_forecast_cf00_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p06_prdgen_cf00_12
          trigger /prod12/gefs_legacy12/gefs12_cf00/jgefs_forecast_cf00_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p07_prdgen_cf00_12
          trigger /prod12/gefs_legacy12/gefs12_cf00/jgefs_forecast_cf00_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p08_prdgen_cf00_12
          trigger /prod12/gefs_legacy12/gefs12_cf00/jgefs_forecast_cf00_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p09_prdgen_cf00_12
          trigger /prod12/gefs_legacy12/gefs12_cf00/jgefs_forecast_cf00_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p10_prdgen_cf00_12
          trigger /prod12/gefs_legacy12/gefs12_cf00/jgefs_forecast_cf00_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p11_prdgen_cf00_12
          trigger /prod12/gefs_legacy12/gefs12_cf00/jgefs_forecast_cf00_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p12_prdgen_cf00_12
          trigger /prod12/gefs_legacy12/gefs12_cf00/jgefs_forecast_cf00_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p13_prdgen_cf00_12
          trigger /prod12/gefs_legacy12/gefs12_cf00/jgefs_forecast_cf00_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p14_prdgen_cf00_12
          trigger /prod12/gefs_legacy12/gefs12_cf00/jgefs_forecast_cf00_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p15_prdgen_cf00_12
          trigger /prod12/gefs_legacy12/gefs12_cf00/jgefs_forecast_cf00_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p16_prdgen_cf00_12
          trigger /prod12/gefs_legacy12/gefs12_cf00/jgefs_forecast_cf00_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p17_prdgen_cf00_12
          trigger /prod12/gefs_legacy12/gefs12_cf00/jgefs_forecast_cf00_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p18_prdgen_cf00_12
          trigger /prod12/gefs_legacy12/gefs12_cf00/jgefs_forecast_cf00_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p19_prdgen_cf00_12
          trigger /prod12/gefs_legacy12/gefs12_cf00/jgefs_forecast_cf00_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p20_prdgen_cf00_12
          trigger /prod12/gefs_legacy12/gefs12_cf00/jgefs_forecast_cf00_12 eq active
          edit ECF_PASS 'FREE'
      endfamily
      task jgefs_post_track_cf00_12
        trigger /prod12/gefs_legacy12/gefs12_cf00/prdgen eq complete
        edit ECF_PASS 'FREE'
    endfamily
    family gefs12_cf06
      task jgefs_forecast_cf06_12
        trigger /prod12/gefs_legacy12/init eq complete and /prod12/gefs_legacy12/gefs12_cf00/jgefs_forecast_cf00_12 eq complete
        edit ECF_PASS 'FREE'
      family post
        task jgefs_p01_post_cf06_12
          trigger /prod12/gefs_legacy12/gefs12_cf06/jgefs_forecast_cf06_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p02_post_cf06_12
          trigger /prod12/gefs_legacy12/gefs12_cf06/jgefs_forecast_cf06_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p03_post_cf06_12
          trigger /prod12/gefs_legacy12/gefs12_cf06/jgefs_forecast_cf06_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p04_post_cf06_12
          trigger /prod12/gefs_legacy12/gefs12_cf06/jgefs_forecast_cf06_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p05_post_cf06_12
          trigger /prod12/gefs_legacy12/gefs12_cf06/jgefs_forecast_cf06_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p06_post_cf06_12
          trigger /prod12/gefs_legacy12/gefs12_cf06/jgefs_forecast_cf06_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p07_post_cf06_12
          trigger /prod12/gefs_legacy12/gefs12_cf06/jgefs_forecast_cf06_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p08_post_cf06_12
          trigger /prod12/gefs_legacy12/gefs12_cf06/jgefs_forecast_cf06_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p09_post_cf06_12
          trigger /prod12/gefs_legacy12/gefs12_cf06/jgefs_forecast_cf06_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p10_post_cf06_12
          trigger /prod12/gefs_legacy12/gefs12_cf06/jgefs_forecast_cf06_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p11_post_cf06_12
          trigger /prod12/gefs_legacy12/gefs12_cf06/jgefs_forecast_cf06_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p12_post_cf06_12
          trigger /prod12/gefs_legacy12/gefs12_cf06/jgefs_forecast_cf06_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p13_post_cf06_12
          trigger /prod12/gefs_legacy12/gefs12_cf06/jgefs_forecast_cf06_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p14_post_cf06_12
          trigger /prod12/gefs_legacy12/gefs12_cf06/jgefs_forecast_cf06_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p15_post_cf06_12
          trigger /prod12/gefs_legacy12/gefs12_cf06/jgefs_forecast_cf06_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p16_post_cf06_12
          trigger /prod12/gefs_legacy12/gefs12_cf06/jgefs_forecast_cf06_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p17_post_cf06_12
          trigger /prod12/gefs_legacy12/gefs12_cf06/jgefs_forecast_cf06_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p18_post_cf06_12
          trigger /prod12/gefs_legacy12/gefs12_cf06/jgefs_forecast_cf06_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p19_post_cf06_12
          trigger /prod12/gefs_legacy12/gefs12_cf06/jgefs_forecast_cf06_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p20_post_cf06_12
          trigger /prod12/gefs_legacy12/gefs12_cf06/jgefs_forecast_cf06_12 eq active
          edit ECF_PASS 'FREE'
      endfamily
      family prdgen
        task jgefs_p01_prdgen_cf06_12
          trigger /prod12/gefs_legacy12/gefs12_cf06/jgefs_forecast_cf06_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p02_prdgen_cf06_12
          trigger /prod12/gefs_legacy12/gefs12_cf06/jgefs_forecast_cf06_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p03_prdgen_cf06_12
          trigger /prod12/gefs_legacy12/gefs12_cf06/jgefs_forecast_cf06_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p04_prdgen_cf06_12
          trigger /prod12/gefs_legacy12/gefs12_cf06/jgefs_forecast_cf06_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p05_prdgen_cf06_12
          trigger /prod12/gefs_legacy12/gefs12_cf06/jgefs_forecast_cf06_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p06_prdgen_cf06_12
          trigger /prod12/gefs_legacy12/gefs12_cf06/jgefs_forecast_cf06_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p07_prdgen_cf06_12
          trigger /prod12/gefs_legacy12/gefs12_cf06/jgefs_forecast_cf06_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p08_prdgen_cf06_12
          trigger /prod12/gefs_legacy12/gefs12_cf06/jgefs_forecast_cf06_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p09_prdgen_cf06_12
          trigger /prod12/gefs_legacy12/gefs12_cf06/jgefs_forecast_cf06_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p10_prdgen_cf06_12
          trigger /prod12/gefs_legacy12/gefs12_cf06/jgefs_forecast_cf06_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p11_prdgen_cf06_12
          trigger /prod12/gefs_legacy12/gefs12_cf06/jgefs_forecast_cf06_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p12_prdgen_cf06_12
          trigger /prod12/gefs_legacy12/gefs12_cf06/jgefs_forecast_cf06_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p13_prdgen_cf06_12
          trigger /prod12/gefs_legacy12/gefs12_cf06/jgefs_forecast_cf06_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p14_prdgen_cf06_12
          trigger /prod12/gefs_legacy12/gefs12_cf06/jgefs_forecast_cf06_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p15_prdgen_cf06_12
          trigger /prod12/gefs_legacy12/gefs12_cf06/jgefs_forecast_cf06_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p16_prdgen_cf06_12
          trigger /prod12/gefs_legacy12/gefs12_cf06/jgefs_forecast_cf06_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p17_prdgen_cf06_12
          trigger /prod12/gefs_legacy12/gefs12_cf06/jgefs_forecast_cf06_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p18_prdgen_cf06_12
          trigger /prod12/gefs_legacy12/gefs12_cf06/jgefs_forecast_cf06_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p19_prdgen_cf06_12
          trigger /prod12/gefs_legacy12/gefs12_cf06/jgefs_forecast_cf06_12 eq active
          edit ECF_PASS 'FREE'
        task jgefs_p20_prdgen_cf06_12
          trigger /prod12/gefs_legacy12/gefs12_cf06/jgefs_forecast_cf06_12 eq active
          edit ECF_PASS 'FREE'
      endfamily
      task jgefs_post_track_cf06_12
        trigger /prod12/gefs_legacy12/gefs12_cf06/prdgen eq complete
        edit ECF_PASS 'FREE'
    endfamily
    task jgefs_post_cleanup_12
      trigger /prod12/gefs_legacy12/gefs12 eq complete and /prod12/gefs_legacy12/gefs12_cf06 eq complete and /prod12/gefs_legacy12/gefs12_cf00 eq complete and /prod12/gefs_legacy12/gefs12_cf18 eq complete
      edit ECF_PASS 'FREE'
  endfamily
  family aqm
    edit COM 'com2'
    edit ECF_FILES '/ecf/ecfnets/scripts/aqm'
    edit PROJ 'CMAQ-OPS'
    edit QUEUE 'prod2'
    family hi
      task jaqm_prephyb_hi
        trigger /prod12/nam/prdgen/jnam_prdgen_f00 == complete
      task jaqm_premaq_hi
        trigger jaqm_prephyb_hi == complete
      task jaqm_forecast_hi
        trigger jaqm_premaq_hi == complete
      task jaqm_post1_hi
        trigger jaqm_forecast_hi == complete
      task jaqm_post2_hi
        trigger jaqm_forecast_hi == complete
      task jaqm_post3_hi
        trigger jaqm_post1_hi == complete and jaqm_post2_hi == complete
    endfamily
    family ak
      task jaqm_prephyb_ak
        trigger /prod12/nam/prdgen/jnam_prdgen_f00 == complete
      task jaqm_premaq_ak
        trigger jaqm_prephyb_ak == complete
      task jaqm_forecast_ak
        trigger jaqm_premaq_ak == complete
      task jaqm_post1_ak
        trigger jaqm_forecast_ak == complete
      task jaqm_post2_ak
        trigger jaqm_forecast_ak == complete
      task jaqm_post3_ak
        trigger jaqm_post1_ak == complete and jaqm_post2_ak == complete
    endfamily
    family conus
      task jaqm_prephyb_conus
        trigger /prod12/nam/prdgen/jnam_prdgen_f00 == complete
      task jaqm_premaq_conus
        trigger jaqm_prephyb_conus == complete
      task jaqm_forecast_conus
        trigger jaqm_premaq_conus == complete
      task jaqm_post1_conus
        trigger jaqm_forecast_conus == complete
      task jaqm_post2_conus
        trigger jaqm_forecast_conus == complete
      task jaqm_post3_conus
        trigger jaqm_forecast_conus == complete
      task jaqm_post_pm25_bicor_conus
        trigger jaqm_forecast_conus == complete
    endfamily
    family emission
      task jaqm_emission_ingest
        time 14:00
      task jaqm_emission_archive
        trigger jaqm_emission_ingest == complete
        event 1 monthly
        event 2 yearly
        event 3 jtable
    endfamily
  endfamily
  family nam
    edit ECF_FILES '/ecf/ecfnets/scripts/nam'
    edit PROJ 'NAM-OPS'
    family mkboundaries
      family coldstart
        defstatus complete
        task jnam_coldstart_prep
          trigger /prod12/nam/mkboundaries/jnam_main:release_nam_coldstart
        task jnam_coldstart_partialcyc
          trigger jnam_coldstart_prep == complete
        task jnam_coldstart_sfcupdate
          trigger jnam_coldstart_partialcyc == complete
        task jnam_coldstart_prep_conus
          trigger /prod12/nam/mkboundaries/jnam_main:release_nam_coldstart
        task jnam_coldstart_partialcyc_conus
          trigger jnam_coldstart_prep_conus == complete
        task jnam_coldstart_sfcupdate_conus
          trigger jnam_coldstart_partialcyc_conus == complete
        task jnam_coldstart_prep_alaska
          trigger /prod12/nam/mkboundaries/jnam_main:release_nam_coldstart
        task jnam_coldstart_partialcyc_alaska
          trigger jnam_coldstart_prep_alaska == complete
        task jnam_coldstart_sfcupdate_alaska
          trigger jnam_coldstart_partialcyc_alaska == complete
        task jnam_coldstart_prep_hawaii
          trigger /prod12/nam/mkboundaries/jnam_main:release_nam_coldstart
        task jnam_coldstart_partialcyc_hawaii
          trigger jnam_coldstart_prep_hawaii == complete
        task jnam_coldstart_sfcupdate_hawaii
          trigger jnam_coldstart_partialcyc_hawaii == complete
        task jnam_coldstart_prep_prico
          trigger /prod12/nam/mkboundaries/jnam_main:release_nam_coldstart
        task jnam_coldstart_partialcyc_prico
          trigger jnam_coldstart_prep_prico == complete
        task jnam_coldstart_sfcupdate_prico
          trigger jnam_coldstart_partialcyc_prico == complete
        task jnam_coldstart_prep_firewx
          trigger /prod12/nam/mkboundaries/jnam_main:release_nam_coldstart
        task jnam_coldstart_partialcyc_firewx
          trigger jnam_coldstart_prep_firewx == complete
        task jnam_coldstart_sfcupdate_firewx
          trigger jnam_coldstart_partialcyc_firewx == complete
      endfamily
      task jnam_main
        trigger /prod12/prod_filemanager/jprod_setup == complete and /prod06/ndas12/time_minus_03hr/jndas_forecast == complete
        event 1 release_mkbnd
        event 2 release_nam_coldstart
        time 13:05
      task jnam_mkbnd
        trigger jnam_main:release_mkbnd
      task jnam_combc
        trigger jnam_mkbnd == complete
    endfamily
    family dump
      task jnam_tropcy_qc_reloc
        time 13:10
      task jnam_dump2
        time 13:10
      task jnam_dump
        time 13:15
      task jnam_dump_alert
        trigger /prod12/prod_filemanager/jprod_setup == complete and jnam_dump_post:release_nam12_dump_alert
      task jnam_dump_post
        trigger jnam_dump == complete and jnam_dump2 == complete
        event 1 release_nam12_dump_alert
      task jmodel_realtime_nam
        trigger jnam_dump_alert == complete
    endfamily
    family analysis
      task jnam_analysis_prico
        trigger /prod12/nam/prep/jnam_prep == complete and /prod12/nam/mkboundaries == complete and /prod06/gdas/enkf == complete
      task jnam_analysis_firewx
        trigger /prod12/nam/prep/jnam_prep == complete and /prod12/nam/mkboundaries == complete and /prod06/gdas/enkf == complete
      task jnam_analysis_hawaii
        trigger /prod12/nam/prep/jnam_prep == complete and /prod12/nam/mkboundaries == complete and /prod06/gdas/enkf == complete
      task jnam_analysis_alaska
        trigger /prod12/nam/prep/jnam_prep == complete and /prod12/nam/mkboundaries == complete and /prod06/gdas/enkf == complete
      task jnam_analysis
        trigger /prod12/nam/prep/jnam_prep == complete and /prod12/nam/mkboundaries == complete and /prod06/gdas/enkf == complete
      task jnam_analysis_conus
        trigger /prod12/nam/prep/jnam_prep == complete and /prod12/nam/mkboundaries == complete and /prod06/gdas/enkf == complete
    endfamily
    family prep
      task jnam_prep
        trigger /prod12/prod_filemanager/jprod_setup == complete and /prod12/nam/dump/jnam_dump == complete and /prod12/nam/dump/jnam_dump2 == complete and /prod12/nam/dump/jnam_tropcy_qc_reloc == complete
      task jnam_prep_post
        trigger /prod12/nam/analysis == complete
    endfamily
    family forecast
      task jnam_forecast_0036h
        trigger /prod12/nam/analysis == complete and /prod12/nam/mkboundaries/jnam_combc == complete
      task jnam_forecast_3660h
        trigger jnam_forecast_0036h == complete
      task jnam_forecast_6084h
        trigger jnam_forecast_3660h == complete
    endfamily
    family nest_firewx
      edit ECF_FILES '/ecf/ecfnets/scripts/nam/nest'
      edit grid 'firewx'
      edit NTASK '8'
      family post
        task jnam_post_manager
          trigger /prod12/nam/forecast == active or /prod12/nam/forecast == complete
          event 1 release_post00
          event 2 release_post01
          event 3 release_post02
          event 4 release_post03
          event 5 release_post04
          event 6 release_post05
          event 7 release_post06
          event 8 release_post07
          event 9 release_post08
          event 10 release_post09
          event 11 release_post10
          event 12 release_post11
          event 13 release_post12
          event 14 release_post13
          event 15 release_post14
          event 16 release_post15
          event 17 release_post16
          event 18 release_post17
          event 19 release_post18
          event 20 release_post19
          event 21 release_post20
          event 22 release_post21
          event 23 release_post22
          event 24 release_post23
          event 25 release_post24
          event 26 release_post25
          event 27 release_post26
          event 28 release_post27
          event 29 release_post28
          event 30 release_post29
          event 31 release_post30
          event 32 release_post31
          event 33 release_post32
          event 34 release_post33
          event 35 release_post34
          event 36 release_post35
          event 37 release_post36
        task jnam_post_f00
          trigger jnam_post_manager:release_post00
        task jnam_post_f01
          trigger jnam_post_manager:release_post01
        task jnam_post_f02
          trigger jnam_post_manager:release_post02
        task jnam_post_f03
          trigger jnam_post_manager:release_post03
        task jnam_post_f04
          trigger jnam_post_manager:release_post04
        task jnam_post_f05
          trigger jnam_post_manager:release_post05
        task jnam_post_f06
          trigger jnam_post_manager:release_post06
        task jnam_post_f07
          trigger jnam_post_manager:release_post07
        task jnam_post_f08
          trigger jnam_post_manager:release_post08
        task jnam_post_f09
          trigger jnam_post_manager:release_post09
        task jnam_post_f10
          trigger jnam_post_manager:release_post10
        task jnam_post_f11
          trigger jnam_post_manager:release_post11
        task jnam_post_f12
          trigger jnam_post_manager:release_post12
        task jnam_post_f13
          trigger jnam_post_manager:release_post13
        task jnam_post_f14
          trigger jnam_post_manager:release_post14
        task jnam_post_f15
          trigger jnam_post_manager:release_post15
        task jnam_post_f16
          trigger jnam_post_manager:release_post16
        task jnam_post_f17
          trigger jnam_post_manager:release_post17
        task jnam_post_f18
          trigger jnam_post_manager:release_post18
        task jnam_post_f19
          trigger jnam_post_manager:release_post19
        task jnam_post_f20
          trigger jnam_post_manager:release_post20
        task jnam_post_f21
          trigger jnam_post_manager:release_post21
        task jnam_post_f22
          trigger jnam_post_manager:release_post22
        task jnam_post_f23
          trigger jnam_post_manager:release_post23
        task jnam_post_f24
          trigger jnam_post_manager:release_post24
        task jnam_post_f25
          trigger jnam_post_manager:release_post25
        task jnam_post_f26
          trigger jnam_post_manager:release_post26
        task jnam_post_f27
          trigger jnam_post_manager:release_post27
        task jnam_post_f28
          trigger jnam_post_manager:release_post28
        task jnam_post_f29
          trigger jnam_post_manager:release_post29
        task jnam_post_f30
          trigger jnam_post_manager:release_post30
        task jnam_post_f31
          trigger jnam_post_manager:release_post31
        task jnam_post_f32
          trigger jnam_post_manager:release_post32
        task jnam_post_f33
          trigger jnam_post_manager:release_post33
        task jnam_post_f34
          trigger jnam_post_manager:release_post34
        task jnam_post_f35
          trigger jnam_post_manager:release_post35
        task jnam_post_f36
          trigger jnam_post_manager:release_post36
      endfamily
      family prdgen
        task jnam_prdgen_f00
          trigger /prod12/nam/nest_firewx/post/jnam_post_f00 == complete
        task jnam_prdgen_f01
          trigger /prod12/nam/nest_firewx/post/jnam_post_f01 == complete
        task jnam_prdgen_f02
          trigger /prod12/nam/nest_firewx/post/jnam_post_f02 == complete
        task jnam_prdgen_f03
          trigger /prod12/nam/nest_firewx/post/jnam_post_f03 == complete
        task jnam_prdgen_f04
          trigger /prod12/nam/nest_firewx/post/jnam_post_f04 == complete
        task jnam_prdgen_f05
          trigger /prod12/nam/nest_firewx/post/jnam_post_f05 == complete
        task jnam_prdgen_f06
          trigger /prod12/nam/nest_firewx/post/jnam_post_f06 == complete
        task jnam_prdgen_f07
          trigger /prod12/nam/nest_firewx/post/jnam_post_f07 == complete
        task jnam_prdgen_f08
          trigger /prod12/nam/nest_firewx/post/jnam_post_f08 == complete
        task jnam_prdgen_f09
          trigger /prod12/nam/nest_firewx/post/jnam_post_f09 == complete
        task jnam_prdgen_f10
          trigger /prod12/nam/nest_firewx/post/jnam_post_f10 == complete
        task jnam_prdgen_f11
          trigger /prod12/nam/nest_firewx/post/jnam_post_f11 == complete
        task jnam_prdgen_f12
          trigger /prod12/nam/nest_firewx/post/jnam_post_f12 == complete
        task jnam_prdgen_f13
          trigger /prod12/nam/nest_firewx/post/jnam_post_f13 == complete
        task jnam_prdgen_f14
          trigger /prod12/nam/nest_firewx/post/jnam_post_f14 == complete
        task jnam_prdgen_f15
          trigger /prod12/nam/nest_firewx/post/jnam_post_f15 == complete
        task jnam_prdgen_f16
          trigger /prod12/nam/nest_firewx/post/jnam_post_f16 == complete
        task jnam_prdgen_f17
          trigger /prod12/nam/nest_firewx/post/jnam_post_f17 == complete
        task jnam_prdgen_f18
          trigger /prod12/nam/nest_firewx/post/jnam_post_f18 == complete
        task jnam_prdgen_f19
          trigger /prod12/nam/nest_firewx/post/jnam_post_f19 == complete
        task jnam_prdgen_f20
          trigger /prod12/nam/nest_firewx/post/jnam_post_f20 == complete
        task jnam_prdgen_f21
          trigger /prod12/nam/nest_firewx/post/jnam_post_f21 == complete
        task jnam_prdgen_f22
          trigger /prod12/nam/nest_firewx/post/jnam_post_f22 == complete
        task jnam_prdgen_f23
          trigger /prod12/nam/nest_firewx/post/jnam_post_f23 == complete
        task jnam_prdgen_f24
          trigger /prod12/nam/nest_firewx/post/jnam_post_f24 == complete
        task jnam_prdgen_f25
          trigger /prod12/nam/nest_firewx/post/jnam_post_f25 == complete
        task jnam_prdgen_f26
          trigger /prod12/nam/nest_firewx/post/jnam_post_f26 == complete
        task jnam_prdgen_f27
          trigger /prod12/nam/nest_firewx/post/jnam_post_f27 == complete
        task jnam_prdgen_f28
          trigger /prod12/nam/nest_firewx/post/jnam_post_f28 == complete
        task jnam_prdgen_f29
          trigger /prod12/nam/nest_firewx/post/jnam_post_f29 == complete
        task jnam_prdgen_f30
          trigger /prod12/nam/nest_firewx/post/jnam_post_f30 == complete
        task jnam_prdgen_f31
          trigger /prod12/nam/nest_firewx/post/jnam_post_f31 == complete
        task jnam_prdgen_f32
          trigger /prod12/nam/nest_firewx/post/jnam_post_f32 == complete
        task jnam_prdgen_f33
          trigger /prod12/nam/nest_firewx/post/jnam_post_f33 == complete
        task jnam_prdgen_f34
          trigger /prod12/nam/nest_firewx/post/jnam_post_f34 == complete
        task jnam_prdgen_f35
          trigger /prod12/nam/nest_firewx/post/jnam_post_f35 == complete
        task jnam_prdgen_f36
          trigger /prod12/nam/nest_firewx/post/jnam_post_f36 == complete
      endfamily
    endfamily
    family nest_hawaii
      edit ECF_FILES '/ecf/ecfnets/scripts/nam/nest'
      edit grid 'hawaii'
      edit NTASK '8'
      family profile
        task jnam_profile_f00
          trigger /prod12/nam/nest_hawaii/post/jnam_post_manager:release_post00
        task jnam_profile_f01
          trigger /prod12/nam/nest_hawaii/post/jnam_post_manager:release_post01
        task jnam_profile_f02
          trigger /prod12/nam/nest_hawaii/post/jnam_post_manager:release_post02
        task jnam_profile_f03
          trigger /prod12/nam/nest_hawaii/post/jnam_post_manager:release_post03
        task jnam_profile_f04
          trigger /prod12/nam/nest_hawaii/post/jnam_post_manager:release_post04
        task jnam_profile_f05
          trigger /prod12/nam/nest_hawaii/post/jnam_post_manager:release_post05
        task jnam_profile_f06
          trigger /prod12/nam/nest_hawaii/post/jnam_post_manager:release_post06
        task jnam_profile_f07
          trigger /prod12/nam/nest_hawaii/post/jnam_post_manager:release_post07
        task jnam_profile_f08
          trigger /prod12/nam/nest_hawaii/post/jnam_post_manager:release_post08
        task jnam_profile_f09
          trigger /prod12/nam/nest_hawaii/post/jnam_post_manager:release_post09
        task jnam_profile_f10
          trigger /prod12/nam/nest_hawaii/post/jnam_post_manager:release_post10
        task jnam_profile_f11
          trigger /prod12/nam/nest_hawaii/post/jnam_post_manager:release_post11
        task jnam_profile_f12
          trigger /prod12/nam/nest_hawaii/post/jnam_post_manager:release_post12
        task jnam_profile_f13
          trigger /prod12/nam/nest_hawaii/post/jnam_post_manager:release_post13
        task jnam_profile_f14
          trigger /prod12/nam/nest_hawaii/post/jnam_post_manager:release_post14
        task jnam_profile_f15
          trigger /prod12/nam/nest_hawaii/post/jnam_post_manager:release_post15
        task jnam_profile_f16
          trigger /prod12/nam/nest_hawaii/post/jnam_post_manager:release_post16
        task jnam_profile_f17
          trigger /prod12/nam/nest_hawaii/post/jnam_post_manager:release_post17
        task jnam_profile_f18
          trigger /prod12/nam/nest_hawaii/post/jnam_post_manager:release_post18
        task jnam_profile_f19
          trigger /prod12/nam/nest_hawaii/post/jnam_post_manager:release_post19
        task jnam_profile_f20
          trigger /prod12/nam/nest_hawaii/post/jnam_post_manager:release_post20
        task jnam_profile_f21
          trigger /prod12/nam/nest_hawaii/post/jnam_post_manager:release_post21
        task jnam_profile_f22
          trigger /prod12/nam/nest_hawaii/post/jnam_post_manager:release_post22
        task jnam_profile_f23
          trigger /prod12/nam/nest_hawaii/post/jnam_post_manager:release_post23
        task jnam_profile_f24
          trigger /prod12/nam/nest_hawaii/post/jnam_post_manager:release_post24
        task jnam_profile_f25
          trigger /prod12/nam/nest_hawaii/post/jnam_post_manager:release_post25
        task jnam_profile_f26
          trigger /prod12/nam/nest_hawaii/post/jnam_post_manager:release_post26
        task jnam_profile_f27
          trigger /prod12/nam/nest_hawaii/post/jnam_post_manager:release_post27
        task jnam_profile_f28
          trigger /prod12/nam/nest_hawaii/post/jnam_post_manager:release_post28
        task jnam_profile_f29
          trigger /prod12/nam/nest_hawaii/post/jnam_post_manager:release_post29
        task jnam_profile_f30
          trigger /prod12/nam/nest_hawaii/post/jnam_post_manager:release_post30
        task jnam_profile_f31
          trigger /prod12/nam/nest_hawaii/post/jnam_post_manager:release_post31
        task jnam_profile_f32
          trigger /prod12/nam/nest_hawaii/post/jnam_post_manager:release_post32
        task jnam_profile_f33
          trigger /prod12/nam/nest_hawaii/post/jnam_post_manager:release_post33
        task jnam_profile_f34
          trigger /prod12/nam/nest_hawaii/post/jnam_post_manager:release_post34
        task jnam_profile_f35
          trigger /prod12/nam/nest_hawaii/post/jnam_post_manager:release_post35
        task jnam_profile_f36
          trigger /prod12/nam/nest_hawaii/post/jnam_post_manager:release_post36
        task jnam_profile_f37
          trigger /prod12/nam/nest_hawaii/post/jnam_post_manager:release_post37
        task jnam_profile_f38
          trigger /prod12/nam/nest_hawaii/post/jnam_post_manager:release_post38
        task jnam_profile_f39
          trigger /prod12/nam/nest_hawaii/post/jnam_post_manager:release_post39
        task jnam_profile_f40
          trigger /prod12/nam/nest_hawaii/post/jnam_post_manager:release_post40
        task jnam_profile_f41
          trigger /prod12/nam/nest_hawaii/post/jnam_post_manager:release_post41
        task jnam_profile_f42
          trigger /prod12/nam/nest_hawaii/post/jnam_post_manager:release_post42
        task jnam_profile_f43
          trigger /prod12/nam/nest_hawaii/post/jnam_post_manager:release_post43
        task jnam_profile_f44
          trigger /prod12/nam/nest_hawaii/post/jnam_post_manager:release_post44
        task jnam_profile_f45
          trigger /prod12/nam/nest_hawaii/post/jnam_post_manager:release_post45
        task jnam_profile_f46
          trigger /prod12/nam/nest_hawaii/post/jnam_post_manager:release_post46
        task jnam_profile_f47
          trigger /prod12/nam/nest_hawaii/post/jnam_post_manager:release_post47
        task jnam_profile_f48
          trigger /prod12/nam/nest_hawaii/post/jnam_post_manager:release_post48
        task jnam_profile_f49
          trigger /prod12/nam/nest_hawaii/post/jnam_post_manager:release_post49
        task jnam_profile_f50
          trigger /prod12/nam/nest_hawaii/post/jnam_post_manager:release_post50
        task jnam_profile_f51
          trigger /prod12/nam/nest_hawaii/post/jnam_post_manager:release_post51
        task jnam_profile_f52
          trigger /prod12/nam/nest_hawaii/post/jnam_post_manager:release_post52
        task jnam_profile_f53
          trigger /prod12/nam/nest_hawaii/post/jnam_post_manager:release_post53
        task jnam_profile_f54
          trigger /prod12/nam/nest_hawaii/post/jnam_post_manager:release_post54
        task jnam_profile_f55
          trigger /prod12/nam/nest_hawaii/post/jnam_post_manager:release_post55
        task jnam_profile_f56
          trigger /prod12/nam/nest_hawaii/post/jnam_post_manager:release_post56
        task jnam_profile_f57
          trigger /prod12/nam/nest_hawaii/post/jnam_post_manager:release_post57
        task jnam_profile_f58
          trigger /prod12/nam/nest_hawaii/post/jnam_post_manager:release_post58
        task jnam_profile_f59
          trigger /prod12/nam/nest_hawaii/post/jnam_post_manager:release_post59
        task jnam_profile_f60
          trigger /prod12/nam/nest_hawaii/post/jnam_post_manager:release_post60
      endfamily
      family post
        task jnam_post_manager
          trigger /prod12/nam/forecast == active or /prod12/nam/forecast == complete
          event 1 release_post00
          event 2 release_post01
          event 3 release_post02
          event 4 release_post03
          event 5 release_post04
          event 6 release_post05
          event 7 release_post06
          event 8 release_post07
          event 9 release_post08
          event 10 release_post09
          event 11 release_post10
          event 12 release_post11
          event 13 release_post12
          event 14 release_post13
          event 15 release_post14
          event 16 release_post15
          event 17 release_post16
          event 18 release_post17
          event 19 release_post18
          event 20 release_post19
          event 21 release_post20
          event 22 release_post21
          event 23 release_post22
          event 24 release_post23
          event 25 release_post24
          event 26 release_post25
          event 27 release_post26
          event 28 release_post27
          event 29 release_post28
          event 30 release_post29
          event 31 release_post30
          event 32 release_post31
          event 33 release_post32
          event 34 release_post33
          event 35 release_post34
          event 36 release_post35
          event 37 release_post36
          event 38 release_post37
          event 39 release_post38
          event 40 release_post39
          event 41 release_post40
          event 42 release_post41
          event 43 release_post42
          event 44 release_post43
          event 45 release_post44
          event 46 release_post45
          event 47 release_post46
          event 48 release_post47
          event 49 release_post48
          event 50 release_post49
          event 51 release_post50
          event 52 release_post51
          event 53 release_post52
          event 54 release_post53
          event 55 release_post54
          event 56 release_post55
          event 57 release_post56
          event 58 release_post57
          event 59 release_post58
          event 60 release_post59
          event 61 release_post60
        task jnam_post_f00
          trigger jnam_post_manager:release_post00
        task jnam_post_f01
          trigger jnam_post_manager:release_post01
        task jnam_post_f02
          trigger jnam_post_manager:release_post02
        task jnam_post_f03
          trigger jnam_post_manager:release_post03
        task jnam_post_f04
          trigger jnam_post_manager:release_post04
        task jnam_post_f05
          trigger jnam_post_manager:release_post05
        task jnam_post_f06
          trigger jnam_post_manager:release_post06
        task jnam_post_f07
          trigger jnam_post_manager:release_post07
        task jnam_post_f08
          trigger jnam_post_manager:release_post08
        task jnam_post_f09
          trigger jnam_post_manager:release_post09
        task jnam_post_f10
          trigger jnam_post_manager:release_post10
        task jnam_post_f11
          trigger jnam_post_manager:release_post11
        task jnam_post_f12
          trigger jnam_post_manager:release_post12
        task jnam_post_f13
          trigger jnam_post_manager:release_post13
        task jnam_post_f14
          trigger jnam_post_manager:release_post14
        task jnam_post_f15
          trigger jnam_post_manager:release_post15
        task jnam_post_f16
          trigger jnam_post_manager:release_post16
        task jnam_post_f17
          trigger jnam_post_manager:release_post17
        task jnam_post_f18
          trigger jnam_post_manager:release_post18
        task jnam_post_f19
          trigger jnam_post_manager:release_post19
        task jnam_post_f20
          trigger jnam_post_manager:release_post20
        task jnam_post_f21
          trigger jnam_post_manager:release_post21
        task jnam_post_f22
          trigger jnam_post_manager:release_post22
        task jnam_post_f23
          trigger jnam_post_manager:release_post23
        task jnam_post_f24
          trigger jnam_post_manager:release_post24
        task jnam_post_f25
          trigger jnam_post_manager:release_post25
        task jnam_post_f26
          trigger jnam_post_manager:release_post26
        task jnam_post_f27
          trigger jnam_post_manager:release_post27
        task jnam_post_f28
          trigger jnam_post_manager:release_post28
        task jnam_post_f29
          trigger jnam_post_manager:release_post29
        task jnam_post_f30
          trigger jnam_post_manager:release_post30
        task jnam_post_f31
          trigger jnam_post_manager:release_post31
        task jnam_post_f32
          trigger jnam_post_manager:release_post32
        task jnam_post_f33
          trigger jnam_post_manager:release_post33
        task jnam_post_f34
          trigger jnam_post_manager:release_post34
        task jnam_post_f35
          trigger jnam_post_manager:release_post35
        task jnam_post_f36
          trigger jnam_post_manager:release_post36
        task jnam_post_f37
          trigger jnam_post_manager:release_post37
        task jnam_post_f38
          trigger jnam_post_manager:release_post38
        task jnam_post_f39
          trigger jnam_post_manager:release_post39
        task jnam_post_f40
          trigger jnam_post_manager:release_post40
        task jnam_post_f41
          trigger jnam_post_manager:release_post41
        task jnam_post_f42
          trigger jnam_post_manager:release_post42
        task jnam_post_f43
          trigger jnam_post_manager:release_post43
        task jnam_post_f44
          trigger jnam_post_manager:release_post44
        task jnam_post_f45
          trigger jnam_post_manager:release_post45
        task jnam_post_f46
          trigger jnam_post_manager:release_post46
        task jnam_post_f47
          trigger jnam_post_manager:release_post47
        task jnam_post_f48
          trigger jnam_post_manager:release_post48
        task jnam_post_f49
          trigger jnam_post_manager:release_post49
        task jnam_post_f50
          trigger jnam_post_manager:release_post50
        task jnam_post_f51
          trigger jnam_post_manager:release_post51
        task jnam_post_f52
          trigger jnam_post_manager:release_post52
        task jnam_post_f53
          trigger jnam_post_manager:release_post53
        task jnam_post_f54
          trigger jnam_post_manager:release_post54
        task jnam_post_f55
          trigger jnam_post_manager:release_post55
        task jnam_post_f56
          trigger jnam_post_manager:release_post56
        task jnam_post_f57
          trigger jnam_post_manager:release_post57
        task jnam_post_f58
          trigger jnam_post_manager:release_post58
        task jnam_post_f59
          trigger jnam_post_manager:release_post59
        task jnam_post_f60
          trigger jnam_post_manager:release_post60
      endfamily
      family prdgen
        task jnam_prdgen_f00
          trigger /prod12/nam/nest_hawaii/post/jnam_post_f00 == complete
        task jnam_prdgen_f01
          trigger /prod12/nam/nest_hawaii/post/jnam_post_f01 == complete
        task jnam_prdgen_f02
          trigger /prod12/nam/nest_hawaii/post/jnam_post_f02 == complete
        task jnam_prdgen_f03
          trigger /prod12/nam/nest_hawaii/post/jnam_post_f03 == complete
        task jnam_prdgen_f04
          trigger /prod12/nam/nest_hawaii/post/jnam_post_f04 == complete
        task jnam_prdgen_f05
          trigger /prod12/nam/nest_hawaii/post/jnam_post_f05 == complete
        task jnam_prdgen_f06
          trigger /prod12/nam/nest_hawaii/post/jnam_post_f06 == complete
        task jnam_prdgen_f07
          trigger /prod12/nam/nest_hawaii/post/jnam_post_f07 == complete
        task jnam_prdgen_f08
          trigger /prod12/nam/nest_hawaii/post/jnam_post_f08 == complete
        task jnam_prdgen_f09
          trigger /prod12/nam/nest_hawaii/post/jnam_post_f09 == complete
        task jnam_prdgen_f10
          trigger /prod12/nam/nest_hawaii/post/jnam_post_f10 == complete
        task jnam_prdgen_f11
          trigger /prod12/nam/nest_hawaii/post/jnam_post_f11 == complete
        task jnam_prdgen_f12
          trigger /prod12/nam/nest_hawaii/post/jnam_post_f12 == complete
        task jnam_prdgen_f13
          trigger /prod12/nam/nest_hawaii/post/jnam_post_f13 == complete
        task jnam_prdgen_f14
          trigger /prod12/nam/nest_hawaii/post/jnam_post_f14 == complete
        task jnam_prdgen_f15
          trigger /prod12/nam/nest_hawaii/post/jnam_post_f15 == complete
        task jnam_prdgen_f16
          trigger /prod12/nam/nest_hawaii/post/jnam_post_f16 == complete
        task jnam_prdgen_f17
          trigger /prod12/nam/nest_hawaii/post/jnam_post_f17 == complete
        task jnam_prdgen_f18
          trigger /prod12/nam/nest_hawaii/post/jnam_post_f18 == complete
        task jnam_prdgen_f19
          trigger /prod12/nam/nest_hawaii/post/jnam_post_f19 == complete
        task jnam_prdgen_f20
          trigger /prod12/nam/nest_hawaii/post/jnam_post_f20 == complete
        task jnam_prdgen_f21
          trigger /prod12/nam/nest_hawaii/post/jnam_post_f21 == complete
        task jnam_prdgen_f22
          trigger /prod12/nam/nest_hawaii/post/jnam_post_f22 == complete
        task jnam_prdgen_f23
          trigger /prod12/nam/nest_hawaii/post/jnam_post_f23 == complete
        task jnam_prdgen_f24
          trigger /prod12/nam/nest_hawaii/post/jnam_post_f24 == complete
        task jnam_prdgen_f25
          trigger /prod12/nam/nest_hawaii/post/jnam_post_f25 == complete
        task jnam_prdgen_f26
          trigger /prod12/nam/nest_hawaii/post/jnam_post_f26 == complete
        task jnam_prdgen_f27
          trigger /prod12/nam/nest_hawaii/post/jnam_post_f27 == complete
        task jnam_prdgen_f28
          trigger /prod12/nam/nest_hawaii/post/jnam_post_f28 == complete
        task jnam_prdgen_f29
          trigger /prod12/nam/nest_hawaii/post/jnam_post_f29 == complete
        task jnam_prdgen_f30
          trigger /prod12/nam/nest_hawaii/post/jnam_post_f30 == complete
        task jnam_prdgen_f31
          trigger /prod12/nam/nest_hawaii/post/jnam_post_f31 == complete
        task jnam_prdgen_f32
          trigger /prod12/nam/nest_hawaii/post/jnam_post_f32 == complete
        task jnam_prdgen_f33
          trigger /prod12/nam/nest_hawaii/post/jnam_post_f33 == complete
        task jnam_prdgen_f34
          trigger /prod12/nam/nest_hawaii/post/jnam_post_f34 == complete
        task jnam_prdgen_f35
          trigger /prod12/nam/nest_hawaii/post/jnam_post_f35 == complete
        task jnam_prdgen_f36
          trigger /prod12/nam/nest_hawaii/post/jnam_post_f36 == complete
        task jnam_prdgen_f39
          trigger /prod12/nam/nest_hawaii/post/jnam_post_f39 == complete
        task jnam_prdgen_f42
          trigger /prod12/nam/nest_hawaii/post/jnam_post_f42 == complete
        task jnam_prdgen_f45
          trigger /prod12/nam/nest_hawaii/post/jnam_post_f45 == complete
        task jnam_prdgen_f48
          trigger /prod12/nam/nest_hawaii/post/jnam_post_f48 == complete
        task jnam_prdgen_f51
          trigger /prod12/nam/nest_hawaii/post/jnam_post_f51 == complete
        task jnam_prdgen_f54
          trigger /prod12/nam/nest_hawaii/post/jnam_post_f54 == complete
        task jnam_prdgen_f57
          trigger /prod12/nam/nest_hawaii/post/jnam_post_f57 == complete
        task jnam_prdgen_f60
          trigger /prod12/nam/nest_hawaii/post/jnam_post_f60 == complete
      endfamily
    endfamily
    family nest_prico
      edit ECF_FILES '/ecf/ecfnets/scripts/nam/nest'
      edit grid 'prico'
      edit NTASK '8'
      family profile
        task jnam_profile_f00
          trigger /prod12/nam/nest_prico/post/jnam_post_manager:release_post00
        task jnam_profile_f01
          trigger /prod12/nam/nest_prico/post/jnam_post_manager:release_post01
        task jnam_profile_f02
          trigger /prod12/nam/nest_prico/post/jnam_post_manager:release_post02
        task jnam_profile_f03
          trigger /prod12/nam/nest_prico/post/jnam_post_manager:release_post03
        task jnam_profile_f04
          trigger /prod12/nam/nest_prico/post/jnam_post_manager:release_post04
        task jnam_profile_f05
          trigger /prod12/nam/nest_prico/post/jnam_post_manager:release_post05
        task jnam_profile_f06
          trigger /prod12/nam/nest_prico/post/jnam_post_manager:release_post06
        task jnam_profile_f07
          trigger /prod12/nam/nest_prico/post/jnam_post_manager:release_post07
        task jnam_profile_f08
          trigger /prod12/nam/nest_prico/post/jnam_post_manager:release_post08
        task jnam_profile_f09
          trigger /prod12/nam/nest_prico/post/jnam_post_manager:release_post09
        task jnam_profile_f10
          trigger /prod12/nam/nest_prico/post/jnam_post_manager:release_post10
        task jnam_profile_f11
          trigger /prod12/nam/nest_prico/post/jnam_post_manager:release_post11
        task jnam_profile_f12
          trigger /prod12/nam/nest_prico/post/jnam_post_manager:release_post12
        task jnam_profile_f13
          trigger /prod12/nam/nest_prico/post/jnam_post_manager:release_post13
        task jnam_profile_f14
          trigger /prod12/nam/nest_prico/post/jnam_post_manager:release_post14
        task jnam_profile_f15
          trigger /prod12/nam/nest_prico/post/jnam_post_manager:release_post15
        task jnam_profile_f16
          trigger /prod12/nam/nest_prico/post/jnam_post_manager:release_post16
        task jnam_profile_f17
          trigger /prod12/nam/nest_prico/post/jnam_post_manager:release_post17
        task jnam_profile_f18
          trigger /prod12/nam/nest_prico/post/jnam_post_manager:release_post18
        task jnam_profile_f19
          trigger /prod12/nam/nest_prico/post/jnam_post_manager:release_post19
        task jnam_profile_f20
          trigger /prod12/nam/nest_prico/post/jnam_post_manager:release_post20
        task jnam_profile_f21
          trigger /prod12/nam/nest_prico/post/jnam_post_manager:release_post21
        task jnam_profile_f22
          trigger /prod12/nam/nest_prico/post/jnam_post_manager:release_post22
        task jnam_profile_f23
          trigger /prod12/nam/nest_prico/post/jnam_post_manager:release_post23
        task jnam_profile_f24
          trigger /prod12/nam/nest_prico/post/jnam_post_manager:release_post24
        task jnam_profile_f25
          trigger /prod12/nam/nest_prico/post/jnam_post_manager:release_post25
        task jnam_profile_f26
          trigger /prod12/nam/nest_prico/post/jnam_post_manager:release_post26
        task jnam_profile_f27
          trigger /prod12/nam/nest_prico/post/jnam_post_manager:release_post27
        task jnam_profile_f28
          trigger /prod12/nam/nest_prico/post/jnam_post_manager:release_post28
        task jnam_profile_f29
          trigger /prod12/nam/nest_prico/post/jnam_post_manager:release_post29
        task jnam_profile_f30
          trigger /prod12/nam/nest_prico/post/jnam_post_manager:release_post30
        task jnam_profile_f31
          trigger /prod12/nam/nest_prico/post/jnam_post_manager:release_post31
        task jnam_profile_f32
          trigger /prod12/nam/nest_prico/post/jnam_post_manager:release_post32
        task jnam_profile_f33
          trigger /prod12/nam/nest_prico/post/jnam_post_manager:release_post33
        task jnam_profile_f34
          trigger /prod12/nam/nest_prico/post/jnam_post_manager:release_post34
        task jnam_profile_f35
          trigger /prod12/nam/nest_prico/post/jnam_post_manager:release_post35
        task jnam_profile_f36
          trigger /prod12/nam/nest_prico/post/jnam_post_manager:release_post36
        task jnam_profile_f37
          trigger /prod12/nam/nest_prico/post/jnam_post_manager:release_post37
        task jnam_profile_f38
          trigger /prod12/nam/nest_prico/post/jnam_post_manager:release_post38
        task jnam_profile_f39
          trigger /prod12/nam/nest_prico/post/jnam_post_manager:release_post39
        task jnam_profile_f40
          trigger /prod12/nam/nest_prico/post/jnam_post_manager:release_post40
        task jnam_profile_f41
          trigger /prod12/nam/nest_prico/post/jnam_post_manager:release_post41
        task jnam_profile_f42
          trigger /prod12/nam/nest_prico/post/jnam_post_manager:release_post42
        task jnam_profile_f43
          trigger /prod12/nam/nest_prico/post/jnam_post_manager:release_post43
        task jnam_profile_f44
          trigger /prod12/nam/nest_prico/post/jnam_post_manager:release_post44
        task jnam_profile_f45
          trigger /prod12/nam/nest_prico/post/jnam_post_manager:release_post45
        task jnam_profile_f46
          trigger /prod12/nam/nest_prico/post/jnam_post_manager:release_post46
        task jnam_profile_f47
          trigger /prod12/nam/nest_prico/post/jnam_post_manager:release_post47
        task jnam_profile_f48
          trigger /prod12/nam/nest_prico/post/jnam_post_manager:release_post48
        task jnam_profile_f49
          trigger /prod12/nam/nest_prico/post/jnam_post_manager:release_post49
        task jnam_profile_f50
          trigger /prod12/nam/nest_prico/post/jnam_post_manager:release_post50
        task jnam_profile_f51
          trigger /prod12/nam/nest_prico/post/jnam_post_manager:release_post51
        task jnam_profile_f52
          trigger /prod12/nam/nest_prico/post/jnam_post_manager:release_post52
        task jnam_profile_f53
          trigger /prod12/nam/nest_prico/post/jnam_post_manager:release_post53
        task jnam_profile_f54
          trigger /prod12/nam/nest_prico/post/jnam_post_manager:release_post54
        task jnam_profile_f55
          trigger /prod12/nam/nest_prico/post/jnam_post_manager:release_post55
        task jnam_profile_f56
          trigger /prod12/nam/nest_prico/post/jnam_post_manager:release_post56
        task jnam_profile_f57
          trigger /prod12/nam/nest_prico/post/jnam_post_manager:release_post57
        task jnam_profile_f58
          trigger /prod12/nam/nest_prico/post/jnam_post_manager:release_post58
        task jnam_profile_f59
          trigger /prod12/nam/nest_prico/post/jnam_post_manager:release_post59
        task jnam_profile_f60
          trigger /prod12/nam/nest_prico/post/jnam_post_manager:release_post60
      endfamily
      family post
        task jnam_post_manager
          trigger /prod12/nam/forecast == active or /prod12/nam/forecast == complete
          event 1 release_post00
          event 2 release_post01
          event 3 release_post02
          event 4 release_post03
          event 5 release_post04
          event 6 release_post05
          event 7 release_post06
          event 8 release_post07
          event 9 release_post08
          event 10 release_post09
          event 11 release_post10
          event 12 release_post11
          event 13 release_post12
          event 14 release_post13
          event 15 release_post14
          event 16 release_post15
          event 17 release_post16
          event 18 release_post17
          event 19 release_post18
          event 20 release_post19
          event 21 release_post20
          event 22 release_post21
          event 23 release_post22
          event 24 release_post23
          event 25 release_post24
          event 26 release_post25
          event 27 release_post26
          event 28 release_post27
          event 29 release_post28
          event 30 release_post29
          event 31 release_post30
          event 32 release_post31
          event 33 release_post32
          event 34 release_post33
          event 35 release_post34
          event 36 release_post35
          event 37 release_post36
          event 38 release_post37
          event 39 release_post38
          event 40 release_post39
          event 41 release_post40
          event 42 release_post41
          event 43 release_post42
          event 44 release_post43
          event 45 release_post44
          event 46 release_post45
          event 47 release_post46
          event 48 release_post47
          event 49 release_post48
          event 50 release_post49
          event 51 release_post50
          event 52 release_post51
          event 53 release_post52
          event 54 release_post53
          event 55 release_post54
          event 56 release_post55
          event 57 release_post56
          event 58 release_post57
          event 59 release_post58
          event 60 release_post59
          event 61 release_post60
        task jnam_post_f00
          trigger jnam_post_manager:release_post00
        task jnam_post_f01
          trigger jnam_post_manager:release_post01
        task jnam_post_f02
          trigger jnam_post_manager:release_post03
        task jnam_post_f03
          trigger jnam_post_manager:release_post03
        task jnam_post_f04
          trigger jnam_post_manager:release_post04
        task jnam_post_f05
          trigger jnam_post_manager:release_post05
        task jnam_post_f06
          trigger jnam_post_manager:release_post06
        task jnam_post_f07
          trigger jnam_post_manager:release_post07
        task jnam_post_f08
          trigger jnam_post_manager:release_post08
        task jnam_post_f09
          trigger jnam_post_manager:release_post09
        task jnam_post_f10
          trigger jnam_post_manager:release_post10
        task jnam_post_f11
          trigger jnam_post_manager:release_post11
        task jnam_post_f12
          trigger jnam_post_manager:release_post12
        task jnam_post_f13
          trigger jnam_post_manager:release_post13
        task jnam_post_f14
          trigger jnam_post_manager:release_post14
        task jnam_post_f15
          trigger jnam_post_manager:release_post15
        task jnam_post_f16
          trigger jnam_post_manager:release_post16
        task jnam_post_f17
          trigger jnam_post_manager:release_post17
        task jnam_post_f18
          trigger jnam_post_manager:release_post18
        task jnam_post_f19
          trigger jnam_post_manager:release_post19
        task jnam_post_f20
          trigger jnam_post_manager:release_post20
        task jnam_post_f21
          trigger jnam_post_manager:release_post21
        task jnam_post_f22
          trigger jnam_post_manager:release_post22
        task jnam_post_f23
          trigger jnam_post_manager:release_post23
        task jnam_post_f24
          trigger jnam_post_manager:release_post24
        task jnam_post_f25
          trigger jnam_post_manager:release_post25
        task jnam_post_f26
          trigger jnam_post_manager:release_post26
        task jnam_post_f27
          trigger jnam_post_manager:release_post27
        task jnam_post_f28
          trigger jnam_post_manager:release_post28
        task jnam_post_f29
          trigger jnam_post_manager:release_post29
        task jnam_post_f30
          trigger jnam_post_manager:release_post30
        task jnam_post_f31
          trigger jnam_post_manager:release_post31
        task jnam_post_f32
          trigger jnam_post_manager:release_post32
        task jnam_post_f33
          trigger jnam_post_manager:release_post33
        task jnam_post_f34
          trigger jnam_post_manager:release_post34
        task jnam_post_f35
          trigger jnam_post_manager:release_post35
        task jnam_post_f36
          trigger jnam_post_manager:release_post36
        task jnam_post_f37
          trigger jnam_post_manager:release_post37
        task jnam_post_f38
          trigger jnam_post_manager:release_post38
        task jnam_post_f39
          trigger jnam_post_manager:release_post39
        task jnam_post_f40
          trigger jnam_post_manager:release_post40
        task jnam_post_f41
          trigger jnam_post_manager:release_post41
        task jnam_post_f42
          trigger jnam_post_manager:release_post42
        task jnam_post_f43
          trigger jnam_post_manager:release_post43
        task jnam_post_f44
          trigger jnam_post_manager:release_post44
        task jnam_post_f45
          trigger jnam_post_manager:release_post45
        task jnam_post_f46
          trigger jnam_post_manager:release_post46
        task jnam_post_f47
          trigger jnam_post_manager:release_post47
        task jnam_post_f48
          trigger jnam_post_manager:release_post48
        task jnam_post_f49
          trigger jnam_post_manager:release_post49
        task jnam_post_f50
          trigger jnam_post_manager:release_post50
        task jnam_post_f51
          trigger jnam_post_manager:release_post51
        task jnam_post_f52
          trigger jnam_post_manager:release_post52
        task jnam_post_f53
          trigger jnam_post_manager:release_post53
        task jnam_post_f54
          trigger jnam_post_manager:release_post54
        task jnam_post_f55
          trigger jnam_post_manager:release_post55
        task jnam_post_f56
          trigger jnam_post_manager:release_post56
        task jnam_post_f57
          trigger jnam_post_manager:release_post57
        task jnam_post_f58
          trigger jnam_post_manager:release_post58
        task jnam_post_f59
          trigger jnam_post_manager:release_post59
        task jnam_post_f60
          trigger jnam_post_manager:release_post60
      endfamily
      family prdgen
        task jnam_prdgen_f00
          trigger /prod12/nam/nest_prico/post/jnam_post_f00 == complete
        task jnam_prdgen_f01
          trigger /prod12/nam/nest_prico/post/jnam_post_f01 == complete
        task jnam_prdgen_f02
          trigger /prod12/nam/nest_prico/post/jnam_post_f02 == complete
        task jnam_prdgen_f03
          trigger /prod12/nam/nest_prico/post/jnam_post_f03 == complete
        task jnam_prdgen_f04
          trigger /prod12/nam/nest_prico/post/jnam_post_f04 == complete
        task jnam_prdgen_f05
          trigger /prod12/nam/nest_prico/post/jnam_post_f05 == complete
        task jnam_prdgen_f06
          trigger /prod12/nam/nest_prico/post/jnam_post_f06 == complete
        task jnam_prdgen_f07
          trigger /prod12/nam/nest_prico/post/jnam_post_f07 == complete
        task jnam_prdgen_f08
          trigger /prod12/nam/nest_prico/post/jnam_post_f08 == complete
        task jnam_prdgen_f09
          trigger /prod12/nam/nest_prico/post/jnam_post_f09 == complete
        task jnam_prdgen_f10
          trigger /prod12/nam/nest_prico/post/jnam_post_f10 == complete
        task jnam_prdgen_f11
          trigger /prod12/nam/nest_prico/post/jnam_post_f11 == complete
        task jnam_prdgen_f12
          trigger /prod12/nam/nest_prico/post/jnam_post_f12 == complete
        task jnam_prdgen_f13
          trigger /prod12/nam/nest_prico/post/jnam_post_f13 == complete
        task jnam_prdgen_f14
          trigger /prod12/nam/nest_prico/post/jnam_post_f14 == complete
        task jnam_prdgen_f15
          trigger /prod12/nam/nest_prico/post/jnam_post_f15 == complete
        task jnam_prdgen_f16
          trigger /prod12/nam/nest_prico/post/jnam_post_f16 == complete
        task jnam_prdgen_f17
          trigger /prod12/nam/nest_prico/post/jnam_post_f17 == complete
        task jnam_prdgen_f18
          trigger /prod12/nam/nest_prico/post/jnam_post_f18 == complete
        task jnam_prdgen_f19
          trigger /prod12/nam/nest_prico/post/jnam_post_f19 == complete
        task jnam_prdgen_f20
          trigger /prod12/nam/nest_prico/post/jnam_post_f20 == complete
        task jnam_prdgen_f21
          trigger /prod12/nam/nest_prico/post/jnam_post_f21 == complete
        task jnam_prdgen_f22
          trigger /prod12/nam/nest_prico/post/jnam_post_f22 == complete
        task jnam_prdgen_f23
          trigger /prod12/nam/nest_prico/post/jnam_post_f23 == complete
        task jnam_prdgen_f24
          trigger /prod12/nam/nest_prico/post/jnam_post_f24 == complete
        task jnam_prdgen_f25
          trigger /prod12/nam/nest_prico/post/jnam_post_f25 == complete
        task jnam_prdgen_f26
          trigger /prod12/nam/nest_prico/post/jnam_post_f26 == complete
        task jnam_prdgen_f27
          trigger /prod12/nam/nest_prico/post/jnam_post_f27 == complete
        task jnam_prdgen_f28
          trigger /prod12/nam/nest_prico/post/jnam_post_f28 == complete
        task jnam_prdgen_f29
          trigger /prod12/nam/nest_prico/post/jnam_post_f29 == complete
        task jnam_prdgen_f30
          trigger /prod12/nam/nest_prico/post/jnam_post_f30 == complete
        task jnam_prdgen_f31
          trigger /prod12/nam/nest_prico/post/jnam_post_f31 == complete
        task jnam_prdgen_f32
          trigger /prod12/nam/nest_prico/post/jnam_post_f32 == complete
        task jnam_prdgen_f33
          trigger /prod12/nam/nest_prico/post/jnam_post_f33 == complete
        task jnam_prdgen_f34
          trigger /prod12/nam/nest_prico/post/jnam_post_f34 == complete
        task jnam_prdgen_f35
          trigger /prod12/nam/nest_prico/post/jnam_post_f35 == complete
        task jnam_prdgen_f36
          trigger /prod12/nam/nest_prico/post/jnam_post_f36 == complete
        task jnam_prdgen_f39
          trigger /prod12/nam/nest_prico/post/jnam_post_f39 == complete
        task jnam_prdgen_f42
          trigger /prod12/nam/nest_prico/post/jnam_post_f42 == complete
        task jnam_prdgen_f45
          trigger /prod12/nam/nest_prico/post/jnam_post_f45 == complete
        task jnam_prdgen_f48
          trigger /prod12/nam/nest_prico/post/jnam_post_f48 == complete
        task jnam_prdgen_f51
          trigger /prod12/nam/nest_prico/post/jnam_post_f51 == complete
        task jnam_prdgen_f54
          trigger /prod12/nam/nest_prico/post/jnam_post_f54 == complete
        task jnam_prdgen_f57
          trigger /prod12/nam/nest_prico/post/jnam_post_f57 == complete
        task jnam_prdgen_f60
          trigger /prod12/nam/nest_prico/post/jnam_post_f60 == complete
      endfamily
    endfamily
    family nest_alaska
      edit ECF_FILES '/ecf/ecfnets/scripts/nam/nest'
      edit grid 'alaska'
      edit NTASK '8'
      family profile
        task jnam_profile_f00
          trigger /prod12/nam/nest_alaska/post/jnam_post_manager:release_post00
        task jnam_profile_f01
          trigger /prod12/nam/nest_alaska/post/jnam_post_manager:release_post01
        task jnam_profile_f02
          trigger /prod12/nam/nest_alaska/post/jnam_post_manager:release_post02
        task jnam_profile_f03
          trigger /prod12/nam/nest_alaska/post/jnam_post_manager:release_post03
        task jnam_profile_f04
          trigger /prod12/nam/nest_alaska/post/jnam_post_manager:release_post04
        task jnam_profile_f05
          trigger /prod12/nam/nest_alaska/post/jnam_post_manager:release_post05
        task jnam_profile_f06
          trigger /prod12/nam/nest_alaska/post/jnam_post_manager:release_post06
        task jnam_profile_f07
          trigger /prod12/nam/nest_alaska/post/jnam_post_manager:release_post07
        task jnam_profile_f08
          trigger /prod12/nam/nest_alaska/post/jnam_post_manager:release_post08
        task jnam_profile_f09
          trigger /prod12/nam/nest_alaska/post/jnam_post_manager:release_post09
        task jnam_profile_f10
          trigger /prod12/nam/nest_alaska/post/jnam_post_manager:release_post10
        task jnam_profile_f11
          trigger /prod12/nam/nest_alaska/post/jnam_post_manager:release_post11
        task jnam_profile_f12
          trigger /prod12/nam/nest_alaska/post/jnam_post_manager:release_post12
        task jnam_profile_f13
          trigger /prod12/nam/nest_alaska/post/jnam_post_manager:release_post13
        task jnam_profile_f14
          trigger /prod12/nam/nest_alaska/post/jnam_post_manager:release_post14
        task jnam_profile_f15
          trigger /prod12/nam/nest_alaska/post/jnam_post_manager:release_post15
        task jnam_profile_f16
          trigger /prod12/nam/nest_alaska/post/jnam_post_manager:release_post16
        task jnam_profile_f17
          trigger /prod12/nam/nest_alaska/post/jnam_post_manager:release_post17
        task jnam_profile_f18
          trigger /prod12/nam/nest_alaska/post/jnam_post_manager:release_post18
        task jnam_profile_f19
          trigger /prod12/nam/nest_alaska/post/jnam_post_manager:release_post19
        task jnam_profile_f20
          trigger /prod12/nam/nest_alaska/post/jnam_post_manager:release_post20
        task jnam_profile_f21
          trigger /prod12/nam/nest_alaska/post/jnam_post_manager:release_post21
        task jnam_profile_f22
          trigger /prod12/nam/nest_alaska/post/jnam_post_manager:release_post22
        task jnam_profile_f23
          trigger /prod12/nam/nest_alaska/post/jnam_post_manager:release_post23
        task jnam_profile_f24
          trigger /prod12/nam/nest_alaska/post/jnam_post_manager:release_post24
        task jnam_profile_f25
          trigger /prod12/nam/nest_alaska/post/jnam_post_manager:release_post25
        task jnam_profile_f26
          trigger /prod12/nam/nest_alaska/post/jnam_post_manager:release_post26
        task jnam_profile_f27
          trigger /prod12/nam/nest_alaska/post/jnam_post_manager:release_post27
        task jnam_profile_f28
          trigger /prod12/nam/nest_alaska/post/jnam_post_manager:release_post28
        task jnam_profile_f29
          trigger /prod12/nam/nest_alaska/post/jnam_post_manager:release_post29
        task jnam_profile_f30
          trigger /prod12/nam/nest_alaska/post/jnam_post_manager:release_post30
        task jnam_profile_f31
          trigger /prod12/nam/nest_alaska/post/jnam_post_manager:release_post31
        task jnam_profile_f32
          trigger /prod12/nam/nest_alaska/post/jnam_post_manager:release_post32
        task jnam_profile_f33
          trigger /prod12/nam/nest_alaska/post/jnam_post_manager:release_post33
        task jnam_profile_f34
          trigger /prod12/nam/nest_alaska/post/jnam_post_manager:release_post34
        task jnam_profile_f35
          trigger /prod12/nam/nest_alaska/post/jnam_post_manager:release_post35
        task jnam_profile_f36
          trigger /prod12/nam/nest_alaska/post/jnam_post_manager:release_post36
        task jnam_profile_f37
          trigger /prod12/nam/nest_alaska/post/jnam_post_manager:release_post37
        task jnam_profile_f38
          trigger /prod12/nam/nest_alaska/post/jnam_post_manager:release_post38
        task jnam_profile_f39
          trigger /prod12/nam/nest_alaska/post/jnam_post_manager:release_post39
        task jnam_profile_f40
          trigger /prod12/nam/nest_alaska/post/jnam_post_manager:release_post40
        task jnam_profile_f41
          trigger /prod12/nam/nest_alaska/post/jnam_post_manager:release_post41
        task jnam_profile_f42
          trigger /prod12/nam/nest_alaska/post/jnam_post_manager:release_post42
        task jnam_profile_f43
          trigger /prod12/nam/nest_alaska/post/jnam_post_manager:release_post43
        task jnam_profile_f44
          trigger /prod12/nam/nest_alaska/post/jnam_post_manager:release_post44
        task jnam_profile_f45
          trigger /prod12/nam/nest_alaska/post/jnam_post_manager:release_post45
        task jnam_profile_f46
          trigger /prod12/nam/nest_alaska/post/jnam_post_manager:release_post46
        task jnam_profile_f47
          trigger /prod12/nam/nest_alaska/post/jnam_post_manager:release_post47
        task jnam_profile_f48
          trigger /prod12/nam/nest_alaska/post/jnam_post_manager:release_post48
        task jnam_profile_f49
          trigger /prod12/nam/nest_alaska/post/jnam_post_manager:release_post49
        task jnam_profile_f50
          trigger /prod12/nam/nest_alaska/post/jnam_post_manager:release_post50
        task jnam_profile_f51
          trigger /prod12/nam/nest_alaska/post/jnam_post_manager:release_post51
        task jnam_profile_f52
          trigger /prod12/nam/nest_alaska/post/jnam_post_manager:release_post52
        task jnam_profile_f53
          trigger /prod12/nam/nest_alaska/post/jnam_post_manager:release_post53
        task jnam_profile_f54
          trigger /prod12/nam/nest_alaska/post/jnam_post_manager:release_post54
        task jnam_profile_f55
          trigger /prod12/nam/nest_alaska/post/jnam_post_manager:release_post55
        task jnam_profile_f56
          trigger /prod12/nam/nest_alaska/post/jnam_post_manager:release_post56
        task jnam_profile_f57
          trigger /prod12/nam/nest_alaska/post/jnam_post_manager:release_post57
        task jnam_profile_f58
          trigger /prod12/nam/nest_alaska/post/jnam_post_manager:release_post58
        task jnam_profile_f59
          trigger /prod12/nam/nest_alaska/post/jnam_post_manager:release_post59
        task jnam_profile_f60
          trigger /prod12/nam/nest_alaska/post/jnam_post_manager:release_post60
      endfamily
      family post
        task jnam_post_manager
          trigger /prod12/nam/forecast == active or /prod12/nam/forecast == complete
          event 1 release_post00
          event 2 release_post01
          event 3 release_post02
          event 4 release_post03
          event 5 release_post04
          event 6 release_post05
          event 7 release_post06
          event 8 release_post07
          event 9 release_post08
          event 10 release_post09
          event 11 release_post10
          event 12 release_post11
          event 13 release_post12
          event 14 release_post13
          event 15 release_post14
          event 16 release_post15
          event 17 release_post16
          event 18 release_post17
          event 19 release_post18
          event 20 release_post19
          event 21 release_post20
          event 22 release_post21
          event 23 release_post22
          event 24 release_post23
          event 25 release_post24
          event 26 release_post25
          event 27 release_post26
          event 28 release_post27
          event 29 release_post28
          event 30 release_post29
          event 31 release_post30
          event 32 release_post31
          event 33 release_post32
          event 34 release_post33
          event 35 release_post34
          event 36 release_post35
          event 37 release_post36
          event 38 release_post37
          event 39 release_post38
          event 40 release_post39
          event 41 release_post40
          event 42 release_post41
          event 43 release_post42
          event 44 release_post43
          event 45 release_post44
          event 46 release_post45
          event 47 release_post46
          event 48 release_post47
          event 49 release_post48
          event 50 release_post49
          event 51 release_post50
          event 52 release_post51
          event 53 release_post52
          event 54 release_post53
          event 55 release_post54
          event 56 release_post55
          event 57 release_post56
          event 58 release_post57
          event 59 release_post58
          event 60 release_post59
          event 61 release_post60
        task jnam_post_f00
          trigger jnam_post_manager:release_post00
        task jnam_post_f01
          trigger jnam_post_manager:release_post01
        task jnam_post_f02
          trigger jnam_post_manager:release_post02
        task jnam_post_f03
          trigger jnam_post_manager:release_post03
        task jnam_post_f04
          trigger jnam_post_manager:release_post04
        task jnam_post_f05
          trigger jnam_post_manager:release_post05
        task jnam_post_f06
          trigger jnam_post_manager:release_post06
        task jnam_post_f07
          trigger jnam_post_manager:release_post07
        task jnam_post_f08
          trigger jnam_post_manager:release_post08
        task jnam_post_f09
          trigger jnam_post_manager:release_post09
        task jnam_post_f10
          trigger jnam_post_manager:release_post10
        task jnam_post_f11
          trigger jnam_post_manager:release_post11
        task jnam_post_f12
          trigger jnam_post_manager:release_post12
        task jnam_post_f13
          trigger jnam_post_manager:release_post13
        task jnam_post_f14
          trigger jnam_post_manager:release_post14
        task jnam_post_f15
          trigger jnam_post_manager:release_post15
        task jnam_post_f16
          trigger jnam_post_manager:release_post16
        task jnam_post_f17
          trigger jnam_post_manager:release_post17
        task jnam_post_f18
          trigger jnam_post_manager:release_post18
        task jnam_post_f19
          trigger jnam_post_manager:release_post19
        task jnam_post_f20
          trigger jnam_post_manager:release_post20
        task jnam_post_f21
          trigger jnam_post_manager:release_post21
        task jnam_post_f22
          trigger jnam_post_manager:release_post22
        task jnam_post_f23
          trigger jnam_post_manager:release_post23
        task jnam_post_f24
          trigger jnam_post_manager:release_post24
        task jnam_post_f25
          trigger jnam_post_manager:release_post25
        task jnam_post_f26
          trigger jnam_post_manager:release_post26
        task jnam_post_f27
          trigger jnam_post_manager:release_post27
        task jnam_post_f28
          trigger jnam_post_manager:release_post28
        task jnam_post_f29
          trigger jnam_post_manager:release_post29
        task jnam_post_f30
          trigger jnam_post_manager:release_post30
        task jnam_post_f31
          trigger jnam_post_manager:release_post31
        task jnam_post_f32
          trigger jnam_post_manager:release_post32
        task jnam_post_f33
          trigger jnam_post_manager:release_post33
        task jnam_post_f34
          trigger jnam_post_manager:release_post34
        task jnam_post_f35
          trigger jnam_post_manager:release_post35
        task jnam_post_f36
          trigger jnam_post_manager:release_post36
        task jnam_post_f37
          trigger jnam_post_manager:release_post37
        task jnam_post_f38
          trigger jnam_post_manager:release_post38
        task jnam_post_f39
          trigger jnam_post_manager:release_post39
        task jnam_post_f40
          trigger jnam_post_manager:release_post40
        task jnam_post_f41
          trigger jnam_post_manager:release_post41
        task jnam_post_f42
          trigger jnam_post_manager:release_post42
        task jnam_post_f43
          trigger jnam_post_manager:release_post43
        task jnam_post_f44
          trigger jnam_post_manager:release_post44
        task jnam_post_f45
          trigger jnam_post_manager:release_post45
        task jnam_post_f46
          trigger jnam_post_manager:release_post46
        task jnam_post_f47
          trigger jnam_post_manager:release_post47
        task jnam_post_f48
          trigger jnam_post_manager:release_post48
        task jnam_post_f49
          trigger jnam_post_manager:release_post49
        task jnam_post_f50
          trigger jnam_post_manager:release_post50
        task jnam_post_f51
          trigger jnam_post_manager:release_post51
        task jnam_post_f52
          trigger jnam_post_manager:release_post52
        task jnam_post_f53
          trigger jnam_post_manager:release_post53
        task jnam_post_f54
          trigger jnam_post_manager:release_post54
        task jnam_post_f55
          trigger jnam_post_manager:release_post55
        task jnam_post_f56
          trigger jnam_post_manager:release_post56
        task jnam_post_f57
          trigger jnam_post_manager:release_post57
        task jnam_post_f58
          trigger jnam_post_manager:release_post58
        task jnam_post_f59
          trigger jnam_post_manager:release_post59
        task jnam_post_f60
          trigger jnam_post_manager:release_post60
      endfamily
      family prdgen
        task jnam_prdgen_f00
          trigger /prod12/nam/nest_alaska/post/jnam_post_f00 == complete
          event 1 post_complete
        task jnam_prdgen_f01
          trigger /prod12/nam/nest_alaska/post/jnam_post_f01 == complete
          event 1 post_complete
        task jnam_prdgen_f02
          trigger /prod12/nam/nest_alaska/post/jnam_post_f02 == complete
          event 1 post_complete
        task jnam_prdgen_f03
          trigger /prod12/nam/nest_alaska/post/jnam_post_f03 == complete
          event 1 post_complete
        task jnam_prdgen_f04
          trigger /prod12/nam/nest_alaska/post/jnam_post_f04 == complete
          event 1 post_complete
        task jnam_prdgen_f05
          trigger /prod12/nam/nest_alaska/post/jnam_post_f05 == complete
          event 1 post_complete
        task jnam_prdgen_f06
          trigger /prod12/nam/nest_alaska/post/jnam_post_f06 == complete
          event 1 post_complete
        task jnam_prdgen_f07
          trigger /prod12/nam/nest_alaska/post/jnam_post_f07 == complete
          event 1 post_complete
        task jnam_prdgen_f08
          trigger /prod12/nam/nest_alaska/post/jnam_post_f08 == complete
          event 1 post_complete
        task jnam_prdgen_f09
          trigger /prod12/nam/nest_alaska/post/jnam_post_f09 == complete
          event 1 post_complete
        task jnam_prdgen_f10
          trigger /prod12/nam/nest_alaska/post/jnam_post_f10 == complete
          event 1 post_complete
        task jnam_prdgen_f11
          trigger /prod12/nam/nest_alaska/post/jnam_post_f11 == complete
          event 1 post_complete
        task jnam_prdgen_f12
          trigger /prod12/nam/nest_alaska/post/jnam_post_f12 == complete
          event 1 post_complete
        task jnam_prdgen_f13
          trigger /prod12/nam/nest_alaska/post/jnam_post_f13 == complete
          event 1 post_complete
        task jnam_prdgen_f14
          trigger /prod12/nam/nest_alaska/post/jnam_post_f14 == complete
          event 1 post_complete
        task jnam_prdgen_f15
          trigger /prod12/nam/nest_alaska/post/jnam_post_f15 == complete
          event 1 post_complete
        task jnam_prdgen_f16
          trigger /prod12/nam/nest_alaska/post/jnam_post_f16 == complete
          event 1 post_complete
        task jnam_prdgen_f17
          trigger /prod12/nam/nest_alaska/post/jnam_post_f17 == complete
        task jnam_prdgen_f18
          trigger /prod12/nam/nest_alaska/post/jnam_post_f18 == complete
          event 1 post_complete
        task jnam_prdgen_f19
          trigger /prod12/nam/nest_alaska/post/jnam_post_f19 == complete
          event 1 post_complete
        task jnam_prdgen_f20
          trigger /prod12/nam/nest_alaska/post/jnam_post_f20 == complete
          event 1 post_complete
        task jnam_prdgen_f21
          trigger /prod12/nam/nest_alaska/post/jnam_post_f21 == complete
          event 1 post_complete
        task jnam_prdgen_f22
          trigger /prod12/nam/nest_alaska/post/jnam_post_f22 == complete
          event 1 post_complete
        task jnam_prdgen_f23
          trigger /prod12/nam/nest_alaska/post/jnam_post_f23 == complete
          event 1 post_complete
        task jnam_prdgen_f24
          trigger /prod12/nam/nest_alaska/post/jnam_post_f24 == complete
          event 1 post_complete
        task jnam_prdgen_f25
          trigger /prod12/nam/nest_alaska/post/jnam_post_f25 == complete
          event 1 post_complete
        task jnam_prdgen_f26
          trigger /prod12/nam/nest_alaska/post/jnam_post_f26 == complete
          event 1 post_complete
        task jnam_prdgen_f27
          trigger /prod12/nam/nest_alaska/post/jnam_post_f27 == complete
          event 1 post_complete
        task jnam_prdgen_f28
          trigger /prod12/nam/nest_alaska/post/jnam_post_f28 == complete
          event 1 post_complete
        task jnam_prdgen_f29
          trigger /prod12/nam/nest_alaska/post/jnam_post_f29 == complete
          event 1 post_complete
        task jnam_prdgen_f30
          trigger /prod12/nam/nest_alaska/post/jnam_post_f30 == complete
          event 1 post_complete
        task jnam_prdgen_f31
          trigger /prod12/nam/nest_alaska/post/jnam_post_f31 == complete
          event 1 post_complete
        task jnam_prdgen_f32
          trigger /prod12/nam/nest_alaska/post/jnam_post_f32 == complete
          event 1 post_complete
        task jnam_prdgen_f33
          trigger /prod12/nam/nest_alaska/post/jnam_post_f33 == complete
        task jnam_prdgen_f34
          trigger /prod12/nam/nest_alaska/post/jnam_post_f34 == complete
          event 1 post_complete
        task jnam_prdgen_f35
          trigger /prod12/nam/nest_alaska/post/jnam_post_f35 == complete
          event 1 post_complete
        task jnam_prdgen_f36
          trigger /prod12/nam/nest_alaska/post/jnam_post_f36 == complete
          event 1 post_complete
        task jnam_prdgen_f39
          trigger /prod12/nam/nest_alaska/post/jnam_post_f39 == complete
          event 1 post_complete
        task jnam_prdgen_f42
          trigger /prod12/nam/nest_alaska/post/jnam_post_f42 == complete
          event 1 post_complete
        task jnam_prdgen_f45
          trigger /prod12/nam/nest_alaska/post/jnam_post_f45 == complete
          event 1 post_complete
        task jnam_prdgen_f48
          trigger /prod12/nam/nest_alaska/post/jnam_post_f48 == complete
          event 1 post_complete
        task jnam_prdgen_f51
          trigger /prod12/nam/nest_alaska/post/jnam_post_f51 == complete
          event 1 post_complete
        task jnam_prdgen_f54
          trigger /prod12/nam/nest_alaska/post/jnam_post_f54 == complete
          event 1 post_complete
        task jnam_prdgen_f57
          trigger /prod12/nam/nest_alaska/post/jnam_post_f57 == complete
          event 1 post_complete
        task jnam_prdgen_f60
          trigger /prod12/nam/nest_alaska/post/jnam_post_f60 == complete
          event 1 post_complete
      endfamily
    endfamily
    family profile
      task jnam_profile_f00
        trigger /prod12/nam/post/jnam_post_manager:release_post00
      task jnam_profile_f01
        trigger /prod12/nam/post/jnam_post_manager:release_post01
      task jnam_profile_f02
        trigger /prod12/nam/post/jnam_post_manager:release_post02
      task jnam_profile_f03
        trigger /prod12/nam/post/jnam_post_manager:release_post03
      task jnam_profile_f04
        trigger /prod12/nam/post/jnam_post_manager:release_post04
      task jnam_profile_f05
        trigger /prod12/nam/post/jnam_post_manager:release_post05
      task jnam_profile_f06
        trigger /prod12/nam/post/jnam_post_manager:release_post06
      task jnam_profile_f07
        trigger /prod12/nam/post/jnam_post_manager:release_post07
      task jnam_profile_f08
        trigger /prod12/nam/post/jnam_post_manager:release_post08
      task jnam_profile_f09
        trigger /prod12/nam/post/jnam_post_manager:release_post09
      task jnam_profile_f10
        trigger /prod12/nam/post/jnam_post_manager:release_post10
      task jnam_profile_f11
        trigger /prod12/nam/post/jnam_post_manager:release_post11
      task jnam_profile_f12
        trigger /prod12/nam/post/jnam_post_manager:release_post12
      task jnam_profile_f13
        trigger /prod12/nam/post/jnam_post_manager:release_post13
      task jnam_profile_f14
        trigger /prod12/nam/post/jnam_post_manager:release_post14
      task jnam_profile_f15
        trigger /prod12/nam/post/jnam_post_manager:release_post15
      task jnam_profile_f16
        trigger /prod12/nam/post/jnam_post_manager:release_post16
      task jnam_profile_f17
        trigger /prod12/nam/post/jnam_post_manager:release_post17
      task jnam_profile_f18
        trigger /prod12/nam/post/jnam_post_manager:release_post18
      task jnam_profile_f19
        trigger /prod12/nam/post/jnam_post_manager:release_post19
      task jnam_profile_f20
        trigger /prod12/nam/post/jnam_post_manager:release_post20
      task jnam_profile_f21
        trigger /prod12/nam/post/jnam_post_manager:release_post21
      task jnam_profile_f22
        trigger /prod12/nam/post/jnam_post_manager:release_post22
      task jnam_profile_f23
        trigger /prod12/nam/post/jnam_post_manager:release_post23
      task jnam_profile_f24
        trigger /prod12/nam/post/jnam_post_manager:release_post24
      task jnam_profile_f25
        trigger /prod12/nam/post/jnam_post_manager:release_post25
      task jnam_profile_f26
        trigger /prod12/nam/post/jnam_post_manager:release_post26
      task jnam_profile_f27
        trigger /prod12/nam/post/jnam_post_manager:release_post27
      task jnam_profile_f28
        trigger /prod12/nam/post/jnam_post_manager:release_post28
      task jnam_profile_f29
        trigger /prod12/nam/post/jnam_post_manager:release_post29
      task jnam_profile_f30
        trigger /prod12/nam/post/jnam_post_manager:release_post30
      task jnam_profile_f31
        trigger /prod12/nam/post/jnam_post_manager:release_post31
      task jnam_profile_f32
        trigger /prod12/nam/post/jnam_post_manager:release_post32
      task jnam_profile_f33
        trigger /prod12/nam/post/jnam_post_manager:release_post33
      task jnam_profile_f34
        trigger /prod12/nam/post/jnam_post_manager:release_post34
      task jnam_profile_f35
        trigger /prod12/nam/post/jnam_post_manager:release_post35
      task jnam_profile_f36
        trigger /prod12/nam/post/jnam_post_manager:release_post36
      task jnam_profile_f37
        trigger /prod12/nam/post/jnam_post_manager:release_post37
      task jnam_profile_f38
        trigger /prod12/nam/post/jnam_post_manager:release_post38
      task jnam_profile_f39
        trigger /prod12/nam/post/jnam_post_manager:release_post39
      task jnam_profile_f40
        trigger /prod12/nam/post/jnam_post_manager:release_post40
      task jnam_profile_f41
        trigger /prod12/nam/post/jnam_post_manager:release_post41
      task jnam_profile_f42
        trigger /prod12/nam/post/jnam_post_manager:release_post42
      task jnam_profile_f43
        trigger /prod12/nam/post/jnam_post_manager:release_post43
      task jnam_profile_f44
        trigger /prod12/nam/post/jnam_post_manager:release_post44
      task jnam_profile_f45
        trigger /prod12/nam/post/jnam_post_manager:release_post45
      task jnam_profile_f46
        trigger /prod12/nam/post/jnam_post_manager:release_post46
      task jnam_profile_f47
        trigger /prod12/nam/post/jnam_post_manager:release_post47
      task jnam_profile_f48
        trigger /prod12/nam/post/jnam_post_manager:release_post48
      task jnam_profile_f49
        trigger /prod12/nam/post/jnam_post_manager:release_post49
      task jnam_profile_f50
        trigger /prod12/nam/post/jnam_post_manager:release_post50
      task jnam_profile_f51
        trigger /prod12/nam/post/jnam_post_manager:release_post51
      task jnam_profile_f52
        trigger /prod12/nam/post/jnam_post_manager:release_post52
      task jnam_profile_f53
        trigger /prod12/nam/post/jnam_post_manager:release_post53
      task jnam_profile_f54
        trigger /prod12/nam/post/jnam_post_manager:release_post54
      task jnam_profile_f55
        trigger /prod12/nam/post/jnam_post_manager:release_post55
      task jnam_profile_f56
        trigger /prod12/nam/post/jnam_post_manager:release_post56
      task jnam_profile_f57
        trigger /prod12/nam/post/jnam_post_manager:release_post57
      task jnam_profile_f58
        trigger /prod12/nam/post/jnam_post_manager:release_post58
      task jnam_profile_f59
        trigger /prod12/nam/post/jnam_post_manager:release_post59
      task jnam_profile_f60
        trigger /prod12/nam/post/jnam_post_manager:release_post60
      task jnam_profile_f61
        trigger /prod12/nam/post/jnam_post_manager:release_post61
      task jnam_profile_f62
        trigger /prod12/nam/post/jnam_post_manager:release_post62
      task jnam_profile_f63
        trigger /prod12/nam/post/jnam_post_manager:release_post63
      task jnam_profile_f64
        trigger /prod12/nam/post/jnam_post_manager:release_post64
      task jnam_profile_f65
        trigger /prod12/nam/post/jnam_post_manager:release_post65
      task jnam_profile_f66
        trigger /prod12/nam/post/jnam_post_manager:release_post66
      task jnam_profile_f67
        trigger /prod12/nam/post/jnam_post_manager:release_post67
      task jnam_profile_f68
        trigger /prod12/nam/post/jnam_post_manager:release_post68
      task jnam_profile_f69
        trigger /prod12/nam/post/jnam_post_manager:release_post69
      task jnam_profile_f70
        trigger /prod12/nam/post/jnam_post_manager:release_post70
      task jnam_profile_f71
        trigger /prod12/nam/post/jnam_post_manager:release_post71
      task jnam_profile_f72
        trigger /prod12/nam/post/jnam_post_manager:release_post72
      task jnam_profile_f73
        trigger /prod12/nam/post/jnam_post_manager:release_post73
      task jnam_profile_f74
        trigger /prod12/nam/post/jnam_post_manager:release_post74
      task jnam_profile_f75
        trigger /prod12/nam/post/jnam_post_manager:release_post75
      task jnam_profile_f76
        trigger /prod12/nam/post/jnam_post_manager:release_post76
      task jnam_profile_f77
        trigger /prod12/nam/post/jnam_post_manager:release_post77
      task jnam_profile_f78
        trigger /prod12/nam/post/jnam_post_manager:release_post78
      task jnam_profile_f79
        trigger /prod12/nam/post/jnam_post_manager:release_post79
      task jnam_profile_f80
        trigger /prod12/nam/post/jnam_post_manager:release_post80
      task jnam_profile_f81
        trigger /prod12/nam/post/jnam_post_manager:release_post81
      task jnam_profile_f82
        trigger /prod12/nam/post/jnam_post_manager:release_post82
      task jnam_profile_f83
        trigger /prod12/nam/post/jnam_post_manager:release_post83
      task jnam_profile_f84
        trigger /prod12/nam/post/jnam_post_manager:release_post84
    endfamily
    family prdgen
      task jnam_prdgen_f00
        trigger /prod12/nam/post/jnam_post_f00 == complete
        event 1 post_1_complete
        event 2 post_2_complete
        event 3 post_3_complete
        event 4 post_4_complete
        event 5 post_5_complete
        event 6 post_6_complete
        event 7 post_7_complete
        event 8 post_8_complete
      task jnam_prdgen_f01
        trigger /prod12/nam/post/jnam_post_f01 == complete
        event 1 post_1_complete
        event 2 post_2_complete
        event 3 post_3_complete
        event 4 post_4_complete
        event 5 post_5_complete
        event 6 post_6_complete
        event 7 post_7_complete
        event 8 post_8_complete
      task jnam_prdgen_f02
        trigger /prod12/nam/post/jnam_post_f02 == complete
        event 1 post_1_complete
        event 2 post_2_complete
        event 3 post_3_complete
        event 4 post_4_complete
        event 5 post_5_complete
        event 6 post_6_complete
        event 7 post_7_complete
        event 8 post_8_complete
      task jnam_prdgen_f03
        trigger /prod12/nam/post/jnam_post_f03 == complete
        event 1 post_1_complete
        event 2 post_2_complete
        event 3 post_3_complete
        event 4 post_4_complete
        event 5 post_5_complete
        event 6 post_6_complete
        event 7 post_7_complete
        event 8 post_8_complete
      task jnam_prdgen_f04
        trigger /prod12/nam/post/jnam_post_f04 == complete
        event 1 post_1_complete
        event 2 post_2_complete
        event 3 post_3_complete
        event 4 post_4_complete
        event 5 post_5_complete
        event 6 post_6_complete
        event 7 post_7_complete
        event 8 post_8_complete
      task jnam_prdgen_f05
        trigger /prod12/nam/post/jnam_post_f05 == complete
        event 1 post_1_complete
        event 2 post_2_complete
        event 3 post_3_complete
        event 4 post_4_complete
        event 5 post_5_complete
        event 6 post_6_complete
        event 7 post_7_complete
        event 8 post_8_complete
      task jnam_prdgen_f06
        trigger /prod12/nam/post/jnam_post_f06 == complete
        event 1 post_1_complete
        event 2 post_2_complete
        event 3 post_3_complete
        event 4 post_4_complete
        event 5 post_5_complete
        event 6 post_6_complete
        event 7 post_7_complete
        event 8 post_8_complete
      task jnam_prdgen_f07
        trigger /prod12/nam/post/jnam_post_f07 == complete
        event 1 post_1_complete
        event 2 post_2_complete
        event 3 post_3_complete
        event 4 post_4_complete
        event 5 post_5_complete
        event 6 post_6_complete
        event 7 post_7_complete
        event 8 post_8_complete
      task jnam_prdgen_f08
        trigger /prod12/nam/post/jnam_post_f08 == complete
        event 1 post_1_complete
        event 2 post_2_complete
        event 3 post_3_complete
        event 4 post_4_complete
        event 5 post_5_complete
        event 6 post_6_complete
        event 7 post_7_complete
        event 8 post_8_complete
      task jnam_prdgen_f09
        trigger /prod12/nam/post/jnam_post_f09 == complete
        event 1 post_1_complete
        event 2 post_2_complete
        event 3 post_3_complete
        event 4 post_4_complete
        event 5 post_5_complete
        event 6 post_6_complete
        event 7 post_7_complete
        event 8 post_8_complete
      task jnam_prdgen_f10
        trigger /prod12/nam/post/jnam_post_f10 == complete
        event 1 post_1_complete
        event 2 post_2_complete
        event 3 post_3_complete
        event 4 post_4_complete
        event 5 post_5_complete
        event 6 post_6_complete
        event 7 post_7_complete
        event 8 post_8_complete
      task jnam_prdgen_f11
        trigger /prod12/nam/post/jnam_post_f11 == complete
        event 1 post_1_complete
        event 2 post_2_complete
        event 3 post_3_complete
        event 4 post_4_complete
        event 5 post_5_complete
        event 6 post_6_complete
        event 7 post_7_complete
        event 8 post_8_complete
      task jnam_prdgen_f12
        trigger /prod12/nam/post/jnam_post_f12 == complete
        event 1 post_1_complete
        event 2 post_2_complete
        event 3 post_3_complete
        event 4 post_4_complete
        event 5 post_5_complete
        event 6 post_6_complete
        event 7 post_7_complete
        event 8 post_8_complete
      task jnam_prdgen_f13
        trigger /prod12/nam/post/jnam_post_f13 == complete
        event 1 post_1_complete
        event 2 post_2_complete
        event 3 post_3_complete
        event 4 post_4_complete
        event 5 post_5_complete
        event 6 post_6_complete
        event 7 post_7_complete
        event 8 post_8_complete
      task jnam_prdgen_f14
        trigger /prod12/nam/post/jnam_post_f14 == complete
        event 1 post_1_complete
        event 2 post_2_complete
        event 3 post_3_complete
        event 4 post_4_complete
        event 5 post_5_complete
        event 6 post_6_complete
        event 7 post_7_complete
        event 8 post_8_complete
      task jnam_prdgen_f15
        trigger /prod12/nam/post/jnam_post_f15 == complete
        event 1 post_1_complete
        event 2 post_2_complete
        event 3 post_3_complete
        event 4 post_4_complete
        event 5 post_5_complete
        event 6 post_6_complete
        event 7 post_7_complete
        event 8 post_8_complete
      task jnam_prdgen_f16
        trigger /prod12/nam/post/jnam_post_f16 == complete
        event 1 post_1_complete
        event 2 post_2_complete
        event 3 post_3_complete
        event 4 post_4_complete
        event 5 post_5_complete
        event 6 post_6_complete
        event 7 post_7_complete
        event 8 post_8_complete
      task jnam_prdgen_f17
        trigger /prod12/nam/post/jnam_post_f17 == complete
        event 1 post_1_complete
        event 2 post_2_complete
        event 3 post_3_complete
        event 4 post_4_complete
        event 5 post_5_complete
        event 6 post_6_complete
        event 7 post_7_complete
        event 8 post_8_complete
      task jnam_prdgen_f18
        trigger /prod12/nam/post/jnam_post_f18 == complete
        event 1 post_1_complete
        event 2 post_2_complete
        event 3 post_3_complete
        event 4 post_4_complete
        event 5 post_5_complete
        event 6 post_6_complete
        event 7 post_7_complete
        event 8 post_8_complete
      task jnam_prdgen_f19
        trigger /prod12/nam/post/jnam_post_f19 == complete
        event 1 post_1_complete
        event 2 post_2_complete
        event 3 post_3_complete
        event 4 post_4_complete
        event 5 post_5_complete
        event 6 post_6_complete
        event 7 post_7_complete
        event 8 post_8_complete
      task jnam_prdgen_f20
        trigger /prod12/nam/post/jnam_post_f20 == complete
        event 1 post_1_complete
        event 2 post_2_complete
        event 3 post_3_complete
        event 4 post_4_complete
        event 5 post_5_complete
        event 6 post_6_complete
        event 7 post_7_complete
        event 8 post_8_complete
      task jnam_prdgen_f21
        trigger /prod12/nam/post/jnam_post_f21 == complete
        event 1 post_1_complete
        event 2 post_2_complete
        event 3 post_3_complete
        event 4 post_4_complete
        event 5 post_5_complete
        event 6 post_6_complete
        event 7 post_7_complete
        event 8 post_8_complete
      task jnam_prdgen_f22
        trigger /prod12/nam/post/jnam_post_f22 == complete
        event 1 post_1_complete
        event 2 post_2_complete
        event 3 post_3_complete
        event 4 post_4_complete
        event 5 post_5_complete
        event 6 post_6_complete
        event 7 post_7_complete
        event 8 post_8_complete
      task jnam_prdgen_f23
        trigger /prod12/nam/post/jnam_post_f23 == complete
        event 1 post_1_complete
        event 2 post_2_complete
        event 3 post_3_complete
        event 4 post_4_complete
        event 5 post_5_complete
        event 6 post_6_complete
        event 7 post_7_complete
        event 8 post_8_complete
      task jnam_prdgen_f24
        trigger /prod12/nam/post/jnam_post_f24 == complete
        event 1 post_1_complete
        event 2 post_2_complete
        event 3 post_3_complete
        event 4 post_4_complete
        event 5 post_5_complete
        event 6 post_6_complete
        event 7 post_7_complete
        event 8 post_8_complete
      task jnam_prdgen_f25
        trigger /prod12/nam/post/jnam_post_f25 == complete
        event 1 post_1_complete
        event 2 post_2_complete
        event 3 post_3_complete
        event 4 post_4_complete
        event 5 post_5_complete
        event 6 post_6_complete
        event 7 post_7_complete
        event 8 post_8_complete
      task jnam_prdgen_f26
        trigger /prod12/nam/post/jnam_post_f26 == complete
        event 1 post_1_complete
        event 2 post_2_complete
        event 3 post_3_complete
        event 4 post_4_complete
        event 5 post_5_complete
        event 6 post_6_complete
        event 7 post_7_complete
        event 8 post_8_complete
      task jnam_prdgen_f27
        trigger /prod12/nam/post/jnam_post_f27 == complete
        event 1 post_1_complete
        event 2 post_2_complete
        event 3 post_3_complete
        event 4 post_4_complete
        event 5 post_5_complete
        event 6 post_6_complete
        event 7 post_7_complete
        event 8 post_8_complete
      task jnam_prdgen_f28
        trigger /prod12/nam/post/jnam_post_f28 == complete
        event 1 post_1_complete
        event 2 post_2_complete
        event 3 post_3_complete
        event 4 post_4_complete
        event 5 post_5_complete
        event 6 post_6_complete
        event 7 post_7_complete
        event 8 post_8_complete
      task jnam_prdgen_f29
        trigger /prod12/nam/post/jnam_post_f29 == complete
        event 1 post_1_complete
        event 2 post_2_complete
        event 3 post_3_complete
        event 4 post_4_complete
        event 5 post_5_complete
        event 6 post_6_complete
        event 7 post_7_complete
        event 8 post_8_complete
      task jnam_prdgen_f30
        trigger /prod12/nam/post/jnam_post_f30 == complete
        event 1 post_1_complete
        event 2 post_2_complete
        event 3 post_3_complete
        event 4 post_4_complete
        event 5 post_5_complete
        event 6 post_6_complete
        event 7 post_7_complete
        event 8 post_8_complete
      task jnam_prdgen_f31
        trigger /prod12/nam/post/jnam_post_f31 == complete
        event 1 post_1_complete
        event 2 post_2_complete
        event 3 post_3_complete
        event 4 post_4_complete
        event 5 post_5_complete
        event 6 post_6_complete
        event 7 post_7_complete
        event 8 post_8_complete
      task jnam_prdgen_f32
        trigger /prod12/nam/post/jnam_post_f32 == complete
        event 1 post_1_complete
        event 2 post_2_complete
        event 3 post_3_complete
        event 4 post_4_complete
        event 5 post_5_complete
        event 6 post_6_complete
        event 7 post_7_complete
        event 8 post_8_complete
      task jnam_prdgen_f33
        trigger /prod12/nam/post/jnam_post_f33 == complete
        event 1 post_1_complete
        event 2 post_2_complete
        event 3 post_3_complete
        event 4 post_4_complete
        event 5 post_5_complete
        event 6 post_6_complete
        event 7 post_7_complete
        event 8 post_8_complete
      task jnam_prdgen_f34
        trigger /prod12/nam/post/jnam_post_f34 == complete
        event 1 post_1_complete
        event 2 post_2_complete
        event 3 post_3_complete
        event 4 post_4_complete
        event 5 post_5_complete
        event 6 post_6_complete
        event 7 post_7_complete
        event 8 post_8_complete
      task jnam_prdgen_f35
        trigger /prod12/nam/post/jnam_post_f35 == complete
        event 1 post_1_complete
        event 2 post_2_complete
        event 3 post_3_complete
        event 4 post_4_complete
        event 5 post_5_complete
        event 6 post_6_complete
        event 7 post_7_complete
        event 8 post_8_complete
      task jnam_prdgen_f36
        trigger /prod12/nam/post/jnam_post_f36 == complete
        event 1 post_1_complete
        event 2 post_2_complete
        event 3 post_3_complete
        event 4 post_4_complete
        event 5 post_5_complete
        event 6 post_6_complete
        event 7 post_7_complete
        event 8 post_8_complete
      task jnam_prdgen_f39
        trigger /prod12/nam/post/jnam_post_f39 == complete
        event 1 post_1_complete
        event 2 post_2_complete
        event 3 post_3_complete
        event 4 post_4_complete
        event 5 post_5_complete
        event 6 post_6_complete
        event 7 post_7_complete
        event 8 post_8_complete
      task jnam_prdgen_f42
        trigger /prod12/nam/post/jnam_post_f42 == complete
        event 1 post_1_complete
        event 2 post_2_complete
        event 3 post_3_complete
        event 4 post_4_complete
        event 5 post_5_complete
        event 6 post_6_complete
        event 7 post_7_complete
        event 8 post_8_complete
      task jnam_prdgen_f45
        trigger /prod12/nam/post/jnam_post_f45 == complete
        event 1 post_1_complete
        event 2 post_2_complete
        event 3 post_3_complete
        event 4 post_4_complete
        event 5 post_5_complete
        event 6 post_6_complete
        event 7 post_7_complete
        event 8 post_8_complete
      task jnam_prdgen_f48
        trigger /prod12/nam/post/jnam_post_f48 == complete
        event 1 post_1_complete
        event 2 post_2_complete
        event 3 post_3_complete
        event 4 post_4_complete
        event 5 post_5_complete
        event 6 post_6_complete
        event 7 post_7_complete
        event 8 post_8_complete
      task jnam_prdgen_f51
        trigger /prod12/nam/post/jnam_post_f51 == complete
        event 1 post_1_complete
        event 2 post_2_complete
        event 3 post_3_complete
        event 4 post_4_complete
        event 5 post_5_complete
        event 6 post_6_complete
        event 7 post_7_complete
        event 8 post_8_complete
      task jnam_prdgen_f54
        trigger /prod12/nam/post/jnam_post_f54 == complete
        event 1 post_1_complete
        event 2 post_2_complete
        event 3 post_3_complete
        event 4 post_4_complete
        event 5 post_5_complete
        event 6 post_6_complete
        event 7 post_7_complete
        event 8 post_8_complete
      task jnam_prdgen_f57
        trigger /prod12/nam/post/jnam_post_f57 == complete
        event 1 post_1_complete
        event 2 post_2_complete
        event 3 post_3_complete
        event 4 post_4_complete
        event 5 post_5_complete
        event 6 post_6_complete
        event 7 post_7_complete
        event 8 post_8_complete
      task jnam_prdgen_f60
        trigger /prod12/nam/post/jnam_post_f60 == complete
        event 1 post_1_complete
        event 2 post_2_complete
        event 3 post_3_complete
        event 4 post_4_complete
        event 5 post_5_complete
        event 6 post_6_complete
        event 7 post_7_complete
        event 8 post_8_complete
      task jnam_prdgen_f63
        trigger /prod12/nam/post/jnam_post_f63 == complete
        event 1 post_1_complete
        event 2 post_2_complete
        event 3 post_3_complete
        event 4 post_4_complete
        event 5 post_5_complete
        event 6 post_6_complete
        event 7 post_7_complete
        event 8 post_8_complete
      task jnam_prdgen_f66
        trigger /prod12/nam/post/jnam_post_f66 == complete
        event 1 post_1_complete
        event 2 post_2_complete
        event 3 post_3_complete
        event 4 post_4_complete
        event 5 post_5_complete
        event 6 post_6_complete
        event 7 post_7_complete
        event 8 post_8_complete
      task jnam_prdgen_f69
        trigger /prod12/nam/post/jnam_post_f69 == complete
        event 1 post_1_complete
        event 2 post_2_complete
        event 3 post_3_complete
        event 4 post_4_complete
        event 5 post_5_complete
        event 6 post_6_complete
        event 7 post_7_complete
        event 8 post_8_complete
      task jnam_prdgen_f72
        trigger /prod12/nam/post/jnam_post_f72 == complete
        event 1 post_1_complete
        event 2 post_2_complete
        event 3 post_3_complete
        event 4 post_4_complete
        event 5 post_5_complete
        event 6 post_6_complete
        event 7 post_7_complete
        event 8 post_8_complete
      task jnam_prdgen_f75
        trigger /prod12/nam/post/jnam_post_f75 == complete
        event 1 post_1_complete
        event 2 post_2_complete
        event 3 post_3_complete
        event 4 post_4_complete
        event 5 post_5_complete
        event 6 post_6_complete
        event 7 post_7_complete
        event 8 post_8_complete
      task jnam_prdgen_f78
        trigger /prod12/nam/post/jnam_post_f78 == complete
        event 1 post_1_complete
        event 2 post_2_complete
        event 3 post_3_complete
        event 4 post_4_complete
        event 5 post_5_complete
        event 6 post_6_complete
        event 7 post_7_complete
        event 8 post_8_complete
      task jnam_prdgen_f81
        trigger /prod12/nam/post/jnam_post_f81 == complete
        event 1 post_1_complete
        event 2 post_2_complete
        event 3 post_3_complete
        event 4 post_4_complete
        event 5 post_5_complete
        event 6 post_6_complete
        event 7 post_7_complete
        event 8 post_8_complete
      task jnam_prdgen_f84
        trigger /prod12/nam/post/jnam_post_f84 == complete
        event 1 post_1_complete
        event 2 post_2_complete
        event 3 post_3_complete
        event 4 post_4_complete
        event 5 post_5_complete
        event 6 post_6_complete
        event 7 post_7_complete
        event 8 post_8_complete
    endfamily
    family post
      task jnam_post_manager
        trigger /prod12/nam/analysis == complete and /prod12/nam/forecast == active
        event 1 release_post00
        event 2 release_post01
        event 3 release_post02
        event 4 release_post03
        event 5 release_post04
        event 6 release_post05
        event 7 release_post06
        event 8 release_post07
        event 9 release_post08
        event 10 release_post09
        event 11 release_post10
        event 12 release_post11
        event 13 release_post12
        event 14 release_post13
        event 15 release_post14
        event 16 release_post15
        event 17 release_post16
        event 18 release_post17
        event 19 release_post18
        event 20 release_post19
        event 21 release_post20
        event 22 release_post21
        event 23 release_post22
        event 24 release_post23
        event 25 release_post24
        event 26 release_post25
        event 27 release_post26
        event 28 release_post27
        event 29 release_post28
        event 30 release_post29
        event 31 release_post30
        event 32 release_post31
        event 33 release_post32
        event 34 release_post33
        event 35 release_post34
        event 36 release_post35
        event 37 release_post36
        event 38 release_post37
        event 39 release_post38
        event 40 release_post39
        event 41 release_post40
        event 42 release_post41
        event 43 release_post42
        event 44 release_post43
        event 45 release_post44
        event 46 release_post45
        event 47 release_post46
        event 48 release_post47
        event 49 release_post48
        event 50 release_post49
        event 51 release_post50
        event 52 release_post51
        event 53 release_post52
        event 54 release_post53
        event 55 release_post54
        event 56 release_post55
        event 57 release_post56
        event 58 release_post57
        event 59 release_post58
        event 60 release_post59
        event 61 release_post60
        event 62 release_post61
        event 63 release_post62
        event 64 release_post63
        event 65 release_post64
        event 66 release_post65
        event 67 release_post66
        event 68 release_post67
        event 69 release_post68
        event 70 release_post69
        event 71 release_post70
        event 72 release_post71
        event 73 release_post72
        event 74 release_post73
        event 75 release_post74
        event 76 release_post75
        event 77 release_post76
        event 78 release_post77
        event 79 release_post78
        event 80 release_post79
        event 81 release_post80
        event 82 release_post81
        event 83 release_post82
        event 84 release_post83
        event 85 release_post84
      task jnam_post_f00
        trigger /prod12/nam/post/jnam_post_manager:release_post00
      task jnam_post_f01
        trigger /prod12/nam/post/jnam_post_manager:release_post01
      task jnam_post_f02
        trigger /prod12/nam/post/jnam_post_manager:release_post02
      task jnam_post_f03
        trigger /prod12/nam/post/jnam_post_manager:release_post03
      task jnam_post_f04
        trigger /prod12/nam/post/jnam_post_manager:release_post04
      task jnam_post_f05
        trigger /prod12/nam/post/jnam_post_manager:release_post05
      task jnam_post_f06
        trigger /prod12/nam/post/jnam_post_manager:release_post06
      task jnam_post_f07
        trigger /prod12/nam/post/jnam_post_manager:release_post07
      task jnam_post_f08
        trigger /prod12/nam/post/jnam_post_manager:release_post08
      task jnam_post_f09
        trigger /prod12/nam/post/jnam_post_manager:release_post09
      task jnam_post_f10
        trigger /prod12/nam/post/jnam_post_manager:release_post10
      task jnam_post_f11
        trigger /prod12/nam/post/jnam_post_manager:release_post11
      task jnam_post_f12
        trigger /prod12/nam/post/jnam_post_manager:release_post12
      task jnam_post_f13
        trigger /prod12/nam/post/jnam_post_manager:release_post13
      task jnam_post_f14
        trigger /prod12/nam/post/jnam_post_manager:release_post14
      task jnam_post_f15
        trigger /prod12/nam/post/jnam_post_manager:release_post15
      task jnam_post_f16
        trigger /prod12/nam/post/jnam_post_manager:release_post16
      task jnam_post_f17
        trigger /prod12/nam/post/jnam_post_manager:release_post17
      task jnam_post_f18
        trigger /prod12/nam/post/jnam_post_manager:release_post18
      task jnam_post_f19
        trigger /prod12/nam/post/jnam_post_manager:release_post19
      task jnam_post_f20
        trigger /prod12/nam/post/jnam_post_manager:release_post20
      task jnam_post_f21
        trigger /prod12/nam/post/jnam_post_manager:release_post21
      task jnam_post_f22
        trigger /prod12/nam/post/jnam_post_manager:release_post22
      task jnam_post_f23
        trigger /prod12/nam/post/jnam_post_manager:release_post23
      task jnam_post_f24
        trigger /prod12/nam/post/jnam_post_manager:release_post24
      task jnam_post_f25
        trigger /prod12/nam/post/jnam_post_manager:release_post25
      task jnam_post_f26
        trigger /prod12/nam/post/jnam_post_manager:release_post26
      task jnam_post_f27
        trigger /prod12/nam/post/jnam_post_manager:release_post27
      task jnam_post_f28
        trigger /prod12/nam/post/jnam_post_manager:release_post28
      task jnam_post_f29
        trigger /prod12/nam/post/jnam_post_manager:release_post29
      task jnam_post_f30
        trigger /prod12/nam/post/jnam_post_manager:release_post30
      task jnam_post_f31
        trigger /prod12/nam/post/jnam_post_manager:release_post31
      task jnam_post_f32
        trigger /prod12/nam/post/jnam_post_manager:release_post32
      task jnam_post_f33
        trigger /prod12/nam/post/jnam_post_manager:release_post33
      task jnam_post_f34
        trigger /prod12/nam/post/jnam_post_manager:release_post34
      task jnam_post_f35
        trigger /prod12/nam/post/jnam_post_manager:release_post35
      task jnam_post_f36
        trigger /prod12/nam/post/jnam_post_manager:release_post36
      task jnam_post_f37
        trigger /prod12/nam/post/jnam_post_manager:release_post37
      task jnam_post_f38
        trigger /prod12/nam/post/jnam_post_manager:release_post38
      task jnam_post_f39
        trigger /prod12/nam/post/jnam_post_manager:release_post39
      task jnam_post_f40
        trigger /prod12/nam/post/jnam_post_manager:release_post40
      task jnam_post_f41
        trigger /prod12/nam/post/jnam_post_manager:release_post41
      task jnam_post_f42
        trigger /prod12/nam/post/jnam_post_manager:release_post42
      task jnam_post_f43
        trigger /prod12/nam/post/jnam_post_manager:release_post43
      task jnam_post_f44
        trigger /prod12/nam/post/jnam_post_manager:release_post44
      task jnam_post_f45
        trigger /prod12/nam/post/jnam_post_manager:release_post45
      task jnam_post_f46
        trigger /prod12/nam/post/jnam_post_manager:release_post46
      task jnam_post_f47
        trigger /prod12/nam/post/jnam_post_manager:release_post47
      task jnam_post_f48
        trigger /prod12/nam/post/jnam_post_manager:release_post48
      task jnam_post_f49
        trigger /prod12/nam/post/jnam_post_manager:release_post49
      task jnam_post_f50
        trigger /prod12/nam/post/jnam_post_manager:release_post50
      task jnam_post_f51
        trigger /prod12/nam/post/jnam_post_manager:release_post51
      task jnam_post_f52
        trigger /prod12/nam/post/jnam_post_manager:release_post52
      task jnam_post_f53
        trigger /prod12/nam/post/jnam_post_manager:release_post53
      task jnam_post_f54
        trigger /prod12/nam/post/jnam_post_manager:release_post54
      task jnam_post_f55
        trigger /prod12/nam/post/jnam_post_manager:release_post55
      task jnam_post_f56
        trigger /prod12/nam/post/jnam_post_manager:release_post56
      task jnam_post_f57
        trigger /prod12/nam/post/jnam_post_manager:release_post57
      task jnam_post_f58
        trigger /prod12/nam/post/jnam_post_manager:release_post58
      task jnam_post_f59
        trigger /prod12/nam/post/jnam_post_manager:release_post59
      task jnam_post_f60
        trigger /prod12/nam/post/jnam_post_manager:release_post60
      task jnam_post_f61
        trigger /prod12/nam/post/jnam_post_manager:release_post61
      task jnam_post_f62
        trigger /prod12/nam/post/jnam_post_manager:release_post62
      task jnam_post_f63
        trigger /prod12/nam/post/jnam_post_manager:release_post63
      task jnam_post_f64
        trigger /prod12/nam/post/jnam_post_manager:release_post64
      task jnam_post_f65
        trigger /prod12/nam/post/jnam_post_manager:release_post65
      task jnam_post_f66
        trigger /prod12/nam/post/jnam_post_manager:release_post66
      task jnam_post_f67
        trigger /prod12/nam/post/jnam_post_manager:release_post67
      task jnam_post_f68
        trigger /prod12/nam/post/jnam_post_manager:release_post68
      task jnam_post_f69
        trigger /prod12/nam/post/jnam_post_manager:release_post69
      task jnam_post_f70
        trigger /prod12/nam/post/jnam_post_manager:release_post70
      task jnam_post_f71
        trigger /prod12/nam/post/jnam_post_manager:release_post71
      task jnam_post_f72
        trigger /prod12/nam/post/jnam_post_manager:release_post72
      task jnam_post_f73
        trigger /prod12/nam/post/jnam_post_manager:release_post73
      task jnam_post_f74
        trigger /prod12/nam/post/jnam_post_manager:release_post74
      task jnam_post_f75
        trigger /prod12/nam/post/jnam_post_manager:release_post75
      task jnam_post_f76
        trigger /prod12/nam/post/jnam_post_manager:release_post76
      task jnam_post_f77
        trigger /prod12/nam/post/jnam_post_manager:release_post77
      task jnam_post_f78
        trigger /prod12/nam/post/jnam_post_manager:release_post78
      task jnam_post_f79
        trigger /prod12/nam/post/jnam_post_manager:release_post79
      task jnam_post_f80
        trigger /prod12/nam/post/jnam_post_manager:release_post80
      task jnam_post_f81
        trigger /prod12/nam/post/jnam_post_manager:release_post81
      task jnam_post_f82
        trigger /prod12/nam/post/jnam_post_manager:release_post82
      task jnam_post_f83
        trigger /prod12/nam/post/jnam_post_manager:release_post83
      task jnam_post_f84
        trigger /prod12/nam/post/jnam_post_manager:release_post84
      task jnam_post_goestb
        trigger /prod12/nam/post/jnam_post_f00 == complete
    endfamily
    family gempak
      task jnam_gempak_nest
        trigger /prod12/nam/nest_conus/prdgen/jnam_prdgen_f00 == complete and /prod12/nam/nest_alaska/prdgen/jnam_prdgen_f00 == complete
      task jnam_gempak_meta
        trigger /prod12/nam/prdgen/jnam_prdgen_f00 == complete
      task jnam_gempak
        trigger /prod12/nam/prdgen/jnam_prdgen_f00 == complete
    endfamily
    family sminit_pr
      edit ECF_FILES '/ecf/ecfnets/scripts/nam/sminit'
      edit RUNTYP 'pr'
      edit grid 'pr'
      edit MEM '2000'
      task jnam_sminit_f00
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/post/jnam_post_f00 == complete and /prod12/nam/post/jnam_post_f01 == complete and /prod12/nam/post/jnam_post_f02 == complete
      task jnam_sminit_f03
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_prico/post/jnam_post_f01 == complete and /prod12/nam/nest_prico/post/jnam_post_f02 == complete and /prod12/nam/nest_prico/post/jnam_post_f03 == complete
      task jnam_sminit_f06
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_prico/post/jnam_post_f04 == complete and /prod12/nam/nest_prico/post/jnam_post_f05 == complete and /prod12/nam/nest_prico/post/jnam_post_f06 == complete and /prod12/nam/sminit_pr/jnam_sminit_f03 == complete
      task jnam_sminit_f09
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_prico/post/jnam_post_f07 == complete and /prod12/nam/nest_prico/post/jnam_post_f08 == complete and /prod12/nam/nest_prico/post/jnam_post_f09 == complete and /prod12/nam/sminit_pr/jnam_sminit_f06 == complete
      task jnam_sminit_f12
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_prico/post/jnam_post_f10 == complete and /prod12/nam/nest_prico/post/jnam_post_f11 == complete and /prod12/nam/nest_prico/post/jnam_post_f12 == complete and /prod12/nam/sminit_pr/jnam_sminit_f09 == complete
      task jnam_sminit_f15
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_prico/post/jnam_post_f13 == complete and /prod12/nam/nest_prico/post/jnam_post_f14 == complete and /prod12/nam/nest_prico/post/jnam_post_f15 == complete
      task jnam_sminit_f18
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_prico/post/jnam_post_f16 == complete and /prod12/nam/nest_prico/post/jnam_post_f17 == complete and /prod12/nam/nest_prico/post/jnam_post_f18 == complete and /prod12/nam/sminit_pr/jnam_sminit_f15 == complete
      task jnam_sminit_f21
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_prico/post/jnam_post_f19 == complete and /prod12/nam/nest_prico/post/jnam_post_f20 == complete and /prod12/nam/nest_prico/post/jnam_post_f21 == complete and /prod12/nam/sminit_pr/jnam_sminit_f18 == complete
      task jnam_sminit_f24
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_prico/post/jnam_post_f22 == complete and /prod12/nam/nest_prico/post/jnam_post_f23 == complete and /prod12/nam/nest_prico/post/jnam_post_f24 == complete and /prod12/nam/sminit_pr/jnam_sminit_f21 == complete
      task jnam_sminit_f27
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_prico/post/jnam_post_f25 == complete and /prod12/nam/nest_prico/post/jnam_post_f26 == complete and /prod12/nam/nest_prico/post/jnam_post_f27 == complete
      task jnam_sminit_f30
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_prico/post/jnam_post_f28 == complete and /prod12/nam/nest_prico/post/jnam_post_f29 == complete and /prod12/nam/nest_prico/post/jnam_post_f30 == complete and /prod12/nam/sminit_pr/jnam_sminit_f27 == complete
      task jnam_sminit_f33
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_prico/post/jnam_post_f31 == complete and /prod12/nam/nest_prico/post/jnam_post_f32 == complete and /prod12/nam/nest_prico/post/jnam_post_f33 == complete and /prod12/nam/sminit_pr/jnam_sminit_f30 == complete
      task jnam_sminit_f36
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_prico/post/jnam_post_f34 == complete and /prod12/nam/nest_prico/post/jnam_post_f35 == complete and /prod12/nam/nest_prico/post/jnam_post_f36 == complete and /prod12/nam/sminit_pr/jnam_sminit_f33 == complete
      task jnam_sminit_f39
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_prico/post/jnam_post_f37 == complete and /prod12/nam/nest_prico/post/jnam_post_f38 == complete and /prod12/nam/nest_prico/post/jnam_post_f39 == complete
      task jnam_sminit_f42
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_prico/post/jnam_post_f40 == complete and /prod12/nam/nest_prico/post/jnam_post_f41 == complete and /prod12/nam/nest_prico/post/jnam_post_f42 == complete and /prod12/nam/sminit_pr/jnam_sminit_f39 == complete
      task jnam_sminit_f45
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_prico/post/jnam_post_f43 == complete and /prod12/nam/nest_prico/post/jnam_post_f44 == complete and /prod12/nam/nest_prico/post/jnam_post_f45 == complete and /prod12/nam/sminit_pr/jnam_sminit_f42 == complete
      task jnam_sminit_f48
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_prico/post/jnam_post_f46 == complete and /prod12/nam/nest_prico/post/jnam_post_f47 == complete and /prod12/nam/nest_prico/post/jnam_post_f48 == complete and /prod12/nam/sminit_pr/jnam_sminit_f45 == complete
      task jnam_sminit_f51
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_prico/post/jnam_post_f49 == complete and /prod12/nam/nest_prico/post/jnam_post_f50 == complete and /prod12/nam/nest_prico/post/jnam_post_f51 == complete
      task jnam_sminit_f54
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_prico/post/jnam_post_f52 == complete and /prod12/nam/nest_prico/post/jnam_post_f53 == complete and /prod12/nam/nest_prico/post/jnam_post_f54 == complete and /prod12/nam/sminit_pr/jnam_sminit_f51 == complete
      task jnam_sminit_f57
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_prico/post/jnam_post_f55 == complete and /prod12/nam/nest_prico/post/jnam_post_f56 == complete and /prod12/nam/nest_prico/post/jnam_post_f57 == complete and /prod12/nam/sminit_pr/jnam_sminit_f54 == complete
      task jnam_sminit_f60
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_prico/post/jnam_post_f58 == complete and /prod12/nam/nest_prico/post/jnam_post_f59 == complete and /prod12/nam/nest_prico/post/jnam_post_f60 == complete and /prod12/nam/sminit_pr/jnam_sminit_f57 == complete
      task jnam_sminit_f63
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/post/jnam_post_f61 == complete and /prod12/nam/post/jnam_post_f62 == complete and /prod12/nam/post/jnam_post_f63 == complete
      task jnam_sminit_f66
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/post/jnam_post_f64 == complete and /prod12/nam/post/jnam_post_f65 == complete and /prod12/nam/post/jnam_post_f66 == complete and /prod12/nam/sminit_pr/jnam_sminit_f63 == complete
      task jnam_sminit_f69
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/post/jnam_post_f67 == complete and /prod12/nam/post/jnam_post_f68 == complete and /prod12/nam/post/jnam_post_f69 == complete and /prod12/nam/sminit_pr/jnam_sminit_f66 == complete
      task jnam_sminit_f72
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/post/jnam_post_f70 == complete and /prod12/nam/post/jnam_post_f71 == complete and /prod12/nam/post/jnam_post_f72 == complete and /prod12/nam/sminit_pr/jnam_sminit_f69 == complete
      task jnam_sminit_f75
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/post/jnam_post_f73 == complete and /prod12/nam/post/jnam_post_f74 == complete and /prod12/nam/post/jnam_post_f75 == complete
      task jnam_sminit_f78
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/post/jnam_post_f76 == complete and /prod12/nam/post/jnam_post_f77 == complete and /prod12/nam/post/jnam_post_f78 == complete and /prod12/nam/sminit_pr/jnam_sminit_f75 == complete
      task jnam_sminit_f81
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/post/jnam_post_f79 == complete and /prod12/nam/post/jnam_post_f80 == complete and /prod12/nam/post/jnam_post_f81 == complete and /prod12/nam/sminit_pr/jnam_sminit_f78 == complete
      task jnam_sminit_f84
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/post/jnam_post_f82 == complete and /prod12/nam/post/jnam_post_f83 == complete and /prod12/nam/post/jnam_post_f84 == complete and /prod12/nam/sminit_pr/jnam_sminit_f81 == complete
    endfamily
    family sminit_hi
      edit ECF_FILES '/ecf/ecfnets/scripts/nam/sminit'
      edit RUNTYP 'hi'
      edit grid 'hi'
      edit MEM '2000'
      task jnam_sminit_f00
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_243_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/post/jnam_post_f00 == complete and /prod12/nam/post/jnam_post_f01 == complete and /prod12/nam/post/jnam_post_f02 == complete
      task jnam_sminit_f03
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_243_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_hawaii/post/jnam_post_f01 == complete and /prod12/nam/nest_hawaii/post/jnam_post_f02 == complete and /prod12/nam/nest_hawaii/post/jnam_post_f03 == complete
      task jnam_sminit_f06
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_243_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_hawaii/post/jnam_post_f04 == complete and /prod12/nam/nest_hawaii/post/jnam_post_f05 == complete and /prod12/nam/nest_hawaii/post/jnam_post_f06 == complete and /prod12/nam/sminit_hi/jnam_sminit_f03 == complete
      task jnam_sminit_f09
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_243_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_hawaii/post/jnam_post_f07 == complete and /prod12/nam/nest_hawaii/post/jnam_post_f08 == complete and /prod12/nam/nest_hawaii/post/jnam_post_f09 == complete and /prod12/nam/sminit_hi/jnam_sminit_f06 == complete
      task jnam_sminit_f12
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_243_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_hawaii/post/jnam_post_f10 == complete and /prod12/nam/nest_hawaii/post/jnam_post_f11 == complete and /prod12/nam/nest_hawaii/post/jnam_post_f12 == complete and /prod12/nam/sminit_hi/jnam_sminit_f09 == complete
      task jnam_sminit_f15
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_243_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_hawaii/post/jnam_post_f13 == complete and /prod12/nam/nest_hawaii/post/jnam_post_f14 == complete and /prod12/nam/nest_hawaii/post/jnam_post_f15 == complete
      task jnam_sminit_f18
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_243_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_hawaii/post/jnam_post_f16 == complete and /prod12/nam/nest_hawaii/post/jnam_post_f17 == complete and /prod12/nam/nest_hawaii/post/jnam_post_f18 == complete and /prod12/nam/sminit_hi/jnam_sminit_f15 == complete
      task jnam_sminit_f21
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_243_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_hawaii/post/jnam_post_f19 == complete and /prod12/nam/nest_hawaii/post/jnam_post_f20 == complete and /prod12/nam/nest_hawaii/post/jnam_post_f21 == complete and /prod12/nam/sminit_hi/jnam_sminit_f18 == complete
      task jnam_sminit_f24
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_243_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_hawaii/post/jnam_post_f22 == complete and /prod12/nam/nest_hawaii/post/jnam_post_f23 == complete and /prod12/nam/nest_hawaii/post/jnam_post_f24 == complete and /prod12/nam/sminit_hi/jnam_sminit_f21 == complete
      task jnam_sminit_f27
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_243_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_hawaii/post/jnam_post_f25 == complete and /prod12/nam/nest_hawaii/post/jnam_post_f26 == complete and /prod12/nam/nest_hawaii/post/jnam_post_f27 == complete
      task jnam_sminit_f30
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_243_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_hawaii/post/jnam_post_f28 == complete and /prod12/nam/nest_hawaii/post/jnam_post_f29 == complete and /prod12/nam/nest_hawaii/post/jnam_post_f30 == complete and /prod12/nam/sminit_hi/jnam_sminit_f27 == complete
      task jnam_sminit_f33
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_243_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_hawaii/post/jnam_post_f31 == complete and /prod12/nam/nest_hawaii/post/jnam_post_f32 == complete and /prod12/nam/nest_hawaii/post/jnam_post_f33 == complete and /prod12/nam/sminit_hi/jnam_sminit_f30 == complete
      task jnam_sminit_f36
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_243_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_hawaii/post/jnam_post_f34 == complete and /prod12/nam/nest_hawaii/post/jnam_post_f35 == complete and /prod12/nam/nest_hawaii/post/jnam_post_f36 == complete and /prod12/nam/sminit_hi/jnam_sminit_f33 == complete
      task jnam_sminit_f39
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_243_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_hawaii/post/jnam_post_f37 == complete and /prod12/nam/nest_hawaii/post/jnam_post_f38 == complete and /prod12/nam/nest_hawaii/post/jnam_post_f39 == complete
      task jnam_sminit_f42
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_243_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_hawaii/post/jnam_post_f40 == complete and /prod12/nam/nest_hawaii/post/jnam_post_f41 == complete and /prod12/nam/nest_hawaii/post/jnam_post_f42 == complete and /prod12/nam/sminit_hi/jnam_sminit_f39 == complete
      task jnam_sminit_f45
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_243_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_hawaii/post/jnam_post_f43 == complete and /prod12/nam/nest_hawaii/post/jnam_post_f44 == complete and /prod12/nam/nest_hawaii/post/jnam_post_f45 == complete and /prod12/nam/sminit_hi/jnam_sminit_f42 == complete
      task jnam_sminit_f48
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_243_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_hawaii/post/jnam_post_f46 == complete and /prod12/nam/nest_hawaii/post/jnam_post_f47 == complete and /prod12/nam/nest_hawaii/post/jnam_post_f48 == complete and /prod12/nam/sminit_hi/jnam_sminit_f45 == complete
      task jnam_sminit_f51
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_243_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_hawaii/post/jnam_post_f49 == complete and /prod12/nam/nest_hawaii/post/jnam_post_f50 == complete and /prod12/nam/nest_hawaii/post/jnam_post_f51 == complete
      task jnam_sminit_f54
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_243_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_hawaii/post/jnam_post_f52 == complete and /prod12/nam/nest_hawaii/post/jnam_post_f53 == complete and /prod12/nam/nest_hawaii/post/jnam_post_f54 == complete and /prod12/nam/sminit_hi/jnam_sminit_f51 == complete
      task jnam_sminit_f57
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_243_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_hawaii/post/jnam_post_f55 == complete and /prod12/nam/nest_hawaii/post/jnam_post_f56 == complete and /prod12/nam/nest_hawaii/post/jnam_post_f57 == complete and /prod12/nam/sminit_hi/jnam_sminit_f54 == complete
      task jnam_sminit_f60
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_243_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_hawaii/post/jnam_post_f58 == complete and /prod12/nam/nest_hawaii/post/jnam_post_f59 == complete and /prod12/nam/nest_hawaii/post/jnam_post_f60 == complete and /prod12/nam/sminit_hi/jnam_sminit_f57 == complete
      task jnam_sminit_f63
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_243_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/post/jnam_post_f61 == complete and /prod12/nam/post/jnam_post_f62 == complete and /prod12/nam/post/jnam_post_f63 == complete
      task jnam_sminit_f66
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_243_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/post/jnam_post_f64 == complete and /prod12/nam/post/jnam_post_f65 == complete and /prod12/nam/post/jnam_post_f66 == complete and /prod12/nam/sminit_hi/jnam_sminit_f63 == complete
      task jnam_sminit_f69
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_243_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/post/jnam_post_f67 == complete and /prod12/nam/post/jnam_post_f68 == complete and /prod12/nam/post/jnam_post_f69 == complete and /prod12/nam/sminit_hi/jnam_sminit_f66 == complete
      task jnam_sminit_f72
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_243_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/post/jnam_post_f70 == complete and /prod12/nam/post/jnam_post_f71 == complete and /prod12/nam/post/jnam_post_f72 == complete and /prod12/nam/sminit_hi/jnam_sminit_f69 == complete
      task jnam_sminit_f75
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_243_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/post/jnam_post_f73 == complete and /prod12/nam/post/jnam_post_f74 == complete and /prod12/nam/post/jnam_post_f75 == complete
      task jnam_sminit_f78
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_243_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/post/jnam_post_f76 == complete and /prod12/nam/post/jnam_post_f77 == complete and /prod12/nam/post/jnam_post_f78 == complete and /prod12/nam/sminit_hi/jnam_sminit_f75 == complete
      task jnam_sminit_f81
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_243_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/post/jnam_post_f79 == complete and /prod12/nam/post/jnam_post_f80 == complete and /prod12/nam/post/jnam_post_f81 == complete and /prod12/nam/sminit_hi/jnam_sminit_f78 == complete
      task jnam_sminit_f84
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_243_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/post/jnam_post_f82 == complete and /prod12/nam/post/jnam_post_f83 == complete and /prod12/nam/post/jnam_post_f84 == complete and /prod12/nam/sminit_hi/jnam_sminit_f81 == complete
    endfamily
    family mos
      edit COM 'com2'
      edit QUEUE 'prod2'
      task jnam_mos
        trigger /prod12/nam/prdgen/jnam_prdgen_f84 == complete
      task jnam_mos_prep
        trigger jnam_mos == complete
      task jnam_mos_gempak
        trigger jnam_mos == complete
    endfamily
    family sminit_ak
      edit ECF_FILES '/ecf/ecfnets/scripts/nam/sminit'
      edit RUNTYP 'ak'
      edit grid 'ak'
      edit MEM '2000'
      task jnam_sminit_f00
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/post/jnam_post_f00 == complete and /prod12/nam/post/jnam_post_f01 == complete and /prod12/nam/post/jnam_post_f02 == complete
      task jnam_sminit_f03
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_alaska/post/jnam_post_f01 == complete and /prod12/nam/nest_alaska/post/jnam_post_f02 == complete and /prod12/nam/nest_alaska/post/jnam_post_f03 == complete
      task jnam_sminit_f06
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_alaska/post/jnam_post_f04 == complete and /prod12/nam/nest_alaska/post/jnam_post_f05 == complete and /prod12/nam/nest_alaska/post/jnam_post_f06 == complete and /prod12/nam/sminit_ak/jnam_sminit_f03 == complete
      task jnam_sminit_f09
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_alaska/post/jnam_post_f07 == complete and /prod12/nam/nest_alaska/post/jnam_post_f08 == complete and /prod12/nam/nest_alaska/post/jnam_post_f09 == complete and /prod12/nam/sminit_ak/jnam_sminit_f06 == complete
      task jnam_sminit_f12
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_alaska/post/jnam_post_f10 == complete and /prod12/nam/nest_alaska/post/jnam_post_f11 == complete and /prod12/nam/nest_alaska/post/jnam_post_f12 == complete and /prod12/nam/sminit_ak/jnam_sminit_f09 == complete
      task jnam_sminit_f15
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_alaska/post/jnam_post_f13 == complete and /prod12/nam/nest_alaska/post/jnam_post_f14 == complete and /prod12/nam/nest_alaska/post/jnam_post_f15 == complete
      task jnam_sminit_f18
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_alaska/post/jnam_post_f16 == complete and /prod12/nam/nest_alaska/post/jnam_post_f17 == complete and /prod12/nam/nest_alaska/post/jnam_post_f18 == complete and /prod12/nam/sminit_ak/jnam_sminit_f15 == complete
      task jnam_sminit_f21
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_alaska/post/jnam_post_f19 == complete and /prod12/nam/nest_alaska/post/jnam_post_f20 == complete and /prod12/nam/nest_alaska/post/jnam_post_f21 == complete and /prod12/nam/sminit_ak/jnam_sminit_f18 == complete
      task jnam_sminit_f24
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_alaska/post/jnam_post_f22 == complete and /prod12/nam/nest_alaska/post/jnam_post_f23 == complete and /prod12/nam/nest_alaska/post/jnam_post_f24 == complete and /prod12/nam/sminit_ak/jnam_sminit_f21 == complete
      task jnam_sminit_f27
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_alaska/post/jnam_post_f25 == complete and /prod12/nam/nest_alaska/post/jnam_post_f26 == complete and /prod12/nam/nest_alaska/post/jnam_post_f27 == complete
      task jnam_sminit_f30
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_alaska/post/jnam_post_f28 == complete and /prod12/nam/nest_alaska/post/jnam_post_f29 == complete and /prod12/nam/nest_alaska/post/jnam_post_f30 == complete and /prod12/nam/sminit_ak/jnam_sminit_f27 == complete
      task jnam_sminit_f33
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_alaska/post/jnam_post_f31 == complete and /prod12/nam/nest_alaska/post/jnam_post_f32 == complete and /prod12/nam/nest_alaska/post/jnam_post_f33 == complete and /prod12/nam/sminit_ak/jnam_sminit_f30 == complete
      task jnam_sminit_f36
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_alaska/post/jnam_post_f34 == complete and /prod12/nam/nest_alaska/post/jnam_post_f35 == complete and /prod12/nam/nest_alaska/post/jnam_post_f36 == complete and /prod12/nam/sminit_ak/jnam_sminit_f33 == complete
      task jnam_sminit_f39
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_alaska/post/jnam_post_f37 == complete and /prod12/nam/nest_alaska/post/jnam_post_f38 == complete and /prod12/nam/nest_alaska/post/jnam_post_f39 == complete
      task jnam_sminit_f42
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_alaska/post/jnam_post_f40 == complete and /prod12/nam/nest_alaska/post/jnam_post_f41 == complete and /prod12/nam/nest_alaska/post/jnam_post_f42 == complete and /prod12/nam/sminit_ak/jnam_sminit_f39 == complete
      task jnam_sminit_f45
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_alaska/post/jnam_post_f43 == complete and /prod12/nam/nest_alaska/post/jnam_post_f44 == complete and /prod12/nam/nest_alaska/post/jnam_post_f45 == complete and /prod12/nam/sminit_ak/jnam_sminit_f42 == complete
      task jnam_sminit_f48
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_alaska/post/jnam_post_f46 == complete and /prod12/nam/nest_alaska/post/jnam_post_f47 == complete and /prod12/nam/nest_alaska/post/jnam_post_f48 == complete and /prod12/nam/sminit_ak/jnam_sminit_f45 == complete
      task jnam_sminit_f51
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_alaska/post/jnam_post_f49 == complete and /prod12/nam/nest_alaska/post/jnam_post_f50 == complete and /prod12/nam/nest_alaska/post/jnam_post_f51 == complete
      task jnam_sminit_f54
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_alaska/post/jnam_post_f52 == complete and /prod12/nam/nest_alaska/post/jnam_post_f53 == complete and /prod12/nam/nest_alaska/post/jnam_post_f54 == complete and /prod12/nam/sminit_ak/jnam_sminit_f51 == complete
      task jnam_sminit_f57
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_alaska/post/jnam_post_f55 == complete and /prod12/nam/nest_alaska/post/jnam_post_f56 == complete and /prod12/nam/nest_alaska/post/jnam_post_f57 == complete and /prod12/nam/sminit_ak/jnam_sminit_f54 == complete
      task jnam_sminit_f60
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_alaska/post/jnam_post_f58 == complete and /prod12/nam/nest_alaska/post/jnam_post_f59 == complete and /prod12/nam/nest_alaska/post/jnam_post_f60 == complete and /prod12/nam/sminit_ak/jnam_sminit_f57 == complete
      task jnam_sminit_f63
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/post/jnam_post_f61 == complete and /prod12/nam/post/jnam_post_f62 == complete and /prod12/nam/post/jnam_post_f63 == complete
      task jnam_sminit_f66
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/post/jnam_post_f64 == complete and /prod12/nam/post/jnam_post_f65 == complete and /prod12/nam/post/jnam_post_f66 == complete and /prod12/nam/sminit_ak/jnam_sminit_f63 == complete
      task jnam_sminit_f69
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/post/jnam_post_f67 == complete and /prod12/nam/post/jnam_post_f68 == complete and /prod12/nam/post/jnam_post_f69 == complete and /prod12/nam/sminit_ak/jnam_sminit_f66 == complete
      task jnam_sminit_f72
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/post/jnam_post_f70 == complete and /prod12/nam/post/jnam_post_f71 == complete and /prod12/nam/post/jnam_post_f72 == complete and /prod12/nam/sminit_ak/jnam_sminit_f69 == complete
      task jnam_sminit_f75
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/post/jnam_post_f73 == complete and /prod12/nam/post/jnam_post_f74 == complete and /prod12/nam/post/jnam_post_f75 == complete
      task jnam_sminit_f78
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/post/jnam_post_f76 == complete and /prod12/nam/post/jnam_post_f77 == complete and /prod12/nam/post/jnam_post_f78 == complete and /prod12/nam/sminit_ak/jnam_sminit_f75 == complete
      task jnam_sminit_f81
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/post/jnam_post_f79 == complete and /prod12/nam/post/jnam_post_f80 == complete and /prod12/nam/post/jnam_post_f81 == complete and /prod12/nam/sminit_ak/jnam_sminit_f78 == complete
      task jnam_sminit_f84
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/post/jnam_post_f82 == complete and /prod12/nam/post/jnam_post_f83 == complete and /prod12/nam/post/jnam_post_f84 == complete and /prod12/nam/sminit_ak/jnam_sminit_f81 == complete
      family akrtmages
        edit RUNTYP 'ak_rtmages'
        task jnam_sminit_akrtmages_f03
          trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/post/jnam_post_f01 == complete and /prod12/nam/post/jnam_post_f02 == complete and /prod12/nam/post/jnam_post_f03 == complete
        task jnam_sminit_akrtmages_f06
          trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/post/jnam_post_f04 == complete and /prod12/nam/post/jnam_post_f05 == complete and /prod12/nam/post/jnam_post_f06 == complete and /prod12/nam/sminit_ak/akrtmages/jnam_sminit_akrtmages_f03 == complete
        task jnam_sminit_akrtmages_f09
          trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/post/jnam_post_f07 == complete and /prod12/nam/post/jnam_post_f08 == complete and /prod12/nam/post/jnam_post_f09 == complete and /prod12/nam/sminit_ak/akrtmages/jnam_sminit_akrtmages_f06 == complete
      endfamily
    endfamily
    family post_processing
      family fax
        task jnam_fax_f00
          trigger /prod12/nam/prdgen/jnam_prdgen_f00 == complete
      endfamily
      task jnam_make_bgrd_grib2
        trigger /prod12/nam/post/jnam_post_f48 == complete
      family bulletins
        task jnam_fbwind
          trigger /prod12/nam/prdgen/jnam_prdgen_f06 == complete and /prod12/nam/prdgen/jnam_prdgen_f12 == complete and /prod12/nam/prdgen/jnam_prdgen_f24 == complete
        task jnam_bulls_f60
          trigger /prod12/nam/prdgen/jnam_prdgen_f60 == complete
        task jnam_bulls_f84
          trigger /prod12/nam/prdgen/jnam_prdgen_f84 == complete
      endfamily
      family grib_awips
        task jnam_awips_f00
          trigger /prod12/nam/prdgen/jnam_prdgen_f00 == complete
        task jnam_awips_f03
          trigger /prod12/nam/prdgen/jnam_prdgen_f03 == complete
        task jnam_awips_f06
          trigger /prod12/nam/prdgen/jnam_prdgen_f06 == complete
        task jnam_awips_f09
          trigger /prod12/nam/prdgen/jnam_prdgen_f09 == complete
        task jnam_awips_f12
          trigger /prod12/nam/prdgen/jnam_prdgen_f12 == complete
        task jnam_awips_f15
          trigger /prod12/nam/prdgen/jnam_prdgen_f15 == complete
        task jnam_awips_f18
          trigger /prod12/nam/prdgen/jnam_prdgen_f18 == complete
        task jnam_awips_f21
          trigger /prod12/nam/prdgen/jnam_prdgen_f21 == complete
        task jnam_awips_f24
          trigger /prod12/nam/prdgen/jnam_prdgen_f24 == complete
        task jnam_awips_f27
          trigger /prod12/nam/prdgen/jnam_prdgen_f27 == complete
        task jnam_awips_f30
          trigger /prod12/nam/prdgen/jnam_prdgen_f30 == complete
        task jnam_awips_f33
          trigger /prod12/nam/prdgen/jnam_prdgen_f33 == complete
        task jnam_awips_f36
          trigger /prod12/nam/prdgen/jnam_prdgen_f36 == complete
        task jnam_awips_f39
          trigger /prod12/nam/prdgen/jnam_prdgen_f39 == complete
        task jnam_awips_f42
          trigger /prod12/nam/prdgen/jnam_prdgen_f42 == complete
        task jnam_awips_f45
          trigger /prod12/nam/prdgen/jnam_prdgen_f45 == complete
        task jnam_awips_f48
          trigger /prod12/nam/prdgen/jnam_prdgen_f48 == complete
        task jnam_awips_f51
          trigger /prod12/nam/prdgen/jnam_prdgen_f51 == complete
        task jnam_awips_f54
          trigger /prod12/nam/prdgen/jnam_prdgen_f54 == complete
        task jnam_awips_f57
          trigger /prod12/nam/prdgen/jnam_prdgen_f57 == complete
        task jnam_awips_f60
          trigger /prod12/nam/prdgen/jnam_prdgen_f60 == complete
        task jnam_awips_f63
          trigger /prod12/nam/prdgen/jnam_prdgen_f63 == complete
        task jnam_awips_f66
          trigger /prod12/nam/prdgen/jnam_prdgen_f66 == complete
        task jnam_awips_f69
          trigger /prod12/nam/prdgen/jnam_prdgen_f69 == complete
        task jnam_awips_f72
          trigger /prod12/nam/prdgen/jnam_prdgen_f72 == complete
        task jnam_awips_f75
          trigger /prod12/nam/prdgen/jnam_prdgen_f75 == complete
        task jnam_awips_f78
          trigger /prod12/nam/prdgen/jnam_prdgen_f78 == complete
        task jnam_awips_f81
          trigger /prod12/nam/prdgen/jnam_prdgen_f81 == complete
        task jnam_awips_f84
          trigger /prod12/nam/prdgen/jnam_prdgen_f84 == complete
      endfamily
      family bufr_sounding
        task jnam_postsnd_prico
          trigger /prod12/nam/nest_prico/profile == complete
        task jnam_postsnd_hawaii
          trigger /prod12/nam/nest_hawaii/profile == complete
        task jnam_postsnd_alaska
          trigger /prod12/nam/nest_alaska/profile == complete
        task jnam_postsnd_conus
          trigger /prod12/nam/nest_conus/profile == complete
        task jnam_postsnd
          trigger /prod12/nam/profile == complete
      endfamily
    endfamily
    family sminit_ak3
      edit ECF_FILES '/ecf/ecfnets/scripts/nam/sminit'
      edit RUNTYP 'aknest3'
      edit grid 'ak3'
      edit MEM '3500'
      task jnam_sminit_f00
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/post/jnam_post_f00 == complete and /prod12/nam/post/jnam_post_f01 == complete and /prod12/nam/post/jnam_post_f02 == complete
      task jnam_sminit_f03
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_alaska/post/jnam_post_f01 == complete and /prod12/nam/nest_alaska/post/jnam_post_f02 == complete and /prod12/nam/nest_alaska/post/jnam_post_f03 == complete
      task jnam_sminit_f06
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_alaska/post/jnam_post_f04 == complete and /prod12/nam/nest_alaska/post/jnam_post_f05 == complete and /prod12/nam/nest_alaska/post/jnam_post_f06 == complete and /prod12/nam/sminit_ak3/jnam_sminit_f03 == complete
      task jnam_sminit_f09
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_alaska/post/jnam_post_f07 == complete and /prod12/nam/nest_alaska/post/jnam_post_f08 == complete and /prod12/nam/nest_alaska/post/jnam_post_f09 == complete and /prod12/nam/sminit_ak3/jnam_sminit_f06 == complete
      task jnam_sminit_f12
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_alaska/post/jnam_post_f10 == complete and /prod12/nam/nest_alaska/post/jnam_post_f11 == complete and /prod12/nam/nest_alaska/post/jnam_post_f12 == complete and /prod12/nam/sminit_ak3/jnam_sminit_f09 == complete
      task jnam_sminit_f15
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_alaska/post/jnam_post_f13 == complete and /prod12/nam/nest_alaska/post/jnam_post_f14 == complete and /prod12/nam/nest_alaska/post/jnam_post_f15 == complete
      task jnam_sminit_f18
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_alaska/post/jnam_post_f16 == complete and /prod12/nam/nest_alaska/post/jnam_post_f17 == complete and /prod12/nam/nest_alaska/post/jnam_post_f18 == complete and /prod12/nam/sminit_ak3/jnam_sminit_f15 == complete
      task jnam_sminit_f21
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_alaska/post/jnam_post_f19 == complete and /prod12/nam/nest_alaska/post/jnam_post_f20 == complete and /prod12/nam/nest_alaska/post/jnam_post_f21 == complete and /prod12/nam/sminit_ak3/jnam_sminit_f18 == complete
      task jnam_sminit_f24
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_alaska/post/jnam_post_f22 == complete and /prod12/nam/nest_alaska/post/jnam_post_f23 == complete and /prod12/nam/nest_alaska/post/jnam_post_f24 == complete and /prod12/nam/sminit_ak3/jnam_sminit_f21 == complete
      task jnam_sminit_f27
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_alaska/post/jnam_post_f25 == complete and /prod12/nam/nest_alaska/post/jnam_post_f26 == complete and /prod12/nam/nest_alaska/post/jnam_post_f27 == complete
      task jnam_sminit_f30
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_alaska/post/jnam_post_f28 == complete and /prod12/nam/nest_alaska/post/jnam_post_f29 == complete and /prod12/nam/nest_alaska/post/jnam_post_f30 == complete and /prod12/nam/sminit_ak3/jnam_sminit_f27 == complete
      task jnam_sminit_f33
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_alaska/post/jnam_post_f31 == complete and /prod12/nam/nest_alaska/post/jnam_post_f32 == complete and /prod12/nam/nest_alaska/post/jnam_post_f33 == complete and /prod12/nam/sminit_ak3/jnam_sminit_f30 == complete
      task jnam_sminit_f36
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_alaska/post/jnam_post_f34 == complete and /prod12/nam/nest_alaska/post/jnam_post_f35 == complete and /prod12/nam/nest_alaska/post/jnam_post_f36 == complete and /prod12/nam/sminit_ak3/jnam_sminit_f33 == complete
      task jnam_sminit_f39
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_alaska/post/jnam_post_f37 == complete and /prod12/nam/nest_alaska/post/jnam_post_f38 == complete and /prod12/nam/nest_alaska/post/jnam_post_f39 == complete
      task jnam_sminit_f42
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_alaska/post/jnam_post_f40 == complete and /prod12/nam/nest_alaska/post/jnam_post_f41 == complete and /prod12/nam/nest_alaska/post/jnam_post_f42 == complete and /prod12/nam/sminit_ak3/jnam_sminit_f39 == complete
      task jnam_sminit_f45
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_alaska/post/jnam_post_f43 == complete and /prod12/nam/nest_alaska/post/jnam_post_f44 == complete and /prod12/nam/nest_alaska/post/jnam_post_f45 == complete and /prod12/nam/sminit_ak3/jnam_sminit_f42 == complete
      task jnam_sminit_f48
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_alaska/post/jnam_post_f46 == complete and /prod12/nam/nest_alaska/post/jnam_post_f47 == complete and /prod12/nam/nest_alaska/post/jnam_post_f48 == complete and /prod12/nam/sminit_ak3/jnam_sminit_f45 == complete
      task jnam_sminit_f51
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_alaska/post/jnam_post_f49 == complete and /prod12/nam/nest_alaska/post/jnam_post_f50 == complete and /prod12/nam/nest_alaska/post/jnam_post_f51 == complete
      task jnam_sminit_f54
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_alaska/post/jnam_post_f52 == complete and /prod12/nam/nest_alaska/post/jnam_post_f53 == complete and /prod12/nam/nest_alaska/post/jnam_post_f54 == complete and /prod12/nam/sminit_ak3/jnam_sminit_f51 == complete
      task jnam_sminit_f57
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_alaska/post/jnam_post_f55 == complete and /prod12/nam/nest_alaska/post/jnam_post_f56 == complete and /prod12/nam/nest_alaska/post/jnam_post_f57 == complete and /prod12/nam/sminit_ak3/jnam_sminit_f54 == complete
      task jnam_sminit_f60
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_alaska/post/jnam_post_f58 == complete and /prod12/nam/nest_alaska/post/jnam_post_f59 == complete and /prod12/nam/nest_alaska/post/jnam_post_f60 == complete and /prod12/nam/sminit_ak3/jnam_sminit_f57 == complete
    endfamily
    family sminit_conus
      edit ECF_FILES '/ecf/ecfnets/scripts/nam/sminit'
      edit RUNTYP 'conus'
      edit grid 'conus'
      edit MEM '2000'
      task jnam_sminit_f00
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/post/jnam_post_f00 == complete and /prod12/nam/post/jnam_post_f01 == complete and /prod12/nam/post/jnam_post_f02 == complete
      task jnam_sminit_f03
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_conus/post/jnam_post_f01 == complete and /prod12/nam/nest_conus/post/jnam_post_f02 == complete and /prod12/nam/nest_conus/post/jnam_post_f03 == complete
      task jnam_sminit_f06
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_conus/post/jnam_post_f04 == complete and /prod12/nam/nest_conus/post/jnam_post_f05 == complete and /prod12/nam/nest_conus/post/jnam_post_f06 == complete and /prod12/nam/sminit_conus/jnam_sminit_f03 == complete
      task jnam_sminit_f09
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_conus/post/jnam_post_f07 == complete and /prod12/nam/nest_conus/post/jnam_post_f08 == complete and /prod12/nam/nest_conus/post/jnam_post_f09 == complete and /prod12/nam/sminit_conus/jnam_sminit_f06 == complete
      task jnam_sminit_f12
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_conus/post/jnam_post_f10 == complete and /prod12/nam/nest_conus/post/jnam_post_f11 == complete and /prod12/nam/nest_conus/post/jnam_post_f12 == complete and /prod12/nam/sminit_conus/jnam_sminit_f09 == complete
      task jnam_sminit_f15
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_conus/post/jnam_post_f13 == complete and /prod12/nam/nest_conus/post/jnam_post_f14 == complete and /prod12/nam/nest_conus/post/jnam_post_f15 == complete
      task jnam_sminit_f18
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_conus/post/jnam_post_f16 == complete and /prod12/nam/nest_conus/post/jnam_post_f17 == complete and /prod12/nam/nest_conus/post/jnam_post_f18 == complete and /prod12/nam/sminit_conus/jnam_sminit_f15 == complete
      task jnam_sminit_f21
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_conus/post/jnam_post_f19 == complete and /prod12/nam/nest_conus/post/jnam_post_f20 == complete and /prod12/nam/nest_conus/post/jnam_post_f21 == complete and /prod12/nam/sminit_conus/jnam_sminit_f18 == complete
      task jnam_sminit_f24
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_conus/post/jnam_post_f22 == complete and /prod12/nam/nest_conus/post/jnam_post_f23 == complete and /prod12/nam/nest_conus/post/jnam_post_f24 == complete and /prod12/nam/sminit_conus/jnam_sminit_f21 == complete
      task jnam_sminit_f27
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_conus/post/jnam_post_f25 == complete and /prod12/nam/nest_conus/post/jnam_post_f26 == complete and /prod12/nam/nest_conus/post/jnam_post_f27 == complete
      task jnam_sminit_f30
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_conus/post/jnam_post_f28 == complete and /prod12/nam/nest_conus/post/jnam_post_f29 == complete and /prod12/nam/nest_conus/post/jnam_post_f30 == complete and /prod12/nam/sminit_conus/jnam_sminit_f27 == complete
      task jnam_sminit_f33
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_conus/post/jnam_post_f31 == complete and /prod12/nam/nest_conus/post/jnam_post_f32 == complete and /prod12/nam/nest_conus/post/jnam_post_f33 == complete and /prod12/nam/sminit_conus/jnam_sminit_f30 == complete
      task jnam_sminit_f36
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_conus/post/jnam_post_f34 == complete and /prod12/nam/nest_conus/post/jnam_post_f35 == complete and /prod12/nam/nest_conus/post/jnam_post_f36 == complete and /prod12/nam/sminit_conus/jnam_sminit_f33 == complete
      task jnam_sminit_f39
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_conus/post/jnam_post_f37 == complete and /prod12/nam/nest_conus/post/jnam_post_f38 == complete and /prod12/nam/nest_conus/post/jnam_post_f39 == complete
      task jnam_sminit_f42
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_conus/post/jnam_post_f40 == complete and /prod12/nam/nest_conus/post/jnam_post_f41 == complete and /prod12/nam/nest_conus/post/jnam_post_f42 == complete and /prod12/nam/sminit_conus/jnam_sminit_f39 == complete
      task jnam_sminit_f45
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_conus/post/jnam_post_f43 == complete and /prod12/nam/nest_conus/post/jnam_post_f44 == complete and /prod12/nam/nest_conus/post/jnam_post_f45 == complete and /prod12/nam/sminit_conus/jnam_sminit_f42 == complete
      task jnam_sminit_f48
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_conus/post/jnam_post_f46 == complete and /prod12/nam/nest_conus/post/jnam_post_f47 == complete and /prod12/nam/nest_conus/post/jnam_post_f48 == complete and /prod12/nam/sminit_conus/jnam_sminit_f45 == complete
      task jnam_sminit_f51
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_conus/post/jnam_post_f49 == complete and /prod12/nam/nest_conus/post/jnam_post_f50 == complete and /prod12/nam/nest_conus/post/jnam_post_f51 == complete
      task jnam_sminit_f54
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_conus/post/jnam_post_f52 == complete and /prod12/nam/nest_conus/post/jnam_post_f53 == complete and /prod12/nam/nest_conus/post/jnam_post_f54 == complete and /prod12/nam/sminit_conus/jnam_sminit_f51 == complete
      task jnam_sminit_f57
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_conus/post/jnam_post_f55 == complete and /prod12/nam/nest_conus/post/jnam_post_f56 == complete and /prod12/nam/nest_conus/post/jnam_post_f57 == complete and /prod12/nam/sminit_conus/jnam_sminit_f54 == complete
      task jnam_sminit_f60
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_conus/post/jnam_post_f58 == complete and /prod12/nam/nest_conus/post/jnam_post_f59 == complete and /prod12/nam/nest_conus/post/jnam_post_f60 == complete and /prod12/nam/sminit_conus/jnam_sminit_f57 == complete
      task jnam_sminit_f63
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/post/jnam_post_f61 == complete and /prod12/nam/post/jnam_post_f62 == complete and /prod12/nam/post/jnam_post_f63 == complete
      task jnam_sminit_f66
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/post/jnam_post_f64 == complete and /prod12/nam/post/jnam_post_f65 == complete and /prod12/nam/post/jnam_post_f66 == complete and /prod12/nam/sminit_conus/jnam_sminit_f63 == complete
      task jnam_sminit_f69
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/post/jnam_post_f67 == complete and /prod12/nam/post/jnam_post_f68 == complete and /prod12/nam/post/jnam_post_f69 == complete and /prod12/nam/sminit_conus/jnam_sminit_f66 == complete
      task jnam_sminit_f72
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/post/jnam_post_f70 == complete and /prod12/nam/post/jnam_post_f71 == complete and /prod12/nam/post/jnam_post_f72 == complete and /prod12/nam/sminit_conus/jnam_sminit_f69 == complete
      task jnam_sminit_f75
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/post/jnam_post_f73 == complete and /prod12/nam/post/jnam_post_f74 == complete and /prod12/nam/post/jnam_post_f75 == complete
      task jnam_sminit_f78
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/post/jnam_post_f76 == complete and /prod12/nam/post/jnam_post_f77 == complete and /prod12/nam/post/jnam_post_f78 == complete and /prod12/nam/sminit_conus/jnam_sminit_f75 == complete
      task jnam_sminit_f81
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/post/jnam_post_f79 == complete and /prod12/nam/post/jnam_post_f80 == complete and /prod12/nam/post/jnam_post_f81 == complete and /prod12/nam/sminit_conus/jnam_sminit_f78 == complete
      task jnam_sminit_f84
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/post/jnam_post_f82 == complete and /prod12/nam/post/jnam_post_f83 == complete and /prod12/nam/post/jnam_post_f84 == complete and /prod12/nam/sminit_conus/jnam_sminit_f81 == complete
    endfamily
    family sminit_conus2p5
      edit ECF_FILES '/ecf/ecfnets/scripts/nam/sminit'
      edit RUNTYP 'conusnest2p5'
      edit grid 'conus2p5'
      edit MEM '6000'
      task jnam_sminit_f00
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_conus/post/jnam_post_f00 == complete and /prod12/nam/nest_conus/post/jnam_post_f01 == complete and /prod12/nam/nest_conus/post/jnam_post_f02 == complete
      task jnam_sminit_f03
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_conus/post/jnam_post_f01 == complete and /prod12/nam/nest_conus/post/jnam_post_f02 == complete and /prod12/nam/nest_conus/post/jnam_post_f03 == complete
      task jnam_sminit_f06
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_conus/post/jnam_post_f04 == complete and /prod12/nam/nest_conus/post/jnam_post_f05 == complete and /prod12/nam/nest_conus/post/jnam_post_f06 == complete and /prod12/nam/sminit_conus2p5/jnam_sminit_f03 == complete
      task jnam_sminit_f09
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_conus/post/jnam_post_f07 == complete and /prod12/nam/nest_conus/post/jnam_post_f08 == complete and /prod12/nam/nest_conus/post/jnam_post_f09 == complete and /prod12/nam/sminit_conus2p5/jnam_sminit_f06 == complete
      task jnam_sminit_f12
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_conus/post/jnam_post_f10 == complete and /prod12/nam/nest_conus/post/jnam_post_f11 == complete and /prod12/nam/nest_conus/post/jnam_post_f12 == complete and /prod12/nam/sminit_conus2p5/jnam_sminit_f09 == complete
      task jnam_sminit_f15
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_conus/post/jnam_post_f13 == complete and /prod12/nam/nest_conus/post/jnam_post_f14 == complete and /prod12/nam/nest_conus/post/jnam_post_f15 == complete
      task jnam_sminit_f18
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_conus/post/jnam_post_f16 == complete and /prod12/nam/nest_conus/post/jnam_post_f17 == complete and /prod12/nam/nest_conus/post/jnam_post_f18 == complete and /prod12/nam/sminit_conus2p5/jnam_sminit_f15 == complete
      task jnam_sminit_f21
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_conus/post/jnam_post_f19 == complete and /prod12/nam/nest_conus/post/jnam_post_f20 == complete and /prod12/nam/nest_conus/post/jnam_post_f21 == complete and /prod12/nam/sminit_conus2p5/jnam_sminit_f18 == complete
      task jnam_sminit_f24
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_conus/post/jnam_post_f22 == complete and /prod12/nam/nest_conus/post/jnam_post_f23 == complete and /prod12/nam/nest_conus/post/jnam_post_f24 == complete and /prod12/nam/sminit_conus2p5/jnam_sminit_f21 == complete
      task jnam_sminit_f27
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_conus/post/jnam_post_f25 == complete and /prod12/nam/nest_conus/post/jnam_post_f26 == complete and /prod12/nam/nest_conus/post/jnam_post_f27 == complete
      task jnam_sminit_f30
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_conus/post/jnam_post_f28 == complete and /prod12/nam/nest_conus/post/jnam_post_f29 == complete and /prod12/nam/nest_conus/post/jnam_post_f30 == complete and /prod12/nam/sminit_conus2p5/jnam_sminit_f27 == complete
      task jnam_sminit_f33
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_conus/post/jnam_post_f31 == complete and /prod12/nam/nest_conus/post/jnam_post_f32 == complete and /prod12/nam/nest_conus/post/jnam_post_f33 == complete and /prod12/nam/sminit_conus2p5/jnam_sminit_f30 == complete
      task jnam_sminit_f36
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_conus/post/jnam_post_f34 == complete and /prod12/nam/nest_conus/post/jnam_post_f35 == complete and /prod12/nam/nest_conus/post/jnam_post_f36 == complete and /prod12/nam/sminit_conus2p5/jnam_sminit_f33 == complete
      task jnam_sminit_f39
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_conus/post/jnam_post_f37 == complete and /prod12/nam/nest_conus/post/jnam_post_f38 == complete and /prod12/nam/nest_conus/post/jnam_post_f39 == complete
      task jnam_sminit_f42
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_conus/post/jnam_post_f40 == complete and /prod12/nam/nest_conus/post/jnam_post_f41 == complete and /prod12/nam/nest_conus/post/jnam_post_f42 == complete and /prod12/nam/sminit_conus2p5/jnam_sminit_f39 == complete
      task jnam_sminit_f45
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_conus/post/jnam_post_f43 == complete and /prod12/nam/nest_conus/post/jnam_post_f44 == complete and /prod12/nam/nest_conus/post/jnam_post_f45 == complete and /prod12/nam/sminit_conus2p5/jnam_sminit_f42 == complete
      task jnam_sminit_f48
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_conus/post/jnam_post_f46 == complete and /prod12/nam/nest_conus/post/jnam_post_f47 == complete and /prod12/nam/nest_conus/post/jnam_post_f48 == complete and /prod12/nam/sminit_conus2p5/jnam_sminit_f45 == complete
      task jnam_sminit_f51
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_conus/post/jnam_post_f49 == complete and /prod12/nam/nest_conus/post/jnam_post_f50 == complete and /prod12/nam/nest_conus/post/jnam_post_f51 == complete
      task jnam_sminit_f54
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_conus/post/jnam_post_f52 == complete and /prod12/nam/nest_conus/post/jnam_post_f53 == complete and /prod12/nam/nest_conus/post/jnam_post_f54 == complete and /prod12/nam/sminit_conus2p5/jnam_sminit_f51 == complete
      task jnam_sminit_f57
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_conus/post/jnam_post_f55 == complete and /prod12/nam/nest_conus/post/jnam_post_f56 == complete and /prod12/nam/nest_conus/post/jnam_post_f57 == complete and /prod12/nam/sminit_conus2p5/jnam_sminit_f54 == complete
      task jnam_sminit_f60
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_212_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/nam/nest_conus/post/jnam_post_f58 == complete and /prod12/nam/nest_conus/post/jnam_post_f59 == complete and /prod12/nam/nest_conus/post/jnam_post_f60 == complete and /prod12/nam/sminit_conus2p5/jnam_sminit_f57 == complete
    endfamily
    family nest_conus
      edit ECF_FILES '/ecf/ecfnets/scripts/nam/nest'
      edit grid 'conus'
      edit NTASK '16'
      family profile
        task jnam_profile_f00
          trigger /prod12/nam/nest_conus/post/jnam_post_manager:release_post00
        task jnam_profile_f01
          trigger /prod12/nam/nest_conus/post/jnam_post_manager:release_post01
        task jnam_profile_f02
          trigger /prod12/nam/nest_conus/post/jnam_post_manager:release_post02
        task jnam_profile_f03
          trigger /prod12/nam/nest_conus/post/jnam_post_manager:release_post03
        task jnam_profile_f04
          trigger /prod12/nam/nest_conus/post/jnam_post_manager:release_post04
        task jnam_profile_f05
          trigger /prod12/nam/nest_conus/post/jnam_post_manager:release_post05
        task jnam_profile_f06
          trigger /prod12/nam/nest_conus/post/jnam_post_manager:release_post06
        task jnam_profile_f07
          trigger /prod12/nam/nest_conus/post/jnam_post_manager:release_post07
        task jnam_profile_f08
          trigger /prod12/nam/nest_conus/post/jnam_post_manager:release_post08
        task jnam_profile_f09
          trigger /prod12/nam/nest_conus/post/jnam_post_manager:release_post09
        task jnam_profile_f10
          trigger /prod12/nam/nest_conus/post/jnam_post_manager:release_post10
        task jnam_profile_f11
          trigger /prod12/nam/nest_conus/post/jnam_post_manager:release_post11
        task jnam_profile_f12
          trigger /prod12/nam/nest_conus/post/jnam_post_manager:release_post12
        task jnam_profile_f13
          trigger /prod12/nam/nest_conus/post/jnam_post_manager:release_post13
        task jnam_profile_f14
          trigger /prod12/nam/nest_conus/post/jnam_post_manager:release_post14
        task jnam_profile_f15
          trigger /prod12/nam/nest_conus/post/jnam_post_manager:release_post15
        task jnam_profile_f16
          trigger /prod12/nam/nest_conus/post/jnam_post_manager:release_post16
        task jnam_profile_f17
          trigger /prod12/nam/nest_conus/post/jnam_post_manager:release_post17
        task jnam_profile_f18
          trigger /prod12/nam/nest_conus/post/jnam_post_manager:release_post18
        task jnam_profile_f19
          trigger /prod12/nam/nest_conus/post/jnam_post_manager:release_post19
        task jnam_profile_f20
          trigger /prod12/nam/nest_conus/post/jnam_post_manager:release_post20
        task jnam_profile_f21
          trigger /prod12/nam/nest_conus/post/jnam_post_manager:release_post21
        task jnam_profile_f22
          trigger /prod12/nam/nest_conus/post/jnam_post_manager:release_post22
        task jnam_profile_f23
          trigger /prod12/nam/nest_conus/post/jnam_post_manager:release_post23
        task jnam_profile_f24
          trigger /prod12/nam/nest_conus/post/jnam_post_manager:release_post24
        task jnam_profile_f25
          trigger /prod12/nam/nest_conus/post/jnam_post_manager:release_post25
        task jnam_profile_f26
          trigger /prod12/nam/nest_conus/post/jnam_post_manager:release_post26
        task jnam_profile_f27
          trigger /prod12/nam/nest_conus/post/jnam_post_manager:release_post27
        task jnam_profile_f28
          trigger /prod12/nam/nest_conus/post/jnam_post_manager:release_post28
        task jnam_profile_f29
          trigger /prod12/nam/nest_conus/post/jnam_post_manager:release_post29
        task jnam_profile_f30
          trigger /prod12/nam/nest_conus/post/jnam_post_manager:release_post30
        task jnam_profile_f31
          trigger /prod12/nam/nest_conus/post/jnam_post_manager:release_post31
        task jnam_profile_f32
          trigger /prod12/nam/nest_conus/post/jnam_post_manager:release_post32
        task jnam_profile_f33
          trigger /prod12/nam/nest_conus/post/jnam_post_manager:release_post33
        task jnam_profile_f34
          trigger /prod12/nam/nest_conus/post/jnam_post_manager:release_post34
        task jnam_profile_f35
          trigger /prod12/nam/nest_conus/post/jnam_post_manager:release_post35
        task jnam_profile_f36
          trigger /prod12/nam/nest_conus/post/jnam_post_manager:release_post36
        task jnam_profile_f37
          trigger /prod12/nam/nest_conus/post/jnam_post_manager:release_post37
        task jnam_profile_f38
          trigger /prod12/nam/nest_conus/post/jnam_post_manager:release_post38
        task jnam_profile_f39
          trigger /prod12/nam/nest_conus/post/jnam_post_manager:release_post39
        task jnam_profile_f40
          trigger /prod12/nam/nest_conus/post/jnam_post_manager:release_post40
        task jnam_profile_f41
          trigger /prod12/nam/nest_conus/post/jnam_post_manager:release_post41
        task jnam_profile_f42
          trigger /prod12/nam/nest_conus/post/jnam_post_manager:release_post42
        task jnam_profile_f43
          trigger /prod12/nam/nest_conus/post/jnam_post_manager:release_post43
        task jnam_profile_f44
          trigger /prod12/nam/nest_conus/post/jnam_post_manager:release_post44
        task jnam_profile_f45
          trigger /prod12/nam/nest_conus/post/jnam_post_manager:release_post45
        task jnam_profile_f46
          trigger /prod12/nam/nest_conus/post/jnam_post_manager:release_post46
        task jnam_profile_f47
          trigger /prod12/nam/nest_conus/post/jnam_post_manager:release_post47
        task jnam_profile_f48
          trigger /prod12/nam/nest_conus/post/jnam_post_manager:release_post48
        task jnam_profile_f49
          trigger /prod12/nam/nest_conus/post/jnam_post_manager:release_post49
        task jnam_profile_f50
          trigger /prod12/nam/nest_conus/post/jnam_post_manager:release_post50
        task jnam_profile_f51
          trigger /prod12/nam/nest_conus/post/jnam_post_manager:release_post51
        task jnam_profile_f52
          trigger /prod12/nam/nest_conus/post/jnam_post_manager:release_post52
        task jnam_profile_f53
          trigger /prod12/nam/nest_conus/post/jnam_post_manager:release_post53
        task jnam_profile_f54
          trigger /prod12/nam/nest_conus/post/jnam_post_manager:release_post54
        task jnam_profile_f55
          trigger /prod12/nam/nest_conus/post/jnam_post_manager:release_post55
        task jnam_profile_f56
          trigger /prod12/nam/nest_conus/post/jnam_post_manager:release_post56
        task jnam_profile_f57
          trigger /prod12/nam/nest_conus/post/jnam_post_manager:release_post57
        task jnam_profile_f58
          trigger /prod12/nam/nest_conus/post/jnam_post_manager:release_post58
        task jnam_profile_f59
          trigger /prod12/nam/nest_conus/post/jnam_post_manager:release_post59
        task jnam_profile_f60
          trigger /prod12/nam/nest_conus/post/jnam_post_manager:release_post60
      endfamily
      family post
        task jnam_post_manager
          trigger /prod12/nam/forecast == active or /prod12/nam/forecast == complete
          event 1 release_post00
          event 2 release_post01
          event 3 release_post02
          event 4 release_post03
          event 5 release_post04
          event 6 release_post05
          event 7 release_post06
          event 8 release_post07
          event 9 release_post08
          event 10 release_post09
          event 11 release_post10
          event 12 release_post11
          event 13 release_post12
          event 14 release_post13
          event 15 release_post14
          event 16 release_post15
          event 17 release_post16
          event 18 release_post17
          event 19 release_post18
          event 20 release_post19
          event 21 release_post20
          event 22 release_post21
          event 23 release_post22
          event 24 release_post23
          event 25 release_post24
          event 26 release_post25
          event 27 release_post26
          event 28 release_post27
          event 29 release_post28
          event 30 release_post29
          event 31 release_post30
          event 32 release_post31
          event 33 release_post32
          event 34 release_post33
          event 35 release_post34
          event 36 release_post35
          event 37 release_post36
          event 38 release_post37
          event 39 release_post38
          event 40 release_post39
          event 41 release_post40
          event 42 release_post41
          event 43 release_post42
          event 44 release_post43
          event 45 release_post44
          event 46 release_post45
          event 47 release_post46
          event 48 release_post47
          event 49 release_post48
          event 50 release_post49
          event 51 release_post50
          event 52 release_post51
          event 53 release_post52
          event 54 release_post53
          event 55 release_post54
          event 56 release_post55
          event 57 release_post56
          event 58 release_post57
          event 59 release_post58
          event 60 release_post59
          event 61 release_post60
        task jnam_post_f00
          trigger jnam_post_manager:release_post00
        task jnam_post_f01
          trigger jnam_post_manager:release_post01
        task jnam_post_f02
          trigger jnam_post_manager:release_post02
        task jnam_post_f03
          trigger jnam_post_manager:release_post03
        task jnam_post_f04
          trigger jnam_post_manager:release_post04
        task jnam_post_f05
          trigger jnam_post_manager:release_post05
        task jnam_post_f06
          trigger jnam_post_manager:release_post06
        task jnam_post_f07
          trigger jnam_post_manager:release_post07
        task jnam_post_f08
          trigger jnam_post_manager:release_post08
        task jnam_post_f09
          trigger jnam_post_manager:release_post09
        task jnam_post_f10
          trigger jnam_post_manager:release_post10
        task jnam_post_f11
          trigger jnam_post_manager:release_post11
        task jnam_post_f12
          trigger jnam_post_manager:release_post12
        task jnam_post_f13
          trigger jnam_post_manager:release_post13
        task jnam_post_f14
          trigger jnam_post_manager:release_post14
        task jnam_post_f15
          trigger jnam_post_manager:release_post15
        task jnam_post_f16
          trigger jnam_post_manager:release_post16
        task jnam_post_f17
          trigger jnam_post_manager:release_post17
        task jnam_post_f18
          trigger jnam_post_manager:release_post18
        task jnam_post_f19
          trigger jnam_post_manager:release_post19
        task jnam_post_f20
          trigger jnam_post_manager:release_post20
        task jnam_post_f21
          trigger jnam_post_manager:release_post21
        task jnam_post_f22
          trigger jnam_post_manager:release_post22
        task jnam_post_f23
          trigger jnam_post_manager:release_post23
        task jnam_post_f24
          trigger jnam_post_manager:release_post24
        task jnam_post_f25
          trigger jnam_post_manager:release_post25
        task jnam_post_f26
          trigger jnam_post_manager:release_post26
        task jnam_post_f27
          trigger jnam_post_manager:release_post27
        task jnam_post_f28
          trigger jnam_post_manager:release_post28
        task jnam_post_f29
          trigger jnam_post_manager:release_post29
        task jnam_post_f30
          trigger jnam_post_manager:release_post30
        task jnam_post_f31
          trigger jnam_post_manager:release_post31
        task jnam_post_f32
          trigger jnam_post_manager:release_post32
        task jnam_post_f33
          trigger jnam_post_manager:release_post33
        task jnam_post_f34
          trigger jnam_post_manager:release_post34
        task jnam_post_f35
          trigger jnam_post_manager:release_post35
        task jnam_post_f36
          trigger jnam_post_manager:release_post36
        task jnam_post_f37
          trigger jnam_post_manager:release_post37
        task jnam_post_f38
          trigger jnam_post_manager:release_post38
        task jnam_post_f39
          trigger jnam_post_manager:release_post39
        task jnam_post_f40
          trigger jnam_post_manager:release_post40
        task jnam_post_f41
          trigger jnam_post_manager:release_post41
        task jnam_post_f42
          trigger jnam_post_manager:release_post42
        task jnam_post_f43
          trigger jnam_post_manager:release_post43
        task jnam_post_f44
          trigger jnam_post_manager:release_post44
        task jnam_post_f45
          trigger jnam_post_manager:release_post45
        task jnam_post_f46
          trigger jnam_post_manager:release_post46
        task jnam_post_f47
          trigger jnam_post_manager:release_post47
        task jnam_post_f48
          trigger jnam_post_manager:release_post48
        task jnam_post_f49
          trigger jnam_post_manager:release_post49
        task jnam_post_f50
          trigger jnam_post_manager:release_post50
        task jnam_post_f51
          trigger jnam_post_manager:release_post51
        task jnam_post_f52
          trigger jnam_post_manager:release_post52
        task jnam_post_f53
          trigger jnam_post_manager:release_post53
        task jnam_post_f54
          trigger jnam_post_manager:release_post54
        task jnam_post_f55
          trigger jnam_post_manager:release_post55
        task jnam_post_f56
          trigger jnam_post_manager:release_post56
        task jnam_post_f57
          trigger jnam_post_manager:release_post57
        task jnam_post_f58
          trigger jnam_post_manager:release_post58
        task jnam_post_f59
          trigger jnam_post_manager:release_post59
        task jnam_post_f60
          trigger jnam_post_manager:release_post60
        task jnam_post_goestb
          trigger jnam_post_f00 == complete
      endfamily
      family prdgen
        task jnam_prdgen_f00
          trigger /prod12/nam/nest_conus/post/jnam_post_f00 == complete
        task jnam_prdgen_f01
          trigger /prod12/nam/nest_conus/post/jnam_post_f01 == complete
        task jnam_prdgen_f02
          trigger /prod12/nam/nest_conus/post/jnam_post_f02 == complete
        task jnam_prdgen_f03
          trigger /prod12/nam/nest_conus/post/jnam_post_f03 == complete
        task jnam_prdgen_f04
          trigger /prod12/nam/nest_conus/post/jnam_post_f04 == complete
        task jnam_prdgen_f05
          trigger /prod12/nam/nest_conus/post/jnam_post_f05 == complete
        task jnam_prdgen_f06
          trigger /prod12/nam/nest_conus/post/jnam_post_f06 == complete
        task jnam_prdgen_f07
          trigger /prod12/nam/nest_conus/post/jnam_post_f07 == complete
        task jnam_prdgen_f08
          trigger /prod12/nam/nest_conus/post/jnam_post_f08 == complete
        task jnam_prdgen_f09
          trigger /prod12/nam/nest_conus/post/jnam_post_f09 == complete
        task jnam_prdgen_f10
          trigger /prod12/nam/nest_conus/post/jnam_post_f10 == complete
        task jnam_prdgen_f11
          trigger /prod12/nam/nest_conus/post/jnam_post_f11 == complete
        task jnam_prdgen_f12
          trigger /prod12/nam/nest_conus/post/jnam_post_f12 == complete
        task jnam_prdgen_f13
          trigger /prod12/nam/nest_conus/post/jnam_post_f13 == complete
        task jnam_prdgen_f14
          trigger /prod12/nam/nest_conus/post/jnam_post_f14 == complete
        task jnam_prdgen_f15
          trigger /prod12/nam/nest_conus/post/jnam_post_f15 == complete
        task jnam_prdgen_f16
          trigger /prod12/nam/nest_conus/post/jnam_post_f16 == complete
        task jnam_prdgen_f17
          trigger /prod12/nam/nest_conus/post/jnam_post_f17 == complete
        task jnam_prdgen_f18
          trigger /prod12/nam/nest_conus/post/jnam_post_f18 == complete
        task jnam_prdgen_f19
          trigger /prod12/nam/nest_conus/post/jnam_post_f19 == complete
        task jnam_prdgen_f20
          trigger /prod12/nam/nest_conus/post/jnam_post_f20 == complete
        task jnam_prdgen_f21
          trigger /prod12/nam/nest_conus/post/jnam_post_f21 == complete
        task jnam_prdgen_f22
          trigger /prod12/nam/nest_conus/post/jnam_post_f22 == complete
        task jnam_prdgen_f23
          trigger /prod12/nam/nest_conus/post/jnam_post_f23 == complete
        task jnam_prdgen_f24
          trigger /prod12/nam/nest_conus/post/jnam_post_f24 == complete
        task jnam_prdgen_f25
          trigger /prod12/nam/nest_conus/post/jnam_post_f25 == complete
        task jnam_prdgen_f26
          trigger /prod12/nam/nest_conus/post/jnam_post_f26 == complete
        task jnam_prdgen_f27
          trigger /prod12/nam/nest_conus/post/jnam_post_f27 == complete
        task jnam_prdgen_f28
          trigger /prod12/nam/nest_conus/post/jnam_post_f28 == complete
        task jnam_prdgen_f29
          trigger /prod12/nam/nest_conus/post/jnam_post_f29 == complete
        task jnam_prdgen_f30
          trigger /prod12/nam/nest_conus/post/jnam_post_f30 == complete
        task jnam_prdgen_f31
          trigger /prod12/nam/nest_conus/post/jnam_post_f31 == complete
        task jnam_prdgen_f32
          trigger /prod12/nam/nest_conus/post/jnam_post_f32 == complete
        task jnam_prdgen_f33
          trigger /prod12/nam/nest_conus/post/jnam_post_f33 == complete
        task jnam_prdgen_f34
          trigger /prod12/nam/nest_conus/post/jnam_post_f34 == complete
        task jnam_prdgen_f35
          trigger /prod12/nam/nest_conus/post/jnam_post_f35 == complete
        task jnam_prdgen_f36
          trigger /prod12/nam/nest_conus/post/jnam_post_f36 == complete
        task jnam_prdgen_f39
          trigger /prod12/nam/nest_conus/post/jnam_post_f39 == complete
        task jnam_prdgen_f42
          trigger /prod12/nam/nest_conus/post/jnam_post_f42 == complete
        task jnam_prdgen_f45
          trigger /prod12/nam/nest_conus/post/jnam_post_f45 == complete
        task jnam_prdgen_f48
          trigger /prod12/nam/nest_conus/post/jnam_post_f48 == complete
        task jnam_prdgen_f51
          trigger /prod12/nam/nest_conus/post/jnam_post_f51 == complete
        task jnam_prdgen_f54
          trigger /prod12/nam/nest_conus/post/jnam_post_f54 == complete
        task jnam_prdgen_f57
          trigger /prod12/nam/nest_conus/post/jnam_post_f57 == complete
        task jnam_prdgen_f60
          trigger /prod12/nam/nest_conus/post/jnam_post_f60 == complete
      endfamily
    endfamily
    task jnam_cleanup
      trigger /prod12/nam/analysis == complete and /prod12/nam/post == complete and /prod12/nam/prdgen == complete and /prod12/nam/forecast == complete and /prod12/nam/nest_conus == complete and /prod12/nam/nest_alaska == complete and /prod12/nam/nest_hawaii == complete and /prod12/nam/nest_prico == complete and /prod12/nam/nest_firewx == complete and /prod12/nam/post_processing/bufr_sounding/jnam_postsnd == complete
  endfamily
  family naefs
    edit PROJ 'GEN-OPS'
    edit ECF_FILES '/ecf/ecfnets/scripts'
    edit CYC '12'
    edit QUEUE 'prod2'
    edit QUEUESERV 'prod2_serv2'
    edit COM 'com2'
    task jnaefs_dvrtma_bias_ak
      trigger /prod12/rtma/rtma_12/rtma2p5/jrtma2p5_post == complete and /prod12/gefs/post_low == complete
    task jnaefs_dvrtma_bias_conus
      trigger /prod12/rtma/rtma_12/rtma2p5/jrtma2p5_post == complete and /prod12/gefs/post_low == complete
    task jnaefs_prob_avgspr
      trigger /prod12/gefs/post_processing/jgefs_debias == complete
    task jnaefs_dvrtma_prob_avgspr_ak
      trigger jnaefs_dvrtma_bias_ak == complete and jnaefs_prob_avgspr == complete
    task jnaefs_dvrtma_prob_avgspr_conus
      trigger jnaefs_prob_avgspr == complete
    family gempak
      task jnaefs_dvrtma_bias_conus_gempak
        trigger ../jnaefs_dvrtma_bias_conus == complete
      task jnaefs_dvrtma_bias_ak_gempak
        trigger ../jnaefs_dvrtma_bias_ak == complete
      task jnaefs_prob_avgspr_gempak
        trigger ../jnaefs_prob_avgspr == complete
      task jnaefs_dvrtma_prob_avgspr_ak_gempak
        trigger ../jnaefs_dvrtma_prob_avgspr_ak == complete
      task jnaefs_dvrtma_prob_avgspr_conus_gempak
        trigger ../jnaefs_dvrtma_prob_avgspr_conus == complete
    endfamily
  endfamily
  family ens_tracker
    edit QUEUE 'prod2'
    edit PROJ 'GEN'
    edit ECF_FILES '/ecf/ecfnets/scripts/ens_tracker'
    family gefs
      task jgfs_tc_track
        trigger /prod12/gfs/post/jgfs_post_f240 == complete
      task jgfs_tc_genesis
        trigger /prod12/gfs/post/jgfs_post_f120 == complete
      task jgefs_tc_track
        trigger ../../gefs/prdgen_low == complete and ( jgfs_tc_track == active or jgfs_tc_track == complete )
      task jgefs_tc_genesis
        trigger ../../gefs/prdgen_low == complete and jgfs_tc_genesis == complete
    endfamily
    family fens
      task jnavgem_tc_track
        trigger /prod12/navgem/jnavgem_prdgen == complete
      task jnavgem_tc_genesis
        trigger /prod12/navgem/jnavgem_prdgen == complete
      task jfens_tc_track
        time 18:15
      task jfens_tc_genesis
        time 17:44
    endfamily
    family cmce
      task jcmc_tc_track
        trigger ../gefs/jgefs_tc_track == complete
        time 17:16
      task jcmc_tc_genesis
        time 17:10
      task jcens_tc_track
        trigger /prod18/cmcens/jcmc_ens_post_12 == complete and ( jcmc_tc_track == active or jcmc_tc_track == complete )
      task jcens_tc_genesis
        trigger /prod18/cmcens/jcmc_ens_post_12 == complete and ( jcmc_tc_track == active or jcmc_tc_track == complete )
    endfamily
    family ecme
      task jecmwf_tc_track
        time 19:00
      task jecmwf_tc_genesis
        time 18:30
      task jeens_grib
        time 20:30
      task jeens_tc_track
        trigger jeens_grib == complete
      task jeens_tc_genesis
        trigger jeens_grib == complete
    endfamily
  endfamily
  family dgex
    edit ECF_FILES '/ecf/ecfnets/scripts/dgex'
    edit PROJ 'DGEX-OPS'
    task jdgex_prelim
      trigger /prod12/nam/prdgen/jnam_prdgen_f60 eq complete
      label NEST " "
      event 1 release_mkbnd
    task jdgex_prep
      trigger jdgex_prelim eq complete and /prod12/nam/prdgen/jnam_prdgen_f78 eq complete and /prod12/nam/prdgen/jnam_prdgen_f81 eq complete
    task jdgex_mkbnd
      trigger jdgex_prelim:release_mkbnd
    task jdgex_sfcupdate
      trigger jdgex_prep eq complete
    task jdgex_combc
      trigger jdgex_mkbnd eq complete
    task jdgex_forecast
      trigger jdgex_combc eq complete and jdgex_sfcupdate eq complete
    family post
      task jdgex_post_manager
        trigger /prod12/dgex/jdgex_forecast eq active
        event 1 release_post03
        event 2 release_post06
        event 3 release_post09
        event 4 release_post12
        event 5 release_post15
        event 6 release_post18
        event 7 release_post21
        event 8 release_post24
        event 9 release_post27
        event 10 release_post30
        event 11 release_post33
        event 12 release_post36
        event 13 release_post39
        event 14 release_post42
        event 15 release_post45
        event 16 release_post48
        event 17 release_post51
        event 18 release_post54
        event 19 release_post57
        event 20 release_post60
        event 21 release_post63
        event 22 release_post66
        event 23 release_post69
        event 24 release_post72
        event 25 release_post75
        event 26 release_post78
        event 27 release_post81
        event 28 release_post84
        event 29 release_post87
        event 30 release_post90
        event 31 release_post93
        event 32 release_post96
        event 33 release_post99
        event 34 release_post102
        event 35 release_post105
        event 36 release_post108
        event 37 release_post111
        event 38 release_post114
      task jdgex_post_f03
        trigger jdgex_post_manager:release_post03
      task jdgex_post_f06
        trigger jdgex_post_manager:release_post06
      task jdgex_post_f09
        trigger jdgex_post_manager:release_post09
      task jdgex_post_f12
        trigger jdgex_post_manager:release_post12
      task jdgex_post_f15
        trigger jdgex_post_manager:release_post15
      task jdgex_post_f18
        trigger jdgex_post_manager:release_post18
      task jdgex_post_f21
        trigger jdgex_post_manager:release_post21
      task jdgex_post_f24
        trigger jdgex_post_manager:release_post24
      task jdgex_post_f27
        trigger jdgex_post_manager:release_post27
      task jdgex_post_f30
        trigger jdgex_post_manager:release_post30
      task jdgex_post_f33
        trigger jdgex_post_manager:release_post33
      task jdgex_post_f36
        trigger jdgex_post_manager:release_post36
      task jdgex_post_f39
        trigger jdgex_post_manager:release_post39
      task jdgex_post_f42
        trigger jdgex_post_manager:release_post42
      task jdgex_post_f45
        trigger jdgex_post_manager:release_post45
      task jdgex_post_f48
        trigger jdgex_post_manager:release_post48
      task jdgex_post_f51
        trigger jdgex_post_manager:release_post51
      task jdgex_post_f54
        trigger jdgex_post_manager:release_post54
      task jdgex_post_f57
        trigger jdgex_post_manager:release_post57
      task jdgex_post_f60
        trigger jdgex_post_manager:release_post60
      task jdgex_post_f63
        trigger jdgex_post_manager:release_post63
      task jdgex_post_f66
        trigger jdgex_post_manager:release_post66
      task jdgex_post_f69
        trigger jdgex_post_manager:release_post69
      task jdgex_post_f72
        trigger jdgex_post_manager:release_post72
      task jdgex_post_f75
        trigger jdgex_post_manager:release_post75
      task jdgex_post_f78
        trigger jdgex_post_manager:release_post78
      task jdgex_post_f81
        trigger jdgex_post_manager:release_post81
      task jdgex_post_f84
        trigger jdgex_post_manager:release_post84
      task jdgex_post_f87
        trigger jdgex_post_manager:release_post87
      task jdgex_post_f90
        trigger jdgex_post_manager:release_post90
      task jdgex_post_f93
        trigger jdgex_post_manager:release_post93
      task jdgex_post_f96
        trigger jdgex_post_manager:release_post96
      task jdgex_post_f99
        trigger jdgex_post_manager:release_post99
      task jdgex_post_f102
        trigger jdgex_post_manager:release_post102
      task jdgex_post_f105
        trigger jdgex_post_manager:release_post105
      task jdgex_post_f108
        trigger jdgex_post_manager:release_post108
      task jdgex_post_f111
        trigger jdgex_post_manager:release_post111
      task jdgex_post_f114
        trigger jdgex_post_manager:release_post114
    endfamily
    family prdgen
      task jdgex_prdgen_f81
        trigger /prod12/dgex/post/jdgex_post_f03 eq complete
      task jdgex_prdgen_f84
        trigger /prod12/dgex/post/jdgex_post_f06 eq complete
      task jdgex_prdgen_f87
        trigger /prod12/dgex/post/jdgex_post_f09 eq complete
      task jdgex_prdgen_f90
        trigger /prod12/dgex/post/jdgex_post_f12 eq complete
      task jdgex_prdgen_f93
        trigger /prod12/dgex/post/jdgex_post_f15 eq complete
      task jdgex_prdgen_f96
        trigger /prod12/dgex/post/jdgex_post_f18 eq complete
      task jdgex_prdgen_f99
        trigger /prod12/dgex/post/jdgex_post_f21 eq complete
      task jdgex_prdgen_f102
        trigger /prod12/dgex/post/jdgex_post_f24 eq complete
      task jdgex_prdgen_f105
        trigger /prod12/dgex/post/jdgex_post_f27 eq complete
      task jdgex_prdgen_f108
        trigger /prod12/dgex/post/jdgex_post_f30 eq complete
      task jdgex_prdgen_f111
        trigger /prod12/dgex/post/jdgex_post_f33 eq complete
      task jdgex_prdgen_f114
        trigger /prod12/dgex/post/jdgex_post_f36 eq complete
      task jdgex_prdgen_f117
        trigger /prod12/dgex/post/jdgex_post_f39 eq complete
      task jdgex_prdgen_f120
        trigger /prod12/dgex/post/jdgex_post_f42 eq complete
      task jdgex_prdgen_f123
        trigger /prod12/dgex/post/jdgex_post_f45 eq complete
      task jdgex_prdgen_f126
        trigger /prod12/dgex/post/jdgex_post_f48 eq complete
      task jdgex_prdgen_f129
        trigger /prod12/dgex/post/jdgex_post_f51 eq complete
      task jdgex_prdgen_f132
        trigger /prod12/dgex/post/jdgex_post_f54 eq complete
      task jdgex_prdgen_f135
        trigger /prod12/dgex/post/jdgex_post_f57 eq complete
      task jdgex_prdgen_f138
        trigger /prod12/dgex/post/jdgex_post_f60 eq complete
      task jdgex_prdgen_f141
        trigger /prod12/dgex/post/jdgex_post_f63 eq complete
      task jdgex_prdgen_f144
        trigger /prod12/dgex/post/jdgex_post_f66 eq complete
      task jdgex_prdgen_f147
        trigger /prod12/dgex/post/jdgex_post_f69 eq complete
      task jdgex_prdgen_f150
        trigger /prod12/dgex/post/jdgex_post_f72 eq complete
      task jdgex_prdgen_f153
        trigger /prod12/dgex/post/jdgex_post_f75 eq complete
      task jdgex_prdgen_f156
        trigger /prod12/dgex/post/jdgex_post_f78 eq complete
      task jdgex_prdgen_f159
        trigger /prod12/dgex/post/jdgex_post_f81 eq complete
      task jdgex_prdgen_f162
        trigger /prod12/dgex/post/jdgex_post_f84 eq complete
      task jdgex_prdgen_f165
        trigger /prod12/dgex/post/jdgex_post_f87 eq complete
      task jdgex_prdgen_f168
        trigger /prod12/dgex/post/jdgex_post_f90 eq complete
      task jdgex_prdgen_f171
        trigger /prod12/dgex/post/jdgex_post_f93 eq complete
      task jdgex_prdgen_f174
        trigger /prod12/dgex/post/jdgex_post_f96 eq complete
      task jdgex_prdgen_f177
        trigger /prod12/dgex/post/jdgex_post_f99 eq complete
      task jdgex_prdgen_f180
        trigger /prod12/dgex/post/jdgex_post_f102 eq complete
      task jdgex_prdgen_f183
        trigger /prod12/dgex/post/jdgex_post_f105 eq complete
      task jdgex_prdgen_f186
        trigger /prod12/dgex/post/jdgex_post_f108 eq complete
      task jdgex_prdgen_f189
        trigger /prod12/dgex/post/jdgex_post_f111 eq complete
      task jdgex_prdgen_f192
        trigger /prod12/dgex/post/jdgex_post_f114 eq complete
    endfamily
    task jdgex_cleanup
      trigger /prod12/dgex/jdgex_forecast eq complete and /prod12/dgex/post eq complete and /prod12/dgex/prdgen eq complete
    family gempak
      task jdgex_gempak
        trigger /prod12/dgex/prdgen/jdgex_prdgen_f192 eq complete
      task jdgex_gempak_meta
        trigger jdgex_gempak eq complete
    endfamily
    family sminit_ak
      edit ECF_FILES '/ecf/ecfnets/scripts/dgex/sminit'
      edit RUNTYP 'dgex_ak'
      edit DOMAIN 'ak'
      task jdgex_sminit_f84
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/dgex/post/jdgex_post_f06 == complete
      task jdgex_sminit_f87
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/dgex/post/jdgex_post_f09 == complete and jdgex_sminit_f84 == complete
      task jdgex_sminit_f90
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/dgex/post/jdgex_post_f12 == complete and jdgex_sminit_f87 == complete
      task jdgex_sminit_f93
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/dgex/post/jdgex_post_f15 == complete and jdgex_sminit_f90 == complete
      task jdgex_sminit_f96
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/dgex/post/jdgex_post_f18 == complete and jdgex_sminit_f93 == complete
      task jdgex_sminit_f99
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/dgex/post/jdgex_post_f21 == complete
      task jdgex_sminit_f102
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/dgex/post/jdgex_post_f24 == complete and jdgex_sminit_f99 == complete
      task jdgex_sminit_f105
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/dgex/post/jdgex_post_f27 == complete and jdgex_sminit_f102 == complete
      task jdgex_sminit_f108
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/dgex/post/jdgex_post_f30 == complete and jdgex_sminit_f105 == complete
      task jdgex_sminit_f111
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/dgex/post/jdgex_post_f33 == complete
      task jdgex_sminit_f114
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/dgex/post/jdgex_post_f36 == complete and jdgex_sminit_f111 == complete
      task jdgex_sminit_f117
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/dgex/post/jdgex_post_f39 == complete and jdgex_sminit_f114 == complete
      task jdgex_sminit_f120
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/dgex/post/jdgex_post_f42 == complete and jdgex_sminit_f117 == complete
      task jdgex_sminit_f123
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/dgex/post/jdgex_post_f45 == complete
      task jdgex_sminit_f126
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/dgex/post/jdgex_post_f48 == complete and jdgex_sminit_f123 == complete
      task jdgex_sminit_f129
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/dgex/post/jdgex_post_f51 == complete and jdgex_sminit_f126 == complete
      task jdgex_sminit_f132
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/dgex/post/jdgex_post_f54 == complete and jdgex_sminit_f129 == complete
      task jdgex_sminit_f135
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/dgex/post/jdgex_post_f57 == complete
      task jdgex_sminit_f138
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/dgex/post/jdgex_post_f60 == complete and jdgex_sminit_f135 == complete
      task jdgex_sminit_f141
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/dgex/post/jdgex_post_f63 == complete and jdgex_sminit_f138 == complete
      task jdgex_sminit_f144
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/dgex/post/jdgex_post_f66 == complete and jdgex_sminit_f141 == complete
      task jdgex_sminit_f147
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/dgex/post/jdgex_post_f69 == complete
      task jdgex_sminit_f150
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/dgex/post/jdgex_post_f72 == complete and jdgex_sminit_f147 == complete
      task jdgex_sminit_f153
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/dgex/post/jdgex_post_f75 == complete and jdgex_sminit_f150 == complete
      task jdgex_sminit_f156
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/dgex/post/jdgex_post_f78 == complete and jdgex_sminit_f153 == complete
      task jdgex_sminit_f159
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/dgex/post/jdgex_post_f81 == complete
      task jdgex_sminit_f162
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/dgex/post/jdgex_post_f84 == complete and jdgex_sminit_f159 == complete
      task jdgex_sminit_f165
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/dgex/post/jdgex_post_f87 == complete and jdgex_sminit_f162 == complete
      task jdgex_sminit_f168
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/dgex/post/jdgex_post_f90 == complete and jdgex_sminit_f165 == complete
      task jdgex_sminit_f171
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/dgex/post/jdgex_post_f93 == complete
      task jdgex_sminit_f174
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/dgex/post/jdgex_post_f96 == complete and jdgex_sminit_f171 == complete
      task jdgex_sminit_f177
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/dgex/post/jdgex_post_f99 == complete and jdgex_sminit_f174 == complete
      task jdgex_sminit_f180
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/dgex/post/jdgex_post_f102 == complete and jdgex_sminit_f177 == complete
      task jdgex_sminit_f183
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/dgex/post/jdgex_post_f105 == complete
      task jdgex_sminit_f186
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/dgex/post/jdgex_post_f108 == complete and jdgex_sminit_f183 == complete
      task jdgex_sminit_f189
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/dgex/post/jdgex_post_f111 == complete and jdgex_sminit_f186 == complete
      task jdgex_sminit_f192
        trigger ( /prod06/sref09/enspost/jsref_enspost:ensgrd_216_ready or /prod06/sref09/enspost/jsref_enspost == complete) and /prod06/sref09/jsref_gefs2sref == complete and /prod12/dgex/post/jdgex_post_f114 == complete and jdgex_sminit_f189 == complete
    endfamily
  endfamily
  family jma12
    task jjma_gempak_12
      time 15:45
  endfamily
  family gefs
    edit ECF_FILES '/ecf/ecfnets/scripts/gefs'
    edit PROJ 'GEN-OPS'
    edit QUEUE 'prod2'
    edit COM 'com2'
    edit QUEUESERV 'prod2_serv'
    edit ECF_JOB_CMD '/ecf/ecfutils/unixsubmit %ECF_JOB% %ECF_JOBOUT% ibmsp'
    family init
      task jgefs_enkf_track
        trigger /prod06/gdas/enkf/forecast eq complete and /prod12/gfs/dump/jgfs_tropcy_qc_reloc eq complete
      task jgefs_init_separate
        trigger /prod12/gfs/prdgen/jgfs_pgrb2_f00 eq complete and jgefs_enkf_track eq complete
      task jgefs_init_process
        trigger jgefs_init_separate eq complete
      task jgefs_init_combine
        trigger jgefs_init_separate eq complete and jgefs_init_process eq complete
    endfamily
    task jgefs_forecast_high
      trigger init eq complete and ( /prod12/gfs/forecast/jgfs_forecast_low == active or /prod12/gfs/forecast/jgfs_forecast_low == complete )
    family post_high
      family c00
        edit MEM 'c00'
        task jgefs_post_high1
          trigger ../../jgefs_forecast_high eq active
          event 1 anl_done
          event 2 f06_done
          event 3 f15_done
          event 4 f24_done
          event 5 f33_done
          event 6 f42_done
          event 7 f51_done
          event 8 f60_done
          event 9 f69_done
          event 10 f78_done
          event 11 f87_done
          event 12 f96_done
          event 13 f105_done
          event 14 f114_done
          event 15 f123_done
          event 16 f132_done
          event 17 f141_done
          event 18 f150_done
          event 19 f159_done
          event 20 f168_done
          event 21 f177_done
          event 22 f186_done
        task jgefs_post_high2
          trigger ../../jgefs_forecast_high eq active
          event 1 f00_done
          event 2 f09_done
          event 3 f18_done
          event 4 f27_done
          event 5 f36_done
          event 6 f45_done
          event 7 f54_done
          event 8 f63_done
          event 9 f72_done
          event 10 f81_done
          event 11 f90_done
          event 12 f99_done
          event 13 f108_done
          event 14 f117_done
          event 15 f126_done
          event 16 f135_done
          event 17 f144_done
          event 18 f153_done
          event 19 f162_done
          event 20 f171_done
          event 21 f180_done
          event 22 f189_done
        task jgefs_post_high3
          trigger ../../jgefs_forecast_high eq active
          event 1 f03_done
          event 2 f12_done
          event 3 f21_done
          event 4 f30_done
          event 5 f39_done
          event 6 f48_done
          event 7 f57_done
          event 8 f66_done
          event 9 f75_done
          event 10 f84_done
          event 11 f93_done
          event 12 f102_done
          event 13 f111_done
          event 14 f120_done
          event 15 f129_done
          event 16 f138_done
          event 17 f147_done
          event 18 f156_done
          event 19 f165_done
          event 20 f174_done
          event 21 f183_done
          event 22 f192_done
      endfamily
      family p01
        edit MEM 'p01'
        task jgefs_post_high1
          trigger ../../jgefs_forecast_high eq active
          event 1 anl_done
          event 2 f06_done
          event 3 f15_done
          event 4 f24_done
          event 5 f33_done
          event 6 f42_done
          event 7 f51_done
          event 8 f60_done
          event 9 f69_done
          event 10 f78_done
          event 11 f87_done
          event 12 f96_done
          event 13 f105_done
          event 14 f114_done
          event 15 f123_done
          event 16 f132_done
          event 17 f141_done
          event 18 f150_done
          event 19 f159_done
          event 20 f168_done
          event 21 f177_done
          event 22 f186_done
        task jgefs_post_high2
          trigger ../../jgefs_forecast_high eq active
          event 1 f00_done
          event 2 f09_done
          event 3 f18_done
          event 4 f27_done
          event 5 f36_done
          event 6 f45_done
          event 7 f54_done
          event 8 f63_done
          event 9 f72_done
          event 10 f81_done
          event 11 f90_done
          event 12 f99_done
          event 13 f108_done
          event 14 f117_done
          event 15 f126_done
          event 16 f135_done
          event 17 f144_done
          event 18 f153_done
          event 19 f162_done
          event 20 f171_done
          event 21 f180_done
          event 22 f189_done
        task jgefs_post_high3
          trigger ../../jgefs_forecast_high eq active
          event 1 f03_done
          event 2 f12_done
          event 3 f21_done
          event 4 f30_done
          event 5 f39_done
          event 6 f48_done
          event 7 f57_done
          event 8 f66_done
          event 9 f75_done
          event 10 f84_done
          event 11 f93_done
          event 12 f102_done
          event 13 f111_done
          event 14 f120_done
          event 15 f129_done
          event 16 f138_done
          event 17 f147_done
          event 18 f156_done
          event 19 f165_done
          event 20 f174_done
          event 21 f183_done
          event 22 f192_done
      endfamily
      family p02
        edit MEM 'p02'
        task jgefs_post_high1
          trigger ../../jgefs_forecast_high eq active
          event 1 anl_done
          event 2 f06_done
          event 3 f15_done
          event 4 f24_done
          event 5 f33_done
          event 6 f42_done
          event 7 f51_done
          event 8 f60_done
          event 9 f69_done
          event 10 f78_done
          event 11 f87_done
          event 12 f96_done
          event 13 f105_done
          event 14 f114_done
          event 15 f123_done
          event 16 f132_done
          event 17 f141_done
          event 18 f150_done
          event 19 f159_done
          event 20 f168_done
          event 21 f177_done
          event 22 f186_done
        task jgefs_post_high2
          trigger ../../jgefs_forecast_high eq active
          event 1 f00_done
          event 2 f09_done
          event 3 f18_done
          event 4 f27_done
          event 5 f36_done
          event 6 f45_done
          event 7 f54_done
          event 8 f63_done
          event 9 f72_done
          event 10 f81_done
          event 11 f90_done
          event 12 f99_done
          event 13 f108_done
          event 14 f117_done
          event 15 f126_done
          event 16 f135_done
          event 17 f144_done
          event 18 f153_done
          event 19 f162_done
          event 20 f171_done
          event 21 f180_done
          event 22 f189_done
        task jgefs_post_high3
          trigger ../../jgefs_forecast_high eq active
          event 1 f03_done
          event 2 f12_done
          event 3 f21_done
          event 4 f30_done
          event 5 f39_done
          event 6 f48_done
          event 7 f57_done
          event 8 f66_done
          event 9 f75_done
          event 10 f84_done
          event 11 f93_done
          event 12 f102_done
          event 13 f111_done
          event 14 f120_done
          event 15 f129_done
          event 16 f138_done
          event 17 f147_done
          event 18 f156_done
          event 19 f165_done
          event 20 f174_done
          event 21 f183_done
          event 22 f192_done
      endfamily
      family p03
        edit MEM 'p03'
        task jgefs_post_high1
          trigger ../../jgefs_forecast_high eq active
          event 1 anl_done
          event 2 f06_done
          event 3 f15_done
          event 4 f24_done
          event 5 f33_done
          event 6 f42_done
          event 7 f51_done
          event 8 f60_done
          event 9 f69_done
          event 10 f78_done
          event 11 f87_done
          event 12 f96_done
          event 13 f105_done
          event 14 f114_done
          event 15 f123_done
          event 16 f132_done
          event 17 f141_done
          event 18 f150_done
          event 19 f159_done
          event 20 f168_done
          event 21 f177_done
          event 22 f186_done
        task jgefs_post_high2
          trigger ../../jgefs_forecast_high eq active
          event 1 f00_done
          event 2 f09_done
          event 3 f18_done
          event 4 f27_done
          event 5 f36_done
          event 6 f45_done
          event 7 f54_done
          event 8 f63_done
          event 9 f72_done
          event 10 f81_done
          event 11 f90_done
          event 12 f99_done
          event 13 f108_done
          event 14 f117_done
          event 15 f126_done
          event 16 f135_done
          event 17 f144_done
          event 18 f153_done
          event 19 f162_done
          event 20 f171_done
          event 21 f180_done
          event 22 f189_done
        task jgefs_post_high3
          trigger ../../jgefs_forecast_high eq active
          event 1 f03_done
          event 2 f12_done
          event 3 f21_done
          event 4 f30_done
          event 5 f39_done
          event 6 f48_done
          event 7 f57_done
          event 8 f66_done
          event 9 f75_done
          event 10 f84_done
          event 11 f93_done
          event 12 f102_done
          event 13 f111_done
          event 14 f120_done
          event 15 f129_done
          event 16 f138_done
          event 17 f147_done
          event 18 f156_done
          event 19 f165_done
          event 20 f174_done
          event 21 f183_done
          event 22 f192_done
      endfamily
      family p04
        edit MEM 'p04'
        task jgefs_post_high1
          trigger ../../jgefs_forecast_high eq active
          event 1 anl_done
          event 2 f06_done
          event 3 f15_done
          event 4 f24_done
          event 5 f33_done
          event 6 f42_done
          event 7 f51_done
          event 8 f60_done
          event 9 f69_done
          event 10 f78_done
          event 11 f87_done
          event 12 f96_done
          event 13 f105_done
          event 14 f114_done
          event 15 f123_done
          event 16 f132_done
          event 17 f141_done
          event 18 f150_done
          event 19 f159_done
          event 20 f168_done
          event 21 f177_done
          event 22 f186_done
        task jgefs_post_high2
          trigger ../../jgefs_forecast_high eq active
          event 1 f00_done
          event 2 f09_done
          event 3 f18_done
          event 4 f27_done
          event 5 f36_done
          event 6 f45_done
          event 7 f54_done
          event 8 f63_done
          event 9 f72_done
          event 10 f81_done
          event 11 f90_done
          event 12 f99_done
          event 13 f108_done
          event 14 f117_done
          event 15 f126_done
          event 16 f135_done
          event 17 f144_done
          event 18 f153_done
          event 19 f162_done
          event 20 f171_done
          event 21 f180_done
          event 22 f189_done
        task jgefs_post_high3
          trigger ../../jgefs_forecast_high eq active
          event 1 f03_done
          event 2 f12_done
          event 3 f21_done
          event 4 f30_done
          event 5 f39_done
          event 6 f48_done
          event 7 f57_done
          event 8 f66_done
          event 9 f75_done
          event 10 f84_done
          event 11 f93_done
          event 12 f102_done
          event 13 f111_done
          event 14 f120_done
          event 15 f129_done
          event 16 f138_done
          event 17 f147_done
          event 18 f156_done
          event 19 f165_done
          event 20 f174_done
          event 21 f183_done
          event 22 f192_done
      endfamily
      family p05
        edit MEM 'p05'
        task jgefs_post_high1
          trigger ../../jgefs_forecast_high eq active
          event 1 anl_done
          event 2 f06_done
          event 3 f15_done
          event 4 f24_done
          event 5 f33_done
          event 6 f42_done
          event 7 f51_done
          event 8 f60_done
          event 9 f69_done
          event 10 f78_done
          event 11 f87_done
          event 12 f96_done
          event 13 f105_done
          event 14 f114_done
          event 15 f123_done
          event 16 f132_done
          event 17 f141_done
          event 18 f150_done
          event 19 f159_done
          event 20 f168_done
          event 21 f177_done
          event 22 f186_done
        task jgefs_post_high2
          trigger ../../jgefs_forecast_high eq active
          event 1 f00_done
          event 2 f09_done
          event 3 f18_done
          event 4 f27_done
          event 5 f36_done
          event 6 f45_done
          event 7 f54_done
          event 8 f63_done
          event 9 f72_done
          event 10 f81_done
          event 11 f90_done
          event 12 f99_done
          event 13 f108_done
          event 14 f117_done
          event 15 f126_done
          event 16 f135_done
          event 17 f144_done
          event 18 f153_done
          event 19 f162_done
          event 20 f171_done
          event 21 f180_done
          event 22 f189_done
        task jgefs_post_high3
          trigger ../../jgefs_forecast_high eq active
          event 1 f03_done
          event 2 f12_done
          event 3 f21_done
          event 4 f30_done
          event 5 f39_done
          event 6 f48_done
          event 7 f57_done
          event 8 f66_done
          event 9 f75_done
          event 10 f84_done
          event 11 f93_done
          event 12 f102_done
          event 13 f111_done
          event 14 f120_done
          event 15 f129_done
          event 16 f138_done
          event 17 f147_done
          event 18 f156_done
          event 19 f165_done
          event 20 f174_done
          event 21 f183_done
          event 22 f192_done
      endfamily
      family p06
        edit MEM 'p06'
        task jgefs_post_high1
          trigger ../../jgefs_forecast_high eq active
          event 1 anl_done
          event 2 f06_done
          event 3 f15_done
          event 4 f24_done
          event 5 f33_done
          event 6 f42_done
          event 7 f51_done
          event 8 f60_done
          event 9 f69_done
          event 10 f78_done
          event 11 f87_done
          event 12 f96_done
          event 13 f105_done
          event 14 f114_done
          event 15 f123_done
          event 16 f132_done
          event 17 f141_done
          event 18 f150_done
          event 19 f159_done
          event 20 f168_done
          event 21 f177_done
          event 22 f186_done
        task jgefs_post_high2
          trigger ../../jgefs_forecast_high eq active
          event 1 f00_done
          event 2 f09_done
          event 3 f18_done
          event 4 f27_done
          event 5 f36_done
          event 6 f45_done
          event 7 f54_done
          event 8 f63_done
          event 9 f72_done
          event 10 f81_done
          event 11 f90_done
          event 12 f99_done
          event 13 f108_done
          event 14 f117_done
          event 15 f126_done
          event 16 f135_done
          event 17 f144_done
          event 18 f153_done
          event 19 f162_done
          event 20 f171_done
          event 21 f180_done
          event 22 f189_done
        task jgefs_post_high3
          trigger ../../jgefs_forecast_high eq active
          event 1 f03_done
          event 2 f12_done
          event 3 f21_done
          event 4 f30_done
          event 5 f39_done
          event 6 f48_done
          event 7 f57_done
          event 8 f66_done
          event 9 f75_done
          event 10 f84_done
          event 11 f93_done
          event 12 f102_done
          event 13 f111_done
          event 14 f120_done
          event 15 f129_done
          event 16 f138_done
          event 17 f147_done
          event 18 f156_done
          event 19 f165_done
          event 20 f174_done
          event 21 f183_done
          event 22 f192_done
      endfamily
      family p07
        edit MEM 'p07'
        task jgefs_post_high1
          trigger ../../jgefs_forecast_high eq active
          event 1 anl_done
          event 2 f06_done
          event 3 f15_done
          event 4 f24_done
          event 5 f33_done
          event 6 f42_done
          event 7 f51_done
          event 8 f60_done
          event 9 f69_done
          event 10 f78_done
          event 11 f87_done
          event 12 f96_done
          event 13 f105_done
          event 14 f114_done
          event 15 f123_done
          event 16 f132_done
          event 17 f141_done
          event 18 f150_done
          event 19 f159_done
          event 20 f168_done
          event 21 f177_done
          event 22 f186_done
        task jgefs_post_high2
          trigger ../../jgefs_forecast_high eq active
          event 1 f00_done
          event 2 f09_done
          event 3 f18_done
          event 4 f27_done
          event 5 f36_done
          event 6 f45_done
          event 7 f54_done
          event 8 f63_done
          event 9 f72_done
          event 10 f81_done
          event 11 f90_done
          event 12 f99_done
          event 13 f108_done
          event 14 f117_done
          event 15 f126_done
          event 16 f135_done
          event 17 f144_done
          event 18 f153_done
          event 19 f162_done
          event 20 f171_done
          event 21 f180_done
          event 22 f189_done
        task jgefs_post_high3
          trigger ../../jgefs_forecast_high eq active
          event 1 f03_done
          event 2 f12_done
          event 3 f21_done
          event 4 f30_done
          event 5 f39_done
          event 6 f48_done
          event 7 f57_done
          event 8 f66_done
          event 9 f75_done
          event 10 f84_done
          event 11 f93_done
          event 12 f102_done
          event 13 f111_done
          event 14 f120_done
          event 15 f129_done
          event 16 f138_done
          event 17 f147_done
          event 18 f156_done
          event 19 f165_done
          event 20 f174_done
          event 21 f183_done
          event 22 f192_done
      endfamily
      family p08
        edit MEM 'p08'
        task jgefs_post_high1
          trigger ../../jgefs_forecast_high eq active
          event 1 anl_done
          event 2 f06_done
          event 3 f15_done
          event 4 f24_done
          event 5 f33_done
          event 6 f42_done
          event 7 f51_done
          event 8 f60_done
          event 9 f69_done
          event 10 f78_done
          event 11 f87_done
          event 12 f96_done
          event 13 f105_done
          event 14 f114_done
          event 15 f123_done
          event 16 f132_done
          event 17 f141_done
          event 18 f150_done
          event 19 f159_done
          event 20 f168_done
          event 21 f177_done
          event 22 f186_done
        task jgefs_post_high2
          trigger ../../jgefs_forecast_high eq active
          event 1 f00_done
          event 2 f09_done
          event 3 f18_done
          event 4 f27_done
          event 5 f36_done
          event 6 f45_done
          event 7 f54_done
          event 8 f63_done
          event 9 f72_done
          event 10 f81_done
          event 11 f90_done
          event 12 f99_done
          event 13 f108_done
          event 14 f117_done
          event 15 f126_done
          event 16 f135_done
          event 17 f144_done
          event 18 f153_done
          event 19 f162_done
          event 20 f171_done
          event 21 f180_done
          event 22 f189_done
        task jgefs_post_high3
          trigger ../../jgefs_forecast_high eq active
          event 1 f03_done
          event 2 f12_done
          event 3 f21_done
          event 4 f30_done
          event 5 f39_done
          event 6 f48_done
          event 7 f57_done
          event 8 f66_done
          event 9 f75_done
          event 10 f84_done
          event 11 f93_done
          event 12 f102_done
          event 13 f111_done
          event 14 f120_done
          event 15 f129_done
          event 16 f138_done
          event 17 f147_done
          event 18 f156_done
          event 19 f165_done
          event 20 f174_done
          event 21 f183_done
          event 22 f192_done
      endfamily
      family p09
        edit MEM 'p09'
        task jgefs_post_high1
          trigger ../../jgefs_forecast_high eq active
          event 1 anl_done
          event 2 f06_done
          event 3 f15_done
          event 4 f24_done
          event 5 f33_done
          event 6 f42_done
          event 7 f51_done
          event 8 f60_done
          event 9 f69_done
          event 10 f78_done
          event 11 f87_done
          event 12 f96_done
          event 13 f105_done
          event 14 f114_done
          event 15 f123_done
          event 16 f132_done
          event 17 f141_done
          event 18 f150_done
          event 19 f159_done
          event 20 f168_done
          event 21 f177_done
          event 22 f186_done
        task jgefs_post_high2
          trigger ../../jgefs_forecast_high eq active
          event 1 f00_done
          event 2 f09_done
          event 3 f18_done
          event 4 f27_done
          event 5 f36_done
          event 6 f45_done
          event 7 f54_done
          event 8 f63_done
          event 9 f72_done
          event 10 f81_done
          event 11 f90_done
          event 12 f99_done
          event 13 f108_done
          event 14 f117_done
          event 15 f126_done
          event 16 f135_done
          event 17 f144_done
          event 18 f153_done
          event 19 f162_done
          event 20 f171_done
          event 21 f180_done
          event 22 f189_done
        task jgefs_post_high3
          trigger ../../jgefs_forecast_high eq active
          event 1 f03_done
          event 2 f12_done
          event 3 f21_done
          event 4 f30_done
          event 5 f39_done
          event 6 f48_done
          event 7 f57_done
          event 8 f66_done
          event 9 f75_done
          event 10 f84_done
          event 11 f93_done
          event 12 f102_done
          event 13 f111_done
          event 14 f120_done
          event 15 f129_done
          event 16 f138_done
          event 17 f147_done
          event 18 f156_done
          event 19 f165_done
          event 20 f174_done
          event 21 f183_done
          event 22 f192_done
      endfamily
      family p10
        edit MEM 'p10'
        task jgefs_post_high1
          trigger ../../jgefs_forecast_high eq active
          event 1 anl_done
          event 2 f06_done
          event 3 f15_done
          event 4 f24_done
          event 5 f33_done
          event 6 f42_done
          event 7 f51_done
          event 8 f60_done
          event 9 f69_done
          event 10 f78_done
          event 11 f87_done
          event 12 f96_done
          event 13 f105_done
          event 14 f114_done
          event 15 f123_done
          event 16 f132_done
          event 17 f141_done
          event 18 f150_done
          event 19 f159_done
          event 20 f168_done
          event 21 f177_done
          event 22 f186_done
        task jgefs_post_high2
          trigger ../../jgefs_forecast_high eq active
          event 1 f00_done
          event 2 f09_done
          event 3 f18_done
          event 4 f27_done
          event 5 f36_done
          event 6 f45_done
          event 7 f54_done
          event 8 f63_done
          event 9 f72_done
          event 10 f81_done
          event 11 f90_done
          event 12 f99_done
          event 13 f108_done
          event 14 f117_done
          event 15 f126_done
          event 16 f135_done
          event 17 f144_done
          event 18 f153_done
          event 19 f162_done
          event 20 f171_done
          event 21 f180_done
          event 22 f189_done
        task jgefs_post_high3
          trigger ../../jgefs_forecast_high eq active
          event 1 f03_done
          event 2 f12_done
          event 3 f21_done
          event 4 f30_done
          event 5 f39_done
          event 6 f48_done
          event 7 f57_done
          event 8 f66_done
          event 9 f75_done
          event 10 f84_done
          event 11 f93_done
          event 12 f102_done
          event 13 f111_done
          event 14 f120_done
          event 15 f129_done
          event 16 f138_done
          event 17 f147_done
          event 18 f156_done
          event 19 f165_done
          event 20 f174_done
          event 21 f183_done
          event 22 f192_done
      endfamily
      family p11
        edit MEM 'p11'
        task jgefs_post_high1
          trigger ../../jgefs_forecast_high eq active
          event 1 anl_done
          event 2 f06_done
          event 3 f15_done
          event 4 f24_done
          event 5 f33_done
          event 6 f42_done
          event 7 f51_done
          event 8 f60_done
          event 9 f69_done
          event 10 f78_done
          event 11 f87_done
          event 12 f96_done
          event 13 f105_done
          event 14 f114_done
          event 15 f123_done
          event 16 f132_done
          event 17 f141_done
          event 18 f150_done
          event 19 f159_done
          event 20 f168_done
          event 21 f177_done
          event 22 f186_done
        task jgefs_post_high2
          trigger ../../jgefs_forecast_high eq active
          event 1 f00_done
          event 2 f09_done
          event 3 f18_done
          event 4 f27_done
          event 5 f36_done
          event 6 f45_done
          event 7 f54_done
          event 8 f63_done
          event 9 f72_done
          event 10 f81_done
          event 11 f90_done
          event 12 f99_done
          event 13 f108_done
          event 14 f117_done
          event 15 f126_done
          event 16 f135_done
          event 17 f144_done
          event 18 f153_done
          event 19 f162_done
          event 20 f171_done
          event 21 f180_done
          event 22 f189_done
        task jgefs_post_high3
          trigger ../../jgefs_forecast_high eq active
          event 1 f03_done
          event 2 f12_done
          event 3 f21_done
          event 4 f30_done
          event 5 f39_done
          event 6 f48_done
          event 7 f57_done
          event 8 f66_done
          event 9 f75_done
          event 10 f84_done
          event 11 f93_done
          event 12 f102_done
          event 13 f111_done
          event 14 f120_done
          event 15 f129_done
          event 16 f138_done
          event 17 f147_done
          event 18 f156_done
          event 19 f165_done
          event 20 f174_done
          event 21 f183_done
          event 22 f192_done
      endfamily
      family p12
        edit MEM 'p12'
        task jgefs_post_high1
          trigger ../../jgefs_forecast_high eq active
          event 1 anl_done
          event 2 f06_done
          event 3 f15_done
          event 4 f24_done
          event 5 f33_done
          event 6 f42_done
          event 7 f51_done
          event 8 f60_done
          event 9 f69_done
          event 10 f78_done
          event 11 f87_done
          event 12 f96_done
          event 13 f105_done
          event 14 f114_done
          event 15 f123_done
          event 16 f132_done
          event 17 f141_done
          event 18 f150_done
          event 19 f159_done
          event 20 f168_done
          event 21 f177_done
          event 22 f186_done
        task jgefs_post_high2
          trigger ../../jgefs_forecast_high eq active
          event 1 f00_done
          event 2 f09_done
          event 3 f18_done
          event 4 f27_done
          event 5 f36_done
          event 6 f45_done
          event 7 f54_done
          event 8 f63_done
          event 9 f72_done
          event 10 f81_done
          event 11 f90_done
          event 12 f99_done
          event 13 f108_done
          event 14 f117_done
          event 15 f126_done
          event 16 f135_done
          event 17 f144_done
          event 18 f153_done
          event 19 f162_done
          event 20 f171_done
          event 21 f180_done
          event 22 f189_done
        task jgefs_post_high3
          trigger ../../jgefs_forecast_high eq active
          event 1 f03_done
          event 2 f12_done
          event 3 f21_done
          event 4 f30_done
          event 5 f39_done
          event 6 f48_done
          event 7 f57_done
          event 8 f66_done
          event 9 f75_done
          event 10 f84_done
          event 11 f93_done
          event 12 f102_done
          event 13 f111_done
          event 14 f120_done
          event 15 f129_done
          event 16 f138_done
          event 17 f147_done
          event 18 f156_done
          event 19 f165_done
          event 20 f174_done
          event 21 f183_done
          event 22 f192_done
      endfamily
      family p13
        edit MEM 'p13'
        task jgefs_post_high1
          trigger ../../jgefs_forecast_high eq active
          event 1 anl_done
          event 2 f06_done
          event 3 f15_done
          event 4 f24_done
          event 5 f33_done
          event 6 f42_done
          event 7 f51_done
          event 8 f60_done
          event 9 f69_done
          event 10 f78_done
          event 11 f87_done
          event 12 f96_done
          event 13 f105_done
          event 14 f114_done
          event 15 f123_done
          event 16 f132_done
          event 17 f141_done
          event 18 f150_done
          event 19 f159_done
          event 20 f168_done
          event 21 f177_done
          event 22 f186_done
        task jgefs_post_high2
          trigger ../../jgefs_forecast_high eq active
          event 1 f00_done
          event 2 f09_done
          event 3 f18_done
          event 4 f27_done
          event 5 f36_done
          event 6 f45_done
          event 7 f54_done
          event 8 f63_done
          event 9 f72_done
          event 10 f81_done
          event 11 f90_done
          event 12 f99_done
          event 13 f108_done
          event 14 f117_done
          event 15 f126_done
          event 16 f135_done
          event 17 f144_done
          event 18 f153_done
          event 19 f162_done
          event 20 f171_done
          event 21 f180_done
          event 22 f189_done
        task jgefs_post_high3
          trigger ../../jgefs_forecast_high eq active
          event 1 f03_done
          event 2 f12_done
          event 3 f21_done
          event 4 f30_done
          event 5 f39_done
          event 6 f48_done
          event 7 f57_done
          event 8 f66_done
          event 9 f75_done
          event 10 f84_done
          event 11 f93_done
          event 12 f102_done
          event 13 f111_done
          event 14 f120_done
          event 15 f129_done
          event 16 f138_done
          event 17 f147_done
          event 18 f156_done
          event 19 f165_done
          event 20 f174_done
          event 21 f183_done
          event 22 f192_done
      endfamily
      family p14
        edit MEM 'p14'
        task jgefs_post_high1
          trigger ../../jgefs_forecast_high eq active
          event 1 anl_done
          event 2 f06_done
          event 3 f15_done
          event 4 f24_done
          event 5 f33_done
          event 6 f42_done
          event 7 f51_done
          event 8 f60_done
          event 9 f69_done
          event 10 f78_done
          event 11 f87_done
          event 12 f96_done
          event 13 f105_done
          event 14 f114_done
          event 15 f123_done
          event 16 f132_done
          event 17 f141_done
          event 18 f150_done
          event 19 f159_done
          event 20 f168_done
          event 21 f177_done
          event 22 f186_done
        task jgefs_post_high2
          trigger ../../jgefs_forecast_high eq active
          event 1 f00_done
          event 2 f09_done
          event 3 f18_done
          event 4 f27_done
          event 5 f36_done
          event 6 f45_done
          event 7 f54_done
          event 8 f63_done
          event 9 f72_done
          event 10 f81_done
          event 11 f90_done
          event 12 f99_done
          event 13 f108_done
          event 14 f117_done
          event 15 f126_done
          event 16 f135_done
          event 17 f144_done
          event 18 f153_done
          event 19 f162_done
          event 20 f171_done
          event 21 f180_done
          event 22 f189_done
        task jgefs_post_high3
          trigger ../../jgefs_forecast_high eq active
          event 1 f03_done
          event 2 f12_done
          event 3 f21_done
          event 4 f30_done
          event 5 f39_done
          event 6 f48_done
          event 7 f57_done
          event 8 f66_done
          event 9 f75_done
          event 10 f84_done
          event 11 f93_done
          event 12 f102_done
          event 13 f111_done
          event 14 f120_done
          event 15 f129_done
          event 16 f138_done
          event 17 f147_done
          event 18 f156_done
          event 19 f165_done
          event 20 f174_done
          event 21 f183_done
          event 22 f192_done
      endfamily
      family p15
        edit MEM 'p15'
        task jgefs_post_high1
          trigger ../../jgefs_forecast_high eq active
          event 1 anl_done
          event 2 f06_done
          event 3 f15_done
          event 4 f24_done
          event 5 f33_done
          event 6 f42_done
          event 7 f51_done
          event 8 f60_done
          event 9 f69_done
          event 10 f78_done
          event 11 f87_done
          event 12 f96_done
          event 13 f105_done
          event 14 f114_done
          event 15 f123_done
          event 16 f132_done
          event 17 f141_done
          event 18 f150_done
          event 19 f159_done
          event 20 f168_done
          event 21 f177_done
          event 22 f186_done
        task jgefs_post_high2
          trigger ../../jgefs_forecast_high eq active
          event 1 f00_done
          event 2 f09_done
          event 3 f18_done
          event 4 f27_done
          event 5 f36_done
          event 6 f45_done
          event 7 f54_done
          event 8 f63_done
          event 9 f72_done
          event 10 f81_done
          event 11 f90_done
          event 12 f99_done
          event 13 f108_done
          event 14 f117_done
          event 15 f126_done
          event 16 f135_done
          event 17 f144_done
          event 18 f153_done
          event 19 f162_done
          event 20 f171_done
          event 21 f180_done
          event 22 f189_done
        task jgefs_post_high3
          trigger ../../jgefs_forecast_high eq active
          event 1 f03_done
          event 2 f12_done
          event 3 f21_done
          event 4 f30_done
          event 5 f39_done
          event 6 f48_done
          event 7 f57_done
          event 8 f66_done
          event 9 f75_done
          event 10 f84_done
          event 11 f93_done
          event 12 f102_done
          event 13 f111_done
          event 14 f120_done
          event 15 f129_done
          event 16 f138_done
          event 17 f147_done
          event 18 f156_done
          event 19 f165_done
          event 20 f174_done
          event 21 f183_done
          event 22 f192_done
      endfamily
      family p16
        edit MEM 'p16'
        task jgefs_post_high1
          trigger ../../jgefs_forecast_high eq active
          event 1 anl_done
          event 2 f06_done
          event 3 f15_done
          event 4 f24_done
          event 5 f33_done
          event 6 f42_done
          event 7 f51_done
          event 8 f60_done
          event 9 f69_done
          event 10 f78_done
          event 11 f87_done
          event 12 f96_done
          event 13 f105_done
          event 14 f114_done
          event 15 f123_done
          event 16 f132_done
          event 17 f141_done
          event 18 f150_done
          event 19 f159_done
          event 20 f168_done
          event 21 f177_done
          event 22 f186_done
        task jgefs_post_high2
          trigger ../../jgefs_forecast_high eq active
          event 1 f00_done
          event 2 f09_done
          event 3 f18_done
          event 4 f27_done
          event 5 f36_done
          event 6 f45_done
          event 7 f54_done
          event 8 f63_done
          event 9 f72_done
          event 10 f81_done
          event 11 f90_done
          event 12 f99_done
          event 13 f108_done
          event 14 f117_done
          event 15 f126_done
          event 16 f135_done
          event 17 f144_done
          event 18 f153_done
          event 19 f162_done
          event 20 f171_done
          event 21 f180_done
          event 22 f189_done
        task jgefs_post_high3
          trigger ../../jgefs_forecast_high eq active
          event 1 f03_done
          event 2 f12_done
          event 3 f21_done
          event 4 f30_done
          event 5 f39_done
          event 6 f48_done
          event 7 f57_done
          event 8 f66_done
          event 9 f75_done
          event 10 f84_done
          event 11 f93_done
          event 12 f102_done
          event 13 f111_done
          event 14 f120_done
          event 15 f129_done
          event 16 f138_done
          event 17 f147_done
          event 18 f156_done
          event 19 f165_done
          event 20 f174_done
          event 21 f183_done
          event 22 f192_done
      endfamily
      family p17
        edit MEM 'p17'
        task jgefs_post_high1
          trigger ../../jgefs_forecast_high eq active
          event 1 anl_done
          event 2 f06_done
          event 3 f15_done
          event 4 f24_done
          event 5 f33_done
          event 6 f42_done
          event 7 f51_done
          event 8 f60_done
          event 9 f69_done
          event 10 f78_done
          event 11 f87_done
          event 12 f96_done
          event 13 f105_done
          event 14 f114_done
          event 15 f123_done
          event 16 f132_done
          event 17 f141_done
          event 18 f150_done
          event 19 f159_done
          event 20 f168_done
          event 21 f177_done
          event 22 f186_done
        task jgefs_post_high2
          trigger ../../jgefs_forecast_high eq active
          event 1 f00_done
          event 2 f09_done
          event 3 f18_done
          event 4 f27_done
          event 5 f36_done
          event 6 f45_done
          event 7 f54_done
          event 8 f63_done
          event 9 f72_done
          event 10 f81_done
          event 11 f90_done
          event 12 f99_done
          event 13 f108_done
          event 14 f117_done
          event 15 f126_done
          event 16 f135_done
          event 17 f144_done
          event 18 f153_done
          event 19 f162_done
          event 20 f171_done
          event 21 f180_done
          event 22 f189_done
        task jgefs_post_high3
          trigger ../../jgefs_forecast_high eq active
          event 1 f03_done
          event 2 f12_done
          event 3 f21_done
          event 4 f30_done
          event 5 f39_done
          event 6 f48_done
          event 7 f57_done
          event 8 f66_done
          event 9 f75_done
          event 10 f84_done
          event 11 f93_done
          event 12 f102_done
          event 13 f111_done
          event 14 f120_done
          event 15 f129_done
          event 16 f138_done
          event 17 f147_done
          event 18 f156_done
          event 19 f165_done
          event 20 f174_done
          event 21 f183_done
          event 22 f192_done
      endfamily
      family p18
        edit MEM 'p18'
        task jgefs_post_high1
          trigger ../../jgefs_forecast_high eq active
          event 1 anl_done
          event 2 f06_done
          event 3 f15_done
          event 4 f24_done
          event 5 f33_done
          event 6 f42_done
          event 7 f51_done
          event 8 f60_done
          event 9 f69_done
          event 10 f78_done
          event 11 f87_done
          event 12 f96_done
          event 13 f105_done
          event 14 f114_done
          event 15 f123_done
          event 16 f132_done
          event 17 f141_done
          event 18 f150_done
          event 19 f159_done
          event 20 f168_done
          event 21 f177_done
          event 22 f186_done
        task jgefs_post_high2
          trigger ../../jgefs_forecast_high eq active
          event 1 f00_done
          event 2 f09_done
          event 3 f18_done
          event 4 f27_done
          event 5 f36_done
          event 6 f45_done
          event 7 f54_done
          event 8 f63_done
          event 9 f72_done
          event 10 f81_done
          event 11 f90_done
          event 12 f99_done
          event 13 f108_done
          event 14 f117_done
          event 15 f126_done
          event 16 f135_done
          event 17 f144_done
          event 18 f153_done
          event 19 f162_done
          event 20 f171_done
          event 21 f180_done
          event 22 f189_done
        task jgefs_post_high3
          trigger ../../jgefs_forecast_high eq active
          event 1 f03_done
          event 2 f12_done
          event 3 f21_done
          event 4 f30_done
          event 5 f39_done
          event 6 f48_done
          event 7 f57_done
          event 8 f66_done
          event 9 f75_done
          event 10 f84_done
          event 11 f93_done
          event 12 f102_done
          event 13 f111_done
          event 14 f120_done
          event 15 f129_done
          event 16 f138_done
          event 17 f147_done
          event 18 f156_done
          event 19 f165_done
          event 20 f174_done
          event 21 f183_done
          event 22 f192_done
      endfamily
      family p19
        edit MEM 'p19'
        task jgefs_post_high1
          trigger ../../jgefs_forecast_high eq active
          event 1 anl_done
          event 2 f06_done
          event 3 f15_done
          event 4 f24_done
          event 5 f33_done
          event 6 f42_done
          event 7 f51_done
          event 8 f60_done
          event 9 f69_done
          event 10 f78_done
          event 11 f87_done
          event 12 f96_done
          event 13 f105_done
          event 14 f114_done
          event 15 f123_done
          event 16 f132_done
          event 17 f141_done
          event 18 f150_done
          event 19 f159_done
          event 20 f168_done
          event 21 f177_done
          event 22 f186_done
        task jgefs_post_high2
          trigger ../../jgefs_forecast_high eq active
          event 1 f00_done
          event 2 f09_done
          event 3 f18_done
          event 4 f27_done
          event 5 f36_done
          event 6 f45_done
          event 7 f54_done
          event 8 f63_done
          event 9 f72_done
          event 10 f81_done
          event 11 f90_done
          event 12 f99_done
          event 13 f108_done
          event 14 f117_done
          event 15 f126_done
          event 16 f135_done
          event 17 f144_done
          event 18 f153_done
          event 19 f162_done
          event 20 f171_done
          event 21 f180_done
          event 22 f189_done
        task jgefs_post_high3
          trigger ../../jgefs_forecast_high eq active
          event 1 f03_done
          event 2 f12_done
          event 3 f21_done
          event 4 f30_done
          event 5 f39_done
          event 6 f48_done
          event 7 f57_done
          event 8 f66_done
          event 9 f75_done
          event 10 f84_done
          event 11 f93_done
          event 12 f102_done
          event 13 f111_done
          event 14 f120_done
          event 15 f129_done
          event 16 f138_done
          event 17 f147_done
          event 18 f156_done
          event 19 f165_done
          event 20 f174_done
          event 21 f183_done
          event 22 f192_done
      endfamily
      family p20
        edit MEM 'p20'
        task jgefs_post_high1
          trigger ../../jgefs_forecast_high eq active
          event 1 anl_done
          event 2 f06_done
          event 3 f15_done
          event 4 f24_done
          event 5 f33_done
          event 6 f42_done
          event 7 f51_done
          event 8 f60_done
          event 9 f69_done
          event 10 f78_done
          event 11 f87_done
          event 12 f96_done
          event 13 f105_done
          event 14 f114_done
          event 15 f123_done
          event 16 f132_done
          event 17 f141_done
          event 18 f150_done
          event 19 f159_done
          event 20 f168_done
          event 21 f177_done
          event 22 f186_done
        task jgefs_post_high2
          trigger ../../jgefs_forecast_high eq active
          event 1 f00_done
          event 2 f09_done
          event 3 f18_done
          event 4 f27_done
          event 5 f36_done
          event 6 f45_done
          event 7 f54_done
          event 8 f63_done
          event 9 f72_done
          event 10 f81_done
          event 11 f90_done
          event 12 f99_done
          event 13 f108_done
          event 14 f117_done
          event 15 f126_done
          event 16 f135_done
          event 17 f144_done
          event 18 f153_done
          event 19 f162_done
          event 20 f171_done
          event 21 f180_done
          event 22 f189_done
        task jgefs_post_high3
          trigger ../../jgefs_forecast_high eq active
          event 1 f03_done
          event 2 f12_done
          event 3 f21_done
          event 4 f30_done
          event 5 f39_done
          event 6 f48_done
          event 7 f57_done
          event 8 f66_done
          event 9 f75_done
          event 10 f84_done
          event 11 f93_done
          event 12 f102_done
          event 13 f111_done
          event 14 f120_done
          event 15 f129_done
          event 16 f138_done
          event 17 f147_done
          event 18 f156_done
          event 19 f165_done
          event 20 f174_done
          event 21 f183_done
          event 22 f192_done
      endfamily
    endfamily
    family prdgen_high
      task jgefs_c00_prdgen_high
        trigger ../post_high/c00 eq active or ../post_high/c00 eq complete
      task jgefs_p01_prdgen_high
        trigger ../post_high/p01 eq active or ../post_high/p01 eq complete
      task jgefs_p02_prdgen_high
        trigger ../post_high/p02 eq active or ../post_high/p02 eq complete
      task jgefs_p03_prdgen_high
        trigger ../post_high/p03 eq active or ../post_high/p03 eq complete
      task jgefs_p04_prdgen_high
        trigger ../post_high/p04 eq active or ../post_high/p04 eq complete
      task jgefs_p05_prdgen_high
        trigger ../post_high/p05 eq active or ../post_high/p05 eq complete
      task jgefs_p06_prdgen_high
        trigger ../post_high/p06 eq active or ../post_high/p06 eq complete
      task jgefs_p07_prdgen_high
        trigger ../post_high/p07 eq active or ../post_high/p07 eq complete
      task jgefs_p08_prdgen_high
        trigger ../post_high/p08 eq active or ../post_high/p08 eq complete
      task jgefs_p09_prdgen_high
        trigger ../post_high/p09 eq active or ../post_high/p09 eq complete
      task jgefs_p10_prdgen_high
        trigger ../post_high/p10 eq active or ../post_high/p10 eq complete
      task jgefs_p11_prdgen_high
        trigger ../post_high/p11 eq active or ../post_high/p11 eq complete
      task jgefs_p12_prdgen_high
        trigger ../post_high/p12 eq active or ../post_high/p12 eq complete
      task jgefs_p13_prdgen_high
        trigger ../post_high/p13 eq active or ../post_high/p13 eq complete
      task jgefs_p14_prdgen_high
        trigger ../post_high/p14 eq active or ../post_high/p14 eq complete
      task jgefs_p15_prdgen_high
        trigger ../post_high/p15 eq active or ../post_high/p15 eq complete
      task jgefs_p16_prdgen_high
        trigger ../post_high/p16 eq active or ../post_high/p16 eq complete
      task jgefs_p17_prdgen_high
        trigger ../post_high/p17 eq active or ../post_high/p17 eq complete
      task jgefs_p18_prdgen_high
        trigger ../post_high/p18 eq active or ../post_high/p18 eq complete
      task jgefs_p19_prdgen_high
        trigger ../post_high/p19 eq active or ../post_high/p19 eq complete
      task jgefs_p20_prdgen_high
        trigger ../post_high/p20 eq active or ../post_high/p20 eq complete
    endfamily
    family sigchgres
      task jgefs_c00_sigchgres
        trigger ../jgefs_forecast_high eq complete
      task jgefs_p01_sigchgres
        trigger ../jgefs_forecast_high eq complete
      task jgefs_p02_sigchgres
        trigger ../jgefs_forecast_high eq complete
      task jgefs_p03_sigchgres
        trigger ../jgefs_forecast_high eq complete
      task jgefs_p04_sigchgres
        trigger ../jgefs_forecast_high eq complete
      task jgefs_p05_sigchgres
        trigger ../jgefs_forecast_high eq complete
      task jgefs_p06_sigchgres
        trigger ../jgefs_forecast_high eq complete
      task jgefs_p07_sigchgres
        trigger ../jgefs_forecast_high eq complete
      task jgefs_p08_sigchgres
        trigger ../jgefs_forecast_high eq complete
      task jgefs_p09_sigchgres
        trigger ../jgefs_forecast_high eq complete
      task jgefs_p10_sigchgres
        trigger ../jgefs_forecast_high eq complete
      task jgefs_p11_sigchgres
        trigger ../jgefs_forecast_high eq complete
      task jgefs_p12_sigchgres
        trigger ../jgefs_forecast_high eq complete
      task jgefs_p13_sigchgres
        trigger ../jgefs_forecast_high eq complete
      task jgefs_p14_sigchgres
        trigger ../jgefs_forecast_high eq complete
      task jgefs_p15_sigchgres
        trigger ../jgefs_forecast_high eq complete
      task jgefs_p16_sigchgres
        trigger ../jgefs_forecast_high eq complete
      task jgefs_p17_sigchgres
        trigger ../jgefs_forecast_high eq complete
      task jgefs_p18_sigchgres
        trigger ../jgefs_forecast_high eq complete
      task jgefs_p19_sigchgres
        trigger ../jgefs_forecast_high eq complete
      task jgefs_p20_sigchgres
        trigger ../jgefs_forecast_high eq complete
    endfamily
    task jgefs_forecast_low
      trigger sigchgres eq complete
    family post_low
      task jgefs_c00_post_low
        trigger ( ../jgefs_forecast_low eq active or ../jgefs_forecast_low eq complete ) and ../post_high/c00 eq complete
      task jgefs_p01_post_low
        trigger ( ../jgefs_forecast_low eq active or ../jgefs_forecast_low eq complete ) and ../post_high/p01 eq complete
      task jgefs_p02_post_low
        trigger ( ../jgefs_forecast_low eq active or ../jgefs_forecast_low eq complete ) and ../post_high/p02 eq complete
      task jgefs_p03_post_low
        trigger ( ../jgefs_forecast_low eq active or ../jgefs_forecast_low eq complete ) and ../post_high/p03 eq complete
      task jgefs_p04_post_low
        trigger ( ../jgefs_forecast_low eq active or ../jgefs_forecast_low eq complete ) and ../post_high/p04 eq complete
      task jgefs_p05_post_low
        trigger ( ../jgefs_forecast_low eq active or ../jgefs_forecast_low eq complete ) and ../post_high/p05 eq complete
      task jgefs_p06_post_low
        trigger ( ../jgefs_forecast_low eq active or ../jgefs_forecast_low eq complete ) and ../post_high/p06 eq complete
      task jgefs_p07_post_low
        trigger ( ../jgefs_forecast_low eq active or ../jgefs_forecast_low eq complete ) and ../post_high/p07 eq complete
      task jgefs_p08_post_low
        trigger ( ../jgefs_forecast_low eq active or ../jgefs_forecast_low eq complete ) and ../post_high/p08 eq complete
      task jgefs_p09_post_low
        trigger ( ../jgefs_forecast_low eq active or ../jgefs_forecast_low eq complete ) and ../post_high/p09 eq complete
      task jgefs_p10_post_low
        trigger ( ../jgefs_forecast_low eq active or ../jgefs_forecast_low eq complete ) and ../post_high/p10 eq complete
      task jgefs_p11_post_low
        trigger ( ../jgefs_forecast_low eq active or ../jgefs_forecast_low eq complete ) and ../post_high/p11 eq complete
      task jgefs_p12_post_low
        trigger ( ../jgefs_forecast_low eq active or ../jgefs_forecast_low eq complete ) and ../post_high/p12 eq complete
      task jgefs_p13_post_low
        trigger ( ../jgefs_forecast_low eq active or ../jgefs_forecast_low eq complete ) and ../post_high/p13 eq complete
      task jgefs_p14_post_low
        trigger ( ../jgefs_forecast_low eq active or ../jgefs_forecast_low eq complete ) and ../post_high/p14 eq complete
      task jgefs_p15_post_low
        trigger ( ../jgefs_forecast_low eq active or ../jgefs_forecast_low eq complete ) and ../post_high/p15 eq complete
      task jgefs_p16_post_low
        trigger ( ../jgefs_forecast_low eq active or ../jgefs_forecast_low eq complete ) and ../post_high/p16 eq complete
      task jgefs_p17_post_low
        trigger ( ../jgefs_forecast_low eq active or ../jgefs_forecast_low eq complete ) and ../post_high/p17 eq complete
      task jgefs_p18_post_low
        trigger ( ../jgefs_forecast_low eq active or ../jgefs_forecast_low eq complete ) and ../post_high/p18 eq complete
      task jgefs_p19_post_low
        trigger ( ../jgefs_forecast_low eq active or ../jgefs_forecast_low eq complete ) and ../post_high/p19 eq complete
      task jgefs_p20_post_low
        trigger ( ../jgefs_forecast_low eq active or ../jgefs_forecast_low eq complete ) and ../post_high/p20 eq complete
    endfamily
    family prdgen_low
      task jgefs_c00_prdgen_low
        trigger ( ../post_low/jgefs_c00_post_low eq active or ../post_low/jgefs_c00_post_low eq complete ) and ../prdgen_high/jgefs_c00_prdgen_high eq complete
        event 1 pgrb2ap5_f240_ready
      task jgefs_p01_prdgen_low
        trigger ( ../post_low/jgefs_p01_post_low eq active or ../post_low/jgefs_p01_post_low eq complete ) and ../prdgen_high/jgefs_p01_prdgen_high eq complete
        event 1 pgrb2ap5_f240_ready
      task jgefs_p02_prdgen_low
        trigger ( ../post_low/jgefs_p02_post_low eq active or ../post_low/jgefs_p02_post_low eq complete ) and ../prdgen_high/jgefs_p02_prdgen_high eq complete
        event 1 pgrb2ap5_f240_ready
      task jgefs_p03_prdgen_low
        trigger ( ../post_low/jgefs_p03_post_low eq active or ../post_low/jgefs_p03_post_low eq complete ) and ../prdgen_high/jgefs_p03_prdgen_high eq complete
        event 1 pgrb2ap5_f240_ready
      task jgefs_p04_prdgen_low
        trigger ( ../post_low/jgefs_p04_post_low eq active or ../post_low/jgefs_p04_post_low eq complete ) and ../prdgen_high/jgefs_p04_prdgen_high eq complete
        event 1 pgrb2ap5_f240_ready
      task jgefs_p05_prdgen_low
        trigger ( ../post_low/jgefs_p05_post_low eq active or ../post_low/jgefs_p05_post_low eq complete ) and ../prdgen_high/jgefs_p05_prdgen_high eq complete
        event 1 pgrb2ap5_f240_ready
      task jgefs_p06_prdgen_low
        trigger ( ../post_low/jgefs_p06_post_low eq active or ../post_low/jgefs_p06_post_low eq complete ) and ../prdgen_high/jgefs_p06_prdgen_high eq complete
        event 1 pgrb2ap5_f240_ready
      task jgefs_p07_prdgen_low
        trigger ( ../post_low/jgefs_p07_post_low eq active or ../post_low/jgefs_p07_post_low eq complete ) and ../prdgen_high/jgefs_p07_prdgen_high eq complete
        event 1 pgrb2ap5_f240_ready
      task jgefs_p08_prdgen_low
        trigger ( ../post_low/jgefs_p08_post_low eq active or ../post_low/jgefs_p08_post_low eq complete ) and ../prdgen_high/jgefs_p08_prdgen_high eq complete
        event 1 pgrb2ap5_f240_ready
      task jgefs_p09_prdgen_low
        trigger ( ../post_low/jgefs_p09_post_low eq active or ../post_low/jgefs_p09_post_low eq complete ) and ../prdgen_high/jgefs_p09_prdgen_high eq complete
        event 1 pgrb2ap5_f240_ready
      task jgefs_p10_prdgen_low
        trigger ( ../post_low/jgefs_p10_post_low eq active or ../post_low/jgefs_p10_post_low eq complete ) and ../prdgen_high/jgefs_p10_prdgen_high eq complete
        event 1 pgrb2ap5_f240_ready
      task jgefs_p11_prdgen_low
        trigger ( ../post_low/jgefs_p11_post_low eq active or ../post_low/jgefs_p11_post_low eq complete ) and ../prdgen_high/jgefs_p11_prdgen_high eq complete
        event 1 pgrb2ap5_f240_ready
      task jgefs_p12_prdgen_low
        trigger ( ../post_low/jgefs_p12_post_low eq active or ../post_low/jgefs_p12_post_low eq complete ) and ../prdgen_high/jgefs_p12_prdgen_high eq complete
        event 1 pgrb2ap5_f240_ready
      task jgefs_p13_prdgen_low
        trigger ( ../post_low/jgefs_p13_post_low eq active or ../post_low/jgefs_p13_post_low eq complete ) and ../prdgen_high/jgefs_p13_prdgen_high eq complete
        event 1 pgrb2ap5_f240_ready
      task jgefs_p14_prdgen_low
        trigger ( ../post_low/jgefs_p14_post_low eq active or ../post_low/jgefs_p14_post_low eq complete ) and ../prdgen_high/jgefs_p14_prdgen_high eq complete
        event 1 pgrb2ap5_f240_ready
      task jgefs_p15_prdgen_low
        trigger ( ../post_low/jgefs_p15_post_low eq active or ../post_low/jgefs_p15_post_low eq complete ) and ../prdgen_high/jgefs_p15_prdgen_high eq complete
        event 1 pgrb2ap5_f240_ready
      task jgefs_p16_prdgen_low
        trigger ( ../post_low/jgefs_p16_post_low eq active or ../post_low/jgefs_p16_post_low eq complete ) and ../prdgen_high/jgefs_p16_prdgen_high eq complete
        event 1 pgrb2ap5_f240_ready
      task jgefs_p17_prdgen_low
        trigger ( ../post_low/jgefs_p17_post_low eq active or ../post_low/jgefs_p17_post_low eq complete ) and ../prdgen_high/jgefs_p17_prdgen_high eq complete
        event 1 pgrb2ap5_f240_ready
      task jgefs_p18_prdgen_low
        trigger ( ../post_low/jgefs_p18_post_low eq active or ../post_low/jgefs_p18_post_low eq complete ) and ../prdgen_high/jgefs_p18_prdgen_high eq complete
        event 1 pgrb2ap5_f240_ready
      task jgefs_p19_prdgen_low
        trigger ( ../post_low/jgefs_p19_post_low eq active or ../post_low/jgefs_p19_post_low eq complete ) and ../prdgen_high/jgefs_p19_prdgen_high eq complete
        event 1 pgrb2ap5_f240_ready
      task jgefs_p20_prdgen_low
        trigger ( ../post_low/jgefs_p20_post_low eq active or ../post_low/jgefs_p20_post_low eq complete ) and ../prdgen_high/jgefs_p20_prdgen_high eq complete
        event 1 pgrb2ap5_f240_ready
    endfamily
    family gempak
      task jgefs_bias_gempak
        trigger ../post_processing/jgefs_bias eq complete
      task jgefs_gempak
        trigger ../prdgen_low eq active or ../prdgen_low eq complete
      task jgefs_avgspr_gempak
        trigger ../post_processing/jgefs_ensstat_low eq complete
      task jgefs_avg_gempak_vgf
        trigger jgefs_avgspr_gempak eq complete
      task jgefs_debias_gempak
        trigger ../post_processing/jgefs_debias eq complete
      task jgefs_avgspr_gempak_meta
        trigger jgefs_avgspr_gempak eq complete
      task jgefs_gempak_meta
        trigger jgefs_gempak eq complete and jgefs_avgspr_gempak eq complete
      task jgefs_prob_avgspr_gempak
        trigger ../post_processing/jgefs_prob_avgspr eq complete
    endfamily
    family post_processing
      task jgefs_bias
        trigger ../init eq active
      task jgefs_prdgen_gfs
        trigger ../init eq complete and ( /prod12/gfs/forecast/jgfs_forecast_low eq active or /prod12/gfs/forecast/jgfs_forecast_low eq complete )
      task jgefs_ensstat_high
        trigger ../post_high eq active or ../post_high eq complete
      task jgefs_ensstat_low
        trigger ( ../post_low eq active or ../post_low eq complete ) and jgefs_ensstat_high eq complete
      task jgefs_enspost
        trigger ../post_low eq active or ../post_low eq complete
        event 1 enspost_grb2_ready
      task jgefs_debias
        trigger jgefs_bias eq complete and ../prdgen_low eq active
        event 1 pgrba_bcf240_gec00
        event 2 pgrba_bcf240_gep01
        event 3 pgrba_bcf240_gep02
        event 4 pgrba_bcf240_gep03
        event 5 pgrba_bcf240_gep04
        event 6 pgrba_bcf240_gep05
        event 7 pgrba_bcf240_gep06
        event 8 pgrba_bcf240_gep07
        event 9 pgrba_bcf240_gep08
        event 10 pgrba_bcf240_gep09
        event 11 pgrba_bcf240_gep10
        event 12 pgrba_bcf240_gep11
        event 13 pgrba_bcf240_gep12
        event 14 pgrba_bcf240_gep13
        event 15 pgrba_bcf240_gep14
        event 16 pgrba_bcf240_gep15
        event 17 pgrba_bcf240_gep16
        event 18 pgrba_bcf240_gep17
        event 19 pgrba_bcf240_gep18
        event 20 pgrba_bcf240_gep19
        event 21 pgrba_bcf240_gep20
      task jgefs_pgrb_enspqpf
        trigger ../prdgen_high eq complete and ../prdgen_low eq complete
      task jgefs_wafs
        trigger jgefs_ensstat_high eq complete and jgefs_ensstat_low eq complete
      task jgefs_prob_avgspr
        trigger jgefs_debias eq complete
      task jgefs_dvrtma_prob_avgspr_ak
        trigger /prod12/naefs/jnaefs_dvrtma_bias_conus eq complete and /prod12/naefs/jnaefs_dvrtma_bias_ak eq complete and jgefs_prob_avgspr eq complete
      task jgefs_dvrtma_prob_avgspr_conus
        trigger /prod12/naefs/jnaefs_dvrtma_bias_conus eq complete and jgefs_prob_avgspr eq complete
    endfamily
    task jgefs_post_cleanup
      trigger post_low eq complete and prdgen_low eq complete and post_processing eq complete and gempak eq complete
  endfamily
  family ccpa12
    task jccpa_conus_12
      time 16:10
    task jccpa_gempak_12
      trigger /prod12/ccpa12/jccpa_conus_12 == complete
  endfamily
  family hwrf
    edit ECF_FILES '/ecf/ecfnets/scripts/hwrf/'
    edit PROJ 'HWRF-OPS'
    edit QUEUESERV 'prod2_serv'
    edit QUEUESERV2 'prod2_serv2'
    edit QUEUE 'prod2'
    edit ECF_JOB_CMD '/ecf/ecfutils/unixsubmit %ECF_JOB% %ECF_JOBOUT% ibmsp'
    family hwrf1
      edit NUM '1'
      family prep
        task jhwrf_launch
          trigger /prod12/gfs/prdgen/jgfs_pgrb2_manager:release_pgrb2_00
          edit ECF_PASS 'FREE'
          label storm1 " "
          event 1 running_GSI
          event 2 not_running_GSI
          time 15:00
        task jhwrf_init_gfs_f00
          trigger jhwrf_launch == complete
        task jhwrf_init_gdas_f03
          trigger jhwrf_launch == complete
        task jhwrf_init_gdas_f06
          trigger jhwrf_launch == complete
        task jhwrf_init_gdas_f09
          trigger jhwrf_launch == complete
        task jhwrf_ocean_init
          trigger jhwrf_launch == complete
          edit ECF_PASS 'FREE'
          event 1 run_couple
          event 2 run_noncouple
        task jhwrf_init_bdy
          trigger jhwrf_launch == complete and jhwrf_init_gfs_f00 == complete
      endfamily
      family analysis
        task jhwrf_fgat_relocate_gfs_f00
          trigger ../prep/jhwrf_init_gfs_f00 == complete
        task jhwrf_fgat_relocate_gdas_f09
          trigger ../prep/jhwrf_init_gdas_f09 == complete
        task jhwrf_fgat_relocate_gdas_f03
          trigger ../prep/jhwrf_init_gdas_f03 == complete
        task jhwrf_fgat_relocate_gdas_f06
          trigger ../prep/jhwrf_init_gdas_f06 == complete
        task jhwrf_bufrprep
          trigger jhwrf_fgat_relocate_gdas_f03 == complete and jhwrf_fgat_relocate_gdas_f06 == complete and jhwrf_fgat_relocate_gdas_f09 == complete and ../prep/jhwrf_launch == complete
        task jhwrf_nmm_gsi_d3
          trigger jhwrf_bufrprep == complete
        task jhwrf_nmm_gsi_d2
          trigger jhwrf_bufrprep == complete
        task jhwrf_merge
          trigger jhwrf_nmm_gsi_d2 == complete and jhwrf_nmm_gsi_d3 == complete and ../prep/jhwrf_ocean_init == complete and ../prep/jhwrf_init_gfs_f00 == complete
      endfamily
      family fcst
        task jhwrf_noncouple_forecast
          trigger ../prep/jhwrf_ocean_init:run_noncouple and ../analysis/jhwrf_merge == complete and ../prep/jhwrf_init_bdy == complete and ( /prod12/gfs/post/jgfs_post_f126 == active or /prod12/gfs/post/jgfs_post_f126 == complete )
        task jhwrf_couple_forecast
          trigger ../prep/jhwrf_ocean_init:run_couple and ../analysis/jhwrf_merge == complete and ../prep/jhwrf_init_bdy == complete and ( /prod12/gfs/post/jgfs_post_f126 == active or /prod12/gfs/post/jgfs_post_f126 == complete )
      endfamily
      family post
        task jhwrf_gsi_post
          trigger ../analysis/jhwrf_nmm_gsi_d2 == complete and ../analysis/jhwrf_nmm_gsi_d3 == complete
        task jhwrf_unpost
          trigger ../fcst == active or ../fcst == complete
        task jhwrf_post
          trigger jhwrf_unpost == complete
        task jhwrf_products
          trigger (jhwrf_unpost == active or jhwrf_unpost == complete) and (jhwrf_post == active or jhwrf_post == complete)
      endfamily
      task jhwrf_output
        trigger post == complete
      family ensda
        task jhwrf_ensda_pre
          trigger (../jhwrf_output == complete and /prod12/gdas/enkf/jgdas_enkf_post == complete)
          edit ECF_PASS 'FREE'
          event 1 run_ensda
          event 2 no_ensda
        family members
          task jhwrf_ensda_m1
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m2
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m3
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m4
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m5
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m6
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m7
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m8
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m9
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m10
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m11
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m12
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m13
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m14
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m15
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m16
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m17
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m18
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m19
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m20
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m21
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m22
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m23
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m24
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m25
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m26
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m27
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m28
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m29
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m30
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m31
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m32
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m33
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m34
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m35
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m36
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m37
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m38
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m39
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m40
            trigger ../jhwrf_ensda_pre == complete
        endfamily
        task jhwrf_ensda_output
          trigger members == complete
      endfamily
      family gempak
        task jhwrf_gempak
          trigger ../jhwrf_output == complete
        task jhwrf_gempak_meta
          trigger jhwrf_gempak == complete
      endfamily
      task jhwrf_archive
        trigger ensda == complete and jhwrf_output == complete
        event 1 hwrf
    endfamily
    family hwrf2
      edit NUM '2'
      family prep
        task jhwrf_launch
          trigger /prod12/gfs/prdgen/jgfs_pgrb2_manager:release_pgrb2_00
          edit ECF_PASS 'FREE'
          label storm2 " "
          event 1 running_GSI
          event 2 not_running_GSI
          time 15:00
        task jhwrf_init_gfs_f00
          trigger jhwrf_launch == complete
        task jhwrf_init_gdas_f03
          trigger jhwrf_launch == complete
        task jhwrf_init_gdas_f06
          trigger jhwrf_launch == complete
        task jhwrf_init_gdas_f09
          trigger jhwrf_launch == complete
        task jhwrf_ocean_init
          trigger jhwrf_launch == complete
          edit ECF_PASS 'FREE'
          event 1 run_couple
          event 2 run_noncouple
        task jhwrf_init_bdy
          trigger jhwrf_launch == complete and jhwrf_init_gfs_f00 == complete
      endfamily
      family analysis
        task jhwrf_fgat_relocate_gfs_f00
          trigger ../prep/jhwrf_init_gfs_f00 == complete
        task jhwrf_fgat_relocate_gdas_f09
          trigger ../prep/jhwrf_init_gdas_f09 == complete
        task jhwrf_fgat_relocate_gdas_f03
          trigger ../prep/jhwrf_init_gdas_f03 == complete
        task jhwrf_fgat_relocate_gdas_f06
          trigger ../prep/jhwrf_init_gdas_f06 == complete
        task jhwrf_bufrprep
          trigger jhwrf_fgat_relocate_gdas_f03 == complete and jhwrf_fgat_relocate_gdas_f06 == complete and jhwrf_fgat_relocate_gdas_f09 == complete and ../prep/jhwrf_launch == complete
        task jhwrf_nmm_gsi_d3
          trigger jhwrf_bufrprep == complete
        task jhwrf_nmm_gsi_d2
          trigger jhwrf_bufrprep == complete
        task jhwrf_merge
          trigger jhwrf_nmm_gsi_d2 == complete and jhwrf_nmm_gsi_d3 == complete and ../prep/jhwrf_ocean_init == complete and ../prep/jhwrf_init_gfs_f00 == complete
      endfamily
      family fcst
        task jhwrf_noncouple_forecast
          trigger ../prep/jhwrf_ocean_init:run_noncouple and ../analysis/jhwrf_merge == complete and ../prep/jhwrf_init_bdy == complete and ( /prod12/gfs/post/jgfs_post_f126 == active or /prod12/gfs/post/jgfs_post_f126 == complete )
        task jhwrf_couple_forecast
          trigger ../prep/jhwrf_ocean_init:run_couple and ../analysis/jhwrf_merge == complete and ../prep/jhwrf_init_bdy == complete and ( /prod12/gfs/post/jgfs_post_f126 == active or /prod12/gfs/post/jgfs_post_f126 == complete )
      endfamily
      family post
        task jhwrf_gsi_post
          trigger ../analysis/jhwrf_nmm_gsi_d2 == complete and ../analysis/jhwrf_nmm_gsi_d3 == complete
        task jhwrf_unpost
          trigger ../fcst == active or ../fcst == complete
        task jhwrf_post
          trigger jhwrf_unpost == complete
        task jhwrf_products
          trigger (jhwrf_unpost == active or jhwrf_unpost == complete) and (jhwrf_post == active or jhwrf_post == complete)
      endfamily
      task jhwrf_output
        trigger post == complete
      family ensda
        task jhwrf_ensda_pre
          trigger (../jhwrf_output == complete and /prod12/gdas/enkf/jgdas_enkf_post == complete)
          edit ECF_PASS 'FREE'
          event 1 run_ensda
          event 2 no_ensda
        family members
          trigger ../../hwrf1/ensda/members == complete
          task jhwrf_ensda_m1
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m2
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m3
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m4
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m5
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m6
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m7
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m8
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m9
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m10
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m11
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m12
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m13
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m14
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m15
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m16
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m17
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m18
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m19
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m20
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m21
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m22
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m23
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m24
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m25
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m26
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m27
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m28
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m29
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m30
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m31
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m32
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m33
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m34
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m35
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m36
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m37
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m38
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m39
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m40
            trigger ../jhwrf_ensda_pre == complete
        endfamily
        task jhwrf_ensda_output
          trigger members == complete
      endfamily
      family gempak
        task jhwrf_gempak
          trigger ../jhwrf_output == complete
        task jhwrf_gempak_meta
          trigger jhwrf_gempak == complete
      endfamily
      task jhwrf_archive
        trigger ensda == complete and jhwrf_output == complete
        event 1 hwrf
    endfamily
    family hwrf3
      edit NUM '3'
      family prep
        task jhwrf_launch
          trigger /prod12/gfs/prdgen/jgfs_pgrb2_manager:release_pgrb2_00
          edit ECF_PASS 'FREE'
          label storm3 " "
          event 1 running_GSI
          event 2 not_running_GSI
          time 15:00
        task jhwrf_init_gfs_f00
          trigger jhwrf_launch == complete
        task jhwrf_init_gdas_f03
          trigger jhwrf_launch == complete
        task jhwrf_init_gdas_f06
          trigger jhwrf_launch == complete
        task jhwrf_init_gdas_f09
          trigger jhwrf_launch == complete
        task jhwrf_ocean_init
          trigger jhwrf_launch == complete
          edit ECF_PASS 'FREE'
          event 1 run_couple
          event 2 run_noncouple
        task jhwrf_init_bdy
          trigger jhwrf_launch == complete and jhwrf_init_gfs_f00 == complete
      endfamily
      family analysis
        task jhwrf_fgat_relocate_gfs_f00
          trigger ../prep/jhwrf_init_gfs_f00 == complete
        task jhwrf_fgat_relocate_gdas_f09
          trigger ../prep/jhwrf_init_gdas_f09 == complete
        task jhwrf_fgat_relocate_gdas_f03
          trigger ../prep/jhwrf_init_gdas_f03 == complete
        task jhwrf_fgat_relocate_gdas_f06
          trigger ../prep/jhwrf_init_gdas_f06 == complete
        task jhwrf_bufrprep
          trigger jhwrf_fgat_relocate_gdas_f03 == complete and jhwrf_fgat_relocate_gdas_f06 == complete and jhwrf_fgat_relocate_gdas_f09 == complete and ../prep/jhwrf_launch == complete
        task jhwrf_nmm_gsi_d3
          trigger jhwrf_bufrprep == complete
        task jhwrf_nmm_gsi_d2
          trigger jhwrf_bufrprep == complete
        task jhwrf_merge
          trigger jhwrf_nmm_gsi_d2 == complete and jhwrf_nmm_gsi_d3 == complete and ../prep/jhwrf_ocean_init == complete and ../prep/jhwrf_init_gfs_f00 == complete
      endfamily
      family fcst
        task jhwrf_noncouple_forecast
          trigger ../prep/jhwrf_ocean_init:run_noncouple and ../analysis/jhwrf_merge == complete and ../prep/jhwrf_init_bdy == complete and ( /prod12/gfs/post/jgfs_post_f126 == active or /prod12/gfs/post/jgfs_post_f126 == complete )
        task jhwrf_couple_forecast
          trigger ../prep/jhwrf_ocean_init:run_couple and ../analysis/jhwrf_merge == complete and ../prep/jhwrf_init_bdy == complete and ( /prod12/gfs/post/jgfs_post_f126 == active or /prod12/gfs/post/jgfs_post_f126 == complete )
      endfamily
      family post
        task jhwrf_gsi_post
          trigger ../analysis/jhwrf_nmm_gsi_d2 == complete and ../analysis/jhwrf_nmm_gsi_d3 == complete
        task jhwrf_unpost
          trigger ../fcst == active or ../fcst == complete
        task jhwrf_post
          trigger jhwrf_unpost == complete
        task jhwrf_products
          trigger (jhwrf_unpost == active or jhwrf_unpost == complete) and (jhwrf_post == active or jhwrf_post == complete)
      endfamily
      task jhwrf_output
        trigger post == complete
      family ensda
        task jhwrf_ensda_pre
          trigger (../jhwrf_output == complete and /prod12/gdas/enkf/jgdas_enkf_post == complete)
          edit ECF_PASS 'FREE'
          event 1 run_ensda
          event 2 no_ensda
        family members
          trigger ../../hwrf2/ensda/members == complete
          task jhwrf_ensda_m1
            trigger ../jhwrf_ensda_pre == complete and ../../../hwrf1/ensda/members == complete
          task jhwrf_ensda_m2
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m3
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m4
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m5
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m6
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m7
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m8
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m9
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m10
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m11
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m12
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m13
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m14
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m15
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m16
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m17
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m18
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m19
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m20
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m21
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m22
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m23
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m24
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m25
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m26
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m27
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m28
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m29
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m30
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m31
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m32
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m33
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m34
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m35
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m36
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m37
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m38
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m39
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m40
            trigger ../jhwrf_ensda_pre == complete
        endfamily
        task jhwrf_ensda_output
          trigger members == complete
      endfamily
      family gempak
        task jhwrf_gempak
          trigger ../jhwrf_output == complete
        task jhwrf_gempak_meta
          trigger jhwrf_gempak == complete
      endfamily
      task jhwrf_archive
        trigger ensda == complete and jhwrf_output == complete
        event 1 hwrf
    endfamily
    family hwrf4
      edit NUM '4'
      family prep
        task jhwrf_launch
          trigger /prod12/gfs/prdgen/jgfs_pgrb2_manager:release_pgrb2_00
          edit ECF_PASS 'FREE'
          label storm4 " "
          event 1 running_GSI
          event 2 not_running_GSI
          time 15:00
        task jhwrf_init_gfs_f00
          trigger jhwrf_launch == complete
        task jhwrf_init_gdas_f03
          trigger jhwrf_launch == complete
        task jhwrf_init_gdas_f06
          trigger jhwrf_launch == complete
        task jhwrf_init_gdas_f09
          trigger jhwrf_launch == complete
        task jhwrf_ocean_init
          trigger jhwrf_launch == complete
          edit ECF_PASS 'FREE'
          event 1 run_couple
          event 2 run_noncouple
        task jhwrf_init_bdy
          trigger jhwrf_launch == complete and jhwrf_init_gfs_f00 == complete
      endfamily
      family analysis
        task jhwrf_fgat_relocate_gfs_f00
          trigger ../prep/jhwrf_init_gfs_f00 == complete
        task jhwrf_fgat_relocate_gdas_f09
          trigger ../prep/jhwrf_init_gdas_f09 == complete
        task jhwrf_fgat_relocate_gdas_f03
          trigger ../prep/jhwrf_init_gdas_f03 == complete
        task jhwrf_fgat_relocate_gdas_f06
          trigger ../prep/jhwrf_init_gdas_f06 == complete
        task jhwrf_bufrprep
          trigger jhwrf_fgat_relocate_gdas_f03 == complete and jhwrf_fgat_relocate_gdas_f06 == complete and jhwrf_fgat_relocate_gdas_f09 == complete and ../prep/jhwrf_launch == complete
        task jhwrf_nmm_gsi_d3
          trigger jhwrf_bufrprep == complete
        task jhwrf_nmm_gsi_d2
          trigger jhwrf_bufrprep == complete
        task jhwrf_merge
          trigger jhwrf_nmm_gsi_d2 == complete and jhwrf_nmm_gsi_d3 == complete and ../prep/jhwrf_ocean_init == complete and ../prep/jhwrf_init_gfs_f00 == complete
      endfamily
      family fcst
        task jhwrf_noncouple_forecast
          trigger ../prep/jhwrf_ocean_init:run_noncouple and ../analysis/jhwrf_merge == complete and ../prep/jhwrf_init_bdy == complete and ( /prod12/gfs/post/jgfs_post_f126 == active or /prod12/gfs/post/jgfs_post_f126 == complete )
        task jhwrf_couple_forecast
          trigger ../prep/jhwrf_ocean_init:run_couple and ../analysis/jhwrf_merge == complete and ../prep/jhwrf_init_bdy == complete and ( /prod12/gfs/post/jgfs_post_f126 == active or /prod12/gfs/post/jgfs_post_f126 == complete )
      endfamily
      family post
        task jhwrf_gsi_post
          trigger ../analysis/jhwrf_nmm_gsi_d2 == complete and ../analysis/jhwrf_nmm_gsi_d3 == complete
        task jhwrf_unpost
          trigger ../fcst == active or ../fcst == complete
        task jhwrf_post
          trigger jhwrf_unpost == complete
        task jhwrf_products
          trigger (jhwrf_unpost == active or jhwrf_unpost == complete) and (jhwrf_post == active or jhwrf_post == complete)
      endfamily
      task jhwrf_output
        trigger post == complete
      family ensda
        task jhwrf_ensda_pre
          trigger (../jhwrf_output == complete and /prod12/gdas/enkf/jgdas_enkf_post == complete)
          edit ECF_PASS 'FREE'
          event 1 run_ensda
          event 2 no_ensda
        family members
          trigger ../../hwrf3/ensda/members == complete
          task jhwrf_ensda_m1
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m2
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m3
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m4
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m5
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m6
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m7
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m8
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m9
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m10
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m11
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m12
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m13
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m14
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m15
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m16
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m17
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m18
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m19
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m20
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m21
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m22
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m23
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m24
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m25
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m26
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m27
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m28
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m29
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m30
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m31
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m32
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m33
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m34
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m35
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m36
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m37
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m38
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m39
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m40
            trigger ../jhwrf_ensda_pre == complete
        endfamily
        task jhwrf_ensda_output
          trigger members == complete
      endfamily
      family gempak
        task jhwrf_gempak
          trigger ../jhwrf_output == complete
        task jhwrf_gempak_meta
          trigger jhwrf_gempak == complete
      endfamily
      task jhwrf_archive
        trigger ensda == complete and jhwrf_output == complete
        event 1 hwrf
    endfamily
    family hwrf5
      edit NUM '5'
      family prep
        task jhwrf_launch
          trigger /prod12/gfs/prdgen/jgfs_pgrb2_manager:release_pgrb2_00
          edit ECF_PASS 'FREE'
          label storm5 " "
          event 1 running_GSI
          event 2 not_running_GSI
          time 15:00
        task jhwrf_init_gfs_f00
          trigger jhwrf_launch == complete
        task jhwrf_init_gdas_f03
          trigger jhwrf_launch == complete
        task jhwrf_init_gdas_f06
          trigger jhwrf_launch == complete
        task jhwrf_init_gdas_f09
          trigger jhwrf_launch == complete
        task jhwrf_ocean_init
          trigger jhwrf_launch == complete
          edit ECF_PASS 'FREE'
          event 1 run_couple
          event 2 run_noncouple
        task jhwrf_init_bdy
          trigger jhwrf_launch == complete and jhwrf_init_gfs_f00 == complete
      endfamily
      family analysis
        task jhwrf_fgat_relocate_gfs_f00
          trigger ../prep/jhwrf_init_gfs_f00 == complete
        task jhwrf_fgat_relocate_gdas_f09
          trigger ../prep/jhwrf_init_gdas_f09 == complete
        task jhwrf_fgat_relocate_gdas_f03
          trigger ../prep/jhwrf_init_gdas_f03 == complete
        task jhwrf_fgat_relocate_gdas_f06
          trigger ../prep/jhwrf_init_gdas_f06 == complete
        task jhwrf_bufrprep
          trigger jhwrf_fgat_relocate_gdas_f03 == complete and jhwrf_fgat_relocate_gdas_f06 == complete and jhwrf_fgat_relocate_gdas_f09 == complete and ../prep/jhwrf_launch == complete
        task jhwrf_nmm_gsi_d3
          trigger jhwrf_bufrprep == complete
        task jhwrf_nmm_gsi_d2
          trigger jhwrf_bufrprep == complete
        task jhwrf_merge
          trigger jhwrf_nmm_gsi_d2 == complete and jhwrf_nmm_gsi_d3 == complete and ../prep/jhwrf_ocean_init == complete and ../prep/jhwrf_init_gfs_f00 == complete
      endfamily
      family fcst
        task jhwrf_noncouple_forecast
          trigger ../prep/jhwrf_ocean_init:run_noncouple and ../analysis/jhwrf_merge == complete and ../prep/jhwrf_init_bdy == complete and ( /prod12/gfs/post/jgfs_post_f126 == active or /prod12/gfs/post/jgfs_post_f126 == complete )
        task jhwrf_couple_forecast
          trigger ../prep/jhwrf_ocean_init:run_couple and ../analysis/jhwrf_merge == complete and ../prep/jhwrf_init_bdy == complete and ( /prod12/gfs/post/jgfs_post_f126 == active or /prod12/gfs/post/jgfs_post_f126 == complete )
      endfamily
      family post
        task jhwrf_gsi_post
          trigger ../analysis/jhwrf_nmm_gsi_d2 == complete and ../analysis/jhwrf_nmm_gsi_d3 == complete
        task jhwrf_unpost
          trigger ../fcst == active or ../fcst == complete
        task jhwrf_post
          trigger jhwrf_unpost == complete
        task jhwrf_products
          trigger (jhwrf_unpost == active or jhwrf_unpost == complete) and (jhwrf_post == active or jhwrf_post == complete)
      endfamily
      task jhwrf_output
        trigger post == complete
      family ensda
        task jhwrf_ensda_pre
          trigger (../jhwrf_output == complete and /prod12/gdas/enkf/jgdas_enkf_post == complete)
          edit ECF_PASS 'FREE'
          event 1 run_ensda
          event 2 no_ensda
        family members
          trigger ../../hwrf4/ensda/members == complete
          task jhwrf_ensda_m1
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m2
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m3
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m4
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m5
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m6
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m7
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m8
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m9
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m10
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m11
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m12
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m13
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m14
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m15
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m16
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m17
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m18
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m19
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m20
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m21
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m22
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m23
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m24
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m25
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m26
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m27
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m28
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m29
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m30
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m31
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m32
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m33
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m34
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m35
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m36
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m37
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m38
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m39
            trigger ../jhwrf_ensda_pre == complete
          task jhwrf_ensda_m40
            trigger ../jhwrf_ensda_pre == complete
        endfamily
        task jhwrf_ensda_output
          trigger members == complete
      endfamily
      family gempak
        task jhwrf_gempak
          trigger ../jhwrf_output == complete
        task jhwrf_gempak_meta
          trigger jhwrf_gempak == complete
      endfamily
      task jhwrf_archive
        trigger ensda == complete and jhwrf_output == complete
        event 1 hwrf
    endfamily
    family hwrf6
      edit NUM '6'
      family prep
        task jhwrf_launch
          trigger /prod12/gfs/prdgen/jgfs_pgrb2_manager:release_pgrb2_00
          edit ECF_PASS 'FREE'
          label storm6 " "
          event 1 running_GSI
          event 2 not_running_GSI
          time 15:00
        task jhwrf_init_gfs_f00
          trigger jhwrf_launch == complete
        task jhwrf_init_gdas_f03
          trigger jhwrf_launch == complete
        task jhwrf_init_gdas_f06
          trigger jhwrf_launch == complete
        task jhwrf_init_gdas_f09
          trigger jhwrf_launch == complete
        task jhwrf_ocean_init
          trigger jhwrf_launch == complete
          edit ECF_PASS 'FREE'
          event 1 run_couple
          event 2 run_noncouple
        task jhwrf_init_bdy
          trigger jhwrf_launch == complete and jhwrf_init_gfs_f00 == complete
      endfamily
      family analysis
        task jhwrf_fgat_relocate_gfs_f00
          trigger ../prep/jhwrf_init_gfs_f00 == complete
        task jhwrf_fgat_relocate_gdas_f09
          trigger ../prep/jhwrf_init_gdas_f09 == complete
        task jhwrf_fgat_relocate_gdas_f03
          trigger ../prep/jhwrf_init_gdas_f03 == complete
        task jhwrf_fgat_relocate_gdas_f06
          trigger ../prep/jhwrf_init_gdas_f06 == complete
        task jhwrf_bufrprep
          trigger jhwrf_fgat_relocate_gdas_f03 == complete and jhwrf_fgat_relocate_gdas_f06 == complete and jhwrf_fgat_relocate_gdas_f09 == complete and ../prep/jhwrf_launch == complete
        task jhwrf_nmm_gsi_d3
          trigger jhwrf_bufrprep == complete
        task jhwrf_nmm_gsi_d2
          trigger jhwrf_bufrprep == complete
        task jhwrf_merge
          trigger jhwrf_nmm_gsi_d2 == complete and jhwrf_nmm_gsi_d3 == complete and ../prep/jhwrf_ocean_init == complete and ../prep/jhwrf_init_gfs_f00 == complete
      endfamily
      family fcst
        task jhwrf_noncouple_forecast
          trigger ../prep/jhwrf_ocean_init:run_noncouple and ../analysis/jhwrf_merge == complete and ../prep/jhwrf_init_bdy == complete and ( /prod12/gfs/post/jgfs_post_f126 == active or /prod12/gfs/post/jgfs_post_f126 == complete )
        task jhwrf_couple_forecast
          trigger ../prep/jhwrf_ocean_init:run_couple and ../analysis/jhwrf_merge == complete and ../prep/jhwrf_init_bdy == complete and ( /prod12/gfs/post/jgfs_post_f126 == active or /prod12/gfs/post/jgfs_post_f126 == complete )
      endfamily
      family post
        task jhwrf_gsi_post
          trigger ../analysis/jhwrf_nmm_gsi_d2 == complete and ../analysis/jhwrf_nmm_gsi_d3 == complete
        task jhwrf_unpost
          trigger ../fcst == active or ../fcst == complete
        task jhwrf_post
          trigger jhwrf_unpost == complete
        task jhwrf_products
          trigger (jhwrf_unpost == active or jhwrf_unpost == complete) and (jhwrf_post == active or jhwrf_post == complete)
      endfamily
      task jhwrf_output
        trigger post == complete
      family gempak
        task jhwrf_gempak
          trigger ../jhwrf_output == complete
        task jhwrf_gempak_meta
          trigger jhwrf_gempak == complete
      endfamily
      task jhwrf_archive
        trigger jhwrf_output == complete
        event 1 hwrf
    endfamily
    family hwrf7
      edit NUM '7'
      family prep
        task jhwrf_launch
          trigger /prod12/gfs/prdgen/jgfs_pgrb2_manager:release_pgrb2_00
          edit ECF_PASS 'FREE'
          label storm7 " "
          event 1 running_GSI
          event 2 not_running_GSI
          time 15:00
        task jhwrf_init_gfs_f00
          trigger jhwrf_launch == complete
        task jhwrf_init_gdas_f03
          trigger jhwrf_launch == complete
        task jhwrf_init_gdas_f06
          trigger jhwrf_launch == complete
        task jhwrf_init_gdas_f09
          trigger jhwrf_launch == complete
        task jhwrf_ocean_init
          trigger jhwrf_launch == complete
          edit ECF_PASS 'FREE'
          event 1 run_couple
          event 2 run_noncouple
        task jhwrf_init_bdy
          trigger jhwrf_launch == complete and jhwrf_init_gfs_f00 == complete
      endfamily
      family analysis
        task jhwrf_fgat_relocate_gfs_f00
          trigger ../prep/jhwrf_init_gfs_f00 == complete
        task jhwrf_fgat_relocate_gdas_f09
          trigger ../prep/jhwrf_init_gdas_f09 == complete
        task jhwrf_fgat_relocate_gdas_f03
          trigger ../prep/jhwrf_init_gdas_f03 == complete
        task jhwrf_fgat_relocate_gdas_f06
          trigger ../prep/jhwrf_init_gdas_f06 == complete
        task jhwrf_bufrprep
          trigger jhwrf_fgat_relocate_gdas_f03 == complete and jhwrf_fgat_relocate_gdas_f06 == complete and jhwrf_fgat_relocate_gdas_f09 == complete and ../prep/jhwrf_launch == complete
        task jhwrf_nmm_gsi_d3
          trigger jhwrf_bufrprep == complete
        task jhwrf_nmm_gsi_d2
          trigger jhwrf_bufrprep == complete
        task jhwrf_merge
          trigger jhwrf_nmm_gsi_d2 == complete and jhwrf_nmm_gsi_d3 == complete and ../prep/jhwrf_ocean_init == complete and ../prep/jhwrf_init_gfs_f00 == complete
      endfamily
      family fcst
        task jhwrf_noncouple_forecast
          trigger ../prep/jhwrf_ocean_init:run_noncouple and ../analysis/jhwrf_merge == complete and ../prep/jhwrf_init_bdy == complete and ( /prod12/gfs/post/jgfs_post_f126 == active or /prod12/gfs/post/jgfs_post_f126 == complete )
        task jhwrf_couple_forecast
          trigger ../prep/jhwrf_ocean_init:run_couple and ../analysis/jhwrf_merge == complete and ../prep/jhwrf_init_bdy == complete and ( /prod12/gfs/post/jgfs_post_f126 == active or /prod12/gfs/post/jgfs_post_f126 == complete )
      endfamily
      family post
        task jhwrf_gsi_post
          trigger ../analysis/jhwrf_nmm_gsi_d2 == complete and ../analysis/jhwrf_nmm_gsi_d3 == complete
        task jhwrf_unpost
          trigger ../fcst == active or ../fcst == complete
        task jhwrf_post
          trigger jhwrf_unpost == complete
        task jhwrf_products
          trigger (jhwrf_unpost == active or jhwrf_unpost == complete) and (jhwrf_post == active or jhwrf_post == complete)
      endfamily
      task jhwrf_output
        trigger post == complete
      family gempak
        task jhwrf_gempak
          trigger ../jhwrf_output == complete
        task jhwrf_gempak_meta
          trigger jhwrf_gempak == complete
      endfamily
      task jhwrf_archive
        trigger jhwrf_output == complete
        event 1 hwrf
    endfamily
  endfamily
  family hur
    edit ECF_FILES '/ecf/ecfnets/scripts/hur/'
    edit COM 'com2'
    edit QUEUE 'prod2'
    edit QUEUESERV 'prod2_serv'
    family hur1
      edit NUM '1'
      edit ECF_JOB_CMD '/ecf/ecfutils/unixsubmit %ECF_JOB% %ECF_JOBOUT% ibmsp'
      family prep
        task jhur_prep_master
          trigger /prod12/gfs/post/jgfs_post_f00 == complete
          label storm1 " "
        task jhur_prep_atmos
          trigger jhur_prep_master == complete and /prod12/hwrf/hwrf1/prep/jhwrf_ocean_init == complete
        task jhur_prep_wake
          trigger jhur_prep_atmos == complete and /prod12/hwrf/hwrf1/prep/jhwrf_ocean_init == complete
        task jhur_prep_lats
          trigger jhur_prep_atmos == complete and ( /prod12/gfs/post/jgfs_post_f126 == active or /prod12/gfs/post/jgfs_post_f126 == complete )
      endfamily
      task jhur_forecast
        trigger prep/jhur_prep_wake == complete and prep/jhur_prep_lats == complete
      family post
        task jhur_post_swath
          trigger ../jhur_forecast == complete
        task jhur_post
          trigger ../jhur_forecast == complete
          event 1 email_sdm
        task jhur_email
          trigger jhur_post:email_sdm
        task jhur_tracker
          trigger jhur_post == complete
        task jhur_archive
          trigger jhur_post == complete
      endfamily
      family gempak
        task jhur_gempak
          trigger ../post/jhur_post == complete
        task jhur_gempak_meta
          trigger jhur_gempak == complete
      endfamily
    endfamily
    family hur2
      edit NUM '2'
      family prep
        task jhur_prep_master
          trigger /prod12/gfs/post/jgfs_post_f00 == complete
          label storm2 " "
        task jhur_prep_atmos
          trigger jhur_prep_master == complete and /prod12/hwrf/hwrf2/prep/jhwrf_ocean_init == complete
        task jhur_prep_wake
          trigger jhur_prep_atmos == complete and /prod12/hwrf/hwrf2/prep/jhwrf_ocean_init == complete
        task jhur_prep_lats
          trigger jhur_prep_atmos == complete and ( /prod12/gfs/post/jgfs_post_f126 == active or /prod12/gfs/post/jgfs_post_f126 == complete )
      endfamily
      task jhur_forecast
        trigger prep/jhur_prep_wake == complete and prep/jhur_prep_lats == complete
      family post
        task jhur_post_swath
          trigger ../jhur_forecast == complete
        task jhur_post
          trigger ../jhur_forecast == complete
          event 1 email_sdm
        task jhur_email
          trigger jhur_post:email_sdm
        task jhur_tracker
          trigger jhur_post == complete
        task jhur_archive
          trigger jhur_post == complete
      endfamily
      family gempak
        task jhur_gempak
          trigger ../post/jhur_post == complete
        task jhur_gempak_meta
          trigger jhur_gempak == complete
      endfamily
    endfamily
    family hur3
      edit NUM '3'
      family prep
        task jhur_prep_master
          trigger /prod12/gfs/post/jgfs_post_f00 == complete
          label storm3 " "
        task jhur_prep_atmos
          trigger jhur_prep_master == complete and /prod12/hwrf/hwrf3/prep/jhwrf_ocean_init == complete
        task jhur_prep_wake
          trigger jhur_prep_atmos == complete and /prod12/hwrf/hwrf3/prep/jhwrf_ocean_init == complete
        task jhur_prep_lats
          trigger jhur_prep_atmos == complete and ( /prod12/gfs/post/jgfs_post_f126 == active or /prod12/gfs/post/jgfs_post_f126 == complete )
      endfamily
      task jhur_forecast
        trigger prep/jhur_prep_wake == complete and prep/jhur_prep_lats == complete
      family post
        task jhur_post_swath
          trigger ../jhur_forecast == complete
        task jhur_post
          trigger ../jhur_forecast == complete
          event 1 email_sdm
        task jhur_email
          trigger jhur_post:email_sdm
        task jhur_tracker
          trigger jhur_post == complete
        task jhur_archive
          trigger jhur_post == complete
      endfamily
      family gempak
        task jhur_gempak
          trigger ../post/jhur_post == complete
        task jhur_gempak_meta
          trigger jhur_gempak == complete
      endfamily
    endfamily
    family hur4
      edit NUM '4'
      family prep
        task jhur_prep_master
          trigger /prod12/gfs/post/jgfs_post_f00 == complete
          label storm4 " "
        task jhur_prep_atmos
          trigger jhur_prep_master == complete and /prod12/hwrf/hwrf4/prep/jhwrf_ocean_init == complete
        task jhur_prep_wake
          trigger jhur_prep_atmos == complete and /prod12/hwrf/hwrf4/prep/jhwrf_ocean_init == complete
        task jhur_prep_lats
          trigger jhur_prep_atmos == complete and ( /prod12/gfs/post/jgfs_post_f126 == active or /prod12/gfs/post/jgfs_post_f126 == complete )
      endfamily
      task jhur_forecast
        trigger prep/jhur_prep_wake == complete and prep/jhur_prep_lats == complete
      family post
        task jhur_post_swath
          trigger ../jhur_forecast == complete
        task jhur_post
          trigger ../jhur_forecast == complete
          event 1 email_sdm
        task jhur_email
          trigger jhur_post:email_sdm
        task jhur_tracker
          trigger jhur_post == complete
        task jhur_archive
          trigger jhur_post == complete
      endfamily
      family gempak
        task jhur_gempak
          trigger ../post/jhur_post == complete
        task jhur_gempak_meta
          trigger jhur_gempak == complete
      endfamily
    endfamily
    family hur5
      edit NUM '5'
      family prep
        task jhur_prep_master
          trigger /prod12/gfs/post/jgfs_post_f00 == complete
          label storm5 " "
        task jhur_prep_atmos
          trigger jhur_prep_master == complete and /prod12/hwrf/hwrf5/prep/jhwrf_ocean_init == complete
        task jhur_prep_wake
          trigger jhur_prep_atmos == complete and /prod12/hwrf/hwrf5/prep/jhwrf_ocean_init == complete
        task jhur_prep_lats
          trigger jhur_prep_atmos == complete and ( /prod12/gfs/post/jgfs_post_f126 == active or /prod12/gfs/post/jgfs_post_f126 == complete )
      endfamily
      task jhur_forecast
        trigger prep/jhur_prep_wake == complete and prep/jhur_prep_lats == complete
      family post
        task jhur_post_swath
          trigger ../jhur_forecast == complete
        task jhur_post
          trigger ../jhur_forecast == complete
          event 1 email_sdm
        task jhur_email
          trigger jhur_post:email_sdm
        task jhur_tracker
          trigger jhur_post == complete
        task jhur_archive
          trigger jhur_post == complete
      endfamily
      family gempak
        task jhur_gempak
          trigger ../post/jhur_post == complete
        task jhur_gempak_meta
          trigger jhur_gempak == complete
      endfamily
    endfamily
  endfamily
  family cdas2_12
    task jcdas2_prep_12
      trigger /prod12/gfs/prep/jgfs_prep == complete
      edit ECF_PASS 'FREE'
      time 16:00
    task jcdas2_forecast_12
      trigger /prod12/cdas2_12/jcdas2_prep_12 == complete
      edit ECF_PASS 'FREE'
  endfamily
  family omb12
    task jfog_12
      trigger /prod12/gfs/prdgen/jgfs_pgrb2_f180 == complete
    task jsice_12
      trigger /prod12/gfs/prdgen/jgfs_pgrb2_f180 == complete
  endfamily
  family rtofs
    edit PROJ 'RTO-OPS'
    edit ECF_FILES '/ecf/ecfnets/scripts/rtofs'
    task jrtofs_global_forecast_step2_pre
      trigger /prod06/rtofs/jrtofs_global_forecast_step1 eq complete
    task jrtofs_global_forecast_step2
      trigger /prod12/rtofs/jrtofs_global_forecast_step2_pre == complete and (/prod12/nam/forecast/jnam_forecast_3660h == complete and /prod12/nam/forecast/jnam_forecast_6084h == active)
    family grib2_post
      task jrtofs_global_forecast_grib2_post_d04
        trigger /prod12/rtofs/jrtofs_global_forecast_step2 eq complete
    endfamily
    family post
      task jrtofs_global_forecast_post_d05
        trigger /prod12/rtofs/jrtofs_global_forecast_step2 eq complete
      task jrtofs_global_forecast_post_d06
        trigger /prod12/rtofs/jrtofs_global_forecast_step2 eq complete
      task jrtofs_global_forecast_post_d07
        trigger /prod12/rtofs/jrtofs_global_forecast_step2 eq complete
      task jrtofs_global_forecast_post_d08
        trigger /prod12/rtofs/jrtofs_global_forecast_step2 eq complete
    endfamily
    task jrtofs_global_gzip
      trigger /prod12/rtofs/post == complete
      edit PTILE '32'
      edit NTASK '96'
  endfamily
  family cdas00
    edit ECF_FILES '/ecf/ecfnets/scripts/cdas'
    edit CYC '00'
    edit PROJ 'REAN-T2O'
    family dump
      task jcdas_dump
        edit ECF_PASS 'FREE'
        time 16:10
      task jcdas_dump_post
        trigger /prod12/cdas00/dump/jcdas_dump == complete
        edit ECF_PASS 'FREE'
    endfamily
    family prep
      task jcdas_prep1
        trigger /prod12/cdas00/dump/jcdas_dump == complete
        edit ECF_PASS 'FREE'
      task jcdas_prep1_post
        trigger /prod12/cdas00/prep/jcdas_prep1 == complete
        edit ECF_PASS 'FREE'
      task jcdas_prep2
        trigger /prod12/cdas00/prep/jcdas_prep1 == complete
        edit ECF_PASS 'FREE'
      task jcdas_prep2_post
        trigger /prod12/cdas00/prep/jcdas_prep2 == complete
        edit ECF_PASS 'FREE'
    endfamily
    task jcdas_forecast
      trigger /prod12/cdas00/prep/jcdas_prep2 == complete
      edit ECF_PASS 'FREE'
  endfamily
  family nos
    edit PROJ 'NMO-OPS'
    family nos_ofs
      family prep
        task jnos_dbofs_prep
          time 13:00
        task jnos_tbofs_prep
          time 13:00
        task jnos_cbofs_prep
          time 13:00
        task jnos_ngofs_prep
          time 14:45
        task jnos_sfbofs_prep
          time 15:25
        task jnos_creofs_prep
          time 15:50
        task jnos_negofs_prep
          trigger ../forecast/jnos_ngofs_nowcst_fcst == complete
        task jnos_nwgofs_prep
          trigger ../forecast/jnos_ngofs_nowcst_fcst == complete
      endfamily
      family forecast
        task jnos_tbofs_nowcst_fcst
          trigger ../prep/jnos_tbofs_prep == complete
        task jnos_dbofs_nowcst_fcst
          trigger ../prep/jnos_dbofs_prep == complete
        task jnos_cbofs_nowcst_fcst
          trigger ../prep/jnos_cbofs_prep == complete
        task jnos_sfbofs_nowcst_fcst
          trigger ../prep/jnos_sfbofs_prep == complete
        task jnos_ngofs_nowcst_fcst
          trigger ../prep/jnos_ngofs_prep == complete
        task jnos_negofs_nowcst_fcst
          trigger ../prep/jnos_negofs_prep == complete
        task jnos_nwgofs_nowcst_fcst
          trigger ../prep/jnos_nwgofs_prep == complete
        task jnos_creofs_nowcst_fcst
          trigger ../prep/jnos_creofs_prep == complete
      endfamily
    endfamily
  endfamily
  family gfs
    edit PROJ 'GFS-OPS'
    family dump
      task jgfs_jtwc_bull_email
        defstatus complete
      task jgfs_tropcy_qc_reloc
        time 14:41
      task jgfs_dump
        event 1 release_sfcprep
        time 14:47
      task jgfs_dump_alert
        trigger jgfs_dump_post:release_gfs12_dump_alert
      task jgfs_dump_post
        trigger jgfs_dump eq complete
        event 1 release_gfs12_dump_alert
      task jmodel_realtime_gfs
        trigger jgfs_dump_alert eq complete
    endfamily
    family prep
      task jgfs_emcsfc_sfc_prep
        trigger ../dump/jgfs_dump:release_sfcprep
      task jgfs_prep
        trigger ../dump/jgfs_dump eq complete and ../dump/jgfs_tropcy_qc_reloc eq complete
      task jgfs_prep_post
        trigger ../jgfs_analysis eq complete
    endfamily
    task jgfs_analysis
      trigger prep/jgfs_prep eq complete and prep/jgfs_emcsfc_sfc_prep == complete
    family forecast
      task jgfs_forecast_high
        trigger ../jgfs_analysis eq complete
      task jgfs_forecast_low
        trigger jgfs_forecast_high eq complete
    endfamily
    family sminit_guam
      task jgfs_sminit_guam
        trigger ../post/jgfs_post_anl eq active or ../post/jgfs_post_anl eq complete
    endfamily
    family post_processing
      family fax
        task jgfs_fax_f00
          trigger ../../prdgen/jgfs_pgrb2_f00 == complete and ../../prdgen/jgfs_pgrb2_anl == complete
        task jgfs_fax_anl
          trigger ../../prdgen/jgfs_pgrb2_anl == complete
        task jgfs_fax_wafs_f12
          trigger ../../prdgen/jgfs_pgrb2_f12 == complete
        task jgfs_fax_wafs_f24
          trigger ../../prdgen/jgfs_pgrb2_f24 == complete
        task jgfs_fax_wafs_f36
          trigger ../../prdgen/jgfs_pgrb2_f36 == complete
        task jgfs_fax_f48
          trigger ../../prdgen/jgfs_pgrb2_f48 == complete
      endfamily
      family grib_wafs
        task jgfs_wafs_f00
          trigger ../../prdgen/jgfs_pgrb2_f00 == complete and ../../prdgen/jgfs_pgrb2_f120 == complete and ../grib2_wafs/jgfs_wafs_grib2 eq complete
        task jgfs_wafs_f06
          trigger ../../prdgen/jgfs_pgrb2_f06 == complete and jgfs_wafs_f00 eq complete
        task jgfs_wafs_f12
          trigger ../../prdgen/jgfs_pgrb2_f12 == complete and jgfs_wafs_f06 eq complete
        task jgfs_wafs_f18
          trigger ../../prdgen/jgfs_pgrb2_f18 == complete and jgfs_wafs_f12 eq complete
        task jgfs_wafs_f24
          trigger ../../prdgen/jgfs_pgrb2_f24 == complete and jgfs_wafs_f18 eq complete
        task jgfs_wafs_f30
          trigger ../../prdgen/jgfs_pgrb2_f30 == complete and jgfs_wafs_f24 eq complete
        task jgfs_wafs_f36
          trigger ../../prdgen/jgfs_pgrb2_f36 == complete and jgfs_wafs_f30 eq complete
        task jgfs_wafs_f42
          trigger ../../prdgen/jgfs_pgrb2_f42 == complete and jgfs_wafs_f36 eq complete
        task jgfs_wafs_f48
          trigger ../../prdgen/jgfs_pgrb2_f48 == complete and jgfs_wafs_f42 eq complete
        task jgfs_wafs_f54
          trigger ../../prdgen/jgfs_pgrb2_f54 == complete and jgfs_wafs_f48 eq complete
        task jgfs_wafs_f60
          trigger ../../prdgen/jgfs_pgrb2_f60 == complete and jgfs_wafs_f54 eq complete
        task jgfs_wafs_f66
          trigger ../../prdgen/jgfs_pgrb2_f66 == complete and jgfs_wafs_f60 eq complete
        task jgfs_wafs_f72
          trigger ../../prdgen/jgfs_pgrb2_f72 == complete and jgfs_wafs_f66 eq complete
        task jgfs_wafs_f78
          trigger ../../prdgen/jgfs_pgrb2_f78 == complete and jgfs_wafs_f72 eq complete
        task jgfs_wafs_f84
          trigger ../../prdgen/jgfs_pgrb2_f84 == complete and jgfs_wafs_f78 eq complete
        task jgfs_wafs_f90
          trigger ../../prdgen/jgfs_pgrb2_f90 == complete and jgfs_wafs_f84 eq complete
        task jgfs_wafs_f96
          trigger ../../prdgen/jgfs_pgrb2_f96 == complete and jgfs_wafs_f90 eq complete
        task jgfs_wafs_f102
          trigger ../../prdgen/jgfs_pgrb2_f102 == complete and jgfs_wafs_f96 eq complete
        task jgfs_wafs_f108
          trigger ../../prdgen/jgfs_pgrb2_f108 == complete and jgfs_wafs_f102 eq complete
        task jgfs_wafs_f114
          trigger ../../prdgen/jgfs_pgrb2_f114 == complete and jgfs_wafs_f108 eq complete
        task jgfs_wafs_f120
          trigger ../../prdgen/jgfs_pgrb2_f120 == complete and jgfs_wafs_f114 eq complete
      endfamily
      family bulletins
        task jgfs_fbwind
          trigger ../../post/jgfs_post_f06 == complete and ../../post/jgfs_post_f12 == complete and ../../post/jgfs_post_f24 == complete
        task jgfs_cyclone_tracker
          trigger ../../post/jgfs_post_f00 == complete and ../../post/jgfs_post_f06 == complete and ../../post/jgfs_post_f12 == complete and ../../post/jgfs_post_f18 == complete and ../../post/jgfs_post_f24 == complete and ../../post/jgfs_post_f30 == complete and ../../post/jgfs_post_f36 == complete and ../../post/jgfs_post_f42 == complete and ../../post/jgfs_post_f48 == complete and ../../post/jgfs_post_f54 == complete and ../../post/jgfs_post_f60 == complete and ../../post/jgfs_post_f66 == complete and ../../post/jgfs_post_f72 == complete and ../../post/jgfs_post_f78 == complete and ../../post/jgfs_post_f84 == complete and ../../post/jgfs_post_f90 == complete and ../../post/jgfs_post_f96 == complete and ../../post/jgfs_post_f102 == complete and ../../post/jgfs_post_f108 == complete and ../../post/jgfs_post_f114 == complete and ../../post/jgfs_post_f120 == complete and ../../post/jgfs_post_f126 == complete and ../../post/jgfs_post_f132 == complete and ../../post/jgfs_post_f138 == complete and ../../post/jgfs_post_f144 == complete and ../../post/jgfs_post_f150 == complete and ../../post/jgfs_post_f156 == complete and ../../post/jgfs_post_f162 == complete and ../../post/jgfs_post_f168 == complete and ../../post/jgfs_post_f174 == complete and ../../post/jgfs_post_f180 == complete
      endfamily
      family grib2_wafs
        task jgfs_wafs_grib2
          trigger ../../prdgen/jgfs_pgrb2_f00 == complete
        task jgfs_wafs_blending
          trigger jgfs_wafs_grib2 == complete
          time 16:33
      endfamily
      family bufr_sounding
        task jgfs_postsnd
          trigger ../../post/jgfs_post_manager:release_post00
      endfamily
      family grib_awips
        task jgfs_awips_f00
          trigger ../../prdgen/jgfs_pgrb2_f00 == complete
        task jgfs_awips_f06
          trigger ../../prdgen/jgfs_pgrb2_f06 == complete
        task jgfs_awips_f12
          trigger ../../prdgen/jgfs_pgrb2_f12 == complete
        task jgfs_awips_f18
          trigger ../../prdgen/jgfs_pgrb2_f18 == complete
        task jgfs_awips_f24
          trigger ../../prdgen/jgfs_pgrb2_f24 == complete
        task jgfs_awips_f30
          trigger ../../prdgen/jgfs_pgrb2_f30 == complete
        task jgfs_awips_f36
          trigger ../../prdgen/jgfs_pgrb2_f36 == complete
        task jgfs_awips_f42
          trigger ../../prdgen/jgfs_pgrb2_f42 == complete
        task jgfs_awips_f48
          trigger ../../prdgen/jgfs_pgrb2_f48 == complete
        task jgfs_awips_f54
          trigger ../../prdgen/jgfs_pgrb2_f54 == complete
        task jgfs_awips_f60
          trigger ../../prdgen/jgfs_pgrb2_f60 == complete
        task jgfs_awips_f66
          trigger ../../prdgen/jgfs_pgrb2_f66 == complete
        task jgfs_awips_f72
          trigger ../../prdgen/jgfs_pgrb2_f72 == complete
        task jgfs_awips_f78
          trigger ../../prdgen/jgfs_pgrb2_f78 == complete
        task jgfs_awips_f84
          trigger ../../prdgen/jgfs_pgrb2_f84 == complete
        task jgfs_awips_f90
          trigger ../../prdgen/jgfs_pgrb2_f90 == complete
        task jgfs_awips_f96
          trigger ../../prdgen/jgfs_pgrb2_f96 == complete
        task jgfs_awips_f102
          trigger ../../prdgen/jgfs_pgrb2_f102 == complete
        task jgfs_awips_f108
          trigger ../../prdgen/jgfs_pgrb2_f108 == complete
        task jgfs_awips_f114
          trigger ../../prdgen/jgfs_pgrb2_f114 == complete
        task jgfs_awips_f120
          trigger ../../prdgen/jgfs_pgrb2_f120 == complete
        task jgfs_awips_f126
          trigger ../../prdgen/jgfs_pgrb2_f126 == complete
        task jgfs_awips_f132
          trigger ../../prdgen/jgfs_pgrb2_f132 == complete
        task jgfs_awips_f138
          trigger ../../prdgen/jgfs_pgrb2_f138 == complete
        task jgfs_awips_f144
          trigger ../../prdgen/jgfs_pgrb2_f144 == complete
        task jgfs_awips_f150
          trigger ../../prdgen/jgfs_pgrb2_f150 == complete
        task jgfs_awips_f156
          trigger ../../prdgen/jgfs_pgrb2_f156 == complete
        task jgfs_awips_f162
          trigger ../../prdgen/jgfs_pgrb2_f162 == complete
        task jgfs_awips_f168
          trigger ../../prdgen/jgfs_pgrb2_f168 == complete
        task jgfs_awips_f174
          trigger ../../prdgen/jgfs_pgrb2_f174 == complete
        task jgfs_awips_f180
          trigger ../../prdgen/jgfs_pgrb2_f180 == complete
        task jgfs_awips_f186
          trigger ../../prdgen/jgfs_pgrb2_f186 == complete
        task jgfs_awips_f192
          trigger ../../prdgen/jgfs_pgrb2_f192 == complete
        task jgfs_awips_f198
          trigger ../../prdgen/jgfs_pgrb2_f198 == complete
        task jgfs_awips_f204
          trigger ../../prdgen/jgfs_pgrb2_f204 == complete
        task jgfs_awips_f210
          trigger ../../prdgen/jgfs_pgrb2_f210 == complete
        task jgfs_awips_f216
          trigger ../../prdgen/jgfs_pgrb2_f216 == complete
        task jgfs_awips_f222
          trigger ../../prdgen/jgfs_pgrb2_f222 == complete
        task jgfs_awips_f228
          trigger ../../prdgen/jgfs_pgrb2_f228 == complete
        task jgfs_awips_f234
          trigger ../../prdgen/jgfs_pgrb2_f234 == complete
        task jgfs_awips_f240
          trigger ../../prdgen/jgfs_pgrb2_f240 == complete
      endfamily
      family awips_1p0deg
        edit ECF_FILES '/ecf/ecfnets/scripts/gfs/post_processing/awips'
        edit RES '1p0deg'
        edit RESC '1P0DEG'
        task jgfs_awips_f000
          trigger ../../prdgen/jgfs_pgrb2_f00 == complete
        task jgfs_awips_f006
          trigger ../../prdgen/jgfs_pgrb2_f06 == complete
        task jgfs_awips_f012
          trigger ../../prdgen/jgfs_pgrb2_f12 == complete
        task jgfs_awips_f018
          trigger ../../prdgen/jgfs_pgrb2_f18 == complete
        task jgfs_awips_f024
          trigger ../../prdgen/jgfs_pgrb2_f24 == complete
        task jgfs_awips_f030
          trigger ../../prdgen/jgfs_pgrb2_f30 == complete
        task jgfs_awips_f036
          trigger ../../prdgen/jgfs_pgrb2_f36 == complete
        task jgfs_awips_f042
          trigger ../../prdgen/jgfs_pgrb2_f42 == complete
        task jgfs_awips_f048
          trigger ../../prdgen/jgfs_pgrb2_f48 == complete
        task jgfs_awips_f054
          trigger ../../prdgen/jgfs_pgrb2_f54 == complete
        task jgfs_awips_f060
          trigger ../../prdgen/jgfs_pgrb2_f60 == complete
        task jgfs_awips_f066
          trigger ../../prdgen/jgfs_pgrb2_f66 == complete
        task jgfs_awips_f072
          trigger ../../prdgen/jgfs_pgrb2_f72 == complete
        task jgfs_awips_f078
          trigger ../../prdgen/jgfs_pgrb2_f78 == complete
        task jgfs_awips_f084
          trigger ../../prdgen/jgfs_pgrb2_f84 == complete
        task jgfs_awips_f090
          trigger ../../prdgen/jgfs_pgrb2_f90 == complete
        task jgfs_awips_f096
          trigger ../../prdgen/jgfs_pgrb2_f96 == complete
        task jgfs_awips_f102
          trigger ../../prdgen/jgfs_pgrb2_f102 == complete
        task jgfs_awips_f108
          trigger ../../prdgen/jgfs_pgrb2_f108 == complete
        task jgfs_awips_f114
          trigger ../../prdgen/jgfs_pgrb2_f114 == complete
        task jgfs_awips_f120
          trigger ../../prdgen/jgfs_pgrb2_f120 == complete
        task jgfs_awips_f126
          trigger ../../prdgen/jgfs_pgrb2_f126 == complete
        task jgfs_awips_f132
          trigger ../../prdgen/jgfs_pgrb2_f132 == complete
        task jgfs_awips_f138
          trigger ../../prdgen/jgfs_pgrb2_f138 == complete
        task jgfs_awips_f144
          trigger ../../prdgen/jgfs_pgrb2_f144 == complete
        task jgfs_awips_f150
          trigger ../../prdgen/jgfs_pgrb2_f150 == complete
        task jgfs_awips_f156
          trigger ../../prdgen/jgfs_pgrb2_f156 == complete
        task jgfs_awips_f162
          trigger ../../prdgen/jgfs_pgrb2_f162 == complete
        task jgfs_awips_f168
          trigger ../../prdgen/jgfs_pgrb2_f168 == complete
        task jgfs_awips_f174
          trigger ../../prdgen/jgfs_pgrb2_f174 == complete
        task jgfs_awips_f180
          trigger ../../prdgen/jgfs_pgrb2_f180 == complete
        task jgfs_awips_f186
          trigger ../../prdgen/jgfs_pgrb2_f186 == complete
        task jgfs_awips_f192
          trigger ../../prdgen/jgfs_pgrb2_f192 == complete
        task jgfs_awips_f198
          trigger ../../prdgen/jgfs_pgrb2_f198 == complete
        task jgfs_awips_f204
          trigger ../../prdgen/jgfs_pgrb2_f204 == complete
        task jgfs_awips_f210
          trigger ../../prdgen/jgfs_pgrb2_f210 == complete
        task jgfs_awips_f216
          trigger ../../prdgen/jgfs_pgrb2_f216 == complete
        task jgfs_awips_f222
          trigger ../../prdgen/jgfs_pgrb2_f222 == complete
        task jgfs_awips_f228
          trigger ../../prdgen/jgfs_pgrb2_f228 == complete
        task jgfs_awips_f234
          trigger ../../prdgen/jgfs_pgrb2_f234 == complete
        task jgfs_awips_f240
          trigger ../../prdgen/jgfs_pgrb2_f240 == complete
      endfamily
      family awips_20km
        edit ECF_FILES '/ecf/ecfnets/scripts/gfs/post_processing/awips'
        edit RES '20km'
        edit RESC '20KM'
        task jgfs_awips_f000
          trigger ../../prdgen/jgfs_pgrb2_f00 == complete
        task jgfs_awips_f003
          trigger ../../prdgen/jgfs_pgrb2_f03 == complete
        task jgfs_awips_f006
          trigger ../../prdgen/jgfs_pgrb2_f06 == complete
        task jgfs_awips_f009
          trigger ../../prdgen/jgfs_pgrb2_f09 == complete
        task jgfs_awips_f012
          trigger ../../prdgen/jgfs_pgrb2_f12 == complete
        task jgfs_awips_f015
          trigger ../../prdgen/jgfs_pgrb2_f15 == complete
        task jgfs_awips_f018
          trigger ../../prdgen/jgfs_pgrb2_f18 == complete
        task jgfs_awips_f021
          trigger ../../prdgen/jgfs_pgrb2_f21 == complete
        task jgfs_awips_f024
          trigger ../../prdgen/jgfs_pgrb2_f24 == complete
        task jgfs_awips_f027
          trigger ../../prdgen/jgfs_pgrb2_f27 == complete
        task jgfs_awips_f030
          trigger ../../prdgen/jgfs_pgrb2_f30 == complete
        task jgfs_awips_f033
          trigger ../../prdgen/jgfs_pgrb2_f33 == complete
        task jgfs_awips_f036
          trigger ../../prdgen/jgfs_pgrb2_f36 == complete
        task jgfs_awips_f039
          trigger ../../prdgen/jgfs_pgrb2_f39 == complete
        task jgfs_awips_f042
          trigger ../../prdgen/jgfs_pgrb2_f42 == complete
        task jgfs_awips_f045
          trigger ../../prdgen/jgfs_pgrb2_f45 == complete
        task jgfs_awips_f048
          trigger ../../prdgen/jgfs_pgrb2_f48 == complete
        task jgfs_awips_f051
          trigger ../../prdgen/jgfs_pgrb2_f51 == complete
        task jgfs_awips_f054
          trigger ../../prdgen/jgfs_pgrb2_f54 == complete
        task jgfs_awips_f057
          trigger ../../prdgen/jgfs_pgrb2_f57 == complete
        task jgfs_awips_f060
          trigger ../../prdgen/jgfs_pgrb2_f60 == complete
        task jgfs_awips_f063
          trigger ../../prdgen/jgfs_pgrb2_f63 == complete
        task jgfs_awips_f066
          trigger ../../prdgen/jgfs_pgrb2_f66 == complete
        task jgfs_awips_f069
          trigger ../../prdgen/jgfs_pgrb2_f69 == complete
        task jgfs_awips_f072
          trigger ../../prdgen/jgfs_pgrb2_f72 == complete
        task jgfs_awips_f075
          trigger ../../prdgen/jgfs_pgrb2_f75 == complete
        task jgfs_awips_f078
          trigger ../../prdgen/jgfs_pgrb2_f78 == complete
        task jgfs_awips_f081
          trigger ../../prdgen/jgfs_pgrb2_f81 == complete
        task jgfs_awips_f084
          trigger ../../prdgen/jgfs_pgrb2_f84 == complete
        task jgfs_awips_f090
          trigger ../../prdgen/jgfs_pgrb2_f90 == complete
        task jgfs_awips_f096
          trigger ../../prdgen/jgfs_pgrb2_f96 == complete
        task jgfs_awips_f102
          trigger ../../prdgen/jgfs_pgrb2_f102 == complete
        task jgfs_awips_f108
          trigger ../../prdgen/jgfs_pgrb2_f108 == complete
        task jgfs_awips_f114
          trigger ../../prdgen/jgfs_pgrb2_f114 == complete
        task jgfs_awips_f120
          trigger ../../prdgen/jgfs_pgrb2_f120 == complete
        task jgfs_awips_f126
          trigger ../../prdgen/jgfs_pgrb2_f126 == complete
        task jgfs_awips_f132
          trigger ../../prdgen/jgfs_pgrb2_f132 == complete
        task jgfs_awips_f138
          trigger ../../prdgen/jgfs_pgrb2_f138 == complete
        task jgfs_awips_f144
          trigger ../../prdgen/jgfs_pgrb2_f144 == complete
        task jgfs_awips_f150
          trigger ../../prdgen/jgfs_pgrb2_f150 == complete
        task jgfs_awips_f156
          trigger ../../prdgen/jgfs_pgrb2_f156 == complete
        task jgfs_awips_f162
          trigger ../../prdgen/jgfs_pgrb2_f162 == complete
        task jgfs_awips_f168
          trigger ../../prdgen/jgfs_pgrb2_f168 == complete
        task jgfs_awips_f174
          trigger ../../prdgen/jgfs_pgrb2_f174 == complete
        task jgfs_awips_f180
          trigger ../../prdgen/jgfs_pgrb2_f180 == complete
        task jgfs_awips_f186
          trigger ../../prdgen/jgfs_pgrb2_f186 == complete
        task jgfs_awips_f192
          trigger ../../prdgen/jgfs_pgrb2_f192 == complete
        task jgfs_awips_f198
          trigger ../../prdgen/jgfs_pgrb2_f198 == complete
        task jgfs_awips_f204
          trigger ../../prdgen/jgfs_pgrb2_f204 == complete
        task jgfs_awips_f210
          trigger ../../prdgen/jgfs_pgrb2_f210 == complete
        task jgfs_awips_f216
          trigger ../../prdgen/jgfs_pgrb2_f216 == complete
        task jgfs_awips_f222
          trigger ../../prdgen/jgfs_pgrb2_f222 == complete
        task jgfs_awips_f228
          trigger ../../prdgen/jgfs_pgrb2_f228 == complete
        task jgfs_awips_f234
          trigger ../../prdgen/jgfs_pgrb2_f234 == complete
        task jgfs_awips_f240
          trigger ../../prdgen/jgfs_pgrb2_f240 == complete
      endfamily
    endfamily
    family prdgen
      task jgfs_npoess_pgrb2_0p5deg
        trigger ../post/jgfs_post_anl eq active
      task jgfs_pgrb2_manager
        trigger ../post/jgfs_post_anl == active or ../post/jgfs_post_anl == complete
        event 2 release_pgrb2_00
        event 3 release_pgrb2_01
        event 4 release_pgrb2_02
        event 5 release_pgrb2_03
        event 6 release_pgrb2_04
        event 7 release_pgrb2_05
        event 8 release_pgrb2_06
        event 9 release_pgrb2_07
        event 10 release_pgrb2_08
        event 11 release_pgrb2_09
        event 12 release_pgrb2_10
        event 13 release_pgrb2_11
        event 14 release_pgrb2_12
        event 15 release_pgrb2_15
        event 16 release_pgrb2_18
        event 17 release_pgrb2_21
        event 18 release_pgrb2_24
        event 19 release_pgrb2_27
        event 20 release_pgrb2_30
        event 21 release_pgrb2_33
        event 22 release_pgrb2_36
        event 23 release_pgrb2_39
        event 24 release_pgrb2_42
        event 25 release_pgrb2_45
        event 26 release_pgrb2_48
        event 27 release_pgrb2_51
        event 28 release_pgrb2_54
        event 29 release_pgrb2_57
        event 30 release_pgrb2_60
        event 31 release_pgrb2_63
        event 32 release_pgrb2_66
        event 33 release_pgrb2_69
        event 34 release_pgrb2_72
        event 35 release_pgrb2_75
        event 36 release_pgrb2_78
        event 37 release_pgrb2_81
        event 38 release_pgrb2_84
        event 39 release_pgrb2_87
        event 40 release_pgrb2_90
        event 41 release_pgrb2_93
        event 42 release_pgrb2_96
        event 43 release_pgrb2_99
        event 44 release_pgrb2_102
        event 45 release_pgrb2_105
        event 46 release_pgrb2_108
        event 47 release_pgrb2_111
        event 48 release_pgrb2_114
        event 49 release_pgrb2_117
        event 50 release_pgrb2_120
        event 51 release_pgrb2_123
        event 52 release_pgrb2_126
        event 53 release_pgrb2_129
        event 54 release_pgrb2_132
        event 55 release_pgrb2_135
        event 56 release_pgrb2_138
        event 57 release_pgrb2_141
        event 58 release_pgrb2_144
        event 59 release_pgrb2_147
        event 60 release_pgrb2_150
        event 61 release_pgrb2_153
        event 62 release_pgrb2_156
        event 63 release_pgrb2_159
        event 64 release_pgrb2_162
        event 65 release_pgrb2_165
        event 66 release_pgrb2_168
        event 67 release_pgrb2_171
        event 68 release_pgrb2_174
        event 69 release_pgrb2_177
        event 70 release_pgrb2_180
        event 71 release_pgrb2_183
        event 72 release_pgrb2_186
        event 73 release_pgrb2_189
        event 74 release_pgrb2_192
        event 75 release_pgrb2_195
        event 76 release_pgrb2_198
        event 77 release_pgrb2_201
        event 78 release_pgrb2_204
        event 79 release_pgrb2_207
        event 80 release_pgrb2_210
        event 81 release_pgrb2_213
        event 82 release_pgrb2_216
        event 83 release_pgrb2_219
        event 84 release_pgrb2_222
        event 85 release_pgrb2_225
        event 86 release_pgrb2_228
        event 87 release_pgrb2_231
        event 88 release_pgrb2_234
        event 89 release_pgrb2_237
        event 90 release_pgrb2_240
        event 91 release_pgrb2_252
        event 92 release_pgrb2_264
        event 93 release_pgrb2_276
        event 94 release_pgrb2_288
        event 95 release_pgrb2_300
        event 96 release_pgrb2_312
        event 97 release_pgrb2_324
        event 98 release_pgrb2_336
        event 99 release_pgrb2_348
        event 100 release_pgrb2_360
        event 101 release_pgrb2_372
        event 102 release_pgrb2_384
      task jgfs_pgrb2_anl
        trigger ../post/jgfs_post_anl:release_pgrb2_anl
      task jgfs_pgrb2_f00
        trigger jgfs_pgrb2_manager:release_pgrb2_00
      task jgfs_pgrb2_f03
        trigger jgfs_pgrb2_manager:release_pgrb2_03
      task jgfs_pgrb2_f06
        trigger jgfs_pgrb2_manager:release_pgrb2_06
      task jgfs_pgrb2_f09
        trigger jgfs_pgrb2_manager:release_pgrb2_09
      task jgfs_pgrb2_f12
        trigger jgfs_pgrb2_manager:release_pgrb2_12
      task jgfs_pgrb2_f15
        trigger jgfs_pgrb2_manager:release_pgrb2_15
      task jgfs_pgrb2_f18
        trigger jgfs_pgrb2_manager:release_pgrb2_18
      task jgfs_pgrb2_f21
        trigger jgfs_pgrb2_manager:release_pgrb2_21
      task jgfs_pgrb2_f24
        trigger jgfs_pgrb2_manager:release_pgrb2_24
      task jgfs_pgrb2_f27
        trigger jgfs_pgrb2_manager:release_pgrb2_27
      task jgfs_pgrb2_f30
        trigger jgfs_pgrb2_manager:release_pgrb2_30
      task jgfs_pgrb2_f33
        trigger jgfs_pgrb2_manager:release_pgrb2_33
      task jgfs_pgrb2_f36
        trigger jgfs_pgrb2_manager:release_pgrb2_36
      task jgfs_pgrb2_f39
        trigger jgfs_pgrb2_manager:release_pgrb2_39
      task jgfs_pgrb2_f42
        trigger jgfs_pgrb2_manager:release_pgrb2_42
      task jgfs_pgrb2_f45
        trigger jgfs_pgrb2_manager:release_pgrb2_45
      task jgfs_pgrb2_f48
        trigger jgfs_pgrb2_manager:release_pgrb2_48
      task jgfs_pgrb2_f51
        trigger jgfs_pgrb2_manager:release_pgrb2_51
      task jgfs_pgrb2_f54
        trigger jgfs_pgrb2_manager:release_pgrb2_54
      task jgfs_pgrb2_f57
        trigger jgfs_pgrb2_manager:release_pgrb2_57
      task jgfs_pgrb2_f60
        trigger jgfs_pgrb2_manager:release_pgrb2_60
      task jgfs_pgrb2_f63
        trigger jgfs_pgrb2_manager:release_pgrb2_63
      task jgfs_pgrb2_f66
        trigger jgfs_pgrb2_manager:release_pgrb2_66
      task jgfs_pgrb2_f69
        trigger jgfs_pgrb2_manager:release_pgrb2_69
      task jgfs_pgrb2_f72
        trigger jgfs_pgrb2_manager:release_pgrb2_72
      task jgfs_pgrb2_f75
        trigger jgfs_pgrb2_manager:release_pgrb2_75
      task jgfs_pgrb2_f78
        trigger jgfs_pgrb2_manager:release_pgrb2_78
      task jgfs_pgrb2_f81
        trigger jgfs_pgrb2_manager:release_pgrb2_81
      task jgfs_pgrb2_f84
        trigger jgfs_pgrb2_manager:release_pgrb2_84
      task jgfs_pgrb2_f87
        trigger jgfs_pgrb2_manager:release_pgrb2_87
      task jgfs_pgrb2_f90
        trigger jgfs_pgrb2_manager:release_pgrb2_90
      task jgfs_pgrb2_f93
        trigger jgfs_pgrb2_manager:release_pgrb2_93
      task jgfs_pgrb2_f96
        trigger jgfs_pgrb2_manager:release_pgrb2_96
      task jgfs_pgrb2_f99
        trigger jgfs_pgrb2_manager:release_pgrb2_99
      task jgfs_pgrb2_f102
        trigger jgfs_pgrb2_manager:release_pgrb2_102
      task jgfs_pgrb2_f105
        trigger jgfs_pgrb2_manager:release_pgrb2_105
      task jgfs_pgrb2_f108
        trigger jgfs_pgrb2_manager:release_pgrb2_108
      task jgfs_pgrb2_f111
        trigger jgfs_pgrb2_manager:release_pgrb2_111
      task jgfs_pgrb2_f114
        trigger jgfs_pgrb2_manager:release_pgrb2_114
      task jgfs_pgrb2_f117
        trigger jgfs_pgrb2_manager:release_pgrb2_117
      task jgfs_pgrb2_f120
        trigger jgfs_pgrb2_manager:release_pgrb2_120
      task jgfs_pgrb2_f123
        trigger jgfs_pgrb2_manager:release_pgrb2_123
      task jgfs_pgrb2_f126
        trigger jgfs_pgrb2_manager:release_pgrb2_126
      task jgfs_pgrb2_f129
        trigger jgfs_pgrb2_manager:release_pgrb2_129
      task jgfs_pgrb2_f132
        trigger jgfs_pgrb2_manager:release_pgrb2_132
      task jgfs_pgrb2_f135
        trigger jgfs_pgrb2_manager:release_pgrb2_135
      task jgfs_pgrb2_f138
        trigger jgfs_pgrb2_manager:release_pgrb2_138
      task jgfs_pgrb2_f141
        trigger jgfs_pgrb2_manager:release_pgrb2_141
      task jgfs_pgrb2_f144
        trigger jgfs_pgrb2_manager:release_pgrb2_144
      task jgfs_pgrb2_f147
        trigger jgfs_pgrb2_manager:release_pgrb2_147
      task jgfs_pgrb2_f150
        trigger jgfs_pgrb2_manager:release_pgrb2_150
      task jgfs_pgrb2_f153
        trigger jgfs_pgrb2_manager:release_pgrb2_153
      task jgfs_pgrb2_f156
        trigger jgfs_pgrb2_manager:release_pgrb2_156
      task jgfs_pgrb2_f159
        trigger jgfs_pgrb2_manager:release_pgrb2_159
      task jgfs_pgrb2_f162
        trigger jgfs_pgrb2_manager:release_pgrb2_162
      task jgfs_pgrb2_f165
        trigger jgfs_pgrb2_manager:release_pgrb2_165
      task jgfs_pgrb2_f168
        trigger jgfs_pgrb2_manager:release_pgrb2_168
      task jgfs_pgrb2_f171
        trigger jgfs_pgrb2_manager:release_pgrb2_171
      task jgfs_pgrb2_f174
        trigger jgfs_pgrb2_manager:release_pgrb2_174
      task jgfs_pgrb2_f177
        trigger jgfs_pgrb2_manager:release_pgrb2_177
      task jgfs_pgrb2_f180
        trigger jgfs_pgrb2_manager:release_pgrb2_180
      task jgfs_pgrb2_f183
        trigger jgfs_pgrb2_manager:release_pgrb2_183
      task jgfs_pgrb2_f186
        trigger jgfs_pgrb2_manager:release_pgrb2_186
      task jgfs_pgrb2_f189
        trigger jgfs_pgrb2_manager:release_pgrb2_189
      task jgfs_pgrb2_f192
        trigger jgfs_pgrb2_manager:release_pgrb2_192
      task jgfs_pgrb2_f195
        trigger jgfs_pgrb2_manager:release_pgrb2_195
      task jgfs_pgrb2_f198
        trigger jgfs_pgrb2_manager:release_pgrb2_198
      task jgfs_pgrb2_f201
        trigger jgfs_pgrb2_manager:release_pgrb2_201
      task jgfs_pgrb2_f204
        trigger jgfs_pgrb2_manager:release_pgrb2_204
      task jgfs_pgrb2_f207
        trigger jgfs_pgrb2_manager:release_pgrb2_207
      task jgfs_pgrb2_f210
        trigger jgfs_pgrb2_manager:release_pgrb2_210
      task jgfs_pgrb2_f213
        trigger jgfs_pgrb2_manager:release_pgrb2_213
      task jgfs_pgrb2_f216
        trigger jgfs_pgrb2_manager:release_pgrb2_216
      task jgfs_pgrb2_f219
        trigger jgfs_pgrb2_manager:release_pgrb2_219
      task jgfs_pgrb2_f222
        trigger jgfs_pgrb2_manager:release_pgrb2_222
      task jgfs_pgrb2_f225
        trigger jgfs_pgrb2_manager:release_pgrb2_225
      task jgfs_pgrb2_f228
        trigger jgfs_pgrb2_manager:release_pgrb2_228
      task jgfs_pgrb2_f231
        trigger jgfs_pgrb2_manager:release_pgrb2_231
      task jgfs_pgrb2_f234
        trigger jgfs_pgrb2_manager:release_pgrb2_234
      task jgfs_pgrb2_f237
        trigger jgfs_pgrb2_manager:release_pgrb2_237
      task jgfs_pgrb2_f240
        trigger jgfs_pgrb2_manager:release_pgrb2_240
      task jgfs_pgrb2_f252
        trigger jgfs_pgrb2_manager:release_pgrb2_252
      task jgfs_pgrb2_f264
        trigger jgfs_pgrb2_manager:release_pgrb2_264
      task jgfs_pgrb2_f276
        trigger jgfs_pgrb2_manager:release_pgrb2_276
      task jgfs_pgrb2_f288
        trigger jgfs_pgrb2_manager:release_pgrb2_288
      task jgfs_pgrb2_f300
        trigger jgfs_pgrb2_manager:release_pgrb2_300
      task jgfs_pgrb2_f312
        trigger jgfs_pgrb2_manager:release_pgrb2_312
      task jgfs_pgrb2_f324
        trigger jgfs_pgrb2_manager:release_pgrb2_324
      task jgfs_pgrb2_f336
        trigger jgfs_pgrb2_manager:release_pgrb2_336
      task jgfs_pgrb2_f348
        trigger jgfs_pgrb2_manager:release_pgrb2_348
      task jgfs_pgrb2_f360
        trigger jgfs_pgrb2_manager:release_pgrb2_360
      task jgfs_pgrb2_f372
        trigger jgfs_pgrb2_manager:release_pgrb2_372
      task jgfs_pgrb2_f384
        trigger jgfs_pgrb2_manager:release_pgrb2_384
    endfamily
    family post
      task jgfs_post_manager
        trigger ../jgfs_analysis eq complete
        edit ECF_PASS 'FREE'
        event 1 release_postanl
        event 2 release_post00
        event 3 release_post01
        event 4 release_post02
        event 5 release_post03
        event 6 release_post04
        event 7 release_post05
        event 8 release_post06
        event 9 release_post07
        event 10 release_post08
        event 11 release_post09
        event 12 release_post10
        event 13 release_post11
        event 14 release_post12
        event 15 release_post15
        event 16 release_post18
        event 17 release_post21
        event 18 release_post24
        event 19 release_post27
        event 20 release_post30
        event 21 release_post33
        event 22 release_post36
        event 23 release_post39
        event 24 release_post42
        event 25 release_post45
        event 26 release_post48
        event 27 release_post51
        event 28 release_post54
        event 29 release_post57
        event 30 release_post60
        event 31 release_post63
        event 32 release_post66
        event 33 release_post69
        event 34 release_post72
        event 35 release_post75
        event 36 release_post78
        event 37 release_post81
        event 38 release_post84
        event 39 release_post87
        event 40 release_post90
        event 41 release_post93
        event 42 release_post96
        event 43 release_post99
        event 44 release_post102
        event 45 release_post105
        event 46 release_post108
        event 47 release_post111
        event 48 release_post114
        event 49 release_post117
        event 50 release_post120
        event 51 release_post123
        event 52 release_post126
        event 53 release_post129
        event 54 release_post132
        event 55 release_post135
        event 56 release_post138
        event 57 release_post141
        event 58 release_post144
        event 59 release_post147
        event 60 release_post150
        event 61 release_post153
        event 62 release_post156
        event 63 release_post159
        event 64 release_post162
        event 65 release_post165
        event 66 release_post168
        event 67 release_post171
        event 68 release_post174
        event 69 release_post177
        event 70 release_post180
        event 71 release_post183
        event 72 release_post186
        event 73 release_post189
        event 74 release_post192
        event 75 release_post195
        event 76 release_post198
        event 77 release_post201
        event 78 release_post204
        event 79 release_post207
        event 80 release_post210
        event 81 release_post213
        event 82 release_post216
        event 83 release_post219
        event 84 release_post222
        event 85 release_post225
        event 86 release_post228
        event 87 release_post231
        event 88 release_post234
        event 89 release_post237
        event 90 release_post240
        event 91 release_post252
        event 92 release_post264
        event 93 release_post276
        event 94 release_post288
        event 95 release_post300
        event 96 release_post312
        event 97 release_post324
        event 98 release_post336
        event 99 release_post348
        event 100 release_post360
        event 101 release_post372
        event 102 release_post384
      task jgfs_post_anl
        trigger jgfs_post_manager:release_postanl
        event 1 release_pgrb2_anl
      task jgfs_post_f00
        trigger jgfs_post_manager:release_post00
      task jgfs_post_f01
        trigger jgfs_post_manager:release_post01
      task jgfs_post_f02
        trigger jgfs_post_manager:release_post02
      task jgfs_post_f03
        trigger jgfs_post_manager:release_post03
      task jgfs_post_f04
        trigger jgfs_post_manager:release_post04
      task jgfs_post_f05
        trigger jgfs_post_manager:release_post05
      task jgfs_post_f06
        trigger jgfs_post_manager:release_post06
      task jgfs_post_f07
        trigger jgfs_post_manager:release_post07
      task jgfs_post_f08
        trigger jgfs_post_manager:release_post08
      task jgfs_post_f09
        trigger jgfs_post_manager:release_post09
      task jgfs_post_f10
        trigger jgfs_post_manager:release_post10
      task jgfs_post_f11
        trigger jgfs_post_manager:release_post11
      task jgfs_post_f12
        trigger jgfs_post_manager:release_post12
      task jgfs_post_f15
        trigger jgfs_post_manager:release_post15
      task jgfs_post_f18
        trigger jgfs_post_manager:release_post18
      task jgfs_post_f21
        trigger jgfs_post_manager:release_post21
      task jgfs_post_f24
        trigger jgfs_post_manager:release_post24
      task jgfs_post_f27
        trigger jgfs_post_manager:release_post27
      task jgfs_post_f30
        trigger jgfs_post_manager:release_post30
      task jgfs_post_f33
        trigger jgfs_post_manager:release_post33
      task jgfs_post_f36
        trigger jgfs_post_manager:release_post36
      task jgfs_post_f39
        trigger jgfs_post_manager:release_post39
      task jgfs_post_f42
        trigger jgfs_post_manager:release_post42
      task jgfs_post_f45
        trigger jgfs_post_manager:release_post45
      task jgfs_post_f48
        trigger jgfs_post_manager:release_post48
      task jgfs_post_f51
        trigger jgfs_post_manager:release_post51
      task jgfs_post_f54
        trigger jgfs_post_manager:release_post54
      task jgfs_post_f57
        trigger jgfs_post_manager:release_post57
      task jgfs_post_f60
        trigger jgfs_post_manager:release_post60
      task jgfs_post_f63
        trigger jgfs_post_manager:release_post63
      task jgfs_post_f66
        trigger jgfs_post_manager:release_post66
      task jgfs_post_f69
        trigger jgfs_post_manager:release_post69
      task jgfs_post_f72
        trigger jgfs_post_manager:release_post72
      task jgfs_post_f75
        trigger jgfs_post_manager:release_post75
      task jgfs_post_f78
        trigger jgfs_post_manager:release_post78
      task jgfs_post_f81
        trigger jgfs_post_manager:release_post81
      task jgfs_post_f84
        trigger jgfs_post_manager:release_post84
      task jgfs_post_f87
        trigger jgfs_post_manager:release_post87
      task jgfs_post_f90
        trigger jgfs_post_manager:release_post90
      task jgfs_post_f93
        trigger jgfs_post_manager:release_post93
      task jgfs_post_f96
        trigger jgfs_post_manager:release_post96
      task jgfs_post_f99
        trigger jgfs_post_manager:release_post99
      task jgfs_post_f102
        trigger jgfs_post_manager:release_post102
      task jgfs_post_f105
        trigger jgfs_post_manager:release_post105
      task jgfs_post_f108
        trigger jgfs_post_manager:release_post108
      task jgfs_post_f111
        trigger jgfs_post_manager:release_post111
      task jgfs_post_f114
        trigger jgfs_post_manager:release_post114
      task jgfs_post_f117
        trigger jgfs_post_manager:release_post117
      task jgfs_post_f120
        trigger jgfs_post_manager:release_post120
      task jgfs_post_f123
        trigger jgfs_post_manager:release_post123
      task jgfs_post_f126
        trigger jgfs_post_manager:release_post126
      task jgfs_post_f129
        trigger jgfs_post_manager:release_post129
      task jgfs_post_f132
        trigger jgfs_post_manager:release_post132
      task jgfs_post_f135
        trigger jgfs_post_manager:release_post135
      task jgfs_post_f138
        trigger jgfs_post_manager:release_post138
      task jgfs_post_f141
        trigger jgfs_post_manager:release_post141
      task jgfs_post_f144
        trigger jgfs_post_manager:release_post144
      task jgfs_post_f147
        trigger jgfs_post_manager:release_post147
      task jgfs_post_f150
        trigger jgfs_post_manager:release_post150
      task jgfs_post_f153
        trigger jgfs_post_manager:release_post153
      task jgfs_post_f156
        trigger jgfs_post_manager:release_post156
      task jgfs_post_f159
        trigger jgfs_post_manager:release_post159
      task jgfs_post_f162
        trigger jgfs_post_manager:release_post162
      task jgfs_post_f165
        trigger jgfs_post_manager:release_post165
      task jgfs_post_f168
        trigger jgfs_post_manager:release_post168
      task jgfs_post_f171
        trigger jgfs_post_manager:release_post171
      task jgfs_post_f174
        trigger jgfs_post_manager:release_post174
      task jgfs_post_f177
        trigger jgfs_post_manager:release_post177
      task jgfs_post_f180
        trigger jgfs_post_manager:release_post180
      task jgfs_post_f183
        trigger jgfs_post_manager:release_post183
      task jgfs_post_f186
        trigger jgfs_post_manager:release_post186
      task jgfs_post_f189
        trigger jgfs_post_manager:release_post189
      task jgfs_post_f192
        trigger jgfs_post_manager:release_post192
      task jgfs_post_f195
        trigger jgfs_post_manager:release_post195
      task jgfs_post_f198
        trigger jgfs_post_manager:release_post198
      task jgfs_post_f201
        trigger jgfs_post_manager:release_post201
      task jgfs_post_f204
        trigger jgfs_post_manager:release_post204
      task jgfs_post_f207
        trigger jgfs_post_manager:release_post207
      task jgfs_post_f210
        trigger jgfs_post_manager:release_post210
      task jgfs_post_f213
        trigger jgfs_post_manager:release_post213
      task jgfs_post_f216
        trigger jgfs_post_manager:release_post216
      task jgfs_post_f219
        trigger jgfs_post_manager:release_post219
      task jgfs_post_f222
        trigger jgfs_post_manager:release_post222
      task jgfs_post_f225
        trigger jgfs_post_manager:release_post225
      task jgfs_post_f228
        trigger jgfs_post_manager:release_post228
      task jgfs_post_f231
        trigger jgfs_post_manager:release_post231
      task jgfs_post_f234
        trigger jgfs_post_manager:release_post234
      task jgfs_post_f237
        trigger jgfs_post_manager:release_post237
      task jgfs_post_f240
        trigger jgfs_post_manager:release_post240
      task jgfs_post_f252
        trigger jgfs_post_manager:release_post252
      task jgfs_post_f264
        trigger jgfs_post_manager:release_post264
      task jgfs_post_f276
        trigger jgfs_post_manager:release_post276
      task jgfs_post_f288
        trigger jgfs_post_manager:release_post288
      task jgfs_post_f300
        trigger jgfs_post_manager:release_post300
      task jgfs_post_f312
        trigger jgfs_post_manager:release_post312
      task jgfs_post_f324
        trigger jgfs_post_manager:release_post324
      task jgfs_post_f336
        trigger jgfs_post_manager:release_post336
      task jgfs_post_f348
        trigger jgfs_post_manager:release_post348
      task jgfs_post_f360
        trigger jgfs_post_manager:release_post360
      task jgfs_post_f372
        trigger jgfs_post_manager:release_post372
      task jgfs_post_f384
        trigger jgfs_post_manager:release_post384
      task jgfs_pgrb2_spec_post
        trigger jgfs_post_f336 eq complete and jgfs_post_f348 eq complete and jgfs_post_f360 eq complete and jgfs_post_f372 eq complete and jgfs_post_f384 eq complete
    endfamily
    family mos
      edit COM 'com2'
      edit QUEUE 'prod2'
      edit QUEUESERV 'prod2_serv'
      family prep
        task jgfsmos_prep
          trigger /prod12/gfs/prdgen/jgfs_pgrb2_f96 == complete
        task jgfsmos_prep47
          trigger /prod12/gfs/prdgen/jgfs_pgrb2_f96 == complete
        task jgfsmos_hrqpf_preprep
          trigger /prod12/gfs/prdgen/jgfs_pgrb2_f105 == complete
        task jgfsmos_hrqpf_prep
          trigger jgfsmos_hrqpf_preprep == complete
        task jgfsmos_hrqpf_extpreprep
          trigger /prod12/gfs/prdgen/jgfs_pgrb2_f216 == complete
        task jgfsmos_hrqpf_extprep
          trigger jgfsmos_hrqpf_preprep == complete and jgfsmos_hrqpf_extpreprep == complete
        task jgfsmos_extprep
          trigger ../prdgen/jgfsmos_station_prdgen == complete and /prod12/gfs/prdgen == complete
        task jgfsmos_extprep47
          trigger ../prdgen/jgfsmos_station_prdgen == complete and /prod12/gfs/prdgen == complete
      endfamily
      family forecast
        task jgfsmos_akgoe_fcst
          trigger ../prep/jgfsmos_prep47 == complete
        task jgfsmos_cooprfcmeso_fcst
          trigger ../prep/jgfsmos_prep47 == complete
        task jgfsmos_metar_fcst
          trigger ../prep/jgfsmos_prep47 == complete
        task jgfsmos_pac_fcst
          trigger ../prep/jgfsmos_prep47 == complete
        task jgfsmos_tstms_fcst
          trigger ../prep/jgfsmos_prep47 == complete
        task jgfsmos_goe_fcst
          trigger ../prep/jgfsmos_prep47 == complete
        task jgfsmos_higoe_fcst
          trigger /prod12/gfs/prdgen/jgfs_pgrb2_f96 == complete
        task jgfsmos_cotsvr_wxfcst
          trigger ../prep/jgfsmos_prep47 == complete and ../forecast/jgfsmos_tstms_fcst == complete
        task jgfsmos_cotstm_wxfcst
          trigger ../prep/jgfsmos_prep47 == complete and ../forecast/jgfsmos_tstms_fcst == complete
        task jgfsmos_hrqpf_fcst
          trigger ../prep/jgfsmos_hrqpf_prep == complete
        task jgfsmos_hrqpf_extfcst
          trigger ../prep/jgfsmos_hrqpf_extprep == complete
        task jgfsmos_higoe_extfcst
          trigger /prod12/gfs/prdgen == complete and jgfsmos_higoe_fcst == complete
        task jgfsmos_pac_extfcst
          trigger ../prep/jgfsmos_extprep47 == complete
        task jgfsmos_tstms_extfcst
          trigger ../prep/jgfsmos_extprep47 == complete
        task jgfsmos_cooprfcmeso_extfcst
          trigger ../prep/jgfsmos_extprep47 == complete
        task jgfsmos_metar_extfcst
          trigger ../prep/jgfsmos_extprep47 == complete
        task jgfsmos_akgoe_extfcst
          trigger ../prep/jgfsmos_extprep47 == complete
        task jgfsmos_goe_extfcst
          trigger ../prep/jgfsmos_extprep47 == complete
        task jgfsmos_cotstm_ext_wxfcst
          trigger ../prep/jgfsmos_extprep47 == complete and ../forecast/jgfsmos_tstms_extfcst == complete
      endfamily
      family gempak
        task jgfs_gempak_mdl
          trigger ../prdgen/jgfsmos_station_prdgen eq complete
      endfamily
      family post
        task jgfsmos_hrqpf_post
          trigger ../forecast/jgfsmos_hrqpf_fcst == active
        task jgfsmos_hrqpf_extpost
          trigger ../forecast/jgfsmos_hrqpf_extfcst == active
      endfamily
      family prdgen
        task jgfsmos_station_prdgen
          trigger jgfsmos_gridded_prdgen == active and jgfsmos_akgridded_prdgen == active
        task jgfsmos_akpopo3_prdgen
          trigger ../prep/jgfsmos_prep47 == complete
        task jgfsmos_akptype_prdgen
          trigger ../prep/jgfsmos_prep47 == complete
        task jgfsmos_copopo3_prdgen
          trigger ../prep/jgfsmos_prep47 == complete
        task jgfsmos_coptype_prdgen
          trigger ../prep/jgfsmos_prep47 == complete
        task jgfsmos_higridded_prdgen
          trigger ../forecast/jgfsmos_higoe_fcst == complete and ../forecast/jgfsmos_metar_fcst == complete and ../forecast/jgfsmos_cooprfcmeso_fcst == complete and ../forecast/jgfsmos_tstms_fcst == complete
        task jgfsmos_akgridded_prdgen
          trigger ../forecast/jgfsmos_metar_fcst == complete and ../forecast/jgfsmos_cooprfcmeso_fcst == complete and ../forecast/jgfsmos_goe_fcst == complete and ../forecast/jgfsmos_akgoe_fcst == complete and ../forecast/jgfsmos_tstms_fcst == complete
        task jgfsmos_gridded_prdgen
          trigger ../forecast/jgfsmos_metar_fcst == complete and ../forecast/jgfsmos_cooprfcmeso_fcst == complete and ../forecast/jgfsmos_goe_fcst == complete and ../forecast/jgfsmos_pac_fcst == complete and ../forecast/jgfsmos_tstms_fcst == complete
        task jgfsmos_cogridded_prdgen
          trigger ../forecast/jgfsmos_goe_fcst == complete and ../forecast/jgfsmos_metar_fcst == complete and ../forecast/jgfsmos_cooprfcmeso_fcst == complete and ../forecast/jgfsmos_tstms_fcst == complete and jgfsmos_station_prdgen == complete
        task jgfsmos_hrqpf_prdgen
          trigger ../post/jgfsmos_hrqpf_post == complete
        task jgfsmos_hrqpf_extprdgen
          trigger ../post/jgfsmos_hrqpf_extpost == complete and jgfsmos_hrqpf_prdgen == complete
        task jgfsmos_akpopo3_extprdgen
          trigger ../prep/jgfsmos_extprep47 == complete
        task jgfsmos_akptype_extprdgen
          trigger ../prep/jgfsmos_extprep47 == complete
        task jgfsmos_station_extprdgen
          trigger jgfsmos_gridded_extprdgen == active and jgfsmos_akgridded_extprdgen == active
        task jgfsmos_copopo3_extprdgen
          trigger ../prep/jgfsmos_extprep47 == complete
        task jgfsmos_coptype_extprdgen
          trigger ../prep/jgfsmos_extprep47 == complete
        task jgfsmos_higridded_extprdgen
          trigger ../forecast/jgfsmos_higoe_extfcst == complete and jgfsmos_higridded_prdgen == complete and ../forecast/jgfsmos_cooprfcmeso_extfcst == complete and ../forecast/jgfsmos_metar_extfcst == complete and ../forecast/jgfsmos_tstms_extfcst == complete
        task jgfsmos_gridded_extprdgen
          trigger ../forecast/jgfsmos_metar_extfcst == complete and ../forecast/jgfsmos_pac_extfcst == complete and ../forecast/jgfsmos_goe_extfcst == complete and ../forecast/jgfsmos_tstms_extfcst == complete and ../forecast/jgfsmos_cooprfcmeso_extfcst == complete
        task jgfsmos_akwx_extprdgen
          trigger jgfsmos_akpopo3_extprdgen == complete and jgfsmos_akptype_extprdgen == complete and jgfsmos_akgridded_extprdgen == complete
        task jgfsmos_akwxprdgen
          trigger jgfsmos_akpopo3_prdgen == complete and jgfsmos_akptype_prdgen == complete and jgfsmos_akgridded_extprdgen == complete
        task jgfsmos_cogridded_extprdgen
          trigger ../forecast/jgfsmos_goe_extfcst == complete and ../forecast/jgfsmos_metar_extfcst == complete and ../forecast/jgfsmos_cooprfcmeso_extfcst == complete and ../forecast/jgfsmos_tstms_extfcst == complete and jgfsmos_cogridded_prdgen == complete and jgfsmos_station_extprdgen == complete
        task jgfsmos_akgridded_extprdgen
          trigger ../forecast/jgfsmos_metar_extfcst == complete and ../forecast/jgfsmos_goe_extfcst == complete and ../forecast/jgfsmos_akgoe_extfcst == complete and ../forecast/jgfsmos_tstms_extfcst == complete and jgfsmos_akgridded_prdgen == complete and ../forecast/jgfsmos_cooprfcmeso_extfcst == complete
        task jgfsmos_cowxprdgen
          trigger jgfsmos_copopo3_prdgen == complete and jgfsmos_coptype_prdgen == complete and ../forecast/jgfsmos_cotsvr_wxfcst == complete and ../forecast/jgfsmos_cotstm_wxfcst == complete and jgfsmos_cogridded_extprdgen == complete
        task jgfsmos_cowx_extprdgen
          trigger jgfsmos_copopo3_extprdgen == complete and jgfsmos_coptype_extprdgen == complete and ../forecast/jgfsmos_cotsvr_wxfcst == complete and ../forecast/jgfsmos_cotstm_wxfcst == complete and jgfsmos_cogridded_extprdgen == complete
      endfamily
    endfamily
    family gempak
      task jgfs_gempak_upapgif
        trigger ../dump/jgfs_dump eq complete
      task jgfs_gempak_ncdc
        trigger jgfs_gempak eq active
      task jgfs_gempak
        trigger ../jgfs_analysis eq complete
      task jgfs_gempak_meta
        trigger ../jgfs_analysis eq complete
      task jgfs_pgrb2_spec_gempak
        trigger ../post/jgfs_pgrb2_spec_post eq complete
    endfamily
  endfamily
  family etss
    edit PROJ 'ETSS-OPS'
    edit QUEUE 'prod2'
    edit COM 'com2'
    edit ECF_FILES '/ecf/ecfnets/scripts/etss'
    task jetss
      trigger /prod12/gfs/prdgen/jgfs_pgrb2_f96 == complete
      event 1 alaska_done
    task jetss_gempak_alaska
      trigger jetss:alaska_done
    task jetss_gempak_conus
      trigger jetss == complete
    task jetss_parsedat
      trigger jetss == complete
    task jetss_griddat
      trigger jetss_parsedat == complete
    task jetss_combdat
      trigger jetss_griddat == complete
  endfamily
  family rcdas
    edit ECF_FILES '/ecf/ecfnets/scripts/rcdas'
    task jrcdas_obs_precip_12
      edit ECF_PASS 'FREE'
      time 16:50
    task jrcdas_prep_12
      trigger /prod12/cdas00/jcdas_forecast == complete and ./jrcdas_obs_precip_12 == complete
      edit ECF_PASS 'FREE'
    task jrcdas_assim_00
      trigger ./jrcdas_prep_12 == complete
      edit ECF_PASS 'FREE'
    task jrcdas_assim_12
      trigger ./jrcdas_prep_12 == complete and ./jrcdas_assim_00 == complete
      edit ECF_PASS 'FREE'
    task jrcdas_post_00
      trigger ./jrcdas_assim_00 == complete and ./jrcdas_assim_12 == complete
      edit ECF_PASS 'FREE'
    task jrcdas_post_12
      trigger ./jrcdas_assim_12 == complete
      edit ECF_PASS 'FREE'
  endfamily
  family lamp
    edit PROJ 'LAMP-OPS'
    edit ECF_FILES '/ecf/ecfnets/scripts/lamp'
    family lamp_12
      edit CYC '12'
      family prep
        task jlamp_prep
          trigger /prod06/gfs/mos/prdgen/jgfsmos_station_prdgen == complete
          time 12:27
        task jlamp_ltng_prep
          trigger /prod06/nam/mos/jnam_mos == complete and /prod12/lamp/lamp_12/prep/jlamp_prep == complete and /prod06/gfs/mos/forecast/jgfsmos_tstms_fcst == complete
        task jlamp_cnv_prep
          trigger /prod06/nam/mos/jnam_mos == complete and /prod12/lamp/lamp_12/prep/jlamp_prep == complete and /prod06/gfs/mos/forecast/jgfsmos_tstms_fcst == complete
      endfamily
      family fcst
        task jlamp_station_fcst
          trigger /prod12/lamp/lamp_12/prep/jlamp_prep == complete
        task jlamp_ltng_fcst
          trigger /prod12/lamp/lamp_12/prep/jlamp_ltng_prep == complete
        task jlamp_cnv_fcst
          trigger /prod12/lamp/lamp_12/prep/jlamp_cnv_prep == complete
      endfamily
      family prdgen
        task jlamp_ltng_prdgen
          trigger /prod12/lamp/lamp_12/fcst/jlamp_ltng_fcst == complete
        task jlamp_cnv_prdgen
          trigger /prod12/lamp/lamp_12/fcst/jlamp_cnv_fcst == complete
        task jlamp_prdgen
          trigger /prod12/lamp/lamp_12/fcst/jlamp_station_fcst == complete and /prod12/lamp/lamp_12/prdgen/jlamp_ltng_prdgen == complete and /prod12/lamp/lamp_12/prdgen/jlamp_cnv_prdgen == complete
      endfamily
    endfamily
    family lamp_13
      edit CYC '13'
      family prep
        task jlamp_prep
          trigger /prod06/gfs/mos/prdgen/jgfsmos_station_prdgen == complete
          time 13:27
        task jlamp_ltng_prep
          trigger /prod06/nam/mos/jnam_mos == complete and /prod12/lamp/lamp_13/prep/jlamp_prep == complete and /prod06/gfs/mos/forecast/jgfsmos_tstms_fcst == complete
        task jlamp_cnv_prep
          trigger /prod06/nam/mos/jnam_mos == complete and /prod12/lamp/lamp_13/prep/jlamp_prep == complete and /prod06/gfs/mos/forecast/jgfsmos_tstms_fcst == complete
      endfamily
      family fcst
        task jlamp_station_fcst
          trigger /prod12/lamp/lamp_13/prep/jlamp_prep == complete
        task jlamp_ltng_fcst
          trigger /prod12/lamp/lamp_13/prep/jlamp_ltng_prep == complete
        task jlamp_cnv_fcst
          trigger /prod12/lamp/lamp_13/prep/jlamp_cnv_prep == complete
      endfamily
      family prdgen
        task jlamp_ltng_prdgen
          trigger /prod12/lamp/lamp_13/fcst/jlamp_ltng_fcst == complete
        task jlamp_cnv_prdgen
          trigger /prod12/lamp/lamp_13/fcst/jlamp_cnv_fcst == complete
        task jlamp_prdgen
          trigger /prod12/lamp/lamp_13/fcst/jlamp_station_fcst == complete and /prod12/lamp/lamp_13/prdgen/jlamp_ltng_prdgen == complete and /prod12/lamp/lamp_13/prdgen/jlamp_cnv_prdgen == complete
      endfamily
    endfamily
    family lamp_14
      edit CYC '14'
      family prep
        task jlamp_prep
          trigger /prod06/gfs/mos/prdgen/jgfsmos_station_prdgen == complete
          time 14:27
        task jlamp_ltng_prep
          trigger /prod06/nam/mos/jnam_mos == complete and /prod12/lamp/lamp_14/prep/jlamp_prep == complete and /prod06/gfs/mos/forecast/jgfsmos_tstms_fcst == complete
        task jlamp_cnv_prep
          trigger /prod06/nam/mos/jnam_mos == complete and /prod12/lamp/lamp_14/prep/jlamp_prep == complete and /prod06/gfs/mos/forecast/jgfsmos_tstms_fcst == complete
      endfamily
      family fcst
        task jlamp_station_fcst
          trigger /prod12/lamp/lamp_14/prep/jlamp_prep == complete
        task jlamp_ltng_fcst
          trigger /prod12/lamp/lamp_14/prep/jlamp_ltng_prep == complete
        task jlamp_cnv_fcst
          trigger /prod12/lamp/lamp_14/prep/jlamp_cnv_prep == complete
      endfamily
      family prdgen
        task jlamp_ltng_prdgen
          trigger /prod12/lamp/lamp_14/fcst/jlamp_ltng_fcst == complete
        task jlamp_cnv_prdgen
          trigger /prod12/lamp/lamp_14/fcst/jlamp_cnv_fcst == complete
        task jlamp_prdgen
          trigger /prod12/lamp/lamp_14/fcst/jlamp_station_fcst == complete and /prod12/lamp/lamp_14/prdgen/jlamp_ltng_prdgen == complete and /prod12/lamp/lamp_14/prdgen/jlamp_cnv_prdgen == complete
      endfamily
    endfamily
    family lamp_15
      edit CYC '15'
      family prep
        task jlamp_prep
          trigger /prod06/gfs/mos/prdgen/jgfsmos_station_prdgen == complete
          time 15:27
        task jlamp_ltng_prep
          trigger /prod06/nam/mos/jnam_mos == complete and /prod12/lamp/lamp_15/prep/jlamp_prep == complete and /prod06/gfs/mos/forecast/jgfsmos_tstms_fcst == complete
        task jlamp_cnv_prep
          trigger /prod06/nam/mos/jnam_mos == complete and /prod12/lamp/lamp_15/prep/jlamp_prep == complete and /prod06/gfs/mos/forecast/jgfsmos_tstms_fcst == complete
      endfamily
      family fcst
        task jlamp_station_fcst
          trigger /prod12/lamp/lamp_15/prep/jlamp_prep == complete
        task jlamp_ltng_fcst
          trigger /prod12/lamp/lamp_15/prep/jlamp_ltng_prep == complete
        task jlamp_cnv_fcst
          trigger /prod12/lamp/lamp_15/prep/jlamp_cnv_prep == complete
      endfamily
      family prdgen
        task jlamp_ltng_prdgen
          trigger /prod12/lamp/lamp_15/fcst/jlamp_ltng_fcst == complete
        task jlamp_cnv_prdgen
          trigger /prod12/lamp/lamp_15/fcst/jlamp_cnv_fcst == complete
        task jlamp_prdgen
          trigger /prod12/lamp/lamp_15/fcst/jlamp_station_fcst == complete and /prod12/lamp/lamp_15/prdgen/jlamp_ltng_prdgen == complete and /prod12/lamp/lamp_15/prdgen/jlamp_cnv_prdgen == complete
      endfamily
    endfamily
    family lamp_16
      edit CYC '16'
      family prep
        task jlamp_prep
          trigger /prod12/gfs/mos/prdgen/jgfsmos_station_prdgen == complete
          time 16:27
        task jlamp_ltng_prep
          trigger /prod12/nam/mos/jnam_mos == complete and /prod12/lamp/lamp_16/prep/jlamp_prep == complete and /prod12/gfs/mos/forecast/jgfsmos_tstms_fcst == complete
        task jlamp_cnv_prep
          trigger /prod12/nam/mos/jnam_mos == complete and /prod12/lamp/lamp_16/prep/jlamp_prep == complete and /prod12/gfs/mos/forecast/jgfsmos_tstms_fcst == complete
      endfamily
      family fcst
        task jlamp_station_fcst
          trigger /prod12/lamp/lamp_16/prep/jlamp_prep == complete
        task jlamp_ltng_fcst
          trigger /prod12/lamp/lamp_16/prep/jlamp_ltng_prep == complete
        task jlamp_cnv_fcst
          trigger /prod12/lamp/lamp_16/prep/jlamp_cnv_prep == complete
      endfamily
      family prdgen
        task jlamp_ltng_prdgen
          trigger /prod12/lamp/lamp_16/fcst/jlamp_ltng_fcst == complete
        task jlamp_cnv_prdgen
          trigger /prod12/lamp/lamp_16/fcst/jlamp_cnv_fcst == complete
        task jlamp_prdgen
          trigger /prod12/lamp/lamp_16/fcst/jlamp_station_fcst == complete and /prod12/lamp/lamp_16/prdgen/jlamp_ltng_prdgen == complete and /prod12/lamp/lamp_16/prdgen/jlamp_cnv_prdgen == complete
      endfamily
    endfamily
    family lamp_17
      edit CYC '17'
      family prep
        task jlamp_prep
          trigger /prod12/gfs/mos/prdgen/jgfsmos_station_prdgen == complete
          time 17:27
        task jlamp_ltng_prep
          trigger /prod12/nam/mos/jnam_mos == complete and /prod12/lamp/lamp_17/prep/jlamp_prep == complete and /prod12/gfs/mos/forecast/jgfsmos_tstms_fcst == complete
        task jlamp_cnv_prep
          trigger /prod12/nam/mos/jnam_mos == complete and /prod12/lamp/lamp_17/prep/jlamp_prep == complete and /prod12/gfs/mos/forecast/jgfsmos_tstms_fcst == complete
      endfamily
      family fcst
        task jlamp_station_fcst
          trigger /prod12/lamp/lamp_17/prep/jlamp_prep == complete
        task jlamp_ltng_fcst
          trigger /prod12/lamp/lamp_17/prep/jlamp_ltng_prep == complete
        task jlamp_cnv_fcst
          trigger /prod12/lamp/lamp_17/prep/jlamp_cnv_prep == complete
      endfamily
      family prdgen
        task jlamp_ltng_prdgen
          trigger /prod12/lamp/lamp_17/fcst/jlamp_ltng_fcst == complete
        task jlamp_cnv_prdgen
          trigger /prod12/lamp/lamp_17/fcst/jlamp_cnv_fcst == complete
        task jlamp_prdgen
          trigger /prod12/lamp/lamp_17/fcst/jlamp_station_fcst == complete and /prod12/lamp/lamp_17/prdgen/jlamp_ltng_prdgen == complete and /prod12/lamp/lamp_17/prdgen/jlamp_cnv_prdgen == complete
      endfamily
    endfamily
  endfamily
  family cmc
    task jcmc_gempak
      edit PROJ 'HOURLY-OPS'
      time 17:19
  endfamily
  family estofs
    edit PROJ 'NMO-OPS'
    edit ECF_FILES '/ecf/ecfnets/scripts/estofs'
    family pac
      edit DOMAIN 'pac'
      task jestofs_prep
        time 15:30
      task jestofs_forecast
        trigger /prod12/estofs/pac/jestofs_prep == complete and /prod12/gfs/prdgen/jgfs_pgrb2_f180 == complete
      task jestofs_post
        trigger /prod12/estofs/pac/jestofs_forecast == complete
        event 4 grib2_ready
      task jestofs_gempak
        trigger /prod12/estofs/pac/jestofs_post:grib2_ready
    endfamily
    family atl
      task jestofs_prep
        time 15:30
      task jestofs_forecast
        trigger /prod12/estofs/atl/jestofs_prep == complete and /prod12/gfs/prdgen/jgfs_pgrb2_f180 == complete
      task jestofs_post
        trigger /prod12/estofs/atl/jestofs_forecast == complete
        event 4 grib2_ready
      task jestofs_gempak_pr
        trigger /prod12/estofs/atl/jestofs_post:grib2_ready
      task jestofs_gempak_conus
        trigger /prod12/estofs/atl/jestofs_post:grib2_ready
    endfamily
  endfamily
  family glamp
    edit ECF_FILES '/ecf/ecfnets/scripts/glamp'
    edit PROJ 'LAMP-OPS'
    edit QUEUE 'prod2'
    edit QUEUESERV 'prod2_serv'
    edit COM 'com2'
    family glamp_12
      edit CYC '12'
      family obs
        task jglamp_obs_prep
          time 12:26
        task jglamp_mos_prep
          time 12:26
        task jglamp_csk_obs_granalysis
          trigger jglamp_obs_prep == complete
        task jglamp_wsd_obs_granalysis
          trigger jglamp_obs_prep == complete and jglamp_mos_prep == complete
        task jglamp_vis_obs_granalysis
          trigger jglamp_obs_prep == complete
        task jglamp_tdp_obs_granalysis
          trigger jglamp_obs_prep == complete and jglamp_mos_prep == complete
        task jglamp_csk_obs_prdgen
          trigger jglamp_csk_obs_granalysis == complete
        task jglamp_wsd_obs_prdgen
          trigger jglamp_wsd_obs_granalysis == complete
        task jglamp_vis_obs_prdgen
          trigger jglamp_vis_obs_granalysis == complete
        task jglamp_tdp_obs_prdgen
          trigger jglamp_tdp_obs_granalysis == complete
      endfamily
      family fcst
        trigger /prod12/lamp/lamp_12/prdgen/jlamp_prdgen == complete
        task jglamp_tdp_fcst_granalysis
          trigger ../obs/jglamp_tdp_obs_granalysis == complete
        task jglamp_wsd_fcst_granalysis
          trigger ../obs/jglamp_wsd_obs_granalysis == complete
        task jglamp_vis_fcst_granalysis
          trigger ../obs/jglamp_obs_prep == complete
        task jglamp_csk_fcst_granalysis
          trigger ../obs/jglamp_obs_prep == complete
        task jglamp_tdp_fcst_prdgen
          trigger jglamp_tdp_fcst_granalysis == complete
        task jglamp_csk_fcst_prdgen
          trigger jglamp_csk_fcst_granalysis == complete
        task jglamp_vis_fcst_prdgen
          trigger jglamp_vis_fcst_granalysis == complete
        task jglamp_wsd_fcst_prdgen
          trigger jglamp_wsd_fcst_granalysis == complete
      endfamily
    endfamily
    family glamp_13
      edit CYC '13'
      family obs
        task jglamp_obs_prep
          time 13:26
        task jglamp_mos_prep
          time 13:26
        task jglamp_csk_obs_granalysis
          trigger jglamp_obs_prep == complete
        task jglamp_wsd_obs_granalysis
          trigger jglamp_obs_prep == complete and jglamp_mos_prep == complete
        task jglamp_vis_obs_granalysis
          trigger jglamp_obs_prep == complete
        task jglamp_tdp_obs_granalysis
          trigger jglamp_obs_prep == complete and jglamp_mos_prep == complete
        task jglamp_csk_obs_prdgen
          trigger jglamp_csk_obs_granalysis == complete
        task jglamp_wsd_obs_prdgen
          trigger jglamp_wsd_obs_granalysis == complete
        task jglamp_vis_obs_prdgen
          trigger jglamp_vis_obs_granalysis == complete
        task jglamp_tdp_obs_prdgen
          trigger jglamp_tdp_obs_granalysis == complete
      endfamily
      family fcst
        trigger /prod12/lamp/lamp_13/prdgen/jlamp_prdgen == complete
        task jglamp_tdp_fcst_granalysis
          trigger ../obs/jglamp_tdp_obs_granalysis == complete
        task jglamp_wsd_fcst_granalysis
          trigger ../obs/jglamp_wsd_obs_granalysis == complete
        task jglamp_vis_fcst_granalysis
          trigger ../obs/jglamp_obs_prep == complete
        task jglamp_csk_fcst_granalysis
          trigger ../obs/jglamp_obs_prep == complete
        task jglamp_tdp_fcst_prdgen
          trigger jglamp_tdp_fcst_granalysis == complete
        task jglamp_csk_fcst_prdgen
          trigger jglamp_csk_fcst_granalysis == complete
        task jglamp_vis_fcst_prdgen
          trigger jglamp_vis_fcst_granalysis == complete
        task jglamp_wsd_fcst_prdgen
          trigger jglamp_wsd_fcst_granalysis == complete
      endfamily
    endfamily
    family glamp_14
      edit CYC '14'
      family obs
        task jglamp_obs_prep
          time 14:26
        task jglamp_mos_prep
          time 14:26
        task jglamp_csk_obs_granalysis
          trigger jglamp_obs_prep == complete
        task jglamp_wsd_obs_granalysis
          trigger jglamp_obs_prep == complete and jglamp_mos_prep == complete
        task jglamp_vis_obs_granalysis
          trigger jglamp_obs_prep == complete
        task jglamp_tdp_obs_granalysis
          trigger jglamp_obs_prep == complete and jglamp_mos_prep == complete
        task jglamp_csk_obs_prdgen
          trigger jglamp_csk_obs_granalysis == complete
        task jglamp_wsd_obs_prdgen
          trigger jglamp_wsd_obs_granalysis == complete
        task jglamp_vis_obs_prdgen
          trigger jglamp_vis_obs_granalysis == complete
        task jglamp_tdp_obs_prdgen
          trigger jglamp_tdp_obs_granalysis == complete
      endfamily
      family fcst
        trigger /prod12/lamp/lamp_14/prdgen/jlamp_prdgen == complete
        task jglamp_tdp_fcst_granalysis
          trigger ../obs/jglamp_tdp_obs_granalysis == complete
        task jglamp_wsd_fcst_granalysis
          trigger ../obs/jglamp_wsd_obs_granalysis == complete
        task jglamp_vis_fcst_granalysis
          trigger ../obs/jglamp_obs_prep == complete
        task jglamp_csk_fcst_granalysis
          trigger ../obs/jglamp_obs_prep == complete
        task jglamp_tdp_fcst_prdgen
          trigger jglamp_tdp_fcst_granalysis == complete
        task jglamp_csk_fcst_prdgen
          trigger jglamp_csk_fcst_granalysis == complete
        task jglamp_vis_fcst_prdgen
          trigger jglamp_vis_fcst_granalysis == complete
        task jglamp_wsd_fcst_prdgen
          trigger jglamp_wsd_fcst_granalysis == complete
      endfamily
    endfamily
    family glamp_15
      edit CYC '15'
      family obs
        task jglamp_obs_prep
          time 15:26
        task jglamp_mos_prep
          time 15:26
        task jglamp_csk_obs_granalysis
          trigger jglamp_obs_prep == complete
        task jglamp_wsd_obs_granalysis
          trigger jglamp_obs_prep == complete and jglamp_mos_prep == complete
        task jglamp_vis_obs_granalysis
          trigger jglamp_obs_prep == complete
        task jglamp_tdp_obs_granalysis
          trigger jglamp_obs_prep == complete and jglamp_mos_prep == complete
        task jglamp_csk_obs_prdgen
          trigger jglamp_csk_obs_granalysis == complete
        task jglamp_wsd_obs_prdgen
          trigger jglamp_wsd_obs_granalysis == complete
        task jglamp_vis_obs_prdgen
          trigger jglamp_vis_obs_granalysis == complete
        task jglamp_tdp_obs_prdgen
          trigger jglamp_tdp_obs_granalysis == complete
      endfamily
      family fcst
        trigger /prod12/lamp/lamp_15/prdgen/jlamp_prdgen == complete
        task jglamp_tdp_fcst_granalysis
          trigger ../obs/jglamp_tdp_obs_granalysis == complete
        task jglamp_wsd_fcst_granalysis
          trigger ../obs/jglamp_wsd_obs_granalysis == complete
        task jglamp_vis_fcst_granalysis
          trigger ../obs/jglamp_obs_prep == complete
        task jglamp_csk_fcst_granalysis
          trigger ../obs/jglamp_obs_prep == complete
        task jglamp_tdp_fcst_prdgen
          trigger jglamp_tdp_fcst_granalysis == complete
        task jglamp_csk_fcst_prdgen
          trigger jglamp_csk_fcst_granalysis == complete
        task jglamp_vis_fcst_prdgen
          trigger jglamp_vis_fcst_granalysis == complete
        task jglamp_wsd_fcst_prdgen
          trigger jglamp_wsd_fcst_granalysis == complete
      endfamily
    endfamily
    family glamp_16
      edit CYC '16'
      family obs
        task jglamp_obs_prep
          time 16:26
        task jglamp_mos_prep
          time 16:26
        task jglamp_csk_obs_granalysis
          trigger jglamp_obs_prep == complete
        task jglamp_wsd_obs_granalysis
          trigger jglamp_obs_prep == complete and jglamp_mos_prep == complete
        task jglamp_vis_obs_granalysis
          trigger jglamp_obs_prep == complete
        task jglamp_tdp_obs_granalysis
          trigger jglamp_obs_prep == complete and jglamp_mos_prep == complete
        task jglamp_csk_obs_prdgen
          trigger jglamp_csk_obs_granalysis == complete
        task jglamp_wsd_obs_prdgen
          trigger jglamp_wsd_obs_granalysis == complete
        task jglamp_vis_obs_prdgen
          trigger jglamp_vis_obs_granalysis == complete
        task jglamp_tdp_obs_prdgen
          trigger jglamp_tdp_obs_granalysis == complete
      endfamily
      family fcst
        trigger /prod12/lamp/lamp_16/prdgen/jlamp_prdgen == complete
        task jglamp_tdp_fcst_granalysis
          trigger ../obs/jglamp_tdp_obs_granalysis == complete
        task jglamp_wsd_fcst_granalysis
          trigger ../obs/jglamp_wsd_obs_granalysis == complete
        task jglamp_vis_fcst_granalysis
          trigger ../obs/jglamp_obs_prep == complete
        task jglamp_csk_fcst_granalysis
          trigger ../obs/jglamp_obs_prep == complete
        task jglamp_tdp_fcst_prdgen
          trigger jglamp_tdp_fcst_granalysis == complete
        task jglamp_csk_fcst_prdgen
          trigger jglamp_csk_fcst_granalysis == complete
        task jglamp_vis_fcst_prdgen
          trigger jglamp_vis_fcst_granalysis == complete
        task jglamp_wsd_fcst_prdgen
          trigger jglamp_wsd_fcst_granalysis == complete
      endfamily
    endfamily
    family glamp_17
      edit CYC '17'
      family obs
        task jglamp_obs_prep
          time 17:26
        task jglamp_mos_prep
          time 17:26
        task jglamp_csk_obs_granalysis
          trigger jglamp_obs_prep == complete
        task jglamp_wsd_obs_granalysis
          trigger jglamp_obs_prep == complete and jglamp_mos_prep == complete
        task jglamp_vis_obs_granalysis
          trigger jglamp_obs_prep == complete
        task jglamp_tdp_obs_granalysis
          trigger jglamp_obs_prep == complete and jglamp_mos_prep == complete
        task jglamp_csk_obs_prdgen
          trigger jglamp_csk_obs_granalysis == complete
        task jglamp_wsd_obs_prdgen
          trigger jglamp_wsd_obs_granalysis == complete
        task jglamp_vis_obs_prdgen
          trigger jglamp_vis_obs_granalysis == complete
        task jglamp_tdp_obs_prdgen
          trigger jglamp_tdp_obs_granalysis == complete
      endfamily
      family fcst
        trigger /prod12/lamp/lamp_17/prdgen/jlamp_prdgen == complete
        task jglamp_tdp_fcst_granalysis
          trigger ../obs/jglamp_tdp_obs_granalysis == complete
        task jglamp_wsd_fcst_granalysis
          trigger ../obs/jglamp_wsd_obs_granalysis == complete
        task jglamp_vis_fcst_granalysis
          trigger ../obs/jglamp_obs_prep == complete
        task jglamp_csk_fcst_granalysis
          trigger ../obs/jglamp_obs_prep == complete
        task jglamp_tdp_fcst_prdgen
          trigger jglamp_tdp_fcst_granalysis == complete
        task jglamp_csk_fcst_prdgen
          trigger jglamp_csk_fcst_granalysis == complete
        task jglamp_vis_fcst_prdgen
          trigger jglamp_vis_fcst_granalysis == complete
        task jglamp_wsd_fcst_prdgen
          trigger jglamp_wsd_fcst_granalysis == complete
      endfamily
    endfamily
  endfamily
  family wave
    edit PROJ 'WAV-OPS'
    edit ECF_JOB_CMD '/ecf/ecfutils/unixsubmit %ECF_JOB% %ECF_JOBOUT% ibmsp'
    family wave_glw
      task jwave_glwn_run_chk
        edit CYC '15'
        event EST
        event DST
        time 13:10
        time 14:10
      task jwave_glwn_prep
        trigger (jwave_glwn_run_chk:DST) || (jwave_glwn_run_chk:EST)
        edit CYC '15'
      task jwave_glwn_ice_warning
        defstatus complete
        edit CYC '15'
        event ice_email_sent
      task jwave_glwn_wind_warning
        defstatus complete
        edit CYC '15'
        event wind_email_sent
      task jwave_glwn_forecast
        trigger jwave_glwn_prep == complete
        edit CYC '15'
      task jwave_glwn_post
        trigger jwave_glwn_forecast == complete
        edit CYC '15'
      task jwave_glwn_pgen
        trigger jwave_glwn_post == complete
        edit CYC '15'
      task jwave_glw_prep
        trigger /prod12/nam/sminit_conus == complete and /prod12/nam/sminit_conus2p5 == complete
      task jwave_glw_forecast
        trigger jwave_glw_prep == complete
      task jwave_glw_post
        trigger jwave_glw_forecast == complete
    endfamily
    family multi_1
      task jwave_multi_1_prep
        trigger /prod12/gfs/post/jgfs_post_f03 == active or /prod12/gfs/post/jgfs_post_f03 == complete
      task jwave_multi_1_forecast
        trigger jwave_multi_1_prep == complete
      task jwave_multi_1_windupdt
        trigger jwave_multi_1_forecast == active
      task jwave_multi_1_grib2
        trigger jwave_multi_1_forecast == active or jwave_multi_1_forecast == complete
        meter glo_30m 0 180 180
        meter ak_4m 0 180 180
        meter at_4m 0 180 180
        meter wc_4m 0 180 180
      task jwave_multi_1_post
        trigger jwave_multi_1_forecast == complete
        event nww3_grib1_ready
      family gempak
        task jwave_global_0p5deg_gempak
          trigger ../jwave_multi_1_grib2:glo_30m ge 24
        task jwave_alaska_gempak
          trigger ../jwave_multi_1_grib2:ak_4m ge 24
        task jwave_alaska_gempak_meta
          trigger jwave_alaska_gempak == complete
        task jwave_wna_gempak
          trigger ../jwave_multi_1_grib2:at_4m ge 24 and ../jwave_multi_1_grib2:wc_4m ge 24
        task jwave_wna_gempak_meta
          trigger jwave_wna_gempak == complete
        task jwave_gulf_tiff
          trigger jwave_wna_gempak == complete
        task jwave_enp_gempak
          trigger ../jwave_multi_1_post == active or ../jwave_multi_1_post == complete
        task jwave_global_gempak
          trigger ../jwave_multi_1_post:nww3_grib1_ready
        task jwave_global_gempak_meta
          trigger jwave_global_gempak == complete
      endfamily
      family prdgen
        trigger jwave_multi_1_post == complete
        task jwave_global_prdgen
        task jwave_multi_1_prdgen
        task jwave_alaska_prdgen
        task jwave_enp_prdgen
        task jwave_wna_prdgen
      endfamily
    endfamily
    family multi_2
      time 17:31
      task jwave_multi_2_prep
        trigger /prod12/gfs/post/jgfs_post_f180 == complete and /prod12/hwrf/hwrf1/post/jhwrf_products == complete and /prod12/hwrf/hwrf2/post/jhwrf_products == complete and /prod12/hwrf/hwrf3/post/jhwrf_products == complete and /prod12/hwrf/hwrf4/post/jhwrf_products == complete and /prod12/hwrf/hwrf6/post/jhwrf_products == complete and /prod12/hwrf/hwrf7/post/jhwrf_products == complete and /prod12/hwrf/hwrf5/post/jhwrf_products == complete and /prod12/hwrf/hwrf1/jhwrf_output == complete and /prod12/hwrf/hwrf2/jhwrf_output == complete and /prod12/hwrf/hwrf3/jhwrf_output == complete and /prod12/hwrf/hwrf4/jhwrf_output == complete and /prod12/hwrf/hwrf5/jhwrf_output == complete and /prod12/hwrf/hwrf6/jhwrf_output == complete and /prod12/hwrf/hwrf7/jhwrf_output == complete
      task jwave_multi_2_forecast
        trigger jwave_multi_2_prep == complete
      task jwave_multi_2_post
        trigger jwave_multi_2_forecast == complete
      task jwave_multi_2_prdgen
        trigger jwave_multi_2_post == complete
      family gempak
        trigger jwave_multi_2_post == complete
        task jwave_nah_gempak
        task jwave_nah_gempak_meta
          trigger jwave_nah_gempak == complete
        task jwave_nph_gempak
        task jwave_multi_2_gempak
      endfamily
    endfamily
  endfamily
  family fnmoc
    family fnmoc_wave
      edit ECF_FILES '/ecf/ecfnets/scripts/fnmoc_wave'
      edit QUEUE 'prod_shared'
      task jfnmoc_wave_gempak
        time 18:35
    endfamily
  endfamily
  family nos12
    family glofs12
      task jnos_glofs_forecast_12
        edit ECF_PASS 'FREE'
        time 12:50
      task jnos_glofs_forecast_13
        edit ECF_PASS 'FREE'
        time 13:50
      task jnos_glofs_forecast_14
        edit ECF_PASS 'FREE'
        time 14:50
      task jnos_glofs_forecast_15
        edit ECF_PASS 'FREE'
        time 15:50
      task jnos_glofs_forecast_16
        edit ECF_PASS 'FREE'
        time 16:50
      task jnos_glofs_forecast_17
        edit ECF_PASS 'FREE'
        time 17:50
    endfamily
  endfamily
  family rtma
    edit ECF_FILES '/ecf/ecfnets/scripts/rtma'
    edit PROJ 'RTMA-OPS'
    edit QUEUE 'prod2'
    edit QUEUESERV 'prod2_serv'
    edit COM 'com2'
    family rtma_12
      edit CYC '12'
      family dump
        task jrtma_dump
          time 12:30
        task jrtma_dump_post
          trigger jrtma_dump == complete
      endfamily
      family prep
        task jrtma_prep
          trigger ../dump/jrtma_dump == complete
        task jrtma_prep_post
          trigger jrtma_prep == complete
      endfamily
      family hi
        task jhirtma_getguess
          trigger ../prep/jrtma_prep == complete and /prod06/nam/sminit_hi/jnam_sminit_f09 == complete
        task jhirtma_gsianl
          trigger jhirtma_getguess == complete
        task jhirtma_post
          trigger jhirtma_gsianl == complete
      endfamily
      family guam
        task jgurtma_getguess
          trigger ../prep/jrtma_prep == complete and /prod06/gfs/sminit_guam/jgfs_sminit_guam == complete
          time 12:30
        task jgurtma_gsianl
          trigger jgurtma_getguess == complete
        task jgurtma_post
          trigger jgurtma_gsianl == complete
        task jgurtma_gempak
          trigger jgurtma_post == complete
      endfamily
      family pr
        task jprrtma_getguess
          trigger ../prep/jrtma_prep == complete and /prod06/nam/sminit_pr/jnam_sminit_f09 == complete
        task jprrtma_gsianl
          trigger jprrtma_getguess == complete
        task jprrtma_post
          trigger jprrtma_gsianl == complete
      endfamily
      family ak
        task jakrtma_getguess
          trigger ../prep/jrtma_prep == complete and /prod06/nam/sminit_ak/jnam_sminit_f09 == complete
        task jakrtma_gsianl
          trigger jakrtma_getguess == complete
        task jakrtma_post
          trigger jakrtma_gsianl == complete
        task jakrtma3p0_gempak
          trigger jakrtma_post == complete
      endfamily
      family rtma2p5
        task jrtma2p5_getguess
          trigger ../prep/jrtma_prep == complete and /prod06/rap/rap_11/prdgen/jrap_prdgen_f01 == complete
        task jrtma2p5_gsianl
          trigger jrtma2p5_getguess == complete
        task jrtma2p5_post
          trigger jrtma2p5_gsianl == complete
        task jrtma2p5_gempak
          trigger jrtma2p5_post == complete
      endfamily
    endfamily
    family rtma_13
      edit CYC '13'
      family dump
        task jrtma_dump
          time 13:30
        task jrtma_dump_post
          trigger jrtma_dump == complete
      endfamily
      family prep
        task jrtma_prep
          trigger ../dump/jrtma_dump == complete
        task jrtma_prep_post
          trigger jrtma_prep == complete
      endfamily
      family hi
        task jhirtma_getguess
          trigger ../prep/jrtma_prep == complete and /prod06/nam/sminit_hi/jnam_sminit_f09 == complete
        task jhirtma_gsianl
          trigger jhirtma_getguess == complete
        task jhirtma_post
          trigger jhirtma_gsianl == complete
      endfamily
      family pr
        task jprrtma_getguess
          trigger ../prep/jrtma_prep == complete and /prod06/nam/sminit_pr/jnam_sminit_f09 == complete
        task jprrtma_gsianl
          trigger jprrtma_getguess == complete
        task jprrtma_post
          trigger jprrtma_gsianl == complete
      endfamily
      family ak
        task jakrtma_getguess
          trigger ../prep/jrtma_prep == complete and /prod06/nam/sminit_ak/jnam_sminit_f09 == complete
        task jakrtma_gsianl
          trigger jakrtma_getguess == complete
        task jakrtma_post
          trigger jakrtma_gsianl == complete
        task jakrtma3p0_gempak
          trigger jakrtma_post == complete
      endfamily
      family rtma2p5
        task jrtma2p5_getguess
          trigger ../prep/jrtma_prep == complete and /prod12/rap/rap_12/prdgen/jrap_prdgen_f01 == complete
        task jrtma2p5_gsianl
          trigger jrtma2p5_getguess == complete
        task jrtma2p5_post
          trigger jrtma2p5_gsianl == complete
        task jrtma2p5_gempak
          trigger jrtma2p5_post == complete
      endfamily
    endfamily
    family rtma_14
      edit CYC '14'
      family dump
        task jrtma_dump
          time 14:30
        task jrtma_dump_post
          trigger jrtma_dump == complete
      endfamily
      family prep
        task jrtma_prep
          trigger ../dump/jrtma_dump == complete
        task jrtma_prep_post
          trigger jrtma_prep == complete
      endfamily
      family hi
        task jhirtma_getguess
          trigger ../prep/jrtma_prep == complete and /prod12/nam/sminit_hi/jnam_sminit_f09 == complete
        task jhirtma_gsianl
          trigger jhirtma_getguess == complete
        task jhirtma_post
          trigger jhirtma_gsianl == complete
      endfamily
      family pr
        task jprrtma_getguess
          trigger ../prep/jrtma_prep == complete and /prod12/nam/sminit_pr/jnam_sminit_f09 == complete
        task jprrtma_gsianl
          trigger jprrtma_getguess == complete
        task jprrtma_post
          trigger jprrtma_gsianl == complete
      endfamily
      family ak
        task jakrtma_getguess
          trigger ../prep/jrtma_prep == complete and /prod12/nam/sminit_ak/jnam_sminit_f09 == complete
        task jakrtma_gsianl
          trigger jakrtma_getguess == complete
        task jakrtma_post
          trigger jakrtma_gsianl == complete
        task jakrtma3p0_gempak
          trigger jakrtma_post == complete
      endfamily
      family rtma2p5
        task jrtma2p5_getguess
          trigger ../prep/jrtma_prep == complete and /prod12/rap/rap_13/prdgen/jrap_prdgen_f01 == complete
        task jrtma2p5_gsianl
          trigger jrtma2p5_getguess == complete
        task jrtma2p5_post
          trigger jrtma2p5_gsianl == complete
        task jrtma2p5_gempak
          trigger jrtma2p5_post == complete
      endfamily
    endfamily
    family rtma_15
      edit CYC '15'
      family dump
        task jrtma_dump
          time 15:30
        task jrtma_dump_post
          trigger jrtma_dump == complete
      endfamily
      family prep
        task jrtma_prep
          trigger ../dump/jrtma_dump == complete
        task jrtma_prep_post
          trigger jrtma_prep == complete
      endfamily
      family pr
        task jprrtma_getguess
          trigger ../prep/jrtma_prep == complete and /prod12/nam/sminit_pr/jnam_sminit_f09 == complete
        task jprrtma_gsianl
          trigger jprrtma_getguess == complete
        task jprrtma_post
          trigger jprrtma_gsianl == complete
      endfamily
      family hi
        task jhirtma_getguess
          trigger ../prep/jrtma_prep == complete and /prod12/nam/sminit_hi/jnam_sminit_f09 == complete
        task jhirtma_gsianl
          trigger jhirtma_getguess == complete
        task jhirtma_post
          trigger jhirtma_gsianl == complete
      endfamily
      family guam
        task jgurtma_getguess
          trigger ../prep/jrtma_prep == complete and /prod06/gfs/sminit_guam/jgfs_sminit_guam == complete
          time 15:30
        task jgurtma_gsianl
          trigger jgurtma_getguess == complete
        task jgurtma_post
          trigger jgurtma_gsianl == complete
        task jgurtma_gempak
          trigger jgurtma_post == complete
      endfamily
      family ak
        task jakrtma_getguess
          trigger ../prep/jrtma_prep == complete and /prod12/nam/sminit_ak/jnam_sminit_f09 == complete
        task jakrtma_gsianl
          trigger jakrtma_getguess == complete
        task jakrtma_post
          trigger jakrtma_gsianl == complete
        task jakrtma3p0_gempak
          trigger jakrtma_post == complete
      endfamily
      family rtma2p5
        task jrtma2p5_getguess
          trigger ../prep/jrtma_prep == complete and /prod12/rap/rap_14/prdgen/jrap_prdgen_f01 == complete
        task jrtma2p5_gsianl
          trigger jrtma2p5_getguess == complete
        task jrtma2p5_post
          trigger jrtma2p5_gsianl == complete
        task jrtma2p5_gempak
          trigger jrtma2p5_post == complete
      endfamily
    endfamily
    family rtma_16
      edit CYC '16'
      family dump
        task jrtma_dump
          time 16:30
        task jrtma_dump_post
          trigger jrtma_dump == complete
      endfamily
      family prep
        task jrtma_prep
          trigger ../dump/jrtma_dump == complete
        task jrtma_prep_post
          trigger jrtma_prep == complete
      endfamily
      family hi
        task jhirtma_getguess
          trigger ../prep/jrtma_prep == complete and /prod12/nam/sminit_hi/jnam_sminit_f09 == complete
        task jhirtma_gsianl
          trigger jhirtma_getguess == complete
        task jhirtma_post
          trigger jhirtma_gsianl == complete
      endfamily
      family pr
        task jprrtma_getguess
          trigger ../prep/jrtma_prep == complete and /prod12/nam/sminit_pr/jnam_sminit_f09 == complete
        task jprrtma_gsianl
          trigger jprrtma_getguess == complete
        task jprrtma_post
          trigger jprrtma_gsianl == complete
      endfamily
      family ak
        task jakrtma_getguess
          trigger ../prep/jrtma_prep == complete and /prod12/nam/sminit_ak/jnam_sminit_f09 == complete
        task jakrtma_gsianl
          trigger jakrtma_getguess == complete
        task jakrtma_post
          trigger jakrtma_gsianl == complete
        task jakrtma3p0_gempak
          trigger jakrtma_post == complete
      endfamily
      family rtma2p5
        task jrtma2p5_getguess
          trigger ../prep/jrtma_prep == complete and /prod12/rap/rap_15/prdgen/jrap_prdgen_f01 == complete
        task jrtma2p5_gsianl
          trigger jrtma2p5_getguess == complete
        task jrtma2p5_post
          trigger jrtma2p5_gsianl == complete
        task jrtma2p5_gempak
          trigger jrtma2p5_post == complete
      endfamily
    endfamily
    family rtma_17
      edit CYC '17'
      family dump
        task jrtma_dump
          time 17:30
        task jrtma_dump_post
          trigger jrtma_dump == complete
      endfamily
      family prep
        task jrtma_prep
          trigger ../dump/jrtma_dump == complete
        task jrtma_prep_post
          trigger jrtma_prep == complete
      endfamily
      family hi
        task jhirtma_getguess
          trigger ../prep/jrtma_prep == complete and /prod12/nam/sminit_hi/jnam_sminit_f09 == complete
        task jhirtma_gsianl
          trigger jhirtma_getguess == complete
        task jhirtma_post
          trigger jhirtma_gsianl == complete
      endfamily
      family pr
        task jprrtma_getguess
          trigger ../prep/jrtma_prep == complete and /prod12/nam/sminit_pr/jnam_sminit_f09 == complete
        task jprrtma_gsianl
          trigger jprrtma_getguess == complete
        task jprrtma_post
          trigger jprrtma_gsianl == complete
      endfamily
      family ak
        task jakrtma_getguess
          trigger ../prep/jrtma_prep == complete and /prod12/nam/sminit_ak/jnam_sminit_f09 == complete
        task jakrtma_gsianl
          trigger jakrtma_getguess == complete
        task jakrtma_post
          trigger jakrtma_gsianl == complete
        task jakrtma3p0_gempak
          trigger jakrtma_post == complete
      endfamily
      family rtma2p5
        task jrtma2p5_getguess
          trigger ../prep/jrtma_prep == complete and /prod12/rap/rap_16/prdgen/jrap_prdgen_f01 == complete
        task jrtma2p5_gsianl
          trigger jrtma2p5_getguess == complete
        task jrtma2p5_post
          trigger jrtma2p5_gsianl == complete
        task jrtma2p5_gempak
          trigger jrtma2p5_post == complete
      endfamily
    endfamily
  endfamily
  family urma
    edit ECF_FILES '/ecf/ecfnets/scripts/urma'
    edit PROJ 'RTMA-OPS'
    edit QUEUE 'prod2'
    edit QUEUESERV 'prod2_serv'
    edit COM 'com2'
    family urma_06
      edit CYC '06'
      family dump
        task jurma_dump
          time 12:30
        task jurma_dump_post
          trigger ../dump/jurma_dump == complete
      endfamily
      family prep
        task jurma_prep
          trigger ../dump/jurma_dump == complete
        task jurma_prep_post
          trigger ../prep/jurma_prep == complete
      endfamily
      family urma2p5
        task jurma2p5_getguess
          trigger ../prep/jurma_prep == complete
        task jurma2p5_gsianl
          trigger jurma2p5_getguess == complete
        task jurma2p5_post
          trigger jurma2p5_gsianl == complete
        task jurma2p5_gempak
          trigger jurma2p5_post == complete
      endfamily
      family ak
        task jakurma_getguess
          trigger ../prep/jurma_prep == complete
        task jakurma_gsianl
          trigger jakurma_getguess == complete
        task jakurma_post
          trigger jakurma_gsianl == complete
        task jakurma_gempak
          trigger jakurma_post == complete
      endfamily
    endfamily
    family urma_07
      edit CYC '07'
      family dump
        task jurma_dump
          time 13:30
        task jurma_dump_post
          trigger ../dump/jurma_dump == complete
      endfamily
      family prep
        task jurma_prep
          trigger ../dump/jurma_dump == complete
        task jurma_prep_post
          trigger ../prep/jurma_prep == complete
        task jurma_maxt_prep
          trigger ../urma2p5 == complete and ../ak == complete
      endfamily
      family urma2p5
        task jurma2p5_getguess
          trigger ../prep/jurma_prep == complete
        task jurma2p5_gsianl
          trigger jurma2p5_getguess == complete
        task jurma2p5_post
          trigger jurma2p5_gsianl == complete
        task jurma2p5_gempak
          trigger jurma2p5_post == complete
      endfamily
      family ak
        task jakurma_getguess
          trigger ../prep/jurma_prep == complete
        task jakurma_gsianl
          trigger jakurma_getguess == complete
        task jakurma_post
          trigger jakurma_gsianl == complete
        task jakurma_gempak
          trigger jakurma_post == complete
      endfamily
    endfamily
    family urma_08
      edit CYC '08'
      family dump
        task jurma_dump
          time 14:30
        task jurma_dump_post
          trigger ../dump/jurma_dump == complete
      endfamily
      family prep
        task jurma_prep
          trigger ../dump/jurma_dump == complete
        task jurma_prep_post
          trigger ../prep/jurma_prep == complete
      endfamily
      family urma2p5
        task jurma2p5_getguess
          trigger ../prep/jurma_prep == complete and ../../urma_07/prep/jurma_maxt_prep == complete
        task jurma2p5_gsianl
          trigger jurma2p5_getguess == complete
        task jurma2p5_post
          trigger jurma2p5_gsianl == complete
        task jurma2p5_gempak
          trigger jurma2p5_post == complete
      endfamily
      family ak
        task jakurma_getguess
          trigger ../prep/jurma_prep == complete and ../../urma_07/prep/jurma_maxt_prep == complete
        task jakurma_gsianl
          trigger jakurma_getguess == complete
        task jakurma_post
          trigger jakurma_gsianl == complete
        task jakurma_gempak
          trigger jakurma_post == complete
      endfamily
    endfamily
    family urma_09
      edit CYC '09'
      family dump
        task jurma_dump
          time 15:30
        task jurma_dump_post
          trigger ../dump/jurma_dump == complete
      endfamily
      family prep
        task jurma_prep
          trigger ../dump/jurma_dump == complete
        task jurma_prep_post
          trigger ../prep/jurma_prep == complete
      endfamily
      family urma2p5
        task jurma2p5_getguess
          trigger ../prep/jurma_prep == complete
        task jurma2p5_gsianl
          trigger jurma2p5_getguess == complete
        task jurma2p5_post
          trigger jurma2p5_gsianl == complete
        task jurma2p5_gempak
          trigger jurma2p5_post == complete
      endfamily
      family ak
        task jakurma_getguess
          trigger ../prep/jurma_prep == complete
        task jakurma_gsianl
          trigger jakurma_getguess == complete
        task jakurma_post
          trigger jakurma_gsianl == complete
        task jakurma_gempak
          trigger jakurma_post == complete
      endfamily
    endfamily
    family urma_10
      edit CYC '10'
      family dump
        task jurma_dump
          time 16:30
        task jurma_dump_post
          trigger ../dump/jurma_dump == complete
      endfamily
      family prep
        task jurma_prep
          trigger ../dump/jurma_dump == complete
        task jurma_prep_post
          trigger ../prep/jurma_prep == complete
      endfamily
      family urma2p5
        task jurma2p5_getguess
          trigger ../prep/jurma_prep == complete
        task jurma2p5_gsianl
          trigger jurma2p5_getguess == complete
        task jurma2p5_post
          trigger jurma2p5_gsianl == complete
        task jurma2p5_gempak
          trigger jurma2p5_post == complete
      endfamily
      family ak
        task jakurma_getguess
          trigger ../prep/jurma_prep == complete
        task jakurma_gsianl
          trigger jakurma_getguess == complete
        task jakurma_post
          trigger jakurma_gsianl == complete
        task jakurma_gempak
          trigger jakurma_post == complete
      endfamily
    endfamily
    family urma_11
      edit CYC '11'
      family dump
        task jurma_dump
          time 17:30
        task jurma_dump_post
          trigger ../dump/jurma_dump == complete
      endfamily
      family prep
        task jurma_prep
          trigger ../dump/jurma_dump == complete
        task jurma_prep_post
          trigger ../prep/jurma_prep == complete
      endfamily
      family urma2p5
        task jurma2p5_getguess
          trigger ../prep/jurma_prep == complete
        task jurma2p5_gsianl
          trigger jurma2p5_getguess == complete
        task jurma2p5_post
          trigger jurma2p5_gsianl == complete
        task jurma2p5_gempak
          trigger jurma2p5_post == complete
      endfamily
      family ak
        task jakurma_getguess
          trigger ../prep/jurma_prep == complete
        task jakurma_gsianl
          trigger jakurma_getguess == complete
        task jakurma_post
          trigger jakurma_gsianl == complete
        task jakurma_gempak
          trigger jakurma_post == complete
      endfamily
    endfamily
  endfamily
#
  family hiresw
    defstatus complete
    edit ECF_FILES '/ecf/ecfnets/scripts/hiresw'
    edit ECF_JOB_CMD '/ecf/ecfutils/unixsubmit %ECF_JOB% %ECF_JOBOUT% xc40p'
    edit COM 'gpfs/hps/nco/ops/com'
    edit PROJ 'HRW-OPS'
    edit QUEUE 'prod'
    family nmmb
      edit MODEL 'nmmb'
      family hi
        edit DOMAIN 'hi'
        family prep
          task jhiresw_ungrib
            trigger /prod12/gfs/post/jgfs_post_f81 == complete
            edit NTASK '4'
            edit PTILE '4'
          task jhiresw_metgrid
            trigger jhiresw_ungrib == complete
            edit NTASK '9'
            edit PTILE '9'
          family nemsinterp
            edit NTASK '4'
            edit PTILE '4'
            task jhiresw_nemsinterp_1
              trigger ../jhiresw_metgrid == complete
            task jhiresw_nemsinterp_2
              trigger ../jhiresw_metgrid == complete
            task jhiresw_nemsinterp_3
              trigger ../jhiresw_metgrid == complete
            task jhiresw_nemsinterp_4
              trigger ../jhiresw_metgrid == complete
          endfamily
        endfamily
        task jhiresw_forecast
          trigger prep/nemsinterp == complete
          edit NTASK '62'
          edit PTILE '21'
          event release_downstream
        task jhiresw_post
          trigger jhiresw_forecast:release_downstream
          edit NTASK '2'
          edit PTILE '2'
        task jhiresw_prdgen
          trigger jhiresw_forecast:release_downstream
          edit NTASK '2'
          edit PTILE '2'
        family post_processing
          task jhiresw_wrfbufrsnd
            trigger ../jhiresw_forecast:release_downstream
            edit NTASK '1'
            edit PTILE '1'
          task jhiresw_smartinit
            trigger ../jhiresw_forecast:release_downstream
            edit NTASK '8'
            edit PTILE '8'
          task jhiresw_smartinitb
            trigger ../jhiresw_forecast:release_downstream
            edit NTASK '1'
            edit PTILE '1'
          task jhiresw_awips
            trigger ../jhiresw_forecast:release_downstream
          task jhiresw_gempak
            trigger ../jhiresw_forecast:release_downstream
        endfamily
      endfamily
      family guam
        edit DOMAIN 'guam'
        family prep
          task jhiresw_ungrib
            trigger /prod12/gfs/post/jgfs_post_f81 == complete
            edit NTASK '4'
            edit PTILE '4'
          task jhiresw_metgrid
            trigger jhiresw_ungrib == complete
            edit NTASK '9'
            edit PTILE '9'
          family nemsinterp
            edit NTASK '4'
            edit PTILE '4'
            task jhiresw_nemsinterp_1
              trigger ../jhiresw_metgrid == complete
            task jhiresw_nemsinterp_2
              trigger ../jhiresw_metgrid == complete
            task jhiresw_nemsinterp_3
              trigger ../jhiresw_metgrid == complete
            task jhiresw_nemsinterp_4
              trigger ../jhiresw_metgrid == complete
          endfamily
        endfamily
        task jhiresw_forecast
          trigger prep/nemsinterp == complete
          edit NTASK '62'
          edit PTILE '21'
          event release_downstream
        task jhiresw_post
          trigger jhiresw_forecast:release_downstream
          edit NTASK '2'
          edit PTILE '2'
        task jhiresw_prdgen
          trigger jhiresw_forecast:release_downstream
          edit NTASK '2'
          edit PTILE '2'
        family post_processing
          task jhiresw_wrfbufrsnd
            trigger ../jhiresw_forecast:release_downstream
            edit NTASK '1'
            edit PTILE '1'
          task jhiresw_smartinit
            trigger ../jhiresw_forecast:release_downstream
            edit NTASK '8'
            edit PTILE '8'
          task jhiresw_smartinitb
            trigger ../jhiresw_forecast:release_downstream
            edit NTASK '1'
            edit PTILE '1'
          task jhiresw_awips
            trigger ../jhiresw_forecast:release_downstream
          task jhiresw_gempak
            trigger ../jhiresw_forecast:release_downstream
        endfamily
      endfamily
      family conus
        edit DOMAIN 'conus'
        family prep
          task jhiresw_preprap
            trigger /prod12/rap/rap_12/prdgen == complete and /prod12/nam/analysis/jnam_analysis == complete
            edit NTASK '9'
            edit PTILE '3'
          task jhiresw_ungrib
            trigger /prod12/gfs/post/jgfs_post_f102 == complete
            edit NTASK '4'
            edit PTILE '4'
          task jhiresw_metgrid
            trigger jhiresw_ungrib == complete
            edit NTASK '9'
            edit PTILE '9'
          family nemsinterp
            edit NTASK '4'
            edit PTILE '4'
            task jhiresw_nemsinterp_1
              trigger ../jhiresw_metgrid == complete
            task jhiresw_nemsinterp_2
              trigger ../jhiresw_metgrid == complete
            task jhiresw_nemsinterp_3
              trigger ../jhiresw_metgrid == complete
            task jhiresw_nemsinterp_4
              trigger ../jhiresw_metgrid == complete
          endfamily
        endfamily
        task jhiresw_forecast
          trigger prep/nemsinterp == complete and prep/jhiresw_preprap == complete
          edit NTASK '600'
          edit PTILE '24'
          event release_downstream
        task jhiresw_post
          trigger jhiresw_forecast:release_downstream
          edit NTASK '24'
          edit PTILE '8'
        task jhiresw_prdgen
          trigger jhiresw_forecast:release_downstream
          edit NTASK '13'
          edit PTILE '13'
        family post_processing
          task jhiresw_wrfbufrsnd
            trigger ../jhiresw_forecast:release_downstream
            edit NTASK '1'
            edit PTILE '1'
          task jhiresw_smartinit
            trigger ../jhiresw_forecast:release_downstream
            edit NTASK '8'
            edit PTILE '8'
          task jhiresw_smartinitb
            trigger ../jhiresw_forecast:release_downstream
            edit NTASK '1'
            edit PTILE '1'
          task jhiresw_awips
            trigger ../jhiresw_forecast:release_downstream
          task jhiresw_gempak
            trigger ../jhiresw_forecast:release_downstream
        endfamily
      endfamily
    endfamily
    family arw
      edit MODEL 'arw'
      family hi
        edit DOMAIN 'hi'
        family prep
          task jhiresw_ungrib
            trigger /prod12/gfs/post/jgfs_post_f81 == complete
            edit NTASK '4'
            edit PTILE '4'
          task jhiresw_metgrid
            trigger jhiresw_ungrib == complete
            edit NTASK '9'
            edit PTILE '9'
          task jhiresw_prepfinal
            trigger jhiresw_metgrid == complete
            edit NTASK '9'
            edit PTILE '3'
        endfamily
        task jhiresw_forecast
          trigger prep/jhiresw_prepfinal == complete
          edit NTASK '68'
          edit PTILE '23'
          event release_downstream
        task jhiresw_post
          trigger jhiresw_forecast:release_downstream
          edit NTASK '2'
          edit PTILE '2'
        task jhiresw_prdgen
          trigger jhiresw_forecast:release_downstream
          edit NTASK '2'
          edit PTILE '2'
        family post_processing
          task jhiresw_wrfbufrsnd
            trigger ../jhiresw_forecast:release_downstream
            edit NTASK '1'
            edit PTILE '1'
          task jhiresw_smartinit
            trigger ../jhiresw_forecast:release_downstream
            edit NTASK '8'
            edit PTILE '8'
          task jhiresw_smartinitb
            trigger ../jhiresw_forecast:release_downstream
            edit NTASK '1'
            edit PTILE '1'
          task jhiresw_awips
            trigger ../jhiresw_forecast:release_downstream
          task jhiresw_gempak
            trigger ../jhiresw_forecast:release_downstream
        endfamily
      endfamily
      family guam
        edit DOMAIN 'guam'
        family prep
          task jhiresw_ungrib
            trigger /prod12/gfs/post/jgfs_post_f81 == complete
            edit NTASK '4'
            edit PTILE '4'
          task jhiresw_metgrid
            trigger jhiresw_ungrib == complete
            edit NTASK '9'
            edit PTILE '9'
          task jhiresw_prepfinal
            trigger jhiresw_metgrid == complete
            edit NTASK '9'
            edit PTILE '3'
        endfamily
        task jhiresw_forecast
          trigger prep/jhiresw_prepfinal == complete
          edit NTASK '66'
          edit PTILE '22'
          event release_downstream
        task jhiresw_post
          trigger jhiresw_forecast:release_downstream
          edit NTASK '2'
          edit PTILE '2'
        task jhiresw_prdgen
          trigger jhiresw_forecast:release_downstream
          edit NTASK '2'
          edit PTILE '2'
        family post_processing
          task jhiresw_wrfbufrsnd
            trigger ../jhiresw_forecast:release_downstream
            edit NTASK '1'
            edit PTILE '1'
          task jhiresw_smartinit
            trigger ../jhiresw_forecast:release_downstream
            edit NTASK '8'
            edit PTILE '8'
          task jhiresw_smartinitb
            trigger ../jhiresw_forecast:release_downstream
            edit NTASK '1'
            edit PTILE '1'
          task jhiresw_awips
            trigger ../jhiresw_forecast:release_downstream
          task jhiresw_gempak
            trigger ../jhiresw_forecast:release_downstream
        endfamily
      endfamily
      family conus
        edit DOMAIN 'conus'
        family prep
          task jhiresw_preprap
            trigger /prod12/rap/rap_12/prdgen == complete
            edit NTASK '9'
            edit PTILE '3'
          task jhiresw_ungrib
            trigger /prod12/gfs/post/jgfs_post_f102 == complete
            edit NTASK '4'
            edit PTILE '4'
          task jhiresw_metgrid
            trigger jhiresw_ungrib == complete
            edit NTASK '9'
            edit PTILE '9'
          task jhiresw_prepfinal
            trigger jhiresw_metgrid == complete
            edit NTASK '9'
            edit PTILE '3'
        endfamily
        task jhiresw_forecast
          trigger prep/jhiresw_prepfinal == complete and prep/jhiresw_preprap == complete
          edit NTASK '840'
          edit PTILE '24'
          event release_downstream
        task jhiresw_post
          trigger jhiresw_forecast:release_downstream
          edit NTASK '24'
          edit PTILE '12'
        task jhiresw_prdgen
          trigger jhiresw_forecast:release_downstream
          edit NTASK '13'
          edit PTILE '13'
        family post_processing
          task jhiresw_wrfbufrsnd
            trigger ../jhiresw_forecast:release_downstream
            edit NTASK '1'
            edit PTILE '1'
          task jhiresw_smartinit
            trigger ../jhiresw_forecast:release_downstream
            edit NTASK '8'
            edit PTILE '8'
          task jhiresw_smartinitb
            trigger ../jhiresw_forecast:release_downstream
            edit NTASK '1'
            edit PTILE '1'
          task jhiresw_awips
            trigger ../jhiresw_forecast:release_downstream
          task jhiresw_gempak
            trigger ../jhiresw_forecast:release_downstream
        endfamily
      endfamily
    endfamily
    task jhiresw_cleanup
      trigger nmmb == complete and arw == complete
  endfamily
#
  family nldas
    edit ECF_FILES '/ecf/ecfnets/scripts/nldas'
    edit PROJ 'NLDAS-OPS'
    family prep
      task jnldas_prep
        trigger /prod12/rcdas/jrcdas_post_12 == complete
    endfamily
    family fcst
      task jnldas_mosaic
        trigger /prod12/nldas/prep/jnldas_prep == complete
      task jnldas_noah
        trigger /prod12/nldas/prep/jnldas_prep == complete
      task jnldas_sac
        trigger /prod12/nldas/fcst/jnldas_noah == complete
      task jnldas_vic
        trigger /prod12/nldas/prep/jnldas_prep == complete
    endfamily
    family routing
      task jnldas_rout_sac
        trigger /prod12/nldas/fcst/jnldas_sac == complete
      task jnldas_rout_mosaic
        trigger /prod12/nldas/fcst/jnldas_mosaic == complete
      task jnldas_rout_noah
        trigger /prod12/nldas/fcst/jnldas_noah == complete
      task jnldas_rout_vic
        trigger /prod12/nldas/fcst/jnldas_vic == complete
    endfamily
  endfamily
  family navgem
    edit PROJ 'GFS-OPS'
    task jnavgem_prdgen
      time 18:25
    task jnavgem_gempak
      trigger jnavgem_prdgen == complete
  endfamily
  family wave_ens12
    edit PROJ 'GWVENS-OPS'
    edit ECF_FILES '/ecf/ecfnets/scripts/prod/wave_ens'
    edit CYC '12'
    edit ECF_JOB_CMD '/ecf/ecfutils/unixsubmit %ECF_JOB% %ECF_JOBOUT% ibmsp'
    task jwave_gwes_prep
      trigger /prod12/gefs/prdgen_low/jgefs_c00_prdgen_low:pgrb2ap5_f240_ready and /prod12/gefs/prdgen_low/jgefs_p01_prdgen_low:pgrb2ap5_f240_ready and /prod12/gefs/prdgen_low/jgefs_p02_prdgen_low:pgrb2ap5_f240_ready and /prod12/gefs/prdgen_low/jgefs_p03_prdgen_low:pgrb2ap5_f240_ready and /prod12/gefs/prdgen_low/jgefs_p04_prdgen_low:pgrb2ap5_f240_ready and /prod12/gefs/prdgen_low/jgefs_p05_prdgen_low:pgrb2ap5_f240_ready and /prod12/gefs/prdgen_low/jgefs_p06_prdgen_low:pgrb2ap5_f240_ready and /prod12/gefs/prdgen_low/jgefs_p07_prdgen_low:pgrb2ap5_f240_ready and /prod12/gefs/prdgen_low/jgefs_p08_prdgen_low:pgrb2ap5_f240_ready and /prod12/gefs/prdgen_low/jgefs_p09_prdgen_low:pgrb2ap5_f240_ready and /prod12/gefs/prdgen_low/jgefs_p10_prdgen_low:pgrb2ap5_f240_ready and /prod12/gefs/prdgen_low/jgefs_p11_prdgen_low:pgrb2ap5_f240_ready and /prod12/gefs/prdgen_low/jgefs_p12_prdgen_low:pgrb2ap5_f240_ready and /prod12/gefs/prdgen_low/jgefs_p13_prdgen_low:pgrb2ap5_f240_ready and /prod12/gefs/prdgen_low/jgefs_p14_prdgen_low:pgrb2ap5_f240_ready and /prod12/gefs/prdgen_low/jgefs_p15_prdgen_low:pgrb2ap5_f240_ready and /prod12/gefs/prdgen_low/jgefs_p16_prdgen_low:pgrb2ap5_f240_ready and /prod12/gefs/prdgen_low/jgefs_p17_prdgen_low:pgrb2ap5_f240_ready and /prod12/gefs/prdgen_low/jgefs_p18_prdgen_low:pgrb2ap5_f240_ready and /prod12/gefs/prdgen_low/jgefs_p19_prdgen_low:pgrb2ap5_f240_ready and /prod12/gefs/prdgen_low/jgefs_p20_prdgen_low:pgrb2ap5_f240_ready
    family wave_gwes_p00
      task jwave_gwes_p00_forecast
        trigger ../jwave_gwes_prep == complete
      task jwave_gwes_p00_post
        trigger jwave_gwes_p00_forecast == complete
      task jwave_gwes_p00_gempak
        trigger jwave_gwes_p00_post == complete
    endfamily
    family wave_gwes_p01
      task jwave_gwes_p01_forecast
        trigger ../jwave_gwes_prep == complete
      task jwave_gwes_p01_post
        trigger jwave_gwes_p01_forecast == complete
      task jwave_gwes_p01_gempak
        trigger jwave_gwes_p01_post == complete
    endfamily
    family wave_gwes_p02
      task jwave_gwes_p02_forecast
        trigger ../jwave_gwes_prep == complete
      task jwave_gwes_p02_post
        trigger jwave_gwes_p02_forecast == complete
      task jwave_gwes_p02_gempak
        trigger jwave_gwes_p02_post == complete
    endfamily
    family wave_gwes_p03
      task jwave_gwes_p03_forecast
        trigger ../jwave_gwes_prep == complete
      task jwave_gwes_p03_post
        trigger jwave_gwes_p03_forecast == complete
      task jwave_gwes_p03_gempak
        trigger jwave_gwes_p03_post == complete
    endfamily
    family wave_gwes_p04
      task jwave_gwes_p04_forecast
        trigger ../jwave_gwes_prep == complete
      task jwave_gwes_p04_post
        trigger jwave_gwes_p04_forecast == complete
      task jwave_gwes_p04_gempak
        trigger jwave_gwes_p04_post == complete
    endfamily
    family wave_gwes_p05
      task jwave_gwes_p05_forecast
        trigger ../jwave_gwes_prep == complete
      task jwave_gwes_p05_post
        trigger jwave_gwes_p05_forecast == complete
      task jwave_gwes_p05_gempak
        trigger jwave_gwes_p05_post == complete
    endfamily
    family wave_gwes_p06
      task jwave_gwes_p06_forecast
        trigger ../jwave_gwes_prep == complete
      task jwave_gwes_p06_post
        trigger jwave_gwes_p06_forecast == complete
      task jwave_gwes_p06_gempak
        trigger jwave_gwes_p06_post == complete
    endfamily
    family wave_gwes_p07
      task jwave_gwes_p07_forecast
        trigger ../jwave_gwes_prep == complete
      task jwave_gwes_p07_post
        trigger jwave_gwes_p07_forecast == complete
      task jwave_gwes_p07_gempak
        trigger jwave_gwes_p07_post == complete
    endfamily
    family wave_gwes_p08
      task jwave_gwes_p08_forecast
        trigger ../jwave_gwes_prep == complete
      task jwave_gwes_p08_post
        trigger jwave_gwes_p08_forecast == complete
      task jwave_gwes_p08_gempak
        trigger jwave_gwes_p08_post == complete
    endfamily
    family wave_gwes_p09
      task jwave_gwes_p09_forecast
        trigger ../jwave_gwes_prep == complete
      task jwave_gwes_p09_post
        trigger jwave_gwes_p09_forecast == complete
      task jwave_gwes_p09_gempak
        trigger jwave_gwes_p09_post == complete
    endfamily
    family wave_gwes_p10
      task jwave_gwes_p10_forecast
        trigger ../jwave_gwes_prep == complete
      task jwave_gwes_p10_post
        trigger jwave_gwes_p10_forecast == complete
      task jwave_gwes_p10_gempak
        trigger jwave_gwes_p10_post == complete
    endfamily
    family wave_gwes_p11
      task jwave_gwes_p11_forecast
        trigger ../jwave_gwes_prep == complete
      task jwave_gwes_p11_post
        trigger jwave_gwes_p11_forecast == complete
      task jwave_gwes_p11_gempak
        trigger jwave_gwes_p11_post == complete
    endfamily
    family wave_gwes_p12
      task jwave_gwes_p12_forecast
        trigger ../jwave_gwes_prep == complete
      task jwave_gwes_p12_post
        trigger jwave_gwes_p12_forecast == complete
      task jwave_gwes_p12_gempak
        trigger jwave_gwes_p12_post == complete
    endfamily
    family wave_gwes_p13
      task jwave_gwes_p13_forecast
        trigger ../jwave_gwes_prep == complete
      task jwave_gwes_p13_post
        trigger jwave_gwes_p13_forecast == complete
      task jwave_gwes_p13_gempak
        trigger jwave_gwes_p13_post == complete
    endfamily
    family wave_gwes_p14
      task jwave_gwes_p14_forecast
        trigger ../jwave_gwes_prep == complete
      task jwave_gwes_p14_post
        trigger jwave_gwes_p14_forecast == complete
      task jwave_gwes_p14_gempak
        trigger jwave_gwes_p14_post == complete
    endfamily
    family wave_gwes_p15
      task jwave_gwes_p15_forecast
        trigger ../jwave_gwes_prep == complete
      task jwave_gwes_p15_post
        trigger jwave_gwes_p15_forecast == complete
      task jwave_gwes_p15_gempak
        trigger jwave_gwes_p15_post == complete
    endfamily
    family wave_gwes_p16
      task jwave_gwes_p16_forecast
        trigger ../jwave_gwes_prep == complete
      task jwave_gwes_p16_post
        trigger jwave_gwes_p16_forecast == complete
      task jwave_gwes_p16_gempak
        trigger jwave_gwes_p16_post == complete
    endfamily
    family wave_gwes_p17
      task jwave_gwes_p17_forecast
        trigger ../jwave_gwes_prep == complete
      task jwave_gwes_p17_post
        trigger jwave_gwes_p17_forecast == complete
      task jwave_gwes_p17_gempak
        trigger jwave_gwes_p17_post == complete
    endfamily
    family wave_gwes_p18
      task jwave_gwes_p18_forecast
        trigger ../jwave_gwes_prep == complete
      task jwave_gwes_p18_post
        trigger jwave_gwes_p18_forecast == complete
      task jwave_gwes_p18_gempak
        trigger jwave_gwes_p18_post == complete
    endfamily
    family wave_gwes_p19
      task jwave_gwes_p19_forecast
        trigger ../jwave_gwes_prep == complete
      task jwave_gwes_p19_post
        trigger jwave_gwes_p19_forecast == complete
      task jwave_gwes_p19_gempak
        trigger jwave_gwes_p19_post == complete
    endfamily
    family wave_gwes_p20
      task jwave_gwes_p20_forecast
        trigger ../jwave_gwes_prep == complete
      task jwave_gwes_p20_post
        trigger jwave_gwes_p20_forecast == complete
      task jwave_gwes_p20_gempak
        trigger jwave_gwes_p20_post == complete
    endfamily
    task jwave_gwes_ensemble
      trigger wave_gwes_p00/jwave_gwes_p00_post == complete and wave_gwes_p01/jwave_gwes_p01_post == complete and wave_gwes_p02/jwave_gwes_p02_post == complete and wave_gwes_p03/jwave_gwes_p03_post == complete and wave_gwes_p04/jwave_gwes_p04_post == complete and wave_gwes_p05/jwave_gwes_p05_post == complete and wave_gwes_p06/jwave_gwes_p06_post == complete and wave_gwes_p07/jwave_gwes_p07_post == complete and wave_gwes_p08/jwave_gwes_p08_post == complete and wave_gwes_p09/jwave_gwes_p09_post == complete and wave_gwes_p10/jwave_gwes_p10_post == complete and wave_gwes_p11/jwave_gwes_p11_post == complete and wave_gwes_p12/jwave_gwes_p12_post == complete and wave_gwes_p13/jwave_gwes_p13_post == complete and wave_gwes_p14/jwave_gwes_p14_post == complete and wave_gwes_p15/jwave_gwes_p15_post == complete and wave_gwes_p16/jwave_gwes_p16_post == complete and wave_gwes_p17/jwave_gwes_p17_post == complete and wave_gwes_p18/jwave_gwes_p18_post == complete and wave_gwes_p19/jwave_gwes_p19_post == complete and wave_gwes_p20/jwave_gwes_p20_post == complete
    family wave_nfcens
      task jwave_nfcens_warning
        defstatus complete
      task jwave_nfcens
        trigger ../jwave_gwes_ensemble == complete
        time 18:10
      task jwave_nfcens_gempak
        trigger jwave_nfcens == complete
    endfamily
  endfamily
  family narre12
    family narre_12
      task jnarre_getrap_12
        trigger /prod12/rap/rap_12/prdgen == complete and /prod12/narre12/narre_12/jnarre_getnam_12:release_getrap
      task jnarre_getnam_12
        trigger /prod12/rap/rap_12/prdgen == complete
        event 1 release_getrap
        event 2 release_ensprod
      task jnarre_ensprod_12
        trigger /prod12/narre12/narre_12/jnarre_getnam_12:release_ensprod
    endfamily
    family narre_13
      task jnarre_getrap_13
        trigger /prod12/rap/rap_13/prdgen == complete and /prod12/narre12/narre_13/jnarre_getnam_13:release_getrap
      task jnarre_getnam_13
        trigger /prod12/rap/rap_13/prdgen == complete
        event 1 release_getrap
        event 2 release_ensprod
      task jnarre_ensprod_13
        trigger /prod12/narre12/narre_13/jnarre_getnam_13:release_ensprod
    endfamily
    family narre_14
      task jnarre_getrap_14
        trigger /prod12/rap/rap_14/prdgen == complete and /prod12/narre12/narre_14/jnarre_getnam_14:release_getrap
      task jnarre_getnam_14
        trigger /prod12/rap/rap_14/prdgen == complete
        event 1 release_getrap
        event 2 release_ensprod
      task jnarre_ensprod_14
        trigger /prod12/narre12/narre_14/jnarre_getnam_14:release_ensprod
    endfamily
    family narre_15
      task jnarre_getrap_15
        trigger /prod12/rap/rap_15/prdgen == complete and /prod12/narre12/narre_15/jnarre_getnam_15:release_getrap
      task jnarre_getnam_15
        trigger /prod12/rap/rap_15/prdgen == complete
        event 1 release_getrap
        event 2 release_ensprod
      task jnarre_ensprod_15
        trigger /prod12/narre12/narre_15/jnarre_getnam_15:release_ensprod
    endfamily
    family narre_16
      task jnarre_getrap_16
        trigger /prod12/rap/rap_16/prdgen == complete and /prod12/narre12/narre_16/jnarre_getnam_16:release_getrap
      task jnarre_getnam_16
        trigger /prod12/rap/rap_16/prdgen == complete
        event 1 release_getrap
        event 2 release_ensprod
      task jnarre_ensprod_16
        trigger /prod12/narre12/narre_16/jnarre_getnam_16:release_ensprod
    endfamily
    family narre_17
      task jnarre_getrap_17
        trigger /prod12/rap/rap_17/prdgen == complete and /prod12/narre12/narre_17/jnarre_getnam_17:release_getrap
      task jnarre_getnam_17
        trigger /prod12/rap/rap_17/prdgen == complete
        event 1 release_getrap
        event 2 release_ensprod
      task jnarre_ensprod_17
        trigger /prod12/narre12/narre_17/jnarre_getnam_17:release_ensprod
    endfamily
  endfamily
  family hrrr
    edit ECF_FILES '/ecf/ecfnets/scripts/hrrr'
    edit PROJ 'HRRR-OPS'
    edit QUEUE 'prod2'
    edit COM 'com2'
    family hrrr_12
      edit CYC '12'
      task jhrrr_makeguess
        time 12:24
      task jhrrr_makebc
        time 12:24
      family prep
        task jhrrr_prep_radar_16
          time 12:24
        task jhrrr_prep_radar_30
          time 12:24
        task jhrrr_prep_radar_46
          time 12:24
        task jhrrr_prep_radar_60
          time 12:24
        task jhrrr_prep_cloud
          trigger /prod12/rap/rap_12/dump/jrap_dump_erly == complete
          time 12:32
        task jhrrr_prep_ref2tten
          trigger /prod12/hrrr/hrrr_12/prep/jhrrr_prep_radar_16 == complete and /prod12/hrrr/hrrr_12/prep/jhrrr_prep_radar_30 == complete and /prod12/hrrr/hrrr_12/prep/jhrrr_prep_radar_46 == complete and /prod12/hrrr/hrrr_12/prep/jhrrr_prep_radar_60 == complete and /prod12/hrrr/hrrr_12/jhrrr_makeguess == complete
      endfamily
      task jhrrr_forecastpre
        trigger /prod12/hrrr/hrrr_12/prep/jhrrr_prep_ref2tten == complete
        event 1 cancel_cycle
      task jhrrr_emailwarning
        defstatus complete
      task jhrrr_analysis
        trigger /prod12/hrrr/hrrr_12/jhrrr_forecastpre == complete
      task jhrrr_forecast
        trigger /prod12/hrrr/hrrr_12/jhrrr_makebc == complete and /prod12/hrrr/hrrr_12/jhrrr_analysis == complete
      family post
        task jhrrr_post_manager
          trigger /prod12/hrrr/hrrr_12/jhrrr_forecast == active or /prod12/hrrr/hrrr_12/jhrrr_forecast == complete
          event 1 release_post00
          event 2 release_post01
          event 3 release_post02
          event 4 release_post03
          event 5 release_post04
          event 6 release_post05
          event 7 release_post06
          event 8 release_post07
          event 9 release_post08
          event 10 release_post09
          event 11 release_post10
          event 12 release_post11
          event 13 release_post12
          event 14 release_post13
          event 15 release_post14
          event 16 release_post15
        family post
          task jhrrr_post_f00
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager:release_post00
          task jhrrr_post_f01
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager:release_post01
          task jhrrr_post_f02
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager:release_post02
          task jhrrr_post_f03
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager:release_post03
          task jhrrr_post_f04
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager:release_post04
          task jhrrr_post_f05
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager:release_post05
          task jhrrr_post_f06
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager:release_post06
          task jhrrr_post_f07
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager:release_post07
          task jhrrr_post_f08
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager:release_post08
          task jhrrr_post_f09
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager:release_post09
          task jhrrr_post_f10
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager:release_post10
          task jhrrr_post_f11
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager:release_post11
          task jhrrr_post_f12
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager:release_post12
          task jhrrr_post_f13
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager:release_post13
          task jhrrr_post_f14
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager:release_post14
          task jhrrr_post_f15
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager:release_post15
        endfamily
        family wrfbufr
          task jhrrr_wrfbufr_f00
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager:release_post00
          task jhrrr_wrfbufr_f01
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager:release_post01
          task jhrrr_wrfbufr_f02
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager:release_post02
          task jhrrr_wrfbufr_f03
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager:release_post03
          task jhrrr_wrfbufr_f04
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager:release_post04
          task jhrrr_wrfbufr_f05
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager:release_post05
          task jhrrr_wrfbufr_f06
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager:release_post06
          task jhrrr_wrfbufr_f07
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager:release_post07
          task jhrrr_wrfbufr_f08
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager:release_post08
          task jhrrr_wrfbufr_f09
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager:release_post09
          task jhrrr_wrfbufr_f10
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager:release_post10
          task jhrrr_wrfbufr_f11
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager:release_post11
          task jhrrr_wrfbufr_f12
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager:release_post12
          task jhrrr_wrfbufr_f13
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager:release_post13
          task jhrrr_wrfbufr_f14
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager:release_post14
          task jhrrr_wrfbufr_f15
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager:release_post15
        endfamily
        task jhrrr_post_manager_subh
          trigger /prod12/hrrr/hrrr_12/jhrrr_forecast == active or /prod12/hrrr/hrrr_12/jhrrr_forecast == complete
          event 1 release_post0000
          event 2 release_post0015
          event 3 release_post0030
          event 4 release_post0045
          event 5 release_post0100
          event 6 release_post0115
          event 7 release_post0130
          event 8 release_post0145
          event 9 release_post0200
          event 10 release_post0215
          event 11 release_post0230
          event 12 release_post0245
          event 13 release_post0300
          event 14 release_post0315
          event 15 release_post0330
          event 16 release_post0345
          event 17 release_post0400
          event 18 release_post0415
          event 19 release_post0430
          event 20 release_post0445
          event 21 release_post0500
          event 22 release_post0515
          event 23 release_post0530
          event 24 release_post0545
          event 25 release_post0600
          event 26 release_post0615
          event 27 release_post0630
          event 28 release_post0645
          event 29 release_post0700
          event 30 release_post0715
          event 31 release_post0730
          event 32 release_post0745
          event 33 release_post0800
          event 34 release_post0815
          event 35 release_post0830
          event 36 release_post0845
          event 37 release_post0900
          event 38 release_post0915
          event 39 release_post0930
          event 40 release_post0945
          event 41 release_post1000
          event 42 release_post1015
          event 43 release_post1030
          event 44 release_post1045
          event 45 release_post1100
          event 46 release_post1115
          event 47 release_post1130
          event 48 release_post1145
          event 49 release_post1200
          event 50 release_post1215
          event 51 release_post1230
          event 52 release_post1245
          event 53 release_post1300
          event 54 release_post1315
          event 55 release_post1330
          event 56 release_post1345
          event 57 release_post1400
          event 58 release_post1415
          event 59 release_post1430
          event 60 release_post1445
          event 61 release_post1500
        family post_subh
          task jhrrr_post_f0000
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager_subh:release_post0000
          task jhrrr_post_f0015
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager_subh:release_post0015
          task jhrrr_post_f0030
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager_subh:release_post0030
          task jhrrr_post_f0045
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager_subh:release_post0045
          task jhrrr_post_f0100
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager_subh:release_post0100
          task jhrrr_post_f0115
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager_subh:release_post0115
          task jhrrr_post_f0130
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager_subh:release_post0130
          task jhrrr_post_f0145
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager_subh:release_post0145
          task jhrrr_post_f0200
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager_subh:release_post0200
          task jhrrr_post_f0215
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager_subh:release_post0215
          task jhrrr_post_f0230
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager_subh:release_post0230
          task jhrrr_post_f0245
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager_subh:release_post0245
          task jhrrr_post_f0300
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager_subh:release_post0300
          task jhrrr_post_f0315
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager_subh:release_post0315
          task jhrrr_post_f0330
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager_subh:release_post0330
          task jhrrr_post_f0345
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager_subh:release_post0345
          task jhrrr_post_f0400
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager_subh:release_post0400
          task jhrrr_post_f0415
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager_subh:release_post0415
          task jhrrr_post_f0430
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager_subh:release_post0430
          task jhrrr_post_f0445
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager_subh:release_post0445
          task jhrrr_post_f0500
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager_subh:release_post0500
          task jhrrr_post_f0515
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager_subh:release_post0515
          task jhrrr_post_f0530
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager_subh:release_post0530
          task jhrrr_post_f0545
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager_subh:release_post0545
          task jhrrr_post_f0600
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager_subh:release_post0600
          task jhrrr_post_f0615
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager_subh:release_post0615
          task jhrrr_post_f0630
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager_subh:release_post0630
          task jhrrr_post_f0645
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager_subh:release_post0645
          task jhrrr_post_f0700
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager_subh:release_post0700
          task jhrrr_post_f0715
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager_subh:release_post0715
          task jhrrr_post_f0730
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager_subh:release_post0730
          task jhrrr_post_f0745
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager_subh:release_post0745
          task jhrrr_post_f0800
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager_subh:release_post0800
          task jhrrr_post_f0815
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager_subh:release_post0815
          task jhrrr_post_f0830
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager_subh:release_post0830
          task jhrrr_post_f0845
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager_subh:release_post0845
          task jhrrr_post_f0900
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager_subh:release_post0900
          task jhrrr_post_f0915
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager_subh:release_post0915
          task jhrrr_post_f0930
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager_subh:release_post0930
          task jhrrr_post_f0945
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager_subh:release_post0945
          task jhrrr_post_f1000
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager_subh:release_post1000
          task jhrrr_post_f1015
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager_subh:release_post1015
          task jhrrr_post_f1030
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager_subh:release_post1030
          task jhrrr_post_f1045
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager_subh:release_post1045
          task jhrrr_post_f1100
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager_subh:release_post1100
          task jhrrr_post_f1115
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager_subh:release_post1115
          task jhrrr_post_f1130
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager_subh:release_post1130
          task jhrrr_post_f1145
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager_subh:release_post1145
          task jhrrr_post_f1200
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager_subh:release_post1200
          task jhrrr_post_f1215
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager_subh:release_post1215
          task jhrrr_post_f1230
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager_subh:release_post1230
          task jhrrr_post_f1245
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager_subh:release_post1245
          task jhrrr_post_f1300
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager_subh:release_post1300
          task jhrrr_post_f1315
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager_subh:release_post1315
          task jhrrr_post_f1330
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager_subh:release_post1330
          task jhrrr_post_f1345
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager_subh:release_post1345
          task jhrrr_post_f1400
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager_subh:release_post1400
          task jhrrr_post_f1415
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager_subh:release_post1415
          task jhrrr_post_f1430
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager_subh:release_post1430
          task jhrrr_post_f1445
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager_subh:release_post1445
          task jhrrr_post_f1500
            trigger /prod12/hrrr/hrrr_12/post/jhrrr_post_manager_subh:release_post1500
        endfamily
      endfamily
      task jhrrr_postsnd
        trigger /prod12/hrrr/hrrr_12/post/wrfbufr == complete
      task jhrrr_gempak
        trigger /prod12/hrrr/hrrr_12/post/post/jhrrr_post_f00 == active or /prod12/hrrr/hrrr_12/post/post/jhrrr_post_f00 == complete
      task jhrrr_cleanup
        trigger /prod12/hrrr/hrrr_12/post == complete and /prod12/hrrr/hrrr_12/jhrrr_postsnd == complete
    endfamily
    family hrrr_13
      edit CYC '13'
      task jhrrr_makeguess
        time 13:24
      task jhrrr_makebc
        time 13:24
      family prep
        task jhrrr_prep_radar_16
          time 13:24
        task jhrrr_prep_radar_30
          time 13:24
        task jhrrr_prep_radar_46
          time 13:24
        task jhrrr_prep_radar_60
          time 13:24
        task jhrrr_prep_cloud
          trigger /prod12/rap/rap_13/dump/jrap_dump == complete
          time 13:32
        task jhrrr_prep_ref2tten
          trigger /prod12/hrrr/hrrr_13/prep/jhrrr_prep_radar_16 == complete and /prod12/hrrr/hrrr_13/prep/jhrrr_prep_radar_30 == complete and /prod12/hrrr/hrrr_13/prep/jhrrr_prep_radar_46 == complete and /prod12/hrrr/hrrr_13/prep/jhrrr_prep_radar_60 == complete and /prod12/hrrr/hrrr_13/jhrrr_makeguess == complete
      endfamily
      task jhrrr_forecastpre
        trigger /prod12/hrrr/hrrr_13/prep/jhrrr_prep_ref2tten == complete
        event 1 cancel_cycle
      task jhrrr_emailwarning
        defstatus complete
      task jhrrr_analysis
        trigger /prod12/hrrr/hrrr_13/jhrrr_forecastpre == complete
      task jhrrr_forecast
        trigger /prod12/hrrr/hrrr_13/jhrrr_makebc == complete and /prod12/hrrr/hrrr_13/jhrrr_analysis == complete
      family post
        task jhrrr_post_manager
          trigger /prod12/hrrr/hrrr_13/jhrrr_forecast == active or /prod12/hrrr/hrrr_13/jhrrr_forecast == complete
          event 1 release_post00
          event 2 release_post01
          event 3 release_post02
          event 4 release_post03
          event 5 release_post04
          event 6 release_post05
          event 7 release_post06
          event 8 release_post07
          event 9 release_post08
          event 10 release_post09
          event 11 release_post10
          event 12 release_post11
          event 13 release_post12
          event 14 release_post13
          event 15 release_post14
          event 16 release_post15
        family post
          task jhrrr_post_f00
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager:release_post00
          task jhrrr_post_f01
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager:release_post01
          task jhrrr_post_f02
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager:release_post02
          task jhrrr_post_f03
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager:release_post03
          task jhrrr_post_f04
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager:release_post04
          task jhrrr_post_f05
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager:release_post05
          task jhrrr_post_f06
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager:release_post06
          task jhrrr_post_f07
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager:release_post07
          task jhrrr_post_f08
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager:release_post08
          task jhrrr_post_f09
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager:release_post09
          task jhrrr_post_f10
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager:release_post10
          task jhrrr_post_f11
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager:release_post11
          task jhrrr_post_f12
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager:release_post12
          task jhrrr_post_f13
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager:release_post13
          task jhrrr_post_f14
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager:release_post14
          task jhrrr_post_f15
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager:release_post15
        endfamily
        family wrfbufr
          task jhrrr_wrfbufr_f00
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager:release_post00
          task jhrrr_wrfbufr_f01
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager:release_post01
          task jhrrr_wrfbufr_f02
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager:release_post02
          task jhrrr_wrfbufr_f03
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager:release_post03
          task jhrrr_wrfbufr_f04
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager:release_post04
          task jhrrr_wrfbufr_f05
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager:release_post05
          task jhrrr_wrfbufr_f06
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager:release_post06
          task jhrrr_wrfbufr_f07
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager:release_post07
          task jhrrr_wrfbufr_f08
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager:release_post08
          task jhrrr_wrfbufr_f09
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager:release_post09
          task jhrrr_wrfbufr_f10
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager:release_post10
          task jhrrr_wrfbufr_f11
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager:release_post11
          task jhrrr_wrfbufr_f12
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager:release_post12
          task jhrrr_wrfbufr_f13
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager:release_post13
          task jhrrr_wrfbufr_f14
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager:release_post14
          task jhrrr_wrfbufr_f15
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager:release_post15
        endfamily
        task jhrrr_post_manager_subh
          trigger /prod12/hrrr/hrrr_13/jhrrr_forecast == active or /prod12/hrrr/hrrr_13/jhrrr_forecast == complete
          event 1 release_post0000
          event 2 release_post0015
          event 3 release_post0030
          event 4 release_post0045
          event 5 release_post0100
          event 6 release_post0115
          event 7 release_post0130
          event 8 release_post0145
          event 9 release_post0200
          event 10 release_post0215
          event 11 release_post0230
          event 12 release_post0245
          event 13 release_post0300
          event 14 release_post0315
          event 15 release_post0330
          event 16 release_post0345
          event 17 release_post0400
          event 18 release_post0415
          event 19 release_post0430
          event 20 release_post0445
          event 21 release_post0500
          event 22 release_post0515
          event 23 release_post0530
          event 24 release_post0545
          event 25 release_post0600
          event 26 release_post0615
          event 27 release_post0630
          event 28 release_post0645
          event 29 release_post0700
          event 30 release_post0715
          event 31 release_post0730
          event 32 release_post0745
          event 33 release_post0800
          event 34 release_post0815
          event 35 release_post0830
          event 36 release_post0845
          event 37 release_post0900
          event 38 release_post0915
          event 39 release_post0930
          event 40 release_post0945
          event 41 release_post1000
          event 42 release_post1015
          event 43 release_post1030
          event 44 release_post1045
          event 45 release_post1100
          event 46 release_post1115
          event 47 release_post1130
          event 48 release_post1145
          event 49 release_post1200
          event 50 release_post1215
          event 51 release_post1230
          event 52 release_post1245
          event 53 release_post1300
          event 54 release_post1315
          event 55 release_post1330
          event 56 release_post1345
          event 57 release_post1400
          event 58 release_post1415
          event 59 release_post1430
          event 60 release_post1445
          event 61 release_post1500
        family post_subh
          task jhrrr_post_f0000
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager_subh:release_post0000
          task jhrrr_post_f0015
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager_subh:release_post0015
          task jhrrr_post_f0030
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager_subh:release_post0030
          task jhrrr_post_f0045
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager_subh:release_post0045
          task jhrrr_post_f0100
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager_subh:release_post0100
          task jhrrr_post_f0115
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager_subh:release_post0115
          task jhrrr_post_f0130
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager_subh:release_post0130
          task jhrrr_post_f0145
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager_subh:release_post0145
          task jhrrr_post_f0200
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager_subh:release_post0200
          task jhrrr_post_f0215
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager_subh:release_post0215
          task jhrrr_post_f0230
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager_subh:release_post0230
          task jhrrr_post_f0245
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager_subh:release_post0245
          task jhrrr_post_f0300
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager_subh:release_post0300
          task jhrrr_post_f0315
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager_subh:release_post0315
          task jhrrr_post_f0330
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager_subh:release_post0330
          task jhrrr_post_f0345
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager_subh:release_post0345
          task jhrrr_post_f0400
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager_subh:release_post0400
          task jhrrr_post_f0415
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager_subh:release_post0415
          task jhrrr_post_f0430
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager_subh:release_post0430
          task jhrrr_post_f0445
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager_subh:release_post0445
          task jhrrr_post_f0500
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager_subh:release_post0500
          task jhrrr_post_f0515
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager_subh:release_post0515
          task jhrrr_post_f0530
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager_subh:release_post0530
          task jhrrr_post_f0545
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager_subh:release_post0545
          task jhrrr_post_f0600
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager_subh:release_post0600
          task jhrrr_post_f0615
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager_subh:release_post0615
          task jhrrr_post_f0630
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager_subh:release_post0630
          task jhrrr_post_f0645
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager_subh:release_post0645
          task jhrrr_post_f0700
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager_subh:release_post0700
          task jhrrr_post_f0715
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager_subh:release_post0715
          task jhrrr_post_f0730
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager_subh:release_post0730
          task jhrrr_post_f0745
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager_subh:release_post0745
          task jhrrr_post_f0800
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager_subh:release_post0800
          task jhrrr_post_f0815
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager_subh:release_post0815
          task jhrrr_post_f0830
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager_subh:release_post0830
          task jhrrr_post_f0845
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager_subh:release_post0845
          task jhrrr_post_f0900
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager_subh:release_post0900
          task jhrrr_post_f0915
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager_subh:release_post0915
          task jhrrr_post_f0930
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager_subh:release_post0930
          task jhrrr_post_f0945
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager_subh:release_post0945
          task jhrrr_post_f1000
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager_subh:release_post1000
          task jhrrr_post_f1015
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager_subh:release_post1015
          task jhrrr_post_f1030
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager_subh:release_post1030
          task jhrrr_post_f1045
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager_subh:release_post1045
          task jhrrr_post_f1100
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager_subh:release_post1100
          task jhrrr_post_f1115
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager_subh:release_post1115
          task jhrrr_post_f1130
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager_subh:release_post1130
          task jhrrr_post_f1145
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager_subh:release_post1145
          task jhrrr_post_f1200
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager_subh:release_post1200
          task jhrrr_post_f1215
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager_subh:release_post1215
          task jhrrr_post_f1230
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager_subh:release_post1230
          task jhrrr_post_f1245
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager_subh:release_post1245
          task jhrrr_post_f1300
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager_subh:release_post1300
          task jhrrr_post_f1315
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager_subh:release_post1315
          task jhrrr_post_f1330
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager_subh:release_post1330
          task jhrrr_post_f1345
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager_subh:release_post1345
          task jhrrr_post_f1400
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager_subh:release_post1400
          task jhrrr_post_f1415
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager_subh:release_post1415
          task jhrrr_post_f1430
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager_subh:release_post1430
          task jhrrr_post_f1445
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager_subh:release_post1445
          task jhrrr_post_f1500
            trigger /prod12/hrrr/hrrr_13/post/jhrrr_post_manager_subh:release_post1500
        endfamily
      endfamily
      task jhrrr_postsnd
        trigger /prod12/hrrr/hrrr_13/post/wrfbufr == complete
      task jhrrr_gempak
        trigger /prod12/hrrr/hrrr_13/post/post/jhrrr_post_f00 == active or /prod12/hrrr/hrrr_13/post/post/jhrrr_post_f00 == complete
      task jhrrr_cleanup
        trigger /prod12/hrrr/hrrr_13/post == complete and /prod12/hrrr/hrrr_13/jhrrr_postsnd == complete
    endfamily
    family hrrr_14
      edit CYC '14'
      task jhrrr_makeguess
        time 14:24
      task jhrrr_makebc
        time 14:24
      family prep
        task jhrrr_prep_radar_16
          time 14:24
        task jhrrr_prep_radar_30
          time 14:24
        task jhrrr_prep_radar_46
          time 14:24
        task jhrrr_prep_radar_60
          time 14:24
        task jhrrr_prep_cloud
          trigger /prod12/rap/rap_14/dump/jrap_dump == complete
          time 14:32
        task jhrrr_prep_ref2tten
          trigger /prod12/hrrr/hrrr_14/prep/jhrrr_prep_radar_16 == complete and /prod12/hrrr/hrrr_14/prep/jhrrr_prep_radar_30 == complete and /prod12/hrrr/hrrr_14/prep/jhrrr_prep_radar_46 == complete and /prod12/hrrr/hrrr_14/prep/jhrrr_prep_radar_60 == complete and /prod12/hrrr/hrrr_14/jhrrr_makeguess == complete
      endfamily
      task jhrrr_forecastpre
        trigger /prod12/hrrr/hrrr_14/prep/jhrrr_prep_ref2tten == complete
        event 1 cancel_cycle
      task jhrrr_emailwarning
        defstatus complete
      task jhrrr_analysis
        trigger /prod12/hrrr/hrrr_14/jhrrr_forecastpre == complete
      task jhrrr_forecast
        trigger /prod12/hrrr/hrrr_14/jhrrr_makebc == complete and /prod12/hrrr/hrrr_14/jhrrr_analysis == complete
      family post
        task jhrrr_post_manager
          trigger /prod12/hrrr/hrrr_14/jhrrr_forecast == active or /prod12/hrrr/hrrr_14/jhrrr_forecast == complete
          event 1 release_post00
          event 2 release_post01
          event 3 release_post02
          event 4 release_post03
          event 5 release_post04
          event 6 release_post05
          event 7 release_post06
          event 8 release_post07
          event 9 release_post08
          event 10 release_post09
          event 11 release_post10
          event 12 release_post11
          event 13 release_post12
          event 14 release_post13
          event 15 release_post14
          event 16 release_post15
        family post
          task jhrrr_post_f00
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager:release_post00
          task jhrrr_post_f01
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager:release_post01
          task jhrrr_post_f02
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager:release_post02
          task jhrrr_post_f03
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager:release_post03
          task jhrrr_post_f04
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager:release_post04
          task jhrrr_post_f05
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager:release_post05
          task jhrrr_post_f06
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager:release_post06
          task jhrrr_post_f07
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager:release_post07
          task jhrrr_post_f08
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager:release_post08
          task jhrrr_post_f09
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager:release_post09
          task jhrrr_post_f10
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager:release_post10
          task jhrrr_post_f11
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager:release_post11
          task jhrrr_post_f12
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager:release_post12
          task jhrrr_post_f13
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager:release_post13
          task jhrrr_post_f14
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager:release_post14
          task jhrrr_post_f15
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager:release_post15
        endfamily
        family wrfbufr
          task jhrrr_wrfbufr_f00
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager:release_post00
          task jhrrr_wrfbufr_f01
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager:release_post01
          task jhrrr_wrfbufr_f02
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager:release_post02
          task jhrrr_wrfbufr_f03
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager:release_post03
          task jhrrr_wrfbufr_f04
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager:release_post04
          task jhrrr_wrfbufr_f05
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager:release_post05
          task jhrrr_wrfbufr_f06
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager:release_post06
          task jhrrr_wrfbufr_f07
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager:release_post07
          task jhrrr_wrfbufr_f08
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager:release_post08
          task jhrrr_wrfbufr_f09
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager:release_post09
          task jhrrr_wrfbufr_f10
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager:release_post10
          task jhrrr_wrfbufr_f11
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager:release_post11
          task jhrrr_wrfbufr_f12
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager:release_post12
          task jhrrr_wrfbufr_f13
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager:release_post13
          task jhrrr_wrfbufr_f14
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager:release_post14
          task jhrrr_wrfbufr_f15
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager:release_post15
        endfamily
        task jhrrr_post_manager_subh
          trigger /prod12/hrrr/hrrr_14/jhrrr_forecast == active or /prod12/hrrr/hrrr_14/jhrrr_forecast == complete
          event 1 release_post0000
          event 2 release_post0015
          event 3 release_post0030
          event 4 release_post0045
          event 5 release_post0100
          event 6 release_post0115
          event 7 release_post0130
          event 8 release_post0145
          event 9 release_post0200
          event 10 release_post0215
          event 11 release_post0230
          event 12 release_post0245
          event 13 release_post0300
          event 14 release_post0315
          event 15 release_post0330
          event 16 release_post0345
          event 17 release_post0400
          event 18 release_post0415
          event 19 release_post0430
          event 20 release_post0445
          event 21 release_post0500
          event 22 release_post0515
          event 23 release_post0530
          event 24 release_post0545
          event 25 release_post0600
          event 26 release_post0615
          event 27 release_post0630
          event 28 release_post0645
          event 29 release_post0700
          event 30 release_post0715
          event 31 release_post0730
          event 32 release_post0745
          event 33 release_post0800
          event 34 release_post0815
          event 35 release_post0830
          event 36 release_post0845
          event 37 release_post0900
          event 38 release_post0915
          event 39 release_post0930
          event 40 release_post0945
          event 41 release_post1000
          event 42 release_post1015
          event 43 release_post1030
          event 44 release_post1045
          event 45 release_post1100
          event 46 release_post1115
          event 47 release_post1130
          event 48 release_post1145
          event 49 release_post1200
          event 50 release_post1215
          event 51 release_post1230
          event 52 release_post1245
          event 53 release_post1300
          event 54 release_post1315
          event 55 release_post1330
          event 56 release_post1345
          event 57 release_post1400
          event 58 release_post1415
          event 59 release_post1430
          event 60 release_post1445
          event 61 release_post1500
        family post_subh
          task jhrrr_post_f0000
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager_subh:release_post0000
          task jhrrr_post_f0015
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager_subh:release_post0015
          task jhrrr_post_f0030
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager_subh:release_post0030
          task jhrrr_post_f0045
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager_subh:release_post0045
          task jhrrr_post_f0100
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager_subh:release_post0100
          task jhrrr_post_f0115
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager_subh:release_post0115
          task jhrrr_post_f0130
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager_subh:release_post0130
          task jhrrr_post_f0145
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager_subh:release_post0145
          task jhrrr_post_f0200
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager_subh:release_post0200
          task jhrrr_post_f0215
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager_subh:release_post0215
          task jhrrr_post_f0230
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager_subh:release_post0230
          task jhrrr_post_f0245
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager_subh:release_post0245
          task jhrrr_post_f0300
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager_subh:release_post0300
          task jhrrr_post_f0315
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager_subh:release_post0315
          task jhrrr_post_f0330
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager_subh:release_post0330
          task jhrrr_post_f0345
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager_subh:release_post0345
          task jhrrr_post_f0400
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager_subh:release_post0400
          task jhrrr_post_f0415
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager_subh:release_post0415
          task jhrrr_post_f0430
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager_subh:release_post0430
          task jhrrr_post_f0445
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager_subh:release_post0445
          task jhrrr_post_f0500
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager_subh:release_post0500
          task jhrrr_post_f0515
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager_subh:release_post0515
          task jhrrr_post_f0530
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager_subh:release_post0530
          task jhrrr_post_f0545
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager_subh:release_post0545
          task jhrrr_post_f0600
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager_subh:release_post0600
          task jhrrr_post_f0615
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager_subh:release_post0615
          task jhrrr_post_f0630
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager_subh:release_post0630
          task jhrrr_post_f0645
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager_subh:release_post0645
          task jhrrr_post_f0700
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager_subh:release_post0700
          task jhrrr_post_f0715
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager_subh:release_post0715
          task jhrrr_post_f0730
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager_subh:release_post0730
          task jhrrr_post_f0745
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager_subh:release_post0745
          task jhrrr_post_f0800
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager_subh:release_post0800
          task jhrrr_post_f0815
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager_subh:release_post0815
          task jhrrr_post_f0830
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager_subh:release_post0830
          task jhrrr_post_f0845
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager_subh:release_post0845
          task jhrrr_post_f0900
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager_subh:release_post0900
          task jhrrr_post_f0915
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager_subh:release_post0915
          task jhrrr_post_f0930
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager_subh:release_post0930
          task jhrrr_post_f0945
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager_subh:release_post0945
          task jhrrr_post_f1000
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager_subh:release_post1000
          task jhrrr_post_f1015
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager_subh:release_post1015
          task jhrrr_post_f1030
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager_subh:release_post1030
          task jhrrr_post_f1045
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager_subh:release_post1045
          task jhrrr_post_f1100
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager_subh:release_post1100
          task jhrrr_post_f1115
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager_subh:release_post1115
          task jhrrr_post_f1130
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager_subh:release_post1130
          task jhrrr_post_f1145
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager_subh:release_post1145
          task jhrrr_post_f1200
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager_subh:release_post1200
          task jhrrr_post_f1215
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager_subh:release_post1215
          task jhrrr_post_f1230
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager_subh:release_post1230
          task jhrrr_post_f1245
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager_subh:release_post1245
          task jhrrr_post_f1300
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager_subh:release_post1300
          task jhrrr_post_f1315
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager_subh:release_post1315
          task jhrrr_post_f1330
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager_subh:release_post1330
          task jhrrr_post_f1345
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager_subh:release_post1345
          task jhrrr_post_f1400
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager_subh:release_post1400
          task jhrrr_post_f1415
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager_subh:release_post1415
          task jhrrr_post_f1430
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager_subh:release_post1430
          task jhrrr_post_f1445
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager_subh:release_post1445
          task jhrrr_post_f1500
            trigger /prod12/hrrr/hrrr_14/post/jhrrr_post_manager_subh:release_post1500
        endfamily
      endfamily
      task jhrrr_postsnd
        trigger /prod12/hrrr/hrrr_14/post/wrfbufr == complete
      task jhrrr_gempak
        trigger /prod12/hrrr/hrrr_14/post/post/jhrrr_post_f00 == active or /prod12/hrrr/hrrr_14/post/post/jhrrr_post_f00 == complete
      task jhrrr_cleanup
        trigger /prod12/hrrr/hrrr_14/post == complete and /prod12/hrrr/hrrr_14/jhrrr_postsnd == complete
    endfamily
    family hrrr_15
      edit CYC '15'
      task jhrrr_makeguess
        time 15:24
      task jhrrr_makebc
        time 15:24
      family prep
        task jhrrr_prep_radar_16
          time 15:24
        task jhrrr_prep_radar_30
          time 15:24
        task jhrrr_prep_radar_46
          time 15:24
        task jhrrr_prep_radar_60
          time 15:24
        task jhrrr_prep_cloud
          trigger /prod12/rap/rap_15/dump/jrap_dump == complete
          time 15:32
        task jhrrr_prep_ref2tten
          trigger /prod12/hrrr/hrrr_15/prep/jhrrr_prep_radar_16 == complete and /prod12/hrrr/hrrr_15/prep/jhrrr_prep_radar_30 == complete and /prod12/hrrr/hrrr_15/prep/jhrrr_prep_radar_46 == complete and /prod12/hrrr/hrrr_15/prep/jhrrr_prep_radar_60 == complete and /prod12/hrrr/hrrr_15/jhrrr_makeguess == complete
      endfamily
      task jhrrr_forecastpre
        trigger /prod12/hrrr/hrrr_15/prep/jhrrr_prep_ref2tten == complete
        event 1 cancel_cycle
      task jhrrr_emailwarning
        defstatus complete
      task jhrrr_analysis
        trigger /prod12/hrrr/hrrr_15/jhrrr_forecastpre == complete
      task jhrrr_forecast
        trigger /prod12/hrrr/hrrr_15/jhrrr_makebc == complete and /prod12/hrrr/hrrr_15/jhrrr_analysis == complete
      family post
        task jhrrr_post_manager
          trigger /prod12/hrrr/hrrr_15/jhrrr_forecast == active or /prod12/hrrr/hrrr_15/jhrrr_forecast == complete
          event 1 release_post00
          event 2 release_post01
          event 3 release_post02
          event 4 release_post03
          event 5 release_post04
          event 6 release_post05
          event 7 release_post06
          event 8 release_post07
          event 9 release_post08
          event 10 release_post09
          event 11 release_post10
          event 12 release_post11
          event 13 release_post12
          event 14 release_post13
          event 15 release_post14
          event 16 release_post15
        family post
          task jhrrr_post_f00
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager:release_post00
          task jhrrr_post_f01
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager:release_post01
          task jhrrr_post_f02
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager:release_post02
          task jhrrr_post_f03
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager:release_post03
          task jhrrr_post_f04
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager:release_post04
          task jhrrr_post_f05
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager:release_post05
          task jhrrr_post_f06
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager:release_post06
          task jhrrr_post_f07
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager:release_post07
          task jhrrr_post_f08
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager:release_post08
          task jhrrr_post_f09
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager:release_post09
          task jhrrr_post_f10
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager:release_post10
          task jhrrr_post_f11
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager:release_post11
          task jhrrr_post_f12
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager:release_post12
          task jhrrr_post_f13
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager:release_post13
          task jhrrr_post_f14
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager:release_post14
          task jhrrr_post_f15
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager:release_post15
        endfamily
        family wrfbufr
          task jhrrr_wrfbufr_f00
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager:release_post00
          task jhrrr_wrfbufr_f01
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager:release_post01
          task jhrrr_wrfbufr_f02
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager:release_post02
          task jhrrr_wrfbufr_f03
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager:release_post03
          task jhrrr_wrfbufr_f04
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager:release_post04
          task jhrrr_wrfbufr_f05
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager:release_post05
          task jhrrr_wrfbufr_f06
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager:release_post06
          task jhrrr_wrfbufr_f07
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager:release_post07
          task jhrrr_wrfbufr_f08
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager:release_post08
          task jhrrr_wrfbufr_f09
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager:release_post09
          task jhrrr_wrfbufr_f10
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager:release_post10
          task jhrrr_wrfbufr_f11
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager:release_post11
          task jhrrr_wrfbufr_f12
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager:release_post12
          task jhrrr_wrfbufr_f13
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager:release_post13
          task jhrrr_wrfbufr_f14
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager:release_post14
          task jhrrr_wrfbufr_f15
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager:release_post15
        endfamily
        task jhrrr_post_manager_subh
          trigger /prod12/hrrr/hrrr_15/jhrrr_forecast == active or /prod12/hrrr/hrrr_15/jhrrr_forecast == complete
          event 1 release_post0000
          event 2 release_post0015
          event 3 release_post0030
          event 4 release_post0045
          event 5 release_post0100
          event 6 release_post0115
          event 7 release_post0130
          event 8 release_post0145
          event 9 release_post0200
          event 10 release_post0215
          event 11 release_post0230
          event 12 release_post0245
          event 13 release_post0300
          event 14 release_post0315
          event 15 release_post0330
          event 16 release_post0345
          event 17 release_post0400
          event 18 release_post0415
          event 19 release_post0430
          event 20 release_post0445
          event 21 release_post0500
          event 22 release_post0515
          event 23 release_post0530
          event 24 release_post0545
          event 25 release_post0600
          event 26 release_post0615
          event 27 release_post0630
          event 28 release_post0645
          event 29 release_post0700
          event 30 release_post0715
          event 31 release_post0730
          event 32 release_post0745
          event 33 release_post0800
          event 34 release_post0815
          event 35 release_post0830
          event 36 release_post0845
          event 37 release_post0900
          event 38 release_post0915
          event 39 release_post0930
          event 40 release_post0945
          event 41 release_post1000
          event 42 release_post1015
          event 43 release_post1030
          event 44 release_post1045
          event 45 release_post1100
          event 46 release_post1115
          event 47 release_post1130
          event 48 release_post1145
          event 49 release_post1200
          event 50 release_post1215
          event 51 release_post1230
          event 52 release_post1245
          event 53 release_post1300
          event 54 release_post1315
          event 55 release_post1330
          event 56 release_post1345
          event 57 release_post1400
          event 58 release_post1415
          event 59 release_post1430
          event 60 release_post1445
          event 61 release_post1500
        family post_subh
          task jhrrr_post_f0000
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager_subh:release_post0000
          task jhrrr_post_f0015
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager_subh:release_post0015
          task jhrrr_post_f0030
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager_subh:release_post0030
          task jhrrr_post_f0045
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager_subh:release_post0045
          task jhrrr_post_f0100
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager_subh:release_post0100
          task jhrrr_post_f0115
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager_subh:release_post0115
          task jhrrr_post_f0130
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager_subh:release_post0130
          task jhrrr_post_f0145
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager_subh:release_post0145
          task jhrrr_post_f0200
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager_subh:release_post0200
          task jhrrr_post_f0215
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager_subh:release_post0215
          task jhrrr_post_f0230
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager_subh:release_post0230
          task jhrrr_post_f0245
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager_subh:release_post0245
          task jhrrr_post_f0300
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager_subh:release_post0300
          task jhrrr_post_f0315
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager_subh:release_post0315
          task jhrrr_post_f0330
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager_subh:release_post0330
          task jhrrr_post_f0345
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager_subh:release_post0345
          task jhrrr_post_f0400
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager_subh:release_post0400
          task jhrrr_post_f0415
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager_subh:release_post0415
          task jhrrr_post_f0430
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager_subh:release_post0430
          task jhrrr_post_f0445
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager_subh:release_post0445
          task jhrrr_post_f0500
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager_subh:release_post0500
          task jhrrr_post_f0515
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager_subh:release_post0515
          task jhrrr_post_f0530
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager_subh:release_post0530
          task jhrrr_post_f0545
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager_subh:release_post0545
          task jhrrr_post_f0600
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager_subh:release_post0600
          task jhrrr_post_f0615
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager_subh:release_post0615
          task jhrrr_post_f0630
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager_subh:release_post0630
          task jhrrr_post_f0645
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager_subh:release_post0645
          task jhrrr_post_f0700
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager_subh:release_post0700
          task jhrrr_post_f0715
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager_subh:release_post0715
          task jhrrr_post_f0730
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager_subh:release_post0730
          task jhrrr_post_f0745
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager_subh:release_post0745
          task jhrrr_post_f0800
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager_subh:release_post0800
          task jhrrr_post_f0815
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager_subh:release_post0815
          task jhrrr_post_f0830
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager_subh:release_post0830
          task jhrrr_post_f0845
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager_subh:release_post0845
          task jhrrr_post_f0900
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager_subh:release_post0900
          task jhrrr_post_f0915
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager_subh:release_post0915
          task jhrrr_post_f0930
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager_subh:release_post0930
          task jhrrr_post_f0945
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager_subh:release_post0945
          task jhrrr_post_f1000
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager_subh:release_post1000
          task jhrrr_post_f1015
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager_subh:release_post1015
          task jhrrr_post_f1030
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager_subh:release_post1030
          task jhrrr_post_f1045
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager_subh:release_post1045
          task jhrrr_post_f1100
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager_subh:release_post1100
          task jhrrr_post_f1115
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager_subh:release_post1115
          task jhrrr_post_f1130
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager_subh:release_post1130
          task jhrrr_post_f1145
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager_subh:release_post1145
          task jhrrr_post_f1200
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager_subh:release_post1200
          task jhrrr_post_f1215
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager_subh:release_post1215
          task jhrrr_post_f1230
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager_subh:release_post1230
          task jhrrr_post_f1245
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager_subh:release_post1245
          task jhrrr_post_f1300
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager_subh:release_post1300
          task jhrrr_post_f1315
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager_subh:release_post1315
          task jhrrr_post_f1330
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager_subh:release_post1330
          task jhrrr_post_f1345
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager_subh:release_post1345
          task jhrrr_post_f1400
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager_subh:release_post1400
          task jhrrr_post_f1415
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager_subh:release_post1415
          task jhrrr_post_f1430
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager_subh:release_post1430
          task jhrrr_post_f1445
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager_subh:release_post1445
          task jhrrr_post_f1500
            trigger /prod12/hrrr/hrrr_15/post/jhrrr_post_manager_subh:release_post1500
        endfamily
      endfamily
      task jhrrr_postsnd
        trigger /prod12/hrrr/hrrr_15/post/wrfbufr == complete
      task jhrrr_gempak
        trigger /prod12/hrrr/hrrr_15/post/post/jhrrr_post_f00 == active or /prod12/hrrr/hrrr_15/post/post/jhrrr_post_f00 == complete
      task jhrrr_cleanup
        trigger /prod12/hrrr/hrrr_15/post == complete and /prod12/hrrr/hrrr_15/jhrrr_postsnd == complete
    endfamily
    family hrrr_16
      edit CYC '16'
      task jhrrr_makeguess
        time 16:24
      task jhrrr_makebc
        time 16:24
      family prep
        task jhrrr_prep_radar_16
          time 16:24
        task jhrrr_prep_radar_30
          time 16:24
        task jhrrr_prep_radar_46
          time 16:24
        task jhrrr_prep_radar_60
          time 16:24
        task jhrrr_prep_cloud
          trigger /prod12/rap/rap_16/dump/jrap_dump == complete
          time 16:32
        task jhrrr_prep_ref2tten
          trigger /prod12/hrrr/hrrr_16/prep/jhrrr_prep_radar_16 == complete and /prod12/hrrr/hrrr_16/prep/jhrrr_prep_radar_30 == complete and /prod12/hrrr/hrrr_16/prep/jhrrr_prep_radar_46 == complete and /prod12/hrrr/hrrr_16/prep/jhrrr_prep_radar_60 == complete and /prod12/hrrr/hrrr_16/jhrrr_makeguess == complete
      endfamily
      task jhrrr_forecastpre
        trigger /prod12/hrrr/hrrr_16/prep/jhrrr_prep_ref2tten == complete
        event 1 cancel_cycle
      task jhrrr_emailwarning
        defstatus complete
      task jhrrr_analysis
        trigger /prod12/hrrr/hrrr_16/jhrrr_forecastpre == complete
      task jhrrr_forecast
        trigger /prod12/hrrr/hrrr_16/jhrrr_makebc == complete and /prod12/hrrr/hrrr_16/jhrrr_analysis == complete
      family post
        task jhrrr_post_manager
          trigger /prod12/hrrr/hrrr_16/jhrrr_forecast == active or /prod12/hrrr/hrrr_16/jhrrr_forecast == complete
          event 1 release_post00
          event 2 release_post01
          event 3 release_post02
          event 4 release_post03
          event 5 release_post04
          event 6 release_post05
          event 7 release_post06
          event 8 release_post07
          event 9 release_post08
          event 10 release_post09
          event 11 release_post10
          event 12 release_post11
          event 13 release_post12
          event 14 release_post13
          event 15 release_post14
          event 16 release_post15
        family post
          task jhrrr_post_f00
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager:release_post00
          task jhrrr_post_f01
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager:release_post01
          task jhrrr_post_f02
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager:release_post02
          task jhrrr_post_f03
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager:release_post03
          task jhrrr_post_f04
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager:release_post04
          task jhrrr_post_f05
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager:release_post05
          task jhrrr_post_f06
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager:release_post06
          task jhrrr_post_f07
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager:release_post07
          task jhrrr_post_f08
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager:release_post08
          task jhrrr_post_f09
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager:release_post09
          task jhrrr_post_f10
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager:release_post10
          task jhrrr_post_f11
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager:release_post11
          task jhrrr_post_f12
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager:release_post12
          task jhrrr_post_f13
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager:release_post13
          task jhrrr_post_f14
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager:release_post14
          task jhrrr_post_f15
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager:release_post15
        endfamily
        family wrfbufr
          task jhrrr_wrfbufr_f00
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager:release_post00
          task jhrrr_wrfbufr_f01
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager:release_post01
          task jhrrr_wrfbufr_f02
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager:release_post02
          task jhrrr_wrfbufr_f03
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager:release_post03
          task jhrrr_wrfbufr_f04
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager:release_post04
          task jhrrr_wrfbufr_f05
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager:release_post05
          task jhrrr_wrfbufr_f06
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager:release_post06
          task jhrrr_wrfbufr_f07
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager:release_post07
          task jhrrr_wrfbufr_f08
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager:release_post08
          task jhrrr_wrfbufr_f09
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager:release_post09
          task jhrrr_wrfbufr_f10
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager:release_post10
          task jhrrr_wrfbufr_f11
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager:release_post11
          task jhrrr_wrfbufr_f12
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager:release_post12
          task jhrrr_wrfbufr_f13
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager:release_post13
          task jhrrr_wrfbufr_f14
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager:release_post14
          task jhrrr_wrfbufr_f15
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager:release_post15
        endfamily
        task jhrrr_post_manager_subh
          trigger /prod12/hrrr/hrrr_16/jhrrr_forecast == active or /prod12/hrrr/hrrr_16/jhrrr_forecast == complete
          event 1 release_post0000
          event 2 release_post0015
          event 3 release_post0030
          event 4 release_post0045
          event 5 release_post0100
          event 6 release_post0115
          event 7 release_post0130
          event 8 release_post0145
          event 9 release_post0200
          event 10 release_post0215
          event 11 release_post0230
          event 12 release_post0245
          event 13 release_post0300
          event 14 release_post0315
          event 15 release_post0330
          event 16 release_post0345
          event 17 release_post0400
          event 18 release_post0415
          event 19 release_post0430
          event 20 release_post0445
          event 21 release_post0500
          event 22 release_post0515
          event 23 release_post0530
          event 24 release_post0545
          event 25 release_post0600
          event 26 release_post0615
          event 27 release_post0630
          event 28 release_post0645
          event 29 release_post0700
          event 30 release_post0715
          event 31 release_post0730
          event 32 release_post0745
          event 33 release_post0800
          event 34 release_post0815
          event 35 release_post0830
          event 36 release_post0845
          event 37 release_post0900
          event 38 release_post0915
          event 39 release_post0930
          event 40 release_post0945
          event 41 release_post1000
          event 42 release_post1015
          event 43 release_post1030
          event 44 release_post1045
          event 45 release_post1100
          event 46 release_post1115
          event 47 release_post1130
          event 48 release_post1145
          event 49 release_post1200
          event 50 release_post1215
          event 51 release_post1230
          event 52 release_post1245
          event 53 release_post1300
          event 54 release_post1315
          event 55 release_post1330
          event 56 release_post1345
          event 57 release_post1400
          event 58 release_post1415
          event 59 release_post1430
          event 60 release_post1445
          event 61 release_post1500
        family post_subh
          task jhrrr_post_f0000
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager_subh:release_post0000
          task jhrrr_post_f0015
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager_subh:release_post0015
          task jhrrr_post_f0030
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager_subh:release_post0030
          task jhrrr_post_f0045
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager_subh:release_post0045
          task jhrrr_post_f0100
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager_subh:release_post0100
          task jhrrr_post_f0115
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager_subh:release_post0115
          task jhrrr_post_f0130
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager_subh:release_post0130
          task jhrrr_post_f0145
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager_subh:release_post0145
          task jhrrr_post_f0200
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager_subh:release_post0200
          task jhrrr_post_f0215
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager_subh:release_post0215
          task jhrrr_post_f0230
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager_subh:release_post0230
          task jhrrr_post_f0245
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager_subh:release_post0245
          task jhrrr_post_f0300
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager_subh:release_post0300
          task jhrrr_post_f0315
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager_subh:release_post0315
          task jhrrr_post_f0330
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager_subh:release_post0330
          task jhrrr_post_f0345
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager_subh:release_post0345
          task jhrrr_post_f0400
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager_subh:release_post0400
          task jhrrr_post_f0415
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager_subh:release_post0415
          task jhrrr_post_f0430
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager_subh:release_post0430
          task jhrrr_post_f0445
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager_subh:release_post0445
          task jhrrr_post_f0500
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager_subh:release_post0500
          task jhrrr_post_f0515
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager_subh:release_post0515
          task jhrrr_post_f0530
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager_subh:release_post0530
          task jhrrr_post_f0545
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager_subh:release_post0545
          task jhrrr_post_f0600
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager_subh:release_post0600
          task jhrrr_post_f0615
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager_subh:release_post0615
          task jhrrr_post_f0630
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager_subh:release_post0630
          task jhrrr_post_f0645
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager_subh:release_post0645
          task jhrrr_post_f0700
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager_subh:release_post0700
          task jhrrr_post_f0715
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager_subh:release_post0715
          task jhrrr_post_f0730
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager_subh:release_post0730
          task jhrrr_post_f0745
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager_subh:release_post0745
          task jhrrr_post_f0800
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager_subh:release_post0800
          task jhrrr_post_f0815
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager_subh:release_post0815
          task jhrrr_post_f0830
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager_subh:release_post0830
          task jhrrr_post_f0845
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager_subh:release_post0845
          task jhrrr_post_f0900
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager_subh:release_post0900
          task jhrrr_post_f0915
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager_subh:release_post0915
          task jhrrr_post_f0930
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager_subh:release_post0930
          task jhrrr_post_f0945
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager_subh:release_post0945
          task jhrrr_post_f1000
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager_subh:release_post1000
          task jhrrr_post_f1015
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager_subh:release_post1015
          task jhrrr_post_f1030
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager_subh:release_post1030
          task jhrrr_post_f1045
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager_subh:release_post1045
          task jhrrr_post_f1100
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager_subh:release_post1100
          task jhrrr_post_f1115
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager_subh:release_post1115
          task jhrrr_post_f1130
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager_subh:release_post1130
          task jhrrr_post_f1145
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager_subh:release_post1145
          task jhrrr_post_f1200
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager_subh:release_post1200
          task jhrrr_post_f1215
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager_subh:release_post1215
          task jhrrr_post_f1230
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager_subh:release_post1230
          task jhrrr_post_f1245
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager_subh:release_post1245
          task jhrrr_post_f1300
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager_subh:release_post1300
          task jhrrr_post_f1315
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager_subh:release_post1315
          task jhrrr_post_f1330
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager_subh:release_post1330
          task jhrrr_post_f1345
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager_subh:release_post1345
          task jhrrr_post_f1400
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager_subh:release_post1400
          task jhrrr_post_f1415
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager_subh:release_post1415
          task jhrrr_post_f1430
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager_subh:release_post1430
          task jhrrr_post_f1445
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager_subh:release_post1445
          task jhrrr_post_f1500
            trigger /prod12/hrrr/hrrr_16/post/jhrrr_post_manager_subh:release_post1500
        endfamily
      endfamily
      task jhrrr_postsnd
        trigger /prod12/hrrr/hrrr_16/post/wrfbufr == complete
      task jhrrr_gempak
        trigger /prod12/hrrr/hrrr_16/post/post/jhrrr_post_f00 == active or /prod12/hrrr/hrrr_16/post/post/jhrrr_post_f00 == complete
      task jhrrr_cleanup
        trigger /prod12/hrrr/hrrr_16/post == complete and /prod12/hrrr/hrrr_16/jhrrr_postsnd == complete
    endfamily
    family hrrr_17
      edit CYC '17'
      task jhrrr_makeguess
        time 17:24
      task jhrrr_makebc
        time 17:24
      family prep
        task jhrrr_prep_radar_16
          time 17:24
        task jhrrr_prep_radar_30
          time 17:24
        task jhrrr_prep_radar_46
          time 17:24
        task jhrrr_prep_radar_60
          time 17:24
        task jhrrr_prep_cloud
          trigger /prod12/rap/rap_17/dump/jrap_dump == complete
          time 17:32
        task jhrrr_prep_ref2tten
          trigger /prod12/hrrr/hrrr_17/prep/jhrrr_prep_radar_16 == complete and /prod12/hrrr/hrrr_17/prep/jhrrr_prep_radar_30 == complete and /prod12/hrrr/hrrr_17/prep/jhrrr_prep_radar_46 == complete and /prod12/hrrr/hrrr_17/prep/jhrrr_prep_radar_60 == complete and /prod12/hrrr/hrrr_17/jhrrr_makeguess == complete
      endfamily
      task jhrrr_forecastpre
        trigger /prod12/hrrr/hrrr_17/prep/jhrrr_prep_ref2tten == complete
        event 1 cancel_cycle
      task jhrrr_emailwarning
        defstatus complete
      task jhrrr_analysis
        trigger /prod12/hrrr/hrrr_17/jhrrr_forecastpre == complete
      task jhrrr_forecast
        trigger /prod12/hrrr/hrrr_17/jhrrr_makebc == complete and /prod12/hrrr/hrrr_17/jhrrr_analysis == complete
      family post
        task jhrrr_post_manager
          trigger /prod12/hrrr/hrrr_17/jhrrr_forecast == active or /prod12/hrrr/hrrr_17/jhrrr_forecast == complete
          event 1 release_post00
          event 2 release_post01
          event 3 release_post02
          event 4 release_post03
          event 5 release_post04
          event 6 release_post05
          event 7 release_post06
          event 8 release_post07
          event 9 release_post08
          event 10 release_post09
          event 11 release_post10
          event 12 release_post11
          event 13 release_post12
          event 14 release_post13
          event 15 release_post14
          event 16 release_post15
        family post
          task jhrrr_post_f00
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager:release_post00
          task jhrrr_post_f01
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager:release_post01
          task jhrrr_post_f02
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager:release_post02
          task jhrrr_post_f03
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager:release_post03
          task jhrrr_post_f04
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager:release_post04
          task jhrrr_post_f05
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager:release_post05
          task jhrrr_post_f06
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager:release_post06
          task jhrrr_post_f07
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager:release_post07
          task jhrrr_post_f08
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager:release_post08
          task jhrrr_post_f09
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager:release_post09
          task jhrrr_post_f10
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager:release_post10
          task jhrrr_post_f11
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager:release_post11
          task jhrrr_post_f12
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager:release_post12
          task jhrrr_post_f13
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager:release_post13
          task jhrrr_post_f14
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager:release_post14
          task jhrrr_post_f15
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager:release_post15
        endfamily
        family wrfbufr
          task jhrrr_wrfbufr_f00
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager:release_post00
          task jhrrr_wrfbufr_f01
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager:release_post01
          task jhrrr_wrfbufr_f02
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager:release_post02
          task jhrrr_wrfbufr_f03
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager:release_post03
          task jhrrr_wrfbufr_f04
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager:release_post04
          task jhrrr_wrfbufr_f05
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager:release_post05
          task jhrrr_wrfbufr_f06
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager:release_post06
          task jhrrr_wrfbufr_f07
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager:release_post07
          task jhrrr_wrfbufr_f08
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager:release_post08
          task jhrrr_wrfbufr_f09
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager:release_post09
          task jhrrr_wrfbufr_f10
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager:release_post10
          task jhrrr_wrfbufr_f11
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager:release_post11
          task jhrrr_wrfbufr_f12
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager:release_post12
          task jhrrr_wrfbufr_f13
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager:release_post13
          task jhrrr_wrfbufr_f14
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager:release_post14
          task jhrrr_wrfbufr_f15
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager:release_post15
        endfamily
        task jhrrr_post_manager_subh
          trigger /prod12/hrrr/hrrr_17/jhrrr_forecast == active or /prod12/hrrr/hrrr_17/jhrrr_forecast == complete
          event 1 release_post0000
          event 2 release_post0015
          event 3 release_post0030
          event 4 release_post0045
          event 5 release_post0100
          event 6 release_post0115
          event 7 release_post0130
          event 8 release_post0145
          event 9 release_post0200
          event 10 release_post0215
          event 11 release_post0230
          event 12 release_post0245
          event 13 release_post0300
          event 14 release_post0315
          event 15 release_post0330
          event 16 release_post0345
          event 17 release_post0400
          event 18 release_post0415
          event 19 release_post0430
          event 20 release_post0445
          event 21 release_post0500
          event 22 release_post0515
          event 23 release_post0530
          event 24 release_post0545
          event 25 release_post0600
          event 26 release_post0615
          event 27 release_post0630
          event 28 release_post0645
          event 29 release_post0700
          event 30 release_post0715
          event 31 release_post0730
          event 32 release_post0745
          event 33 release_post0800
          event 34 release_post0815
          event 35 release_post0830
          event 36 release_post0845
          event 37 release_post0900
          event 38 release_post0915
          event 39 release_post0930
          event 40 release_post0945
          event 41 release_post1000
          event 42 release_post1015
          event 43 release_post1030
          event 44 release_post1045
          event 45 release_post1100
          event 46 release_post1115
          event 47 release_post1130
          event 48 release_post1145
          event 49 release_post1200
          event 50 release_post1215
          event 51 release_post1230
          event 52 release_post1245
          event 53 release_post1300
          event 54 release_post1315
          event 55 release_post1330
          event 56 release_post1345
          event 57 release_post1400
          event 58 release_post1415
          event 59 release_post1430
          event 60 release_post1445
          event 61 release_post1500
        family post_subh
          task jhrrr_post_f0000
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager_subh:release_post0000
          task jhrrr_post_f0015
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager_subh:release_post0015
          task jhrrr_post_f0030
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager_subh:release_post0030
          task jhrrr_post_f0045
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager_subh:release_post0045
          task jhrrr_post_f0100
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager_subh:release_post0100
          task jhrrr_post_f0115
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager_subh:release_post0115
          task jhrrr_post_f0130
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager_subh:release_post0130
          task jhrrr_post_f0145
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager_subh:release_post0145
          task jhrrr_post_f0200
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager_subh:release_post0200
          task jhrrr_post_f0215
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager_subh:release_post0215
          task jhrrr_post_f0230
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager_subh:release_post0230
          task jhrrr_post_f0245
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager_subh:release_post0245
          task jhrrr_post_f0300
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager_subh:release_post0300
          task jhrrr_post_f0315
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager_subh:release_post0315
          task jhrrr_post_f0330
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager_subh:release_post0330
          task jhrrr_post_f0345
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager_subh:release_post0345
          task jhrrr_post_f0400
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager_subh:release_post0400
          task jhrrr_post_f0415
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager_subh:release_post0415
          task jhrrr_post_f0430
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager_subh:release_post0430
          task jhrrr_post_f0445
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager_subh:release_post0445
          task jhrrr_post_f0500
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager_subh:release_post0500
          task jhrrr_post_f0515
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager_subh:release_post0515
          task jhrrr_post_f0530
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager_subh:release_post0530
          task jhrrr_post_f0545
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager_subh:release_post0545
          task jhrrr_post_f0600
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager_subh:release_post0600
          task jhrrr_post_f0615
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager_subh:release_post0615
          task jhrrr_post_f0630
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager_subh:release_post0630
          task jhrrr_post_f0645
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager_subh:release_post0645
          task jhrrr_post_f0700
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager_subh:release_post0700
          task jhrrr_post_f0715
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager_subh:release_post0715
          task jhrrr_post_f0730
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager_subh:release_post0730
          task jhrrr_post_f0745
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager_subh:release_post0745
          task jhrrr_post_f0800
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager_subh:release_post0800
          task jhrrr_post_f0815
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager_subh:release_post0815
          task jhrrr_post_f0830
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager_subh:release_post0830
          task jhrrr_post_f0845
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager_subh:release_post0845
          task jhrrr_post_f0900
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager_subh:release_post0900
          task jhrrr_post_f0915
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager_subh:release_post0915
          task jhrrr_post_f0930
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager_subh:release_post0930
          task jhrrr_post_f0945
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager_subh:release_post0945
          task jhrrr_post_f1000
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager_subh:release_post1000
          task jhrrr_post_f1015
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager_subh:release_post1015
          task jhrrr_post_f1030
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager_subh:release_post1030
          task jhrrr_post_f1045
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager_subh:release_post1045
          task jhrrr_post_f1100
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager_subh:release_post1100
          task jhrrr_post_f1115
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager_subh:release_post1115
          task jhrrr_post_f1130
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager_subh:release_post1130
          task jhrrr_post_f1145
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager_subh:release_post1145
          task jhrrr_post_f1200
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager_subh:release_post1200
          task jhrrr_post_f1215
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager_subh:release_post1215
          task jhrrr_post_f1230
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager_subh:release_post1230
          task jhrrr_post_f1245
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager_subh:release_post1245
          task jhrrr_post_f1300
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager_subh:release_post1300
          task jhrrr_post_f1315
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager_subh:release_post1315
          task jhrrr_post_f1330
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager_subh:release_post1330
          task jhrrr_post_f1345
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager_subh:release_post1345
          task jhrrr_post_f1400
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager_subh:release_post1400
          task jhrrr_post_f1415
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager_subh:release_post1415
          task jhrrr_post_f1430
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager_subh:release_post1430
          task jhrrr_post_f1445
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager_subh:release_post1445
          task jhrrr_post_f1500
            trigger /prod12/hrrr/hrrr_17/post/jhrrr_post_manager_subh:release_post1500
        endfamily
      endfamily
      task jhrrr_postsnd
        trigger /prod12/hrrr/hrrr_17/post/wrfbufr == complete
      task jhrrr_gempak
        trigger /prod12/hrrr/hrrr_17/post/post/jhrrr_post_f00 == active or /prod12/hrrr/hrrr_17/post/post/jhrrr_post_f00 == complete
      task jhrrr_cleanup
        trigger /prod12/hrrr/hrrr_17/post == complete and /prod12/hrrr/hrrr_17/jhrrr_postsnd == complete
    endfamily
  endfamily
  family ukmet
    edit RES '0p67'
    family hires_fh18
      edit FH '18'
      task jukmet_hires_gempak
        time 16:00
    endfamily
    family hires_fh42
      edit FH '42'
      task jukmet_hires_gempak
        time 16:15
    endfamily
    family hires_fh72
      edit FH '72'
      task jukmet_hires_gempak
        time 16:30
    endfamily
    family hires_fh108
      edit FH '108'
      task jukmet_hires_gempak
        time 16:45
    endfamily
    family hires_fh144
      edit FH '144'
      task jukmet_hires_gempak
        time 17:00
    endfamily
    task jukmet_early
      time 17:00
      time 17:45
    task jukmet
      time 18:25
    task jukmet_gempak
      trigger hires_fh18 eq complete && hires_fh42 eq complete && hires_fh72 eq complete && hires_fh108 eq complete && hires_fh144 eq complete && jukmet eq complete
    task jukmet_jtwc
      trigger jukmet eq complete
    task jukmet_gempak_meta
      trigger hires_fh18 eq complete && hires_fh42 eq complete && hires_fh72 eq complete && hires_fh108 eq complete && hires_fh144 eq complete
      edit RES '0p67'
    task jukmet_gempak_graphics
      trigger jukmet_gempak_meta eq complete && jukmet_gempak eq complete
  endfamily
  family arl
    edit ECF_FILES '/ecf/ecfnets/scripts'
    edit PROJ 'HYS-OPS'
    family dust
      task jdust_fcst
        trigger ../hysplit/hls_canned/jhyspt_nam_prep eq complete
    endfamily
    family hysplit
      edit ECF_JOB_CMD '/ecf/ecfutils/unixsubmit %ECF_JOB% %ECF_JOBOUT% ibmsp'
      family ondemand_ctbto
        task jhyspt_ctbto_chk
          defstatus complete
        task jhyspt_ctbto_fcst
          defstatus complete
          trigger jhyspt_ctbto_chk eq complete
          edit ECF_PASS 'FREE'
        task jhyspt_ctbto_post
          defstatus complete
          trigger jhyspt_ctbto_fcst eq complete
        task jhyspt_ctbto_archive
          defstatus complete
          trigger jhyspt_ctbto_post eq complete
      endfamily
      family hls_ondemand_sdm
        defstatus complete
        task jhyspt_datachk
          defstatus complete
          label meteo " "
        task jhyspt_fcst
          defstatus complete
          trigger jhyspt_datachk eq complete
        task jhyspt_post
          defstatus complete
          trigger jhyspt_fcst eq complete
          event 10 boulder.hypush
          event 20 ncorzdm
        task jhyspt_wafs
          defstatus complete
        task jhyspt_post_web_rsmc
          defstatus complete
          edit ECF_JOB_CMD '/ecf/ecfutils/unixsubmit %ECF_JOB% %ECF_JOBOUT% ibmsp'
      endfamily
      family hls_canned
        task jhyspt_hinest_prep
          trigger /prod12/nam/nest_hawaii/prdgen/jnam_prdgen_f48 eq complete and /prod12/nam/nest_hawaii/prdgen/jnam_prdgen_f45 eq complete and /prod12/nam/nest_hawaii/prdgen/jnam_prdgen_f42 eq complete and /prod12/nam/nest_hawaii/prdgen/jnam_prdgen_f39 eq complete
        task jhyspt_rap_prep
          trigger /prod12/rap/rap_12/prdgen eq complete
        task jhyspt_firewx_prep
          trigger /prod12/nam/nest_firewx eq complete
        task jhyspt_nam12k_prep
          trigger /prod12/nam/post_processing/grib_awips/jnam_awips_f75 == complete and /prod12/nam/post_processing/grib_awips/jnam_awips_f78 == complete and /prod12/nam/post_processing/grib_awips/jnam_awips_f81 == complete and /prod12/nam/post_processing/grib_awips/jnam_awips_f84 == complete
        task jhyspt_nam_prep
          trigger /prod12/nam/post_processing/grib_awips/jnam_awips_f75 == complete and /prod12/nam/post_processing/grib_awips/jnam_awips_f78 == complete and /prod12/nam/post_processing/grib_awips/jnam_awips_f81 == complete and /prod12/nam/post_processing/grib_awips/jnam_awips_f84 == complete
        task jhyspt_fcst
          trigger jhyspt_nam12k_prep eq complete and jhyspt_firewx_prep eq complete and jhyspt_nam_prep eq complete and jhyspt_rap_prep eq complete
        task jhyspt_post_web
          trigger jhyspt_fcst eq complete
          event 10 boulder.hypush
          event 20 ncorzdm
        task jhyspt_conusnest_prep
          trigger /prod12/nam/nest_conus/post eq complete
        task jhyspt_rap_prep_15
          trigger /prod12/rap/rap_15/prdgen eq complete
          edit CYC '15'
        task jhyspt_gfsonedeg_prep
          trigger /prod12/gfs/prdgen eq complete
        task jhyspt_gfslongrange_prep
          trigger /prod12/gfs/prdgen eq complete
        task jhyspt_gfs_nceppost_prep
          trigger /prod12/gfs/prdgen/jgfs_pgrb2_f192 eq complete
        task jhyspt_gdas_prep
          trigger /prod12/gdas/post/jgdas_post_high eq complete
      endfamily
    endfamily
  endfamily
  family ecmwf
    edit ECF_FILES '/ecf/ecfnets/scripts/ecmwf/mos'
    edit COM 'com2'
    edit PROJ 'MDLST-OPS'
    edit QUEUE 'prod2_shared'
    family mos
      family short_range
        task jecmmos_prep
          time 18:21
        task jecmmos_prep23
          time 18:22
        task jecmmos_metar_forecast
          trigger jecmmos_prep == complete
        task jecmmos_tstms_forecast
          trigger jecmmos_prep == complete
        task jecmmos_stations_prdgen
          trigger jecmmos_metar_forecast == complete and jecmmos_tstms_forecast == complete
      endfamily
      family extended_range
        task jecmmos_ext_prep
          trigger ../short_range/jecmmos_prep == complete
          time 19:01
        task jecmmos_metar_ext_forecast
          trigger jecmmos_ext_prep == complete and ../short_range/jecmmos_stations_prdgen == complete
        task jecmmos_tstms_ext_forecast
          trigger jecmmos_ext_prep == complete and ../short_range/jecmmos_stations_prdgen == complete
        task jecmmos_stations_ext_prdgen
          trigger jecmmos_metar_ext_forecast == complete and jecmmos_tstms_ext_forecast == complete
        task jecmmos_ext_prep23
          trigger ../short_range/jecmmos_prep23 == complete
          time 19:02
      endfamily
      family ensemble
        task jecmmos_stations_ens_fcst
          trigger ../extended_range/jecmmos_stations_ext_prdgen == complete
          edit QUEUE 'prod2'
          time 20:51
        task jecmmos_stations_ens_stats
          trigger jecmmos_stations_ens_fcst == complete
        task jecmmos_stations_ens_prdgen
          trigger jecmmos_stations_ens_stats == complete
      endfamily
    endfamily
  endfamily
  family sref15
    edit ECF_FILES '/ecf/ecfnets/scripts/sref'
    edit PROJ 'SREF-OPS'
    edit COM 'com2'
    edit QUEUE 'prod2'
    edit CYC '15'
    edit ECF_JOB_CMD '/ecf/ecfutils/unixsubmit %ECF_JOB% %ECF_JOBOUT% ibmsp'
    family getglb
      task jsref_wrf_gfsprep_ctl
        trigger /prod12/gfs/post/jgfs_post_f192 == complete
      task jsref_wrf_gefsprep_c00
        trigger /prod12/gefs/post_high/c00 == complete
      task jsref_wrf_gefsprep_p01
        trigger /prod12/gefs/post_high/p01 == complete
      task jsref_wrf_gefsprep_p02
        trigger /prod12/gefs/post_high/p02 == complete
      task jsref_wrf_gefsprep_p03
        trigger /prod12/gefs/post_high/p03 == complete
      task jsref_wrf_gefsprep_p04
        trigger /prod12/gefs/post_high/p04 == complete
      task jsref_wrf_gefsprep_p05
        trigger /prod12/gefs/post_high/p05 == complete
      task jsref_wrf_gefsprep_p06
        trigger /prod12/gefs/post_high/p06 == complete
      task jsref_wrf_gefsprep_p07
        trigger /prod12/gefs/post_high/p07 == complete
      task jsref_wrf_gefsprep_p08
        trigger /prod12/gefs/post_high/p08 == complete
      task jsref_wrf_gefsprep_p09
        trigger /prod12/gefs/post_high/p09 == complete
      task jsref_wrf_gefsprep_p10
        trigger /prod12/gefs/post_high/p10 == complete
      task jsref_wrf_gefsprep_p11
        trigger /prod12/gefs/post_high/p11 == complete
      task jsref_wrf_gefsprep_p12
        trigger /prod12/gefs/post_high/p12 == complete
      task jsref_wrf_gefsprep_p13
        trigger /prod12/gefs/post_high/p13 == complete
      task jsref_wrf_gefsprep_p14
        trigger /prod12/gefs/post_high/p14 == complete
      task jsref_wrf_gefsprep_p15
        trigger /prod12/gefs/post_high/p15 == complete
      task jsref_wrf_gefsprep_p16
        trigger /prod12/gefs/post_high/p16 == complete
      task jsref_wrf_gefsprep_p17
        trigger /prod12/gefs/post_high/p17 == complete
      task jsref_wrf_gefsprep_p18
        trigger /prod12/gefs/post_high/p18 == complete
      task jsref_wrf_gefsprep_p19
        trigger /prod12/gefs/post_high/p19 == complete
      task jsref_wrf_gefsprep_p20
        trigger /prod12/gefs/post_high/p20 == complete
    endfamily
    task jsref_gefs2sref
      trigger /prod12/sref15/getglb == complete
    family wrfnmb_ctl
      edit MEM 'ctl'
      edit MEM0 'ctl'
      edit MOD 'nmb'
      edit MODEL 'NMB'
      task jsref_wrf_prep
        trigger /prod12/sref15/getglb == complete
      task jsref_wrf_real
        trigger /prod12/sref15/wrfnmb_ctl/jsref_wrf_prep == complete
      task jsref_wrf_fcst
        trigger /prod12/sref15/wrfnmb_ctl/jsref_wrf_real == complete
        edit NTASK '112'
      task jsref_wrf_post
        trigger /prod12/sref15/wrfnmb_ctl/jsref_wrf_fcst == active
      task jsref_wrf_post_hrly
        trigger /prod12/sref15/wrfnmb_ctl/jsref_wrf_fcst == active
      task jsref_wrf_gempak
        trigger /prod12/sref15/wrfnmb_ctl/jsref_wrf_post == complete
      task jsref_wrf_bufr
        trigger /prod12/sref15/wrfnmb_ctl/jsref_wrf_fcst == active and /prod12/sref15/wrfnmb_ctl/jsref_wrf_post == active
      task jsref_wrf_post132
        trigger /prod12/sref15/wrfnmb_ctl/jsref_wrf_fcst == active
      task jsref_wrf_gempak132
        trigger /prod12/sref15/wrfnmb_ctl/jsref_wrf_post132 == complete
    endfamily
    family wrfnmb_p1
      edit MEM 'p1'
      edit MEM0 'p01'
      edit MOD 'nmb'
      edit MODEL 'NMB'
      task jsref_wrf_prep
        trigger /prod12/sref15/getglb == complete
      task jsref_wrf_real
        trigger /prod12/sref15/wrfnmb_p1/jsref_wrf_prep == complete and /prod12/sref15/wrfnmb_ctl/jsref_wrf_prep == complete
      task jsref_wrf_fcst
        trigger /prod12/sref15/wrfnmb_p1/jsref_wrf_real == complete
        edit NTASK '112'
      task jsref_wrf_post
        trigger /prod12/sref15/wrfnmb_p1/jsref_wrf_fcst == active
      task jsref_wrf_post_hrly
        trigger /prod12/sref15/wrfnmb_p1/jsref_wrf_fcst == active
      task jsref_wrf_gempak
        trigger /prod12/sref15/wrfnmb_p1/jsref_wrf_post == complete
      task jsref_wrf_bufr
        trigger /prod12/sref15/wrfnmb_p1/jsref_wrf_fcst == active and /prod12/sref15/wrfnmb_p1/jsref_wrf_post == active
      task jsref_wrf_post132
        trigger /prod12/sref15/wrfnmb_p1/jsref_wrf_fcst == active
      task jsref_wrf_gempak132
        trigger /prod12/sref15/wrfnmb_p1/jsref_wrf_post132 == complete
    endfamily
    family wrfnmb_n1
      edit MEM 'n1'
      edit MEM0 'n01'
      edit MOD 'nmb'
      edit MODEL 'NMB'
      task jsref_wrf_prep
        trigger /prod12/sref15/getglb == complete
      task jsref_wrf_real
        trigger /prod12/sref15/wrfnmb_n1/jsref_wrf_prep == complete and /prod12/sref15/wrfnmb_ctl/jsref_wrf_prep == complete
      task jsref_wrf_fcst
        trigger /prod12/sref15/wrfnmb_n1/jsref_wrf_real == complete
        edit NTASK '112'
      task jsref_wrf_post
        trigger /prod12/sref15/wrfnmb_n1/jsref_wrf_fcst == active
      task jsref_wrf_post_hrly
        trigger /prod12/sref15/wrfnmb_n1/jsref_wrf_fcst == active
      task jsref_wrf_gempak
        trigger /prod12/sref15/wrfnmb_n1/jsref_wrf_post == complete
      task jsref_wrf_bufr
        trigger /prod12/sref15/wrfnmb_n1/jsref_wrf_fcst == active and /prod12/sref15/wrfnmb_n1/jsref_wrf_post == active
      task jsref_wrf_post132
        trigger /prod12/sref15/wrfnmb_n1/jsref_wrf_fcst == active
      task jsref_wrf_gempak132
        trigger /prod12/sref15/wrfnmb_n1/jsref_wrf_post132 == complete
    endfamily
    family wrfnmb_n2
      edit MEM 'n2'
      edit MEM0 'n02'
      edit MOD 'nmb'
      edit MODEL 'NMB'
      task jsref_wrf_prep
        trigger /prod12/sref15/getglb == complete
      task jsref_wrf_real
        trigger /prod12/sref15/wrfnmb_n2/jsref_wrf_prep == complete and /prod12/sref15/wrfnmb_ctl/jsref_wrf_prep == complete
      task jsref_wrf_fcst
        trigger /prod12/sref15/wrfnmb_n2/jsref_wrf_real == complete
        edit NTASK '96'
      task jsref_wrf_post
        trigger /prod12/sref15/wrfnmb_n2/jsref_wrf_fcst == active
      task jsref_wrf_post_hrly
        trigger /prod12/sref15/wrfnmb_n2/jsref_wrf_fcst == active
      task jsref_wrf_gempak
        trigger /prod12/sref15/wrfnmb_n2/jsref_wrf_post == complete
      task jsref_wrf_bufr
        trigger /prod12/sref15/wrfnmb_n2/jsref_wrf_fcst == active and /prod12/sref15/wrfnmb_n2/jsref_wrf_post == active
      task jsref_wrf_post132
        trigger /prod12/sref15/wrfnmb_n2/jsref_wrf_fcst == active
      task jsref_wrf_gempak132
        trigger /prod12/sref15/wrfnmb_n2/jsref_wrf_post132 == complete
    endfamily
    family wrfnmb_p2
      edit MEM 'p2'
      edit MEM0 'p02'
      edit MOD 'nmb'
      edit MODEL 'NMB'
      task jsref_wrf_prep
        trigger /prod12/sref15/getglb == complete
      task jsref_wrf_real
        trigger /prod12/sref15/wrfnmb_p2/jsref_wrf_prep == complete and /prod12/sref15/wrfnmb_ctl/jsref_wrf_prep == complete
      task jsref_wrf_fcst
        trigger /prod12/sref15/wrfnmb_p2/jsref_wrf_real == complete
        edit NTASK '128'
      task jsref_wrf_post
        trigger /prod12/sref15/wrfnmb_p2/jsref_wrf_fcst == active
      task jsref_wrf_post_hrly
        trigger /prod12/sref15/wrfnmb_p2/jsref_wrf_fcst == active
      task jsref_wrf_gempak
        trigger /prod12/sref15/wrfnmb_p2/jsref_wrf_post == complete
      task jsref_wrf_bufr
        trigger /prod12/sref15/wrfnmb_p2/jsref_wrf_fcst == active and /prod12/sref15/wrfnmb_p2/jsref_wrf_post == active
      task jsref_wrf_post132
        trigger /prod12/sref15/wrfnmb_p2/jsref_wrf_fcst == active
      task jsref_wrf_gempak132
        trigger /prod12/sref15/wrfnmb_p2/jsref_wrf_post132 == complete
    endfamily
    family wrfnmb_n3
      edit MEM 'n3'
      edit MEM0 'n03'
      edit MOD 'nmb'
      edit MODEL 'NMB'
      task jsref_wrf_prep
        trigger /prod12/sref15/getglb == complete
      task jsref_wrf_real
        trigger /prod12/sref15/wrfnmb_n3/jsref_wrf_prep == complete and /prod12/sref15/wrfnmb_ctl/jsref_wrf_prep == complete
      task jsref_wrf_fcst
        trigger /prod12/sref15/wrfnmb_n3/jsref_wrf_real == complete
        edit NTASK '96'
      task jsref_wrf_post
        trigger /prod12/sref15/wrfnmb_n3/jsref_wrf_fcst == active
      task jsref_wrf_post_hrly
        trigger /prod12/sref15/wrfnmb_n3/jsref_wrf_fcst == active
      task jsref_wrf_gempak
        trigger /prod12/sref15/wrfnmb_n3/jsref_wrf_post == complete
      task jsref_wrf_bufr
        trigger /prod12/sref15/wrfnmb_n3/jsref_wrf_fcst == active and /prod12/sref15/wrfnmb_n3/jsref_wrf_post == active
      task jsref_wrf_post132
        trigger /prod12/sref15/wrfnmb_n3/jsref_wrf_fcst == active
      task jsref_wrf_gempak132
        trigger /prod12/sref15/wrfnmb_n3/jsref_wrf_post132 == complete
    endfamily
    family wrfnmb_p3
      edit MEM 'p3'
      edit MEM0 'p03'
      edit MOD 'nmb'
      edit MODEL 'NMB'
      task jsref_wrf_prep
        trigger /prod12/sref15/getglb == complete
      task jsref_wrf_real
        trigger /prod12/sref15/wrfnmb_p3/jsref_wrf_prep == complete and /prod12/sref15/wrfnmb_ctl/jsref_wrf_prep == complete
      task jsref_wrf_fcst
        trigger /prod12/sref15/wrfnmb_p3/jsref_wrf_real == complete
        edit NTASK '128'
      task jsref_wrf_post
        trigger /prod12/sref15/wrfnmb_p3/jsref_wrf_fcst == active
      task jsref_wrf_post_hrly
        trigger /prod12/sref15/wrfnmb_p3/jsref_wrf_fcst == active
      task jsref_wrf_gempak
        trigger /prod12/sref15/wrfnmb_p3/jsref_wrf_post == complete
      task jsref_wrf_bufr
        trigger /prod12/sref15/wrfnmb_p3/jsref_wrf_fcst == active and /prod12/sref15/wrfnmb_p3/jsref_wrf_post == active
      task jsref_wrf_post132
        trigger /prod12/sref15/wrfnmb_p3/jsref_wrf_fcst == active
      task jsref_wrf_gempak132
        trigger /prod12/sref15/wrfnmb_p3/jsref_wrf_post132 == complete
    endfamily
    family wrfnmb_n4
      edit MEM 'n4'
      edit MEM0 'n04'
      edit MOD 'nmb'
      edit MODEL 'NMB'
      task jsref_wrf_prep
        trigger /prod12/sref15/getglb == complete
      task jsref_wrf_real
        trigger /prod12/sref15/wrfnmb_n4/jsref_wrf_prep == complete and /prod12/sref15/wrfnmb_ctl/jsref_wrf_prep == complete
      task jsref_wrf_fcst
        trigger /prod12/sref15/wrfnmb_n4/jsref_wrf_real == complete
        edit NTASK '128'
      task jsref_wrf_post
        trigger /prod12/sref15/wrfnmb_n4/jsref_wrf_fcst == active
      task jsref_wrf_post_hrly
        trigger /prod12/sref15/wrfnmb_n4/jsref_wrf_fcst == active
      task jsref_wrf_gempak
        trigger /prod12/sref15/wrfnmb_n4/jsref_wrf_post == complete
      task jsref_wrf_bufr
        trigger /prod12/sref15/wrfnmb_n4/jsref_wrf_fcst == active and /prod12/sref15/wrfnmb_n4/jsref_wrf_post == active
      task jsref_wrf_post132
        trigger /prod12/sref15/wrfnmb_n4/jsref_wrf_fcst == active
      task jsref_wrf_gempak132
        trigger /prod12/sref15/wrfnmb_n4/jsref_wrf_post132 == complete
    endfamily
    family wrfnmb_p4
      edit MEM 'p4'
      edit MEM0 'p04'
      edit MOD 'nmb'
      edit MODEL 'NMB'
      task jsref_wrf_prep
        trigger /prod12/sref15/getglb == complete
      task jsref_wrf_real
        trigger /prod12/sref15/wrfnmb_p4/jsref_wrf_prep == complete and /prod12/sref15/wrfnmb_ctl/jsref_wrf_prep == complete
      task jsref_wrf_fcst
        trigger /prod12/sref15/wrfnmb_p4/jsref_wrf_real == complete
        edit NTASK '96'
      task jsref_wrf_post
        trigger /prod12/sref15/wrfnmb_p4/jsref_wrf_fcst == active
      task jsref_wrf_post_hrly
        trigger /prod12/sref15/wrfnmb_p4/jsref_wrf_fcst == active
      task jsref_wrf_gempak
        trigger /prod12/sref15/wrfnmb_p4/jsref_wrf_post == complete
      task jsref_wrf_bufr
        trigger /prod12/sref15/wrfnmb_p4/jsref_wrf_fcst == active and /prod12/sref15/wrfnmb_p4/jsref_wrf_post == active
      task jsref_wrf_post132
        trigger /prod12/sref15/wrfnmb_p4/jsref_wrf_fcst == active
      task jsref_wrf_gempak132
        trigger /prod12/sref15/wrfnmb_p4/jsref_wrf_post132 == complete
    endfamily
    family wrfnmb_n5
      edit MEM 'n5'
      edit MEM0 'n05'
      edit MOD 'nmb'
      edit MODEL 'NMB'
      task jsref_wrf_prep
        trigger /prod12/sref15/getglb == complete
      task jsref_wrf_real
        trigger /prod12/sref15/wrfnmb_n5/jsref_wrf_prep == complete and /prod12/sref15/wrfnmb_ctl/jsref_wrf_prep == complete
      task jsref_wrf_fcst
        trigger /prod12/sref15/wrfnmb_n5/jsref_wrf_real == complete
        edit NTASK '128'
      task jsref_wrf_post
        trigger /prod12/sref15/wrfnmb_n5/jsref_wrf_fcst == active
      task jsref_wrf_post_hrly
        trigger /prod12/sref15/wrfnmb_n5/jsref_wrf_fcst == active
      task jsref_wrf_gempak
        trigger /prod12/sref15/wrfnmb_n5/jsref_wrf_post == complete
      task jsref_wrf_bufr
        trigger /prod12/sref15/wrfnmb_n5/jsref_wrf_fcst == active and /prod12/sref15/wrfnmb_n5/jsref_wrf_post == active
      task jsref_wrf_post132
        trigger /prod12/sref15/wrfnmb_n5/jsref_wrf_fcst == active
      task jsref_wrf_gempak132
        trigger /prod12/sref15/wrfnmb_n5/jsref_wrf_post132 == complete
    endfamily
    family wrfnmb_p5
      edit MEM 'p5'
      edit MEM0 'p05'
      edit MOD 'nmb'
      edit MODEL 'NMB'
      task jsref_wrf_prep
        trigger /prod12/sref15/getglb == complete
      task jsref_wrf_real
        trigger /prod12/sref15/wrfnmb_p5/jsref_wrf_prep == complete and /prod12/sref15/wrfnmb_ctl/jsref_wrf_prep == complete
      task jsref_wrf_fcst
        trigger /prod12/sref15/wrfnmb_p5/jsref_wrf_real == complete
        edit NTASK '112'
      task jsref_wrf_post
        trigger /prod12/sref15/wrfnmb_p5/jsref_wrf_fcst == active
      task jsref_wrf_post_hrly
        trigger /prod12/sref15/wrfnmb_p5/jsref_wrf_fcst == active
      task jsref_wrf_gempak
        trigger /prod12/sref15/wrfnmb_p5/jsref_wrf_post == complete
      task jsref_wrf_bufr
        trigger /prod12/sref15/wrfnmb_p5/jsref_wrf_fcst == active and /prod12/sref15/wrfnmb_p5/jsref_wrf_post == active
      task jsref_wrf_post132
        trigger /prod12/sref15/wrfnmb_p5/jsref_wrf_fcst == active
      task jsref_wrf_gempak132
        trigger /prod12/sref15/wrfnmb_p5/jsref_wrf_post132 == complete
    endfamily
    family wrfnmb_n6
      edit MEM 'n6'
      edit MEM0 'n06'
      edit MOD 'nmb'
      edit MODEL 'NMB'
      task jsref_wrf_prep
        trigger /prod12/sref15/getglb == complete
      task jsref_wrf_real
        trigger /prod12/sref15/wrfnmb_n6/jsref_wrf_prep == complete and /prod12/sref15/wrfnmb_ctl/jsref_wrf_prep == complete
      task jsref_wrf_fcst
        trigger /prod12/sref15/wrfnmb_n6/jsref_wrf_real == complete
        edit NTASK '96'
      task jsref_wrf_post
        trigger /prod12/sref15/wrfnmb_n6/jsref_wrf_fcst == active
      task jsref_wrf_post_hrly
        trigger /prod12/sref15/wrfnmb_n6/jsref_wrf_fcst == active
      task jsref_wrf_gempak
        trigger /prod12/sref15/wrfnmb_n6/jsref_wrf_post == complete
      task jsref_wrf_bufr
        trigger /prod12/sref15/wrfnmb_n6/jsref_wrf_fcst == active and /prod12/sref15/wrfnmb_n6/jsref_wrf_post == active
      task jsref_wrf_post132
        trigger /prod12/sref15/wrfnmb_n6/jsref_wrf_fcst == active
      task jsref_wrf_gempak132
        trigger /prod12/sref15/wrfnmb_n6/jsref_wrf_post132 == complete
    endfamily
    family wrfnmb_p6
      edit MEM 'p6'
      edit MEM0 'p06'
      edit MOD 'nmb'
      edit MODEL 'NMB'
      task jsref_wrf_prep
        trigger /prod12/sref15/getglb == complete
      task jsref_wrf_real
        trigger /prod12/sref15/wrfnmb_p6/jsref_wrf_prep == complete and /prod12/sref15/wrfnmb_ctl/jsref_wrf_prep == complete
      task jsref_wrf_fcst
        trigger /prod12/sref15/wrfnmb_p6/jsref_wrf_real == complete
        edit NTASK '96'
      task jsref_wrf_post
        trigger /prod12/sref15/wrfnmb_p6/jsref_wrf_fcst == active
      task jsref_wrf_post_hrly
        trigger /prod12/sref15/wrfnmb_p6/jsref_wrf_fcst == active
      task jsref_wrf_gempak
        trigger /prod12/sref15/wrfnmb_p6/jsref_wrf_post == complete
      task jsref_wrf_bufr
        trigger /prod12/sref15/wrfnmb_p6/jsref_wrf_fcst == active and /prod12/sref15/wrfnmb_p6/jsref_wrf_post == active
      task jsref_wrf_post132
        trigger /prod12/sref15/wrfnmb_p6/jsref_wrf_fcst == active
      task jsref_wrf_gempak132
        trigger /prod12/sref15/wrfnmb_p6/jsref_wrf_post132 == complete
    endfamily
    family wrfarw_ctl
      edit MEM 'ctl'
      edit MEM0 'ctl'
      edit MOD 'arw'
      edit MODEL 'ARW'
      task jsref_wrf_prep
        trigger /prod12/sref15/getglb == complete
      task jsref_wrf_real
        trigger /prod12/sref15/wrfarw_ctl/jsref_wrf_prep == complete
      task jsref_wrf_fcst
        trigger /prod12/sref15/wrfarw_ctl/jsref_wrf_real == complete
        edit NTASK '160'
      task jsref_wrf_post_hrly
        trigger /prod12/sref15/wrfarw_ctl/jsref_wrf_fcst == active
      task jsref_wrf_post
        trigger /prod12/sref15/wrfarw_ctl/jsref_wrf_fcst == active
      task jsref_wrf_post132
        trigger /prod12/sref15/wrfarw_ctl/jsref_wrf_fcst == active
      task jsref_wrf_gempak
        trigger /prod12/sref15/wrfarw_ctl/jsref_wrf_post == complete
      task jsref_wrf_gempak132
        trigger /prod12/sref15/wrfarw_ctl/jsref_wrf_post132 == complete
      task jsref_wrf_bufr
        trigger /prod12/sref15/wrfarw_ctl/jsref_wrf_fcst == active and /prod12/sref15/wrfarw_ctl/jsref_wrf_post == active
    endfamily
    family wrfarw_n1
      edit MEM 'n1'
      edit MEM0 'n01'
      edit MOD 'arw'
      edit MODEL 'ARW'
      task jsref_wrf_prep
        trigger /prod12/sref15/getglb == complete
      task jsref_wrf_real
        trigger /prod12/sref15/wrfarw_n1/jsref_wrf_prep == complete and /prod12/sref15/wrfarw_ctl/jsref_wrf_prep == complete
      task jsref_wrf_fcst
        trigger /prod12/sref15/wrfarw_n1/jsref_wrf_real == complete
        edit NTASK '144'
      task jsref_wrf_post_hrly
        trigger /prod12/sref15/wrfarw_n1/jsref_wrf_fcst == active
      task jsref_wrf_post
        trigger /prod12/sref15/wrfarw_n1/jsref_wrf_fcst == active
      task jsref_wrf_post132
        trigger /prod12/sref15/wrfarw_n1/jsref_wrf_fcst == active
      task jsref_wrf_gempak
        trigger /prod12/sref15/wrfarw_n1/jsref_wrf_post == complete
      task jsref_wrf_gempak132
        trigger /prod12/sref15/wrfarw_n1/jsref_wrf_post132 == complete
      task jsref_wrf_bufr
        trigger /prod12/sref15/wrfarw_n1/jsref_wrf_fcst == active and /prod12/sref15/wrfarw_n1/jsref_wrf_post == active
    endfamily
    family wrfarw_p1
      edit MEM 'p1'
      edit MEM0 'p01'
      edit MOD 'arw'
      edit MODEL 'ARW'
      task jsref_wrf_prep
        trigger /prod12/sref15/getglb == complete
      task jsref_wrf_real
        trigger /prod12/sref15/wrfarw_p1/jsref_wrf_prep == complete and /prod12/sref15/wrfarw_ctl/jsref_wrf_prep == complete
      task jsref_wrf_fcst
        trigger /prod12/sref15/wrfarw_p1/jsref_wrf_real == complete
        edit NTASK '192'
      task jsref_wrf_post_hrly
        trigger /prod12/sref15/wrfarw_p1/jsref_wrf_fcst == active
      task jsref_wrf_post
        trigger /prod12/sref15/wrfarw_p1/jsref_wrf_fcst == active
      task jsref_wrf_post132
        trigger /prod12/sref15/wrfarw_p1/jsref_wrf_fcst == active
      task jsref_wrf_gempak
        trigger /prod12/sref15/wrfarw_p1/jsref_wrf_post == complete
      task jsref_wrf_gempak132
        trigger /prod12/sref15/wrfarw_p1/jsref_wrf_post132 == complete
      task jsref_wrf_bufr
        trigger /prod12/sref15/wrfarw_p1/jsref_wrf_fcst == active and /prod12/sref15/wrfarw_p1/jsref_wrf_post == active
    endfamily
    family wrfarw_n2
      edit MEM 'n2'
      edit MEM0 'n02'
      edit MOD 'arw'
      edit MODEL 'ARW'
      task jsref_wrf_prep
        trigger /prod12/sref15/getglb == complete
      task jsref_wrf_real
        trigger /prod12/sref15/wrfarw_n2/jsref_wrf_prep == complete and /prod12/sref15/wrfarw_ctl/jsref_wrf_prep == complete
      task jsref_wrf_fcst
        trigger /prod12/sref15/wrfarw_n2/jsref_wrf_real == complete
        edit NTASK '144'
      task jsref_wrf_post_hrly
        trigger /prod12/sref15/wrfarw_n2/jsref_wrf_fcst == active
      task jsref_wrf_post
        trigger /prod12/sref15/wrfarw_n2/jsref_wrf_fcst == active
      task jsref_wrf_gempak
        trigger /prod12/sref15/wrfarw_n2/jsref_wrf_post == complete
      task jsref_wrf_post132
        trigger /prod12/sref15/wrfarw_n2/jsref_wrf_fcst == active
      task jsref_wrf_gempak132
        trigger /prod12/sref15/wrfarw_n2/jsref_wrf_post132 == complete
      task jsref_wrf_bufr
        trigger /prod12/sref15/wrfarw_n2/jsref_wrf_fcst == active and /prod12/sref15/wrfarw_n2/jsref_wrf_post == active
    endfamily
    family wrfarw_p2
      edit MEM 'p2'
      edit MEM0 'p02'
      edit MOD 'arw'
      edit MODEL 'ARW'
      task jsref_wrf_prep
        trigger /prod12/sref15/getglb == complete
      task jsref_wrf_real
        trigger /prod12/sref15/wrfarw_p2/jsref_wrf_prep == complete and /prod12/sref15/wrfarw_ctl/jsref_wrf_prep == complete
      task jsref_wrf_fcst
        trigger /prod12/sref15/wrfarw_p2/jsref_wrf_real == complete
        edit NTASK '224'
      task jsref_wrf_post_hrly
        trigger /prod12/sref15/wrfarw_p2/jsref_wrf_fcst == active
      task jsref_wrf_post
        trigger /prod12/sref15/wrfarw_p2/jsref_wrf_fcst == active
      task jsref_wrf_gempak
        trigger /prod12/sref15/wrfarw_p2/jsref_wrf_post == complete
      task jsref_wrf_post132
        trigger /prod12/sref15/wrfarw_p2/jsref_wrf_fcst == active
      task jsref_wrf_gempak132
        trigger /prod12/sref15/wrfarw_p2/jsref_wrf_post132 == complete
      task jsref_wrf_bufr
        trigger /prod12/sref15/wrfarw_p2/jsref_wrf_fcst == active and /prod12/sref15/wrfarw_p2/jsref_wrf_post == active
    endfamily
    family wrfarw_n3
      edit MEM 'n3'
      edit MEM0 'n03'
      edit MOD 'arw'
      edit MODEL 'ARW'
      task jsref_wrf_prep
        trigger /prod12/sref15/getglb == complete
      task jsref_wrf_real
        trigger /prod12/sref15/wrfarw_n3/jsref_wrf_prep == complete and /prod12/sref15/wrfarw_ctl/jsref_wrf_prep == complete
      task jsref_wrf_fcst
        trigger /prod12/sref15/wrfarw_n3/jsref_wrf_real == complete
        edit NTASK '208'
      task jsref_wrf_post_hrly
        trigger /prod12/sref15/wrfarw_n3/jsref_wrf_fcst == active
      task jsref_wrf_post
        trigger /prod12/sref15/wrfarw_n3/jsref_wrf_fcst == active
      task jsref_wrf_post132
        trigger /prod12/sref15/wrfarw_n3/jsref_wrf_fcst == active
      task jsref_wrf_gempak
        trigger /prod12/sref15/wrfarw_n3/jsref_wrf_post == complete
      task jsref_wrf_gempak132
        trigger /prod12/sref15/wrfarw_n3/jsref_wrf_post132 == complete
      task jsref_wrf_bufr
        trigger /prod12/sref15/wrfarw_n3/jsref_wrf_fcst == active and /prod12/sref15/wrfarw_n3/jsref_wrf_post == active
    endfamily
    family wrfarw_p3
      edit MEM 'p3'
      edit MEM0 'p03'
      edit MOD 'arw'
      edit MODEL 'ARW'
      task jsref_wrf_prep
        trigger /prod12/sref15/getglb == complete
      task jsref_wrf_real
        trigger /prod12/sref15/wrfarw_p3/jsref_wrf_prep == complete and /prod12/sref15/wrfarw_ctl/jsref_wrf_prep == complete
      task jsref_wrf_fcst
        trigger /prod12/sref15/wrfarw_p3/jsref_wrf_real == complete
        edit NTASK '160'
      task jsref_wrf_post_hrly
        trigger /prod12/sref15/wrfarw_p3/jsref_wrf_fcst == active
      task jsref_wrf_post
        trigger /prod12/sref15/wrfarw_p3/jsref_wrf_fcst == active
      task jsref_wrf_gempak
        trigger /prod12/sref15/wrfarw_p3/jsref_wrf_post == complete
      task jsref_wrf_post132
        trigger /prod12/sref15/wrfarw_p3/jsref_wrf_fcst == active
      task jsref_wrf_gempak132
        trigger /prod12/sref15/wrfarw_p3/jsref_wrf_post132 == complete
      task jsref_wrf_bufr
        trigger /prod12/sref15/wrfarw_p3/jsref_wrf_fcst == active and /prod12/sref15/wrfarw_p3/jsref_wrf_post == active
    endfamily
    family wrfarw_n4
      edit MEM 'n4'
      edit MEM0 'n04'
      edit MOD 'arw'
      edit MODEL 'ARW'
      task jsref_wrf_prep
        trigger /prod12/sref15/getglb == complete
      task jsref_wrf_real
        trigger /prod12/sref15/wrfarw_n4/jsref_wrf_prep == complete and /prod12/sref15/wrfarw_ctl/jsref_wrf_prep == complete
      task jsref_wrf_fcst
        trigger /prod12/sref15/wrfarw_n4/jsref_wrf_real == complete
        edit NTASK '192'
      task jsref_wrf_post_hrly
        trigger /prod12/sref15/wrfarw_n4/jsref_wrf_fcst == active
      task jsref_wrf_post
        trigger /prod12/sref15/wrfarw_n4/jsref_wrf_fcst == active
      task jsref_wrf_post132
        trigger /prod12/sref15/wrfarw_n4/jsref_wrf_fcst == active
      task jsref_wrf_gempak
        trigger /prod12/sref15/wrfarw_n4/jsref_wrf_post == complete
      task jsref_wrf_gempak132
        trigger /prod12/sref15/wrfarw_n4/jsref_wrf_post132 == complete
      task jsref_wrf_bufr
        trigger /prod12/sref15/wrfarw_n4/jsref_wrf_fcst == active and /prod12/sref15/wrfarw_n4/jsref_wrf_post == active
    endfamily
    family wrfarw_p4
      edit MEM 'p4'
      edit MEM0 'p04'
      edit MOD 'arw'
      edit MODEL 'ARW'
      task jsref_wrf_prep
        trigger /prod12/sref15/getglb == complete
      task jsref_wrf_real
        trigger /prod12/sref15/wrfarw_p4/jsref_wrf_prep == complete and /prod12/sref15/wrfarw_ctl/jsref_wrf_prep == complete
      task jsref_wrf_fcst
        trigger /prod12/sref15/wrfarw_p4/jsref_wrf_real == complete
        edit NTASK '160'
      task jsref_wrf_post_hrly
        trigger /prod12/sref15/wrfarw_p4/jsref_wrf_fcst == active
      task jsref_wrf_post
        trigger /prod12/sref15/wrfarw_p4/jsref_wrf_fcst == active
      task jsref_wrf_gempak
        trigger /prod12/sref15/wrfarw_p4/jsref_wrf_post == complete
      task jsref_wrf_post132
        trigger /prod12/sref15/wrfarw_p4/jsref_wrf_fcst == active
      task jsref_wrf_gempak132
        trigger /prod12/sref15/wrfarw_p4/jsref_wrf_post132 == complete
      task jsref_wrf_bufr
        trigger /prod12/sref15/wrfarw_p4/jsref_wrf_fcst == active and /prod12/sref15/wrfarw_p4/jsref_wrf_post == active
    endfamily
    family wrfarw_n5
      edit MEM 'n5'
      edit MEM0 'n05'
      edit MOD 'arw'
      edit MODEL 'ARW'
      task jsref_wrf_prep
        trigger /prod12/sref15/getglb == complete
      task jsref_wrf_real
        trigger /prod12/sref15/wrfarw_n5/jsref_wrf_prep == complete and /prod12/sref15/wrfarw_ctl/jsref_wrf_prep == complete
      task jsref_wrf_fcst
        trigger /prod12/sref15/wrfarw_n5/jsref_wrf_real == complete
        edit NTASK '144'
      task jsref_wrf_post_hrly
        trigger /prod12/sref15/wrfarw_n5/jsref_wrf_fcst == active
      task jsref_wrf_post
        trigger /prod12/sref15/wrfarw_n5/jsref_wrf_fcst == active
      task jsref_wrf_post132
        trigger /prod12/sref15/wrfarw_n5/jsref_wrf_fcst == active
      task jsref_wrf_gempak
        trigger /prod12/sref15/wrfarw_n5/jsref_wrf_post == complete
      task jsref_wrf_gempak132
        trigger /prod12/sref15/wrfarw_n5/jsref_wrf_post132 == complete
      task jsref_wrf_bufr
        trigger /prod12/sref15/wrfarw_n5/jsref_wrf_fcst == active and /prod12/sref15/wrfarw_n5/jsref_wrf_post == active
    endfamily
    family wrfarw_p5
      edit MEM 'p5'
      edit MEM0 'p05'
      edit MOD 'arw'
      edit MODEL 'ARW'
      task jsref_wrf_prep
        trigger /prod12/sref15/getglb == complete
      task jsref_wrf_real
        trigger /prod12/sref15/wrfarw_p5/jsref_wrf_prep == complete and /prod12/sref15/wrfarw_ctl/jsref_wrf_prep == complete
      task jsref_wrf_fcst
        trigger /prod12/sref15/wrfarw_p5/jsref_wrf_real == complete
        edit NTASK '160'
      task jsref_wrf_post_hrly
        trigger /prod12/sref15/wrfarw_p5/jsref_wrf_fcst == active
      task jsref_wrf_post
        trigger /prod12/sref15/wrfarw_p5/jsref_wrf_fcst == active
      task jsref_wrf_gempak
        trigger /prod12/sref15/wrfarw_p5/jsref_wrf_post == complete
      task jsref_wrf_post132
        trigger /prod12/sref15/wrfarw_p5/jsref_wrf_fcst == active
      task jsref_wrf_gempak132
        trigger /prod12/sref15/wrfarw_p5/jsref_wrf_post132 == complete
      task jsref_wrf_bufr
        trigger /prod12/sref15/wrfarw_p5/jsref_wrf_fcst == active and /prod12/sref15/wrfarw_p5/jsref_wrf_post == active
    endfamily
    family wrfarw_n6
      edit MEM 'n6'
      edit MEM0 'n06'
      edit MOD 'arw'
      edit MODEL 'ARW'
      task jsref_wrf_prep
        trigger /prod12/sref15/getglb == complete
      task jsref_wrf_real
        trigger /prod12/sref15/wrfarw_n6/jsref_wrf_prep == complete and /prod12/sref15/wrfarw_ctl/jsref_wrf_prep == complete
      task jsref_wrf_fcst
        trigger /prod12/sref15/wrfarw_n6/jsref_wrf_real == complete
        edit NTASK '160'
      task jsref_wrf_post_hrly
        trigger /prod12/sref15/wrfarw_n6/jsref_wrf_fcst == active
      task jsref_wrf_post
        trigger /prod12/sref15/wrfarw_n6/jsref_wrf_fcst == active
      task jsref_wrf_post132
        trigger /prod12/sref15/wrfarw_n6/jsref_wrf_fcst == active
      task jsref_wrf_gempak
        trigger /prod12/sref15/wrfarw_n6/jsref_wrf_post == complete
      task jsref_wrf_gempak132
        trigger /prod12/sref15/wrfarw_n6/jsref_wrf_post132 == complete
      task jsref_wrf_bufr
        trigger /prod12/sref15/wrfarw_n6/jsref_wrf_fcst == active and /prod12/sref15/wrfarw_n6/jsref_wrf_post == active
    endfamily
    family wrfarw_p6
      edit MEM 'p6'
      edit MEM0 'p06'
      edit MOD 'arw'
      edit MODEL 'ARW'
      task jsref_wrf_prep
        trigger /prod12/sref15/getglb == complete
      task jsref_wrf_real
        trigger /prod12/sref15/wrfarw_p6/jsref_wrf_prep == complete and /prod12/sref15/wrfarw_ctl/jsref_wrf_prep == complete
      task jsref_wrf_fcst
        trigger /prod12/sref15/wrfarw_p6/jsref_wrf_real == complete
        edit NTASK '256'
      task jsref_wrf_post_hrly
        trigger /prod12/sref15/wrfarw_p6/jsref_wrf_fcst == active
      task jsref_wrf_post
        trigger /prod12/sref15/wrfarw_p6/jsref_wrf_fcst == active
      task jsref_wrf_gempak
        trigger /prod12/sref15/wrfarw_p6/jsref_wrf_post == complete
      task jsref_wrf_post132
        trigger /prod12/sref15/wrfarw_p6/jsref_wrf_fcst == active
      task jsref_wrf_gempak132
        trigger /prod12/sref15/wrfarw_p6/jsref_wrf_post132 == complete
      task jsref_wrf_bufr
        trigger /prod12/sref15/wrfarw_p6/jsref_wrf_fcst == active and /prod12/sref15/wrfarw_p6/jsref_wrf_post == active
    endfamily
    family misc
      task jsref_calfcsterr
        trigger /prod12/ndas18/time_minus_03hr/jndas_forecast == complete
      task jsref_qpfbiasestimate
        trigger /prod12/sref15/misc/jsref_calfcsterr == complete and /prod12/sref15/enspost/jsref_enspost:release_qpfbiasestimate
      task jsref_meanbufr
        trigger /prod12/sref15/wrfnmb_ctl/jsref_wrf_bufr == complete and /prod12/sref15/wrfnmb_p1/jsref_wrf_bufr == complete and /prod12/sref15/wrfnmb_p2/jsref_wrf_bufr == complete and /prod12/sref15/wrfnmb_p3/jsref_wrf_bufr == complete and /prod12/sref15/wrfnmb_p4/jsref_wrf_bufr == complete and /prod12/sref15/wrfnmb_p5/jsref_wrf_bufr == complete and /prod12/sref15/wrfnmb_p6/jsref_wrf_bufr == complete and /prod12/sref15/wrfnmb_n1/jsref_wrf_bufr == complete and /prod12/sref15/wrfnmb_n2/jsref_wrf_bufr == complete and /prod12/sref15/wrfnmb_n3/jsref_wrf_bufr == complete and /prod12/sref15/wrfnmb_n4/jsref_wrf_bufr == complete and /prod12/sref15/wrfnmb_n5/jsref_wrf_bufr == complete and /prod12/sref15/wrfnmb_n6/jsref_wrf_bufr == complete and /prod12/sref15/wrfarw_ctl/jsref_wrf_bufr == complete and /prod12/sref15/wrfarw_n1/jsref_wrf_bufr == complete and /prod12/sref15/wrfarw_p1/jsref_wrf_bufr == complete and /prod12/sref15/wrfarw_n2/jsref_wrf_bufr == complete and /prod12/sref15/wrfarw_p2/jsref_wrf_bufr == complete and /prod12/sref15/wrfarw_n3/jsref_wrf_bufr == complete and /prod12/sref15/wrfarw_p3/jsref_wrf_bufr == complete and /prod12/sref15/wrfarw_n4/jsref_wrf_bufr == complete and /prod12/sref15/wrfarw_p4/jsref_wrf_bufr == complete and /prod12/sref15/wrfarw_n5/jsref_wrf_bufr == complete and /prod12/sref15/wrfarw_p5/jsref_wrf_bufr == complete and /prod12/sref15/wrfarw_n6/jsref_wrf_bufr == complete and /prod12/sref15/wrfarw_p6/jsref_wrf_bufr == complete
      task jsref_biasestimate
        trigger /prod12/sref15/wrfarw_ctl/jsref_wrf_post == complete and /prod12/sref15/wrfarw_n1/jsref_wrf_post == complete and /prod12/sref15/wrfarw_p1/jsref_wrf_post == complete and /prod12/sref15/wrfarw_n2/jsref_wrf_post == complete and /prod12/sref15/wrfarw_p2/jsref_wrf_post == complete and /prod12/sref15/wrfarw_n3/jsref_wrf_post == complete and /prod12/sref15/wrfarw_p3/jsref_wrf_post == complete and /prod12/sref15/wrfarw_n4/jsref_wrf_post == complete and /prod12/sref15/wrfarw_p4/jsref_wrf_post == complete and /prod12/sref15/wrfarw_n5/jsref_wrf_post == complete and /prod12/sref15/wrfarw_p5/jsref_wrf_post == complete and /prod12/sref15/wrfarw_n6/jsref_wrf_post == complete and /prod12/sref15/wrfarw_p6/jsref_wrf_post == complete and /prod12/sref15/wrfnmb_ctl/jsref_wrf_post == complete and /prod12/sref15/wrfnmb_n1/jsref_wrf_post == complete and /prod12/sref15/wrfnmb_p1/jsref_wrf_post == complete and /prod12/sref15/wrfnmb_n2/jsref_wrf_post == complete and /prod12/sref15/wrfnmb_p2/jsref_wrf_post == complete and /prod12/sref15/wrfnmb_n3/jsref_wrf_post == complete and /prod12/sref15/wrfnmb_p3/jsref_wrf_post == complete and /prod12/sref15/wrfnmb_n4/jsref_wrf_post == complete and /prod12/sref15/wrfnmb_p4/jsref_wrf_post == complete and /prod12/sref15/wrfnmb_n5/jsref_wrf_post == complete and /prod12/sref15/wrfnmb_p5/jsref_wrf_post == complete and /prod12/sref15/wrfnmb_n6/jsref_wrf_post == complete and /prod12/sref15/wrfnmb_p6/jsref_wrf_post == complete
        event 1 release_biasc_gempak
      task jsref_cluster
        trigger /prod12/sref15/wrfnmb_ctl/jsref_wrf_post == complete and /prod12/sref15/wrfnmb_n1/jsref_wrf_post == complete and /prod12/sref15/wrfnmb_p1/jsref_wrf_post == complete and /prod12/sref15/wrfnmb_n2/jsref_wrf_post == complete and /prod12/sref15/wrfnmb_p2/jsref_wrf_post == complete and /prod12/sref15/wrfnmb_n3/jsref_wrf_post == complete and /prod12/sref15/wrfnmb_p3/jsref_wrf_post == complete and /prod12/sref15/wrfnmb_n4/jsref_wrf_post == complete and /prod12/sref15/wrfnmb_p4/jsref_wrf_post == complete and /prod12/sref15/wrfnmb_n5/jsref_wrf_post == complete and /prod12/sref15/wrfnmb_p5/jsref_wrf_post == complete and /prod12/sref15/wrfnmb_n6/jsref_wrf_post == complete and /prod12/sref15/wrfnmb_p6/jsref_wrf_post == complete and /prod12/sref15/wrfarw_ctl/jsref_wrf_post == complete and /prod12/sref15/wrfarw_n1/jsref_wrf_post == complete and /prod12/sref15/wrfarw_p1/jsref_wrf_post == complete and /prod12/sref15/wrfarw_n2/jsref_wrf_post == complete and /prod12/sref15/wrfarw_p2/jsref_wrf_post == complete and /prod12/sref15/wrfarw_n3/jsref_wrf_post == complete and /prod12/sref15/wrfarw_p3/jsref_wrf_post == complete and /prod12/sref15/wrfarw_n4/jsref_wrf_post == complete and /prod12/sref15/wrfarw_p4/jsref_wrf_post == complete and /prod12/sref15/wrfarw_n5/jsref_wrf_post == complete and /prod12/sref15/wrfarw_p5/jsref_wrf_post == complete and /prod12/sref15/wrfarw_n6/jsref_wrf_post == complete and /prod12/sref15/wrfarw_p6/jsref_wrf_post == complete
      task jsref_downscaling
        trigger /prod12/sref15/misc/jsref_biasestimate == complete
    endfamily
    task jsref_cleanup
      trigger /prod12/sref15/wrfarw_ctl == complete and /prod12/sref15/wrfarw_n1 == complete and /prod12/sref15/wrfarw_p1 == complete and /prod12/sref15/wrfarw_n2 == complete and /prod12/sref15/wrfarw_p2 == complete and /prod12/sref15/wrfarw_n3 == complete and /prod12/sref15/wrfarw_p3 == complete and /prod12/sref15/wrfarw_n4 == complete and /prod12/sref15/wrfarw_p4 == complete and /prod12/sref15/wrfarw_n5 == complete and /prod12/sref15/wrfarw_p5 == complete and /prod12/sref15/wrfarw_n6 == complete and /prod12/sref15/wrfarw_p6 == complete and /prod12/sref15/wrfnmb_ctl == complete and /prod12/sref15/wrfnmb_n1 == complete and /prod12/sref15/wrfnmb_p1 == complete and /prod12/sref15/wrfnmb_n2 == complete and /prod12/sref15/wrfnmb_p2 == complete and /prod12/sref15/wrfnmb_n3 == complete and /prod12/sref15/wrfnmb_p3 == complete and /prod12/sref15/wrfnmb_n4 == complete and /prod12/sref15/wrfnmb_p4 == complete and /prod12/sref15/wrfnmb_n5 == complete and /prod12/sref15/wrfnmb_p5 == complete and /prod12/sref15/wrfnmb_n6 == complete and /prod12/sref15/wrfnmb_n6 == complete and /prod12/sref15/misc/jsref_downscaling == complete
      edit QUEUESERV 'prod2_serv'
    family enspost
      task jsref_enspost_hrly
        trigger /prod12/sref15/wrfnmb_ctl/jsref_wrf_post_hrly == complete and /prod12/sref15/wrfnmb_n1/jsref_wrf_post_hrly == complete and /prod12/sref15/wrfnmb_p1/jsref_wrf_post_hrly == complete and /prod12/sref15/wrfnmb_n2/jsref_wrf_post_hrly == complete and /prod12/sref15/wrfnmb_p2/jsref_wrf_post_hrly == complete and /prod12/sref15/wrfnmb_n3/jsref_wrf_post_hrly == complete and /prod12/sref15/wrfnmb_p3/jsref_wrf_post_hrly == complete and /prod12/sref15/wrfnmb_n4/jsref_wrf_post_hrly == complete and /prod12/sref15/wrfnmb_p4/jsref_wrf_post_hrly == complete and /prod12/sref15/wrfnmb_n5/jsref_wrf_post_hrly == complete and /prod12/sref15/wrfnmb_p5/jsref_wrf_post_hrly == complete and /prod12/sref15/wrfnmb_n6/jsref_wrf_post_hrly == complete and /prod12/sref15/wrfnmb_p6/jsref_wrf_post_hrly == complete and /prod12/sref15/wrfarw_ctl/jsref_wrf_post_hrly == complete and /prod12/sref15/wrfarw_n1/jsref_wrf_post_hrly == complete and /prod12/sref15/wrfarw_p1/jsref_wrf_post_hrly == complete and /prod12/sref15/wrfarw_n2/jsref_wrf_post_hrly == complete and /prod12/sref15/wrfarw_p2/jsref_wrf_post_hrly == complete and /prod12/sref15/wrfarw_n3/jsref_wrf_post_hrly == complete and /prod12/sref15/wrfarw_p3/jsref_wrf_post_hrly == complete and /prod12/sref15/wrfarw_n4/jsref_wrf_post_hrly == complete and /prod12/sref15/wrfarw_p4/jsref_wrf_post_hrly == complete and /prod12/sref15/wrfarw_n5/jsref_wrf_post_hrly == complete and /prod12/sref15/wrfarw_p5/jsref_wrf_post_hrly == complete and /prod12/sref15/wrfarw_n6/jsref_wrf_post_hrly == complete and /prod12/sref15/wrfarw_p6/jsref_wrf_post_hrly == complete
      task jsref_awips
        trigger /prod12/sref15/enspost/jsref_enspost:ensgrd_212_ready and /prod12/sref15/enspost/jsref_enspost:ensgrd_243_ready and /prod12/sref15/enspost/jsref_enspost:ensgrd_216_ready
      task jsref_enspost
        trigger /prod12/sref15/wrfnmb_ctl/jsref_wrf_post == complete and /prod12/sref15/wrfnmb_n1/jsref_wrf_post == complete and /prod12/sref15/wrfnmb_p1/jsref_wrf_post == complete and /prod12/sref15/wrfnmb_n2/jsref_wrf_post == complete and /prod12/sref15/wrfnmb_p2/jsref_wrf_post == complete and /prod12/sref15/wrfnmb_n3/jsref_wrf_post == complete and /prod12/sref15/wrfnmb_p3/jsref_wrf_post == complete and /prod12/sref15/wrfnmb_n4/jsref_wrf_post == complete and /prod12/sref15/wrfnmb_p4/jsref_wrf_post == complete and /prod12/sref15/wrfnmb_n5/jsref_wrf_post == complete and /prod12/sref15/wrfnmb_p5/jsref_wrf_post == complete and /prod12/sref15/wrfnmb_n6/jsref_wrf_post == complete and /prod12/sref15/wrfnmb_p6/jsref_wrf_post == complete and /prod12/sref15/wrfarw_ctl/jsref_wrf_post == complete and /prod12/sref15/wrfarw_n1/jsref_wrf_post == complete and /prod12/sref15/wrfarw_p1/jsref_wrf_post == complete and /prod12/sref15/wrfarw_n2/jsref_wrf_post == complete and /prod12/sref15/wrfarw_p2/jsref_wrf_post == complete and /prod12/sref15/wrfarw_n3/jsref_wrf_post == complete and /prod12/sref15/wrfarw_p3/jsref_wrf_post == complete and /prod12/sref15/wrfarw_n4/jsref_wrf_post == complete and /prod12/sref15/wrfarw_p4/jsref_wrf_post == complete and /prod12/sref15/wrfarw_n5/jsref_wrf_post == complete and /prod12/sref15/wrfarw_p5/jsref_wrf_post == complete and /prod12/sref15/wrfarw_n6/jsref_wrf_post == complete and /prod12/sref15/wrfarw_p6/jsref_wrf_post == complete
        event 1 release_qpfbiasestimate
        event 2 ensgrd_212_ready
        event 3 ensgrd_243_ready
        event 4 ensgrd_216_ready
      family grid132
        task jsref_enspost_g132_01
          trigger /prod12/sref15/wrfnmb_ctl/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n1/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p1/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n2/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p2/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n3/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p3/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n4/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p4/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n5/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p5/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n6/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p6/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_ctl/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n1/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p1/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n2/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p2/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n3/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p3/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n4/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p4/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n5/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p5/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n6/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p6/jsref_wrf_post132 == complete
        task jsref_enspost_g132_02
          trigger /prod12/sref15/wrfnmb_ctl/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n1/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p1/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n2/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p2/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n3/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p3/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n4/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p4/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n5/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p5/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n6/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p6/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_ctl/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n1/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p1/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n2/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p2/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n3/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p3/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n4/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p4/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n5/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p5/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n6/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p6/jsref_wrf_post132 == complete
        task jsref_enspost_g132_03
          trigger /prod12/sref15/wrfnmb_ctl/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n1/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p1/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n2/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p2/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n3/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p3/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n4/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p4/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n5/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p5/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n6/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p6/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_ctl/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n1/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p1/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n2/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p2/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n3/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p3/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n4/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p4/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n5/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p5/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n6/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p6/jsref_wrf_post132 == complete
        task jsref_enspost_g132_04
          trigger /prod12/sref15/wrfnmb_ctl/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n1/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p1/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n2/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p2/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n3/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p3/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n4/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p4/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n5/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p5/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n6/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p6/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_ctl/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n1/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p1/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n2/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p2/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n3/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p3/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n4/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p4/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n5/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p5/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n6/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p6/jsref_wrf_post132 == complete
        task jsref_enspost_g132_05
          trigger /prod12/sref15/wrfnmb_ctl/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n1/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p1/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n2/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p2/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n3/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p3/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n4/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p4/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n5/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p5/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n6/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p6/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_ctl/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n1/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p1/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n2/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p2/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n3/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p3/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n4/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p4/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n5/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p5/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n6/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p6/jsref_wrf_post132 == complete
        task jsref_enspost_g132_06
          trigger /prod12/sref15/wrfnmb_ctl/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n1/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p1/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n2/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p2/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n3/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p3/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n4/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p4/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n5/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p5/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n6/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p6/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_ctl/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n1/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p1/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n2/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p2/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n3/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p3/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n4/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p4/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n5/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p5/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n6/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p6/jsref_wrf_post132 == complete
        task jsref_enspost_g132_07
          trigger /prod12/sref15/wrfnmb_ctl/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n1/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p1/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n2/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p2/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n3/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p3/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n4/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p4/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n5/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p5/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n6/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p6/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_ctl/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n1/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p1/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n2/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p2/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n3/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p3/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n4/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p4/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n5/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p5/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n6/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p6/jsref_wrf_post132 == complete
        task jsref_enspost_g132_08
          trigger /prod12/sref15/wrfnmb_ctl/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n1/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p1/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n2/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p2/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n3/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p3/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n4/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p4/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n5/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p5/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n6/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p6/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_ctl/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n1/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p1/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n2/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p2/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n3/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p3/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n4/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p4/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n5/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p5/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n6/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p6/jsref_wrf_post132 == complete
        task jsref_enspost_g132_09
          trigger /prod12/sref15/wrfnmb_ctl/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n1/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p1/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n2/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p2/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n3/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p3/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n4/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p4/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n5/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p5/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n6/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p6/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_ctl/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n1/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p1/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n2/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p2/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n3/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p3/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n4/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p4/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n5/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p5/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n6/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p6/jsref_wrf_post132 == complete
        task jsref_enspost_g132_10
          trigger /prod12/sref15/wrfnmb_ctl/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n1/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p1/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n2/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p2/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n3/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p3/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n4/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p4/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n5/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p5/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n6/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p6/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_ctl/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n1/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p1/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n2/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p2/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n3/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p3/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n4/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p4/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n5/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p5/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n6/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p6/jsref_wrf_post132 == complete
        task jsref_enspost_g132_11
          trigger /prod12/sref15/wrfnmb_ctl/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n1/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p1/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n2/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p2/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n3/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p3/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n4/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p4/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n5/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p5/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n6/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p6/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_ctl/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n1/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p1/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n2/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p2/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n3/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p3/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n4/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p4/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n5/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p5/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n6/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p6/jsref_wrf_post132 == complete
        task jsref_enspost_g132_12
          trigger /prod12/sref15/wrfnmb_ctl/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n1/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p1/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n2/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p2/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n3/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p3/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n4/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p4/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n5/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p5/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n6/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p6/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_ctl/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n1/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p1/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n2/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p2/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n3/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p3/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n4/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p4/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n5/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p5/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n6/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p6/jsref_wrf_post132 == complete
        task jsref_enspost_g132_13
          trigger /prod12/sref15/wrfnmb_ctl/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n1/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p1/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n2/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p2/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n3/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p3/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n4/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p4/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n5/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p5/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n6/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p6/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_ctl/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n1/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p1/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n2/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p2/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n3/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p3/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n4/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p4/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n5/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p5/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n6/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p6/jsref_wrf_post132 == complete
        task jsref_enspost_g132_14
          trigger /prod12/sref15/wrfnmb_ctl/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n1/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p1/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n2/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p2/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n3/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p3/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n4/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p4/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n5/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p5/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n6/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p6/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_ctl/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n1/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p1/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n2/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p2/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n3/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p3/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n4/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p4/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n5/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p5/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n6/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p6/jsref_wrf_post132 == complete
        task jsref_enspost_g132_15
          trigger /prod12/sref15/wrfnmb_ctl/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n1/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p1/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n2/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p2/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n3/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p3/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n4/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p4/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n5/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p5/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n6/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p6/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_ctl/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n1/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p1/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n2/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p2/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n3/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p3/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n4/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p4/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n5/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p5/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n6/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p6/jsref_wrf_post132 == complete
        task jsref_enspost_g132_16
          trigger /prod12/sref15/wrfnmb_ctl/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n1/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p1/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n2/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p2/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n3/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p3/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n4/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p4/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n5/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p5/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n6/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p6/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_ctl/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n1/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p1/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n2/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p2/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n3/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p3/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n4/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p4/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n5/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p5/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n6/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p6/jsref_wrf_post132 == complete
        task jsref_enspost_g132_17
          trigger /prod12/sref15/wrfnmb_ctl/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n1/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p1/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n2/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p2/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n3/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p3/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n4/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p4/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n5/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p5/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n6/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p6/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_ctl/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n1/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p1/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n2/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p2/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n3/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p3/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n4/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p4/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n5/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p5/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n6/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p6/jsref_wrf_post132 == complete
        task jsref_enspost_g132_18
          trigger /prod12/sref15/wrfnmb_ctl/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n1/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p1/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n2/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p2/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n3/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p3/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n4/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p4/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n5/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p5/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n6/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p6/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_ctl/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n1/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p1/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n2/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p2/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n3/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p3/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n4/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p4/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n5/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p5/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n6/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p6/jsref_wrf_post132 == complete
        task jsref_enspost_g132_19
          trigger /prod12/sref15/wrfnmb_ctl/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n1/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p1/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n2/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p2/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n3/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p3/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n4/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p4/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n5/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p5/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n6/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p6/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_ctl/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n1/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p1/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n2/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p2/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n3/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p3/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n4/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p4/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n5/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p5/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n6/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p6/jsref_wrf_post132 == complete
        task jsref_enspost_g132_20
          trigger /prod12/sref15/wrfnmb_ctl/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n1/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p1/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n2/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p2/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n3/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p3/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n4/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p4/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n5/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p5/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n6/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p6/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_ctl/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n1/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p1/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n2/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p2/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n3/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p3/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n4/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p4/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n5/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p5/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n6/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p6/jsref_wrf_post132 == complete
        task jsref_enspost_g132_21
          trigger /prod12/sref15/wrfnmb_ctl/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n1/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p1/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n2/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p2/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n3/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p3/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n4/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p4/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n5/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p5/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n6/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p6/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_ctl/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n1/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p1/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n2/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p2/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n3/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p3/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n4/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p4/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n5/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p5/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n6/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p6/jsref_wrf_post132 == complete
        task jsref_enspost_g132_22
          trigger /prod12/sref15/wrfnmb_ctl/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n1/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p1/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n2/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p2/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n3/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p3/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n4/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p4/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n5/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p5/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n6/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p6/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_ctl/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n1/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p1/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n2/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p2/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n3/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p3/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n4/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p4/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n5/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p5/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n6/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p6/jsref_wrf_post132 == complete
        task jsref_enspost_g132_23
          trigger /prod12/sref15/wrfnmb_ctl/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n1/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p1/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n2/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p2/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n3/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p3/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n4/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p4/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n5/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p5/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n6/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p6/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_ctl/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n1/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p1/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n2/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p2/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n3/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p3/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n4/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p4/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n5/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p5/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n6/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p6/jsref_wrf_post132 == complete
        task jsref_enspost_g132_24
          trigger /prod12/sref15/wrfnmb_ctl/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n1/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p1/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n2/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p2/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n3/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p3/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n4/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p4/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n5/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p5/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n6/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p6/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_ctl/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n1/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p1/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n2/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p2/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n3/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p3/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n4/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p4/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n5/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p5/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n6/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p6/jsref_wrf_post132 == complete
        task jsref_enspost_g132_25
          trigger /prod12/sref15/wrfnmb_ctl/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n1/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p1/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n2/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p2/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n3/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p3/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n4/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p4/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n5/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p5/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n6/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p6/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_ctl/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n1/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p1/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n2/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p2/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n3/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p3/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n4/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p4/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n5/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p5/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n6/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p6/jsref_wrf_post132 == complete
        task jsref_enspost_g132_26
          trigger /prod12/sref15/wrfnmb_ctl/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n1/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p1/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n2/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p2/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n3/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p3/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n4/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p4/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n5/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p5/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n6/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p6/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_ctl/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n1/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p1/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n2/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p2/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n3/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p3/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n4/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p4/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n5/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p5/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n6/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p6/jsref_wrf_post132 == complete
        task jsref_enspost_g132_27
          trigger /prod12/sref15/wrfnmb_ctl/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n1/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p1/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n2/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p2/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n3/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p3/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n4/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p4/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n5/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p5/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n6/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p6/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_ctl/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n1/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p1/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n2/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p2/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n3/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p3/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n4/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p4/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n5/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p5/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n6/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p6/jsref_wrf_post132 == complete
        task jsref_enspost_g132_28
          trigger /prod12/sref15/wrfnmb_ctl/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n1/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p1/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n2/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p2/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n3/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p3/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n4/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p4/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n5/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p5/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n6/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p6/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_ctl/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n1/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p1/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n2/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p2/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n3/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p3/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n4/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p4/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n5/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p5/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n6/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p6/jsref_wrf_post132 == complete
        task jsref_enspost_g132_29
          trigger /prod12/sref15/wrfnmb_ctl/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n1/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p1/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n2/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p2/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n3/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p3/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n4/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p4/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n5/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p5/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n6/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p6/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_ctl/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n1/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p1/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n2/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p2/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n3/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p3/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n4/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p4/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n5/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p5/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n6/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p6/jsref_wrf_post132 == complete
        task jsref_enspost_g132_30
          trigger /prod12/sref15/wrfnmb_ctl/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n1/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p1/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n2/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p2/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n3/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p3/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n4/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p4/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n5/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p5/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_n6/jsref_wrf_post132 == complete and /prod12/sref15/wrfnmb_p6/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_ctl/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n1/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p1/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n2/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p2/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n3/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p3/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n4/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p4/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n5/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p5/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_n6/jsref_wrf_post132 == complete and /prod12/sref15/wrfarw_p6/jsref_wrf_post132 == complete
      endfamily
    endfamily
    family gempak
      task jsref_gempak
        trigger /prod12/sref15/enspost/jsref_enspost:ensgrd_212_ready and /prod12/sref15/enspost/jsref_enspost:ensgrd_216_ready and /prod12/sref15/enspost/jsref_enspost:ensgrd_243_ready
      task jsref_biasc_gempak
        trigger /prod12/sref15/misc/jsref_biasestimate:release_biasc_gempak
      task jsref_cluster_gempak
        trigger /prod12/sref15/misc/jsref_cluster == complete
      task jsref_gempak132
        trigger /prod12/sref15/enspost/grid132 == complete
    endfamily
  endfamily
  family spcsref15
    edit ECF_FILES '/ecf/ecfnets/scripts/spcsref'
    edit PROJ 'SREF-OPS'
    edit COM 'com2'
    edit QUEUE 'prod2'
    edit CYC '15'
    edit ECF_JOB_CMD '/ecf/ecfutils/unixsubmit %ECF_JOB% %ECF_JOBOUT% ibmsp'
    task jspcsref_gempak_postmgr
      trigger ( /prod12/sref15/wrfarw_ctl/jsref_wrf_post == active or /prod12/sref15/wrfarw_ctl/jsref_wrf_post == complete ) and ( /prod12/sref15/wrfarw_n1/jsref_wrf_post == active or /prod12/sref15/wrfarw_n1/jsref_wrf_post == complete ) and ( /prod12/sref15/wrfarw_n2/jsref_wrf_post == active or /prod12/sref15/wrfarw_n2/jsref_wrf_post == complete ) and ( /prod12/sref15/wrfarw_n3/jsref_wrf_post == active or /prod12/sref15/wrfarw_n3/jsref_wrf_post == complete ) and ( /prod12/sref15/wrfarw_n4/jsref_wrf_post == active or /prod12/sref15/wrfarw_n4/jsref_wrf_post == complete ) and ( /prod12/sref15/wrfarw_n5/jsref_wrf_post == active or /prod12/sref15/wrfarw_n5/jsref_wrf_post == complete ) and ( /prod12/sref15/wrfarw_n6/jsref_wrf_post == active or /prod12/sref15/wrfarw_n6/jsref_wrf_post == complete ) and ( /prod12/sref15/wrfarw_p1/jsref_wrf_post == active or /prod12/sref15/wrfarw_p1/jsref_wrf_post == complete ) and ( /prod12/sref15/wrfarw_p2/jsref_wrf_post == active or /prod12/sref15/wrfarw_p2/jsref_wrf_post == complete ) and ( /prod12/sref15/wrfarw_p3/jsref_wrf_post == active or /prod12/sref15/wrfarw_p3/jsref_wrf_post == complete ) and ( /prod12/sref15/wrfarw_p4/jsref_wrf_post == active or /prod12/sref15/wrfarw_p4/jsref_wrf_post == complete ) and ( /prod12/sref15/wrfarw_p5/jsref_wrf_post == active or /prod12/sref15/wrfarw_p5/jsref_wrf_post == complete ) and ( /prod12/sref15/wrfarw_p6/jsref_wrf_post == active or /prod12/sref15/wrfarw_p6/jsref_wrf_post == complete ) and ( /prod12/sref15/wrfnmb_ctl/jsref_wrf_post == active or /prod12/sref15/wrfnmb_ctl/jsref_wrf_post == complete ) and ( /prod12/sref15/wrfnmb_n1/jsref_wrf_post == active or /prod12/sref15/wrfnmb_n1/jsref_wrf_post == complete ) and ( /prod12/sref15/wrfnmb_n2/jsref_wrf_post == active or /prod12/sref15/wrfnmb_n2/jsref_wrf_post == complete ) and ( /prod12/sref15/wrfnmb_n3/jsref_wrf_post == active or /prod12/sref15/wrfnmb_n3/jsref_wrf_post == complete ) and ( /prod12/sref15/wrfnmb_n4/jsref_wrf_post == active or /prod12/sref15/wrfnmb_n4/jsref_wrf_post == complete ) and ( /prod12/sref15/wrfnmb_n5/jsref_wrf_post == active or /prod12/sref15/wrfnmb_n5/jsref_wrf_post == complete ) and ( /prod12/sref15/wrfnmb_n6/jsref_wrf_post == active or /prod12/sref15/wrfnmb_n6/jsref_wrf_post == complete ) and ( /prod12/sref15/wrfnmb_p1/jsref_wrf_post == active or /prod12/sref15/wrfnmb_p1/jsref_wrf_post == complete ) and ( /prod12/sref15/wrfnmb_p2/jsref_wrf_post == active or /prod12/sref15/wrfnmb_p2/jsref_wrf_post == complete ) and ( /prod12/sref15/wrfnmb_p3/jsref_wrf_post == active or /prod12/sref15/wrfnmb_p3/jsref_wrf_post == complete ) and ( /prod12/sref15/wrfnmb_p4/jsref_wrf_post == active or /prod12/sref15/wrfnmb_p4/jsref_wrf_post == complete ) and ( /prod12/sref15/wrfnmb_p5/jsref_wrf_post == active or /prod12/sref15/wrfnmb_p5/jsref_wrf_post == complete ) and ( /prod12/sref15/wrfnmb_p6/jsref_wrf_post == active or /prod12/sref15/wrfnmb_p6/jsref_wrf_post == complete )
      event 1 f00_done
      event 2 f01_done
      event 3 f02_done
      event 4 f03_done
      event 5 f04_done
      event 6 f05_done
      event 7 f06_done
      event 8 f07_done
      event 9 f08_done
      event 10 f09_done
      event 11 f10_done
      event 12 f11_done
      event 13 f12_done
      event 14 f13_done
      event 15 f14_done
      event 16 f15_done
      event 17 f16_done
      event 18 f17_done
      event 19 f18_done
      event 20 f19_done
      event 21 f20_done
      event 22 f21_done
      event 23 f22_done
      event 24 f23_done
      event 25 f24_done
      event 26 f25_done
      event 27 f26_done
      event 28 f27_done
      event 29 f28_done
      event 30 f29_done
      event 31 f30_done
      event 32 f31_done
      event 33 f32_done
      event 34 f33_done
      event 35 f34_done
      event 36 f35_done
      event 37 f36_done
      event 38 f37_done
      event 39 f38_done
      event 40 f39_done
      event 41 f42_done
      event 42 f45_done
      event 43 f48_done
      event 44 f51_done
      event 45 f54_done
      event 46 f57_done
      event 47 f60_done
      event 48 f63_done
      event 49 f66_done
      event 50 f69_done
      event 51 f72_done
      event 52 f75_done
      event 53 f78_done
      event 54 f81_done
      event 55 f84_done
      event 56 f87_done
    family pgrb212_gempak
      task jspcsref_pgrb212_gempak_f00
        trigger /prod12/spcsref15/jspcsref_gempak_postmgr:f00_done
      task jspcsref_pgrb212_gempak_f01
        trigger /prod12/spcsref15/jspcsref_gempak_postmgr:f01_done
      task jspcsref_pgrb212_gempak_f02
        trigger /prod12/spcsref15/jspcsref_gempak_postmgr:f02_done
      task jspcsref_pgrb212_gempak_f03
        trigger /prod12/spcsref15/jspcsref_gempak_postmgr:f03_done
      task jspcsref_pgrb212_gempak_f04
        trigger /prod12/spcsref15/jspcsref_gempak_postmgr:f04_done
      task jspcsref_pgrb212_gempak_f05
        trigger /prod12/spcsref15/jspcsref_gempak_postmgr:f05_done
      task jspcsref_pgrb212_gempak_f06
        trigger /prod12/spcsref15/jspcsref_gempak_postmgr:f06_done
      task jspcsref_pgrb212_gempak_f07
        trigger /prod12/spcsref15/jspcsref_gempak_postmgr:f07_done
      task jspcsref_pgrb212_gempak_f08
        trigger /prod12/spcsref15/jspcsref_gempak_postmgr:f08_done
      task jspcsref_pgrb212_gempak_f09
        trigger /prod12/spcsref15/jspcsref_gempak_postmgr:f09_done
      task jspcsref_pgrb212_gempak_f10
        trigger /prod12/spcsref15/jspcsref_gempak_postmgr:f10_done
      task jspcsref_pgrb212_gempak_f11
        trigger /prod12/spcsref15/jspcsref_gempak_postmgr:f11_done
      task jspcsref_pgrb212_gempak_f12
        trigger /prod12/spcsref15/jspcsref_gempak_postmgr:f12_done
      task jspcsref_pgrb212_gempak_f13
        trigger /prod12/spcsref15/jspcsref_gempak_postmgr:f13_done
      task jspcsref_pgrb212_gempak_f14
        trigger /prod12/spcsref15/jspcsref_gempak_postmgr:f14_done
      task jspcsref_pgrb212_gempak_f15
        trigger /prod12/spcsref15/jspcsref_gempak_postmgr:f15_done
      task jspcsref_pgrb212_gempak_f16
        trigger /prod12/spcsref15/jspcsref_gempak_postmgr:f16_done
      task jspcsref_pgrb212_gempak_f17
        trigger /prod12/spcsref15/jspcsref_gempak_postmgr:f17_done
      task jspcsref_pgrb212_gempak_f18
        trigger /prod12/spcsref15/jspcsref_gempak_postmgr:f18_done
      task jspcsref_pgrb212_gempak_f19
        trigger /prod12/spcsref15/jspcsref_gempak_postmgr:f19_done
      task jspcsref_pgrb212_gempak_f20
        trigger /prod12/spcsref15/jspcsref_gempak_postmgr:f20_done
      task jspcsref_pgrb212_gempak_f21
        trigger /prod12/spcsref15/jspcsref_gempak_postmgr:f21_done
      task jspcsref_pgrb212_gempak_f22
        trigger /prod12/spcsref15/jspcsref_gempak_postmgr:f22_done
      task jspcsref_pgrb212_gempak_f23
        trigger /prod12/spcsref15/jspcsref_gempak_postmgr:f23_done
      task jspcsref_pgrb212_gempak_f24
        trigger /prod12/spcsref15/jspcsref_gempak_postmgr:f24_done
      task jspcsref_pgrb212_gempak_f25
        trigger /prod12/spcsref15/jspcsref_gempak_postmgr:f25_done
      task jspcsref_pgrb212_gempak_f26
        trigger /prod12/spcsref15/jspcsref_gempak_postmgr:f26_done
      task jspcsref_pgrb212_gempak_f27
        trigger /prod12/spcsref15/jspcsref_gempak_postmgr:f27_done
      task jspcsref_pgrb212_gempak_f28
        trigger /prod12/spcsref15/jspcsref_gempak_postmgr:f28_done
      task jspcsref_pgrb212_gempak_f29
        trigger /prod12/spcsref15/jspcsref_gempak_postmgr:f29_done
      task jspcsref_pgrb212_gempak_f30
        trigger /prod12/spcsref15/jspcsref_gempak_postmgr:f30_done
      task jspcsref_pgrb212_gempak_f31
        trigger /prod12/spcsref15/jspcsref_gempak_postmgr:f31_done
      task jspcsref_pgrb212_gempak_f32
        trigger /prod12/spcsref15/jspcsref_gempak_postmgr:f32_done
      task jspcsref_pgrb212_gempak_f33
        trigger /prod12/spcsref15/jspcsref_gempak_postmgr:f33_done
      task jspcsref_pgrb212_gempak_f34
        trigger /prod12/spcsref15/jspcsref_gempak_postmgr:f34_done
      task jspcsref_pgrb212_gempak_f35
        trigger /prod12/spcsref15/jspcsref_gempak_postmgr:f35_done
      task jspcsref_pgrb212_gempak_f36
        trigger /prod12/spcsref15/jspcsref_gempak_postmgr:f36_done
      task jspcsref_pgrb212_gempak_f37
        trigger /prod12/spcsref15/jspcsref_gempak_postmgr:f37_done
      task jspcsref_pgrb212_gempak_f38
        trigger /prod12/spcsref15/jspcsref_gempak_postmgr:f38_done
      task jspcsref_pgrb212_gempak_f39
        trigger /prod12/spcsref15/jspcsref_gempak_postmgr:f39_done
      task jspcsref_pgrb212_gempak_f42
        trigger /prod12/spcsref15/jspcsref_gempak_postmgr:f42_done
      task jspcsref_pgrb212_gempak_f45
        trigger /prod12/spcsref15/jspcsref_gempak_postmgr:f45_done
      task jspcsref_pgrb212_gempak_f48
        trigger /prod12/spcsref15/jspcsref_gempak_postmgr:f48_done
      task jspcsref_pgrb212_gempak_f51
        trigger /prod12/spcsref15/jspcsref_gempak_postmgr:f51_done
      task jspcsref_pgrb212_gempak_f54
        trigger /prod12/spcsref15/jspcsref_gempak_postmgr:f54_done
      task jspcsref_pgrb212_gempak_f57
        trigger /prod12/spcsref15/jspcsref_gempak_postmgr:f57_done
      task jspcsref_pgrb212_gempak_f60
        trigger /prod12/spcsref15/jspcsref_gempak_postmgr:f60_done
      task jspcsref_pgrb212_gempak_f63
        trigger /prod12/spcsref15/jspcsref_gempak_postmgr:f63_done
      task jspcsref_pgrb212_gempak_f66
        trigger /prod12/spcsref15/jspcsref_gempak_postmgr:f66_done
      task jspcsref_pgrb212_gempak_f69
        trigger /prod12/spcsref15/jspcsref_gempak_postmgr:f69_done
      task jspcsref_pgrb212_gempak_f72
        trigger /prod12/spcsref15/jspcsref_gempak_postmgr:f72_done
      task jspcsref_pgrb212_gempak_f75
        trigger /prod12/spcsref15/jspcsref_gempak_postmgr:f75_done
      task jspcsref_pgrb212_gempak_f78
        trigger /prod12/spcsref15/jspcsref_gempak_postmgr:f78_done
      task jspcsref_pgrb212_gempak_f81
        trigger /prod12/spcsref15/jspcsref_gempak_postmgr:f81_done
      task jspcsref_pgrb212_gempak_f84
        trigger /prod12/spcsref15/jspcsref_gempak_postmgr:f84_done
      task jspcsref_pgrb212_gempak_f87
        trigger /prod12/spcsref15/jspcsref_gempak_postmgr:f87_done
    endfamily
    family xhail_gempak
      task jspcsref_xhail_gempak_f00
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f00 == complete and /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f03 == complete
      task jspcsref_xhail_gempak_f03
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f00 == complete and /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f03 == complete and /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f06 == complete
      task jspcsref_xhail_gempak_f06
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f03 == complete and /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f06 == complete and /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f09 == complete
      task jspcsref_xhail_gempak_f09
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f06 == complete and /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f09 == complete and /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f12 == complete
      task jspcsref_xhail_gempak_f12
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f09 == complete and /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f12 == complete and /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f15 == complete
      task jspcsref_xhail_gempak_f15
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f12 == complete and /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f15 == complete and /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f18 == complete
      task jspcsref_xhail_gempak_f18
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f15 == complete and /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f18 == complete and /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f21 == complete
      task jspcsref_xhail_gempak_f21
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f18 == complete and /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f21 == complete and /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f24 == complete
      task jspcsref_xhail_gempak_f24
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f27 == complete and /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f21 == complete and /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f24 == complete
      task jspcsref_xhail_gempak_f27
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f27 == complete and /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f30 == complete and /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f24 == complete
      task jspcsref_xhail_gempak_f30
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f27 == complete and /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f30 == complete and /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f33 == complete
      task jspcsref_xhail_gempak_f33
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f36 == complete and /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f30 == complete and /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f33 == complete
      task jspcsref_xhail_gempak_f36
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f36 == complete and /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f39 == complete and /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f33 == complete
      task jspcsref_xhail_gempak_f39
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f36 == complete and /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f39 == complete and /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f42 == complete
      task jspcsref_xhail_gempak_f42
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f45 == complete and /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f39 == complete and /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f42 == complete
      task jspcsref_xhail_gempak_f45
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f45 == complete and /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f48 == complete and /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f42 == complete
      task jspcsref_xhail_gempak_f48
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f45 == complete and /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f48 == complete and /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f51 == complete
      task jspcsref_xhail_gempak_f51
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f54 == complete and /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f48 == complete and /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f51 == complete
      task jspcsref_xhail_gempak_f54
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f54 == complete and /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f57 == complete and /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f51 == complete
      task jspcsref_xhail_gempak_f57
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f54 == complete and /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f57 == complete and /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f60 == complete
      task jspcsref_xhail_gempak_f60
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f63 == complete and /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f57 == complete and /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f60 == complete
      task jspcsref_xhail_gempak_f63
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f63 == complete and /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f66 == complete and /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f60 == complete
      task jspcsref_xhail_gempak_f66
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f63 == complete and /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f66 == complete and /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f69 == complete
      task jspcsref_xhail_gempak_f69
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f72 == complete and /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f66 == complete and /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f69 == complete
      task jspcsref_xhail_gempak_f72
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f72 == complete and /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f75 == complete and /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f69 == complete
      task jspcsref_xhail_gempak_f75
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f72 == complete and /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f75 == complete and /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f78 == complete
      task jspcsref_xhail_gempak_f78
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f81 == complete and /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f75 == complete and /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f78 == complete
      task jspcsref_xhail_gempak_f81
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f81 == complete and /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f84 == complete and /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f78 == complete
      task jspcsref_xhail_gempak_f84
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f81 == complete and /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f84 == complete and /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f87 == complete
      task jspcsref_xhail_gempak_f87
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f87 == complete and /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f84 == complete
    endfamily
    family gempak
      task jspcsref_gempak_f00
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f00 == complete
      task jspcsref_gempak_f01
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f01 == complete
      task jspcsref_gempak_f02
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f02 == complete
      task jspcsref_gempak_f03
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f03 == complete
      task jspcsref_gempak_f04
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f04 == complete
      task jspcsref_gempak_f05
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f05 == complete
      task jspcsref_gempak_f06
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f06 == complete
      task jspcsref_gempak_f07
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f07 == complete
      task jspcsref_gempak_f08
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f08 == complete
      task jspcsref_gempak_f09
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f09 == complete
      task jspcsref_gempak_f10
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f10 == complete
      task jspcsref_gempak_f11
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f11 == complete
      task jspcsref_gempak_f12
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f12 == complete
      task jspcsref_gempak_f13
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f13 == complete and /prod12/spcsref15/gempak/jspcsref_gempak_f03 == complete
      task jspcsref_gempak_f14
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f14 == complete and /prod12/spcsref15/gempak/jspcsref_gempak_f03 == complete
      task jspcsref_gempak_f15
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f15 == complete and /prod12/spcsref15/gempak/jspcsref_gempak_f03 == complete
      task jspcsref_gempak_f16
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f16 == complete and /prod12/spcsref15/gempak/jspcsref_gempak_f06 == complete
      task jspcsref_gempak_f17
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f17 == complete and /prod12/spcsref15/gempak/jspcsref_gempak_f06 == complete
      task jspcsref_gempak_f18
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f18 == complete and /prod12/spcsref15/gempak/jspcsref_gempak_f06 == complete
      task jspcsref_gempak_f19
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f19 == complete and /prod12/spcsref15/gempak/jspcsref_gempak_f09 == complete
      task jspcsref_gempak_f20
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f20 == complete and /prod12/spcsref15/gempak/jspcsref_gempak_f09 == complete
      task jspcsref_gempak_f21
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f21 == complete and /prod12/spcsref15/gempak/jspcsref_gempak_f09 == complete
      task jspcsref_gempak_f22
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f22 == complete and /prod12/spcsref15/gempak/jspcsref_gempak_f12 == complete
      task jspcsref_gempak_f23
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f23 == complete and /prod12/spcsref15/gempak/jspcsref_gempak_f12 == complete
      task jspcsref_gempak_f24
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f24 == complete and /prod12/spcsref15/gempak/jspcsref_gempak_f12 == complete
      task jspcsref_gempak_f25
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f25 == complete and /prod12/spcsref15/gempak/jspcsref_gempak_f15 == complete
      task jspcsref_gempak_f26
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f26 == complete and /prod12/spcsref15/gempak/jspcsref_gempak_f15 == complete
      task jspcsref_gempak_f27
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f27 == complete and /prod12/spcsref15/gempak/jspcsref_gempak_f15 == complete
      task jspcsref_gempak_f28
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f28 == complete and /prod12/spcsref15/gempak/jspcsref_gempak_f18 == complete
      task jspcsref_gempak_f29
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f29 == complete and /prod12/spcsref15/gempak/jspcsref_gempak_f18 == complete
      task jspcsref_gempak_f30
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f30 == complete and /prod12/spcsref15/gempak/jspcsref_gempak_f18 == complete
      task jspcsref_gempak_f31
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f31 == complete and /prod12/spcsref15/gempak/jspcsref_gempak_f21 == complete
      task jspcsref_gempak_f32
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f32 == complete and /prod12/spcsref15/gempak/jspcsref_gempak_f21 == complete
      task jspcsref_gempak_f33
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f33 == complete and /prod12/spcsref15/gempak/jspcsref_gempak_f21 == complete
      task jspcsref_gempak_f34
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f34 == complete and /prod12/spcsref15/gempak/jspcsref_gempak_f24 == complete
      task jspcsref_gempak_f35
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f35 == complete and /prod12/spcsref15/gempak/jspcsref_gempak_f24 == complete
      task jspcsref_gempak_f36
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f36 == complete and /prod12/spcsref15/gempak/jspcsref_gempak_f24 == complete
      task jspcsref_gempak_f37
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f37 == complete and /prod12/spcsref15/gempak/jspcsref_gempak_f27 == complete
      task jspcsref_gempak_f38
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f38 == complete and /prod12/spcsref15/gempak/jspcsref_gempak_f27 == complete
      task jspcsref_gempak_f39
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f39 == complete and /prod12/spcsref15/gempak/jspcsref_gempak_f27 == complete
      task jspcsref_gempak_f42
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f42 == complete and /prod12/spcsref15/gempak/jspcsref_gempak_f30 == complete
      task jspcsref_gempak_f45
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f45 == complete and /prod12/spcsref15/gempak/jspcsref_gempak_f33 == complete
      task jspcsref_gempak_f48
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f48 == complete and /prod12/spcsref15/gempak/jspcsref_gempak_f36 == complete
      task jspcsref_gempak_f51
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f51 == complete and /prod12/spcsref15/gempak/jspcsref_gempak_f39 == complete
      task jspcsref_gempak_f54
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f54 == complete and /prod12/spcsref15/gempak/jspcsref_gempak_f42 == complete
      task jspcsref_gempak_f57
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f57 == complete and /prod12/spcsref15/gempak/jspcsref_gempak_f45 == complete
      task jspcsref_gempak_f60
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f60 == complete and /prod12/spcsref15/gempak/jspcsref_gempak_f48 == complete
      task jspcsref_gempak_f63
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f63 == complete and /prod12/spcsref15/gempak/jspcsref_gempak_f51 == complete
      task jspcsref_gempak_f66
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f66 == complete and /prod12/spcsref15/gempak/jspcsref_gempak_f54 == complete
      task jspcsref_gempak_f69
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f69 == complete and /prod12/spcsref15/gempak/jspcsref_gempak_f57 == complete
      task jspcsref_gempak_f72
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f72 == complete and /prod12/spcsref15/gempak/jspcsref_gempak_f60 == complete
      task jspcsref_gempak_f75
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f75 == complete and /prod12/spcsref15/gempak/jspcsref_gempak_f63 == complete
      task jspcsref_gempak_f78
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f78 == complete and /prod12/spcsref15/gempak/jspcsref_gempak_f66 == complete
      task jspcsref_gempak_f81
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f81 == complete and /prod12/spcsref15/gempak/jspcsref_gempak_f69 == complete
      task jspcsref_gempak_f84
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f84 == complete and /prod12/spcsref15/gempak/jspcsref_gempak_f72 == complete
      task jspcsref_gempak_f87
        trigger /prod12/spcsref15/pgrb212_gempak/jspcsref_pgrb212_gempak_f87 == complete and /prod12/spcsref15/gempak/jspcsref_gempak_f75 == complete
    endfamily
  endfamily
  family gdas
    edit PROJ 'GDAS-OPS'
    edit ECF_JOB_CMD '/ecf/ecfutils/unixsubmit %ECF_JOB% %ECF_JOBOUT% ibmsp'
    task jgdas_verfrad
      trigger enkf == complete
    family dump
      task jgdas_tropcy_qc_reloc
        time 17:45
      task jgdas_dump_alert
        trigger jgdas_dump_post:release_gdas112_dump_alert
      task jgdas_dump_post
        trigger jgdas_dump == complete
        event 1 release_gdas112_dump_alert
      task jmodel_realtime_gdas
        trigger jgdas_dump_alert == complete
      task jgdas_dump
        event 1 release_sfcprep
        time 17:50
    endfamily
    family prep
      task jgdas_emcsfc_sfc_prep
        trigger ../dump/jgdas_dump:release_sfcprep
      task jgdas_prep
        trigger ../dump/jgdas_dump == complete and ../dump/jgdas_tropcy_qc_reloc == complete
      task jgdas_prep_post
        trigger ../analysis/jgdas_analysis_high == complete
    endfamily
    family analysis
      task jgdas_analysis_high
        trigger ../prep/jgdas_prep == complete and ../prep/jgdas_emcsfc_sfc_prep == complete
        event 1 release_fcst
    endfamily
    family forecast
      task jgdas_forecast_high
        trigger ../analysis/jgdas_analysis_high:release_fcst and ../enkf/innovate == complete
    endfamily
    family gempak
      task jgdas_gempak
        trigger ../forecast/jgdas_forecast_high == complete
      task jgdas_gempak_meta
        trigger jgdas_gempak == complete
      task jgdas_gempak_ncdc
        trigger jgdas_gempak == complete
    endfamily
    family post_processing
      family bulletins
        task jgdas_mknavybulls
          trigger ../../dump/jgdas_dump_post == complete
        task jgdas_bulls
          trigger ../../post/jgdas_post_high == complete
      endfamily
    endfamily
    family post
      task jgdas_post_high
        trigger ../forecast/jgdas_forecast_high == complete
      task jgdas_post_hrly_high
        trigger ../forecast/jgdas_forecast_high == complete
    endfamily
    family enkf
      task jgdas_enkf_select_obs
        trigger ../prep/jgdas_prep == complete
      family innovate
        task jgdas_enkf_innovate_obs_grp01
          trigger ../jgdas_enkf_select_obs == complete
        task jgdas_enkf_innovate_obs_grp02
          trigger ../jgdas_enkf_select_obs == complete
        task jgdas_enkf_innovate_obs_grp03
          trigger ../jgdas_enkf_select_obs == complete
        task jgdas_enkf_innovate_obs_grp04
          trigger ../jgdas_enkf_select_obs == complete
        task jgdas_enkf_innovate_obs_grp05
          trigger ../jgdas_enkf_select_obs == complete
        task jgdas_enkf_innovate_obs_grp06
          trigger ../jgdas_enkf_select_obs == complete
        task jgdas_enkf_innovate_obs_grp07
          trigger ../jgdas_enkf_select_obs == complete
        task jgdas_enkf_innovate_obs_grp08
          trigger ../jgdas_enkf_select_obs == complete
        task jgdas_enkf_innovate_obs_grp09
          trigger ../jgdas_enkf_select_obs == complete
        task jgdas_enkf_innovate_obs_grp10
          trigger ../jgdas_enkf_select_obs == complete
      endfamily
      task jgdas_enkf_update
        trigger innovate == complete
      task jgdas_enkf_inflate_recenter
        trigger jgdas_enkf_update == complete and ../analysis/jgdas_analysis_high == complete
      family forecast
        task jgdas_enkf_fcst_grp01
          trigger ../jgdas_enkf_inflate_recenter == complete
        task jgdas_enkf_fcst_grp02
          trigger ../jgdas_enkf_inflate_recenter == complete
        task jgdas_enkf_fcst_grp03
          trigger ../jgdas_enkf_inflate_recenter == complete
        task jgdas_enkf_fcst_grp04
          trigger ../jgdas_enkf_inflate_recenter == complete
        task jgdas_enkf_fcst_grp05
          trigger ../jgdas_enkf_inflate_recenter == complete
        task jgdas_enkf_fcst_grp06
          trigger ../jgdas_enkf_inflate_recenter == complete
        task jgdas_enkf_fcst_grp07
          trigger ../jgdas_enkf_inflate_recenter == complete
        task jgdas_enkf_fcst_grp08
          trigger ../jgdas_enkf_inflate_recenter == complete
        task jgdas_enkf_fcst_grp09
          trigger ../jgdas_enkf_inflate_recenter == complete
        task jgdas_enkf_fcst_grp10
          trigger ../jgdas_enkf_inflate_recenter == complete
      endfamily
      task jgdas_enkf_post
        trigger forecast == complete
    endfamily
  endfamily
  family rap
    edit ECF_FILES '/ecf/ecfnets/scripts/rap'
    edit PROJ 'RAP-OPS'
    family rap_12
      edit CYC '12'
      family dump
        task jrap_dump_ehrrr
          time 12:16
        task jrap_dump_erly
          time 12:26
        task jrap_dump_post_erly
          trigger /prod12/rap/rap_12/dump/jrap_dump_erly == complete
        task jrap_dump
          time 12:58
        task jrap_realtime
          trigger /prod12/rap/rap_12/dump/jrap_dump == complete
        task jrap_dump_post
          trigger /prod12/rap/rap_12/dump/jrap_dump == complete
      endfamily
      family prep
        task jrap_prep_erly
          trigger /prod12/rap/rap_12/dump/jrap_dump_erly == complete
        task jrap_prep_post_erly
          trigger /prod12/rap/rap_12/prep/jrap_prep_erly == complete
        task jrap_prep
          trigger /prod12/rap/rap_12/dump/jrap_dump == complete
        task jrap_process_hydro
          trigger /prod12/rap/rap_12/prep/jrap_prep == complete
        task jrap_prep_post
          trigger /prod12/rap/rap_12/jrap_analysis == complete
      endfamily
      task jrap_analysis
        trigger /prod12/rap/rap_12/prep/jrap_process_hydro == complete
      task jrap_updatebc
        trigger /prod12/rap/rap_12/jrap_analysis == complete
      task jrap_forecast
        trigger /prod12/rap/rap_12/jrap_updatebc == complete
      family post
        task jrap_post_manager
          trigger /prod12/rap/rap_12/jrap_forecast == active or /prod12/rap/rap_12/jrap_forecast == complete
          event 1 release_post00
          event 2 release_post01
          event 3 release_post02
          event 4 release_post03
          event 5 release_post04
          event 6 release_post05
          event 7 release_post06
          event 8 release_post07
          event 9 release_post08
          event 10 release_post09
          event 11 release_post10
          event 12 release_post11
          event 13 release_post12
          event 14 release_post13
          event 15 release_post14
          event 16 release_post15
          event 17 release_post16
          event 18 release_post17
          event 19 release_post18
        family wrfbufr
          task jrap_wrfbufr_f00
            trigger /prod12/rap/rap_12/post/post/jrap_post_f00 == complete
          task jrap_wrfbufr_f01
            trigger /prod12/rap/rap_12/post/jrap_post_manager:release_post01
          task jrap_wrfbufr_f02
            trigger /prod12/rap/rap_12/post/jrap_post_manager:release_post02
          task jrap_wrfbufr_f03
            trigger /prod12/rap/rap_12/post/jrap_post_manager:release_post03
          task jrap_wrfbufr_f04
            trigger /prod12/rap/rap_12/post/jrap_post_manager:release_post04
          task jrap_wrfbufr_f05
            trigger /prod12/rap/rap_12/post/jrap_post_manager:release_post05
          task jrap_wrfbufr_f06
            trigger /prod12/rap/rap_12/post/jrap_post_manager:release_post06
          task jrap_wrfbufr_f07
            trigger /prod12/rap/rap_12/post/jrap_post_manager:release_post07
          task jrap_wrfbufr_f08
            trigger /prod12/rap/rap_12/post/jrap_post_manager:release_post08
          task jrap_wrfbufr_f09
            trigger /prod12/rap/rap_12/post/jrap_post_manager:release_post09
          task jrap_wrfbufr_f10
            trigger /prod12/rap/rap_12/post/jrap_post_manager:release_post10
          task jrap_wrfbufr_f11
            trigger /prod12/rap/rap_12/post/jrap_post_manager:release_post11
          task jrap_wrfbufr_f12
            trigger /prod12/rap/rap_12/post/jrap_post_manager:release_post12
          task jrap_wrfbufr_f13
            trigger /prod12/rap/rap_12/post/jrap_post_manager:release_post13
          task jrap_wrfbufr_f14
            trigger /prod12/rap/rap_12/post/jrap_post_manager:release_post14
          task jrap_wrfbufr_f15
            trigger /prod12/rap/rap_12/post/jrap_post_manager:release_post15
          task jrap_wrfbufr_f16
            trigger /prod12/rap/rap_12/post/jrap_post_manager:release_post16
          task jrap_wrfbufr_f17
            trigger /prod12/rap/rap_12/post/jrap_post_manager:release_post17
          task jrap_wrfbufr_f18
            trigger /prod12/rap/rap_12/post/jrap_post_manager:release_post18
        endfamily
        family post
          task jrap_post_f00
            trigger /prod12/rap/rap_12/post/jrap_post_manager:release_post00
          task jrap_post_f01
            trigger /prod12/rap/rap_12/post/jrap_post_manager:release_post01
          task jrap_post_f02
            trigger /prod12/rap/rap_12/post/jrap_post_manager:release_post02
          task jrap_post_f03
            trigger /prod12/rap/rap_12/post/jrap_post_manager:release_post03
          task jrap_post_f04
            trigger /prod12/rap/rap_12/post/jrap_post_manager:release_post04
          task jrap_post_f05
            trigger /prod12/rap/rap_12/post/jrap_post_manager:release_post05
          task jrap_post_f06
            trigger /prod12/rap/rap_12/post/jrap_post_manager:release_post06
          task jrap_post_f07
            trigger /prod12/rap/rap_12/post/jrap_post_manager:release_post07
          task jrap_post_f08
            trigger /prod12/rap/rap_12/post/jrap_post_manager:release_post08
          task jrap_post_f09
            trigger /prod12/rap/rap_12/post/jrap_post_manager:release_post09
          task jrap_post_f10
            trigger /prod12/rap/rap_12/post/jrap_post_manager:release_post10
          task jrap_post_f11
            trigger /prod12/rap/rap_12/post/jrap_post_manager:release_post11
          task jrap_post_f12
            trigger /prod12/rap/rap_12/post/jrap_post_manager:release_post12
          task jrap_post_f13
            trigger /prod12/rap/rap_12/post/jrap_post_manager:release_post13
          task jrap_post_f14
            trigger /prod12/rap/rap_12/post/jrap_post_manager:release_post14
          task jrap_post_f15
            trigger /prod12/rap/rap_12/post/jrap_post_manager:release_post15
          task jrap_post_f16
            trigger /prod12/rap/rap_12/post/jrap_post_manager:release_post16
          task jrap_post_f17
            trigger /prod12/rap/rap_12/post/jrap_post_manager:release_post17
          task jrap_post_f18
            trigger /prod12/rap/rap_12/post/jrap_post_manager:release_post18
        endfamily
      endfamily
      task jrap_postsnd
        trigger /prod12/rap/rap_12/post/wrfbufr == complete
      family prdgen
        task jrap_prdgen_f00
          trigger /prod12/rap/rap_12/post/post/jrap_post_f00 == complete
          event 1 grib_ready
        task jrap_prdgen_f01
          trigger /prod12/rap/rap_12/post/post/jrap_post_f01 == complete and /prod12/rap/rap_12/prdgen/jrap_prdgen_f00:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f02
          trigger /prod12/rap/rap_12/post/post/jrap_post_f02 == complete and /prod12/rap/rap_12/prdgen/jrap_prdgen_f00:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f03
          trigger /prod12/rap/rap_12/post/post/jrap_post_f03 == complete and /prod12/rap/rap_12/prdgen/jrap_prdgen_f00:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f04
          trigger /prod12/rap/rap_12/post/post/jrap_post_f04 == complete and /prod12/rap/rap_12/prdgen/jrap_prdgen_f01:grib_ready and /prod12/rap/rap_12/prdgen/jrap_prdgen_f03:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f05
          trigger /prod12/rap/rap_12/post/post/jrap_post_f05 == complete and /prod12/rap/rap_12/prdgen/jrap_prdgen_f02:grib_ready and /prod12/rap/rap_12/prdgen/jrap_prdgen_f03:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f06
          trigger /prod12/rap/rap_12/post/post/jrap_post_f06 == complete and /prod12/rap/rap_12/prdgen/jrap_prdgen_f03:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f07
          trigger /prod12/rap/rap_12/post/post/jrap_post_f07 == complete and /prod12/rap/rap_12/prdgen/jrap_prdgen_f04:grib_ready and /prod12/rap/rap_12/prdgen/jrap_prdgen_f06:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f08
          trigger /prod12/rap/rap_12/post/post/jrap_post_f08 == complete and /prod12/rap/rap_12/prdgen/jrap_prdgen_f05:grib_ready and /prod12/rap/rap_12/prdgen/jrap_prdgen_f06:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f09
          trigger /prod12/rap/rap_12/post/post/jrap_post_f09 == complete and /prod12/rap/rap_12/prdgen/jrap_prdgen_f06:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f10
          trigger /prod12/rap/rap_12/post/post/jrap_post_f10 == complete and /prod12/rap/rap_12/prdgen/jrap_prdgen_f07:grib_ready and /prod12/rap/rap_12/prdgen/jrap_prdgen_f09:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f11
          trigger /prod12/rap/rap_12/post/post/jrap_post_f11 == complete and /prod12/rap/rap_12/prdgen/jrap_prdgen_f08:grib_ready and /prod12/rap/rap_12/prdgen/jrap_prdgen_f09:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f12
          trigger /prod12/rap/rap_12/post/post/jrap_post_f12 == complete and /prod12/rap/rap_12/prdgen/jrap_prdgen_f09:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f13
          trigger /prod12/rap/rap_12/post/post/jrap_post_f13 == complete and /prod12/rap/rap_12/prdgen/jrap_prdgen_f10:grib_ready and /prod12/rap/rap_12/prdgen/jrap_prdgen_f12:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f14
          trigger /prod12/rap/rap_12/post/post/jrap_post_f14 == complete and /prod12/rap/rap_12/prdgen/jrap_prdgen_f11:grib_ready and /prod12/rap/rap_12/prdgen/jrap_prdgen_f12:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f15
          trigger /prod12/rap/rap_12/post/post/jrap_post_f15 == complete and /prod12/rap/rap_12/prdgen/jrap_prdgen_f12:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f16
          trigger /prod12/rap/rap_12/post/post/jrap_post_f16 == complete and /prod12/rap/rap_12/prdgen/jrap_prdgen_f13:grib_ready and /prod12/rap/rap_12/prdgen/jrap_prdgen_f15:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f17
          trigger /prod12/rap/rap_12/post/post/jrap_post_f17 == complete and /prod12/rap/rap_12/prdgen/jrap_prdgen_f14:grib_ready and /prod12/rap/rap_12/prdgen/jrap_prdgen_f15:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f18
          trigger /prod12/rap/rap_12/post/post/jrap_post_f18 == complete and /prod12/rap/rap_12/prdgen/jrap_prdgen_f15:grib_ready
          event 1 grib_ready
      endfamily
      task jrap_cleanup
        trigger /prod12/rap/rap_12/prdgen == complete and /prod12/rap/rap_12/jrap_postsnd == complete
      family gempak
        task jrap_realtime_gempak
          trigger /prod12/rap/rap_12/dump/jrap_realtime == complete
        task jrap_gempak_meta
          trigger /prod12/rap/rap_12/gempak/jrap_gempak:release_meta
        task jrap_gempak
          trigger /prod12/rap/rap_12/prdgen/jrap_prdgen_f00:grib_ready
          event 1 release_meta
      endfamily
    endfamily
    family rap_13
      edit CYC '13'
      family dump
        task jrap_dump_ehrrr
          time 13:16
        task jrap_dump
          time 13:26
        task jrap_realtime
          trigger /prod12/rap/rap_13/dump/jrap_dump == complete
        task jrap_dump_post
          trigger /prod12/rap/rap_13/dump/jrap_dump == complete
      endfamily
      family prep
        task jrap_prep
          trigger /prod12/rap/rap_13/dump/jrap_dump == complete
        task jrap_process_hydro
          trigger /prod12/rap/rap_13/prep/jrap_prep == complete
        task jrap_prep_post
          trigger /prod12/rap/rap_13/jrap_analysis == complete
      endfamily
      task jrap_analysis
        trigger /prod12/rap/rap_13/prep/jrap_process_hydro == complete
      task jrap_updatebc
        trigger /prod12/rap/rap_13/jrap_analysis == complete
      task jrap_forecast
        trigger /prod12/rap/rap_13/jrap_updatebc == complete
      family post
        task jrap_post_manager
          trigger /prod12/rap/rap_13/jrap_forecast == active or /prod12/rap/rap_13/jrap_forecast == complete
          event 1 release_post00
          event 2 release_post01
          event 3 release_post02
          event 4 release_post03
          event 5 release_post04
          event 6 release_post05
          event 7 release_post06
          event 8 release_post07
          event 9 release_post08
          event 10 release_post09
          event 11 release_post10
          event 12 release_post11
          event 13 release_post12
          event 14 release_post13
          event 15 release_post14
          event 16 release_post15
          event 17 release_post16
          event 18 release_post17
          event 19 release_post18
        family wrfbufr
          task jrap_wrfbufr_f00
            trigger /prod12/rap/rap_13/post/post/jrap_post_f00 == complete
          task jrap_wrfbufr_f01
            trigger /prod12/rap/rap_13/post/jrap_post_manager:release_post01
          task jrap_wrfbufr_f02
            trigger /prod12/rap/rap_13/post/jrap_post_manager:release_post02
          task jrap_wrfbufr_f03
            trigger /prod12/rap/rap_13/post/jrap_post_manager:release_post03
          task jrap_wrfbufr_f04
            trigger /prod12/rap/rap_13/post/jrap_post_manager:release_post04
          task jrap_wrfbufr_f05
            trigger /prod12/rap/rap_13/post/jrap_post_manager:release_post05
          task jrap_wrfbufr_f06
            trigger /prod12/rap/rap_13/post/jrap_post_manager:release_post06
          task jrap_wrfbufr_f07
            trigger /prod12/rap/rap_13/post/jrap_post_manager:release_post07
          task jrap_wrfbufr_f08
            trigger /prod12/rap/rap_13/post/jrap_post_manager:release_post08
          task jrap_wrfbufr_f09
            trigger /prod12/rap/rap_13/post/jrap_post_manager:release_post09
          task jrap_wrfbufr_f10
            trigger /prod12/rap/rap_13/post/jrap_post_manager:release_post10
          task jrap_wrfbufr_f11
            trigger /prod12/rap/rap_13/post/jrap_post_manager:release_post11
          task jrap_wrfbufr_f12
            trigger /prod12/rap/rap_13/post/jrap_post_manager:release_post12
          task jrap_wrfbufr_f13
            trigger /prod12/rap/rap_13/post/jrap_post_manager:release_post13
          task jrap_wrfbufr_f14
            trigger /prod12/rap/rap_13/post/jrap_post_manager:release_post14
          task jrap_wrfbufr_f15
            trigger /prod12/rap/rap_13/post/jrap_post_manager:release_post15
          task jrap_wrfbufr_f16
            trigger /prod12/rap/rap_13/post/jrap_post_manager:release_post16
          task jrap_wrfbufr_f17
            trigger /prod12/rap/rap_13/post/jrap_post_manager:release_post17
          task jrap_wrfbufr_f18
            trigger /prod12/rap/rap_13/post/jrap_post_manager:release_post18
        endfamily
        family post
          task jrap_post_f00
            trigger /prod12/rap/rap_13/post/jrap_post_manager:release_post00
          task jrap_post_f01
            trigger /prod12/rap/rap_13/post/jrap_post_manager:release_post01
          task jrap_post_f02
            trigger /prod12/rap/rap_13/post/jrap_post_manager:release_post02
          task jrap_post_f03
            trigger /prod12/rap/rap_13/post/jrap_post_manager:release_post03
          task jrap_post_f04
            trigger /prod12/rap/rap_13/post/jrap_post_manager:release_post04
          task jrap_post_f05
            trigger /prod12/rap/rap_13/post/jrap_post_manager:release_post05
          task jrap_post_f06
            trigger /prod12/rap/rap_13/post/jrap_post_manager:release_post06
          task jrap_post_f07
            trigger /prod12/rap/rap_13/post/jrap_post_manager:release_post07
          task jrap_post_f08
            trigger /prod12/rap/rap_13/post/jrap_post_manager:release_post08
          task jrap_post_f09
            trigger /prod12/rap/rap_13/post/jrap_post_manager:release_post09
          task jrap_post_f10
            trigger /prod12/rap/rap_13/post/jrap_post_manager:release_post10
          task jrap_post_f11
            trigger /prod12/rap/rap_13/post/jrap_post_manager:release_post11
          task jrap_post_f12
            trigger /prod12/rap/rap_13/post/jrap_post_manager:release_post12
          task jrap_post_f13
            trigger /prod12/rap/rap_13/post/jrap_post_manager:release_post13
          task jrap_post_f14
            trigger /prod12/rap/rap_13/post/jrap_post_manager:release_post14
          task jrap_post_f15
            trigger /prod12/rap/rap_13/post/jrap_post_manager:release_post15
          task jrap_post_f16
            trigger /prod12/rap/rap_13/post/jrap_post_manager:release_post16
          task jrap_post_f17
            trigger /prod12/rap/rap_13/post/jrap_post_manager:release_post17
          task jrap_post_f18
            trigger /prod12/rap/rap_13/post/jrap_post_manager:release_post18
        endfamily
      endfamily
      task jrap_postsnd
        trigger /prod12/rap/rap_13/post/wrfbufr == complete
      family prdgen
        task jrap_prdgen_f00
          trigger /prod12/rap/rap_13/post/post/jrap_post_f00 == complete
          event 1 grib_ready
        task jrap_prdgen_f01
          trigger /prod12/rap/rap_13/post/post/jrap_post_f01 == complete and /prod12/rap/rap_13/prdgen/jrap_prdgen_f00:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f02
          trigger /prod12/rap/rap_13/post/post/jrap_post_f02 == complete and /prod12/rap/rap_13/prdgen/jrap_prdgen_f00:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f03
          trigger /prod12/rap/rap_13/post/post/jrap_post_f03 == complete and /prod12/rap/rap_13/prdgen/jrap_prdgen_f00:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f04
          trigger /prod12/rap/rap_13/post/post/jrap_post_f04 == complete and /prod12/rap/rap_13/prdgen/jrap_prdgen_f01:grib_ready and /prod12/rap/rap_13/prdgen/jrap_prdgen_f03:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f05
          trigger /prod12/rap/rap_13/post/post/jrap_post_f05 == complete and /prod12/rap/rap_13/prdgen/jrap_prdgen_f02:grib_ready and /prod12/rap/rap_13/prdgen/jrap_prdgen_f03:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f06
          trigger /prod12/rap/rap_13/post/post/jrap_post_f06 == complete and /prod12/rap/rap_13/prdgen/jrap_prdgen_f03:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f07
          trigger /prod12/rap/rap_13/post/post/jrap_post_f07 == complete and /prod12/rap/rap_13/prdgen/jrap_prdgen_f04:grib_ready and /prod12/rap/rap_13/prdgen/jrap_prdgen_f06:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f08
          trigger /prod12/rap/rap_13/post/post/jrap_post_f08 == complete and /prod12/rap/rap_13/prdgen/jrap_prdgen_f05:grib_ready and /prod12/rap/rap_13/prdgen/jrap_prdgen_f06:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f09
          trigger /prod12/rap/rap_13/post/post/jrap_post_f09 == complete and /prod12/rap/rap_13/prdgen/jrap_prdgen_f06:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f10
          trigger /prod12/rap/rap_13/post/post/jrap_post_f10 == complete and /prod12/rap/rap_13/prdgen/jrap_prdgen_f07:grib_ready and /prod12/rap/rap_13/prdgen/jrap_prdgen_f09:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f11
          trigger /prod12/rap/rap_13/post/post/jrap_post_f11 == complete and /prod12/rap/rap_13/prdgen/jrap_prdgen_f08:grib_ready and /prod12/rap/rap_13/prdgen/jrap_prdgen_f09:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f12
          trigger /prod12/rap/rap_13/post/post/jrap_post_f12 == complete and /prod12/rap/rap_13/prdgen/jrap_prdgen_f09:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f13
          trigger /prod12/rap/rap_13/post/post/jrap_post_f13 == complete and /prod12/rap/rap_13/prdgen/jrap_prdgen_f10:grib_ready and /prod12/rap/rap_13/prdgen/jrap_prdgen_f12:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f14
          trigger /prod12/rap/rap_13/post/post/jrap_post_f14 == complete and /prod12/rap/rap_13/prdgen/jrap_prdgen_f11:grib_ready and /prod12/rap/rap_13/prdgen/jrap_prdgen_f12:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f15
          trigger /prod12/rap/rap_13/post/post/jrap_post_f15 == complete and /prod12/rap/rap_13/prdgen/jrap_prdgen_f12:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f16
          trigger /prod12/rap/rap_13/post/post/jrap_post_f16 == complete and /prod12/rap/rap_13/prdgen/jrap_prdgen_f13:grib_ready and /prod12/rap/rap_13/prdgen/jrap_prdgen_f15:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f17
          trigger /prod12/rap/rap_13/post/post/jrap_post_f17 == complete and /prod12/rap/rap_13/prdgen/jrap_prdgen_f14:grib_ready and /prod12/rap/rap_13/prdgen/jrap_prdgen_f15:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f18
          trigger /prod12/rap/rap_13/post/post/jrap_post_f18 == complete and /prod12/rap/rap_13/prdgen/jrap_prdgen_f15:grib_ready
          event 1 grib_ready
      endfamily
      task jrap_cleanup
        trigger /prod12/rap/rap_13/prdgen == complete and /prod12/rap/rap_13/jrap_postsnd == complete
      family gempak
        task jrap_realtime_gempak
          trigger /prod12/rap/rap_13/dump/jrap_realtime == complete
        task jrap_gempak_meta
          trigger /prod12/rap/rap_13/gempak/jrap_gempak:release_meta
        task jrap_gempak
          trigger /prod12/rap/rap_13/prdgen/jrap_prdgen_f00:grib_ready
          event 1 release_meta
      endfamily
    endfamily
    family rap_14
      edit CYC '14'
      family dump
        task jrap_dump_ehrrr
          time 14:16
        task jrap_dump
          time 14:26
        task jrap_realtime
          trigger /prod12/rap/rap_14/dump/jrap_dump == complete
        task jrap_dump_post
          trigger /prod12/rap/rap_14/dump/jrap_dump == complete
      endfamily
      family prep
        task jrap_prep
          trigger /prod12/rap/rap_14/dump/jrap_dump == complete
        task jrap_process_hydro
          trigger /prod12/rap/rap_14/prep/jrap_prep == complete
        task jrap_prep_post
          trigger /prod12/rap/rap_14/jrap_analysis == complete
      endfamily
      task jrap_analysis
        trigger /prod12/rap/rap_14/prep/jrap_process_hydro == complete
      task jrap_updatebc
        trigger /prod12/rap/rap_14/jrap_analysis == complete
      task jrap_forecast
        trigger /prod12/rap/rap_14/jrap_updatebc == complete
      family post
        task jrap_post_manager
          trigger /prod12/rap/rap_14/jrap_forecast == active or /prod12/rap/rap_14/jrap_forecast == complete
          event 1 release_post00
          event 2 release_post01
          event 3 release_post02
          event 4 release_post03
          event 5 release_post04
          event 6 release_post05
          event 7 release_post06
          event 8 release_post07
          event 9 release_post08
          event 10 release_post09
          event 11 release_post10
          event 12 release_post11
          event 13 release_post12
          event 14 release_post13
          event 15 release_post14
          event 16 release_post15
          event 17 release_post16
          event 18 release_post17
          event 19 release_post18
        family wrfbufr
          task jrap_wrfbufr_f00
            trigger /prod12/rap/rap_14/post/post/jrap_post_f00 == complete
          task jrap_wrfbufr_f01
            trigger /prod12/rap/rap_14/post/jrap_post_manager:release_post01
          task jrap_wrfbufr_f02
            trigger /prod12/rap/rap_14/post/jrap_post_manager:release_post02
          task jrap_wrfbufr_f03
            trigger /prod12/rap/rap_14/post/jrap_post_manager:release_post03
          task jrap_wrfbufr_f04
            trigger /prod12/rap/rap_14/post/jrap_post_manager:release_post04
          task jrap_wrfbufr_f05
            trigger /prod12/rap/rap_14/post/jrap_post_manager:release_post05
          task jrap_wrfbufr_f06
            trigger /prod12/rap/rap_14/post/jrap_post_manager:release_post06
          task jrap_wrfbufr_f07
            trigger /prod12/rap/rap_14/post/jrap_post_manager:release_post07
          task jrap_wrfbufr_f08
            trigger /prod12/rap/rap_14/post/jrap_post_manager:release_post08
          task jrap_wrfbufr_f09
            trigger /prod12/rap/rap_14/post/jrap_post_manager:release_post09
          task jrap_wrfbufr_f10
            trigger /prod12/rap/rap_14/post/jrap_post_manager:release_post10
          task jrap_wrfbufr_f11
            trigger /prod12/rap/rap_14/post/jrap_post_manager:release_post11
          task jrap_wrfbufr_f12
            trigger /prod12/rap/rap_14/post/jrap_post_manager:release_post12
          task jrap_wrfbufr_f13
            trigger /prod12/rap/rap_14/post/jrap_post_manager:release_post13
          task jrap_wrfbufr_f14
            trigger /prod12/rap/rap_14/post/jrap_post_manager:release_post14
          task jrap_wrfbufr_f15
            trigger /prod12/rap/rap_14/post/jrap_post_manager:release_post15
          task jrap_wrfbufr_f16
            trigger /prod12/rap/rap_14/post/jrap_post_manager:release_post16
          task jrap_wrfbufr_f17
            trigger /prod12/rap/rap_14/post/jrap_post_manager:release_post17
          task jrap_wrfbufr_f18
            trigger /prod12/rap/rap_14/post/jrap_post_manager:release_post18
        endfamily
        family post
          task jrap_post_f00
            trigger /prod12/rap/rap_14/post/jrap_post_manager:release_post00
          task jrap_post_f01
            trigger /prod12/rap/rap_14/post/jrap_post_manager:release_post01
          task jrap_post_f02
            trigger /prod12/rap/rap_14/post/jrap_post_manager:release_post02
          task jrap_post_f03
            trigger /prod12/rap/rap_14/post/jrap_post_manager:release_post03
          task jrap_post_f04
            trigger /prod12/rap/rap_14/post/jrap_post_manager:release_post04
          task jrap_post_f05
            trigger /prod12/rap/rap_14/post/jrap_post_manager:release_post05
          task jrap_post_f06
            trigger /prod12/rap/rap_14/post/jrap_post_manager:release_post06
          task jrap_post_f07
            trigger /prod12/rap/rap_14/post/jrap_post_manager:release_post07
          task jrap_post_f08
            trigger /prod12/rap/rap_14/post/jrap_post_manager:release_post08
          task jrap_post_f09
            trigger /prod12/rap/rap_14/post/jrap_post_manager:release_post09
          task jrap_post_f10
            trigger /prod12/rap/rap_14/post/jrap_post_manager:release_post10
          task jrap_post_f11
            trigger /prod12/rap/rap_14/post/jrap_post_manager:release_post11
          task jrap_post_f12
            trigger /prod12/rap/rap_14/post/jrap_post_manager:release_post12
          task jrap_post_f13
            trigger /prod12/rap/rap_14/post/jrap_post_manager:release_post13
          task jrap_post_f14
            trigger /prod12/rap/rap_14/post/jrap_post_manager:release_post14
          task jrap_post_f15
            trigger /prod12/rap/rap_14/post/jrap_post_manager:release_post15
          task jrap_post_f16
            trigger /prod12/rap/rap_14/post/jrap_post_manager:release_post16
          task jrap_post_f17
            trigger /prod12/rap/rap_14/post/jrap_post_manager:release_post17
          task jrap_post_f18
            trigger /prod12/rap/rap_14/post/jrap_post_manager:release_post18
        endfamily
      endfamily
      task jrap_postsnd
        trigger /prod12/rap/rap_14/post/wrfbufr == complete
      family prdgen
        task jrap_prdgen_f00
          trigger /prod12/rap/rap_14/post/post/jrap_post_f00 == complete
          event 1 grib_ready
        task jrap_prdgen_f01
          trigger /prod12/rap/rap_14/post/post/jrap_post_f01 == complete and /prod12/rap/rap_14/prdgen/jrap_prdgen_f00:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f02
          trigger /prod12/rap/rap_14/post/post/jrap_post_f02 == complete and /prod12/rap/rap_14/prdgen/jrap_prdgen_f00:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f03
          trigger /prod12/rap/rap_14/post/post/jrap_post_f03 == complete and /prod12/rap/rap_14/prdgen/jrap_prdgen_f00:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f04
          trigger /prod12/rap/rap_14/post/post/jrap_post_f04 == complete and /prod12/rap/rap_14/prdgen/jrap_prdgen_f01:grib_ready and /prod12/rap/rap_14/prdgen/jrap_prdgen_f03:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f05
          trigger /prod12/rap/rap_14/post/post/jrap_post_f05 == complete and /prod12/rap/rap_14/prdgen/jrap_prdgen_f02:grib_ready and /prod12/rap/rap_14/prdgen/jrap_prdgen_f03:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f06
          trigger /prod12/rap/rap_14/post/post/jrap_post_f06 == complete and /prod12/rap/rap_14/prdgen/jrap_prdgen_f03:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f07
          trigger /prod12/rap/rap_14/post/post/jrap_post_f07 == complete and /prod12/rap/rap_14/prdgen/jrap_prdgen_f04:grib_ready and /prod12/rap/rap_14/prdgen/jrap_prdgen_f06:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f08
          trigger /prod12/rap/rap_14/post/post/jrap_post_f08 == complete and /prod12/rap/rap_14/prdgen/jrap_prdgen_f05:grib_ready and /prod12/rap/rap_14/prdgen/jrap_prdgen_f06:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f09
          trigger /prod12/rap/rap_14/post/post/jrap_post_f09 == complete and /prod12/rap/rap_14/prdgen/jrap_prdgen_f06:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f10
          trigger /prod12/rap/rap_14/post/post/jrap_post_f10 == complete and /prod12/rap/rap_14/prdgen/jrap_prdgen_f07:grib_ready and /prod12/rap/rap_14/prdgen/jrap_prdgen_f09:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f11
          trigger /prod12/rap/rap_14/post/post/jrap_post_f11 == complete and /prod12/rap/rap_14/prdgen/jrap_prdgen_f08:grib_ready and /prod12/rap/rap_14/prdgen/jrap_prdgen_f09:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f12
          trigger /prod12/rap/rap_14/post/post/jrap_post_f12 == complete and /prod12/rap/rap_14/prdgen/jrap_prdgen_f09:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f13
          trigger /prod12/rap/rap_14/post/post/jrap_post_f13 == complete and /prod12/rap/rap_14/prdgen/jrap_prdgen_f10:grib_ready and /prod12/rap/rap_14/prdgen/jrap_prdgen_f12:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f14
          trigger /prod12/rap/rap_14/post/post/jrap_post_f14 == complete and /prod12/rap/rap_14/prdgen/jrap_prdgen_f11:grib_ready and /prod12/rap/rap_14/prdgen/jrap_prdgen_f12:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f15
          trigger /prod12/rap/rap_14/post/post/jrap_post_f15 == complete and /prod12/rap/rap_14/prdgen/jrap_prdgen_f12:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f16
          trigger /prod12/rap/rap_14/post/post/jrap_post_f16 == complete and /prod12/rap/rap_14/prdgen/jrap_prdgen_f13:grib_ready and /prod12/rap/rap_14/prdgen/jrap_prdgen_f15:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f17
          trigger /prod12/rap/rap_14/post/post/jrap_post_f17 == complete and /prod12/rap/rap_14/prdgen/jrap_prdgen_f14:grib_ready and /prod12/rap/rap_14/prdgen/jrap_prdgen_f15:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f18
          trigger /prod12/rap/rap_14/post/post/jrap_post_f18 == complete and /prod12/rap/rap_14/prdgen/jrap_prdgen_f15:grib_ready
          event 1 grib_ready
      endfamily
      task jrap_cleanup
        trigger /prod12/rap/rap_14/prdgen == complete and /prod12/rap/rap_14/jrap_postsnd == complete
      family gempak
        task jrap_realtime_gempak
          trigger /prod12/rap/rap_14/dump/jrap_realtime == complete
        task jrap_gempak_meta
          trigger /prod12/rap/rap_14/gempak/jrap_gempak:release_meta
        task jrap_gempak
          trigger /prod12/rap/rap_14/prdgen/jrap_prdgen_f00:grib_ready
          event 1 release_meta
      endfamily
    endfamily
    task jrap_makebc
      trigger /prod12/gfs/prdgen/jgfs_pgrb2_f192 == complete
    family rap_15
      edit CYC '15'
      family dump
        task jrap_dump_ehrrr
          time 15:16
        task jrap_dump
          time 15:26
        task jrap_realtime
          trigger /prod12/rap/rap_15/dump/jrap_dump == complete
        task jrap_dump_post
          trigger /prod12/rap/rap_15/dump/jrap_dump == complete
      endfamily
      family prep
        task jrap_prep
          trigger /prod12/rap/rap_15/dump/jrap_dump == complete
        task jrap_process_hydro
          trigger /prod12/rap/rap_15/prep/jrap_prep == complete
        task jrap_prep_post
          trigger /prod12/rap/rap_15/jrap_analysis == complete
      endfamily
      task jrap_analysis
        trigger /prod12/rap/rap_15/prep/jrap_process_hydro == complete
      task jrap_updatebc
        trigger /prod12/rap/rap_15/jrap_analysis == complete
      task jrap_forecast
        trigger /prod12/rap/rap_15/jrap_updatebc == complete
      family post
        task jrap_post_manager
          trigger /prod12/rap/rap_15/jrap_forecast == active or /prod12/rap/rap_15/jrap_forecast == complete
          event 1 release_post00
          event 2 release_post01
          event 3 release_post02
          event 4 release_post03
          event 5 release_post04
          event 6 release_post05
          event 7 release_post06
          event 8 release_post07
          event 9 release_post08
          event 10 release_post09
          event 11 release_post10
          event 12 release_post11
          event 13 release_post12
          event 14 release_post13
          event 15 release_post14
          event 16 release_post15
          event 17 release_post16
          event 18 release_post17
          event 19 release_post18
        family wrfbufr
          task jrap_wrfbufr_f00
            trigger /prod12/rap/rap_15/post/post/jrap_post_f00 == complete
          task jrap_wrfbufr_f01
            trigger /prod12/rap/rap_15/post/jrap_post_manager:release_post01
          task jrap_wrfbufr_f02
            trigger /prod12/rap/rap_15/post/jrap_post_manager:release_post02
          task jrap_wrfbufr_f03
            trigger /prod12/rap/rap_15/post/jrap_post_manager:release_post03
          task jrap_wrfbufr_f04
            trigger /prod12/rap/rap_15/post/jrap_post_manager:release_post04
          task jrap_wrfbufr_f05
            trigger /prod12/rap/rap_15/post/jrap_post_manager:release_post05
          task jrap_wrfbufr_f06
            trigger /prod12/rap/rap_15/post/jrap_post_manager:release_post06
          task jrap_wrfbufr_f07
            trigger /prod12/rap/rap_15/post/jrap_post_manager:release_post07
          task jrap_wrfbufr_f08
            trigger /prod12/rap/rap_15/post/jrap_post_manager:release_post08
          task jrap_wrfbufr_f09
            trigger /prod12/rap/rap_15/post/jrap_post_manager:release_post09
          task jrap_wrfbufr_f10
            trigger /prod12/rap/rap_15/post/jrap_post_manager:release_post10
          task jrap_wrfbufr_f11
            trigger /prod12/rap/rap_15/post/jrap_post_manager:release_post11
          task jrap_wrfbufr_f12
            trigger /prod12/rap/rap_15/post/jrap_post_manager:release_post12
          task jrap_wrfbufr_f13
            trigger /prod12/rap/rap_15/post/jrap_post_manager:release_post13
          task jrap_wrfbufr_f14
            trigger /prod12/rap/rap_15/post/jrap_post_manager:release_post14
          task jrap_wrfbufr_f15
            trigger /prod12/rap/rap_15/post/jrap_post_manager:release_post15
          task jrap_wrfbufr_f16
            trigger /prod12/rap/rap_15/post/jrap_post_manager:release_post16
          task jrap_wrfbufr_f17
            trigger /prod12/rap/rap_15/post/jrap_post_manager:release_post17
          task jrap_wrfbufr_f18
            trigger /prod12/rap/rap_15/post/jrap_post_manager:release_post18
        endfamily
        family post
          task jrap_post_f00
            trigger /prod12/rap/rap_15/post/jrap_post_manager:release_post00
          task jrap_post_f01
            trigger /prod12/rap/rap_15/post/jrap_post_manager:release_post01
          task jrap_post_f02
            trigger /prod12/rap/rap_15/post/jrap_post_manager:release_post02
          task jrap_post_f03
            trigger /prod12/rap/rap_15/post/jrap_post_manager:release_post03
          task jrap_post_f04
            trigger /prod12/rap/rap_15/post/jrap_post_manager:release_post04
          task jrap_post_f05
            trigger /prod12/rap/rap_15/post/jrap_post_manager:release_post05
          task jrap_post_f06
            trigger /prod12/rap/rap_15/post/jrap_post_manager:release_post06
          task jrap_post_f07
            trigger /prod12/rap/rap_15/post/jrap_post_manager:release_post07
          task jrap_post_f08
            trigger /prod12/rap/rap_15/post/jrap_post_manager:release_post08
          task jrap_post_f09
            trigger /prod12/rap/rap_15/post/jrap_post_manager:release_post09
          task jrap_post_f10
            trigger /prod12/rap/rap_15/post/jrap_post_manager:release_post10
          task jrap_post_f11
            trigger /prod12/rap/rap_15/post/jrap_post_manager:release_post11
          task jrap_post_f12
            trigger /prod12/rap/rap_15/post/jrap_post_manager:release_post12
          task jrap_post_f13
            trigger /prod12/rap/rap_15/post/jrap_post_manager:release_post13
          task jrap_post_f14
            trigger /prod12/rap/rap_15/post/jrap_post_manager:release_post14
          task jrap_post_f15
            trigger /prod12/rap/rap_15/post/jrap_post_manager:release_post15
          task jrap_post_f16
            trigger /prod12/rap/rap_15/post/jrap_post_manager:release_post16
          task jrap_post_f17
            trigger /prod12/rap/rap_15/post/jrap_post_manager:release_post17
          task jrap_post_f18
            trigger /prod12/rap/rap_15/post/jrap_post_manager:release_post18
        endfamily
      endfamily
      task jrap_postsnd
        trigger /prod12/rap/rap_15/post/wrfbufr == complete
      family prdgen
        task jrap_prdgen_f00
          trigger /prod12/rap/rap_15/post/post/jrap_post_f00 == complete
          event 1 grib_ready
        task jrap_prdgen_f01
          trigger /prod12/rap/rap_15/post/post/jrap_post_f01 == complete and /prod12/rap/rap_15/prdgen/jrap_prdgen_f00:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f02
          trigger /prod12/rap/rap_15/post/post/jrap_post_f02 == complete and /prod12/rap/rap_15/prdgen/jrap_prdgen_f00:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f03
          trigger /prod12/rap/rap_15/post/post/jrap_post_f03 == complete and /prod12/rap/rap_15/prdgen/jrap_prdgen_f00:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f04
          trigger /prod12/rap/rap_15/post/post/jrap_post_f04 == complete and /prod12/rap/rap_15/prdgen/jrap_prdgen_f01:grib_ready and /prod12/rap/rap_15/prdgen/jrap_prdgen_f03:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f05
          trigger /prod12/rap/rap_15/post/post/jrap_post_f05 == complete and /prod12/rap/rap_15/prdgen/jrap_prdgen_f02:grib_ready and /prod12/rap/rap_15/prdgen/jrap_prdgen_f03:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f06
          trigger /prod12/rap/rap_15/post/post/jrap_post_f06 == complete and /prod12/rap/rap_15/prdgen/jrap_prdgen_f03:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f07
          trigger /prod12/rap/rap_15/post/post/jrap_post_f07 == complete and /prod12/rap/rap_15/prdgen/jrap_prdgen_f04:grib_ready and /prod12/rap/rap_15/prdgen/jrap_prdgen_f06:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f08
          trigger /prod12/rap/rap_15/post/post/jrap_post_f08 == complete and /prod12/rap/rap_15/prdgen/jrap_prdgen_f05:grib_ready and /prod12/rap/rap_15/prdgen/jrap_prdgen_f06:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f09
          trigger /prod12/rap/rap_15/post/post/jrap_post_f09 == complete and /prod12/rap/rap_15/prdgen/jrap_prdgen_f06:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f10
          trigger /prod12/rap/rap_15/post/post/jrap_post_f10 == complete and /prod12/rap/rap_15/prdgen/jrap_prdgen_f07:grib_ready and /prod12/rap/rap_15/prdgen/jrap_prdgen_f09:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f11
          trigger /prod12/rap/rap_15/post/post/jrap_post_f11 == complete and /prod12/rap/rap_15/prdgen/jrap_prdgen_f08:grib_ready and /prod12/rap/rap_15/prdgen/jrap_prdgen_f09:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f12
          trigger /prod12/rap/rap_15/post/post/jrap_post_f12 == complete and /prod12/rap/rap_15/prdgen/jrap_prdgen_f09:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f13
          trigger /prod12/rap/rap_15/post/post/jrap_post_f13 == complete and /prod12/rap/rap_15/prdgen/jrap_prdgen_f10:grib_ready and /prod12/rap/rap_15/prdgen/jrap_prdgen_f12:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f14
          trigger /prod12/rap/rap_15/post/post/jrap_post_f14 == complete and /prod12/rap/rap_15/prdgen/jrap_prdgen_f11:grib_ready and /prod12/rap/rap_15/prdgen/jrap_prdgen_f12:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f15
          trigger /prod12/rap/rap_15/post/post/jrap_post_f15 == complete and /prod12/rap/rap_15/prdgen/jrap_prdgen_f12:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f16
          trigger /prod12/rap/rap_15/post/post/jrap_post_f16 == complete and /prod12/rap/rap_15/prdgen/jrap_prdgen_f13:grib_ready and /prod12/rap/rap_15/prdgen/jrap_prdgen_f15:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f17
          trigger /prod12/rap/rap_15/post/post/jrap_post_f17 == complete and /prod12/rap/rap_15/prdgen/jrap_prdgen_f14:grib_ready and /prod12/rap/rap_15/prdgen/jrap_prdgen_f15:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f18
          trigger /prod12/rap/rap_15/post/post/jrap_post_f18 == complete and /prod12/rap/rap_15/prdgen/jrap_prdgen_f15:grib_ready
          event 1 grib_ready
      endfamily
      task jrap_cleanup
        trigger /prod12/rap/rap_15/prdgen == complete and /prod12/rap/rap_15/jrap_postsnd == complete
      family gempak
        task jrap_realtime_gempak
          trigger /prod12/rap/rap_15/dump/jrap_realtime == complete
        task jrap_gempak_meta
          trigger /prod12/rap/rap_15/gempak/jrap_gempak:release_meta
        task jrap_gempak
          trigger /prod12/rap/rap_15/prdgen/jrap_prdgen_f00:grib_ready
          event 1 release_meta
      endfamily
      family pcyc
        family dump
          task jrap_dump_post_pcyc
            trigger /prod12/rap/rap_15/pcyc/dump/jrap_dump_pcyc == complete
          task jrap_dump_pcyc
            time 19:00
        endfamily
        family prep
          task jrap_prep_pcyc
            trigger /prod12/rap/rap_15/pcyc/dump/jrap_dump_pcyc == complete
          task jrap_prep_post_pcyc
            trigger /prod12/rap/rap_15/pcyc/jrap_analysis_pcyc == complete
        endfamily
        task jrap_analysis_pcyc
          trigger /prod12/rap/rap_15/pcyc/prep/jrap_prep_pcyc == complete and /prod12/rap/rap_15/prep/jrap_process_hydro == complete and /prod12/rap/rap_15/jrap_forecast == complete
        task jrap_updatebc_pcyc
          trigger /prod12/rap/rap_15/pcyc/jrap_analysis_pcyc == complete
        task jrap_forecast_pcyc
          trigger /prod12/rap/rap_15/pcyc/jrap_updatebc_pcyc == complete
      endfamily
    endfamily
    family rap_16
      edit CYC '16'
      family dump
        task jrap_dump_ehrrr
          time 16:16
        task jrap_dump
          time 16:26
        task jrap_realtime
          trigger /prod12/rap/rap_16/dump/jrap_dump == complete
        task jrap_dump_post
          trigger /prod12/rap/rap_16/dump/jrap_dump == complete
      endfamily
      family prep
        task jrap_prep
          trigger /prod12/rap/rap_16/dump/jrap_dump == complete
        task jrap_process_hydro
          trigger /prod12/rap/rap_16/prep/jrap_prep == complete
        task jrap_prep_post
          trigger /prod12/rap/rap_16/jrap_analysis == complete
      endfamily
      task jrap_analysis
        trigger /prod12/rap/rap_16/prep/jrap_process_hydro == complete
      task jrap_updatebc
        trigger /prod12/rap/rap_16/jrap_analysis == complete
      task jrap_forecast
        trigger /prod12/rap/rap_16/jrap_updatebc == complete
      family post
        task jrap_post_manager
          trigger /prod12/rap/rap_16/jrap_forecast == active or /prod12/rap/rap_16/jrap_forecast == complete
          event 1 release_post00
          event 2 release_post01
          event 3 release_post02
          event 4 release_post03
          event 5 release_post04
          event 6 release_post05
          event 7 release_post06
          event 8 release_post07
          event 9 release_post08
          event 10 release_post09
          event 11 release_post10
          event 12 release_post11
          event 13 release_post12
          event 14 release_post13
          event 15 release_post14
          event 16 release_post15
          event 17 release_post16
          event 18 release_post17
          event 19 release_post18
        family wrfbufr
          task jrap_wrfbufr_f00
            trigger /prod12/rap/rap_16/post/post/jrap_post_f00 == complete
          task jrap_wrfbufr_f01
            trigger /prod12/rap/rap_16/post/jrap_post_manager:release_post01
          task jrap_wrfbufr_f02
            trigger /prod12/rap/rap_16/post/jrap_post_manager:release_post02
          task jrap_wrfbufr_f03
            trigger /prod12/rap/rap_16/post/jrap_post_manager:release_post03
          task jrap_wrfbufr_f04
            trigger /prod12/rap/rap_16/post/jrap_post_manager:release_post04
          task jrap_wrfbufr_f05
            trigger /prod12/rap/rap_16/post/jrap_post_manager:release_post05
          task jrap_wrfbufr_f06
            trigger /prod12/rap/rap_16/post/jrap_post_manager:release_post06
          task jrap_wrfbufr_f07
            trigger /prod12/rap/rap_16/post/jrap_post_manager:release_post07
          task jrap_wrfbufr_f08
            trigger /prod12/rap/rap_16/post/jrap_post_manager:release_post08
          task jrap_wrfbufr_f09
            trigger /prod12/rap/rap_16/post/jrap_post_manager:release_post09
          task jrap_wrfbufr_f10
            trigger /prod12/rap/rap_16/post/jrap_post_manager:release_post10
          task jrap_wrfbufr_f11
            trigger /prod12/rap/rap_16/post/jrap_post_manager:release_post11
          task jrap_wrfbufr_f12
            trigger /prod12/rap/rap_16/post/jrap_post_manager:release_post12
          task jrap_wrfbufr_f13
            trigger /prod12/rap/rap_16/post/jrap_post_manager:release_post13
          task jrap_wrfbufr_f14
            trigger /prod12/rap/rap_16/post/jrap_post_manager:release_post14
          task jrap_wrfbufr_f15
            trigger /prod12/rap/rap_16/post/jrap_post_manager:release_post15
          task jrap_wrfbufr_f16
            trigger /prod12/rap/rap_16/post/jrap_post_manager:release_post16
          task jrap_wrfbufr_f17
            trigger /prod12/rap/rap_16/post/jrap_post_manager:release_post17
          task jrap_wrfbufr_f18
            trigger /prod12/rap/rap_16/post/jrap_post_manager:release_post18
        endfamily
        family post
          task jrap_post_f00
            trigger /prod12/rap/rap_16/post/jrap_post_manager:release_post00
          task jrap_post_f01
            trigger /prod12/rap/rap_16/post/jrap_post_manager:release_post01
          task jrap_post_f02
            trigger /prod12/rap/rap_16/post/jrap_post_manager:release_post02
          task jrap_post_f03
            trigger /prod12/rap/rap_16/post/jrap_post_manager:release_post03
          task jrap_post_f04
            trigger /prod12/rap/rap_16/post/jrap_post_manager:release_post04
          task jrap_post_f05
            trigger /prod12/rap/rap_16/post/jrap_post_manager:release_post05
          task jrap_post_f06
            trigger /prod12/rap/rap_16/post/jrap_post_manager:release_post06
          task jrap_post_f07
            trigger /prod12/rap/rap_16/post/jrap_post_manager:release_post07
          task jrap_post_f08
            trigger /prod12/rap/rap_16/post/jrap_post_manager:release_post08
          task jrap_post_f09
            trigger /prod12/rap/rap_16/post/jrap_post_manager:release_post09
          task jrap_post_f10
            trigger /prod12/rap/rap_16/post/jrap_post_manager:release_post10
          task jrap_post_f11
            trigger /prod12/rap/rap_16/post/jrap_post_manager:release_post11
          task jrap_post_f12
            trigger /prod12/rap/rap_16/post/jrap_post_manager:release_post12
          task jrap_post_f13
            trigger /prod12/rap/rap_16/post/jrap_post_manager:release_post13
          task jrap_post_f14
            trigger /prod12/rap/rap_16/post/jrap_post_manager:release_post14
          task jrap_post_f15
            trigger /prod12/rap/rap_16/post/jrap_post_manager:release_post15
          task jrap_post_f16
            trigger /prod12/rap/rap_16/post/jrap_post_manager:release_post16
          task jrap_post_f17
            trigger /prod12/rap/rap_16/post/jrap_post_manager:release_post17
          task jrap_post_f18
            trigger /prod12/rap/rap_16/post/jrap_post_manager:release_post18
        endfamily
      endfamily
      task jrap_postsnd
        trigger /prod12/rap/rap_16/post/wrfbufr == complete
      family prdgen
        task jrap_prdgen_f00
          trigger /prod12/rap/rap_16/post/post/jrap_post_f00 == complete
          event 1 grib_ready
        task jrap_prdgen_f01
          trigger /prod12/rap/rap_16/post/post/jrap_post_f01 == complete and /prod12/rap/rap_16/prdgen/jrap_prdgen_f00:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f02
          trigger /prod12/rap/rap_16/post/post/jrap_post_f02 == complete and /prod12/rap/rap_16/prdgen/jrap_prdgen_f00:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f03
          trigger /prod12/rap/rap_16/post/post/jrap_post_f03 == complete and /prod12/rap/rap_16/prdgen/jrap_prdgen_f00:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f04
          trigger /prod12/rap/rap_16/post/post/jrap_post_f04 == complete and /prod12/rap/rap_16/prdgen/jrap_prdgen_f01:grib_ready and /prod12/rap/rap_16/prdgen/jrap_prdgen_f03:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f05
          trigger /prod12/rap/rap_16/post/post/jrap_post_f05 == complete and /prod12/rap/rap_16/prdgen/jrap_prdgen_f02:grib_ready and /prod12/rap/rap_16/prdgen/jrap_prdgen_f03:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f06
          trigger /prod12/rap/rap_16/post/post/jrap_post_f06 == complete and /prod12/rap/rap_16/prdgen/jrap_prdgen_f03:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f07
          trigger /prod12/rap/rap_16/post/post/jrap_post_f07 == complete and /prod12/rap/rap_16/prdgen/jrap_prdgen_f04:grib_ready and /prod12/rap/rap_16/prdgen/jrap_prdgen_f06:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f08
          trigger /prod12/rap/rap_16/post/post/jrap_post_f08 == complete and /prod12/rap/rap_16/prdgen/jrap_prdgen_f05:grib_ready and /prod12/rap/rap_16/prdgen/jrap_prdgen_f06:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f09
          trigger /prod12/rap/rap_16/post/post/jrap_post_f09 == complete and /prod12/rap/rap_16/prdgen/jrap_prdgen_f06:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f10
          trigger /prod12/rap/rap_16/post/post/jrap_post_f10 == complete and /prod12/rap/rap_16/prdgen/jrap_prdgen_f07:grib_ready and /prod12/rap/rap_16/prdgen/jrap_prdgen_f09:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f11
          trigger /prod12/rap/rap_16/post/post/jrap_post_f11 == complete and /prod12/rap/rap_16/prdgen/jrap_prdgen_f08:grib_ready and /prod12/rap/rap_16/prdgen/jrap_prdgen_f09:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f12
          trigger /prod12/rap/rap_16/post/post/jrap_post_f12 == complete and /prod12/rap/rap_16/prdgen/jrap_prdgen_f09:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f13
          trigger /prod12/rap/rap_16/post/post/jrap_post_f13 == complete and /prod12/rap/rap_16/prdgen/jrap_prdgen_f10:grib_ready and /prod12/rap/rap_16/prdgen/jrap_prdgen_f12:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f14
          trigger /prod12/rap/rap_16/post/post/jrap_post_f14 == complete and /prod12/rap/rap_16/prdgen/jrap_prdgen_f11:grib_ready and /prod12/rap/rap_16/prdgen/jrap_prdgen_f12:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f15
          trigger /prod12/rap/rap_16/post/post/jrap_post_f15 == complete and /prod12/rap/rap_16/prdgen/jrap_prdgen_f12:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f16
          trigger /prod12/rap/rap_16/post/post/jrap_post_f16 == complete and /prod12/rap/rap_16/prdgen/jrap_prdgen_f13:grib_ready and /prod12/rap/rap_16/prdgen/jrap_prdgen_f15:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f17
          trigger /prod12/rap/rap_16/post/post/jrap_post_f17 == complete and /prod12/rap/rap_16/prdgen/jrap_prdgen_f14:grib_ready and /prod12/rap/rap_16/prdgen/jrap_prdgen_f15:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f18
          trigger /prod12/rap/rap_16/post/post/jrap_post_f18 == complete and /prod12/rap/rap_16/prdgen/jrap_prdgen_f15:grib_ready
          event 1 grib_ready
      endfamily
      task jrap_cleanup
        trigger /prod12/rap/rap_16/prdgen == complete and /prod12/rap/rap_16/jrap_postsnd == complete
      family gempak
        task jrap_realtime_gempak
          trigger /prod12/rap/rap_16/dump/jrap_realtime == complete
        task jrap_gempak_meta
          trigger /prod12/rap/rap_16/gempak/jrap_gempak:release_meta
        task jrap_gempak
          trigger /prod12/rap/rap_16/prdgen/jrap_prdgen_f00:grib_ready
          event 1 release_meta
      endfamily
      family pcyc
        family dump
          task jrap_dump_pcyc
            time 19:00
          task jrap_dump_post_pcyc
            trigger /prod12/rap/rap_16/pcyc/dump/jrap_dump_pcyc == complete
        endfamily
        family prep
          task jrap_prep_pcyc
            trigger /prod12/rap/rap_16/pcyc/dump/jrap_dump_pcyc == complete
          task jrap_prep_post_pcyc
            trigger /prod12/rap/rap_16/pcyc/jrap_analysis_pcyc == complete
        endfamily
        task jrap_analysis_pcyc
          trigger /prod12/rap/rap_16/pcyc/prep/jrap_prep_pcyc == complete and /prod12/rap/rap_16/prep/jrap_process_hydro == complete and /prod12/rap/rap_16/jrap_forecast == complete and /prod12/rap/rap_15/pcyc/jrap_forecast_pcyc == complete
        task jrap_updatebc_pcyc
          trigger /prod12/rap/rap_16/pcyc/jrap_analysis_pcyc == complete
        task jrap_forecast_pcyc
          trigger /prod12/rap/rap_16/pcyc/jrap_updatebc_pcyc == complete
      endfamily
    endfamily
    family rap_17
      edit CYC '17'
      family dump
        task jrap_dump_ehrrr
          time 17:16
        task jrap_dump
          time 17:26
        task jrap_realtime
          trigger /prod12/rap/rap_17/dump/jrap_dump == complete
        task jrap_dump_post
          trigger /prod12/rap/rap_17/dump/jrap_dump == complete
      endfamily
      family prep
        task jrap_prep
          trigger /prod12/rap/rap_17/dump/jrap_dump == complete
        task jrap_process_hydro
          trigger /prod12/rap/rap_17/prep/jrap_prep == complete
        task jrap_prep_post
          trigger /prod12/rap/rap_17/jrap_analysis == complete
      endfamily
      task jrap_analysis
        trigger /prod12/rap/rap_17/prep/jrap_process_hydro == complete
      task jrap_updatebc
        trigger /prod12/rap/rap_17/jrap_analysis == complete
      task jrap_forecast
        trigger /prod12/rap/rap_17/jrap_updatebc == complete
      family post
        task jrap_post_manager
          trigger /prod12/rap/rap_17/jrap_forecast == active or /prod12/rap/rap_17/jrap_forecast == complete
          event 1 release_post00
          event 2 release_post01
          event 3 release_post02
          event 4 release_post03
          event 5 release_post04
          event 6 release_post05
          event 7 release_post06
          event 8 release_post07
          event 9 release_post08
          event 10 release_post09
          event 11 release_post10
          event 12 release_post11
          event 13 release_post12
          event 14 release_post13
          event 15 release_post14
          event 16 release_post15
          event 17 release_post16
          event 18 release_post17
          event 19 release_post18
        family wrfbufr
          task jrap_wrfbufr_f00
            trigger /prod12/rap/rap_17/post/post/jrap_post_f00 == complete
          task jrap_wrfbufr_f01
            trigger /prod12/rap/rap_17/post/jrap_post_manager:release_post01
          task jrap_wrfbufr_f02
            trigger /prod12/rap/rap_17/post/jrap_post_manager:release_post02
          task jrap_wrfbufr_f03
            trigger /prod12/rap/rap_17/post/jrap_post_manager:release_post03
          task jrap_wrfbufr_f04
            trigger /prod12/rap/rap_17/post/jrap_post_manager:release_post04
          task jrap_wrfbufr_f05
            trigger /prod12/rap/rap_17/post/jrap_post_manager:release_post05
          task jrap_wrfbufr_f06
            trigger /prod12/rap/rap_17/post/jrap_post_manager:release_post06
          task jrap_wrfbufr_f07
            trigger /prod12/rap/rap_17/post/jrap_post_manager:release_post07
          task jrap_wrfbufr_f08
            trigger /prod12/rap/rap_17/post/jrap_post_manager:release_post08
          task jrap_wrfbufr_f09
            trigger /prod12/rap/rap_17/post/jrap_post_manager:release_post09
          task jrap_wrfbufr_f10
            trigger /prod12/rap/rap_17/post/jrap_post_manager:release_post10
          task jrap_wrfbufr_f11
            trigger /prod12/rap/rap_17/post/jrap_post_manager:release_post11
          task jrap_wrfbufr_f12
            trigger /prod12/rap/rap_17/post/jrap_post_manager:release_post12
          task jrap_wrfbufr_f13
            trigger /prod12/rap/rap_17/post/jrap_post_manager:release_post13
          task jrap_wrfbufr_f14
            trigger /prod12/rap/rap_17/post/jrap_post_manager:release_post14
          task jrap_wrfbufr_f15
            trigger /prod12/rap/rap_17/post/jrap_post_manager:release_post15
          task jrap_wrfbufr_f16
            trigger /prod12/rap/rap_17/post/jrap_post_manager:release_post16
          task jrap_wrfbufr_f17
            trigger /prod12/rap/rap_17/post/jrap_post_manager:release_post17
          task jrap_wrfbufr_f18
            trigger /prod12/rap/rap_17/post/jrap_post_manager:release_post18
        endfamily
        family post
          task jrap_post_f00
            trigger /prod12/rap/rap_17/post/jrap_post_manager:release_post00
          task jrap_post_f01
            trigger /prod12/rap/rap_17/post/jrap_post_manager:release_post01
          task jrap_post_f02
            trigger /prod12/rap/rap_17/post/jrap_post_manager:release_post02
          task jrap_post_f03
            trigger /prod12/rap/rap_17/post/jrap_post_manager:release_post03
          task jrap_post_f04
            trigger /prod12/rap/rap_17/post/jrap_post_manager:release_post04
          task jrap_post_f05
            trigger /prod12/rap/rap_17/post/jrap_post_manager:release_post05
          task jrap_post_f06
            trigger /prod12/rap/rap_17/post/jrap_post_manager:release_post06
          task jrap_post_f07
            trigger /prod12/rap/rap_17/post/jrap_post_manager:release_post07
          task jrap_post_f08
            trigger /prod12/rap/rap_17/post/jrap_post_manager:release_post08
          task jrap_post_f09
            trigger /prod12/rap/rap_17/post/jrap_post_manager:release_post09
          task jrap_post_f10
            trigger /prod12/rap/rap_17/post/jrap_post_manager:release_post10
          task jrap_post_f11
            trigger /prod12/rap/rap_17/post/jrap_post_manager:release_post11
          task jrap_post_f12
            trigger /prod12/rap/rap_17/post/jrap_post_manager:release_post12
          task jrap_post_f13
            trigger /prod12/rap/rap_17/post/jrap_post_manager:release_post13
          task jrap_post_f14
            trigger /prod12/rap/rap_17/post/jrap_post_manager:release_post14
          task jrap_post_f15
            trigger /prod12/rap/rap_17/post/jrap_post_manager:release_post15
          task jrap_post_f16
            trigger /prod12/rap/rap_17/post/jrap_post_manager:release_post16
          task jrap_post_f17
            trigger /prod12/rap/rap_17/post/jrap_post_manager:release_post17
          task jrap_post_f18
            trigger /prod12/rap/rap_17/post/jrap_post_manager:release_post18
        endfamily
      endfamily
      family prdgen
        task jrap_prdgen_f00
          trigger /prod12/rap/rap_17/post/post/jrap_post_f00 == complete
          event 1 grib_ready
        task jrap_prdgen_f01
          trigger /prod12/rap/rap_17/post/post/jrap_post_f01 == complete and /prod12/rap/rap_17/prdgen/jrap_prdgen_f00:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f02
          trigger /prod12/rap/rap_17/post/post/jrap_post_f02 == complete and /prod12/rap/rap_17/prdgen/jrap_prdgen_f00:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f03
          trigger /prod12/rap/rap_17/post/post/jrap_post_f03 == complete and /prod12/rap/rap_17/prdgen/jrap_prdgen_f00:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f04
          trigger /prod12/rap/rap_17/post/post/jrap_post_f04 == complete and /prod12/rap/rap_17/prdgen/jrap_prdgen_f01:grib_ready and /prod12/rap/rap_17/prdgen/jrap_prdgen_f03:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f05
          trigger /prod12/rap/rap_17/post/post/jrap_post_f05 == complete and /prod12/rap/rap_17/prdgen/jrap_prdgen_f02:grib_ready and /prod12/rap/rap_17/prdgen/jrap_prdgen_f03:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f06
          trigger /prod12/rap/rap_17/post/post/jrap_post_f06 == complete and /prod12/rap/rap_17/prdgen/jrap_prdgen_f03:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f07
          trigger /prod12/rap/rap_17/post/post/jrap_post_f07 == complete and /prod12/rap/rap_17/prdgen/jrap_prdgen_f04:grib_ready and /prod12/rap/rap_17/prdgen/jrap_prdgen_f06:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f08
          trigger /prod12/rap/rap_17/post/post/jrap_post_f08 == complete and /prod12/rap/rap_17/prdgen/jrap_prdgen_f05:grib_ready and /prod12/rap/rap_17/prdgen/jrap_prdgen_f06:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f09
          trigger /prod12/rap/rap_17/post/post/jrap_post_f09 == complete and /prod12/rap/rap_17/prdgen/jrap_prdgen_f06:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f10
          trigger /prod12/rap/rap_17/post/post/jrap_post_f10 == complete and /prod12/rap/rap_17/prdgen/jrap_prdgen_f07:grib_ready and /prod12/rap/rap_17/prdgen/jrap_prdgen_f09:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f11
          trigger /prod12/rap/rap_17/post/post/jrap_post_f11 == complete and /prod12/rap/rap_17/prdgen/jrap_prdgen_f08:grib_ready and /prod12/rap/rap_17/prdgen/jrap_prdgen_f09:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f12
          trigger /prod12/rap/rap_17/post/post/jrap_post_f12 == complete and /prod12/rap/rap_17/prdgen/jrap_prdgen_f09:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f13
          trigger /prod12/rap/rap_17/post/post/jrap_post_f13 == complete and /prod12/rap/rap_17/prdgen/jrap_prdgen_f10:grib_ready and /prod12/rap/rap_17/prdgen/jrap_prdgen_f12:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f14
          trigger /prod12/rap/rap_17/post/post/jrap_post_f14 == complete and /prod12/rap/rap_17/prdgen/jrap_prdgen_f11:grib_ready and /prod12/rap/rap_17/prdgen/jrap_prdgen_f12:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f15
          trigger /prod12/rap/rap_17/post/post/jrap_post_f15 == complete and /prod12/rap/rap_17/prdgen/jrap_prdgen_f12:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f16
          trigger /prod12/rap/rap_17/post/post/jrap_post_f16 == complete and /prod12/rap/rap_17/prdgen/jrap_prdgen_f13:grib_ready and /prod12/rap/rap_17/prdgen/jrap_prdgen_f15:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f17
          trigger /prod12/rap/rap_17/post/post/jrap_post_f17 == complete and /prod12/rap/rap_17/prdgen/jrap_prdgen_f14:grib_ready and /prod12/rap/rap_17/prdgen/jrap_prdgen_f15:grib_ready
          event 1 grib_ready
        task jrap_prdgen_f18
          trigger /prod12/rap/rap_17/post/post/jrap_post_f18 == complete and /prod12/rap/rap_17/prdgen/jrap_prdgen_f15:grib_ready
          event 1 grib_ready
      endfamily
      task jrap_postsnd
        trigger /prod12/rap/rap_17/post/wrfbufr == complete
      task jrap_cleanup
        trigger /prod12/rap/rap_17/prdgen == complete and /prod12/rap/rap_17/jrap_postsnd == complete
      family gempak
        task jrap_realtime_gempak
          trigger /prod12/rap/rap_17/dump/jrap_realtime == complete
        task jrap_gempak_meta
          trigger /prod12/rap/rap_17/gempak/jrap_gempak:release_meta
        task jrap_gempak
          trigger /prod12/rap/rap_17/prdgen/jrap_prdgen_f00:grib_ready
          event 1 release_meta
      endfamily
      family pcyc
        family dump
          task jrap_dump_pcyc
            time 20:00
          task jrap_dump_post_pcyc
            trigger /prod12/rap/rap_17/pcyc/dump/jrap_dump_pcyc == complete
        endfamily
        family prep
          task jrap_prep_pcyc
            trigger /prod12/rap/rap_17/pcyc/dump/jrap_dump_pcyc == complete
          task jrap_prep_post_pcyc
            trigger /prod12/rap/rap_17/pcyc/jrap_analysis_pcyc == complete
        endfamily
        task jrap_analysis_pcyc
          trigger /prod12/rap/rap_17/pcyc/prep/jrap_prep_pcyc == complete and /prod12/rap/rap_16/pcyc/jrap_forecast_pcyc == complete and /prod12/rap/rap_17/prep/jrap_process_hydro == complete and /prod12/rap/rap_17/jrap_forecast == complete
        task jrap_updatebc_pcyc
          trigger /prod12/rap/rap_17/pcyc/jrap_analysis_pcyc == complete
        task jrap_forecast_pcyc
          trigger /prod12/rap/rap_17/pcyc/jrap_updatebc_pcyc == complete
      endfamily
    endfamily
  endfamily
  family gtg
    edit ECF_FILES '/ecf/ecfnets/scripts/gtg'
    edit PROJ 'HOURLY'
    edit QUEUE 'prod2'
    family gtg_12
      edit CYC '12'
      task forecast_f00
        trigger /prod12/rap/rap_12/prdgen/jrap_prdgen_f00:grib_ready
        edit FHR '00'
      task forecast_f01
        trigger /prod12/rap/rap_12/prdgen/jrap_prdgen_f01:grib_ready
        edit FHR '01'
      task forecast_f02
        trigger /prod12/rap/rap_12/prdgen/jrap_prdgen_f02:grib_ready
        edit FHR '02'
      task forecast_f03
        trigger /prod12/rap/rap_12/prdgen/jrap_prdgen_f03:grib_ready
        edit FHR '03'
      task forecast_f06
        trigger /prod12/rap/rap_12/prdgen/jrap_prdgen_f06:grib_ready
        edit FHR '06'
      task forecast_f09
        trigger /prod12/rap/rap_12/prdgen/jrap_prdgen_f09:grib_ready
        edit FHR '09'
      task forecast_f12
        trigger /prod12/rap/rap_12/prdgen/jrap_prdgen_f12:grib_ready
        edit FHR '12'
      task forecast_f15
        trigger /prod12/rap/rap_12/prdgen/jrap_prdgen_f15:grib_ready
        edit FHR '15'
      task forecast_f18
        trigger /prod12/rap/rap_12/prdgen/jrap_prdgen_f18:grib_ready
        edit FHR '18'
    endfamily
    family gtg_13
      edit CYC '13'
      task forecast_f00
        trigger /prod12/rap/rap_13/prdgen/jrap_prdgen_f00:grib_ready
        edit FHR '00'
      task forecast_f01
        trigger /prod12/rap/rap_13/prdgen/jrap_prdgen_f01:grib_ready
        edit FHR '01'
      task forecast_f02
        trigger /prod12/rap/rap_13/prdgen/jrap_prdgen_f02:grib_ready
        edit FHR '02'
      task forecast_f03
        trigger /prod12/rap/rap_13/prdgen/jrap_prdgen_f03:grib_ready
        edit FHR '03'
      task forecast_f06
        trigger /prod12/rap/rap_13/prdgen/jrap_prdgen_f06:grib_ready
        edit FHR '06'
      task forecast_f09
        trigger /prod12/rap/rap_13/prdgen/jrap_prdgen_f09:grib_ready
        edit FHR '09'
      task forecast_f12
        trigger /prod12/rap/rap_13/prdgen/jrap_prdgen_f12:grib_ready
        edit FHR '12'
      task forecast_f15
        trigger /prod12/rap/rap_13/prdgen/jrap_prdgen_f15:grib_ready
        edit FHR '15'
      task forecast_f18
        trigger /prod12/rap/rap_13/prdgen/jrap_prdgen_f18:grib_ready
        edit FHR '18'
    endfamily
    family gtg_14
      edit CYC '14'
      task forecast_f00
        trigger /prod12/rap/rap_14/prdgen/jrap_prdgen_f00:grib_ready
        edit FHR '00'
      task forecast_f01
        trigger /prod12/rap/rap_14/prdgen/jrap_prdgen_f01:grib_ready
        edit FHR '01'
      task forecast_f02
        trigger /prod12/rap/rap_14/prdgen/jrap_prdgen_f02:grib_ready
        edit FHR '02'
      task forecast_f03
        trigger /prod12/rap/rap_14/prdgen/jrap_prdgen_f03:grib_ready
        edit FHR '03'
      task forecast_f06
        trigger /prod12/rap/rap_14/prdgen/jrap_prdgen_f06:grib_ready
        edit FHR '06'
      task forecast_f09
        trigger /prod12/rap/rap_14/prdgen/jrap_prdgen_f09:grib_ready
        edit FHR '09'
      task forecast_f12
        trigger /prod12/rap/rap_14/prdgen/jrap_prdgen_f12:grib_ready
        edit FHR '12'
      task forecast_f15
        trigger /prod12/rap/rap_14/prdgen/jrap_prdgen_f15:grib_ready
        edit FHR '15'
      task forecast_f18
        trigger /prod12/rap/rap_14/prdgen/jrap_prdgen_f18:grib_ready
        edit FHR '18'
    endfamily
    family gtg_15
      edit CYC '15'
      task forecast_f00
        trigger /prod12/rap/rap_15/prdgen/jrap_prdgen_f00:grib_ready
        edit FHR '00'
      task forecast_f01
        trigger /prod12/rap/rap_15/prdgen/jrap_prdgen_f01:grib_ready
        edit FHR '01'
      task forecast_f02
        trigger /prod12/rap/rap_15/prdgen/jrap_prdgen_f02:grib_ready
        edit FHR '02'
      task forecast_f03
        trigger /prod12/rap/rap_15/prdgen/jrap_prdgen_f03:grib_ready
        edit FHR '03'
      task forecast_f06
        trigger /prod12/rap/rap_15/prdgen/jrap_prdgen_f06:grib_ready
        edit FHR '06'
      task forecast_f09
        trigger /prod12/rap/rap_15/prdgen/jrap_prdgen_f09:grib_ready
        edit FHR '09'
      task forecast_f12
        trigger /prod12/rap/rap_15/prdgen/jrap_prdgen_f12:grib_ready
        edit FHR '12'
      task forecast_f15
        trigger /prod12/rap/rap_15/prdgen/jrap_prdgen_f15:grib_ready
        edit FHR '15'
      task forecast_f18
        trigger /prod12/rap/rap_15/prdgen/jrap_prdgen_f18:grib_ready
        edit FHR '18'
    endfamily
    family gtg_16
      edit CYC '16'
      task forecast_f00
        trigger /prod12/rap/rap_16/prdgen/jrap_prdgen_f00:grib_ready
        edit FHR '00'
      task forecast_f01
        trigger /prod12/rap/rap_16/prdgen/jrap_prdgen_f01:grib_ready
        edit FHR '01'
      task forecast_f02
        trigger /prod12/rap/rap_16/prdgen/jrap_prdgen_f02:grib_ready
        edit FHR '02'
      task forecast_f03
        trigger /prod12/rap/rap_16/prdgen/jrap_prdgen_f03:grib_ready
        edit FHR '03'
      task forecast_f06
        trigger /prod12/rap/rap_16/prdgen/jrap_prdgen_f06:grib_ready
        edit FHR '06'
      task forecast_f09
        trigger /prod12/rap/rap_16/prdgen/jrap_prdgen_f09:grib_ready
        edit FHR '09'
      task forecast_f12
        trigger /prod12/rap/rap_16/prdgen/jrap_prdgen_f12:grib_ready
        edit FHR '12'
      task forecast_f15
        trigger /prod12/rap/rap_16/prdgen/jrap_prdgen_f15:grib_ready
        edit FHR '15'
      task forecast_f18
        trigger /prod12/rap/rap_16/prdgen/jrap_prdgen_f18:grib_ready
        edit FHR '18'
    endfamily
    family gtg_17
      edit CYC '17'
      task forecast_f00
        trigger /prod12/rap/rap_17/prdgen/jrap_prdgen_f00:grib_ready
        edit FHR '00'
      task forecast_f01
        trigger /prod12/rap/rap_17/prdgen/jrap_prdgen_f01:grib_ready
        edit FHR '01'
      task forecast_f02
        trigger /prod12/rap/rap_17/prdgen/jrap_prdgen_f02:grib_ready
        edit FHR '02'
      task forecast_f03
        trigger /prod12/rap/rap_17/prdgen/jrap_prdgen_f03:grib_ready
        edit FHR '03'
      task forecast_f06
        trigger /prod12/rap/rap_17/prdgen/jrap_prdgen_f06:grib_ready
        edit FHR '06'
      task forecast_f09
        trigger /prod12/rap/rap_17/prdgen/jrap_prdgen_f09:grib_ready
        edit FHR '09'
      task forecast_f12
        trigger /prod12/rap/rap_17/prdgen/jrap_prdgen_f12:grib_ready
        edit FHR '12'
      task forecast_f15
        trigger /prod12/rap/rap_17/prdgen/jrap_prdgen_f15:grib_ready
        edit FHR '15'
      task forecast_f18
        trigger /prod12/rap/rap_17/prdgen/jrap_prdgen_f18:grib_ready
        edit FHR '18'
    endfamily
  endfamily
  family verf
    edit ECF_FILES '/ecf/ecfnets/scripts/verf'
    edit PROJ 'FVS-OPS'
    family grid2grid
      edit CYC '00'
      edit CYCLES '00'
      task jverf_grid2grid_dust
        time 12:30
      task jverf_grid2grid_smoke
        time 12:40
    endfamily
    family gridtobs_12
      edit CYC '12'
      edit CYCLES '12'
      task jverf_gridtobs_srefak
        edit QUEUE 'prod2'
        time 12:40
      task jverf_gridtobs_hirtma
        time 12:30
      task jverf_gridtobs_prrtma
        time 12:30
      task jverf_gridtobs_akrtma
        time 12:30
      task jverf_gridtobs_aqm
        edit CYCLES '12 13 14'
        time 12:30
      task jverf_gridtobs_pm1
        edit CYCLES '12 13 14'
        time 12:30
      task jverf_gridtobs_pm
        edit CYCLES '12 13 14'
        time 12:30
      task jverf_gridtobs_ngac
        time 12:30
      task jverf_gridtobs_gdas
        time 12:30
      task jverf_gridtobs_dgex
        time 12:32
      task jverf_gridtobs_hawaiinest
        time 12:32
      task jverf_gridtobs_priconest
        time 12:32
      task jverf_gridtobs_rtma2p5
        time 12:32
      task jverf_gridtobs_urma2p5
        time 12:32
      task jverf_gridtobs_ndas
        time 12:32
      task jverf_gridtobs_hrrr
        edit QUEUE 'prod2'
        time 12:05
      task jverf_gridtobs_aknest
        time 12:35
      task jverf_gridtobs_narre
        time 12:35
      task jverf_gridtobs_fwis
        time 12:35
      task jverf_gridtobs_smartinit
        time 12:35
      task jverf_gridtobs_smartinit2p5
        time 12:35
      task jverf_gridtobs_conusnest
        edit QUEUE 'prod2'
        time 12:35
      task jverf_gridtobs_nssl
        edit CYCLES '12 13 14'
        time 12:35
      task jverf_gridtobs_hiresw
        time 12:40
      task jverf_gridtobs_nam
        time 12:40
      task jverf_gridtobs_rap
        time 12:40
      task jverf_gridtobs_gfs
        time 12:40
      task jverf_gridtobs_srefx
        edit QUEUE 'prod2'
        time 12:40
      task jverf_gridtobs_gfse
        time 12:40
      task jverf_gridtobs_srefensx
        trigger /prod12/verf/gridtobs_12/jverf_gridtobs_srefx eq complete
        edit QUEUE 'prod2'
        edit NPROC '33'
      task jverf_gridtobs_sref
        edit QUEUE 'prod2'
        time 12:40
      task jverf_gridtobs_srefens
        trigger /prod12/verf/gridtobs_12/jverf_gridtobs_sref eq complete
        edit QUEUE 'prod2'
        edit NPROC '33'
    endfamily
    family gridtobs_13
      edit CYC '13'
      edit CYCLES '13'
      task jverf_gridtobs_hrrr
        edit QUEUE 'prod2'
        time 13:05
      task jverf_gridtobs_akrtma
        time 13:30
      task jverf_gridtobs_hirtma
        time 13:30
      task jverf_gridtobs_prrtma
        time 13:30
      task jverf_gridtobs_rtma2p5
        time 13:32
      task jverf_gridtobs_urma2p5
        time 13:32
      task jverf_gridtobs_fwis
        time 13:35
      task jverf_gridtobs_narre
        time 13:35
      task jverf_gridtobs_conusnest
        edit QUEUE 'prod2'
        time 13:35
      task jverf_gridtobs_rap
        time 13:40
      task jverf_gridtobs_nam
        time 13:40
    endfamily
    family gridtobs_14
      edit CYC '14'
      edit CYCLES '14'
      task jverf_gridtobs_hrrr
        edit QUEUE 'prod2'
        time 14:05
      task jverf_gridtobs_akrtma
        time 14:30
      task jverf_gridtobs_hirtma
        time 14:30
      task jverf_gridtobs_prrtma
        time 14:30
      task jverf_gridtobs_rtma2p5
        time 14:32
      task jverf_gridtobs_urma2p5
        time 14:32
      task jverf_gridtobs_fwis
        time 14:35
      task jverf_gridtobs_narre
        time 14:35
      task jverf_gridtobs_conusnest
        edit QUEUE 'prod2'
        time 14:35
      task jverf_gridtobs_rap
        time 14:40
      task jverf_gridtobs_nam
        time 14:40
    endfamily
    family gridtobs_15
      edit CYC '15'
      edit CYCLES '15'
      task jverf_gridtobs_srefak
        edit QUEUE 'prod2'
        time 15:40
      task jverf_gridtobs_hirtma
        time 15:30
      task jverf_gridtobs_prrtma
        time 15:30
      task jverf_gridtobs_akrtma
        time 15:30
      task jverf_gridtobs_aqm
        edit CYCLES '15 16 17'
        time 15:30
      task jverf_gridtobs_pm1
        edit CYCLES '15 16 17'
        time 15:30
      task jverf_gridtobs_pm
        edit CYCLES '15 16 17'
        time 15:30
      task jverf_gridtobs_ngac
        time 15:30
      task jverf_gridtobs_gdas
        time 15:30
      task jverf_gridtobs_hawaiinest
        time 15:32
      task jverf_gridtobs_priconest
        time 15:32
      task jverf_gridtobs_urma2p5
        time 15:32
      task jverf_gridtobs_rtma2p5
        time 15:32
      task jverf_gridtobs_ndas
        time 15:32
      task jverf_gridtobs_hrrr
        edit QUEUE 'prod2'
        time 15:05
      task jverf_gridtobs_aknest
        time 15:35
      task jverf_gridtobs_fwis
        time 15:35
      task jverf_gridtobs_narre
        time 15:35
      task jverf_gridtobs_smartinit
        time 15:35
      task jverf_gridtobs_smartinit2p5
        time 15:35
      task jverf_gridtobs_conusnest
        edit QUEUE 'prod2'
        time 15:35
      task jverf_gridtobs_nssl
        edit CYCLES '15 16 17'
        time 15:35
      task jverf_gridtobs_hiresw
        time 15:40
      task jverf_gridtobs_nam
        time 15:40
      task jverf_gridtobs_rap
        time 15:40
      task jverf_gridtobs_gfs
        time 15:40
      task jverf_gridtobs_srefx
        edit QUEUE 'prod2'
        time 15:40
      task jverf_gridtobs_srefensx
        trigger /prod12/verf/gridtobs_15/jverf_gridtobs_srefx eq complete
        edit QUEUE 'prod2'
        edit NPROC '27'
      task jverf_gridtobs_sref
        edit QUEUE 'prod2'
        time 15:40
      task jverf_gridtobs_srefens
        trigger /prod12/verf/gridtobs_15/jverf_gridtobs_sref eq complete
        edit QUEUE 'prod2'
        edit NPROC '27'
    endfamily
    family gridtobs_16
      edit CYC '16'
      edit CYCLES '16'
      task jverf_gridtobs_hrrr
        edit QUEUE 'prod2'
        time 16:05
      task jverf_gridtobs_akrtma
        time 16:30
      task jverf_gridtobs_hirtma
        time 16:30
      task jverf_gridtobs_prrtma
        time 16:30
      task jverf_gridtobs_rtma2p5
        time 16:32
      task jverf_gridtobs_urma2p5
        time 16:32
      task jverf_gridtobs_narre
        time 16:35
      task jverf_gridtobs_fwis
        time 16:35
      task jverf_gridtobs_conusnest
        edit QUEUE 'prod2'
        time 16:35
      task jverf_gridtobs_rap
        time 16:40
      task jverf_gridtobs_nam
        time 16:40
    endfamily
    family gridtobs_17
      edit CYC '17'
      edit CYCLES '17'
      task jverf_gridtobs_hrrr
        edit QUEUE 'prod2'
        time 17:05
      task jverf_gridtobs_akrtma
        time 17:30
      task jverf_gridtobs_hirtma
        time 17:30
      task jverf_gridtobs_prrtma
        time 17:30
      task jverf_gridtobs_rtma2p5
        time 17:32
      task jverf_gridtobs_urma2p5
        time 17:32
      task jverf_gridtobs_fwis
        time 17:35
      task jverf_gridtobs_narre
        time 17:35
      task jverf_gridtobs_conusnest
        edit QUEUE 'prod2'
        time 17:35
      task jverf_gridtobs_rap
        time 17:40
      task jverf_gridtobs_nam
        time 17:40
    endfamily
  endfamily
  family ndas18
    edit ECF_JOB_CMD '/ecf/ecfutils/unixsubmit %ECF_JOB% %ECF_JOBOUT% ibmsp'
    edit ECF_FILES '/ecf/ecfnets/scripts/ndas'
    edit PROJ 'RDAS-OPS'
    edit CYC '18'
    family main
      task jndas_main
        trigger /prod12/prod_filemanager/jprod_setup == complete
        event 1 release_mkbnd_tm12
        event 2 release_mkbnd_tm09
        event 3 release_mkbnd_tm06
        event 4 release_mkbnd_tm03
        event 5 release_ndas_coldstart
        time 16:30
      family coldstart
        defstatus complete
        task jndas_coldstart_prep
          trigger /prod12/ndas18/main/jndas_main:release_ndas_coldstart
        task jndas_coldstart_partialcyc
          trigger jndas_coldstart_prep == complete
        task jndas_coldstart_sfcupdate
          trigger jndas_coldstart_partialcyc== complete
      endfamily
    endfamily
    family time_minus_12hr
      edit TMMARK '12'
      family mkboundaries
        task jndas_mkbnd
          trigger /prod12/ndas18/main/jndas_main:release_mkbnd_tm12
        task jndas_combc
          trigger jndas_mkbnd == complete
      endfamily
      family dump
        task jndas_tropcy_reloc
          time 16:40
        task jndas_dump
          time 16:40
        task jndas_dump2
          time 16:40
        task jndas_dump_post
          trigger jndas_dump == complete
      endfamily
      family prep
        task jndas_prep
          trigger /prod12/prod_filemanager/jprod_setup == complete and /prod12/ndas18/time_minus_12hr/dump/jndas_dump == complete and /prod12/ndas18/time_minus_12hr/dump/jndas_dump2 == complete and /prod12/ndas18/time_minus_12hr/dump/jndas_tropcy_reloc == complete
        task jndas_prep_post
          trigger /prod12/ndas18/time_minus_12hr/jndas_analysis == complete
      endfamily
      task jndas_analysis
        trigger /prod12/ndas18/main == complete and /prod12/ndas18/time_minus_12hr/mkboundaries/jndas_combc == complete and /prod12/ndas18/time_minus_12hr/prep/jndas_prep == complete
      task jndas_verfrad
        trigger jndas_analysis == complete
      task jndas_forecast
        trigger jndas_analysis == complete
      task jndas_post
        trigger jndas_forecast == active
    endfamily
    family time_minus_09hr
      edit TMMARK '09'
      family mkboundaries
        task jndas_mkbnd
          trigger /prod12/ndas18/main/jndas_main:release_mkbnd_tm09
        task jndas_combc
          trigger jndas_mkbnd == complete
      endfamily
      family dump
        task jndas_tropcy_reloc
          time 16:40
        task jndas_dump
          time 16:40
        task jndas_dump2
          time 16:40
        task jndas_dump_post
          trigger jndas_dump == complete
      endfamily
      family prep
        task jndas_prep
          trigger /prod12/prod_filemanager/jprod_setup == complete and /prod12/ndas18/time_minus_09hr/dump/jndas_dump == complete and /prod12/ndas18/time_minus_09hr/dump/jndas_dump2 == complete and /prod12/ndas18/time_minus_09hr/dump/jndas_tropcy_reloc == complete
        task jndas_prep_post
          trigger /prod12/ndas18/time_minus_09hr/jndas_analysis == complete
      endfamily
      task jndas_analysis
        trigger /prod12/ndas18/time_minus_09hr/prep/jndas_prep == complete and /prod12/ndas18/time_minus_12hr/jndas_forecast == complete
      task jndas_verfrad
        trigger jndas_analysis == complete
      task jndas_forecast
        trigger jndas_analysis == complete
      task jndas_post
        trigger jndas_forecast == active
    endfamily
    family time_minus_06hr
      edit TMMARK '06'
      family mkboundaries
        task jndas_mkbnd
          trigger /prod12/ndas18/main/jndas_main:release_mkbnd_tm06
        task jndas_combc
          trigger jndas_mkbnd == complete
      endfamily
      family dump
        task jndas_tropcy_reloc
          time 16:40
        task jndas_dump
          time 16:40
        task jndas_dump2
          time 16:40
        task jndas_dump_post
          trigger jndas_dump == complete
      endfamily
      family prep
        task jndas_prep
          trigger /prod12/prod_filemanager/jprod_setup == complete and /prod12/ndas18/time_minus_06hr/dump/jndas_dump == complete and /prod12/ndas18/time_minus_06hr/dump/jndas_dump2 == complete and /prod12/ndas18/time_minus_06hr/dump/jndas_tropcy_reloc == complete
        task jndas_prep_post
          trigger /prod12/ndas18/time_minus_06hr/jndas_analysis == complete
      endfamily
      task jndas_analysis
        trigger /prod12/ndas18/time_minus_06hr/prep/jndas_prep == complete and /prod12/ndas18/time_minus_09hr/jndas_forecast == complete
      task jndas_verfrad
        trigger jndas_analysis == complete
      task jndas_forecast
        trigger jndas_analysis == complete
      task jndas_post
        trigger jndas_forecast == active
    endfamily
    family nest_prico
      edit DOMAIN 'prico'
      edit N '8'
      task jndas_npsguess
        trigger /prod12/ndas18/time_minus_03hr/jndas_prdgen_guess == complete
      task jndas_nestiniguess
        trigger jndas_npsguess == complete
      task jndas_nestsfcupdate
        trigger jndas_nestiniguess == complete
    endfamily
    family nest_hawaii
      edit DOMAIN 'hawaii'
      edit N '8'
      task jndas_npsguess
        trigger /prod12/ndas18/time_minus_03hr/jndas_prdgen_guess == complete
      task jndas_nestiniguess
        trigger jndas_npsguess == complete
      task jndas_nestsfcupdate
        trigger jndas_nestiniguess == complete
    endfamily
    family nest_firewx
      edit DOMAIN 'firewx'
      edit N '8'
      task jndas_npsguess
        trigger /prod12/ndas18/time_minus_03hr/jndas_prdgen_guess == complete
      task jndas_nestiniguess
        trigger jndas_npsguess == complete
      task jndas_nestsfcupdate
        trigger jndas_nestiniguess == complete
    endfamily
    family nest_alaska
      edit DOMAIN 'alaska'
      edit N '16'
      task jndas_npsguess
        trigger /prod12/ndas18/time_minus_03hr/jndas_prdgen_guess == complete
      task jndas_nestiniguess
        trigger jndas_npsguess == complete
      task jndas_nestsfcupdate
        trigger jndas_nestiniguess == complete
    endfamily
    family nest_conus
      edit DOMAIN 'conus'
      edit N '24'
      task jndas_npsguess
        trigger /prod12/ndas18/time_minus_03hr/jndas_prdgen_guess == complete
      task jndas_nestiniguess
        trigger jndas_npsguess == complete
      task jndas_nestsfcupdate
        trigger jndas_nestiniguess == complete
    endfamily
    family time_minus_03hr
      edit TMMARK '03'
      family mkboundaries
        task jndas_mkbnd
          trigger /prod12/ndas18/main/jndas_main:release_mkbnd_tm03
        task jndas_combc
          trigger jndas_mkbnd == complete
      endfamily
      family dump
        task jndas_tropcy_reloc
          time 17:00
        task jndas_dump
          time 17:00
        task jndas_dump2
          time 17:00
        task jndas_dump_post
          trigger jndas_dump == complete
      endfamily
      family prep
        task jndas_prep
          trigger /prod12/prod_filemanager/jprod_setup == complete and /prod12/ndas18/time_minus_03hr/dump/jndas_dump == complete and /prod12/ndas18/time_minus_03hr/dump/jndas_dump2 == complete and /prod12/ndas18/time_minus_03hr/dump/jndas_tropcy_reloc == complete
        task jndas_prep_post
          trigger /prod12/ndas18/time_minus_03hr/jndas_analysis == complete
      endfamily
      task jndas_analysis
        trigger /prod12/ndas18/time_minus_03hr/prep/jndas_prep == complete and /prod12/ndas18/time_minus_06hr/jndas_forecast == complete
      task jndas_verfrad
        trigger jndas_analysis == complete
      task jndas_forecast
        trigger jndas_analysis == complete
      task jndas_post_guess
        trigger jndas_forecast == complete
      task jndas_prdgen_guess
        trigger jndas_post_guess == complete
      task jndas_post
        trigger jndas_forecast == active
    endfamily
    task jndas_cleanup
      trigger /prod12/ndas18/main == complete and /prod12/ndas18/time_minus_12hr == complete and /prod12/ndas18/time_minus_09hr == complete and /prod12/ndas18/time_minus_06hr == complete and /prod12/ndas18/time_minus_03hr == complete and /prod12/ndas18/nest_prico == complete and /prod12/ndas18/nest_hawaii == complete and /prod12/ndas18/nest_firewx == complete and /prod12/ndas18/nest_alaska == complete and /prod12/ndas18/nest_conus == complete
    task jndas_tropcy_qc
      time 22:00
  endfamily
  family genesis_tracker
    edit PROJ 'HOURLY-OPS'
    edit QUEUE 'prod2'
    task jtracker_nvgm
      edit CYC3 '15'
      time 20:00
    task jtracker_nam
      trigger /prod12/nam/prdgen/jnam_prdgen_f84 == complete
    task jtracker_eens
      trigger /prod12/ens_tracker/ecme/jeens_tc_track == complete
    task jtracker_gfs
      trigger /prod12/gfs/prdgen/jgfs_pgrb2_f180 == complete
    task jtracker_cmc
      trigger /prod12/ens_tracker/cmce/jcmc_tc_track == complete
      time 18:00
    task jtracker_ukmet
      trigger /prod12/ukmet/jukmet == complete
      time 18:25
    task jtracker_gefs
      trigger ../gefs/post_processing/jgefs_ensstat_low == complete and ../ens_tracker/gefs/jgefs_tc_track == complete
      edit RES '0p50'
      edit ECF_JOB_CMD '/ecf/ecfutils/unixsubmit %ECF_JOB% %ECF_JOBOUT% ibmsp'
    task jtracker_sref
      trigger ../sref15/gempak/jsref_gempak == complete
      edit CYC3 '15'
    task jtracker_ecmwf
      trigger /hourly/ecmwf/jecmwf_12 == complete
      time 21:40
    task jtracker_cens
      trigger /prod18/cmcens/jcmc_ens_post_12 == complete
      time 22:00
  endfamily
  family raw12
    task jrw112
      trigger /prod12/prod_filemanager/jprod_setup == complete
      edit ECF_PASS 'FREE'
      time 12:20
    task jrw212
      trigger /prod12/prod_filemanager/jprod_setup == complete
      edit ECF_PASS 'FREE'
      time 12:40
    task jrw013
      trigger /prod12/prod_filemanager/jprod_setup == complete
      edit ECF_PASS 'FREE'
      time 13:05
    task jrw313
      trigger /prod12/prod_filemanager/jprod_setup == complete
      edit ECF_PASS 'FREE'
      time 13:20
    task jrw413
      trigger /prod12/prod_filemanager/jprod_setup == complete
      edit ECF_PASS 'FREE'
      time 13:40
    task jrw514
      trigger /prod12/prod_filemanager/jprod_setup == complete
      edit ECF_PASS 'FREE'
      time 14:20
    task jrw015
      trigger /prod12/prod_filemanager/jprod_setup == complete
      edit ECF_PASS 'FREE'
      time 15:05
    task jrw115
      trigger /prod12/prod_filemanager/jprod_setup == complete
      edit ECF_PASS 'FREE'
      time 15:20
    task jrw215
      trigger /prod12/prod_filemanager/jprod_setup == complete
      edit ECF_PASS 'FREE'
      time 15:40
    task jrw316
      trigger /prod12/prod_filemanager/jprod_setup == complete
      edit ECF_PASS 'FREE'
      time 16:20
    task jrw416
      trigger /prod12/prod_filemanager/jprod_setup == complete
      edit ECF_PASS 'FREE'
      time 16:40
    task jrw517
      trigger /prod12/prod_filemanager/jprod_setup == complete
      edit ECF_PASS 'FREE'
      time 17:20
    task jrw018
      trigger /prod12/prod_filemanager/jprod_setup == complete
      edit ECF_PASS 'FREE'
      time 18:05
    task jrw118
      trigger /prod12/prod_filemanager/jprod_setup == complete
      edit ECF_PASS 'FREE'
      time 18:20
    task jrw218
      trigger /prod12/prod_filemanager/jprod_setup == complete
      edit ECF_PASS 'FREE'
      time 18:40
    task jrw319
      trigger /prod12/prod_filemanager/jprod_setup == complete
      edit ECF_PASS 'FREE'
      time 19:20
    task jrw419
      trigger /prod12/prod_filemanager/jprod_setup == complete
      edit ECF_PASS 'FREE'
      time 19:40
    task jrw520
      trigger /prod12/prod_filemanager/jprod_setup == complete
      edit ECF_PASS 'FREE'
      time 20:20
    task jrw021
      trigger /prod12/prod_filemanager/jprod_setup == complete
      edit ECF_PASS 'FREE'
      time 21:05
    task jrw121
      trigger /prod12/prod_filemanager/jprod_setup == complete
      edit ECF_PASS 'FREE'
      time 21:20
    task jrw221
      trigger /prod12/prod_filemanager/jprod_setup == complete
      edit ECF_PASS 'FREE'
      time 21:40
    task jrw322
      trigger /prod12/prod_filemanager/jprod_setup == complete
      edit ECF_PASS 'FREE'
      time 22:20
    task jrw422
      trigger /prod12/prod_filemanager/jprod_setup == complete
      edit ECF_PASS 'FREE'
      time 22:40
    task jrw523
      trigger /prod12/prod_filemanager/jprod_setup == complete
      edit ECF_PASS 'FREE'
      time 23:20
  endfamily
  family nwps
    edit PROJ 'OPS'
    family prep
      family estofs
        edit ECF_FILES '/ecf/ecfnets/scripts/nwps'
        edit ECF_STATUS_CMD '/ecf/ecfutils/unixstatus.20141218 %ECF_JOB% %ECF_JOBOUT% > %ECF_JOBOUT%.stat'
        edit MODEL 'nwps'
        edit COMROOT '/com'
        edit MODULES 'grib_util cfp NetCDF prod_util ibmpe ics'
        edit QUEUESERV 'prod_serv'
        edit ECF_URL_BASE 'http://www2.nco.ncep.noaa.gov'
        edit ECF_URL 'pmb/spatools/nwps_status'
        edit KEEPDATA 'NO'
        edit OFSTYPE 'estofs'
        task jnwps_ofs_prep
          late -s +00:03 -a 17:41
          trigger (/prod12/estofs/atl/jestofs_post == complete)
          edit ECF_JOB_CMD '/ecf/ecfutils/unixsubmit %ECF_JOB% %ECF_JOBOUT% ibmsp'
          time 17:40
      endfamily
    endfamily
  endfamily
  family fnmocens
    edit PROJ 'FNMOCENS-OPS'
    edit COM '/com2'
    edit QUEUE 'prod2'
    task jfnmoc_ens_bias
      time 15:30
    task jfnmoc_ens_prdgen
      time 18:35
    task jfnmoc_ens_gempak
      trigger jfnmoc_ens_prdgen == complete
    task jfnmoc_ens_debias
      time 19:00
    task jfnmoc_ens_debias_gempak
      trigger jfnmoc_ens_debias == complete
  endfamily
  task cycle_end
    edit ECF_JOB_CMD '%ECF_JOB% 1> %ECF_JOBOUT% 2>&1'
    edit ECF_PASS 'FREE'
    time 11:00
endsuite
