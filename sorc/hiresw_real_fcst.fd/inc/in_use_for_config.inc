!STARTOFREGISTRYGENERATEDINCLUDE 'inc/scalar_indices.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
IF(TRIM(vname).EQ.'dfi_pd')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_pint')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_dwdt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_t')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_q')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_u')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_v')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_q2')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_cwm')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_rrw')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_stc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_smc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_sh2o')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_snow')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_snowh')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_canwat')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_nmm_tsk')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_snowc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.1
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.1
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_pd')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_pint')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_dwdt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_t')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_q')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_u')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_v')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_q2')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_cwm')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_rrw')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_stc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_smc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_sh2o')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_snow')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_snowh')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_canwat')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_nmm_tsk')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_snowc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.2
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.2
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_pd')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_pint')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_dwdt')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_t')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_q')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_u')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_v')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_q2')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_cwm')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_rrw')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_stc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_smc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_sh2o')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_snow')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_snowh')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_canwat')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_nmm_tsk')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
IF(TRIM(vname).EQ.'dfi_snowc')THEN
  IF(uses.EQ.0)THEN
    in_use = model_config_rec%dfi_opt.EQ.3
    uses = 1
  ELSE
    in_use = in_use.OR.model_config_rec%dfi_opt.EQ.3
  ENDIF
ENDIF
!ENDOFREGISTRYGENERATEDINCLUDE
