set -x
###############################################################################
#
# Export variables to the default values
#  - first common variables, then model specific ones
#  - different machines, different defaults:
#
###############################################################################

if [ $MACHINE_ID = wcoss ]; then

  TASKS_dflt=32  ; TPN_dflt=16 ; INPES_dflt=05 ; JNPES_dflt=06 ; WTPG_dflt=2
  TASKS_thrd=16  ; TPN_thrd=08 ; INPES_thrd=03 ; JNPES_thrd=05 ; WTPG_thrd=1
  TASKS_nest=96  ; TPN_nest=16 ; INPES_nest=02 ; JNPES_nest=02 ; WTPG_nest=1
  TASKS_fltr=64  ; TPN_fltr=16 ; INPES_fltr=02 ; JNPES_fltr=02 ; WTPG_fltr=1
  TASKS_mvg1=96  ; TPN_mvg1=16 ; INPES_mvg1=05 ; JNPES_mvg1=07 ; WTPG_mvg1=1
  TASKS_mvg2=96  ; TPN_mvg2=16 ; INPES_mvg2=05 ; JNPES_mvg2=18 ; WTPG_mvg2=2

elif [ $MACHINE_ID = gaea -o $MACHINE_ID = theia ]; then

  TASKS_dflt=48  ; TPN_dflt=   ; INPES_dflt=05 ; JNPES_dflt=09 ; WTPG_dflt=3
  TASKS_thrd=48  ; TPN_thrd=   ; INPES_thrd=05 ; JNPES_thrd=09 ; WTPG_thrd=3
  TASKS_nest=96  ; TPN_nest=   ; INPES_nest=02 ; JNPES_nest=02 ; WTPG_nest=1
  TASKS_fltr=64  ; TPN_fltr=   ; INPES_fltr=02 ; JNPES_fltr=02 ; WTPG_fltr=1
  TASKS_mvg1=96  ; TPN_mvg1=   ; INPES_mvg1=05 ; JNPES_mvg1=07 ; WTPG_mvg1=1
  TASKS_mvg2=96  ; TPN_mvg2=   ; INPES_mvg2=05 ; JNPES_mvg2=18 ; WTPG_mvg2=2

fi

export_common ()
{
export THRD=1
export WTPG=$WTPG_dflt
export WLCLK=15
export GEFS_ENSEMBLE=0
export GEN_ENSEMBLE=0
export WRITE_DOPOST=.false.
export POST_GRIBVERSION='"grib1"'
export QUEUE=debug
}

export_nmm ()
{
export_common
export INPES=$INPES_dflt ; export JNPES=$JNPES_dflt ; export WTPG=$WTPG_dflt
export TASKS=$TASKS_dflt ; export TPN=$TPN_dflt
export GBRG=reg          ; export NEMSI=false       ; export RSTRT=false
export AFFN=core         ; export NODE=1
export NCHILD=0          ; export MODE=1-way        ; export WGT=0.0
export FCSTL=48
export PCPFLG=false      ; export WPREC=false
export TS=false
export RADTN=rrtm        ; export CONVC=bmj         ; export TURBL=myj
export NP3D=3            ; export CLDFRACTION=default
export SFC_LAYER=myj     ; export LAND_SURFACE=noah
export MICRO=fer_hires   ; export gfsP=false        ; export SPEC_ADV=false
}

export_gsm ()
{
export_common
if [ ${pex:-1} -eq 2 ] ; then
 export TASKS=48  ; export PE1=48       ; export NSOUT=0       ; export QUILT=.false.
else
 export TASKS=32  ; export PE1=32       ; export NSOUT=0       ; export QUILT=.false.
fi
export NDAYS=2   ; export CP2=.false.  ; export IAER=0        ; export FHRES=180
export WRTGP=1   ; export FDFI=0       ; export ADIABATIC=.false. ; export REDUCEDGRID=.true.
export FHZER=6
export wave=62   ; export THRD=1
export lm=64     ; export lsoil=4         ; export MEMBER_NAMES=c00
export IDVC=2    ; export THERMODYN_ID=1  ; export SFCPRESS_ID=1 ; export SPECTRALLOOP=2
export NEMSIOIN=.false.  ; export NEMSIOOUT=.false. ; export rungfstest=.true.
export SIGIOIN=.true.    ; export SIGIOOUT=.true.   ; export SFCIOOUT=.true.
export FHSWR=3600        ;  export FHLWR=3600       ; LDFI_SPECT=.true.
export CDATE=2012010100
export GOCART_AER2POST=.false.
export NST_FCST=0  ; export NDSLFV=.false.  ; export IDEA=.false.
export SLG=.false.
export A2OI_OUT=.false. ; export NGRID_A2OI=48
}

export_fim ()
{
export_common
export FIM_USE_NEMS=true
}

export_nems ()
{
export atm_model=none
export atm_petlist_bounds="-1 -1"
export ocn_model=none
export ocn_petlist_bounds="-1 -1"
export ice_model=none
export ice_petlist_bounds="-1 -1"
export med_model=nems
export med_petlist_bounds="-1 -1"
export med_atm_coupling_interval_sec=-1
export med_ocn_coupling_interval_sec=-1
}

