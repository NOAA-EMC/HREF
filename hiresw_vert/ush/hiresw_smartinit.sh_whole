#!/bin/ksh 
#
# Author:        Geoff Manikin       Org: NP22         Date: 2007-08-06
#
# Script history log:
# 2007-08-06  Geoff Manikin
# 2012-07-24  Jeff McQueen  cleaned up redundant codes
#    Created precip threshold loop for creating sref prob precip files
# 2012-09-26  JTM : Modified for wcoss
#                   Combined on & off cycles options into one script 
# 2012-10-22  JTM : Combined addprecip and makeprecip codes
# 2012-10-26  JTM : Combined various nam region scripts
#                   smartinit: getgrib.f: fixed bug with reading sref prob file
# 2012-10-31  JTM : moved smartinit system to tide
# 2012-12-03  JTM : Unified for nam parent and nested region runs
# 2014-01-23  Matthew Pyle : Overhaul for HiresWindow purposes
#======================================================================
#  Set Defaults fcst hours,cycle,model,region in config_nam_nwpara called in parent job

# RUNTYP: OUTPUT REGION TO DOWNSCALE TO 
#=================================================================
# conus        : Downscale NAM 12 over CONUS  
#              :  SREF-GRD=212  NAM-GRD=bgrd  NDFD-GRD=197
# pr           :  SREF-GRD=212  NAM-GRD=bgrd  NDFD-GRD=195
# hi           :  SREF-GRD=243  NAM-GRD=bgrd  NDFD-GRD=196
# ak_rtmages   :

# conusnest    :  SREF-GRD=212   NAM-GRD=conusnest.bsmart      NDFD-GRD=197
# conusnest2p5 :  SREF-GRD=212   NAM-GRID=conusnest.bsmart     NDFD-GRD=184
# priconest    :  SREF=GRD=212   NAM-GRD= prinest.bsmart       NDFD-GRD=195
# hawaiinest   :  SREF-GRID=243  NAM-GRID=hawaiinest.bsmart    NDFD-GRD=196  
# aknest3      :  SREF-GRID=216  NAM-GRID=alaskanest.bsmart    NDFD-GRD=91
#======================================================================

set -x 

# Check if this is a nest run
# inest=`echo $RUNTYP|awk '{ print( index($0,"nest") )}' `
# export rg=`echo $RUNTYP |cut -c1-2` 

echo RUNTYP $RUNTYP

inest=1  # treat hiresw domains like they are nests


export rg=$RUNTYP

#=====================================================================
# Set special filename extensions for mdl,sref,master,wgt,output files
# mdl input file         : mdlgrd,natgrd
# eg: nam.t12z.conusnest.bsmart.tm00

# prdgen master ctl file : RUNTYPE
# eg: nam_smartmasteraknest3.ctl     

# prdgen wgt ctl file    : mdlgrd, ogrd
# eg: nam_wgt_184_conusnest

# smartinit in/out file  : rg / outreg
# eg: MESOAK.NDFD
# eg: nam.t12z.smartconus2p5.f03
#=====================================================================

#INPUT MODEL GRID filename extension
export mdlgrd=$RUNTYP  
export mdl=$RUNTYP

#SMARTINIT OUTPUT grid filename extension
outreg=$rg
case $RUNTYP in
  conusarw|conusnmmb) rg=conus; outreg=conus;;
  hiarw|hinmmb) rg=hi; outreg=hi;;
  prarw|prnmmb) rg=pr; outreg=pr;;
  akarw|aknmmb) rg=ak; outreg=ak;;
  guamarw|guamnmmb) rg=guam; outreg=guam;;
esac

cycon=0

# case $cyc in 
#    00|12 ) cycon=1;; 
# esac

# Set previous cyc and forecast hour to use prev. cycle 6 hr forecast for 00 analysis
# in case of nam coldstart (08/2013)

# typeset -Z2 pcfhr pcyc
# let pcfhr=06          #prev. cycle forecast hour
# let pcyc=$cyc-$pcfhr  #previous cycle
# pcdate=$PDY
# if [ $pcyc -lt 00 ];then
#   let pcyc=24-$pcyc
#   let pcdate=$PDYm1
# fi

# 12 hour max/mins must be computed at 00 and 12 UTC

# FOR NESTS,parent script, exnam, sets forecast range (60 or 54h)
case $cyc in
  00|12) set -A A6HR 12 24 36 48 999;;
  * )    set -A A6HR 18 30 42 999;;
esac

# srefcyc and gefscyc set in parent job (JNAM_SMINIT)
typeset -Z2 srefcyc gefscyc pcphrl


#======================================================================
#  Configure input met grib, land-sea mask and topo file names
#======================================================================

#  Set indices to determine input met file name 
#       eg: MDL.tCYCz.MDLGRD.NATGRD${FHR}.tm00
#       eg: nam.t12z.bgrd3d24.tm00
#       eg: nam.t12z.conusnest.bsmart24.tm00

# prdgfl=meso${rg}.NDFD  # output prdgen grid name (eg: mesocon.NDFD,mesoak...)

if [ $rg = "hi" ]
then
rgprdgen=HI
elif [ $rg = "pr" ]
then
rgprdgen=PR
elif [ $rg = "conus" ]
then
rgprdgen=CONUS
elif [ $rg = "ak" ]
then
rgprdgen=AK
elif [ $rg = "guam" ]
then
rgprdgen=GU
fi
echo set rgprdgen to $rgprdgen

if [ $MODEL = "nmmb" ]
then
prdgfl=wrf.${rgprdgen}04
elif [ $MODEL = "arw" ]
then
prdgfl=wrf.EM${rgprdgen}04
fi

echo prdgfl $prdgfl

# if [ $inest -eq 0 ];then 
#   natgrd="bgrd3d"
#   if [ $rg = con ];then prdgfl=meso.NDFD;fi
# else

  natgrd="wrfprs"       # native model type grid extension (eg: bgrd3d, bsmart)

#  if [ $RUNTYP = aknest3 ];then prdgfl=mesoak.NDFD;fi
# fi
#-------------------------------------------------------------------------
#   For all grids, set the following:
#   sgrb : Input SREF grid grib number (eg: 212, 216, 243)
#   grid : output grid to copygb sref precip and nam precip buckets to 
#          one exception for non-nests where nam precip buckets are 
#          interpolated to smartinit output (ogrd)
#   ogrd : output grib number for prdgen and smartinit codes 
#          (eg: 197,196,195,184)
#--------------------------------------------------------------------------
grdext=" 0 64 25000 25000"

# case $RUNTYP in 
#   conus|conusnest)
#     maskpre=ruc2_vegtype_ndfd
#     topopre=ruc2_ndfdtopo
#     ext=dat
#     grid="255 3 1073 689 20192 238446 8 265000 5079 5079 $grdext"
#     sgrb=212       # SREF input GRIB File grid indicator
#     ogrd=197;;     # smartinit output grid number
#   *) 

   maskpre=hiresw_smartmask${rg}
   topopre=hiresw_smarttopo${rg}
   ext=grb
   case $RUNTYP in
     guamnmmb|guamarw)          sgrb=999;ogrd=199
      grid="255 1 193 193 12350 143687 128 16794 148280 20000  $grdext";;
     hiarw)            sgrb=243;ogrd=196
      grid="255 1 321 225 18067 -161626 128 23082 -153969 20000 $grdext";;
     hinmmb)            sgrb=243;ogrd=196
      grid="255 1 321 225 18067 -161626 128 23082 -153969 20000 $grdext";;
     prarw)            sgrb=212;ogrd=195
      grid="255 1 177 129 16829  -68196 128 19747  -63972 20000 $grdext";;
     prnmmb)            sgrb=212;ogrd=195
      grid="255 1 177 129 16829  -68196 128 19747  -63972 20000 $grdext";;
     akarw)  sgrb=216;ogrd=91
      grid="255 5 1649 1105 40530 181429 8 210000 2976 2976 0 64 0 25000 25000";;
     aknmmb)  sgrb=216;ogrd=91
      grid="255 5 1649 1105 40530 181429 8 210000 2976 2976 0 64 0 25000 25000";;
     conusarw)  sgrb=212;ogrd=184 
      grid="255 3 2145 1377 20192 238446 8 265000 2540 2540 $grdext"
      topopre=ruc2_ndfd_elevtiles.ndfd2.5
      maskpre=ruc2_ndfd_vegtiles.ndfd2.5;;
     conusnmmb)  sgrb=212;ogrd=184 
      grid="255 3 2145 1377 20192 238446 8 265000 2540 2540 $grdext"
      topopre=ruc2_ndfd_elevtiles.ndfd2.5
      maskpre=ruc2_ndfd_vegtiles.ndfd2.5;;
#  NESTS--------------------------------------------------------------------
   *)
      echo RUNTYP  ${RUNTYP} configuration not available $mdlgrd $rg
      exit;;
   esac
# esac

maskfl=${maskpre}.${ext}
topofl=${topopre}.${ext}

echo
echo "============================================================"
echo BEGIN SMARTINIT PROCESSING FOR FFHR $ffhr  CYCLE $cyc
echo RUNTYP:  $RUNTYP $mdlgrd  $rg
echo INTERP GRID: $grid
echo OUTPUT GRID: $ogrd $outreg

datestr=`date`
echo starting at $datestr

echo "============================================================"
echo 

#  Set Defaults pcp hours and frequencies
let pcphr=ffhr+3
let pcphrl=ffhr+3
let pcphr12=pcphr-12
let pcphr6=pcphr-6
let pcphr3=pcphr-3

#======================================================================
#  CREATE SREF PROB. PRECIP FILES
#======================================================================

# fhr should be gt 0 since precip is not available at initial time
if [ $ffhr -gt 0 -a $sgrb -ne 999 ]; then

# get the sref precip fields that we need
  cp $COMIN_SREF/sref.t${srefcyc}z.pgrb${sgrb}.prob_3hrly SREFPROB

  if [ ! -s SREFPROB ]; then
    cp $COMIN_GEFS/${gefscyc}/sref.t${gefscyc}z.pgrb${sgrb}.prob_3hrly SREFPROB
  fi

  $utilexec/grbindex SREFPROB SREFPROBI
  # check for missing sref data
  export err=$?; err_chk

  let IP=0
  if [ $ffhr -lt 6 ]; then pcphr6=;pcphr12=;fi
  if [ $ffhr -lt 12 ]; then pcphr12=;fi
  grbpre="2 0 0 0 0"
  for PHR in $pcphr3 $pcphr6 $pcphr12;do 

#   prob of pcp > 0.01
    $utilexec/wgrib -PDS10 SREFPROB |grep "${grbpre} 64 64 0 0"|grep "0 1 $PHR $pcphr 4"|$utilexec/wgrib -i -grib -o dump SREFPROB

    let IP=IP+1
    mv dump srefpcp$IP

#   prob of pcp > 0.05
    $utilexec/wgrib -PDS10 SREFPROB |grep "${grbpre} 65 20 81 236"| grep "0 1 $PHR $pcphr 4"|$utilexec/wgrib -i -grib -o dump SREFPROB
    let IP=IP+1
    mv dump srefpcp$IP

#   prob of pcp > 0.10
    $utilexec/wgrib -PDS10 SREFPROB |grep "${grbpre} 65 40 163 215"| grep "0 1 $PHR $pcphr 4"|$utilexec/wgrib -i -grib -o dump SREFPROB
    let IP=IP+1
    mv dump srefpcp$IP

#   prob of pcp > 0.25
    $utilexec/wgrib -PDS10 SREFPROB |grep "${grbpre} 65 101 153 154"| grep "0 1 $PHR $pcphr 4"|$utilexec/wgrib -i -grib -o dump SREFPROB
    let IP=IP+1
    mv dump srefpcp$IP

#   prob of pcp > 0.50
    $utilexec/wgrib -PDS10 SREFPROB |grep "${grbpre} 65 203 51 51"| grep "0 1 $PHR $pcphr 4"|$utilexec/wgrib -i -grib -o dump SREFPROB
    let IP=IP+1
    mv dump srefpcp$IP
  done

  cat srefpcp1 srefpcp2 srefpcp3 srefpcp4 srefpcp5 > srefallpcp
  if [ $ffhr -ge 6 ]; then
    cat srefpcp6 srefpcp7 srefpcp8 srefpcp9 srefpcp10 >> srefallpcp
  fi
  if [ $ffhr -ge 12 ]; then
    cat srefpcp11 srefpcp12 srefpcp13 srefpcp14 srefpcp15 >> srefallpcp
  fi

  $utilexec/copygb -g "$grid" -x srefallpcp srefpcp${rg}_${SREF_PDY}${srefcyc}f0${pcphrl}
  $utilexec/grbindex srefpcp${rg}_${SREF_PDY}${srefcyc}f0${pcphrl} srefpcp${rg}i_${SREF_PDY}${srefcyc}f0${pcphrl}

fi #fhr -ge 0

let ffhr1=ffhr-1
let ffhr2=ffhr-2
hours="${ffhr}"
if [ $ffhr -ge 3 ];then hours="${ffhr2} ${ffhr1} ${ffhr}";fi


datestr=`date`
echo PAST WGRIB of SREF, START PREC BUCKETS $datestr

#===========================================================
#  CREATE Accum precip buckets if necessary 
#===========================================================
for fhr in $hours; do

datestr=`date`
echo TOP OF LOOP for $fhr AT $datestr

  rm -f *out${fhr}
  mk3p=0;mk6p=0;mk12p=0
  let check=fhr%3
  let check6=fhr%6
  let fhr1=fhr-1
  let fhr2=fhr-2
  let fhr3=fhr-3
  let fhr6=fhr-6
  let fhr9=fhr-9
  let fhr12=fhr-12
  typeset -Z2 fhr1 fhr2 fhr3 fhr6 fhr9 fhr12 fhr ffhr

# Reduce the input model file size for prdgen on wcoss 32 bit limited machines
#  if [ $natgrd = "bgrd3d" ];then

# Check that NAM 00 hr analysis is from NDAS or GDAS (08/2013)
#    if [ $fhr -eq 00 -a $GUESS = GDAS ];then
#      echo;echo "WARNING  GUESS = " $GUESS INDICATES $mdl COLD START
#      echo USING PREVIOUS $pcdate $ ${cyc}Z CYCLE $mdl $pcfhr FORECAST;echo
#      mdlin=${COM_IN}/${mdl}.${pcdate}/${mdl}.t${pcyc}z.${natgrd}${pcfhr}.tm00
#      ln -fs ${mdlin} fort.11
#      ln -fs WRFPRS${fhr}.tm00 fort.51
#      echo ${PDY}${cyc} | ${utilexec}/overdate.grib
#    else

      cp $COMIN/$mdl.t${cyc}z.${natgrd}${fhr} WRFPRS${fhr}.tm00

#    fi
    ${utilexec}/wgrib -s WRFPRS${fhr}.tm00 | \
    grep -f ${PARMhiresw}/hiresw_smartinit.parmlist | \
    ${utilexec}/wgrib -i -grib -o temp WRFPRS${fhr}.tm00 > wgrib.out
    mv temp WRFPRS${fhr}.tm00

#  else
#    cp $COMIN/nam.t${cyc}z.${mdlgrd}${natgrd}${fhr}.tm00 WRFPRS${fhr}.tm00
#  fi

  $utilexec/grbindex WRFPRS${fhr}.tm00 WRFPRS${fhr}i.tm00

#   if [ $inest -eq 0 ];then
#     cp -p $PARMnam/nam_master${outreg}.ctl master${fhr}.ctl
#   else
#    case $rg in
#      ak|hi|pr) cp -p $PARMhiresw/${mdl}_master${outreg}.ctl master${fhr}.ctl;;
#       con|ak3) cp -p $PARMhiresw/${mdl}_smartmaster${RUNTYP}.ctl master${fhr}.ctl;;
#    esac

cp $PARMhiresw/hiresw_${MODEL}_mastersmart.${RUNTYP}.ctl  master${fhr}.ctl

#  fi

  if [ $fhr -gt 0 ];then
#-------------------------------------------------------------
#   OFF-CYC & Nests: Create 6/12 hour buckets, 3 hr buckets available
#   ON-CYC :
#     3hr precip available at only 3,15, 27,39... forcast fhours
#     other hours, create 3 hr precip
#     6hr precip: Create only at 00/12 UTC valid times 
#           eg: fhr=12,24,36
#     Create 12 hour precip at 00/12 UTC valid times
#-------------------------------------------------------------
	echo check $check
	echo MODEL $MODEL
    if [ $check -eq 0 -a $MODEL = "arw" ]
    then
	echo MADE to mk3p definition
      mk3p=3
      ppgm=make
    else
      mk3p=0
    fi
    if [ $check6 -eq 0 ];then 
      mk6p=6
	if [ MODEL = "nmmb" ] 
        then 
      ppgm=add
        else
      ppgm=make
        fi
    fi

#   hr3bkt flag determines when to run smartprecip to create 3 hr buckets
    let hr3bkt=$((fhr-3))%12

	echo hr3bkt is $hr3bkt

#-------------------------------------------------------------
#   ON-CYC: The 3-hr fhrs (3,15,27,39....) --> Already have 3-hr buckets
#   For in-between fhrs (22,23,25) --> Create 3-hr buckets
#   since we only gather max/min data at those hours to compute 12 hr max/mins
#-------------------------------------------------------------
#     if [ $cycon -eq 1 -a $inest -eq 0 ];then
#      if [ $hr3bkt -ne 0 -a $check -eq 0 ];then
#        mk3p=3
#        ppgm=make
#      fi
#    else

	if [ $MODEL = "nmmb" ]
        then
      hr3bkt=0
        fi

#    fi #cycon check
  fi  #fhr -ne 0

#-------------------------------------------------------------
# ON-CYCLE:  At 12-hr times:  Need 6 hour buckets as well
# Except for 6 hr times (18,30,42...) : Already have 6 hour buckets
# In addition, For 00/12 UTC valid times: Need to make 12 hour accumulations
#-------------------------------------------------------------
  case $fhr in 
    ${A6HR[0]}|${A6HR[1]}|${A6HR[2]}|${A6HR[3]}|${A6HR[4]}|${A6HR[5]}|${A6HR[6]} )
#    if [ $cycon -eq 1 -a inest -eq 0 ];then
#      mk6p=6
#      ppgm=make
#    else
#     off-cycles and  Nests have 3 hr buckets but need 6,12 hour precip
	echo "make both 6 and 12 hour buckets"
      mk6p=6
      mk12p=12

	if [ $MODEL = "nmmb" ]
        then
      ppgm=add
        else
      ppgm=make
        fi

      mk12p=12;;

#    fi;;
  esac 
datestr=`date`
echo TO SMARTPRECIP $datestr

  echo MKPCP Flags: MK3P $mk3p   MK6P $mk6p   MK12P $mk12p
  for MKPCP in $mk3p $mk6p $mk12p;do
    if [ $MKPCP -ne 0 ];then
    echo ====================================================================
    echo BEGIN Making $MKPCP hr PRECIP Buckets for $fhr Hour $ppgm freq $freq
    echo ====================================================================
      pfhr3=-99;pfhr4=-99

      case $MKPCP in
        $mk3p )
        FHRFRQ=$fhr3;freq=3
        pfhr1=$fhr;pfhr2=$fhr3;;

        $mk6p )
        FHRFRQ=$fhr6;freq=6
        echo fhr $fhr
        echo fhr6 $fhr6
        pfhr1=$fhr;pfhr2=$fhr6
        if [ $ppgm = add ];then 
          FHRFRQ=$fhr3
          pfhr1=$fhr3;pfhr2=$fhr
	echo add so pfhr1 is $pfhr1
	echo add so pfhr2 is $pfhr2
        fi;;

        $mk12p )
	if [ $MODEL = "nmmb" ]
        then
        FHRFRQ=$fhr9;freq=12
        else
        FHRFRQ=$fhr12;freq=12
        pfhr1=$fhr;pfhr2=$fhr12
        fi

        if [ $ppgm = add ];then 
        pfhr1=$fhr9;pfhr2=$fhr6;pfhr3=$fhr3;pfhr4=$fhr
        fi;;

      esac


   
#      if [ $natgrd = "bgrd3d" ];then 
        cp $COMIN/${mdl}.t${cyc}z.${natgrd}${FHRFRQ} WRFPRS${FHRFRQ}.tm00
        ${utilexec}/wgrib -s WRFPRS${FHRFRQ}.tm00 |grep -f ${PARMhiresw}/hiresw_smartinit.parmlist \
                        |${utilexec}/wgrib -i -grib -o temp WRFPRS${FHRFRQ}.tm00 > wgrib.out
        mv temp WRFPRS${FHRFRQ}.tm00
#      else
#        cp $COMIN/nam.t${cyc}z.${mdlgrd}${natgrd}${FHRFRQ}.tm00 WRFPRS${FHRFRQ}.tm00
#      fi

      $utilexec/grbindex WRFPRS${fhr}.tm00 WRFPRS${fhr}i.tm00
      $utilexec/grbindex WRFPRS${FHRFRQ}.tm00 WRFPRS${FHRFRQ}i.tm00

      export pgm=nam_smartprecip; . prep_step
      ln -sf "WRFPRS${FHRFRQ}.tm00"  fort.13  
      ln -sf "WRFPRS${FHRFRQ}i.tm00" fort.14
      ln -sf "WRFPRS${fhr}.tm00"     fort.15
      ln -sf "WRFPRS${fhr}i.tm00"    fort.16
      ln -sf "${freq}precip.${fhr}"  fort.50
#     ln -sf "${freq}cprecip.${fhr}" fort.51
      ln -sf "${freq}snow.${fhr}"    fort.52

      if [ $MKPCP -eq $mk12p ];then


	if [ $MODEL = "nmmb" ]
	then

          cp $COMIN/${mdl}.t${cyc}z.${natgrd}${fhr3} WRFPRS${fhr3}.tm00
          ${utilexec}/wgrib -s WRFPRS${fhr3}.tm00 |grep -f ${PARMhiresw}/hiresw_smartinit.parmlist \
                          |${utilexec}/wgrib -i -grib -o temp WRFPRS${fhr3}.tm00 > wgrib.out
          mv temp WRFPRS${fhr3}.tm00
        $utilexec/grbindex WRFPRS${fhr3}.tm00 WRFPRS${fhr3}i.tm00

          cp $COMIN/${mdl}.t${cyc}z.${natgrd}${fhr6} WRFPRS${fhr6}.tm00
          ${utilexec}/wgrib -s WRFPRS${fhr6}.tm00 |grep -f ${PARMhiresw}/hiresw_smartinit.parmlist \
                           |${utilexec}/wgrib -i -grib -o temp WRFPRS${fhr6}.tm00 > wgrib.out
          mv temp WRFPRS${fhr6}.tm00
        $utilexec/grbindex WRFPRS${fhr6}.tm00 WRFPRS${fhr6}i.tm00

        ln -sf "WRFPRS${fhr6}.tm00"      fort.15    
        ln -sf "WRFPRS${fhr6}i.tm00"     fort.16
        ln -sf "WRFPRS${fhr3}.tm00"      fort.17
        ln -sf "WRFPRS${fhr3}i.tm00"     fort.18
        ln -sf "WRFPRS${fhr}.tm00"       fort.19
        ln -sf "WRFPRS${fhr}i.tm00"      fort.20

	else

          cp $COMIN/${mdl}.t${cyc}z.${natgrd}${fhr3} WRFPRS${fhr3}.tm00
          ${utilexec}/wgrib -s WRFPRS${fhr3}.tm00 |grep -f ${PARMhiresw}/hiresw_smartinit.parmlist \
                          |${utilexec}/wgrib -i -grib -o temp WRFPRS${fhr3}.tm00 > wgrib.out
          mv temp WRFPRS${fhr3}.tm00
        $utilexec/grbindex WRFPRS${fhr3}.tm00 WRFPRS${fhr3}i.tm00

          cp $COMIN/${mdl}.t${cyc}z.${natgrd}${fhr6} WRFPRS${fhr6}.tm00
          ${utilexec}/wgrib -s WRFPRS${fhr6}.tm00 |grep -f ${PARMhiresw}/hiresw_smartinit.parmlist \
                           |${utilexec}/wgrib -i -grib -o temp WRFPRS${fhr6}.tm00 > wgrib.out
          mv temp WRFPRS${fhr6}.tm00
        $utilexec/grbindex WRFPRS${fhr6}.tm00 WRFPRS${fhr6}i.tm00

        ln -sf "WRFPRS${fhr}.tm00"       fort.15
        ln -sf "WRFPRS${fhr}i.tm00"      fort.16

	fi

      fi  # mk12p

#===============================================================
# nam_smartprecip : Create Precip Buckets for smartinit 
#===============================================================
     echo MAKE $freq HR PRECIP BUCKET FILE using fhrs $pfhr1 $pfhr2 $pfhr3 $pfhr4


# IARW=1 means no special treatment for snow in smartprecip code

	if [ $MODEL = "arw" ]
	then
	IARW=1
	else
	IARW=1
	fi

	ls -l fort.1?
     $EXEChiresw/hiresw_smartprecip <<EOF > ${ppgm}precip${fhr}.out
$pfhr1 $pfhr2 $pfhr3 $pfhr4 $IARW
EOF

#	if [ ${fhr} -eq 18 ]
#	then
#	err=27
#	exit
#	fi
#
export err=$?; err_chk

#    Interp precip to smartinit GRID
     cpgbgrd=$grid
     if [ $inest -gt 0 ];then cpgbgrd=$ogrd;fi
     if [ $RUNTYP = aknest3 ];then cpgbgrd=$grid;fi
     $utilexec/copygb -g "$cpgbgrd" -i3 -x ${freq}precip.${fhr} ${freq}precip
     $utilexec/grbindex ${freq}precip ${freq}precipi
     $utilexec/copygb -g "$cpgbgrd" -i3 -x ${freq}snow.${fhr} ${freq}snow
     $utilexec/grbindex ${freq}snow ${freq}snowi
    fi #MKPCP>0
  done #MKPCP loop

datestr=`date`
echo PAST SMARTPRECIP $datestr

# exit


#=================================================================
#  RUN PRODUCT GENERATOR
#=================================================================

  $utilexec/grbindex WRFPRS${fhr}.tm00 WRFPRS${fhr}i.tm00

  echo creating $prdgfl file for fhr $fhr
  cat >input${fhr}.prd <<EOF5
WRFPRS${fhr}.tm00
EOF5

  export pgm=hiresw_prdgen;. prep_step
  ln -sf $FIXhiresw/hiresw_wgt_${mdl}.g${ogrd}     fort.21

#   if [ $inest -gt 0 ];then
#     case $rg in
#       ak3|con|pr) ln -sf $FIXnam/nam_wgt_${ogrd}_${mdlgrd} fort.21;;
#            ak|hi) ln -sf $FIXnam/nam_wgt_${mdlgrd}         fort.21;;
#     esac
#  fi

  ln -sf master${fhr}.ctl            fort.10
  ln -sf input${fhr}.prd             fort.621   #WCOSS CHANGE
 
  ${EXEChiresw}/hiresw_prdgen < input${fhr}.prd > prdgen.out${fhr}
  export err=$?; err_chk

  cp ${COMROOT}/date/t${cyc}z DATE

  # JY - the following three lines are for canned data test, can be deleted later
  curDd=`date +%Y%m%d`
  sed "s/$curDd/$PDY/" DATE > ./tmp-date
  mv ./tmp-date DATE

ls -l  $prdgfl

  if [ -s $prdgfl ];then  
    mv ${prdgfl} meso${rg}.NDFDf${fhr}  
    echo $prdgfl FOUND FOR FORECAST HOUR ${fhr}
  elif [ -s ${prdgfl}${fhr} ];then    # check for hawaii ???
    mv ${prdgfl}${fhr} meso${rg}.NDFDf${fhr}  
    echo $prdgfl${fhr} FOUND FOR FORECAST HOUR ${fhr}
  else
    echo $prdgfl NOT FOUND FOR FORECAST HOUR ${fhr}
    exit
  fi
  $utilexec/grbindex meso${rg}.NDFDf${fhr} meso${rg}.NDFDif${fhr}

datestr=`date`
echo PAST PRODUCT GENERATOR $datestr

#=================================================================
#   DECLARE INPUTS and RUN SMARTINIT 
#=================================================================

# CHANGE : for non-conus look in FIXnam for topo,land files
  cp $FIXhiresw/${topofl} TOPONDFD
  cp $FIXhiresw/${maskfl} LANDNDFD
  ln -sf TOPONDFD     fort.46
  ln -sf LANDNDFD     fort.48
  if [ $ext = grb ];then
    $utilexec/grbindex TOPONDFD TOPONDFDi
    $utilexec/grbindex LANDNDFD LANDNDFDi
    ln -sf TOPONDFDi  fort.47
    ln -sf LANDNDFDi  fort.49
  fi

  mksmart=1
  if [ $check -eq 0 -a $fhr -ne 00 ];then 

	if [ $sgrb -ne 999 ] ; then
    cp srefpcp${rg}_${SREF_PDY}${srefcyc}f0${pcphrl} SREFPCP
    cp srefpcp${rg}i_${SREF_PDY}${srefcyc}f0${pcphrl} SREFPCPi
        fi
    cp MAXMIN${fhr2}.tm00 MAXMIN2
    cp MAXMIN${fhr1}.tm00 MAXMIN1
    $utilexec/grbindex MAXMIN1 MAXMIN1i
    $utilexec/grbindex MAXMIN2 MAXMIN2i
  fi

  freq=6;fmx=21   #fmx =  maxmin unit number for 1st maxmin file

### if arw, should freq be 3 here?

#  if [ $cycon -eq 1 ];then 
#    if [ $inest -eq 0 ];then 
#      freq=3;fmx=23
#    fi
#  else
#    fmx=19   
#  fi

	if [ $MODEL = "arw" -a $check -eq 0 ]
	then
	cat 3precip meso${rg}.NDFDf${fhr} > new_meso${rg}.NDFDf${fhr}
	mv new_meso${rg}.NDFDf${fhr} meso${rg}.NDFDf${fhr}
        $utilexec/grbindex meso${rg}.NDFDf${fhr} meso${rg}.NDFDif${fhr}
        fi

  ln -sf "meso${rg}.NDFDf${fhr}"    fort.11
  ln -sf "meso${rg}.NDFDif${fhr}"   fort.12

	ls -l meso${rg}.NDFDf${fhr} meso${rg}.NDFDif${fhr}

	if [ $sgrb -ne 999 ]; then
  ln -sf "SREFPCP"                  fort.13
  ln -sf "SREFPCPi"                 fort.14
	fi
  ln -sf "${freq}precip"            fort.15
  ln -sf "${freq}precipi"           fort.16

# At 12-hr times, input 12-hr max/min temps and 3 and 6-hr buckets
  case $fhr in 
    ${A6HR[0]}|${A6HR[1]}|${A6HR[2]}|${A6HR[3]}|${A6HR[4]}|${A6HR[5]}|${A6HR[6]} )
    echo "********************************************************"
    echo RUN SMARTINIT for 12h valid 00 or 12Z fcst hours: $fhr

	fmx=21

#    if [ $cycon -eq 0 ];then fmx=21;fi
    cp $COMOUT/${mdl}.t${cyc}z.smart${outreg}f${fhr3} MAXMIN3
    cp $COMOUT/${mdl}.t${cyc}z.smart${outreg}f${fhr6} MAXMIN4
    cp $COMOUT/${mdl}.t${cyc}z.smart${outreg}f${fhr9} MAXMIN5
    $utilexec/grbindex MAXMIN3 MAXMIN3i
    $utilexec/grbindex MAXMIN4 MAXMIN4i
    $utilexec/grbindex MAXMIN5 MAXMIN5i


###  revisit this....we might need 3 precip for ARW

#    if [ $cycon -eq 1 -a inest -eq 0 ];then
#     READ 3/6 hr precip from special files created by makeprecip
#      ln -sf "6precip"   fort.17
#      ln -sf "6precipi"  fort.18
#      ln -sf "3snow"     fort.19
#      ln -sf "3snowi"    fort.20
#      ln -sf "6snow"     fort.21
#      ln -sf "6snowi"    fort.22
#    else     
#     READ 6/12 hr precip from special files created by makeprecip
      ln -sf "6snow"      fort.17
      ln -sf "6snowi"     fort.18
      ln -sf "12precip"   fort.19
      ln -sf "12precipi"  fort.20
#    fi   
    ln -sf "MAXMIN1"   fort.$fmx
    ln -sf "MAXMIN2"   fort.$((fmx+1))
    ln -sf "MAXMIN3"   fort.$((fmx+2))
    ln -sf "MAXMIN4"   fort.$((fmx+3))
    ln -sf "MAXMIN5"   fort.$((fmx+4))
    ln -sf "MAXMIN1i"  fort.$((fmx+5))
    ln -sf "MAXMIN2i"  fort.$((fmx+6))
    ln -sf "MAXMIN3i"  fort.$((fmx+7))
    ln -sf "MAXMIN4i"  fort.$((fmx+8))
    ln -sf "MAXMIN5i"  fort.$((fmx+9));;

    *)   # Not 00/12 UTC valid times

     if [ $check -eq 0 -a $fhr -ne 0 ];then

#      READ PRECIP FROM SPECIAL FILES CREATED BY SMARTPRECIP
#      ON-CYC: All forecast hours divisible by 3 except for (3,15,27....), 
#      read  3-hr buckets max/min temp data for the previous 2 hours
#      OFF-CYC: Set input files to read 6 hr prcp from makeprecip files

#mptest       if [ $hr3bkt -ne 0 -o $mk6p -ne 0 ];then
       if [ $mk6p -ne 0 ];then
         echo "****************************************************************"

         case $cycon in
          1) echo RUN SMARTINIT for ON-CYC  hrs without 3 hr buckets : $fhr;;
          *) echo RUN SMARTINIT for OFF-CYC hrs without 6 hr buckets : $fhr;;
         esac

 echo "here with freq " $freq

         ln -fs "${freq}snow"  fort.17
         ln -sf "${freq}snowi" fort.18
         ln -sf "MAXMIN2"   fort.19
         ln -sf "MAXMIN1"   fort.20
         ln -sf "MAXMIN2i"  fort.21
         ln -sf "MAXMIN1i"  fort.22

       else           
#        READ PRECIP FROM INPUT NAM GRIB FILE 
#        ON-CYC:  Forecast hours 3,15,27,39....already  have 3-hr buckets,
#        OFF-CYC: 3 hour buckets available for all 3 hour forecast times
#        ALL-CYC: Input only  max/min temp data for the previous 2 hours

         echo "****************************************************"
         echo RUN SMARTINIT for hours with 3 hr buckets: $fhr
         ln -sf "MAXMIN2"   fort.15
         ln -sf "MAXMIN1"   fort.16
         ln -sf "MAXMIN2i"  fort.17
         ln -sf "MAXMIN1i"  fort.18
       fi  

     else   # fhr%3 -ne 0
#    For all "in-between" forecast hours (13,14,16....)
#    No special data needed
       echo "*****************************************************"
       echo RUN SMARTINIT for in-between hour: $fhr
       ln -fs " " fort.13
       ln -fs " " fort.14
       ln -fs " " fort.15

       ln -fs " " fort.16
       mksmart=0
       if [ $fhr -eq 00 ];then mksmart=1;fi
     fi;;
  esac

#========================================================
# Run Smartinit
#========================================================
  case $RUNTYP in
   conus|conusnest) RGIN=CS;;
      conusnest2p5) RGIN=CS2P;;
        ak_rtmages) RGIN=AKRT;;
                 *) RGIN=`echo $rg |tr '[a-z]'  '[A-Z]' `;;
  esac

	echo linked files into hiresw_smartinit

ls -l fort.??

	echo end linked files into hiresw_smartinit

datestr=`date`
echo TO SMARTINIT RUN $datestr

	echo RGIN into smartinit $RGIN
  export pgm=nam_smartinit;. prep_step
  $EXEChiresw/hiresw_smartinit $cyc $fhr $ogrd $RGIN $inest $MODEL >smartinit.out${fhr}
  export err=$?; err_chk
datestr=`date`
echo PAST SMARTINIT RUN $datestr

# Save smartinit output for RTMA 1st guess for AK, HI(nest), PR(nest) 03-13-13
# But do not perform nco post-processing on in between hours for these downscaled nests except fhr=00
  if [ $fhr -le 9 ];then
    case $RUNTYP in
     ak_rtmages) mksmart=0
       mv MESO${RGIN}${fhr}.tm00  $COMOUT/${mdl}.t${cyc}z.smart${RUNTYP}${fhr}.tm00;;
     hawaiinest|priconest)
       cp MESO${RGIN}${fhr}.tm00  $COMOUT/${mdl}.t${cyc}z.smart${rg}${fhr}.tm00;;
   esac
  fi

  if [ $mksmart -eq 1 ];then
# Run NCO processing to convert output to grib2 and awips
   export RUNTYP
   export RGIN=$RGIN  # Region id (eg: CS, HI, PR,AK..)
   export outreg
   export cyc  
   export fhr=$fhr
   export ogrd 
   ${USHhiresw}/hiresw_ncoproc.sh
datestr=`date`
echo PAST NCOPROC $datestr
echo "- - - - - - - - - - - - - - - - -     " $datestr

else

datestr=`date`
echo "- - - - - INTERMEDIATE TIME FINISH - - - - - - - - - - -     " $datestr


  fi
  echo
done  #fhr loop

exit
