#!/bin/ksh

################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         nam_prdgen.sh
# Script description:  Run 00-12 h nam product generator jobs
#
# Author:        Eric Rogers       Org: NP22         Date: 1999-06-23
#
# Abstract: This script runs the 00-12 h Nam PRDGEN jobs
#
# Script history log:
# 1999-06-23  Eric Rogers
# 1999-08-25  Brent Gordon  Pulled the PRDGEN here file out of the post
#                           jobs into this script.
# 2006-05-17  Eric Rogers   Modified script to use ops NAM input files
#
                                                                                           
set -x

export fhr=$1
export PS4='PRDGEN${fhr}_T$SECONDS + '

cd $DATA/$fhr

# Run the util setup script:
/nwprod/util/ush/setup.sh

cp $PARMhiresw/hiresw_master.${NEST}.ctl master${fhr}.ctl

cat >input${fhr}.prd <<EOF5
$COMNAM/nam.${cycle}.egdawp${fhr}.tm00
EOF5

rm fort.*

export pgm=hiresw_prdgen_hls;. prep_step
export XLFUNIT_10="master${fhr}.ctl"
export XLFUNIT_21="$FIXhiresw/hiresw_nam12_wgt_${NEST}_reg"
$EXEChiresw/hiresw_prdgen_hls < input${fhr}.prd > prdgen.out${fhr}
export err=$?;err_chk

if [ $cycle = t00z -o $cycle = t12z ] ; then
  if [ $fhr -ne 00 -a $fhr -ne 01 -a $fhr -ne 13 ] ; then
   let fhr1=fhr-1
   typeset -Z2 fhr1
   # Check for the availability of the previous hours
   icnt=1
   while [ $icnt -lt 100 ]
   do
      if [ -s $COMOUT/${NEST}.${cycle}.awpreg${fhr1}.tm00 ]
      then
         break
      else
         sleep 10
         icnt=$((icnt + 1))
      fi
   done
   cp $COMOUT/${NEST}.${cycle}.awpreg${fhr1}.tm00 ${fhr}.${NEST}.AWPREG${fhr1}
   cp $COMOUT/${NEST}.${cycle}.awpregi${fhr1}.tm00 ${fhr}.AWPREGi${fhr1}
   cp meso.AWPREG${fhr}.tm00 ${fhr}.${NEST}.AWPREG${fhr}
   $utilexec/grbindex ${fhr}.${NEST}.AWPREG${fhr} AWPREGi${fhr}
   cp AWPREGi${fhr} ${fhr}.AWPREGi${fhr}

   export XLFUNIT_13="${fhr}.${NEST}.AWPREG${fhr1}"
   export XLFUNIT_14="${fhr}.AWPREGi${fhr1}"
   export XLFUNIT_15="${fhr}.${NEST}.AWPREG${fhr}"
   export XLFUNIT_16="${fhr}.AWPREGi${fhr}"
   export XLFUNIT_50="3preciphls.${fhr}"
   export XLFUNIT_51="3cpreciphls.${fhr}"
   $EXEChiresw/hiresw_makeprecip_hls <<EOF>pgmout.${fhr}
$fhr $fhr1
EOF
    cat 3preciphls.${fhr} >> meso.AWPREG${fhr}.tm00
    cat 3cpreciphls.${fhr} >> meso.AWPREG${fhr}.tm00
  fi

else

  if [ $fhr -ne 00 -a $fhr -ne 01 -a $fhr -ne 04 -a $fhr -ne 07 -a \
       $fhr -ne 10 -a $fhr -ne 13 -a $fhr -ne 16 ] ; then
   let fhr1=fhr-1
   typeset -Z2 fhr1

   # Check for the availability of the previous hours
   icnt=1
   while [ $icnt -lt 100 ]
   do
      if [ -s $COMOUT/${NEST}.${cycle}.awpreg${fhr1}.tm00 ]
      then
         break
      else
         sleep 10
         icnt=$((icnt + 1))
      fi
   done

   cp $COMOUT/${NEST}.${cycle}.awpreg${fhr1}.tm00 ${fhr}.${NEST}.AWPREG${fhr1}
   cp $COMOUT/${NEST}.${cycle}.awpregi${fhr1}.tm00 ${fhr}.AWPREGi${fhr1}
   cp meso.AWPREG${fhr}.tm00 ${fhr}.${NEST}.AWPREG${fhr}
   $utilexec/grbindex ${fhr}.${NEST}.AWPREG${fhr} AWPREGi${fhr}
   cp AWPREGi${fhr} ${fhr}.AWPREGi${fhr}

   export XLFUNIT_13="${fhr}.${NEST}.AWPREG${fhr1}"
   export XLFUNIT_14="${fhr}.AWPREGi${fhr1}"
   export XLFUNIT_15="${fhr}.${NEST}.AWPREG${fhr}"
   export XLFUNIT_16="${fhr}.AWPREGi${fhr}"
   export XLFUNIT_50="3preciphls.${fhr}"
   export XLFUNIT_51="3cpreciphls.${fhr}"
   $EXEChiresw/hiresw_makeprecip_hls <<EOF>pgmout.${fhr}
$fhr $fhr1
EOF
    cat 3preciphls.${fhr} >> meso.AWPREG${fhr}.tm00
    cat 3cpreciphls.${fhr} >> meso.AWPREG${fhr}.tm00
  fi
fi

if test $SENDCOM = 'YES'
then
    $utilexec/grbindex meso.AWPREG${fhr}.tm00 AWPREGi${fhr}.tm00
    cp AWPREGi${fhr}.tm00 $COMOUT/${RUN}.${cycle}.awpregi${fhr}.tm00
    cp meso.AWPREG${fhr}.tm00 $COMOUT/${RUN}.${cycle}.awpreg${fhr}.tm00

    if test $SENDDBN = 'YES'
    then
     $DBNROOT/bin/dbn_alert MODEL NAM_${DBN_NEST}_AWIP $job $COMOUT/${RUN}.${cycle}.awpreg${fhr}.tm00
     $DBNROOT/bin/dbn_alert MODEL NAM_${DBN_NEST}_AWIPI $job $COMOUT/${RUN}.${cycle}.awpregi${fhr}.tm00
    fi
fi

if test "$SENDSMS" = 'YES'
then
  $SMSBIN/setev post_${fhr}_complete
fi

echo done >$FCST_DIR/postdone$fhr.tm00
echo EXITING $0
exit

