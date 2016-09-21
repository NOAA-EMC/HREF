#!/bin/ksh
#####################################################
# produce commom ensemble products (mean, spread and
# probability) of selected variables and write them
# out in grib1 format
#
#  Scirpt: prepare_ensprod.sh
# Purpose: run ensemble product generator to generate 
#          required ensemble products accoriding to variable.tbl 
#  Author: B. Zhou IMSG/EMC/NCEP
#          10/19/2011
#    Modification: B. Zhou IMSG/EMC/NCEP 3/21/2013  
#          Transfered to WCOSS as parallel runs
#          Run 12 fhr parallel runs with one poe 


set -x

export XLFRTEOPTS="namelist=old"

yy=`echo ${PDY} | cut -c 1-4`
mm=`echo ${PDY} | cut -c 5-6`
dd=`echo ${PDY} | cut -c 7-8`

ff=$fhr
cd $DATA/${ff}/

###############################

typeset -Z2 cycloc     #temp variable here
typeset -Z2 fcst    
typeset -Z2 m

if [ $cyc -ge 0 ] && [ $cyc -le 5 ] ; then

  files="9 namnest namnest namnest namnest namnest conusarw conusnmmb conusarw conusnmmb conusarw conusnmmb"
  set -A file  $files
  mbrs=" 6  7  8  9 10 11 1  2  3  4  5"

  if [ $cyc = '00' ] ; then
    days="9 $PDY $PDYm1 $PDYm1 $PDYm1 $PDYm1 $PDY $PDY $PDYm1 $PDYm1 $PDYm1 $PDYm1"
    cycs="9 00 18 12 06 00 00 00 12 12 00 00"
    ages="9  0  6 12 18 24  0  0 12 12 24 24"
  fi


  if [ $cyc = '03' ] ; then
    days="9 $PDY $PDYm1 $PDYm1 $PDYm1 $PDYm1 $PDY $PDY $PDYm1 $PDYm1 $PDYm1 $PDYm1"
    cycs="9 00 18 12 06 00 00 00 12 12 00 00"
    ages="9  3  9 15 21 27  3  3 15 15 27 27"
  fi

    set -A  day $days
    set -A  cycloc $cycs
    set -A  age $ages

elif [ $cyc -ge 6 ] ; then

  files="9 namnest namnest namnest namnest namnest namnest conusarw conusnmmb conusarw conusnmmb conusarw conusnmmb"
  set -A file  $files
  mbrs=" 7  8  9 10 11 12 1  2  3  4  5  6" 

  if [ $cyc = '06' ] ; then
     days="9 $PDY $PDY $PDYm1 $PDYm1 $PDYm1 $PDYm1 $PDY $PDY $PDYm1 $PDYm1 $PDYm1 $PDYm1"
     cycs="9 06 00 18 12 06 00 00 00 12 12 00 00"
     ages="9  0  6 12 18 24 30  6  6 18 18 30 30"
  fi

  if [ $cyc = '09' ] ; then
     days="9 $PDY $PDY $PDYm1 $PDYm1 $PDYm1 $PDYm1 $PDY $PDY $PDYm1 $PDYm1 $PDYm1 $PDYm1"
     cycs="9 06 00 18 12 06 00 00 00 12 12 00 00"
     ages="9  3  9 15 21 27 33  9  9 21 21 33 33"
  fi

  if [ $cyc = '12' ] ; then
     days="9 $PDY $PDY $PDY $PDYm1 $PDYm1 $PDYm1 $PDY $PDY $PDY $PDY $PDYm1 $PDYm1"
     cycs="9 12 06 00 18 12 06 12 12 00 00 12 12"
     ages="9  0  6 12 18 24 30  0  0 12 12 24 24"
  fi

  if [ $cyc = '15' ] ; then
     days="9 $PDY $PDY $PDY $PDYm1 $PDYm1 $PDYm1 $PDY $PDY $PDY $PDY $PDYm1 $PDYm1"
     cycs="9 12 06 00 18 12 06 12 12 00 00 12 12"
     ages="9  3  9 15 21 27 33  3  3 15 15 27 27"
  fi

  if [ $cyc = '18' ] ; then
     days="9 $PDY $PDY $PDY $PDY $PDYm1 $PDYm1 $PDY $PDY $PDY $PDY $PDYm1 $PDYm1"
     cycs="9 18 12 06 00 18 12 12 12 00 00 12 12"
     ages="9  0  6 12 18 24 30  6  6 18 18 30 30"
  fi

  if [ $cyc = '21' ] ; then
     days="9 $PDY $PDY $PDY $PDY $PDYm1 $PDYm1 $PDY $PDY $PDY $PDY $PDYm1 $PDYm1"
     cycs="9 18 12 06 00 18 12 12 12 00 00 12 12"
     ages="9  3  9 15 21 27 33  9  9 21 21 33 33"
  fi

     set -A  day $days
     set -A  cycloc $cycs
     set -A  age $ages

else

 echo $cyc ' is not a cycle'

fi

 nmbr=0
 for m in $mbrs ; do              
   fcst=` expr ${age[$m]} + $ff`
     weight=`echo "scale=2; 1-${age[$m]}/48" | bc`

      if [ $weight -lt 1.0 ] ; then
        weight='0'$weight
      fi

   if [ -s $DATA/href.m${m}.t${cyc}z.f$ff ] ; then
       nmbr=` expr $nmbr + 1`
       echo "   "$weight href.m${m}.t${cyc}z.f$ff "->" ${file[$m]}.t${cycloc[$m]}z.f${fcst} >> temp.f${ff}
       ln -sf $DATA/href.m${m}.t${cyc}z.f$ff .
       ln -sf $DATA/href.m${m}.t${cyc}z.f$ff prcip.m${m}.t${cyc}z.f$ff
   fi
 done

  echo $yy $mm $dd $cyc $ff "227 39" "36" "3" "12"  > filename    #first 36 is leadtime, second 12 is fcst times = leadtime/interval
  cat temp.f${ff} >> filename
  rm -f temp.f${ff}

ln -sf $PARMhref/href_variable_grib2.tbl variable.tbl

$EXEChref/href_ensprod > $DATA/$ff/output_ensprod.$ff 
export err=$?; err_chk;

if [ $SENDCOM = YES ]; then
 cp $DATA/$ff/href.mean.t${cyc}z.f$ff $COMOUT/href.t${cyc}z.mean.f$ff.grib2
 $WGRIB2 $COMOUT/href.t${cyc}z.mean.f$ff.grib2  -s >  $COMOUT/href.t${cyc}z.mean.f$ff.grib2.idx
 cp $DATA/$ff/href.prob.t${cyc}z.f$ff $COMOUT/href.t${cyc}z.prob.f$ff.grib2
 $WGRIB2 $COMOUT/href.t${cyc}z.prob.f$ff.grib2  -s >  $COMOUT/href.t${cyc}z.prob.f$ff.grib2.idx
 cp $DATA/$ff/href.sprd.t${cyc}z.f$ff $COMOUT/href.t${cyc}z.sprd.f$ff.grib2
 $WGRIB2 $COMOUT/href.t${cyc}z.sprd.f$ff.grib2  -s >  $COMOUT/href.t${cyc}z.sprd.f$ff.grib2.idx
fi
if [ $SENDDBN = YES ]; then
  $DBNROOT/bin/dbn_alert MODEL HREF_GB2 $job $COMOUT/href.t${cyc}z.mean.f$ff.grib2
  $DBNROOT/bin/dbn_alert MODEL HREF_GB2_IDX $job $COMOUT/href.t${cyc}z.mean.f$ff.grib2.idx
  $DBNROOT/bin/dbn_alert MODEL HREF_GB2 $job $COMOUT/href.t${cyc}z.prob.f$ff.grib2
  $DBNROOT/bin/dbn_alert MODEL HREF_GB2_IDX $job $COMOUT/href.t${cyc}z.prob.f$ff.grib2.idx
  $DBNROOT/bin/dbn_alert MODEL HREF_GB2 $job $COMOUT/href.t${cyc}z.sprd.f$ff.grib2
  $DBNROOT/bin/dbn_alert MODEL HREF_GB2_IDX $job $COMOUT/href.t${cyc}z.sprd.f$ff.grib2.idx
fi

exit
