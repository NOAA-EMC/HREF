#!/bin/ksh
#####################################################
# produce commom ensemble products (mean, spread and
#.${dom}.probability) of selected variables and write them
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

# export XLFRTEOPTS="namelist=old"

yy=`echo ${PDY} | cut -c 1-4`
mm=`echo ${PDY} | cut -c 5-6`
dd=`echo ${PDY} | cut -c 7-8`

ff=$fhr
dom=${NEST}

cd $DATA/${ff}/

ln -sf $FIXhref/new*rrfs* .

if [ $NEST = 'conusavoidnow' ]
# if [ $NEST = 'conus' ]
then

cp $COMINffg/${RUN}.t${cyc}z.ffg1h.3km.grib2 ./${RUN}.ffg1h.3km.grib2
err1=$?
cp $COMINffg/${RUN}.t${cyc}z.ffg3h.3km.grib2 ./${RUN}.ffg3h.3km.grib2
err2=$?
cp $COMINffg/${RUN}.t${cyc}z.ffg6h.3km.grib2 ./${RUN}.ffg6h.3km.grib2
err3=$?

if [ $cyc = '00' ]; then
 cycold='18'
 COMINffg=${COMINffgm1}
elif [ $cyc = '06' ]; then
 cycold='00'
elif [ $cyc = '12' ]; then
 cycold='06'
elif [ $cyc = '18' ]; then
 cycold='12'
fi

if [ $err1 -ne 0 ]
then
echo "WARNING: using previous cycle FFG1H file" $COMINffg/${RUN}.${cycold}z.ffg1h.3km.grib2
cp $COMINffg/${RUN}.${cycold}z.ffg1h.3km.grib2 ./${RUN}.ffg1h.3km.grib2
err=$? ; err_chk
fi

if [ $err2 -ne 0 ]
then
echo "WARNING: using previous cycle FFG3H file" $COMINffg/${RUN}.${cycold}z.ffg3h.3km.grib2
cp $COMINffg/${RUN}.${cycold}z.ffg3h.3km.grib2 ./${RUN}.ffg3h.3km.grib2
err=$? ; err_chk
fi

if [ $err3 -ne 0 ]
then
echo "WARNING: using previous cycle FFG6H file" $COMINffg/${RUN}.${cycold}z.ffg6h.3km.grib2
cp $COMINffg/${RUN}.${cycold}z.ffg6h.3km.grib2 ./${RUN}.ffg6h.3km.grib2
err=$? ; err_chk
fi


fi

###############################

typeset -Z2 cycloc     #temp variable here
typeset -Z2 fcst    
typeset -Z2 m

if [ $dom = 'conus' ]
  then
    files="9 fv3s fv3s fv3s fv3s fv3s fv3s fv3s fv3s fv3s"
elif [ $dom = 'hi' ]
  then
    files="9 hifv3s hifv3s hifv3s hifv3s hifv3s hifv3s hifv3s hifv3s hifv3s"
elif [ $dom = 'pr' ]
  then
    files="9 prfv3s prfv3s prfv3s prfv3s prfv3s prfv3s prfv3s prfv3s prfv3s"
elif [ $dom = 'ak' ]
   then
    files="9 akfv3s akfv3s akfv3s akfv3s akfv3s akfv3s akfv3s akfv3s akfv3s"
else
    echo "bad domain $dom"
    msg="FATAL ERROR: dom was not conus, hi, pr, or ak"
    err_exit $msg
fi

set -A file  $files
days="9 $PDY  $PDY  $PDY  $PDY  $PDY $PDY  $PDY $PDY  $PDY"
cycs="9 $cyc  $cyc  $cyc  $cyc  $cyc $cyc  $cyc $cyc  $cyc"
ages="9  0     0      0     0     0    0     0    0      0"
set -A  day  $days
set -A  cycloc $cycs
set -A  age  $ages
mbrs="1  2  3  4  5  6  7  8  9"

mbr=0

echo mbrs is $mbrs
 for m in $mbrs ; do              
   fcst=` expr ${age[$m]} + $ff`
     weight=`echo "scale=2; 1-${age[$m]}/48" | bc`

      if [ $weight -lt 1.0 ] ; then
        weight='0'$weight
      fi

   if [ -s $DATA/${RUN}.m${m}.t${cyc}z.f$ff ] ; then
       nmbr=` expr $nmbr + 1`
       echo "   "$weight ${RUN}.m${m}.t${cyc}z.f$ff "->" ${file[$m]}.t${cycloc[$m]}z.f${fcst} >> temp.f${ff}
       ln -sf $DATA/${RUN}.m${m}.t${cyc}z.f$ff .
   fi
 done

echo dom is $dom

  if [ $dom = 'conus' ]
  then
 echo $yy $mm $dd $cyc $ff "255 39" "36" "3" "12"  > filename    #first 36 is leadtime, second 12 is fcst times = leadtime/interval
  elif [ $dom = 'ak' ]
  then
 echo $yy $mm $dd $cyc $ff "999 39" "36" "3" "12"  > filename    #first 36 is leadtime, second 12 is fcst times = leadtime/interval
  elif [ $dom = 'hi' ]
  then
 echo $yy $mm $dd $cyc $ff "998 39" "36" "3" "12"  > filename    #first 36 is leadtime, second 12 is fcst times = leadtime/interval
  elif [ $dom = 'pr' ]
  then
 echo $yy $mm $dd $cyc $ff "997 39" "36" "3" "12"  > filename    #first 36 is leadtime, second 12 is fcst times = leadtime/interval
  fi
 cat temp.f${ff} >> filename
 rm -f temp.f${ff}

if [ $dom = 'conus' ] ; then

echo $dom is conus for defining the variable parm file

 if [ $ff -gt 0 ]
 then
  if [ ${ff}%3 -eq 0 ]
  then
    ln -sf $PARMhref/href_variable_grib2.tbl_3h variable.tbl
  else
    ln -sf $PARMhref/href_variable_grib2.tbl    variable.tbl
  fi
 else
  ln -sf $PARMhref/href_variable_grib2.tbl    variable.tbl
 fi

else

echo $dom is nonconus for defining the variable parm file
 if [ $ff -gt 0 ]
 then
  if [ ${ff}%3 -eq 0 ]
  then
    ln -sf $PARMhref/href_variable_grib2.tbl_3h_nonconus variable.tbl
  else
    ln -sf $PARMhref/href_variable_grib2.tbl_nonconus    variable.tbl
  fi
 else
  ln -sf $PARMhref/href_variable_grib2.tbl_nonconus    variable.tbl
 fi

fi

$EXEChref/href_ensprod   > $DATA/$ff/output_ensprod.$ff 2>&1
errsave=$?
echo past href_ensprod for ff $ff
export err=$errsave; err_chk

if [ ! -e $COMOUT/log ]
then
mkdir -p $COMOUT/log
fi

cp $DATA/$ff/output_ensprod.$ff $COMOUT/log/output_ensprod.t${cyc}z.$ff

if [ $dom = 'conus' ]
then
types="mean pmmn avrg prob sprd lpmm ffri"
else
types="mean pmmn avrg prob sprd lpmm"
fi

if [ $SENDCOM = YES ]; then

 for typ in $types
 do
  cp $DATA/$ff/${RUN}.${typ}.t${cyc}z.f$ff $COMOUT/ensprod/${RUN}.t${cyc}z.${dom}.${typ}.f$ff.grib2
  $WGRIB2 $COMOUT/ensprod/${RUN}.t${cyc}z.${dom}.${typ}.f$ff.grib2  -s >  $COMOUT/ensprod/${RUN}.t${cyc}z.${dom}.${typ}.f$ff.grib2.idx
 done

 if [ ${ff}%3 -eq 0 ]
 then

  if [ ! -e $COMOUT/verf_g2g ]
  then
   msg="FATAL ERROR: no $COMOUT/verf_g2g directory to copy member files to" 
   err_exit $msg
  fi

  for m in $mbrs ; do
   cp -d $DATA/href.m${m}.t${cyc}z.f${ff}  $COMOUT/verf_g2g/href.m${m}.t${cyc}z.${NEST}.f${ff}
   cp -d $DATA/prcip.m${m}.t${cyc}z.f${ff} $COMOUT/verf_g2g/prcip.m${m}.t${cyc}z.${NEST}.f${ff}
   cp -d $DATA/${ff}/filename              $COMOUT/verf_g2g/filename.t${cyc}z.${NEST}.f${ff}
  done
 fi
fi

if [ $SENDDBN = YES ]; then
 for typ in $types
 do
  $DBNROOT/bin/dbn_alert MODEL HREF_GB2 $job $COMOUT/ensprod/${RUN}.t${cyc}z.${dom}.${typ}.f$ff.grib2
  $DBNROOT/bin/dbn_alert MODEL HREF_GB2_WIDX $job $COMOUT/ensprod/${RUN}.t${cyc}z.${dom}.${typ}.f$ff.grib2.idx
 done
fi

exit
