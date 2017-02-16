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

export XLFRTEOPTS="namelist=old"

yy=`echo ${PDY} | cut -c 1-4`
mm=`echo ${PDY} | cut -c 5-6`
dd=`echo ${PDY} | cut -c 7-8`

ff=$fhr
dom=${NEST}

cd $DATA/${ff}/

# ln -sf  $FIXhref/*href5km .

# ln -sf $DATA/ffg*href5km .
# ln -sf $COMINffg/ffg*href5km .



###############################

typeset -Z2 cycloc     #temp variable here
typeset -Z2 fcst    
typeset -Z2 m

if [ $cyc -ge 0 ] && [ $cyc -le 5 ] ; then

  if [ $dom = 'conus' ]
    then
        echo "in conus block"
     files="9 namnestx namnestx conusarw conusnmmb conusmem2arw conusarw conusnmmb conusmem2arw"
     set -A file  $files
     if [ $cyc = '00' ] ; then
      days="9 $PDY $PDYm1 $PDY $PDY $PDY $PDYm1 $PDYm1 $PDYm1"
      cycs="9 00    18     00    00   00   12     12    12"
      ages="9  0     6      0     0    0   12     12    12"
     fi
     set -A  day  $days
     set -A  cycloc $cycs
     set -A  age  $ages
     mbrs="1  2  3  4  5  6  7  8"


  elif [ $dom = 'hi' ]
    then

     files="9  hiarw hinmmb himem2arw hiarw hinmmb himem2arw"
     set -A file  $files
     if [ $cyc = '00' ] ; then
      days="9  $PDY $PDY $PDY $PDYm1 $PDYm1 $PDYm1"
      cycs="9   00    00   00   12     12    12"
      ages="9   0      0    0   12     12    12"
     fi
     set -A  day  $days
     set -A  cycloc $cycs
     set -A  age  $ages
     mbrs="1  2  3  4  5  6"


  elif [ $dom = 'ak' ]
    then
     files="9 akarw aknmmb akmem2arw akarw aknmmb akmem2arw"
     set -A file  $files
     days="9 $PDYm1 $PDYm1 $PDYm1  $PDYm1 $PDYm1 $PDYm1"
     cycs="9 18      18      18     06     06    06"
     ages="9  6       6       6     18     18    18"
     set -A  day  $days
     set -A  cycloc $cycs
     set -A  age  $ages
     mbrs="1  2  3  4  5  6"

  elif [ $dom = 'pr' ]
    then
     files="9 prarw prnmmb prmem2arw prarw prnmmb prmem2arw"
     set -A file  $files
     days="9 $PDYm1 $PDYm1 $PDYm1  $PDYm1 $PDYm1 $PDYm1"
     cycs="9 18      18      18     06     06    06"
     ages="9  6       6       6     18     18    18"
     set -A  day  $days
     set -A  cycloc $cycs
     set -A  age  $ages
     mbrs="1  2  3  4  5  6"

    else
      echo "bad domain"
    fi


elif [ $cyc -ge 6 ] ; then

  echo ge6 cyc
  echo know dom $dom

  if [ $dom = 'conus' ]
  then

  files="9 namnestx namnestx conusarw conusnmmb conusmem2arw conusarw conusnmmb conusmem2arw"
  set -A file  $files
  mbrs="1  2  3  4  5  6  7  8"

  if [ $cyc = '06' ] ; then
    days="9 $PDY $PDY $PDY $PDY $PDY $PDYm1 $PDYm1 $PDYm1"
    cycs="9 06 00 00 00 00 12 12 12"
    ages="9  0  6  6  6  6 18 18 18"
  fi

  if [ $cyc = '12' ] ; then
    days="9 $PDY $PDY $PDY $PDY $PDY $PDY $PDY $PDY"
    cycs="9 12 06 12 12 12  00  00 00"
    ages="9  0  6  0  0  0  12 12 12"
        echo cycs $cycs
        echo ages $ages
  fi

  if [ $cyc = '18' ] ; then
    days="9 $PDY $PDY $PDY $PDY $PDY $PDY $PDY $PDY"
    cycs="9 18 12 12 12 12 00 00 00"
    ages="9  0  6  6  6  6 18 18 18"
  fi

  set -A  day $days
  set -A  cycloc $cycs
  set -A  age $ages

  elif [ $dom = 'ak' ]
  then
  files="9 akarw aknmmb akmem2arw akarw aknmmb akmem2arw"
  set -A file  $files
  mbrs="1  2  3  4  5  6"

  if [ $cyc = '06' ] ; then
    days="9 $PDY $PDY $PDY  $PDYm1 $PDYm1 $PDYm1"
    cycs="9  06   06   06     18     18    18"
    ages="9  0     0    0     12     12    12"
  fi

  if [ $cyc = '12' ] ; then
    days="9 $PDY $PDY $PDY  $PDYm1 $PDYm1 $PDYm1"
    cycs="9  06   06   06     18     18    18"
    ages="9  6     6    6     18     18    18"
  fi

  if [ $cyc = '18' ] ; then
    days="9 $PDY $PDY $PDY  $PDY    $PDY  $PDY"
    cycs="9  18   18   18     06     06    06"
    ages="9  0     0    0     12     12    12"
  fi

  set -A  day $days
  set -A  cycloc $cycs
  set -A  age $ages

  elif [ $dom = 'hi' ]
  then

  files="9 hiarw hinmmb himem2arw hiarw hinmmb himem2arw"
  set -A file  $files
  mbrs="1  2  3  4  5  6"

  if [ $cyc = '06' ] ; then
    days="9 $PDY $PDY $PDY $PDYm1 $PDYm1 $PDYm1"
    cycs="9 00 00 00 12 12 12"
    ages="9 6  6  6  18 18 18"
  fi

  if [ $cyc = '12' ] ; then
    days="9 $PDY $PDY $PDY $PDY $PDY $PDY"
    cycs="9 12 12 12  00 00 00"
    ages="9  0  0  0  12 12 12"
  fi

  if [ $cyc = '18' ] ; then
    days="9 $PDY $PDY $PDY $PDY $PDY $PDY $PDY"
    cycs="9 12 12 12 00 00 00"
    ages="9 6  6  6  18 18 18"
  fi

  set -A  day $days
  set -A  cycloc $cycs
  set -A  age $ages

  elif [ $dom = 'pr' ]
  then

	echo "defining pr for 06 12 18 cycles"
  files="9 prarw prnmmb prmem2arw prarw prnmmb prmem2arw"

  set -A file  $files

  if [ $cyc = '06' ] ; then
    days="9 $PDY $PDY $PDY  $PDYm1 $PDYm1 $PDYm1"
    cycs="9  06   06   06     18     18    18"
    ages="9  0     0    0     12     12    12"
  fi

  if [ $cyc = '12' ] ; then
    days="9 $PDY $PDY $PDY  $PDYm1 $PDYm1 $PDYm1"
    cycs="9  06   06   06     18     18    18"
    ages="9  6     6    6     18     18    18"
  fi

  if [ $cyc = '18' ] ; then
    days="9 $PDY $PDY $PDY  $PDY    $PDY  $PDY"
    cycs="9  18   18   18     06     06    06"
    ages="9  0     0    0     12     12    12"
  fi

  set -A  day $days
  set -A  cycloc $cycs
  set -A  age $ages
	echo defining mbrs for pr
  mbrs="1  2  3  4  5  6"

fi

else

 echo $cyc ' is not a cycle'

fi

 nmbr=0

	echo mbrs is $mbrs
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
   fi
 done

echo dom is $dom

  if [ $dom = 'conus' ]
  then
 echo $yy $mm $dd $cyc $ff "227 39" "36" "3" "12"  > filename    #first 36 is leadtime, second 12 is fcst times = leadtime/interval
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

$EXEChref/sseo_ensprod   > $DATA/$ff/output_ensprod.$ff 2>&1
errsave=$?
echo past sseo_ensprod for ff $ff
export err=$errsave; err_chk;

if [ $SENDCOM = YES ]; then
 cp $DATA/$ff/href.mean.t${cyc}z.f$ff $COMOUT/href.t${cyc}z.${dom}.mean.f$ff.grib2
 $WGRIB2 $COMOUT/href.t${cyc}z.${dom}.mean.f$ff.grib2  -s >  $COMOUT/href.t${cyc}z.${dom}.mean.f$ff.grib2.idx

 cp $DATA/$ff/href.pmmn.t${cyc}z.f$ff $COMOUT/href.t${cyc}z.${dom}.pmmn.f$ff.grib2
 $WGRIB2 $COMOUT/href.t${cyc}z.${dom}.pmmn.f$ff.grib2  -s >  $COMOUT/href.t${cyc}z.${dom}.pmmn.f$ff.grib2.idx

#cp $DATA/$ff/href.ffri.t${cyc}z.f$ff $COMOUT/href.t${cyc}z.${dom}.ffri.f$ff.grib2
#$WGRIB2 $COMOUT/href.t${cyc}z.${dom}.ffri.f$ff.grib2  -s >  $COMOUT/href.t${cyc}z.${dom}.ffri.f$ff.grib2.idx

 cp $DATA/$ff/href.avrg.t${cyc}z.f$ff $COMOUT/href.t${cyc}z.${dom}.avrg.f$ff.grib2
 $WGRIB2 $COMOUT/href.t${cyc}z.${dom}.avrg.f$ff.grib2  -s >  $COMOUT/href.t${cyc}z.${dom}.avrg.f$ff.grib2.idx

 cp $DATA/$ff/href.prob.t${cyc}z.f$ff $COMOUT/href.t${cyc}z.${dom}.prob.f$ff.grib2
 $WGRIB2 $COMOUT/href.t${cyc}z.${dom}.prob.f$ff.grib2  -s >  $COMOUT/href.t${cyc}z.${dom}.prob.f$ff.grib2.idx

 cp $DATA/$ff/href.sprd.t${cyc}z.f$ff $COMOUT/href.t${cyc}z.${dom}.sprd.f$ff.grib2
 $WGRIB2 $COMOUT/href.t${cyc}z.${dom}.sprd.f$ff.grib2  -s >  $COMOUT/href.t${cyc}z.${dom}.sprd.f$ff.grib2.idx

 cp $DATA/$ff/href.mean.t${cyc}z.f$ff $COMOUTPERM/href.t${cyc}z.${dom}.mean.f$ff.grib2
 $WGRIB2 $COMOUTPERM/href.t${cyc}z.${dom}.mean.f$ff.grib2  -s >  $COMOUTPERM/href.t${cyc}z.${dom}.mean.f$ff.grib2.idx

 cp $DATA/$ff/href.pmmn.t${cyc}z.f$ff $COMOUTPERM/href.t${cyc}z.${dom}.pmmn.f$ff.grib2
 $WGRIB2 $COMOUTPERM/href.t${cyc}z.${dom}.pmmn.f$ff.grib2  -s >  $COMOUTPERM/href.t${cyc}z.${dom}.pmmn.f$ff.grib2.idx

#cp $DATA/$ff/href.ffri.t${cyc}z.f$ff $COMOUTPERM/href.t${cyc}z.${dom}.ffri.f$ff.grib2
#$WGRIB2 $COMOUTPERM/href.t${cyc}z.${dom}.ffri.f$ff.grib2  -s >  $COMOUTPERM/href.t${cyc}z.${dom}.ffri.f$ff.grib2.idx

 cp $DATA/$ff/href.avrg.t${cyc}z.f$ff $COMOUTPERM/href.t${cyc}z.${dom}.avrg.f$ff.grib2
 $WGRIB2 $COMOUTPERM/href.t${cyc}z.${dom}.avrg.f$ff.grib2  -s >  $COMOUTPERM/href.t${cyc}z.${dom}.avrg.f$ff.grib2.idx

 cp $DATA/$ff/href.prob.t${cyc}z.f$ff $COMOUTPERM/href.t${cyc}z.${dom}.prob.f$ff.grib2
 $WGRIB2 $COMOUTPERM/href.t${cyc}z.${dom}.prob.f$ff.grib2  -s >  $COMOUTPERM/href.t${cyc}z.${dom}.prob.f$ff.grib2.idx

# cp $DATA/$ff/href.sprd.t${cyc}z.f$ff $COMOUTPERM/href.t${cyc}z.${dom}.sprd.f$ff.grib2
# $WGRIB2 $COMOUTPERM/href.t${cyc}z.${dom}.sprd.f$ff.grib2  -s >  $COMOUTPERM/href.t${cyc}z.${dom}.sprd.f$ff.grib2.idx

fi
if [ $SENDDBN = YES ]; then
  $DBNROOT/bin/dbn_alert MODEL HREF_GB2 $job $COMOUT/href.t${cyc}z.${dom}.mean.f$ff.grib2
  $DBNROOT/bin/dbn_alert MODEL HREF_GB2_IDX $job $COMOUT/href.t${cyc}z.${dom}.mean.f$ff.grib2.idx
  $DBNROOT/bin/dbn_alert MODEL HREF_GB2 $job $COMOUT/href.t${cyc}z.${dom}.pmmn.f$ff.grib2
  $DBNROOT/bin/dbn_alert MODEL HREF_GB2_IDX $job $COMOUT/href.t${cyc}z.${dom}.pmmn.f$ff.grib2.idx
  $DBNROOT/bin/dbn_alert MODEL HREF_GB2 $job $COMOUT/href.t${cyc}z.${dom}.ffri.f$ff.grib2
  $DBNROOT/bin/dbn_alert MODEL HREF_GB2_IDX $job $COMOUT/href.t${cyc}z.${dom}.ffri.f$ff.grib2.idx
  $DBNROOT/bin/dbn_alert MODEL HREF_GB2 $job $COMOUT/href.t${cyc}z.${dom}.avrg.f$ff.grib2
  $DBNROOT/bin/dbn_alert MODEL HREF_GB2_IDX $job $COMOUT/href.t${cyc}z.${dom}.avrg.f$ff.grib2.idx
  $DBNROOT/bin/dbn_alert MODEL HREF_GB2 $job $COMOUT/href.t${cyc}z.${dom}.prob.f$ff.grib2
  $DBNROOT/bin/dbn_alert MODEL HREF_GB2_IDX $job $COMOUT/href.t${cyc}z.${dom}.prob.f$ff.grib2.idx
  $DBNROOT/bin/dbn_alert MODEL HREF_GB2 $job $COMOUT/href.t${cyc}z.${dom}.sprd.f$ff.grib2
  $DBNROOT/bin/dbn_alert MODEL HREF_GB2_IDX $job $COMOUT/href.t${cyc}z.${dom}.sprd.f$ff.grib2.idx
fi

exit