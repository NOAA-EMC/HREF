#!/bin/ksh
#######################################################################################
#  Script of Name:href_getmbr.sh 
#  Purpose: this script is to get softlink of previous of runs namnest, hireswnmmb and 
#           hireswarw according to time table (since all of them are on same #227 grid
#           no copygb2 is involved) 
#  History: 2015-02-02: Binbin Zhou created 
#    Usage: href_getmbr.sh fhr cycle Day 
######################################################################################
set -x         

typeset -Z2 cycloc
typeset -Z2 fcst
typeset -Z2 m

fhr=$1

wgrib2def="lambert:265:25:25 226.541:1473:5079 12.190:1025:5079"


cd $DATA

if [ $cyc -ge 0 ] && [ $cyc -le 5 ] ; then 

  files="9 namnest namnest namnest namnest namnest conusarw conusnmmb conusarw conusnmmb conusarw conusnmmb"
  set -A file  $files

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

    set -A  day  $days
    set -A  cycloc $cycs
    set -A  age  $ages
    mbrs="   1  2  3  4  5  6  7  8  9 10 11"

elif [ $cyc -ge 6 ] ; then

  files="9 namnest namnest namnest namnest namnest namnest conusarw conusnmmb conusarw conusnmmb conusarw conusnmmb"
  set -A file  $files
     mbrs="   1  2  3  4  5  6  7  8  9 10 11 12"

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


mbr=0

ff=$fhr
  mkdir -p $DATA/${ff} 
   mbr=0
   for m in $mbrs ; do
      fcst=` expr ${age[$m]} + $ff`   #$ff is forecast hours of ensemble member to be built, $fcst is forecast hours of base model requested

      echo ff $ff
      echo fcst $fcst

      echo href.m${m}.t${cyc}z.f${ff} 

      if [ ${file[$m]} = 'namnest' ] ; then     

# now need to interpolate NAM nest data to the 5 km grid 227 used by HREF

      filecheck=${COMINnam}.${day[$m]}/nam.t${cycloc[$m]}z.conusnest.hiresf${fcst}.tm00.grib2

	if [ -e $filecheck ]
        then

         $WGRIB2 $filecheck | grep -F -f $PARMhref/namnest_subset.txt_11 | $WGRIB2 -i -grib ./${ff}/inputs1.grb $filecheck
         $WGRIB2 $filecheck | grep -F -f $PARMhref/namnest_subset.txt_22 | $WGRIB2 -i -grib ./${ff}/inputs2.grb $filecheck
         $WGRIB2 $filecheck | grep -F -f $PARMhref/namnest_subset.txt_33 | $WGRIB2 -i -grib ./${ff}/inputs3.grb $filecheck

         $WGRIB2 $filecheck  -match ":(APCP|HINDEX|TSOIL|SOILW|CSNOW|CICEP|CFRZR|CRAIN|RETOP|REFD|MAXREF):" -grib ./${ff}/nn.grb
         $WGRIB2 $filecheck  -match "HGT:cloud ceiling:" -grib ./${ff}/ceiling.grb
         cat ./${ff}/nn.grb ./${ff}/ceiling.grb > ./${ff}/inputs_nn.grb
         rm ./${ff}/nn.grb ./${ff}/ceiling.grb

         cd $ff

         datestr=`date`
         echo "start WGRIB2 interpolation" $datestr

# could it be poeized?

         $WGRIB2  inputs1.grb   -set_grib_type jpeg -new_grid_winds grid -new_grid ${wgrib2def}  main1.grib2 &
         $WGRIB2  inputs2.grb   -set_grib_type jpeg -new_grid_winds grid -new_grid ${wgrib2def}  main2.grib2 &
         $WGRIB2  inputs3.grb   -set_grib_type jpeg -new_grid_winds grid -new_grid ${wgrib2def}  main3.grib2 &
         $WGRIB2  inputs_nn.grb -set_grib_type jpeg -new_grid_interpolation neighbor  -new_grid_winds grid -new_grid ${wgrib2def} nn.grib2 &
         wait

#no         echo "$WGRIB2  inputs1.grb   -set_grib_type jpeg -new_grid_winds grid -new_grid ${wgrib2def}  main1.grib2" > poe.wgrib2
#no         echo "$WGRIB2  inputs2.grb   -set_grib_type jpeg -new_grid_winds grid -new_grid ${wgrib2def}  main2.grib2" >> poe.wgrib2
#no         echo "$WGRIB2  inputs3.grb   -set_grib_type jpeg -new_grid_winds grid -new_grid ${wgrib2def}  main3.grib2" >> poe.wgrib2
#no         echo "$WGRIB2  inputs_nn.grb -set_grib_type jpeg -new_grid_interpolation neighbor  -new_grid_winds grid -new_grid ${wgrib2def} nn.grib2" >> poe.wgrib2

#no         export MP_CMDFILE=poe.wgrib2
#no         export MP_PGMMODEL=mpmd
#no         export MP_EUILIB=us
#no         export MP_LABELIO=YES
#no         export MP_INFOLEVEL=3
#no         mpirun.lsf

         cat main1.grib2 main2.grib2 main3.grib2 nn.grib2  > $DATA/href.m${m}.t${cyc}z.f${ff}
         rm inputs1.grb inputs2.grib inputs_nn.grb  
         rm main1.grib2 main2.grib2 main3.grib2 nn.grib2
         cd ../
         datestr=`date`
         echo "finish WGRIB2 interpolation" $datestr

         ln -sf  $DATA/href.m${m}.t${cyc}z.f${ff} $DATA/${ff}/href.m${m}.t${cyc}z.f${ff}

        fi
      fi
 
      if [ ${file[$m]} = 'conusarw' ] ; then
        ln -sf ${COMINhiresw}.${day[$m]}/hiresw.t${cycloc[$m]}z.arw_5km.f${fcst}.conus.grib2 $DATA/href.m${m}.t${cyc}z.f${ff}
        ln -sf ${COMINhiresw}.${day[$m]}/hiresw.t${cycloc[$m]}z.arw_5km.f${fcst}.conus.grib2 $DATA/${ff}/href.m${m}.t${cyc}z.f${ff}
      fi

      if [ ${file[$m]} = 'conusnmmb' ] ; then
        ln -sf ${COMINhiresw}.${day[$m]}/hiresw.t${cycloc[$m]}z.nmmb_5km.f${fcst}.conus.grib2 $DATA/href.m${m}.t${cyc}z.f${ff}
        ln -sf ${COMINhiresw}.${day[$m]}/hiresw.t${cycloc[$m]}z.nmmb_5km.f${fcst}.conus.grib2 $DATA/${ff}/href.m${m}.t${cyc}z.f${ff}
      fi

   done

exit
