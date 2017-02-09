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
dom=${2}

wgrib2def="lambert:265:25:25 226.541:1473:5079 12.190:1025:5079"

echo here in ush script with dom $dom

cd $DATA


mbr=0

ff=$fhr

mbrs=1


  mkdir -p $DATA/${ff} 
   mbr=0
   for m in $mbrs ; do
      fcst=$ff   #$ff is forecast hours of ensemble member to be built, $fcst is forecast hours of base model requested

      echo ff $ff m $m
      echo fcst $fcst

      echo href.m${m}.t${cyc}z.f${ff} 


###### namnest


        filecheck=${COMINnamx}.${PDY}/nam.t${cyc}z.conusnest.hiresf${fcst}.tm00.grib2
        if [ -e $filecheck ]
        then
	echo WGRIB2 is $WGRIB2
        $WGRIB2 $filecheck | grep -F -f $PARMhref/href_namx_filter.txt | $WGRIB2 -i -grib namx.f${ff} $filecheck
        $WGRIB2 namx.f${ff} -set_grib_type  jpeg -new_grid_winds grid -new_grid ${wgrib2def} $DATA/href.m${m}.t${cyc}z.f${ff}
        ln -sf $DATA/href.m${m}.t${cyc}z.f${ff}  $DATA/${ff}/href.m${m}.t${cyc}z.f${ff}


   done #members

exit
