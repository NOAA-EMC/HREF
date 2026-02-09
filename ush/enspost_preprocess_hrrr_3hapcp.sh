#! /bin/ksh
#####################################################
#
#
#  Script: preprocess_hrrr_3hapcp.sh.ecf
#
# Purpose: Generates 3 h QPF/snow buckets from the HRRR
#
#  Author: Matthew Pyle
#          March 2020
#
#          02/09/2026, M. Pyle - updated for hour by hour option

set -x

if [ $# -ne 4 ]
then
echo need 4 inputs: dom, day, cyc, fhr
exit
fi


dom=${1}
PDY=${2}
cyc=${3}
fhr=${4}


if [ ! -e $GESOUT.${PDY} ]
then
mkdir -p $GESOUT.${PDY}
fi


cd $DATA

EXECrefs=${HOMErefs}/exec

hrsln="00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48"

for hr in $fhr
do
filecheck=hrrr.t${cyc}z.${dom}.f${hr}.grib2

if [ -e $filecheck ]
then
ln -sf hrrr.t${cyc}z.${dom}.f${hr}.grib2 hrrr.t${cyc}z.f${hr}.grib2
fi
done

for hr in $hrs
do

let old3=hr-3
let old2=hr-2
let old1=hr-1

hrold3=$(printf %2.2i $old3)
hrold2=$(printf %2.2i $old2)
hrold1=$(printf %2.2i $old1)


filecheck=hrrr.t${cyc}z.${dom}.f${hr}.grib2

if [ -e $filecheck ]
then


        if [ $hr -gt 0 ]
        then
        echo here a $hr

        ln -sf ../temp.t${cyc}z.f${hrold3}.grib2 hrrr.t${cyc}z.${dom}.f${hrold3}.grib2
        ln -sf ../temp.t${cyc}z.f${hrold2}.grib2 hrrr.t${cyc}z.${dom}.f${hrold2}.grib2
        ln -sf ../temp.t${cyc}z.f${hrold1}.grib2 hrrr.t${cyc}z.${dom}.f${hrold1}.grib2

## the hourly will sum up three hourly at the appropriate time.
        echo hrrr.t${cyc}z.f $hr .false. .false. .true. .false. .false. 1 ${dom} no 8 |$EXECrefs/enspost_get_prcip > output.refs_get_prcip1h.f${hr}.${dom}
        export err=$? ; err_chk

        if [ ${hr}%3 -eq 0 ]
        then
        cat prcip3h.t${cyc}z.f${hr}.grib2 >> hrrr.t${cyc}z.${dom}.f${hr}.grib2
        fi
        fi

else
        msg="FATAL ERROR: $filecheck missing"
        err_exit $msg
fi

done

for hr in $hrsln
do
 cp hrrr.t${cyc}z.${dom}.f${hr}.grib2 ${GESOUT}.${PDY}
 err=$?
 export err ; err_chk
done
