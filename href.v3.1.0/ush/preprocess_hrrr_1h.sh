#! /bin/ksh
#####################################################
#
#
#  Script: preprocess_hrrr_1h.sh.ecf
#
# Purpose: Filters out needed inputs from HRRR for HREF,
#          and interpolates onto the HREF domain.
#
#  Author: Matthew Pyle
#          March 2020


if [ $# -ne 3 ]
then
echo "ERROR: need cycle and forecast hour and nest"
exit
fi

cyc=${1}
NEST=${2}
hr=${3}

echo $NEST $hr

if [ ! -e $GESOUT.${PDY} ]
then
mkdir -p $GESOUT.${PDY}
fi

cd ${DATA}

mkdir hrrr_${NEST}_${hr}
cd hrrr_${NEST}_${hr}


if [ $NEST = "conus" ]
then
 wgrib2def="lambert:265:25:25 226.541:1473:5079 12.190:1025:5079"
 NESTLOC=conus
 filecheck=${COMINhrrr}.${PDY}/${NESTLOC}/hrrr.t${cyc}z.wrfprsf${hr}.grib2
elif [ $NEST = "ak" ]
then 
 NESTLOC=alaska
 wgrib2def="nps:210:60 185.5:825:5000 44.8:603:5000"
 filecheck=${COMINhrrr}.${PDY}/${NESTLOC}/hrrr.t${cyc}z.wrfprsf${hr}.ak.grib2
fi


        if [ -e $filecheck ]
        then

         $WGRIB2 $filecheck | grep -F -f $PARMhref/href_hrrr_filter.txt | $WGRIB2 -i -grib hrrr.t${cyc}z.f${hr} $filecheck
         $WGRIB2 $filecheck -match ":(HINDEX|TSOIL|SOILW|CSNOW|CICEP|CFRZR|CRAIN|REFD|MAXREF|APCP):" -grib nn.t${cyc}z.f${hr}.grb
         $WGRIB2 $filecheck -match "LTNG" -set_byte 4 23 1 -grib ltng.t${cyc}z.f${hr}.grb

         $WGRIB2 $filecheck -match "RETOP" -set_byte 4 23 200 -grib retop.t${cyc}z.f${hr}.grb
         $WGRIB2 retop.t${cyc}z.f${hr}.grb -set_byte 4 11 197 -grib new_retop.t${cyc}z.f${hr}.grb
         mv  new_retop.t${cyc}z.f${hr}.grb retop.t${cyc}z.f${hr}.grb

         $WGRIB2 $filecheck -match "REFC" -set_byte 4 23 200 -grib refc.t${cyc}z.f${hr}.grb
         $WGRIB2 $filecheck -match "TCDC" -set_byte 4 23 200 -grib tcdc.t${cyc}z.f${hr}.grb

         $WGRIB2 $filecheck -match "WEASD" -match "hour acc fcst" -grib nn2.t${cyc}z.f${hr}.grb
         $WGRIB2 $filecheck -match "HGT:cloud ceiling:" -grib ceiling.t${cyc}z.f${hr}.grb

         cat nn.t${cyc}z.f${hr}.grb  nn2.t${cyc}z.f${hr}.grb ceiling.t${cyc}z.f${hr}.grb retop.t${cyc}z.f${hr}.grb  \
         refc.t${cyc}z.f${hr}.grb tcdc.t${cyc}z.f${hr}.grb ltng.t${cyc}z.f${hr}.grb > inputs_nn.t${cyc}z.f${hr}.grb

         rm nn.t${cyc}z.f${hr}.grb  nn2.t${cyc}z.f${hr}.grb ceiling.t${cyc}z.f${hr}.grb retop.t${cyc}z.f${hr}.grb  \
         refc.t${cyc}z.f${hr}.grb tcdc.t${cyc}z.f${hr}.grb ltng.t${cyc}z.f${hr}.grb

         $WGRIB2 hrrr.t${cyc}z.f${hr} -set_grib_type  jpeg -new_grid_winds grid -new_grid ${wgrib2def} interp.t${cyc}z.f${hr}
         $WGRIB2  inputs_nn.t${cyc}z.f${hr}.grb -new_grid_interpolation neighbor -set_grib_type jpeg -new_grid_winds grid -new_grid ${wgrib2def} interp_nn.t${cyc}z.f${hr}

         cat interp.t${cyc}z.f${hr}  interp_nn.t${cyc}z.f${hr}  > ../hrrr.t${cyc}z.${NEST}.f${hr}.grib2
         rm interp.t${cyc}z.f${hr}  interp_nn.t${cyc}z.f${hr}  inputs_nn.t${cyc}z.f${hr}.grb   hrrr.t${cyc}z.f${hr}

        else
         msg="FATAL ERROR: $filecheck missing"
         err_exit $msg
        fi

