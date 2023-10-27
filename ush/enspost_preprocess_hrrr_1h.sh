#! /bin/ksh
#####################################################
#
#
#  Script: preprocess_hrrr_1h.sh.ecf
#
# Purpose: Filters out needed inputs from HRRR for RRFSens,
#
#  Author: Matthew Pyle
#          March 2020
#
#  09/2023 - M. Pyle - updates for inclusion in RRFS ensemble

set -x

if [ $# -ne 6 ]
then
echo "FATAL ERROR: need 6 arguments, day,cycle,member,member file name,forecast hour,and domain"
exit
fi

PDY=${1}
cyc=${2}
mem=${3}
name=${4}
hr=${5}
NEST=${6}


if [ $NEST = 'conus' ]
then
dim1=1799
dim2=1059
elif [ $NEST = 'ak' ]
then
dim1=1649
dim2=1105
else
echo "FATAL ERROR: improper region for HRRR preprocessing job" $NEST
exit 99
fi


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
 wgrib2def='nps:210.0:60.0 181.429:1649:2976.0 40.530:1105:2976.0'
 filecheck=${COMINhrrr}.${PDY}/${NESTLOC}/hrrr.t${cyc}z.wrfprsf${hr}.ak.grib2
fi


        if [ -e $filecheck ]
        then

         $WGRIB2 $filecheck | grep -F -f $PARMrrfs/enspost_hrrr_filter.txt | $WGRIB2 -i -grib hrrr.t${cyc}z.f${hr} $filecheck
         $WGRIB2 $filecheck -match ":(HINDEX|TSOIL|SOILW|CSNOW|CICEP|CFRZR|CRAIN|REFD|MAXREF|APCP):" -grib nn.t${cyc}z.f${hr}.grb
         $WGRIB2 $filecheck -match "LTNG" -set_byte 4 23 1 -grib ltng.t${cyc}z.f${hr}.grb
         $WGRIB2 $filecheck -match "MSLMA" -set_byte 4 11 192 -grib mslet.t${cyc}z.f${hr}.grb
	 $WGRIB2 $filecheck -match MAXUVV -set_byte 4 23 100 -set_byte 4 29 100 -grib maxuvv.t${cyc}z.f${hr}.grb

	 $WGRIB2 $filecheck -match "HPBL" -set_byte 4 10 3 -grib pblh_start.t${cyc}z.f${hr}.grb
	 $WGRIB2 pblh_start.t${cyc}z.f${hr}.grb -set_byte 4 11 5 -grib pblh_mid.t${cyc}z.f${hr}.grb
	 $WGRIB2 pblh_mid.t${cyc}z.f${hr}.grb -set_byte 4 23 220 -grib pblh.t${cyc}z.f${hr}.grb

         $WGRIB2 $filecheck -match "RETOP" -set_byte 4 23 200 -grib retop.t${cyc}z.f${hr}.grb
         $WGRIB2 retop.t${cyc}z.f${hr}.grb -set_byte 4 11 197 -grib new_retop.t${cyc}z.f${hr}.grb
         mv  new_retop.t${cyc}z.f${hr}.grb retop.t${cyc}z.f${hr}.grb

         $WGRIB2 $filecheck -match "REFC" -set_byte 4 23 200 -grib refc.t${cyc}z.f${hr}.grb
         $WGRIB2 $filecheck -match "TCDC" -set_byte 4 23 200 -grib tcdc.t${cyc}z.f${hr}.grb

         $WGRIB2 $filecheck -match "WEASD" -match "hour acc fcst" -grib nn2.t${cyc}z.f${hr}.grb
         $WGRIB2 $filecheck -match "ASNOW" -grib nn3.t${cyc}z.f${hr}.grb
         $WGRIB2 $filecheck -match "HGT:cloud ceiling:" -grib ceiling.t${cyc}z.f${hr}.grb

         $WGRIB2 $filecheck -match "HGT:cloud base:" -grib base.t${cyc}z.f${hr}.grb
         $WGRIB2 $filecheck -match "HGT:cloud top:" -grib top.t${cyc}z.f${hr}.grb
         $WGRIB2 $filecheck -match "0C isotherm:" -grib frzh.t${cyc}z.f${hr}.grb


         cat nn.t${cyc}z.f${hr}.grb  nn2.t${cyc}z.f${hr}.grb  nn3.t${cyc}z.f${hr}.grb \
	 ceiling.t${cyc}z.f${hr}.grb retop.t${cyc}z.f${hr}.grb  \
         top.t${cyc}z.f${hr}.grb base.t${cyc}z.f${hr}.grb frzh.t${cyc}z.f${hr}.grb \
         refc.t${cyc}z.f${hr}.grb tcdc.t${cyc}z.f${hr}.grb ltng.t${cyc}z.f${hr}.grb > inputs_nn.t${cyc}z.f${hr}.grb

         rm nn.t${cyc}z.f${hr}.grb  nn2.t${cyc}z.f${hr}.grb nn3.t${cyc}z.f${hr}.grb \
	 ceiling.t${cyc}z.f${hr}.grb retop.t${cyc}z.f${hr}.grb  \
         refc.t${cyc}z.f${hr}.grb tcdc.t${cyc}z.f${hr}.grb ltng.t${cyc}z.f${hr}.grb  \
	 top.t${cyc}z.f${hr}.grb base.t${cyc}z.f${hr}.grb frzh.t${cyc}z.f${hr}.grb

	 cat mslet.t${cyc}z.f${hr}.grb pblh.t${cyc}z.f${hr}.grb maxuvv.t${cyc}z.f${hr}.grb >> hrrr.t${cyc}z.f${hr}
         rm mslet.t${cyc}z.f${hr}.grb pblh*.t${cyc}z.f${hr}.grb

	if [ $NEST = "ak" ]
then
         $WGRIB2 hrrr.t${cyc}z.f${hr} -set_grib_type  jpeg -new_grid_winds grid -new_grid ${wgrib2def} interp.t${cyc}z.f${hr}
         $WGRIB2  inputs_nn.t${cyc}z.f${hr}.grb -new_grid_interpolation neighbor -set_grib_type jpeg -new_grid_winds grid -new_grid ${wgrib2def} interp_nn.t${cyc}z.f${hr}

         cat interp.t${cyc}z.f${hr}  interp_nn.t${cyc}z.f${hr}  > ../hrrr.t${cyc}z.${NEST}.f${hr}.grib2
         rm interp.t${cyc}z.f${hr}  interp_nn.t${cyc}z.f${hr}  inputs_nn.t${cyc}z.f${hr}.grb   hrrr.t${cyc}z.f${hr}

else # conus, so no interp
	cat hrrr.t${cyc}z.f${hr}  inputs_nn.t${cyc}z.f${hr}.grb  > ../hrrr.t${cyc}z.${NEST}.f${hr}.grib2
fi

        else
         msg="FATAL ERROR: $filecheck missing"
         err_exit $msg
        fi

