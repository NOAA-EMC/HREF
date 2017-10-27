#! /bin/sh

if [ $# -eq 1 ]
then
cyc=${1}

else

echo default to 00Z cycle

cyc=00
fi


hrs="03 06 09 12 15 18 21 24 27 30 33"

doms="CONUS NW NC NE SW SC SE Great_Lakes MIDATL"
# doms="CONUS Great_Lakes"

dirbase=/gpfs/hps3/ptmp/Matthew.Pyle

opsdir=${dirbase}/href_ops_python_${cyc}
paradir=${dirbase}/href_v2awip_python_${cyc}

# MEAN

vars="apcp3h vis pwat"
vars36="apcp3h vis pwat apcp36h"

for hr in $hrs
do
for var in $vars
do
for dom in $doms
do

convert ${opsdir}/href_${var}_${dom}_f${hr}_CONUSOPS.gif ${paradir}/href_${var}_${dom}_f${hr}_CONUSMEAN.gif \
+append -scale 1400x700 ${opsdir}/combo_${var}_${dom}_f${hr}_CONUSMEAN.gif

echo DONE $hr $var $dom

done
done
done

hr=36
for var in $vars36
do
for dom in $doms
do
convert ${opsdir}/href_${var}_${dom}_f${hr}_CONUSOPS.gif ${paradir}/href_${var}_${dom}_f${hr}_CONUSMEAN.gif \
+append -scale 1400x700 ${opsdir}/combo_${var}_${dom}_f${hr}_CONUSMEAN.gif

done
done

# PROB 

hrs="03 06 09 12 15 18 21 24 27 30 33 36"
vars="ceil305 ceil610 mx1kmrefdprob40 onekmrefdprob40 pwat37p5 windprobgt20 vis800 windprobgt30 mxuphlprob25 refcgt50"

for hr in $hrs
do
for var in $vars
do
for dom in $doms
do

convert ${opsdir}/href_${var}_${dom}_f${hr}_CONUSOPS.gif ${paradir}/href_${var}_${dom}_f${hr}_CONUSPROB.gif \
+append -scale 1400x700 ${opsdir}/combo_${var}_${dom}_f${hr}_CONUSPROB.gif

echo DONE $hr $var $dom

done
done
done

for hr in $hrs
do
for dom  in $doms
do

convert ${opsdir}/href_apcp3h12p4_${dom}_f${hr}_CONUSOPS.gif ${paradir}/href_apcp3h12p7_${dom}_f${hr}_CONUSPROB.gif \
+append -scale 1400x700 ${opsdir}/combo_apcp3h12p7_${dom}_f${hr}_CONUSPROB.gif

convert ${opsdir}/href_apcp3h25p1_${dom}_f${hr}_CONUSOPS.gif ${paradir}/href_apcp3h25p4_${dom}_f${hr}_CONUSPROB.gif \
+append -scale 1400x700 ${opsdir}/combo_apcp3h25p4_${dom}_f${hr}_CONUSPROB.gif

convert ${opsdir}/href_etopgt10000_${dom}_f${hr}_CONUSOPS.gif ${paradir}/href_etopgt10668_${dom}_f${hr}_CONUSPROB.gif \
+append -scale 1400x700 ${opsdir}/combo_etopgt10668_${dom}_f${hr}_CONUSPROB.gif

done
done
