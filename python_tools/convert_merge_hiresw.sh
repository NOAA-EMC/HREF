#! /bin/sh
#BSUB -oo /gpfs/hps3/emc/meso/noscrub/Matthew.Pyle/python_tools/test_graphics.out_pyth_00z_hiresw_montage
#BSUB -eo /gpfs/hps3/emc/meso/noscrub/Matthew.Pyle/python_tools/test_graphics.err_pyth_00z_hiresw_montage
#BSUB -M 1500
#BSUB -P HRW-T2O
#BSUB -J HIRESW_GRAPHICS_MERGE
#BSUB -q "devhigh"
#BSUB -cwd /gpfs/hps3/emc/meso/noscrub/Matthew.Pyle/
#BSUB -extsched 'CRAYLINUX[]' -R '1*{select[craylinux && !vnode]} + 8*{select[craylinux && vnode]span[ptile=8] cu[type=cabinet]}'
#BSUB -W 0:29

# module load ics
# module load ibmpe

module load prod_envir
module load prod_util
module load grib_util/1.0.3

cyc=00

hrs="03 06 09 12 15 18 21 24 27 30 33"
hrs="01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48"
hrs="01 02 03"

doms="CONUS NW NC NE SW SC SE Great_Lakes"
# doms="CONUS"

dirbase=/gpfs/hps3/ptmp/Matthew.Pyle

opsdir=${dirbase}/hiresw_python_${cyc}
paradir=${dirbase}/hiresw_python_${cyc}

vars="1kmrefd 2mtdw 2mtw cape cfzr cicep crain csnow max10mw mx1kmrefd mxuphl ptypemean pwat refc vis"

cores="ARW NMMB"

for core in $cores
do
for hr in $hrs
do
for var in $vars
do

echo "#!/bin/sh " > poe.${core}.${hr}.${var}

for dom in $doms
do
echo "convert ${opsdir}/hiresw_${var}_${dom}_f${hr}_CONUS${core}OPS.gif ${paradir}/hiresw_${var}_${dom}_f${hr}_CONUS${core}.gif \
+append -scale 1400x700 ${opsdir}/combo_${var}_${dom}_f${hr}_CONUS${core}.gif" >> poe.${core}.${hr}.${var}
done
chmod 775 ./poe.${core}.${hr}.${var}
aprun -n 1 -N 1 -d 8 ./poe.${core}.${hr}.${var} &
wait
rm ./poe.${core}.${hr}.${var}
done
done
scp ${opsdir}/combo*${core}*.gif mpyle@emcrzdm.ncep.noaa.gov:/home/www/emc/htdocs/mmb/mpyle/hiresw/conus${cyc}/
done
