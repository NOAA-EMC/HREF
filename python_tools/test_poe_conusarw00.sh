#! /bin/ksh

#BSUB -oo /meso/save/Matthew.Pyle/spcprodlike/jobs/test_graphics.out_pyth
#BSUB -eo /meso/save/Matthew.Pyle/spcprodlike/jobs/test_graphics.err_pyth
#BSUB -R span[ptile=8]
#BSUB -x
#BSUB -P HRW-T2O
#BSUB -J SPCPROD_GRAPHICS
#BSUB -q "debug"
#BSUB -n 49
#BSUB -W 0:16
#BSUB -a poe


module load ics
module load ibmpe

module use -a /u/Rahul.Mahajan/modulefiles
module load anaconda
export PYTHONPATH=/meso/save/Jacob.Carley/python/lib

cd /meso/save/Matthew.Pyle/python_tools
base=/meso/save/Matthew.Pyle/python_tools
rm poe.script

cyc=00

PDY=`cat /com/date/t${cyc}z | cut -c7-14`

# # generate 6 h and 36 h precips
# ${base}/test_precip_add.sh_00_conusarw


hrs="06 12 18 24 30 36 42 48"

# for hr in $hrs
# do
# cat precip_6h_${hr}.grib2 >>  /meso2/noscrub/Matthew.Pyle/com/hiresw/prod/spcprod.${PDY}/conusnmm.t${cyc}z.awpreg${hr}.tm00.grib2
# done

# cat precip_36h_36.grib2 >>  /meso2/noscrub/Matthew.Pyle/com/hiresw/prod/spcprod.${PDY}/conusnmm.t${cyc}z.awpreg36.tm00.grib2

hrs="00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48"
for hr in $hrs
do 
echo "python plt_grib2_conusarw_test.py /gpfs/hps/nco/ops/com/hiresw/prod/hiresw.${PDY}/hiresw.t00z.arw_5km.f${hr}.conus.grib2 CONUS arw" >> poe.script
done

chmod 775 poe.script
export MP_PGMMODEL=mpmd
export MP_CMDFILE=poe.script
export MP_LABELIO=YES
export MP_INFOLEVEL=3
export MP_STDOUTMODE=ordered

mpirun.lsf
