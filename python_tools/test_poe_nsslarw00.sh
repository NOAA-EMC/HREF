#! /bin/ksh

#BSUB -oo /meso/save/Matthew.Pyle/spcprodlike/jobs/test_graphics.out_pyth_00z_test
#BSUB -eo /meso/save/Matthew.Pyle/spcprodlike/jobs/test_graphics.err_pyth_00z_test
#BSUB -R span[ptile=9]
#BSUB -x
#BSUB -P HRW-T2O
#BSUB -J SPCPROD_GRAPHICS
#BSUB -q "debug"
#BSUB -n 49
#BSUB -W 0:18
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

# # generate 6 h and 48 h precips
${base}/test_precip_add.sh_00_nssl

hrs="06 12 18 24 30 36 42 48"

for hr in $hrs
do
cat precip_6h_${hr}.grib2 >>  /gpfs/hps/ptmp/Matthew.Pyle/com/hiresw/test/hiresw.${PDY}/hiresw.t${cyc}z.arw_5km.f${hr}.conusnssl.grib2
done

cat precip_48h_48.grib2 >>  /gpfs/hps/ptmp/Matthew.Pyle/com/hiresw/test/hiresw.${PDY}/hiresw.t${cyc}z.arw_5km.f48.conusnssl.grib2

hrs="00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48"
for hr in $hrs
do 
echo "python plt_grib2_nsslwrf_test.py /gpfs/hps/ptmp/Matthew.Pyle/com/hiresw/test/hiresw.${PDY}/hiresw.t00z.arw_5km.f${hr}.conusnssl.grib2 CONUS nsslarw" >> poe.script
done

chmod 775 poe.script
export MP_PGMMODEL=mpmd
export MP_CMDFILE=poe.script
export MP_LABELIO=YES
export MP_INFOLEVEL=3
export MP_STDOUTMODE=ordered

mpirun.lsf

bsub < ${base}/send_python_graphics.sh_00
