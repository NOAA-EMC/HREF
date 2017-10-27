#BSUB -oo /gpfs/hps/emc/meso/noscrub/Matthew.Pyle/python_tools/test_graphics.out_pyth_00z_sseo
#BSUB -eo /gpfs/hps/emc/meso/noscrub/Matthew.Pyle/python_tools/test_graphics.err_pyth_00z_sseo
#BSUB -M 1500
#BSUB -P HRW-T2O
#BSUB -J SSEO_GRAPHICS
#BSUB -q "dev"
#BSUB -cwd /gpfs/hps/emc/meso/noscrub/
#BSUB -extsched 'CRAYLINUX[]' -R '1*{select[craylinux && !vnode]} + 1*{select[craylinux && vnode]span[ptile=1] cu[type=cabinet]}'
#BSUB -W 0:29

module load prod_envir
module load prod_util
module load grib_util/1.0.3

module use -a /u/Rahul.Mahajan/modulefiles
module load anaconda
export PYTHONPATH=/gpfs/hps/emc/meso/noscrub/Matthew.Pyle/python_tools/lib

srcdir=/gpfs/hps/emc/meso/noscrub/Matthew.Pyle/python_tools

cyc=00

mkdir -p /gpfs/hps/ptmp/Matthew.Pyle/href_sseo_python_${cyc}/
cd  /gpfs/hps/ptmp/Matthew.Pyle/href_sseo_python_${cyc}/
rm  /gpfs/hps/ptmp/Matthew.Pyle/href_sseo_python_${cyc}/*

cp $srcdir/filter_href_grib.scr .
cp $srcdir/*href*.py .
cp $srcdir/test_precip_add.sh_hrefsseo .

./filter_href_grib.scr ${cyc} sseo

# # generate 6 h and 36 h precips
./test_precip_add.sh_hrefsseo ${cyc}

types="mean pmmn avrg"
for typ in $types
do
cat ${typ}_36h_36.grib2 >>  para.f36.grb2_${typ}
done

types="mean pmmn avrg ffri prob"
hrs="03 06 09 12 15 18 21 24 27 30 33 36"

for hr in $hrs
do
for typ in $types
do
python href_generic.py para.f${hr}.grb2_${typ} CONUS ${typ}
done
done

### ops

for hr in $hrs
do
python href_generic.py ops.f${hr}.grb2_2 CONUS ops
done

cd $srcdir

bsub < send_python_graphics_sseo.sh_${cyc}
