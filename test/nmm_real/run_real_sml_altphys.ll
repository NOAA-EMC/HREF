#! /bin/ksh
# @ job_name=wrf_single
# @ notification=never
# @ output=wrf_nmm_real.out
# @ error=wrf_nmm_real.err
# @ job_type = parallel
# @ total_tasks = 1
# @ node = 1
# @ network.MPI = csss,shared,us
# @ class=dev
# @ node_usage=shared
# @ wall_clock_limit=00:29:00
# @ queue

SIDOM=${1}
ALTNAM=${1}
CYC=${2}

exedir=WRFV1

cd /emc2/wx20py/wrf_nmm/WRFV1/test/nmm_real/

### modify namelist file
ystart=`cat /gpfstmp/wx20py/date/t${CYC}z | cut -c7-10`
mstart=`cat /gpfstmp/wx20py/date/t${CYC}z | cut -c11-12`
dstart=`cat /gpfstmp/wx20py/date/t${CYC}z | cut -c13-14`

dstart=10

start=$ystart$mstart$dstart

tmptom=`./tomorrow $start`

end=`./tomorrow $tmptom`

yend=`echo $end | cut -c1-4`
mend=`echo $end | cut -c5-6`
dend=`echo $end | cut -c7-8`

cat namelist.input_smlin_altphys | sed s:YSTART:$ystart: | sed s:MSTART:$mstart: \
 | sed s:DSTART:$dstart: | sed s:HSTART:$CYC: | sed s:YEND:$yend: \
 | sed s:MEND:$mend:     | sed s:DEND:$dend: | sed s:HEND:$CYC: > namelist.input


mkdir -p /gpfstmp/wx20py/tmp_wrfreal_$ALTNAM

cd /gpfstmp/wx20py/tmp_wrfreal_$ALTNAM
rm /gpfstmp/wx20py/tmp_wrfreal_$ALTNAM/*

rm -rf /gpfstmp/wx20py/tmp_wrfreal_$ALTNAM/coredir*


cp /gpfstmp/wx20py/dataroot/$SIDOM/siprd/wrf_real* .
cp /gpfstmp/wx20py/date/t${CYC}z .


if [ $? -ne 0 ] 
then
exit
fi

cp /emc2/wx20py/wrf_nmm/WRFV1/test/nmm_real/namelist.input .

cp /emc2/wx20py/wrf_nmm/$exedir/main/real.exe .
cp /emc2/wx20py/wrf_nmm/$exedir/test/nmm_real/LANDUSE.TBL .
cp /emc2/wx20py/wrf_nmm/$exedir/test/nmm_real/RRTM_DATA .
cp /emc2/wx20py/wrf_nmm/$exedir/test/nmm_real/tr* .
cp /emc2/wx20py/wrf_nmm/$exedir/test/nmm_real/eta* .


export INP=/emc2/wx22tb/input08
ln -s -f $INP/co2.60_hyb_bot40m fort.14

./real.exe

date > runcyc
