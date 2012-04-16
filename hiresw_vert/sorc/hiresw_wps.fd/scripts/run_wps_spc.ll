#! /bin/ksh

CYC=${1}

cd /meso/save/wx20py/WPS+WRFV2/WPS

### modify namelist file
ystart=`cat /meso/scrub/wx20py/date/t${CYC}z | cut -c7-10`
mstart=`cat /meso/scrub/wx20py/date/t${CYC}z | cut -c11-12`
dstart=`cat /meso/scrub/wx20py/date/t${CYC}z | cut -c13-14`
hstart=$CYC

start=$ystart$mstart$dstart${CYC}

end=`/nwprod/util/exec/ndate +36 $start`

yend=`echo $end | cut -c1-4`
mend=`echo $end | cut -c5-6`
dend=`echo $end | cut -c7-8`
hend=`echo $end | cut -c9-10`

cat namelist.wps_4km_spc_in | sed s:YSTART:$ystart: | sed s:MSTART:$mstart: \
 | sed s:DSTART:$dstart: | sed s:HSTART:$CYC: | sed s:YEND:$yend: \
 | sed s:MEND:$mend:     | sed s:DEND:$dend: | sed s:HEND:$hend: > namelist.wps


./run_ungrib.ll_spc 

llsubmit run_met.ll


