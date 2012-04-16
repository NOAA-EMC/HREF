# @ step_name = run_wps_ungrib
# @ output = ungrib.log
# @ error = ungrib.log
# @ notification = never
# @ wall_clock_limit = 00:12:00
# @ arguments = 20070312 00
# @ job_type = parallel
# @ total_tasks = 1
### @ blocking=UNLIMITED
# @ resources=ConsumableCPUS(1)ConsumableMemory(1 GB)
# @ class=dev
# @ group=meso
# @ node_usage=shared
# @ account_no=HRW-T2O
# @ network.MPI = csss,shared,us
# @ queue


DATE=${1}
CYC=${2}

rm Vtable

######################################################

### GFS block
# mod=gfs
# type=pgrbf
# suf=" "
# cp ungrib/Variable_Tables/Vtable.GFS Vtable

### NAM block
mod=nam
type=awip32
suf=".tm00"
cp ungrib/Variable_Tables/Vtable.NAM Vtable

######################################################

# cd /nbns/meso/wx20py/WPS+WRFV2/WPS
cd /meso/save/wx20py/WPS+WRFV2/WPS

mkdir -p /meso/scrub/wx20py/run_ungrib

rm /meso/scrub/wx20py/run_ungrib/*

cp Vtable namelist.wps /meso/scrub/wx20py/run_ungrib/
cp ungrib.exe /meso/scrub/wx20py/run_ungrib/

cd /meso/scrub/wx20py/run_ungrib/

rm GRIBFILE.*

filedir=/com/${mod}/prod/${mod}.${DATE}

### filedir=/meso/scrub/wx20py/20060801_oper_gfs

# filedir=/meso/scrub/wx20py/nam_20070110_00
# filedir=/meso/scrub/wx20py/ta_20050123

ln -sf $filedir/${mod}.t${CYC}z.${type}00${suf} GRIBFILE.AAA
ln -sf $filedir/${mod}.t${CYC}z.${type}03${suf} GRIBFILE.AAB
ln -sf $filedir/${mod}.t${CYC}z.${type}06${suf} GRIBFILE.AAC
ln -sf $filedir/${mod}.t${CYC}z.${type}09${suf} GRIBFILE.AAD
ln -sf $filedir/${mod}.t${CYC}z.${type}12${suf} GRIBFILE.AAE
ln -sf $filedir/${mod}.t${CYC}z.${type}15${suf} GRIBFILE.AAF
ln -sf $filedir/${mod}.t${CYC}z.${type}18${suf} GRIBFILE.AAG
ln -sf $filedir/${mod}.t${CYC}z.${type}21${suf} GRIBFILE.AAH
ln -sf $filedir/${mod}.t${CYC}z.${type}24${suf} GRIBFILE.AAI
ln -sf $filedir/${mod}.t${CYC}z.${type}27${suf} GRIBFILE.AAJ
ln -sf $filedir/${mod}.t${CYC}z.${type}30${suf} GRIBFILE.AAK
ln -sf $filedir/${mod}.t${CYC}z.${type}33${suf} GRIBFILE.AAL
ln -sf $filedir/${mod}.t${CYC}z.${type}36${suf} GRIBFILE.AAM
ln -sf $filedir/${mod}.t${CYC}z.${type}39${suf} GRIBFILE.AAN
ln -sf $filedir/${mod}.t${CYC}z.${type}42${suf} GRIBFILE.AAO
ln -sf $filedir/${mod}.t${CYC}z.${type}45${suf} GRIBFILE.AAP
ln -sf $filedir/${mod}.t${CYC}z.${type}48${suf} GRIBFILE.AAQ
ln -sf $filedir/${mod}.t${CYC}z.${type}51${suf} GRIBFILE.AAR
ln -sf $filedir/${mod}.t${CYC}z.${type}54${suf} GRIBFILE.AAS
ln -sf $filedir/${mod}.t${CYC}z.${type}57${suf} GRIBFILE.AAT
ln -sf $filedir/${mod}.t${CYC}z.${type}60${suf} GRIBFILE.AAU
ln -sf $filedir/${mod}.t${CYC}z.${type}63${suf} GRIBFILE.AAV
ln -sf $filedir/${mod}.t${CYC}z.${type}66${suf} GRIBFILE.AAW
ln -sf $filedir/${mod}.t${CYC}z.${type}69${suf} GRIBFILE.AAX
ln -sf $filedir/${mod}.t${CYC}z.${type}72${suf} GRIBFILE.AAY

./ungrib.exe
