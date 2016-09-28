# @ step_name = run_wps_ungrib
# @ output = ungrib.log
# @ error = ungrib.log
# @ notification = never
# @ wall_clock_limit = 00:12:00
# @ job_type = serial
## @ total_tasks = 1
## @ blocking=UNLIMITED
# @ resources=ConsumableCPUS(1)ConsumableMemory(1 GB)
# @ class=dev
# @ group=devonprod
## @ node_usage=shared
# @ account_no=HRW-T2O
## @ network.MPI = csss,shared,us
# @ queue


DATE=${1}
CYC=${2}
flen=${3}
GRIB=${4}

PACKDIR=/ptmp/wx20py/NMMB_init
WORKDIR=/stmp/wx20py/nmmb_init

mkdir -p $WORKDIR/ungrib
cd $WORKDIR/ungrib
rm $WORKDIR/ungrib/*
rm $WORKDIR/*


######################################################

cp $PACKDIR/namelist.nps .

echo PASSED IN GRIB AS $GRIB

if [ $GRIB == "GFS" ]
then
### GFS block
mod=gfs
type=pgrb2f
suf=" "

cp $PACKDIR/NPS/ungrib/Variable_Tables/Vtable.GFS Vtable

elif [ $GRIB == "NAM" ]
then
### NAM block
mod=nam
type=awip32
suf=".tm00"
cp $PACKDIR/NPS/ungrib/Variable_Tables/Vtable.NAM Vtable

elif [ $GRIB == "RUC" ]
then
### RUC block
mod=ruc2
type=pgrb20f
suf=" "

cp $PACKDIR/NPS/ungrib/Variable_Tables/Vtable.RUCp Vtable

else
echo "GRIB is $GRIB and quitting"
fi

####################################

cp $PACKDIR/NPS/ungrib.exe .

echo DATE $DATE
echo CYC $CYC

rm GRIBFILE.*

if [ $GRIB == "RUC" ]
then
filedir=/com/ruc/prod/ruc2a.${DATE}
else
filedir=/com/${mod}/prod/${mod}.${DATE}
fi


if [ $GRIB == "RUC" ]
then
ln -sf $filedir/${mod}.t${CYC}z.pgrb20anl GRIBFILE.AAA
else
ln -sf $filedir/${mod}.t${CYC}z.${type}00${suf} GRIBFILE.AAA
fi

ln -sf $filedir/${mod}.t${CYC}z.${type}03${suf} GRIBFILE.AAB
ln -sf $filedir/${mod}.t${CYC}z.${type}06${suf} GRIBFILE.AAC
ln -sf $filedir/${mod}.t${CYC}z.${type}09${suf} GRIBFILE.AAD
ln -sf $filedir/${mod}.t${CYC}z.${type}12${suf} GRIBFILE.AAE
ln -sf $filedir/${mod}.t${CYC}z.${type}15${suf} GRIBFILE.AAF

if [ $GRIB != "RUC" ]
then
ln -sf $filedir/${mod}.t${CYC}z.${type}18${suf} GRIBFILE.AAG
ln -sf $filedir/${mod}.t${CYC}z.${type}21${suf} GRIBFILE.AAH
ln -sf $filedir/${mod}.t${CYC}z.${type}24${suf} GRIBFILE.AAI


echo flen is $flen

if [ $flen -ge 27 ] 
then
ln -sf $filedir/${mod}.t${CYC}z.${type}27${suf} GRIBFILE.AAJ
fi

if [ $flen -ge 30 ] 
then
ln -sf $filedir/${mod}.t${CYC}z.${type}30${suf} GRIBFILE.AAK
fi

if [ $flen -ge 33 ] 
then
ln -sf $filedir/${mod}.t${CYC}z.${type}33${suf} GRIBFILE.AAL
fi

if [ $flen -ge 36 ] 
then
ln -sf $filedir/${mod}.t${CYC}z.${type}36${suf} GRIBFILE.AAM
fi

if [ $flen -ge 39 ] 
then
ln -sf $filedir/${mod}.t${CYC}z.${type}39${suf} GRIBFILE.AAN
fi

if [ $flen -ge 42 ] 
then
ln -sf $filedir/${mod}.t${CYC}z.${type}42${suf} GRIBFILE.AAO
fi

if [ $flen -ge 45 ] 
then
ln -sf $filedir/${mod}.t${CYC}z.${type}45${suf} GRIBFILE.AAP
fi

if [ $flen -ge 48 ] 
then
echo LINKED 48
ln -sf $filedir/${mod}.t${CYC}z.${type}48${suf} GRIBFILE.AAQ
fi

if [ $flen -ge 51 ] 
then
echo LINKED 51
ln -sf $filedir/${mod}.t${CYC}z.${type}51${suf} GRIBFILE.AAR
fi

if [ $flen -ge 54 ] 
then
ln -sf $filedir/${mod}.t${CYC}z.${type}54${suf} GRIBFILE.AAS
fi

if [ $flen -ge 57 ] 
then
ln -sf $filedir/${mod}.t${CYC}z.${type}57${suf} GRIBFILE.AAT
fi

if [ $flen -ge 60 ] 
then
ln -sf $filedir/${mod}.t${CYC}z.${type}60${suf} GRIBFILE.AAU
fi

if [ $flen -ge 63 ] 
then
ln -sf $filedir/${mod}.t${CYC}z.${type}63${suf} GRIBFILE.AAV
fi

if [ $flen -ge 66 ] 
then
ln -sf $filedir/${mod}.t${CYC}z.${type}66${suf} GRIBFILE.AAW
fi

if [ $flen -ge 69 ] 
then
ln -sf $filedir/${mod}.t${CYC}z.${type}69${suf} GRIBFILE.AAX
fi

if [ $flen -ge 72 ] 
then
ln -sf $filedir/${mod}.t${CYC}z.${type}72${suf} GRIBFILE.AAY
fi

if [ $flen -ge 75 ] 
then
ln -sf $filedir/${mod}.t${CYC}z.${type}75${suf} GRIBFILE.AAZ
fi

if [ $flen -ge 78 ] 
then
ln -sf $filedir/${mod}.t${CYC}z.${type}78${suf} GRIBFILE.ABA
fi

if [ $flen -ge 81 ] 
then
ln -sf $filedir/${mod}.t${CYC}z.${type}81${suf} GRIBFILE.ABB
fi

if [ $flen -ge 84 ] 
then
ln -sf $filedir/${mod}.t${CYC}z.${type}84${suf} GRIBFILE.ABC
fi

if [ $flen -ge 87 ] 
then
echo NEED TO LINK MORE GRIB FILES IN run_ungrib.ll
exit 9
fi

fi


diff $PACKDIR/NPS/ungrib/Variable_Tables/Vtable.GFS $PACKDIR/NPS/ungrib/Variable_Tables/Vtable.GFS_spectral > /dev/null 2>&1
is_spectral=$?
echo is_spectral is $is_spectral

if [ $is_spectral = 0 ] 
then
ln -sf /com/gfs/prod/gfs.${DATE}/gfs.t${CYC}z.sf00 fort.27
ln -sf /com/gfs/prod/gfs.${DATE}/gfs.t${CYC}z.sf03 fort.28
ln -sf /com/gfs/prod/gfs.${DATE}/gfs.t${CYC}z.sf06 fort.29
ln -sf /com/gfs/prod/gfs.${DATE}/gfs.t${CYC}z.sf09 fort.30
ln -sf /com/gfs/prod/gfs.${DATE}/gfs.t${CYC}z.sf12 fort.31
ln -sf /com/gfs/prod/gfs.${DATE}/gfs.t${CYC}z.sf15 fort.32
ln -sf /com/gfs/prod/gfs.${DATE}/gfs.t${CYC}z.sf18 fort.33
ln -sf /com/gfs/prod/gfs.${DATE}/gfs.t${CYC}z.sf21 fort.34
ln -sf /com/gfs/prod/gfs.${DATE}/gfs.t${CYC}z.sf24 fort.35
ln -sf /com/gfs/prod/gfs.${DATE}/gfs.t${CYC}z.sf27 fort.36
ln -sf /com/gfs/prod/gfs.${DATE}/gfs.t${CYC}z.sf30 fort.37
ln -sf /com/gfs/prod/gfs.${DATE}/gfs.t${CYC}z.sf33 fort.38
ln -sf /com/gfs/prod/gfs.${DATE}/gfs.t${CYC}z.sf36 fort.39
ln -sf /com/gfs/prod/gfs.${DATE}/gfs.t${CYC}z.sf39 fort.40
ln -sf /com/gfs/prod/gfs.${DATE}/gfs.t${CYC}z.sf42 fort.41
ln -sf /com/gfs/prod/gfs.${DATE}/gfs.t${CYC}z.sf45 fort.42
ln -sf /com/gfs/prod/gfs.${DATE}/gfs.t${CYC}z.sf48 fort.43
ln -sf /com/gfs/prod/gfs.${DATE}/gfs.t${CYC}z.sf51 fort.44
ln -sf /com/gfs/prod/gfs.${DATE}/gfs.t${CYC}z.sf54 fort.45
ln -sf /com/gfs/prod/gfs.${DATE}/gfs.t${CYC}z.sf57 fort.46
ln -sf /com/gfs/prod/gfs.${DATE}/gfs.t${CYC}z.sf60 fort.47
ln -sf /com/gfs/prod/gfs.${DATE}/gfs.t${CYC}z.sf63 fort.48
ln -sf /com/gfs/prod/gfs.${DATE}/gfs.t${CYC}z.sf66 fort.49
ln -sf /com/gfs/prod/gfs.${DATE}/gfs.t${CYC}z.sf69 fort.50
ln -sf /com/gfs/prod/gfs.${DATE}/gfs.t${CYC}z.sf72 fort.51
ln -sf /com/gfs/prod/gfs.${DATE}/gfs.t${CYC}z.sf75 fort.52
ln -sf /com/gfs/prod/gfs.${DATE}/gfs.t${CYC}z.sf78 fort.53
ln -sf /com/gfs/prod/gfs.${DATE}/gfs.t${CYC}z.sf81 fort.54
ln -sf /com/gfs/prod/gfs.${DATE}/gfs.t${CYC}z.sf84 fort.55
fi

./ungrib.exe

cp FILE* ../
