dom=${1}
core=${2}
cyc=${3}


DATE=`cat /com/date/t${cyc}z | cut -c7-14`

DATECYC=${DATE}${cyc}

ayer=`ndate -24 $DATECYC`
echo $ayer

cat <<EOF > stnmlist_input
1
/com/hiresw/prod/hiresw.${DATE}/${dom}${core}.t${cyc}z.class1.bufr
./bufr.${cyc}/${dom}${core}.bufr
EOF

mkdir -p ./bufr.${cyc}

export pgm=hiresw_stnmlist
export FORT20=/com/hiresw/prod/hiresw.${DATE}/${dom}${core}.t${cyc}z.class1.bufr
export DIRD=./bufr.${cyc}/${dom}${core}.bufr


EXEChiresw=/meso/save/Matthew.Pyle/hiresw.v6.0.10/exec

$EXEChiresw/hiresw_stnmlist < stnmlist_input >> pgmout 2>errfile

cd ./bufr.${cyc}

ssh -l mpyle emcrzdm.ncep.noaa.gov "rm /home/www/emc/htdocs/mmb/mpyle/hiresw/bufr/${dom}${core}.bufr.*.${ayer}"

scp ${dom}${core}.bufr.*.${DATE}${cyc} mpyle@emcrzdm.ncep.noaa.gov:/home/www/emc/htdocs/mmb/mpyle/hiresw/bufr/
