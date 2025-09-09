#! /bin/ksh
#####################################################
#

#  Script: preprocess_rrfs_3hapcp.sh.ecf
#
# Purpose: Generates 3 h QPF buckets from the FV3
#
#  Author: Matthew Pyle
#          April 2021
#
#  05/01/2023, Jun Du -- added a timelag option ($type)
#  06/12/2025, M Pyle -- updated to hour by hour option (stream --> fhr)
#
####################################################


set -x 

if [ $# -ne 6 ]
then
echo need 6 inputs: dom, day, cyc, mem, and file name, fhr
exit
fi

dom=${1}
day=${2}
cyc=${3}
mem=${4}
name=${5}
fhr=${6}


if [ $dom = 'conus' ]
then
dim1=1799
dim2=1059
elif [ $dom = 'ak' ]
then
dim1=1649
dim2=1105
elif [ $region = 'hi' ]
then
dim1=321
dim2=225
elif [ $region = 'pr' ]
then
dim1=544
dim2=310
fi

let "name1 = $name + 01"
echo $name1
if [ $name1 -lt 10 ]; then
 name1=0$name1
else
 name1=$name1
fi

cd $DATA

mkdir -p $DATA/pcp_${name1}

cd $DATA/pcp_${name1}

EXECrefs=${HOMErefs}/exec

hrs=$fhr

for hr in $hrs

do
filein=../temp.t${cyc}z.m${mem}.f${hr}.grib2
fileout=../fv3s.t${cyc}z.${dom}.m${mem}.f${hr}.grib2

if [ -s $filein ]
then
sleep 1
ln -sf $filein rrfs.t${cyc}z.f${hr}.grib2
fi
done

for hr in $hrs
do

let old3=hr-3
let old2=hr-2
let old1=hr-1

hrold3=$(printf %2.2i $old3)
hrold2=$(printf %2.2i $old2)
hrold1=$(printf %2.2i $old1)

if [ -e $filein ]
then

        if [ $hr -gt 0 ]
        then
        echo here a $hr

        if [ $hr%3 -eq 0 ]
        then

        ln -sf ../temp.t${cyc}z.m${mem}.f${hrold3}.grib2 rrfs.t${cyc}z.f${hrold3}.grib2
        ln -sf ../temp.t${cyc}z.m${mem}.f${hrold2}.grib2 rrfs.t${cyc}z.f${hrold2}.grib2
	ln -sf ../temp.t${cyc}z.m${mem}.f${hrold1}.grib2 rrfs.t${cyc}z.f${hrold1}.grib2

## do 3 h QPF from hireswfv3_bucket

  curpath=`pwd`
	
  echo "${curpath}" > input.card.${mem}.${hr}
  echo "rrfs.t${cyc}z.f" >> input.card.${mem}.${hr}
  echo $hrold3 >> input.card.${mem}.${hr}
  echo $hr >> input.card.${mem}.${hr}

if [ $hr = '03' ]
then
# just take later period if f03
  echo 1 >> input.card.${mem}.${hr}
else
  echo 0 >> input.card.${mem}.${hr}
fi

  echo "$dim1 $dim2" >> input.card.${mem}.${hr}

if [ ${mem} = '01' ]
then     
JPDTN_USE=8
else
JPDTN_USE=11
fi

  echo $JPDTN_USE >> input.card.${mem}.${hr}


 $EXECrefs/enspost_3hqpf < input.card.${mem}.${hr}
 export err=$? ; err_chk

 cat ./PCP3HR${hr}.tm00 >> ../fv3s.t${cyc}z.${dom}.m${mem}.f${hr}.grib2
 cp PCP3HR${hr}.tm00 PCP3HR${hr}.tm00_qpf

  fi
else
  echo not a three hour time $hr
  fi


else
        msg="FATAL ERROR: 3hr $filein missing"
        err_exit $msg
fi

done

cd ../


pwd

echo hrs down here is $hrs

for hr in $hrs
do
cp fv3s.t${cyc}z.${dom}.m${mem}.f${hr}.grib2 ${GESOUT}.${day}/fv3s.t${cyc}z.${dom}.m${name1}.f${hr}.grib2

 err=$?
 export err # ; err_chk
done
