#! /bin/sh

cd /u/$USER    # cron does this for us - this is here just to be safe
. /etc/profile

if [ -a .profile ]; then
   . ./.profile
fi

if [ -a .bashrc ]; then
   . ./.bashrc
fi

module load -a /gpfs/hps/nco/ops/nwprod/modulefiles
module load prod_envir/1.0.0
module load prod_util

dom=${1}
core=${2}
cyc=${3}

DATE=`cat $COMROOT/date/t${cyc}z | cut -c7-14`

echo DATE $DATE

base=/gpfs/hps3/emc/meso/noscrub/Matthew.Pyle/python_tools

cd $base


looplim=180
sleeptim=60

# make sure OPS output exists

if [ $core = "arw" ]
then

loop=1
while [ $loop -lt $looplim -a ! -e $COMROOT/hiresw/prod/hiresw.${DATE}/hiresw.t${cyc}z.arw_5km.f48.${dom}.grib2 ]
do
sleep $sleeptim
echo sleepa with loop $loop
let loop=loop+1

 if [ $loop -eq $looplim -a ! -e $COMROOT/hiresw/prod/hiresw.${DATE}/hiresw.t${cyc}z.arw_5km.f48.${dom}.grib2 ]
 then
 echo hit limit for OPS ARW and lacking f48 GRIB so quit
 exit
 fi

done
echo OPS ARW exists

loop=1
while [ $loop -lt $looplim -a ! -e /gpfs/hps2/ptmp/Matthew.Pyle/com/hiresw/test/hiresw.${DATE}/hiresw.t${cyc}z.arw_5km.f48.${dom}.grib2 ]
do
sleep $sleeptim
echo sleepd with loop $loop
let loop=loop+1
 if [ $loop -eq $looplim -a ! -e  /gpfs/hps2/ptmp/Matthew.Pyle/com/hiresw/test/hiresw.${DATE}/hiresw.t${cyc}z.arw_5km.f48.${dom}.grib2 ]
 then
 echo hit limit for PARA ARW and lacking f48 GRIB so quit
 exit
 fi
done
echo PARA ARW exists

if [ $dom != "guam" ]
then

loop=1
while [ $loop -lt $looplim -a ! -e /gpfs/hps2/ptmp/Matthew.Pyle/com/hiresw/test/hiresw.${DATE}/hiresw.t${cyc}z.arw_5km.f48.${dom}mem2.grib2 ]
do
sleep $sleeptim
echo sleepe with loop $loop
let loop=loop+1
 if [ $loop -eq $looplim -a ! -e  /gpfs/hps2/ptmp/Matthew.Pyle/com/hiresw/test/hiresw.${DATE}/hiresw.t${cyc}z.arw_5km.f48.${dom}mem2.grib2 ]
 then
 echo hit limit for PARA ARW MEM2 and lacking f48 GRIB so quit
 exit
 fi
done

echo PARA ARW mem2 exists 

fi



elif [ $core = "nmmb" ]
then

loop=1
while [ $loop -lt $looplim -a ! -e $COMROOT/hiresw/prod/hiresw.${DATE}/hiresw.t${cyc}z.nmmb_5km.f48.${dom}.grib2 ]
do
sleep $sleeptim
echo sleepb with loop $loop
let loop=loop+1
 if [ $loop -eq $looplim -a ! -e $COMROOT/hiresw/prod/hiresw.${DATE}/hiresw.t${cyc}z.nmmb_5km.f48.${dom}.grib2 ]
 then
 echo hit limit for OPS NMMB and lacking f48 GRIB so quit
 exit
 fi
done
echo OPS NMMB exists

# make sure PARA output exists
loop=1
while [ $loop -lt $looplim -a ! -e /gpfs/hps2/ptmp/Matthew.Pyle/com/hiresw/test/hiresw.${DATE}/hiresw.t${cyc}z.nmmb_5km.f48.${dom}.grib2 ]
do
sleep $sleeptim
echo sleepc with loop $loop
let loop=loop+1
 if [ $loop -eq $looplim -a ! -e  /gpfs/hps2/ptmp/Matthew.Pyle/com/hiresw/test/hiresw.${DATE}/hiresw.t${cyc}z.nmmb_5km.f48.${dom}.grib2 ]
 then
 echo hit limit for PARA NMMB and lacking f48 GRIB so quit
 exit
 fi
done
echo PARA NMMB exists

fi


bsub < ${base}/all_hiresw_${cyc}.sh_para_${core}_${dom}
