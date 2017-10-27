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

base=/gpfs/hps3/emc/meso/noscrub/Matthew.Pyle/python_tools

cd $base

cyc=${1}
dom=${2}
core=${3}


cp /gpfs/hps/nco/ops/com/date/t${cyc}z .

PDY=`cat t${cyc}z | cut -c7-14`

echo PDY is $PDY

dir=/gpfs/hps3/ptmp/Matthew.Pyle/hiresw_python_${cyc}_${core}_${dom}

if [ -e ${dir}/ncepdate ]
then
PDYdir=`cat ${dir}/ncepdate | cut -c7-14`
echo defined PDYdir to be $PDYdir
else
PDYdir=99999999
fi

loop=1
looplim=80
while [ $PDY -ne $PDYdir -a $loop -lt $looplim ]
do
sleep 120
if [ -e ${dir}/ncepdate ]
then
PDYdir=`cat ${dir}/ncepdate | cut -c7-14`
fi
let loop=loop+1
echo "first loop incremented to $loop"
done

if [ $loop -ge $looplim -a $PDY -ne $PDYdir ]
then
echo "still wrong date in graphics directory - quit"
exit
fi

echo DATE agrees as $PDY

loop=1
while [ ! -e ${dir}/graphicsdone -a $loop -lt $looplim ]
do
sleep 100
let loop=loop+1
echo "second loop incremented to $loop"
done

if [ $loop -eq $looplim -a ! -e  ${dir}/graphicsdone ]
then
echo "never found graphics done file...so quitting"
exit
fi

bsub < ${base}/send_python_graphics_hiresw.sh_${cyc}_${dom}
