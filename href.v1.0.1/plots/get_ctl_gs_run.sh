#!/bin/ksh

. /u/Binbin.Zhou/.kshrc
#TODAY=`date +%Y%m%d`
#Y=`date +%Y`
#M=`date +%b`
#D=`date +%d`
#T='12'

T=$1           #Cycle
TODAY=$2

Y=`echo ${TODAY} | cut -c 1-4`
mm=`echo ${TODAY} | cut -c 5-6`
D=`echo ${TODAY} | cut -c 7-8`
if [ $mm = '01' ] ; then
 M='Jan'
elif [ $mm = '02' ] ; then
 M='Feb'
elif [ $mm = '03' ] ; then
 M='Mar'
elif [ $mm = '04' ] ; then
 M='Apr'
elif [ $mm = '05' ] ; then
 M='May'
elif [ $mm = '06' ] ; then
 M='Jun'
elif [ $mm = '07' ] ; then
 M='Jul'
elif [ $mm = '08' ] ; then
 M='Aug'
elif [ $mm = '09' ] ; then
 M='Sep'
elif [ $mm = '10' ] ; then
 M='Oct'
elif [ $mm = '11' ] ; then
 M='Nov'
elif [ $mm = '12' ] ; then
 M='Dec'
else
 echo 'Wrong Month'
exit
fi



#TODAY='20131019'
#Y='2013'
#M='Oct'
#D='19'

s=/u/Binbin.Zhou/work/HREF/plots
ptmp=/ptmpp1/Binbin.Zhou/href/plots.${TODAY}${T}
mkdir -p $ptmp
cd $ptmp

rm -f $ptmp/*.gif

$s/get_all_ens.sh $TODAY $T
cp $s/cbar.gs $ptmp/.

sed -e "s!TODAY!$TODAY!g" -e "s!MMM!$M!g" -e "s!DD!$D!g"  -e "s!TIME!$T!g"  -e "s!YYYY!$Y!"  $s/get_mean.ctl.base > $ptmp/get_mean.ctl
sed -e "s!TODAY!$TODAY!g" -e "s!MMM!$M!g" -e "s!DD!$D!g"  -e "s!TIME!$T!g"  -e "s!YYYY!$Y!"  $s/get_spread.ctl.base > $ptmp/get_spread.ctl
sed -e "s!TODAY!$TODAY!g" -e "s!MMM!$M!g" -e "s!DD!$D!g"  -e "s!TIME!$T!g"  -e "s!YYYY!$Y!"  $s/get_prob.ctl.base > $ptmp/get_prob.ctl
/usrx/local/GrADS/2.0.2/bin/gribmap -i $ptmp/get_mean.ctl
/usrx/local/GrADS/2.0.2/bin/gribmap -i $ptmp/get_spread.ctl
/usrx/local/GrADS/2.0.2/bin/gribmap -i $ptmp/get_prob.ctl


lat[0]="lat 20 54"
lat[1]="lat 36 50"
lat[2]="lat 20 38"
lat[3]="lat 36 50"
lat[4]="lat 25 40"
lat[5]="lat 38 50"
lat[6]="lat 30 42"

lon[0]="lon 226 295"
lon[1]="lon 275 295"
lon[2]="lon 270 290"
lon[3]="lon 255 275"
lon[4]="lon 255 275"
lon[5]="lon 235 255"
lon[6]="lon 235 255"

reg[0]=us
reg[1]=ne
reg[2]=se
reg[3]=mw
reg[4]=ms
reg[5]=nw
reg[6]=sw

 sed -e "s!PTMP!${ptmp}!g" $s/poescript_gs.sh.base > $ptmp/poescript_gs.${T}.sh
 sed -e "s!MBR!7!g" -e "s!TODAY!$TODAY!g" -e "s!CYC!$T!g" $s/run_gs_poe.sh.base > $ptmp/run_gs_poe.sh
chmod +x $ptmp/poescript_gs.${T}.sh
chmod +x $ptmp/run_gs_poe.sh

#for i in 0 1 2 3 4 5 6 ; do
for i in 0 ; do

if [ $i -eq 0 ] || [ $i -eq 1 ] || [ $i -eq 3 ] || [ $i -eq 5 ] || [ $i -eq 6 ] ; then 
  sed -e "s!string 1.5!string 5!g" -e "s!LATI!${lat[$i]}!g" -e "s!LONG!${lon[$i]}!g" -e "s!REG!${reg[$i]}!g" -e "s!TODAY!$TODAY!g" -e "s!MMM!$M!g" -e "s!DD!$D!g" -e "s!TIME!$T!g" -e "s!YYYY!$Y!g" $s/get_mean_prob.gs.base > $ptmp/get_mean_prob.${reg[$i]}.gs
else
   sed -e "s!LATI!${lat[$i]}!g" -e "s!LONG!${lon[$i]}!g" -e "s!REG!${reg[$i]}!g" -e "s!TODAY!$TODAY!g" -e "s!MMM!$M!g" -e "s!DD!$D!g" -e "s!TIME!$T!g" -e "s!YYYY!$Y!g" $s/get_mean_prob.gs.base > $ptmp/get_mean_prob.${reg[$i]}.gs
fi

  chmod +x $ptmp/get_mean_prob.${reg[$i]}.gs
  #$ptmp/get_mean_prob.${reg[$i]}.gs

done
#bsub < $ptmp/run_gs_poe.sh
$ptmp/get_mean_prob.us.gs


wait_time=0
until [ -s $ptmp/visb.t${T}z.36.us.gif ] ; do
echo "waiting  run_gs_poe.sh to run  for 60 sec ... "
sleep 60;
wait_time=`expr $wait_time + 60`
if [ $wait_time -gt 2400 ] ; then
echo "waiting  run_gs_poe.sh timeout, exit"
exit
fi

done
echo 'run_gs_poe.sh begins, waiting another 120 sec....'
sleep 120;


#for i in 0 1 2 3 4 5 6 ; do
for i in 0 ; do

sftp  wd20bz@emcrzdm << EOF
prompt

mkdir /home/www/emc/htdocs/mmb/SREF_avia/FCST/HREF/$TODAY
cd /home/www/emc/htdocs/mmb/SREF_avia/FCST/HREF/$TODAY

lcd $ptmp
mput *${reg[$i]}.gif
EOF

done

