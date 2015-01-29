#!/bin/ksh

TODAY=`echo $PDY | cut -c1-8`

export USHhiresw
export FIXhiresw  # needed?

################
$USHhiresw/retrieve_2d_3d_hrrr.sh.new $cyc
################

wait_time=0
until [ $DATA/${TODAY}/Retriev_2d_3d_hrrr_done.t${cyc}z ] ; do
echo "has waited $wait_time, waiting Retriev_2d_3d_hrrr_done.t${cyc}z for another 60 sec..."
sleep 60
wait_time=`expr $wait_time + 60`
if [ $wait_time -gt 2700 ] ; then
exit
fi
done

echo to here


exit





