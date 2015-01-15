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


################
$USHhiresw/get_copygb_poe_bsub.sh.new $cyc $TODAY
################

wait_time=0

until [ -s $DATA/out.poe_01 ] && [ -s $DATA/out.poe_02 ] && [ -s $DATA/out.poe_03 ] && \
[ -s $DATA/out.poe_04 ] && [ -s $DATA/out.poe_05 ] && [ -s $DATA/out.poe_06 ] && \
[ -s $DATA/out.poe_07 ] && [ -s $DATA/out.poe_08 ] && [ -s $DATA/out.poe_09 ] && \
[ -s $DATA/out.poe_10 ] && [ -s $DATA/out.poe_11 ] && [ -s $DATA/out.poe_12 ] || [ $wait_time -eq 3300 ] ; do

echo "waiting copygb out.poe for 60 sec "
sleep 60;
wait_time=`expr $wait_time + 60`
if [ $wait_time -gt 3600 ] ; then
exit
fi

done

exit

################
$USHhiresw/get_all_prcp_bsub.sh.new $cyc $TODAY
################

wait_time=0
until [ -s $DATA/out.prcp ] ; do
echo "waiting  out.prcp for 60sec " 
sleep 60;
wait_time=`expr $wait_time + 60`
if [ $wait_time -gt 1800 ] ; then
exit
fi

done 
echo 'Begin get_all_prcp_bsub.sh, waiting another 900 sec ...'
sleep 900; 

################
$USHhiresw/get_ensprod_bsub.sh.new $cyc $TODAY
################



#what is special about this item here to wait for it?

wait_time=0
until [ -s $DATA/09/nsse.prob.t${cyc}z.f09 ] ; do 
echo "waiting  $DATA/09/nsse.prob.t${cyc}z.f09 "
sleep 60;
wait_time=`expr $wait_time + 60`
if [ $wait_time -gt 1800 ] ; then
exit
fi

done


echo 'get_ensprod_bsub.sh begin to run, waiting another 300 sec....'
sleep 600

echo "DONE ALL JOBS!!!!!!!!!!!!!!!!!!!!!!!" 

exit





