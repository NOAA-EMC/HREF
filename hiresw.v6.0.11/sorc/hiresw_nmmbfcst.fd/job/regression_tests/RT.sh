#!/bin/ksh
##########################################
# temporary fix for AIX
# ratko, May 6th, 2013
##########################################

. ./detect_machine.sh
export MACHINE_ID
export ACCNR

if [ $MACHINE_ID = ccs -o $MACHINE_ID = wcoss ]; then
 echo "#!/bin/ksh" > tmp.sh
 cat RT.sh_IN >> tmp.sh
 chmod 755 tmp.sh
 cp gfs_fcst_run.IN_IBM gfs_fcst_run.IN
else
 echo "#!/bin/ksh -l" > tmp.sh
 cat RT.sh_IN >> tmp.sh
 chmod 755 tmp.sh
 cp gfs_fcst_run.IN_Linux gfs_fcst_run.IN
fi

./tmp.sh $*

rm -f tmp.sh gfs_fcst_run.IN

exit
