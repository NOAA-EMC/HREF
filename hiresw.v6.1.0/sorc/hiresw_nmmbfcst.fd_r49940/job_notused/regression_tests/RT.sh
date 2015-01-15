#!/bin/ksh
##########################################
# temporary fix for WCOSS
# ratko, May 6th, 2013
##########################################

#export ACCNR=null

#- good choice for NEMS people on Zeus:
export ACCNR=nems

#- good choice for EMC people on Gaea:
#export ACCNR=cmp

#- good choice for ESRL people on Gaea:
#export ACCNR=esrl_bmcs

. ./detect_machine.sh
export MACHINE_ID

if [ $MACHINE_ID = wcoss ]; then
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
