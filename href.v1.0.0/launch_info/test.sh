export RUN_ENVIR=test
export envir=test
export SUB_ENVIR=dev2
export cyc=$1

export DATAROOT=/stmpd3/Matthew.Pyle/tmpnwprd

export MYCOMROOT=/ptmpd3/Matthew.Pyle/com/
export COMINhiresw=/com/hiresw/prod/hiresw
export COMINnam=/com/nam/prod/nam

href_ver=`cat /u/$LOGNAME/href_version`

export HOMEhref=/meso/save/$LOGNAME/href.${href_ver}

${HOMEhref}/jobs/JHREF_RUNALL.ecf
