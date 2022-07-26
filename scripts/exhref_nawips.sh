#!/bin/ksh
###################################################################
echo "----------------------------------------------------"
echo "exnawips - convert NCEP GRIB files into GEMPAK Grids"
echo "----------------------------------------------------"
echo "History: Mar 2000 - First implementation of this new script."
#####################################################################

cd $DATA

set -xa

export TYPE=$1
export  'PS4=$TYPE:$SECONDS + '
mkdir -p $DATA/$TYPE
cd $DATA/$TYPE

msg="Begin job for $job"
postmsg "$jlogfile" "$msg"

cp $GEMPAKhref/fix/*.tbl .

#
NAGRIB_TABLE=$GEMPAKhref/fix/nagrib.tbl
NAGRIB=nagrib2
#

entry=`grep "^$RUN " $NAGRIB_TABLE | awk 'index($1,"#") != 1 {print $0}'`

if [ "$entry" != "" ] ; then
  cpyfil=`echo $entry  | awk 'BEGIN {FS="|"} {print $2}'`
  garea=`echo $entry   | awk 'BEGIN {FS="|"} {print $3}'`
  gbtbls=`echo $entry  | awk 'BEGIN {FS="|"} {print $4}'`
  maxgrd=`echo $entry  | awk 'BEGIN {FS="|"} {print $5}'`
  kxky=`echo $entry    | awk 'BEGIN {FS="|"} {print $6}'`
  grdarea=`echo $entry | awk 'BEGIN {FS="|"} {print $7}'`
  proj=`echo $entry    | awk 'BEGIN {FS="|"} {print $8}'`
  output=`echo $entry  | awk 'BEGIN {FS="|"} {print $9}'`
else
  cpyfil=gds
  garea=dset
  gbtbls=
  maxgrd=4999
  kxky=
  grdarea=
  proj=
  output=T
fi  
pdsext=no

maxtries=180
fhcnt=$fstart
while [ $fhcnt -le $fend ] ; do
  if [ $fhcnt -ge 100 ] ; then
    fhcnt=$(printf "%03d" $fhcnt)
  else
    fhcnt=$(printf "%02d" $fhcnt)
  fi
  fhr=$fhcnt

  fhr3=$fhcnt
  fhr3=$(printf "%03d" $fhr3)

  GRIBIN=$COMIN/${RUN}.t${cyc}z.${NEST}.${TYPE}.f${fhr}.grib2
  GEMGRD=${RUN}_${NEST}_${TYPE}_${PDY}${cyc}f${fhr3} 
  GRIBIN_chk=$COMIN/${RUN}.t${cyc}z.${NEST}.${TYPE}.f${fhr}.grib2.idx

  icnt=1
  while [ $icnt -lt 1000 ]
  do
    if [ -r $GRIBIN_chk ] ; then
      break
    else
      let "icnt=icnt+1"
      sleep 20
    fi
    if [ $icnt -ge $maxtries ]
    then
      msg="FATAL ERROR: ABORTING after 1 hour of waiting for F$fhr to end."
      err_exit $msg
    fi
  done

  cp $GRIBIN grib$fhr

  export pgm="nagrib2 F$fhr"

 startmsg

  $GEMEXE/$NAGRIB << EOF
   GBFILE   = grib$fhr
   INDXFL   =
   GDOUTF   = $GEMGRD
   PROJ     = $proj
   GRDAREA  = $grdarea
   KXKY     = $kxky
   MAXGRD   = $maxgrd
   CPYFIL   = $cpyfil
   GAREA    = $garea
   OUTPUT   = $output
   GBTBLS   = $gbtbls
   GBDIAG   =
   PDSEXT   = $pdsext
  l
  r
EOF

  export err=$?;err_chk

  #####################################################
  # GEMPAK DOES NOT ALWAYS HAVE A NON ZERO RETURN CODE
  # WHEN IT CAN NOT PRODUCE THE DESIRED GRID.  CHECK
  # FOR THIS CASE HERE.
  #####################################################

  if [ "$NAGRIB" = "nagrib2" ] ; then
    gpend
  fi

  if [ $SENDCOM = "YES" ] ; then
    cp $GEMGRD $COMOUT/.${GEMGRD}
    export err=$?;err_chk
    mv $COMOUT/.${GEMGRD} $COMOUT/${GEMGRD}

    if [ $SENDDBN = "YES" ] ; then
         $DBNROOT/bin/dbn_alert MODEL ${DBN_ALERT_TYPE} $job \
           $COMOUT/$GEMGRD
    else
    echo "##### DBN_ALERT_TYPE is: ${DBN_ALERT_TYPE} #####"
    fi
  fi
  let fhcnt=fhcnt+finc
done # while fhcnt -lt fend


#####################################################################
# GOOD RUN
set +x
echo "**************JOB HREF NAWIPS COMPLETED NORMALLY "
echo "**************JOB HREF NAWIPS COMPLETED NORMALLY "
echo "**************JOB HREF NAWIPS COMPLETED NORMALLY "
set -x
#####################################################################

msg='Job completed normally.'
echo $msg
postmsg "$jlogfile" "$msg"

############################### END OF SCRIPT #######################
