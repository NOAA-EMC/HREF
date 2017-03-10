#!/bin/sh

##############################################################
#
# hiresw_chkhur.sh
#
# This Script checks the number of storms being run by the GFDL
# hurricane model.  If the number is >4, all of the sms jobs for
# the current threat cycle are set to complete except for guam.
#
# 7/26/04: E. Rogers modified for WRF HIRESW ensemble ; if one
# storm running cancel WRF-ARW run, if 2 storms running cancel both
#
# 3/16/11: L. Lin modified for 2011 HIRESW upgrade.  Several jobs
# have been added according to its domain and model.  So setting
# jobs to completion has to include those new jobs.
#
##############################################################

set -x

MODEL=$1

hsrun=`echo $NEST | cut -c1-2`
if [ "$hsrun" = "hs" ]
then
   exit
fi


prodcyc=$cyc

stormdate=`cat ${COMINhur}/stormdate`
nstms=`cat ${COMINhur}/nstorms`
PDY2=`echo $PDY | cut -c3-`

if [ "$PDY2$cyc" = "$stormdate" ] ; then
 if [ $NEST = "pr" -o $NEST = "hi" ]; then  
  #if [ $nstms -gt 5 ] ; then 
  if [ $nstms -gt 6 ] ; then  ### changed to 6 to stop HIRESW preemption for hurricane season 2013 only
    #
    # More than four GFDLs running, cancel the two small domain runs pr and hi but keep the guam.
    #
    postmsg "$jlogfile" "${nstms} GFDL HURRICANE MODELS RUNNING."
    postmsg "$jlogfile" "THE $cyc UTC ${NEST}${MODEL} HIRESW RUN IS CANCELED."
    echo "$nstms GFDL HURRICANE MODELS RUNNING." > /com/hiresw/${envir}/hiresw_status.${cyc}
    echo "The $cyc UTC ${NEST}${MODEL} HIRESW RUN IS CANCELED." >> /com/hiresw/${envir}/hiresw_status.${cyc}
   if [ "$SENDECF" = "YES" -a $MODEL = "arw" ] ; then
      ecflow_client --label NEST "$cyc UTC ${NEST}${MODEL} IS CANCELED"
      ECF_PASS=FREE
      ecflow_client --force=complete /${envir}${prodcyc}/hiresw${cyc}/${MODEL}_ctl/jhiresw_${NEST}${MODEL}_ctl_awips_${cyc}
      ecflow_client --force=complete /${envir}${prodcyc}/hiresw${cyc}/${MODEL}_ctl/jhiresw_${NEST}${MODEL}_ctl_gempak_${cyc}
      ecflow_client --force=complete /${envir}${prodcyc}/hiresw${cyc}/${MODEL}_ctl/jhiresw_${NEST}${MODEL}_ctl_prdgen_${cyc}
      ecflow_client --force=complete /${envir}${prodcyc}/hiresw${cyc}/${MODEL}_ctl/jhiresw_${NEST}${MODEL}_ctl_wrfbufr_${cyc}
      ecflow_client --force=complete /${envir}${prodcyc}/hiresw${cyc}/${MODEL}_ctl/jhiresw_${NEST}${MODEL}_ctl_post_${cyc}
      ecflow_client --force=complete /${envir}${prodcyc}/hiresw${cyc}/${MODEL}_ctl/jhiresw_${NEST}${MODEL}_ctl_forecast_${cyc}
      exit 99
   elif [ "$SENDECF" = "YES" -a $MODEL = "nmm" ] ; then
      ecflow_client --label NEST "$cyc UTC ${NEST}${MODEL} IS CANCELED"
      ECF_PASS=FREE
      ecflow_client --force=complete /${envir}${prodcyc}/hiresw${cyc}/${MODEL}_ctl/jhiresw_${NEST}${MODEL}_ctl_awips_${cyc}
      ecflow_client --force=complete /${envir}${prodcyc}/hiresw${cyc}/${MODEL}_ctl/jhiresw_${NEST}${MODEL}_ctl_gempak_${cyc}
      ecflow_client --force=complete /${envir}${prodcyc}/hiresw${cyc}/${MODEL}_ctl/jhiresw_${NEST}${MODEL}_ctl_prdgen_${cyc}
      ecflow_client --force=complete /${envir}${prodcyc}/hiresw${cyc}/${MODEL}_ctl/jhiresw_${NEST}${MODEL}_ctl_ensembleproduct_all_${cyc}
      ecflow_client --force=complete /${envir}${prodcyc}/hiresw${cyc}/${MODEL}_ctl/jhiresw_${NEST}${MODEL}_ctl_hybridensemble_${cyc}
      ecflow_client --force=complete /${envir}${prodcyc}/hiresw${cyc}/${MODEL}_ctl/jhiresw_${NEST}${MODEL}_ctl_wrfbufr_${cyc}
      ecflow_client --force=complete /${envir}${prodcyc}/hiresw${cyc}/${MODEL}_ctl/jhiresw_${NEST}${MODEL}_ctl_post_${cyc}
      ecflow_client --force=complete /${envir}${prodcyc}/hiresw${cyc}/${MODEL}_ctl/jhiresw_${NEST}${MODEL}_ctl_forecast_${cyc}
      exit 99
   fi
  fi
else
  #if [ $nstms -ge 4 -a $NEST != "guam" ] ; then
  if [ $nstms -ge 6 -a $NEST != "guam" ] ; then ### changed to 6 to stop HIRESW preemption for hurricane season 2013 only
    #
    # If there are four HURRICANE or more runs cancel the HIRESW for the two large domains, MODEL=ARW and MODEL=NMM
    #    but not small domain "guam".
    #
    postmsg "$jlogfile" "${nstms} GFDL HURRICANE MODELS RUNNING."
    postmsg "$jlogfile" "THE $cyc UTC ${NEST}${MODEL} HIRESW RUN IS CANCELED."
    echo "$nstms GFDL HURRICANE MODELS RUNNING." > /com/hiresw/${envir}/hiresw_status.${cyc}
    echo "THE $cyc UTC ${NEST}${MODEL} HIRESW RUN IS CANCELED." >> /com/hiresw/${envir}/hiresw_status.${cyc}
    if [ "$SENDECF" = "YES" -a $MODEL = "arw" ] ; then
      ecflow_client --label NEST "$cyc UTC ${NEST}${MODEL} IS CANCELED"
      ECF_PASS=FREE
      ecflow_client --force=complete /${envir}${prodcyc}/hiresw${cyc}/${MODEL}_ctl/jhiresw_${MODEL}_ctl_awips_${cyc}
      ecflow_client --force=complete /${envir}${prodcyc}/hiresw${cyc}/${MODEL}_ctl/jhiresw_${MODEL}_ctl_gempak_${cyc}
      ecflow_client --force=complete /${envir}${prodcyc}/hiresw${cyc}/${MODEL}_ctl/jhiresw_${MODEL}_ctl_prdgen_${cyc}
      ecflow_client --force=complete /${envir}${prodcyc}/hiresw${cyc}/${MODEL}_ctl/jhiresw_${MODEL}_ctl_wrfbufr_${cyc}
      ecflow_client --force=complete /${envir}${prodcyc}/hiresw${cyc}/${MODEL}_ctl/jhiresw_${MODEL}_ctl_post_${cyc}
      ecflow_client --force=complete /${envir}${prodcyc}/hiresw${cyc}/${MODEL}_ctl/jhiresw_${MODEL}_ctl_forecast_${cyc}
      exit 99
    elif [ "$SENDECF" = "YES" -a $MODEL = "nmm" ] ; then
      ecflow_client --label NEST "$cyc UTC ${NEST}${MODEL} IS CANCELED"
      ECF_PASS=FREE
      ecflow_client --force=complete /${envir}${prodcyc}/hiresw${cyc}/${MODEL}_ctl/jhiresw_${MODEL}_ctl_awips_${cyc}
      ecflow_client --force=complete /${envir}${prodcyc}/hiresw${cyc}/${MODEL}_ctl/jhiresw_${MODEL}_ctl_gempak_${cyc}
      ecflow_client --force=complete /${envir}${prodcyc}/hiresw${cyc}/${MODEL}_ctl/jhiresw_${MODEL}_ctl_prdgen_${cyc}
      ecflow_client --force=complete /${envir}${prodcyc}/hiresw${cyc}/${MODEL}_ctl/jhiresw_${MODEL}_ctl_ensembleproduct_all_${cyc}
      ecflow_client --force=complete /${envir}${prodcyc}/hiresw${cyc}/${MODEL}_ctl/jhiresw_${MODEL}_ctl_hybridensemble_${cyc}
      ecflow_client --force=complete /${envir}${prodcyc}/hiresw${cyc}/${MODEL}_ctl/jhiresw_${MODEL}_ctl_wrfbufr_${cyc}
      ecflow_client --force=complete /${envir}${prodcyc}/hiresw${cyc}/${MODEL}_ctl/jhiresw_${MODEL}_ctl_post_${cyc}
      ecflow_client --force=complete /${envir}${prodcyc}/hiresw${cyc}/${MODEL}_ctl/jhiresw_${MODEL}_ctl_forecast_${cyc}
      exit 99
    fi
  fi
 fi
else
# if cycle date not equal to storm date assume no hurricane models ran so reset nstms to zero
  nstms=0
fi

#
# If we get here, assume we can run this threat.  Set the NEST flag and exit
#
echo "$nstms GFDL HURRICANE MODELS RUNNING." > /com/hiresw/${envir}/hiresw_status.${cyc}
echo "THE $cyc UTC $NEST HI-RES WINDOW RUN WILL PROCEED." >> /com/hiresw/${envir}/hiresw_status.${cyc}

if [ "$SENDECF" = "YES" ] ; then
  ecflow_client --label NEST "$NEST"
fi

exit 0
