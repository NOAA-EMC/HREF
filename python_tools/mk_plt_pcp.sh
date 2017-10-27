#!/bin/bash -l

# Function to make the config file for plotting precip accumulations
function pcpconfig() {
fstart=$1
fstop=$2
mydomain=$3
prefix=$4 #${gbprefix}
#comdir=$5
bucket_length=$5
#p=$fstart

(( f = fstart + bucket_length ))
(( nhours = fstop - fstart ))
file=conf${fstart}${fstop}.in
cat > $file << EOF
[precip]
nhours:${nhours}
domid:${mydomain}
bucket_length=${bucket_length}
EOF
z=${bucket_length}
while [ $f -le $fstop ]; do
  x=$(printf "%.2d" "$f")
#  y=$(printf "%.2d" "$p")
cat >> $file << EOF
gb${z}=${prefix}${x}.tm00.grib2   
EOF
  (( f= f + bucket_length ))
#  (( p= p + bucket_length ))
  (( z= z + bucket_length ))
done
}

#################################################################

set -x

CDATE=${PDY}${cyc}

mkdir -p $figout
cd ${DATA}

domid=`echo $domid | awk '{print toupper($0)}'`

#Set FSWITCH
if [ $domid = NAMRR ]; then
  gbprefix=$COMOUT/${RUN}.t${cyc}z.awphys
  if [ $RUNTYPE = CATCHUP ]; then
    FSWITCH=$PRDGENSWITCH_HOUR_PARENT
  else
    FSWITCH=$FMAX_HOURLY_PARENT
  fi
elif [ $domid = CONUSNEST -o $domid = ALASKANEST  ]; then
  mydom=`echo $domid | awk '{print tolower($0)}'`
  gbprefix=$COMOUT/${RUN}.t${cyc}z.${mydom}.hiresf
  if [ $RUNTYPE = CATCHUP ]; then
    FSWITCH=$PRDGENSWITCH_HOUR_NEST
  else
    FSWITCH=$FMAX_HOURLY_NEST
  fi
fi

  # Must set starting fhr correctly!
  vdate=`$NDATE ${fhr} ${CDATE}`
  valpdy=`echo $vdate | cut -c 1-8`
  valcyc=`echo $vdate | cut -c 9-10`

  # Now substract one hour to get the right right starting fhr
  # Remove any leading zeros so bash does not think this is octal
  stripped_fhr=$(echo ${fhr} | sed 's/^0*//')

  if [ $fhr -le $FSWITCH ]; then
    (( startfhr = stripped_fhr - 1 ))
    # Plot hourlies (or 3 hourlies) here - handle the big accums afterward
    export vdate=$vdate
    mystartfhr=$(printf "%.2d" "${startfhr#0}")
    myfhr=$(printf "%.2d" "${stripped_fhr#0}")
    python ${USHnam}/plt_pcp.py ${COMOUT}/f${mystartfhr}_${myfhr}hrpcp_${domid}_${CDATE}.gb2 ${domid} 1
    err1=$?
  fi

  # Make 3hrly precip too
  y=`expr $fhr % 3`
  if [ $y -eq 0 ]; then
    myfhr=$(printf "%.2d" "${stripped_fhr#0}")
    gbfile=${gbprefix}${fhr}.tm00.grib2      
    python ${USHnam}/plt_pcp.py $gbfile ${domid} 3
    err1=$?
  fi

  mv *png ${figout}/

case $fhr in
   12)
       pcpconfig 0 12 ${domid} ${gbprefix} 3
       python ${USHnam}/rd_add_plt_pcp.py conf012.in
       err=$?
       (( err = err + err1 ))
       mv *png ${figout}/
     ;;
   18)
       pcpconfig 0 18 ${domid} ${gbprefix} 3
       python ${USHnam}/rd_add_plt_pcp.py conf018.in
       err=$?
       (( err = err + err1 ))
       mv *png ${figout}/
     ;;
   24)
       pcpconfig 12 24 ${domid} ${gbprefix} 3
       pcpconfig 0 24 ${domid} ${gbprefix} 3
       python ${USHnam}/rd_add_plt_pcp.py conf1224.in
       python ${USHnam}/rd_add_plt_pcp.py conf024.in
       err=$?
       (( err = err + err1 ))
       mv *png ${figout}/
     ;;
   36)
       pcpconfig 12 36 ${domid} ${gbprefix} 3
       pcpconfig 24 36 ${domid} ${gbprefix} 3
       pcpconfig 0 36 ${domid} ${gbprefix} 3
       python ${USHnam}/rd_add_plt_pcp.py conf1236.in
       python ${USHnam}/rd_add_plt_pcp.py conf2436.in
       python ${USHnam}/rd_add_plt_pcp.py conf036.in
       err=$?
       (( err = err + err1 ))
       mv *png ${figout}/
     ;;
   48)
       pcpconfig 24 48 ${domid} ${gbprefix} 3
       pcpconfig 36 48 ${domid} ${gbprefix} 3
       pcpconfig 0 48 ${domid} ${gbprefix} 3
       python ${USHnam}/rd_add_plt_pcp.py conf2448.in
       python ${USHnam}/rd_add_plt_pcp.py conf3648.in
       python ${USHnam}/rd_add_plt_pcp.py conf048.in
       err=$?
       (( err = err + err1 ))
       mv *png ${figout}/
     ;;
   60)
       pcpconfig 36 60 ${domid} ${gbprefix} 3
       pcpconfig 48 60 ${domid} ${gbprefix} 3
       pcpconfig 0 60 ${domid} ${gbprefix} 3
       python ${USHnam}/rd_add_plt_pcp.py conf3660.in
       python ${USHnam}/rd_add_plt_pcp.py conf4860.in
       python ${USHnam}/rd_add_plt_pcp.py conf060.in
       err=$?
       (( err = err + err1 ))
       mv *png ${figout}/
     ;;
   72)
       pcpconfig 48 72 ${domid} ${gbprefix} 3
       pcpconfig 60 72 ${domid} ${gbprefix} 3
       pcpconfig 0 72 ${domid} ${gbprefix} 3
       python ${USHnam}/rd_add_plt_pcp.py conf4872.in
       python ${USHnam}/rd_add_plt_pcp.py conf6072.in
       python ${USHnam}/rd_add_plt_pcp.py conf072.in
       err=$?
       (( err = err + err1 ))
       mv *png ${figout}/
     ;;
   84)
       pcpconfig 60 84 ${domid} ${gbprefix} 3
       pcpconfig 72 84 ${domid} ${gbprefix} 3
       pcpconfig 0 84 ${domid} ${gbprefix} 3
       python ${USHnam}/rd_add_plt_pcp.py conf6084.in
       python ${USHnam}/rd_add_plt_pcp.py conf7284.in
       python ${USHnam}/rd_add_plt_pcp.py conf084.in
       err=$?
       (( err = err + err1 ))
       mv *png ${figout}/
     ;;
   *)
     err=$err1 
     echo "No need to plot accumulations for fhr $fhr"
   ;;

 esac

exit $err
