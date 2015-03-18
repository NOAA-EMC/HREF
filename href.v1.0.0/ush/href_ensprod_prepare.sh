#!/bin/ksh
#####################################################
# produce commom ensemble products (mean, spread and
# probability) of selected variables and write them
# out in grib1 format
#
#  Scirpt: prepare_ensprod.sh
# Purpose: run ensemble product generator to generate 
#          required ensemble products accoriding to variable.tbl 
#  Author: B. Zhou IMSG/EMC/NCEP
#          10/19/2011
#    Modification: B. Zhou IMSG/EMC/NCEP 3/21/2013  
#          Transfered to WCOSS as parrell runs
#          Run 12 fhr parallel runs with one poe 


#set -x


export XLFRTEOPTS="namelist=old"


run=$1   #cycles
T=$2

YS=`ndate -24 ${T}12`
Y=`echo ${YS} | cut -c 1-8`
yy=`date +%Y`
mm=`date +%m`
dd=`date +%d`

yy=`echo ${T} | cut -c 1-4`
mm=`echo ${T} | cut -c 5-6`
dd=`echo ${T} | cut -c 7-8`

rundir=$run_dir
cd $rundir

rm -f poescript.run_post  poescript.post filename.* 

cp $PARMhref/href_variable_grib2.tbl variable.tbl



###############################

typeset -Z2 cyc     #temp variable here
typeset -Z2 run     #cycles
typeset -Z2 fcst    
typeset -Z2 m

if [ $run -ge 0 ] && [ $run -le 5 ] ; then

  files="9 namnest namnest namnest namnest namnest conusarw conusnmmb conusarw conusnmmb conusarw conusnmmb"
  set -A file  $files
  mbrs=" 6  7  8  9 10 11 1  2  3  4  5"

  if [ $run = '00' ] ; then
    days="9 $T $Y $Y $Y $Y $T $T $Y $Y $Y $Y"
    cycs="9 00 18 12 06 00 00 00 12 12 00 00"
    ages="9  0  6 12 18 24  0  0 12 12 24 24"
    set -A  day  $days
    set -A  cyc $cycs
    set -A  age  $ages
  fi


  if [ $run = '03' ] ; then
    days="9 $T $Y $Y $Y $Y $T $T $Y $Y $Y $Y"
    cycs="9 00 18 12 06 00 00 00 12 12 00 00"
    ages="9  3  9 15 21 27  3  3 15 15 27 27"
    set -A  day $days
    set -A  cyc $cycs
    set -A  age $ages
  fi


elif [ $run -ge 6 ] ; then


  files="9 namnest namnest namnest namnest namnest namnest conusarw conusnmmb conusarw conusnmmb conusarw conusnmmb"
  set -A file  $files
  mbrs=" 7  8  9 10 11 12 1  2  3  4  5  6" 

  if [ $run = '06' ] ; then
     days="9 $T $T $Y $Y $Y $Y $T $T $Y $Y $Y $Y"
     cycs="9 06 00 18 12 06 00 00 00 12 12 00 00"
     ages="9  0  6 12 18 24 30  6  6 18 18 30 30"
     set -A  day $days
     set -A  cyc $cycs
     set -A  age $ages
  fi

  if [ $run = '09' ] ; then
     days="9 $T $T $Y $Y $Y $Y $T $T $Y $Y $Y $Y"
     cycs="9 06 00 18 12 06 00 00 00 12 12 00 00"
     ages="9  3  9 15 21 27 33  9  9 21 21 33 33"
     set -A  day $days
     set -A  cyc $cycs
     set -A  age $ages
  fi

  if [ $run = '12' ] ; then
     days="9 $T $T $T $Y $Y $Y $T $T $T $T $Y $Y"
     cycs="9 12 06 00 18 12 06 12 12 00 00 12 12"
     ages="9  0  6 12 18 24 30  0  0 12 12 24 24"

     set -A  day $days
     set -A  cyc $cycs
     set -A  age $ages
  fi

  if [ $run = '15' ] ; then
     days="9 $T $T $T $Y $Y $Y $T $T $T $T $Y $Y"
     cycs="9 12 06 00 18 12 06 12 12 00 00 12 12"
     ages="9  3  9 15 21 27 33  3  3 15 15 27 27"
     set -A  day $days
     set -A  cyc $cycs
     set -A  age $ages
  fi

  if [ $run = '18' ] ; then
     days="9 $T $T $T $T $Y $Y $T $T $T $T $Y $Y"
     cycs="9 18 12 06 00 18 12 12 12 00 00 12 12"
     ages="9  0  6 12 18 24 30  6  6 18 18 30 30"
     set -A  day $days
     set -A  cyc $cycs
     set -A  age $ages
  fi

  if [ $run = '21' ] ; then
     days="9 $T $T $T $T $Y $Y $T $T $T $T $Y $Y"
     cycs="9 18 12 06 00 18 12 12 12 00 00 12 12"
     ages="9  3  9 15 21 27 33  9  9 21 21 33 33"
     set -A  day $days
     set -A  cyc $cycs
     set -A  age $ages
  fi


else

 echo $run ' is not a cycle'

fi



for ff in 03 06 09 12 15 18 21 24 27 30 33 36 ; do

 nmbr=0
 for m in $mbrs ; do              
   fcst=` expr ${age[$m]} + $ff`
     weight=`echo "scale=2; 1-${age[$m]}/48" | bc`

      if [ $weight -lt 1.0 ] ; then
        weight='0'$weight
      fi

   if [ -s $rundir/href.m${m}.t${run}z.f$ff ] ; then
       nmbr=` expr $nmbr + 1`
       echo "   "$weight href.m${m}.t${run}z.f$ff "->" ${file[$m]}.t${cyc[$m]}z.f${fcst} >> temp.f${ff}
       ln -sf $rundir/href.m${m}.t${run}z.f$ff $rundir/$ff/href.m${m}.t${run}z.f$ff
       ln -sf $rundir/href.m${m}.t${run}z.f$ff $rundir/$ff/prcip.m${m}.t${run}z.f$ff
   fi
 done

  echo $yy $mm $dd $run $ff "227 39" "36" "3" "12"  > filename.f${ff}    #first 36 is leadtime, second 12 is fcst times = leadtime/interval
  cat temp.f${ff} >> filename.f${ff}
  rm -f temp.f${ff}

rm -f fhrs.input.$ff
ln -fs $rundir/filename.f$ff $rundir/$ff/filename
ln -fs $rundir/variable.tbl $rundir/$ff/variable.tbl
ln -fs $EXEChref/href_ensprod $rundir/$ff/href_ensprod

done  #done for $ff 


#Create POE script files to run all 12 fhrs's jobs
for ff in 03 06 09 12 15 18; do
 #define procedures in each tasks
 echo "cd $rundir/$ff" > run_ensprod_poe_one.$ff
 echo "$rundir/$ff/href_ensprod > $rundir/$ff/output_ensprod.$ff " >> $rundir/run_ensprod_poe_one.$ff
 echo "cp $rundir/$ff/href.mean.t${run}z.f$ff $COMOUT" >> $rundir/run_ensprod_poe_one.$ff
 echo "cp $rundir/$ff/href.prob.t${run}z.f$ff $COMOUT" >> $rundir/run_ensprod_poe_one.$ff
 echo "cp $rundir/$ff/href.sprd.t${run}z.f$ff $COMOUT" >> $rundir/run_ensprod_poe_one.$ff
 chmod +x run_ensprod_poe_one.$ff
 #list this task into poe task-list 
 echo "$rundir/run_ensprod_poe_one.$ff" >> $rundir/run_ensprod_poe_1-6.sh
done

# prepare poe script for bsub 
chmod +x $rundir/run_ensprod_poe_1-6.sh
sed -e "s!FHR!6!g" -e "s!RUNDIR!$rundir!g" -e "s!RUNENVIR!$RUN_ENVIR!g" -e "s!ALL!1-6!g" -e "s!PIECE!1!g"  $USHhref/href_enprod_poe.sh > $rundir/href_enprod_poe_1-6.sh
chmod +x $rundir/href_enprod_poe_1-6.sh

for ff in 21 24 27 30 33 36 ; do
 #define procedures in each tasks
 echo "cd $rundir/$ff" > run_ensprod_poe_one.$ff
 echo "$rundir/$ff/href_ensprod > $rundir/$ff/output_ensprod.$ff " >> $rundir/run_ensprod_poe_one.$ff
 echo "cp $rundir/$ff/href.mean.t${run}z.f$ff $COMOUT" >> $rundir/run_ensprod_poe_one.$ff
 echo "cp $rundir/$ff/href.prob.t${run}z.f$ff $COMOUT" >> $rundir/run_ensprod_poe_one.$ff
 echo "cp $rundir/$ff/href.sprd.t${run}z.f$ff $COMOUT" >> $rundir/run_ensprod_poe_one.$ff
 chmod +x run_ensprod_poe_one.$ff
 #list this task into poe task-list
 echo "$rundir/run_ensprod_poe_one.$ff" >> $rundir/run_ensprod_poe_7-12.sh
done

# prepare poe script for bsub
chmod +x $rundir/run_ensprod_poe_7-12.sh
sed -e "s!FHR!6!g" -e "s!RUNDIR!$rundir!g" -e "s!RUNENVIR!$RUN_ENVIR!g" -e "s!ALL!7-12!g" -e "s!PIECE!2!g" $USHhref/href_enprod_poe.sh > $rundir/href_enprod_poe_7-12.sh
chmod +x $rundir/href_enprod_poe_7-12.sh

exit

