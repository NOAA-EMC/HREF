#!/bin/ksh
#######################################################################################
#  Script of Name:href_getmbr.sh 
#  Purpose: This script is to get softlink of previous of runs namnest, hireswnmmb and 
#           hireswarw according to time table (since all of them are on same #227 grid
#           no copygb2 is involved) 
#  History: 2015-02-02: Binbin Zhou created 
#    Usage: href_getmbr.sh fhr cycle Day 
######################################################################################
set -x         

typeset -Z2 cyc
typeset -Z2 run
typeset -Z2 fcst
typeset -Z2 m
fhr=$1
run=$2
T=$3

YS=`ndate -24 ${T}12`
Y=`echo ${YS} | cut -c 1-8`
#$mkdir -p $DATA/$T
#mkdir -p $DATA/$Y
#run_dir=$DATA/run_dir.${T}${run}
#mkdir -p $run_dir

cd $run_dir



if [ $run -ge 0 ] && [ $run -le 5 ] ; then 

  files="9 namnest namnest namnest namnest namnest conusarw conusnmmb conusarw conusnmmb conusarw conusnmmb"
  set -A file  $files

  if [ $run = '00' ] ; then
    days="9 $T $Y $Y $Y $Y $T $T $Y $Y $Y $Y"
    cycs="9 00 18 12 06 00 00 00 12 12 00 00"
    ages="9  0  6 12 18 24  0  0 12 12 24 24"
    mbrs="   1  2  3  4  5  6  7  8  9 10 11"
    set -A  day  $days
    set -A  cyc $cycs
    set -A  age  $ages
  fi


  if [ $run = '03' ] ; then
    days="9 $T $Y $Y $Y $Y $T $T $Y $Y $Y $Y"
    cycs="9 00 18 12 06 00 00 00 12 12 00 00"
    ages="9  3  9 15 21 27  3  3 15 15 27 27"
    mbrs="   1  2  3  4  5  6  7  8  9 10 11"
    set -A  day $days
    set -A  cyc $cycs
    set -A  age $ages
  fi

elif [ $run -ge 6 ] ; then

  files="9 namnest namnest namnest namnest namnest namnest conusarw conusnmmb conusarw conusnmmb conusarw conusnmmb"
  set -A file  $files

  if [ $run = '06' ] ; then
     days="9 $T $T $Y $Y $Y $Y $T $T $Y $Y $Y $Y"
     cycs="9 06 00 18 12 06 00 00 00 12 12 00 00"
     ages="9  0  6 12 18 24 30  6  6 18 18 30 30"
     mbrs="   1  2  3  4  5  6  7  8  9 10 11 12"
     set -A  day $days
     set -A  cyc $cycs
     set -A  age $ages
  fi

  if [ $run = '09' ] ; then
     days="9 $T $T $Y $Y $Y $Y $T $T $Y $Y $Y $Y"
     cycs="9 06 00 18 12 06 00 00 00 12 12 00 00"
     ages="9  3  9 15 21 27 33  9  9 21 21 33 33"
     mbrs="   1  2  3  4  5  6  7  8  9 10 11 12"
     set -A  day $days
     set -A  cyc $cycs
     set -A  age $ages
  fi

  if [ $run = '12' ] ; then
     days="9 $T $T $T $Y $Y $Y $T $T $T $T $Y $Y"
     cycs="9 12 06 00 18 12 06 12 12 00 00 12 12"
     ages="9  0  6 12 18 24 30  0  0 12 12 24 24"
     mbrs="   1  2  3  4  5  6  7  8  9 10 11 12"
     
     set -A  day $days
     set -A  cyc $cycs
     set -A  age $ages
  fi

  if [ $run = '15' ] ; then
     days="9 $T $T $T $Y $Y $Y $T $T $T $T $Y $Y"
     cycs="9 12 06 00 18 12 06 12 12 00 00 12 12"
     ages="9  3  9 15 21 27 33  3  3 15 15 27 27"
     mbrs="   1  2  3  4  5  6  7  8  9 10 11 12"
     set -A  day $days
     set -A  cyc $cycs
     set -A  age $ages
  fi

  if [ $run = '18' ] ; then
     days="9 $T $T $T $T $Y $Y $T $T $T $T $Y $Y"
     cycs="9 18 12 06 00 18 12 12 12 00 00 12 12"
     ages="9  0  6 12 18 24 30  6  6 18 18 30 30"
     mbrs="   1  2  3  4  5  6  7  8  9 10 11 12"
     set -A  day $days
     set -A  cyc $cycs
     set -A  age $ages
  fi

  if [ $run = '21' ] ; then
     days="9 $T $T $T $T $Y $Y $T $T $T $T $Y $Y"
     cycs="9 18 12 06 00 18 12 12 12 00 00 12 12"
     ages="9  3  9 15 21 27 33  9  9 21 21 33 33"
     mbrs="   1  2  3  4  5  6  7  8  9 10 11 12"
     set -A  day $days
     set -A  cyc $cycs
     set -A  age $ages
  fi


else

 echo $run ' is not a cycle'

fi


mbr=0
#NAMnest grid 227
#grid="30 6 0 0 0 0 0 0 1473 1025 12190000 226541000 8 25000000 265000000 5079000 5079000 0 64 25000000 25000000 0 0"  #grid277 namnest grid
for ff in $fhr ; do
  mkdir -p $run_dir/${ff} 
   mbr=0
   for m in $mbrs ; do
      fcst=` expr ${age[$m]} + $ff`   #$ff is forecast hours of ensemble member to be built, $fcst is forecast hours of base model requested

      echo ff $ff
      echo fcst $fcst

      echo href.m${m}.t${run}z.f${ff} 

      if [ ${file[$m]} = 'namnest' ] ; then     
        ln -sf /com/nam/prod/nam.${day[$m]}/nam.t${cyc[$m]}z.conusnest.hiresf${fcst}.tm00.grib2 $run_dir/href.m${m}.t${run}z.f${ff}
        ln -sf /com/nam/prod/nam.${day[$m]}/nam.t${cyc[$m]}z.conusnest.hiresf${fcst}.tm00.grib2 $run_dir/${ff}/href.m${m}.t${run}z.f${ff}
      fi
 

      if [ ${file[$m]} = 'conusarw' ] || [ ${file[$m]} = 'conusnmmb' ] ; then
	
        if [ -e /com/hiresw/prod/hiresw.${day[$m]}/${file[$m]}.t${cyc[$m]}z.awp5kmf${fcst}.grib2 ]
        then
        ln -sf /com/hiresw/prod/hiresw.${day[$m]}/${file[$m]}.t${cyc[$m]}z.awp5kmf${fcst}.grib2 $run_dir/href.m${m}.t${run}z.f${ff}
        ln -sf /com/hiresw/prod/hiresw.${day[$m]}/${file[$m]}.t${cyc[$m]}z.awp5kmf${fcst}.grib2 $run_dir/${ff}/href.m${m}.t${run}z.f${ff}
        fi
      fi

   done
done



exit


