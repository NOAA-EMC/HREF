#!/bin/ksh
#######################################################################################
#  Script of Name:href_getmbr.sh 
#  Purpose: this script is to get softlink of previous of runs namnest, hiresw fv3 and 
#           hiresw arw, and HRRR according to time table (since all of them are on same #227 grid
#           no copygb2 is involved) 
#  History: 2015-02-02: Binbin Zhou created 
#           2019-09-10: Matthew Pyle added HRRR and FV3, eliminated NMMB
#           2022-03-29: Matthew Pyle - stripped to just FV3
#    Usage: href_getmbr.sh fhr cycle Day 
######################################################################################
##tst set -x         

typeset -Z2 cycloc
typeset -Z2 fcst
typeset -Z2 m

fhr=$1
dom=${2}

looplim=10
sleeptime=6

echo here in ush script with dom $dom

# fcheck=$fhr

let fcheck=fhr+2

typeset -Z2 fcheck

cd $DATA

if [ $dom = 'conus' ]
  then
     files="9 fv3s fv3s fv3s fv3s fv3s fv3s fv3s fv3s fv3s"
elif [ $dom = 'hi' ]
  then
     files="9 hifv3s hifv3s hifv3s hifv3s hifv3s hifv3s hifv3s hifv3s hifv3s"
elif [ $dom = 'pr' ]
  then
     files="9 prfv3s prfv3s prfv3s prfv3s prfv3s prfv3s prfv3s prfv3s prfv3s"
elif [ $dom = 'ak' ]
  then
     files="9 akfv3s akfv3s akfv3s akfv3s akfv3s akfv3s akfv3s akfv3s akfv3s"
else
   echo "bad domain $dom"
   msg="FATAL ERROR: dom was not conus, hi, pr, or ak"
   err_exit $msg
fi
set -A file  $files
days="9 $PDY  $PDY  $PDY  $PDY  $PDY $PDY  $PDY $PDY  $PDY"
cycs="9 $cyc  $cyc  $cyc  $cyc  $cyc $cyc  $cyc $cyc  $cyc"
ages="9  0     0      0     0     0    0     0    0      0"
set -A  day  $days
set -A  cycloc $cycs
set -A  age  $ages
mbrs="1  2  3  4  5  6  7  8  9" 


mbr=0

ff=$fhr

## are older files available?
	if [ $ff = '06' -o $ff = '09' ]
        then
        fcheck=` expr $ff - 03`
        elif [  $ff = '12' -o  $ff = '15' -o $ff = '18' -o $ff = '21' ]
        then
        fcheck=` expr $ff - 09`
        elif [  $ff = '24' -o  $ff = '27' -o $ff = '30' -o $ff = '33' -o $ff = '36' -o  $ff = '39' -o $ff = '42' -o $ff = '45' -o $ff = '48' ]
        then
        fcheck=` expr $ff - 21`
        elif [ $ff -gt 0 ]
        then
        fcheck=`expr $ff - 01`
	fi

typeset -Z2 fcheck
echo working things with ff as $ff and  fcheck as $fcheck

  mkdir -p $DATA/${ff} 
   mbr=0
   for m in $mbrs ; do
      fcst=` expr ${age[$m]} + $ff`   #$ff is forecast hours of ensemble member to be built, $fcst is forecast hours of base model requested

      echo ff $ff m $m
      echo fcst $fcst

#      echo href.m${m}.t${cyc}z.f${ff} 

###### FV3

# hifv3s prfv3s akfv3s

      if [  ${file[$m]} = 'fv3s'  -a $fcst -le 60  ] ; then

        filecheck=${COMINrrfs}.${day[$m]}/fv3s.t${cycloc[$m]}z.m${m}.f${fcst}.grib2

	if [ -e $filecheck ]
        then
         ln -sf $filecheck  $DATA/${RUN}.m${m}.t${cyc}z.f${ff}
         ln -sf $DATA/${RUN}.m${m}.t${cyc}z.f${ff}  $DATA/${ff}/${RUN}.m${m}.t${cyc}z.f${ff}
        else
         msg="FATAL ERROR: $filecheck missing but required"
         err_exit $msg
	fi

	fcheckloc=$fcheck
	while [ $fcheckloc -le $ff -a $fcheckloc -ne 0 ]
        do
	echo check on $DATA/${RUN}.m${m}.t${cyc}z.f${fcheckloc} working $ff
        loop=0
        while [ ! -e $DATA/${RUN}.m${m}.t${cyc}z.f${fcheckloc} -a $loop -lt $looplim ]
	do
	echo waiting on $DATA/${RUN}.m${m}.t${cyc}z.f${fcheckloc}
          sleep ${sleeptime}
          let loop=loop+1
        done	
        let fcheckloc=fcheckloc+1
typeset -Z2 fcheckloc
        echo new fcheckloc is $fcheckloc
        done
	
        if [ $ff -gt 0 ]
        then
	echo here a $ff
        if [ ${ff}%3 -eq 0 ]
        then
        echo ${RUN}.m${m}.t${cyc}z. $ff .false. .false. .false. .false. .false. 3 conus |$EXEChref/href_get_prcip > $DATA/output.href_get_prcip3h.m${m}.f${ff} 2>&1
        export err=$? ; err_chk
        fi
        fi
        echo ${RUN}.m${m}.t${cyc}z. $ff .false. .false. .false. .false. .false. 1 conus |$EXEChref/href_get_prcip > $DATA/output.href_get_prcip1h.m${m}.f${ff} 2>&1
        export err=$? ; err_chk

        if [ ${ff}%3 -eq 0 ] 
        then
        cat $DATA/prcip3h.m${m}.t${cyc}z.f${ff} >> $DATA/prcip.m${m}.t${cyc}z.f${ff}
	fi

        ln -sf $DATA/prcip.m${m}.t${cyc}z.f${ff} $DATA/${ff}/prcip.m${m}.t${cyc}z.f${ff}

      fi
 

###### FV3S - NON CONUS
       echo down here trying to define with ${file[$m]}

      if [ ${file[$m]} = ${dom}'fv3s' -a $fcst -le 60 ] ; then
	echo "in non-CONUS FV3S block"
# hifv3s prfv3s akfv3s

#        filecheck=${COMINhiresw}.${day[$m]}/hiresw.t${cycloc[$m]}z.fv3_5km.f${fcst}.${dom}.grib2
        filecheck=${COMINrrfs}.${day[$m]}/fv3s.t${cycloc[$m]}z.m${m}.f${fcst}.grib2

	if [ -e $filecheck ]
        then
         ln -sf    ${filecheck} $DATA/${RUN}.m${m}.t${cyc}z.f${ff}
         ln -sf    $DATA/${RUN}.m${m}.t${cyc}z.f${ff}  $DATA/${ff}/${RUN}.m${m}.t${cyc}z.f${ff}
        else
         
         msg="FATAL ERROR: $filecheck missing but required"
         err_exit $msg
	fi

	echo ${dom}fv3s $m $ff

	fcheckloc=$fcheck

	while [ $fcheckloc -le $ff -a $fcheckloc -ne 0 ]
        do
         echo check on $DATA/${RUN}.m${m}.t${cyc}z.f${fcheckloc} working $ff
         loop=0
        while [ ! -e $DATA/${RUN}.m${m}.t${cyc}z.f${fcheckloc} -a $loop -lt $looplim ]
	do
         echo waiting on $DATA/${RUN}.m${m}.t${cyc}z.f${fcheckloc}
         sleep ${sleeptime}
         let loop=loop+1
        done	
        let fcheckloc=fcheckloc+1
typeset -Z2 fcheckloc
        done


        if [ $ff -gt 0 ]
        then
	echo here a $ff

        if [ ${ff}%3 -eq 0 ]
        then
        echo ${RUN}.m${m}.t${cyc}z. $ff .false. .false. .false. .false. .false. 3 $dom |$EXEChref/href_get_prcip > $DATA/output.href_get_prcip3h.m${m}.f${ff} 2>&1
        export err=$? ; err_chk
        fi

        echo ${RUN}.m${m}.t${cyc}z. $ff .false. .false. .false. .false. .false. 1 $dom |$EXEChref/href_get_prcip > $DATA/output.href_get_prcip1h.m${m}.f${ff} 2>&1
        export err=$? ; err_chk

        if [ ${ff}%3 -eq 0 ] 
        then
        cat $DATA/prcip3h.m${m}.t${cyc}z.f${ff} >> $DATA/prcip.m${m}.t${cyc}z.f${ff}
	fi

        ln -sf $DATA/prcip.m${m}.t${cyc}z.f${ff} $DATA/${ff}/prcip.m${m}.t${cyc}z.f${ff}
        
        fi

      fi

   done #members

exit
