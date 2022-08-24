#! /bin/sh

dom=hi
cyc=12
date=20220824


if [ ${dom} == 'conus' ]
then
	./run_preproc_nam.sh ${dom} ${cyc} ${date}
	./run_preproc_hrrr.sh ${dom} ${cyc} ${date}
	./run_ffg_gen.sh ${dom} ${cyc} ${date}
	echo "will wait to submit eas and ensprod jobs"
	sleep 240
elif [ ${dom} == 'ak' ]
then
        ./run_preproc_hrrr.sh ${dom} ${cyc} ${date}
	if [ $cyc == '06' -o $cyc == '18' ]
	then
	sleep 240
	echo submit AK EAS and ensprod jobs
        ./run_eas_1.sh      ${dom} ${cyc} ${date}
        ./run_eas_2.sh      ${dom} ${cyc} ${date}
        ./run_ensprod_1.sh  ${dom} ${cyc} ${date}
        ./run_ensprod_2.sh  ${dom} ${cyc} ${date}
         fi
fi

if [ ${dom} != 'ak' ] 
then
./run_eas_1.sh      ${dom} ${cyc} ${date}
./run_eas_2.sh      ${dom} ${cyc} ${date}
./run_ensprod_1.sh  ${dom} ${cyc} ${date}
./run_ensprod_2.sh  ${dom} ${cyc} ${date}
fi

# these get submitted by ensprod_2
# ./run_awips.sh ${dom} ${cyc} ${date}
# ./run_gempak.sh ${dom} ${cyc} ${date}

echo all done
