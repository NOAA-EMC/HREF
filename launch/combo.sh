#! /bin/sh

dom=conus
cyc=12
date=20220805


if [ ${dom} == 'conus' ]
then
	./run_preproc_nam.sh ${dom} ${cyc} ${date}
	./run_preproc_hrrr.sh ${dom} ${cyc} ${date}
	./run_ffg_gen.sh ${dom} ${cyc} ${date}
	sleep 240
elif [ ${dom} == 'ak' ]
then
        ./run_preproc_hrrr.sh ${dom} ${cyc} ${date}
	sleep 240
fi

./run_eas_1.sh      ${dom} ${cyc} ${date}
./run_eas_2.sh      ${dom} ${cyc} ${date}
./run_ensprod_1.sh  ${dom} ${cyc} ${date}
./run_ensprod_2.sh  ${dom} ${cyc} ${date}


# ./run_awips.sh ${dom} ${cyc} ${date}
# ./run_gempak.sh ${dom} ${cyc} ${date}
