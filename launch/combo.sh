#! /bin/sh

dom=conus
cyc=12
date=20230501


if [ ${dom} == 'conus' ]
then
	./run_enspost_preproc_fv3.sh ${dom} ${cyc} ${date} ${type}
	./run_ffg_gen.sh ${dom} ${cyc} ${date} ${type}
	sleep 240
elif [ ${dom} == 'ak' ]
then
	        ./run_preproc_hrrr.sh ${dom} ${cyc} ${date}
	sleep 240
fi

./run_enspost_eas_1.sh      ${dom} ${cyc} ${date} ${type}
./run_enspost_eas_2.sh      ${dom} ${cyc} ${date} ${type}
./run_enspost_ensprod_1.sh  ${dom} ${cyc} ${date} ${type}
./run_enspost_ensprod_2.sh  ${dom} ${cyc} ${date} ${type}


# ./run_enspost_awips.sh ${dom} ${cyc} ${date} ${type}
# ./run_enspost_gempak.sh ${dom} ${cyc} ${date} ${type}
