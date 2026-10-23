#! /bin/bash

targ=../../parm/wmo
doms="conus hi pr ak"

for dom in $doms
do
	mv grib2_refs_${dom}_meanf?? ${targ}
	mv grib2_refs_${dom}_pmmnf?? ${targ}
	mv grib2_refs_${dom}_probf?? ${targ}
done

