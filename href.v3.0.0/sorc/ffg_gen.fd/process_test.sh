#! /bin/sh



date=20190429
files=`ls ffg.${date}.009.???`

rm *.g2*

for fl in $files
do
cnvgrib -g12 ${fl} ${fl}.g2
done

echo ${date} > input
tst_read_grib1.x < input

for fl in $files
do
wgrib2 ${fl}.g2out -new_grid_winds grid -new_grid lambert:265:25:25 226.541:1473:5079 12.190:1025:5079 ${fl}.g2out.227
done
