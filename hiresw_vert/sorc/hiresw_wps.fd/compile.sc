#!/bin/sh

TARGDIR=/scratch2/portfolios/NCEPDEV/meso/save/Matthew.Pyle/wcoss_transition/exec

# run after ./configure (select option3, DM parallel, NO GRIB2

./compile >& compile.sc.log

cp ungrib.exe $TARGDIR/hiresw_ungrib
cp metgrid.exe $TARGDIR/hiresw_metgrid
