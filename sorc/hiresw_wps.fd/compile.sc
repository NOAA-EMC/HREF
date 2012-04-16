#!/bin/sh

TARGDIR=/nwtest/exec

# run after ./configure (select option3, DM parallel, NO GRIB2

./compile > compile.sc.log

cp ungrib.exe $TARGDIR/hiresw_ungrib
cp metgrid.exe $TARGDIR/hiresw_metgrid
