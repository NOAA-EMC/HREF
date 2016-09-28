#!/bin/bash

i3=001

rm -f GRIBFILE.??? >& /dev/null

for f in $*
do
    i3=$( printf "%03d" "$i3" )
    ln -sf ${f} GRIBFILE.$i3
    i3=`expr $i3 + 1`
done
