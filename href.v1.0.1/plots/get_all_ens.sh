
today=$1
cyc=$2
ptmp=/ptmpp1/Binbin.Zhou/href/plots.${today}${cyc}
com=/ptmpp1/Binbin.Zhou/com/hiresw/dev/href.${today}/ensprod

for fhr in 03 06 09 12 15 18 21 24 27 30 33 36 ; do
cp $com/href.prob.t${cyc}z.f$fhr $ptmp/.
cp $com/href.mean.t${cyc}z.f$fhr $ptmp/.
cp $com/href.sprd.t${cyc}z.f$fhr $ptmp/.
done

