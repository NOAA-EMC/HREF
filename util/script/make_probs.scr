#! /bin/bash

RUN=refs

targs="Y A H P"

for t1targ in $targs
do

if [ ${t1targ} = "Y" ]
then
domtarg='conus'
fi

if [ ${t1targ} = "A" ]
then
domtarg='ak'
fi

if [ ${t1targ} = "H" ]
then
domtarg='hi'
fi

if [ ${t1targ} = "P" ]
then
domtarg='pr'
fi


#########################################################
base=grib2_${RUN}_conus_prob_3h
basealt=grib2_${RUN}_conus_prob_3h_noffri
#########################################################

hr=3
hrold=2
hr3old=0
TCODE=D

if [ ${domtarg} = 'conus' ]
then
 cat ${base} |  sed s:_T1_:${t1targ}:g | sed s:_CHAR_:${TCODE}:g | sed s:_F_:${hr}:g | sed s:_FOLD_:${hrold}:g \
            | sed s:_FOLD3_:${hr3old}:g | sed s:_FOLD6_:${hr6old}:g > grib2_${RUN}_${domtarg}_probf0${hr}

else
 cat ${basealt} |  sed s:_T1_:${t1targ}:g | sed s:_CHAR_:${TCODE}:g | sed s:_F_:${hr}:g | sed s:_FOLD_:${hrold}:g \
            | sed s:_FOLD3_:${hr3old}:g | sed s:_FOLD6_:${hr6old}:g > grib2_${RUN}_${domtarg}_probf0${hr}
fi


###########################################################################
base=grib2_${RUN}_conus_prob_6h
basealt=grib2_${RUN}_conus_prob_6h_noffri
###########################################################################

hr=6
hrold=5
hr3old=3
hr6old=0
TCODE=G

if [ ${domtarg} = 'conus' ]
then
 cat ${base} |  sed s:_T1_:${t1targ}:g | sed s:_CHAR_:${TCODE}:g | sed s:_F_:${hr}:g | sed s:_FOLD_:${hrold}:g \
            |  sed s:_T1_:${t1targ}:g | sed s:_FOLD3_:${hr3old}:g | sed s:_FOLD6_:${hr6old}:g > grib2_${RUN}_${domtarg}_probf0${hr}

else
 cat ${basealt} |  sed s:_T1_:${t1targ}:g | sed s:_CHAR_:${TCODE}:g | sed s:_F_:${hr}:g | sed s:_FOLD_:${hrold}:g \
            | sed s:_FOLD3_:${hr3old}:g | sed s:_FOLD6_:${hr6old}:g > grib2_${RUN}_${domtarg}_probf0${hr}
fi

hr=9
hrold=8
hr3old=6
hr6old=3
TCODE=J
if [ ${domtarg} = 'conus' ]
then
 cat ${base} |  sed s:_T1_:${t1targ}:g | sed s:_CHAR_:${TCODE}:g | sed s:_F_:${hr}:g | sed s:_FOLD_:${hrold}:g \
            | sed s:_FOLD3_:${hr3old}:g | sed s:_FOLD6_:${hr6old}:g > grib2_${RUN}_${domtarg}_probf0${hr}
else
 cat ${basealt} |  sed s:_T1_:${t1targ}:g | sed s:_CHAR_:${TCODE}:g | sed s:_F_:${hr}:g | sed s:_FOLD_:${hrold}:g \
            | sed s:_FOLD3_:${hr3old}:g | sed s:_FOLD6_:${hr6old}:g > grib2_${RUN}_${domtarg}_probf0${hr}
fi

echo DONE 6h

###########################################################################
base=grib2_${RUN}_conus_prob_12h
basealt=grib2_${RUN}_conus_prob_12h_noffri
###########################################################################

hr=12
hrold=11
hr3old=9
hr6old=6
hr12old=0
TCODE=M

if [ ${domtarg} = 'conus' ]
then

 cat ${base} |  sed s:_T1_:${t1targ}:g | sed s:_CHAR_:${TCODE}:g | sed s:_F_:${hr}:g | sed s:_FOLD_:${hrold}:g \
            | sed s:_FOLD3_:${hr3old}:g | sed s:_FOLD6_:${hr6old}:g | sed s:_FOLD12_:${hr12old}:g > grib2_${RUN}_${domtarg}_probf${hr}
else
 cat ${basealt} |  sed s:_T1_:${t1targ}:g | sed s:_CHAR_:${TCODE}:g | sed s:_F_:${hr}:g | sed s:_FOLD_:${hrold}:g \
            | sed s:_FOLD3_:${hr3old}:g | sed s:_FOLD6_:${hr6old}:g | sed s:_FOLD12_:${hr12old}:g > grib2_${RUN}_${domtarg}_probf${hr}
fi

hr=15
hrold=14
hr3old=12
hr6old=9
hr12old=3
TCODE=P
if [ ${domtarg} = 'conus' ]
then
 cat ${base} |  sed s:_T1_:${t1targ}:g | sed s:_CHAR_:${TCODE}:g | sed s:_F_:${hr}:g | sed s:_FOLD_:${hrold}:g \
            | sed s:_FOLD3_:${hr3old}:g | sed s:_FOLD6_:${hr6old}:g | sed s:_FOLD12_:${hr12old}:g > grib2_${RUN}_${domtarg}_probf${hr}
else
 cat ${basealt} |  sed s:_T1_:${t1targ}:g | sed s:_CHAR_:${TCODE}:g | sed s:_F_:${hr}:g | sed s:_FOLD_:${hrold}:g \
            | sed s:_FOLD3_:${hr3old}:g | sed s:_FOLD6_:${hr6old}:g | sed s:_FOLD12_:${hr12old}:g > grib2_${RUN}_${domtarg}_probf${hr}
fi

hr=18
hrold=17
hr3old=15
hr6old=12
hr12old=6
TCODE=S
if [ ${domtarg} = 'conus' ]
then
 cat ${base} |  sed s:_T1_:${t1targ}:g | sed s:_CHAR_:${TCODE}:g | sed s:_F_:${hr}:g | sed s:_FOLD_:${hrold}:g \
            | sed s:_FOLD3_:${hr3old}:g | sed s:_FOLD6_:${hr6old}:g | sed s:_FOLD12_:${hr12old}:g > grib2_${RUN}_${domtarg}_probf${hr}
else
 cat ${basealt} |  sed s:_T1_:${t1targ}:g | sed s:_CHAR_:${TCODE}:g | sed s:_F_:${hr}:g | sed s:_FOLD_:${hrold}:g \
            | sed s:_FOLD3_:${hr3old}:g | sed s:_FOLD6_:${hr6old}:g | sed s:_FOLD12_:${hr12old}:g > grib2_${RUN}_${domtarg}_probf${hr}
fi

hr=21
hrold=20
hr3old=18
hr6old=15
hr12old=9
TCODE=Z
if [ ${domtarg} = 'conus' ]
then
 cat ${base} |  sed s:_T1_:${t1targ}:g | sed s:_CHAR_:${TCODE}:g | sed s:_F_:${hr}:g | sed s:_FOLD_:${hrold}:g \
            | sed s:_FOLD3_:${hr3old}:g | sed s:_FOLD6_:${hr6old}:g | sed s:_FOLD12_:${hr12old}:g > grib2_${RUN}_${domtarg}_probf${hr}
else
 cat ${basealt} |  sed s:_T1_:${t1targ}:g | sed s:_CHAR_:${TCODE}:g | sed s:_F_:${hr}:g | sed s:_FOLD_:${hrold}:g \
            | sed s:_FOLD3_:${hr3old}:g | sed s:_FOLD6_:${hr6old}:g | sed s:_FOLD12_:${hr12old}:g > grib2_${RUN}_${domtarg}_probf${hr}
fi

echo DONE 12h

###########################################################################
###########################################################################
base=grib2_${RUN}_conus_prob_24h
basealt=grib2_${RUN}_conus_prob_24h_noffri
###########################################################################

hr=24
hrold=23
hr3old=21
hr6old=18
hr12old=12
hr24old=0
TCODE=T
if [ ${domtarg} = 'conus' ]
then
 cat ${base} |  sed s:_T1_:${t1targ}:g | sed s:_CHAR_:${TCODE}:g | sed s:_F_:${hr}:g | sed s:_FOLD_:${hrold}:g \
            | sed s:_FOLD3_:${hr3old}:g | sed s:_FOLD6_:${hr6old}:g | sed s:_FOLD12_:${hr12old}:g | sed s:_FOLD24_:${hr24old}:g > grib2_${RUN}_${domtarg}_probf${hr}
else

 cat ${basealt} |  sed s:_T1_:${t1targ}:g | sed s:_CHAR_:${TCODE}:g | sed s:_F_:${hr}:g | sed s:_FOLD_:${hrold}:g \
            | sed s:_FOLD3_:${hr3old}:g | sed s:_FOLD6_:${hr6old}:g | sed s:_FOLD12_:${hr12old}:g | sed s:_FOLD24_:${hr24old}:g > grib2_${RUN}_${domtarg}_probf${hr}
fi

hr=27
hrold=26
hr3old=24
hr6old=21
hr12old=15
hr24old=3
TCODE=Z
if [ ${domtarg} = 'conus' ]
then
 cat ${base} |  sed s:_T1_:${t1targ}:g | sed s:_CHAR_:${TCODE}:g | sed s:_F_:${hr}:g | sed s:_FOLD_:${hrold}:g \
            | sed s:_FOLD3_:${hr3old}:g | sed s:_FOLD6_:${hr6old}:g | sed s:_FOLD12_:${hr12old}:g | sed s:_FOLD24_:${hr24old}:g > grib2_${RUN}_${domtarg}_probf${hr}
else
 cat ${basealt} |  sed s:_T1_:${t1targ}:g | sed s:_CHAR_:${TCODE}:g | sed s:_F_:${hr}:g | sed s:_FOLD_:${hrold}:g \
            | sed s:_FOLD3_:${hr3old}:g | sed s:_FOLD6_:${hr6old}:g | sed s:_FOLD12_:${hr12old}:g | sed s:_FOLD24_:${hr24old}:g > grib2_${RUN}_${domtarg}_probf${hr}
fi

hr=30
hrold=29
hr3old=27
hr6old=24
hr12old=18
hr24old=6
TCODE=Z
if [ ${domtarg} = 'conus' ]
then
 cat ${base} |  sed s:_T1_:${t1targ}:g | sed s:_CHAR_:${TCODE}:g | sed s:_F_:${hr}:g | sed s:_FOLD_:${hrold}:g \
            | sed s:_FOLD3_:${hr3old}:g | sed s:_FOLD6_:${hr6old}:g | sed s:_FOLD12_:${hr12old}:g | sed s:_FOLD24_:${hr24old}:g > grib2_${RUN}_${domtarg}_probf${hr}
else
 cat ${basealt} |  sed s:_T1_:${t1targ}:g | sed s:_CHAR_:${TCODE}:g | sed s:_F_:${hr}:g | sed s:_FOLD_:${hrold}:g \
            | sed s:_FOLD3_:${hr3old}:g | sed s:_FOLD6_:${hr6old}:g | sed s:_FOLD12_:${hr12old}:g | sed s:_FOLD24_:${hr24old}:g > grib2_${RUN}_${domtarg}_probf${hr}
fi

hr=33
hrold=32
hr3old=30
hr6old=27
hr12old=21
hr24old=9
TCODE=Z
if [ ${domtarg} = 'conus' ]
then
 cat ${base} |  sed s:_T1_:${t1targ}:g | sed s:_CHAR_:${TCODE}:g | sed s:_F_:${hr}:g | sed s:_FOLD_:${hrold}:g \
            | sed s:_FOLD3_:${hr3old}:g | sed s:_FOLD6_:${hr6old}:g | sed s:_FOLD12_:${hr12old}:g | sed s:_FOLD24_:${hr24old}:g > grib2_${RUN}_${domtarg}_probf${hr}
else
 cat ${basealt} |  sed s:_T1_:${t1targ}:g | sed s:_CHAR_:${TCODE}:g | sed s:_F_:${hr}:g | sed s:_FOLD_:${hrold}:g \
            | sed s:_FOLD3_:${hr3old}:g | sed s:_FOLD6_:${hr6old}:g | sed s:_FOLD12_:${hr12old}:g | sed s:_FOLD24_:${hr24old}:g > grib2_${RUN}_${domtarg}_probf${hr}
fi

hr=36
hrold=35
hr3old=33
hr6old=30
hr12old=24
hr24old=12
TCODE=U
if [ ${domtarg} = 'conus' ]
then
 cat ${base} |  sed s:_T1_:${t1targ}:g | sed s:_CHAR_:${TCODE}:g | sed s:_F_:${hr}:g | sed s:_FOLD_:${hrold}:g \
            | sed s:_FOLD3_:${hr3old}:g | sed s:_FOLD6_:${hr6old}:g | sed s:_FOLD12_:${hr12old}:g | sed s:_FOLD24_:${hr24old}:g > grib2_${RUN}_${domtarg}_probf${hr}
else
 cat ${basealt} |  sed s:_T1_:${t1targ}:g | sed s:_CHAR_:${TCODE}:g | sed s:_F_:${hr}:g | sed s:_FOLD_:${hrold}:g \
            | sed s:_FOLD3_:${hr3old}:g | sed s:_FOLD6_:${hr6old}:g | sed s:_FOLD12_:${hr12old}:g | sed s:_FOLD24_:${hr24old}:g > grib2_${RUN}_${domtarg}_probf${hr}
fi

hr=39
hrold=38
hr3old=36
hr6old=33
hr12old=27
hr24old=15
TCODE=Z
if [ ${domtarg} = 'conus' ]
then
 cat ${base} |  sed s:_T1_:${t1targ}:g | sed s:_CHAR_:${TCODE}:g | sed s:_F_:${hr}:g | sed s:_FOLD_:${hrold}:g \
            | sed s:_FOLD3_:${hr3old}:g | sed s:_FOLD6_:${hr6old}:g | sed s:_FOLD12_:${hr12old}:g | sed s:_FOLD24_:${hr24old}:g > grib2_${RUN}_${domtarg}_probf${hr}
else
 cat ${basealt} |  sed s:_T1_:${t1targ}:g | sed s:_CHAR_:${TCODE}:g | sed s:_F_:${hr}:g | sed s:_FOLD_:${hrold}:g \
            | sed s:_FOLD3_:${hr3old}:g | sed s:_FOLD6_:${hr6old}:g | sed s:_FOLD12_:${hr12old}:g | sed s:_FOLD24_:${hr24old}:g > grib2_${RUN}_${domtarg}_probf${hr}
fi

hr=42
hrold=41
hr3old=39
hr6old=36
hr12old=30
hr24old=18
TCODE=Z
if [ ${domtarg} = 'conus' ]
then
cat ${base} |  sed s:_T1_:${t1targ}:g | sed s:_CHAR_:${TCODE}:g | sed s:_F_:${hr}:g | sed s:_FOLD_:${hrold}:g \
            | sed s:_FOLD3_:${hr3old}:g | sed s:_FOLD6_:${hr6old}:g | sed s:_FOLD12_:${hr12old}:g | sed s:_FOLD24_:${hr24old}:g > grib2_${RUN}_${domtarg}_probf${hr}
else
cat ${basealt} |  sed s:_T1_:${t1targ}:g | sed s:_CHAR_:${TCODE}:g | sed s:_F_:${hr}:g | sed s:_FOLD_:${hrold}:g \
            | sed s:_FOLD3_:${hr3old}:g | sed s:_FOLD6_:${hr6old}:g | sed s:_FOLD12_:${hr12old}:g | sed s:_FOLD24_:${hr24old}:g > grib2_${RUN}_${domtarg}_probf${hr}
fi

hr=45
hrold=44
hr3old=42
hr6old=39
hr12old=33
hr24old=21
TCODE=Z
if [ ${domtarg} = 'conus' ]
then
 cat ${base} |  sed s:_T1_:${t1targ}:g | sed s:_CHAR_:${TCODE}:g | sed s:_F_:${hr}:g | sed s:_FOLD_:${hrold}:g \
            | sed s:_FOLD3_:${hr3old}:g | sed s:_FOLD6_:${hr6old}:g | sed s:_FOLD12_:${hr12old}:g | sed s:_FOLD24_:${hr24old}:g > grib2_${RUN}_${domtarg}_probf${hr}
else
 cat ${basealt} |  sed s:_T1_:${t1targ}:g | sed s:_CHAR_:${TCODE}:g | sed s:_F_:${hr}:g | sed s:_FOLD_:${hrold}:g \
            | sed s:_FOLD3_:${hr3old}:g | sed s:_FOLD6_:${hr6old}:g | sed s:_FOLD12_:${hr12old}:g | sed s:_FOLD24_:${hr24old}:g > grib2_${RUN}_${domtarg}_probf${hr}
fi

hr=48
hrold=47
hr3old=45
hr6old=42
hr12old=36
hr24old=24
TCODE=V
if [ ${domtarg} = 'conus' ]
then
 cat ${base} |  sed s:_T1_:${t1targ}:g | sed s:_CHAR_:${TCODE}:g | sed s:_F_:${hr}:g | sed s:_FOLD_:${hrold}:g \
            | sed s:_FOLD3_:${hr3old}:g | sed s:_FOLD6_:${hr6old}:g | sed s:_FOLD12_:${hr12old}:g | sed s:_FOLD24_:${hr24old}:g > grib2_${RUN}_${domtarg}_probf${hr}
else
 cat ${basealt} |  sed s:_T1_:${t1targ}:g | sed s:_CHAR_:${TCODE}:g | sed s:_F_:${hr}:g | sed s:_FOLD_:${hrold}:g \
            | sed s:_FOLD3_:${hr3old}:g | sed s:_FOLD6_:${hr6old}:g | sed s:_FOLD12_:${hr12old}:g | sed s:_FOLD24_:${hr24old}:g > grib2_${RUN}_${domtarg}_probf${hr}
fi

# TCODE=Z beyond f48????
#
hr=51
hrold=50
hr3old=48
hr6old=45
hr12old=39
hr24old=27
TCODE=Z
# cat ${base} | sed s:_CHAR_:${TCODE}:g | sed s:_F_:${hr}:g | sed s:_FOLD_:${hrold}:g \
#             | sed s:_FOLD3_:${hr3old}:g | sed s:_FOLD6_:${hr6old}:g | sed s:_FOLD12_:${hr12old}:g | sed s:_FOLD24_:${hr24old}:g > grib2_${RUN}_${domtarg}_probf${hr}

# cat ${basealt} | sed s:_CHAR_:${TCODE}:g | sed s:_F_:${hr}:g | sed s:_FOLD_:${hrold}:g \
#            | sed s:_FOLD3_:${hr3old}:g | sed s:_FOLD6_:${hr6old}:g | sed s:_FOLD12_:${hr12old}:g | sed s:_FOLD24_:${hr24old}:g > grib2_${RUN}_${domtarg}_probf${hr}


hr=54
hrold=53
hr3old=51
hr6old=48
hr12old=42
hr24old=30
TCODE=Z
if [ ${domtarg} = 'conus' ]
then
 cat ${base} |  sed s:_T1_:${t1targ}:g | sed s:_CHAR_:${TCODE}:g | sed s:_F_:${hr}:g | sed s:_FOLD_:${hrold}:g \
            | sed s:_FOLD3_:${hr3old}:g | sed s:_FOLD6_:${hr6old}:g | sed s:_FOLD12_:${hr12old}:g | sed s:_FOLD24_:${hr24old}:g > grib2_${RUN}_${domtarg}_probf${hr}
else
 cat ${basealt} |  sed s:_T1_:${t1targ}:g | sed s:_CHAR_:${TCODE}:g | sed s:_F_:${hr}:g | sed s:_FOLD_:${hrold}:g \
            | sed s:_FOLD3_:${hr3old}:g | sed s:_FOLD6_:${hr6old}:g | sed s:_FOLD12_:${hr12old}:g | sed s:_FOLD24_:${hr24old}:g > grib2_${RUN}_${domtarg}_probf${hr}
fi

hr=57
hrold=56
hr3old=54
hr6old=51
hr12old=45
hr24old=33
TCODE=Z
# cat ${base} | sed s:_CHAR_:${TCODE}:g | sed s:_F_:${hr}:g | sed s:_FOLD_:${hrold}:g \
#             | sed s:_FOLD3_:${hr3old}:g | sed s:_FOLD6_:${hr6old}:g | sed s:_FOLD12_:${hr12old}:g | sed s:_FOLD24_:${hr24old}:g > grib2_${RUN}_${domtarg}_probf${hr}
#
# cat ${basealt} | sed s:_CHAR_:${TCODE}:g | sed s:_F_:${hr}:g | sed s:_FOLD_:${hrold}:g \
#            | sed s:_FOLD3_:${hr3old}:g | sed s:_FOLD6_:${hr6old}:g | sed s:_FOLD12_:${hr12old}:g | sed s:_FOLD24_:${hr24old}:g > grib2_${RUN}_${domtarg}_probf${hr}

hr=60
hrold=59
hr3old=57
hr6old=54
hr12old=48
hr24old=36
TCODE=W
if [ ${domtarg} = 'conus' ]
then
  cat ${base} |  sed s:_T1_:${t1targ}:g | sed s:_CHAR_:${TCODE}:g | sed s:_F_:${hr}:g | sed s:_FOLD_:${hrold}:g \
            | sed s:_FOLD3_:${hr3old}:g | sed s:_FOLD6_:${hr6old}:g | sed s:_FOLD12_:${hr12old}:g | sed s:_FOLD24_:${hr24old}:g > grib2_${RUN}_${domtarg}_probf${hr}
else

 cat ${basealt} |  sed s:_T1_:${t1targ}:g | sed s:_CHAR_:${TCODE}:g | sed s:_F_:${hr}:g | sed s:_FOLD_:${hrold}:g \
            | sed s:_FOLD3_:${hr3old}:g | sed s:_FOLD6_:${hr6old}:g | sed s:_FOLD12_:${hr12old}:g | sed s:_FOLD24_:${hr24old}:g > grib2_${RUN}_${domtarg}_probf${hr}
fi

echo DONE 24h
done

