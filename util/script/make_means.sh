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
base=grib2_${RUN}_conus_mean_3h
#########################################################

hr=3
hrold=2
hr3old=0
TCODE=D

cat ${base} | sed s:_T1_:${t1targ}:g | sed s:_CHAR_:${TCODE}:g | sed s:_F_:${hr}:g | sed s:_FOLD_:${hrold}:g \
            | sed s:_FOLD3_:${hr3old}:g > grib2_${RUN}_${domtarg}_meanf0${hr}

hr=6
hrold=5
hr3old=3
TCODE=G
cat ${base} | sed s:_T1_:${t1targ}:g | sed s:_CHAR_:${TCODE}:g | sed s:_F_:${hr}:g | sed s:_FOLD_:${hrold}:g \
            | sed s:_FOLD3_:${hr3old}:g > grib2_${RUN}_${domtarg}_meanf0${hr}

hr=9
hrold=8
hr3old=6
TCODE=J
cat ${base} | sed s:_T1_:${t1targ}:g | sed s:_CHAR_:${TCODE}:g | sed s:_F_:${hr}:g | sed s:_FOLD_:${hrold}:g \
            | sed s:_FOLD3_:${hr3old}:g  > grib2_${RUN}_${domtarg}_meanf0${hr}


hr=12
hrold=11
hr3old=9
TCODE=M
cat ${base} | sed s:_T1_:${t1targ}:g |  sed s:_CHAR_:${TCODE}:g | sed s:_F_:${hr}:g | sed s:_FOLD_:${hrold}:g \
            | sed s:_FOLD3_:${hr3old}:g > grib2_${RUN}_${domtarg}_meanf${hr}

hr=15
hrold=14
hr3old=12
TCODE=P
cat ${base} | sed s:_T1_:${t1targ}:g | sed s:_CHAR_:${TCODE}:g | sed s:_F_:${hr}:g | sed s:_FOLD_:${hrold}:g \
            | sed s:_FOLD3_:${hr3old}:g > grib2_${RUN}_${domtarg}_meanf${hr}

hr=18
hrold=17
hr3old=15
TCODE=S
cat ${base} | sed s:_T1_:${t1targ}:g | sed s:_CHAR_:${TCODE}:g | sed s:_F_:${hr}:g | sed s:_FOLD_:${hrold}:g \
            | sed s:_FOLD3_:${hr3old}:g > grib2_${RUN}_${domtarg}_meanf${hr}

hr=21
hrold=20
hr3old=18
TCODE=Z
cat ${base} | sed s:_T1_:${t1targ}:g |  sed s:_CHAR_:${TCODE}:g | sed s:_F_:${hr}:g | sed s:_FOLD_:${hrold}:g \
            | sed s:_FOLD3_:${hr3old}:g > grib2_${RUN}_${domtarg}_meanf${hr}

hr=24
hrold=23
hr3old=21
TCODE=T
cat ${base} | sed s:_T1_:${t1targ}:g | sed s:_CHAR_:${TCODE}:g | sed s:_F_:${hr}:g | sed s:_FOLD_:${hrold}:g \
            | sed s:_FOLD3_:${hr3old}:g > grib2_${RUN}_${domtarg}_meanf${hr}

hr=27
hrold=26
hr3old=24
TCODE=Z
cat ${base} | sed s:_T1_:${t1targ}:g | sed s:_CHAR_:${TCODE}:g | sed s:_F_:${hr}:g | sed s:_FOLD_:${hrold}:g \
            | sed s:_FOLD3_:${hr3old}:g  > grib2_${RUN}_${domtarg}_meanf${hr}

hr=30
hrold=29
hr3old=27
TCODE=Z
cat ${base} | sed s:_T1_:${t1targ}:g | sed s:_CHAR_:${TCODE}:g | sed s:_F_:${hr}:g | sed s:_FOLD_:${hrold}:g \
            | sed s:_FOLD3_:${hr3old}:g  > grib2_${RUN}_${domtarg}_meanf${hr}

hr=33
hrold=32
hr3old=30
TCODE=Z
cat ${base} | sed s:_T1_:${t1targ}:g | sed s:_CHAR_:${TCODE}:g | sed s:_F_:${hr}:g | sed s:_FOLD_:${hrold}:g \
            | sed s:_FOLD3_:${hr3old}:g > grib2_${RUN}_${domtarg}_meanf${hr}

hr=36
hrold=35
hr3old=33
TCODE=U
cat ${base} | sed s:_T1_:${t1targ}:g | sed s:_CHAR_:${TCODE}:g | sed s:_F_:${hr}:g | sed s:_FOLD_:${hrold}:g \
            | sed s:_FOLD3_:${hr3old}:g  > grib2_${RUN}_${domtarg}_meanf${hr}

hr=39
hrold=38
hr3old=36
TCODE=Z
cat ${base} | sed s:_T1_:${t1targ}:g | sed s:_CHAR_:${TCODE}:g | sed s:_F_:${hr}:g | sed s:_FOLD_:${hrold}:g \
            | sed s:_FOLD3_:${hr3old}:g > grib2_${RUN}_${domtarg}_meanf${hr}

hr=42
hrold=41
hr3old=39
TCODE=Z
cat ${base} | sed s:_T1_:${t1targ}:g |  sed s:_CHAR_:${TCODE}:g | sed s:_F_:${hr}:g | sed s:_FOLD_:${hrold}:g \
            | sed s:_FOLD3_:${hr3old}:g  > grib2_${RUN}_${domtarg}_meanf${hr}

hr=45
hrold=44
hr3old=42
TCODE=Z
cat ${base} | sed s:_T1_:${t1targ}:g | sed s:_CHAR_:${TCODE}:g | sed s:_F_:${hr}:g | sed s:_FOLD_:${hrold}:g \
            | sed s:_FOLD3_:${hr3old}:g > grib2_${RUN}_${domtarg}_meanf${hr}

hr=48
hrold=47
hr3old=45
TCODE=V
cat ${base} | sed s:_T1_:${t1targ}:g | sed s:_CHAR_:${TCODE}:g | sed s:_F_:${hr}:g | sed s:_FOLD_:${hrold}:g \
            | sed s:_FOLD3_:${hr3old}:g  > grib2_${RUN}_${domtarg}_meanf${hr}

# are all beyond f48 Z time code?
# hr=51
# hrold=50
# hr3old=48
# TCODE=Z
# cat ${base} | sed s:_CHAR_:${TCODE}:g | sed s:_F_:${hr}:g | sed s:_FOLD_:${hrold}:g \
#             | sed s:_FOLD3_:${hr3old}:g > grib2_${RUN}_${domtarg}_meanf${hr}

hr=54
hrold=53
hr3old=51
TCODE=Z
cat ${base} | sed s:_T1_:${t1targ}:g | sed s:_CHAR_:${TCODE}:g | sed s:_F_:${hr}:g | sed s:_FOLD_:${hrold}:g \
            | sed s:_FOLD3_:${hr3old}:g > grib2_${RUN}_${domtarg}_meanf${hr}

# hr=57
# hrold=56
# hr3old=54
# TCODE=Z
# cat ${base} | sed s:_T1_:${t1targ}:g | sed s:_CHAR_:${TCODE}:g | sed s:_F_:${hr}:g | sed s:_FOLD_:${hrold}:g \
#            | sed s:_FOLD3_:${hr3old}:g > grib2_${RUN}_${domtarg}_meanf${hr}

hr=60
hrold=59
hr3old=57
TCODE=W
cat ${base} | sed s:_T1_:${t1targ}:g | sed s:_CHAR_:${TCODE}:g | sed s:_F_:${hr}:g | sed s:_FOLD_:${hrold}:g \
            | sed s:_FOLD3_:${hr3old}:g > grib2_${RUN}_${domtarg}_meanf${hr}

done
