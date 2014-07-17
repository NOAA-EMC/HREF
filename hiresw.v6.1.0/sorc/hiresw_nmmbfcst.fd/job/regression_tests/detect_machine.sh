HOSTNAME='hostname -f' ; if [ `uname` = AIX ]; then HOSTNAME='hostname' ; fi

case `$HOSTNAME` in

  c1n6.ncep.noaa.gov)                MACHINE_ID=ccs ;;     ### cirrus1
  c2n6.ncep.noaa.gov)                MACHINE_ID=ccs ;;     ### cirrus2

  s1n6.ncep.noaa.gov)                MACHINE_ID=ccs ;;     ### stratus1
  s2n6.ncep.noaa.gov)                MACHINE_ID=ccs ;;     ### stratus2

  g10a1.ncep.noaa.gov)               MACHINE_ID=wcoss ;;   ### gyre 1
  g10a2.ncep.noaa.gov)               MACHINE_ID=wcoss ;;   ### gyre 2
  g14a1.ncep.noaa.gov)               MACHINE_ID=wcoss ;;   ### gyre 3
  g14a2.ncep.noaa.gov)               MACHINE_ID=wcoss ;;   ### gyre 4

  t10a1.ncep.noaa.gov)               MACHINE_ID=wcoss ;;   ### tide 1
  t10a2.ncep.noaa.gov)               MACHINE_ID=wcoss ;;   ### tide 2
  t14a1.ncep.noaa.gov)               MACHINE_ID=wcoss ;;   ### tide 3
  t14a2.ncep.noaa.gov)               MACHINE_ID=wcoss ;;   ### tide 4

  gaea1.ncrc.gov)                    MACHINE_ID=gaea ;;    ### gaea1
  gaea2.ncrc.gov)                    MACHINE_ID=gaea ;;    ### gaea2
  gaea3.ncrc.gov)                    MACHINE_ID=gaea ;;    ### gaea3
  gaea4.ncrc.gov)                    MACHINE_ID=gaea ;;    ### gaea4
  gaea5.ncrc.gov)                    MACHINE_ID=gaea ;;    ### gaea5
  gaea6.ncrc.gov)                    MACHINE_ID=gaea ;;    ### gaea6
  gaea7.ncrc.gov)                    MACHINE_ID=gaea ;;    ### gaea7
  gaea8.ncrc.gov)                    MACHINE_ID=gaea ;;    ### gaea8
  gaea9.ncrc.gov)                    MACHINE_ID=gaea ;;    ### gaea9
  gaea10.ncrc.gov)                   MACHINE_ID=gaea ;;    ### gaea10

  fe1.zeus.fairmont.rdhpcs.noaa.gov) MACHINE_ID=zeus ;;    ### zeus1
  fe2.zeus.fairmont.rdhpcs.noaa.gov) MACHINE_ID=zeus ;;    ### zeus2
  fe3.zeus.fairmont.rdhpcs.noaa.gov) MACHINE_ID=zeus ;;    ### zeus3
  fe4.zeus.fairmont.rdhpcs.noaa.gov) MACHINE_ID=zeus ;;    ### zeus4
  fe5.zeus.fairmont.rdhpcs.noaa.gov) MACHINE_ID=zeus ;;    ### zeus5
  fe6.zeus.fairmont.rdhpcs.noaa.gov) MACHINE_ID=zeus ;;    ### zeus6
  fe7.zeus.fairmont.rdhpcs.noaa.gov) MACHINE_ID=zeus ;;    ### zeus7
  fe8.zeus.fairmont.rdhpcs.noaa.gov) MACHINE_ID=zeus ;;    ### zeus8

esac
echo $MACHINE_ID


# --- for Zeus, find available account ID
if [ ${MACHINE_ID} = zeus ]; then
  clear
  ACCNR=null
  for i in cmp gefs omd nems rm gm ada
  do
    printf %s "Looking for account ID: " $i " ..."
    nr=`account_params 2>&1 | grep -v Initial | grep " "$i" " | wc -l`

    if [ $nr -eq 2 ]; then
      ACCNR=$i ; echo OK
    else
      echo
    fi
  done
  if [ $ACCNR = null ]; then echo "Check your account ID"; exit ; fi
  clear
fi
