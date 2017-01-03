#!/bin/bash

HOSTNAME='hostname -f'
export ACCNR=nems
ACCNR=${ACCNR:-null}

case `$HOSTNAME` in

  g10a1.ncep.noaa.gov)               MACHINE_ID=wcoss ;; ### gyre 1
  g10a2.ncep.noaa.gov)               MACHINE_ID=wcoss ;; ### gyre 2
  g14a1.ncep.noaa.gov)               MACHINE_ID=wcoss ;; ### gyre 3
  g14a2.ncep.noaa.gov)               MACHINE_ID=wcoss ;; ### gyre 4

  t10a1.ncep.noaa.gov)               MACHINE_ID=wcoss ;; ### tide 1
  t10a2.ncep.noaa.gov)               MACHINE_ID=wcoss ;; ### tide 2
  t14a1.ncep.noaa.gov)               MACHINE_ID=wcoss ;; ### tide 3
  t14a2.ncep.noaa.gov)               MACHINE_ID=wcoss ;; ### tide 4

  gaea1.ncrc.gov)                    MACHINE_ID=gaea ;; ### gaea1
  gaea2.ncrc.gov)                    MACHINE_ID=gaea ;; ### gaea2
  gaea3.ncrc.gov)                    MACHINE_ID=gaea ;; ### gaea3
  gaea4.ncrc.gov)                    MACHINE_ID=gaea ;; ### gaea4
  gaea5.ncrc.gov)                    MACHINE_ID=gaea ;; ### gaea5
  gaea6.ncrc.gov)                    MACHINE_ID=gaea ;; ### gaea6
  gaea7.ncrc.gov)                    MACHINE_ID=gaea ;; ### gaea7
  gaea8.ncrc.gov)                    MACHINE_ID=gaea ;; ### gaea8
  gaea9.ncrc.gov)                    MACHINE_ID=gaea ;; ### gaea9
  gaea10.ncrc.gov)                   MACHINE_ID=gaea ;; ### gaea10

  fe1.zeus.fairmont.rdhpcs.noaa.gov) MACHINE_ID=zeus ;; ### zeus1
  fe2.zeus.fairmont.rdhpcs.noaa.gov) MACHINE_ID=zeus ;; ### zeus2
  fe3.zeus.fairmont.rdhpcs.noaa.gov) MACHINE_ID=zeus ;; ### zeus3
  fe4.zeus.fairmont.rdhpcs.noaa.gov) MACHINE_ID=zeus ;; ### zeus4
  fe5.zeus.fairmont.rdhpcs.noaa.gov) MACHINE_ID=zeus ;; ### zeus5
  fe6.zeus.fairmont.rdhpcs.noaa.gov) MACHINE_ID=zeus ;; ### zeus6
  fe7.zeus.fairmont.rdhpcs.noaa.gov) MACHINE_ID=zeus ;; ### zeus7
  fe8.zeus.fairmont.rdhpcs.noaa.gov) MACHINE_ID=zeus ;; ### zeus8

esac

echo "Machine: " $MACHINE_ID "    Account: " $ACCNR

#return 2>/dev/null || exit 1

# --- for Zeus, find available account ID
  AP=account_params          # Account info
if [ ${MACHINE_ID} = zeus  ]; then
if [ ${ACCNR:-null} = null ]; then

  ac=`$AP 2>&1 | grep '^\s*Allocation: [0-9]' | awk '$4>100{print $3}'| head -1`
  nr=`echo $ac|wc -w`

  if [ $nr -eq 1 ]; then
    ACCNR=$ac
    echo "Found a valid account: using $ac"
  else
    ac=`$AP 2>&1 | grep '^\s*Allocation: [0-9]' | awk '{print $3}'| head -1`
    nr=`echo $ac|wc -w`
    if [ $nr -eq 1 ]; then
      ACCNR=$ac
      echo "Could not an find account with positive balance: using $ac"
      echo "NOTE: Will run in windfall; longer wait times, be patient!"
    else
      echo "Check your account ID; No compute allocations found"
    fi
  fi
else
  cphr=`$AP 2>&1 | grep '^\s*Allocation: [0-9]' | grep $ACCNR | awk '{print $4}'`
  nr=`echo $cphr|wc -w`
  if [ $nr -eq 0 ]; then echo 'Wrong account choice: ' $ACCNR ; exit ; fi
  echo "Account: " $ACCNR", available: " $cphr " CPU hrs"
fi
fi
