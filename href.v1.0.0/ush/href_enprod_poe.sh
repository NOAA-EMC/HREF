#!/bin/ksh
#
#BSUB -a poe
#BSUB -n FHR
#BSUB -x
#BSUB -oo RUNDIR/out.poe_ensprod
#BSUB -eo RUNDIR/err.poe_ensprod
#BSUB -R "span[ptile=1]" 
#BSUB -R "affinity[core]" 
#BSUB -J "href_poe_FHR"
#BSUB -W 00:30
#BSUB -q "RUNENVIR"
#BSUB -P HRW-T2O

export MP_CMDFILE=RUNDIR/run_ensprod_poe_ALL.sh
export MP_PGMMODEL=mpmd
export MP_EUILIB=us
export MP_LABELIO=YES
export MP_INFOLEVEL=3 
export MP_PGMMODEL=mpmd
mpirun.lsf 

echo "DONE" > RUNDIR/donepoe_PIECE
