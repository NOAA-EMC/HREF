#! /bin/sh

# ENSPOST_FIX=/lfs/h2/emc/lam/noscrub/Jun.Du/rrfs.v1.0.0/fix
ENSPOST_FIX=/lfs/h2/emc/lam/noscrub/Matthew.Pyle/rrfs_enspost_fix



mkdir -p ../fix

cd ../fix

# ln -sf ${ENSPOST_FIX}/* .

cp ${ENSPOST_FIX}/* .

cd ../sorc/



