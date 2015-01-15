#! /bin/ksh

set -x
if [[ $# -lt 3 ]] ; then
  echo "Illegal number of parrameters: setup_mom_input.sh dst_dir ocn_data_dir ice_data_dir"
  exit 1
fi

dst_dir=$1
ocn_data_dir=$2
ice_data_dir=$3

if [[ ! -d $dst_dir ]] ; then
  echo "Destination direction $dst_dir does not exist"
  exit 2
fi
if [[ ! -d $ocn_data_dir ]] ; then
  echo "Ocean data input dir does not exist"
  exit 3
fi
if [[ ! -d $ice_data_dir ]] ; then
  echo "Ice data input dir does not exist"
  exit 3
fi

# clean up dst_dir if needed
rm -rf $dst_dir/RESTART/                                         
rm -rf $dst_dir/input.nml

# copy core forcing data set and grid file for mom5

cp -r $ocn_data_dir/INPUT/                $dst_dir
cp $ocn_data_dir/INPUT/input.nml          $dst_dir
cp $ocn_data_dir/INPUT/diag_table         $dst_dir
cp $ocn_data_dir/INPUT/field_table        $dst_dir
cp $ocn_data_dir/INPUT/data_table         $dst_dir
cp $ocn_data_dir/INPUT/grid_spec.nc       $dst_dir
#cp $ocn_data_dir/INPUT/ocean_hgrid.nc    $dst_dir
mkdir $dst_dir/RESTART
mkdir $dst_dir/restart
mkdir $dst_dir/history
cp -r $ice_data_dir/restart/*              $dst_dir/restart
cp -r $ice_data_dir/ice_in                 $dst_dir
cp -r $ice_data_dir/grid                   $dst_dir
cp -r $ice_data_dir/kmt                    $dst_dir
cp -r $ice_data_dir/global_gx3_gridspec.nc $dst_dir

exit 0
