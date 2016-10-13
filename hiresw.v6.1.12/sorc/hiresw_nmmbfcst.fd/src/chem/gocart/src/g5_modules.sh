#!/usr/bin/sh
#=======================================================================
# name - g5_modules.sh
# purpose - 
#   This script can be "sourced" from the bash shell to set environment
#   variables and modules needed for building and running the GEOS system.
#
# Notes:
# 1. This script needs to be located in the same directory as g5_modules.
# 2. This script calls g5_modules with the "sh" argument and then sources
#    the file, .g5_modules.sh, which is written by g5_modules.
# 3. Once .g5_modules.sh has been written, it can be sourced directly,
#    though it is always safer to source this script instead, because
#    .g5_modules.sh is not automatically updated when the parameters in
#    the g5_modules file are modified, whereas using this script will
#    always access the latest values in g5_modules.
#
#  REVISION HISTORY
#  24Aug2012  Stassi   Initial version of code
#=======================================================================
DIR=$( cd "$( dirname "$BASH_SOURCE" )" && pwd )

g5modules="$DIR/g5_modules"
if [ ! -e $g5modules ]; then
   echo
   echo "Error. Cannot find $g5modules"
   echo
   return
fi

cd $DIR
$g5modules sh
source .g5_modules.sh
