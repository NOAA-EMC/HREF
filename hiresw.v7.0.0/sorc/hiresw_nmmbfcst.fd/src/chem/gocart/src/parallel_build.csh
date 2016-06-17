#!/bin/tcsh -f
#------------------------------------------------------------------------
# name: parallel_build.csh
# purpose: This subroutine builds using pinstall
#          for compilation parallelization.
#
# !REVISION HISTORY
# 15Mar2007  Stassi  Modified version of g5das_parallel_build.csh
# 22Mar2007  Kokron  Add support for discover
# 28Mar2007  Stassi  Attempt to find default group for job submittal
# 13Apr2007  Stassi  Change order in which LOG/info names get iterated
# 04May2007  Stassi  Hard-code NCPUS=4 for discover
# 10Aug2007  TO/JS   Added distclean option; reordered script
# 15Apr2008  Stassi  Added code to cd to src directory, if not already
#                    there. Rename ESMASRC variable to ESMADIR
# 16Apr2008  Stassi  Use only half of CPUs on palm; mpi needs more memory
# 30Mar2008  Stassi  Add runtime options: -h, -hdf, -q queue
# 13Apr2009  Stassi  Updated for pleiades
# 05Aug2009  Stassi  Added -debug and -tmpdir flags
# 19Aug2010  Stassi  Added -walltime flag
# 01Apr2011  Stassi  Added clean and realclean options
#------------------------------------------------------------------------
set name = $0
set scriptname = $name
set BUILD_LOG_DIR = BUILD_LOG_DIR

setenv ARCH `uname -s`
#--set time = (1000 "%Uu %Ss %E  %X+%Dk %Mk %I+%Oio %Fpf+%Ww")
set time = ( 1000 "%Uu %Ss %E" )
set NCPUs_min = 6

#=====================
# determine the site 
#=====================
setenv SITE undetermined

set node = `uname -n`
if (($node == courant)      \
      || ($node == dirac)   \
      || ($node == jimpf)   \
      || ($node == mintz)   \
      || ($node == palm)    \
      || ($node == tropic)  \
      || ($node =~ e?)      \
      || ($node =~ borg*)   \
      || ($node =~ dali*)   \
      || ($node =~ warp*)   \
      || ($node =~ discover*)) then
   setenv SITE nccs

else if (($node == bs%)) then
   setenv SITE ncar

else if (($node =~ cerebus*)  \
      || ($node =~ nsipp02*)  \
      || ($node =~ ping-ge*)  \
      || ($node =~ boygo*)) then
   setenv SITE nsipp

else if (($node =~ pfe*)      \
      || ($node =~ bridge*)   \
      || ($node =~ r[0-9]*i[0-9]*n[0-9]*)) then
   setenv SITE nas
endif

# if batch, then skip over job submission
#----------------------------------------
if ($?Parallel_build_bypass_flag) then
   goto build
endif

# change to src directory, if not already there
#----------------------------------------------
if ($name != $name:t) then
   set scriptname = $name:t
   cd $name:h
endif
set srcdir = `pwd`

# set defaults
#-------------
setenv ddb    0
setenv debug  ""
setenv interactive 0
setenv optflg ""
setenv proc   ""
setenv prompt 1
setenv queue  ""
setenv sdflag "ESMA_SDF=netcdf"
setenv tmpdir ""
setenv walltime ""

# check for runtime parameters
#-----------------------------
@ n = 0
while ($#argv)

   # usage information
   #------------------
   if ("$1" == "-help") goto usage
   if ("$1" == "-h"   ) goto usage

   # compile for HDF
   #----------------
   if ("$1" == "-hdf") setenv sdflag "ESMA_SDF=hdf"

   # developer's debug
   #------------------
   if ("$1" == "-ddb") set ddb = 1

   # compile with debug
   #-------------------
   if (("$1" == "-debug") || ("$1" == "-db")) then
      if ("$optflg" == "") then
         setenv debug "BOPT=g"
         setenv optflg ""
      endif
   endif

   # compile with GPU (if using pgfortran compiler)
   #-----------------------------------------------
   if ("$1" == "-gpu") then
      set GPU = 1
   endif

   # specify node type
   #------------------
   if ("$1" == "-ivy")  set nodeTYPE = "IvyBridge"
   if ("$1" == "-sand") set nodeTYPE = "SandyBridge"
   if ("$1" == "-west") set nodeTYPE = "Westmere"

   # set optimization flags
   #-----------------------
   if (("$1" == "-opt") && ($debug == "")) then
      shift; if (! $#argv) goto usage
      set optval = "$1"
      if (($optval != 1) && ($optval != 2) \
        && ($optval != 3) && ($optval != 4) && ($optval != 5)) goto usage
      setenv optflg "FOPT=-O$optval COPT=-O$optval"
   endif

   # reset Fortran TMPDIR
   #---------------------
   if ("$1" == "-tmpdir") then
      shift; if (! $#argv) goto usage
      setenv tmpdir $1
   endif

   # specify Fortran compiler
   #-------------------------
   if ("$1" =~ *=*) then
      set var = `echo $1 | cut -d= -f1`
      set val = `echo $1 | cut -d= -f2`
      if ($var == "ESMA_FC") setenv ESMA_FC $val
   endif

   # run job interactively
   #----------------------
   if ("$1" == "-i") set interactive = 1

   # submit pbs job to alternative queue
   #------------------------------------
   if ("$1" == "-q") then
      shift; if (! $#argv) goto usage
      setenv queue "-q $1"
   endif

   # set PBS walltime
   #-----------------
   if ("$1" == "-walltime") then
      shift; if (! $#argv) goto usage
      setenv walltime $1

      set hr = `echo $walltime | cut -d: -f1`
      set ss = `echo $walltime | cut -d: -f3`
      if (("$hr" == "$walltime") || ("$ss" == "")) then
         echo ""
         echo walltime must be in hr:mm:ss format
         goto usage
      endif
   endif

   # set noprompt option
   #--------------------
   if (("$1" == "-np") || ("$1" == "-noprompt")) then
      setenv prompt 0
   endif
   shift
end

# set BOPT=GPU if appropriate
#----------------------------
if ($?GPU && $?ESMA_FC) then
   if ("$ESMA_FC" == "pgfortran") setenv debug "BOPT=GPU"
endif

# default nodeTYPE
#-----------------
if (! $?nodeTYPE) then
   if ($SITE == nccs) set nodeTYPE = "Westmere"
   if ($SITE == nas)  set nodeTYPE = "IvyBridge"
endif

# at NCCS
#--------
if ($SITE == nccs) then
   set nT = `echo $nodeTYPE| tr "[A-Z]" "[a-z]" | cut -c1-4 `
   if (($nT != west) && ($nT != sand)) then
      echo "ERROR. Unknown node type at NCCS: $nodeTYPE"
      exit
   endif
   set proc = ":proc=$nT"
   if ($nT == west) @ NCPUS_DFLT = 12
   if ($nT == sand) @ NCPUS_DFLT = 16
endif

# at NAS
#-------
if (($node =~ pfe*) || ($node =~ p4fe*) ) then
   set nT = `echo $nodeTYPE | cut -c1-3 | tr "[A-Z]" "[a-z]"`
   if (($nT != wes) && ($nT != san) && ($nT != ivy)) then
      echo "ERROR. Unknown node type at NAS: $nodeTYPE"
      exit
   endif
   set proc = ":model=$nT"
   if ($nT == wes) @ NCPUS_DFLT = 12
   if ($nT == san) @ NCPUS_DFLT = 16
   if ($nT == ivy) @ NCPUS_DFLT = 20

   # TMPDIR needs to be reset
   #-------------------------
   if (! "$tmpdir") then
      set tmpdirDFLT = "/nobackup/$USER/scratch/"
      if ($prompt) then
         echo ""
         echo -n "Define TMPDIR location "
         echo    "where scratch files can be written during build."
         echo -n "TMPDIR [$tmpdirDFLT] "
         setenv tmpdir $<
         if ("$tmpdir" == "") setenv tmpdir $tmpdirDFLT
      endif
   endif
   echo "TMPDIR: $tmpdir"

endif

# developer's debug
#------------------
if ($ddb) then
   echo "sdflag = $sdflag"
   echo "debug = $debug"
   if ($?nodeTYPE) then
      echo "nodeTYPE = $nodeTYPE"
   endif
   echo "optflg = $optflg"
   echo "tmpdir = $tmpdir"
   if ($?ESMA_FC) then
      echo "ESMA_FC = $ESMA_FC"
   endif
   echo "proc = $proc"
   echo "interactive = $interactive"
   echo "queue = $queue"
   echo "walltime = $walltime"
   echo "prompt = $prompt"
   echo "NCPUS_DFLT = $NCPUS_DFLT"
   exit
endif

# if user defines TMPDIR ...
#---------------------------
if ("$tmpdir" != "") then

   # ... mkdir if it does not exist
   #-------------------------------
   if (! -d $tmpdir) then
      echo "Making TMPDIR directory: $tmpdir"
      mkdir -p $tmpdir
      if ($status) then
         echo ">> Error << mkdir $tmpdir "
         exit
      endif
   endif

   # ... check that it is writeable
   #-------------------------------
   if (! -w $tmpdir) then
      echo ">> Error << TMPIR is not writeable: $tmpdir"  
      exit
   endif
   echo ""

endif

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                            JOB SUBMISSION
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# set environment variables
#--------------------------
source $srcdir/g5_modules
setenv ESMADIR $srcdir:h
setenv Pbuild_source_directory $srcdir
setenv Parallel_build_bypass_flag
set jobname = "parallel_build"

#========
# intro
#========
echo ""
echo "   ================"
echo "    PARALLEL BUILD "
echo "   ================"

#===========================
# query for number of CPUs
#===========================
if ( ($node =~ discover*) || ($node =~ dali*) \
  || ($node =~ pfe*)      || ($node =~ p4fe*) ) then

   # use set number of CPUs on discover/dali and pleiades
   #-----------------------------------------------------
   @ ncpus_val  = $NCPUS_DFLT
   @ numjobs_val  = $ncpus_val - 2  # save some CPUs for memory
   echo ""
   echo "The build will proceed with $numjobs_val parallel processes on $ncpus_val CPUs."

else

   # how many?
   #----------
   echo ""
   echo -n "Parallel build using how many CPUs ($NCPUs_min minimum)? "
   echo -n "[$NCPUS_DFLT] "
   set ncpus_val = $<
   if ("$ncpus_val" == "") set ncpus_val = $NCPUS_DFLT
   echo ""

   # check for minimum number of CPUs
   #---------------------------------
   if ($ncpus_val < $NCPUs_min) then
      @ ncpus_val = $NCPUs_min
      echo "Defaulting to minimum $ncpus_val CPUs."
   endif

   # use only even number of CPUs
   #-----------------------------
   @ check = ( $ncpus_val / 2 ) * 2
   if ($check != $ncpus_val) then
      @ ncpus_val ++
      echo "Rounding up to an even $ncpus_val CPUs."
   endif

   # save some CPUs for memory
   #--------------------------
   @ numjobs_val = $ncpus_val / 2
   echo -n "The build will proceed with $numjobs_val parallel processes"
   echo    " on $ncpus_val CPUs."

endif

setenv ncpus   $ncpus_val
setenv numjobs $numjobs_val

#=================================================
# check for LOG and info files from previous build
#=================================================
setenv bldlogdir $Pbuild_source_directory/$BUILD_LOG_DIR
setenv buildlog  $bldlogdir/LOG
setenv buildinfo $bldlogdir/info
setenv cleanlog  $bldlogdir/cleanlog
setenv cleanFLAG ""

ls $buildlog $buildinfo >& /dev/null
if ($status == 0) then
   if ($prompt) then
      echo ''
      echo 'Previous build detected - Do you want to clean?'
      echo '(c)     clean: removes *.[aox] files + *.[Mm][Oo][Dd] files'
      echo '(d) distclean: "clean" + removes *.d files'
      echo '(r) realclean: "distclean" + removes'" src/$ARCH directory"
      echo '(n)  no clean'
      echo ''
      echo -n 'Select (c,d,r,n) <<d>> '

      set do_clean = $<
      if ("$do_clean" != "c" && "$do_clean" != "r" && "$do_clean" != "n") then
         set do_clean = "d"
      endif
   else
      set do_clean = "d"
   endif

   if ("$do_clean" == "c") then
      setenv cleanFLAG clean
      echo  "make clean before rebuild"
   else if ("$do_clean" == "d") then
      setenv cleanFLAG distclean
      echo  "make distclean before rebuild"
   else if ("$do_clean" == "r") then
      setenv cleanFLAG realclean
      echo  "make realclean before rebuild"
   else
      echo "No clean before rebuild"
   endif 
endif

#==============
# info for user
#==============
echo ""
echo "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
echo "LOG and info will be written to the src/$BUILD_LOG_DIR directory."
echo "Do the following to check the status/success of the build:"
echo ""
echo "  cd $BUILD_LOG_DIR"
echo "  ./gmh.pl [-v] LOG[.n]"
echo ""
echo "Note: Check the latest version of LOG for the most recent build info."
echo ""

# give option to build interactively if in interative PBS mode
#-------------------------------------------------------------
if ($?PBS_JOBID) then
   if ($prompt) then
      echo -n "Run build procedure interactively (y/n)? [n] "
      set dummy = $<
      set reponse = `echo $dummy | tr /A-Z/ /a-z/`
      if (($reponse == "y") || ($reponse == "yes")) goto build
   endif
endif

#=============
# submit job
#=============
set groupflag = ""
set sponsorflag = ""
if (-e `which getsponsor` && (! $interactive)) then
   set group = ""
   set sponsorinfo = `getsponsor`

   while ("$group" == "")
      @ n = 1
      while ($n < $#sponsorinfo)
         if ($sponsorinfo[$n] =~ g[0..9]*) then
            set group = $sponsorinfo[$n]
            #--break  # uncomment this line to make 1st entry the default
         endif
         @ n++
      end

      getsponsor
      if ("$group" != "") then
         echo -n "select group: [$group] "
      else
         echo -n "select group:  "
      endif
      if ($prompt) then
         set reponse = $<
         if ("$reponse" != "") set group = $reponse
      endif
   end

   set groupflag = "-W group_list=$group"
   set sponsorflag = "-P $group"
endif

if ($interactive) then
   @ numjobs = $numjobs - 2
   goto build
else if ($node == "palm") then
   if ("$walltime" == "") then
      setenv walltime = "1:00:00"
      if ($ncpus < 8) setenv walltime "2:00:00"
   endif

   set echo
   qsub  $groupflag $queue     \
        -N $jobname            \
        -l ncpus=$ncpus        \
        -l walltime=$walltime  \
        -S /bin/csh            \
        -V -j oe               \
        $0
   unset echo
   sleep 1
   qstat -a | grep $USER
else if ( $SITE == nas ) then
   if ("$walltime" == "") setenv walltime "1:00:00"
   set echo
   qsub  $groupflag $queue     \
        -N $jobname            \
        -l select=1:ncpus=${ncpus}:mpiprocs=${numjobs}$proc \
        -l walltime=$walltime  \
        -S /bin/csh            \
        -V -j oe               \
        $0
   unset echo
   sleep 1
   qstat -a | grep $USER
else if ( $SITE == nccs ) then
   if ("$walltime" == "") setenv walltime "1:00:00"
   set echo
   qsub  $groupflag $queue     \
        -N $jobname            \
        -o $jobname.o%j        \
        -l select=1:ncpus=${ncpus}:mpiprocs=${numjobs}$proc \
        -l walltime=$walltime  \
        -V                     \
        $0
   unset echo
   sleep 1
   qstat -a | grep $USER
else if ($node =~ halem?) then
   set echo
   bsub  $sponsorflag \
        -J $jobname            \
        -n $ncpus              \
        -o ${jobname}.log      \
        $0
   unset echo
else
   echo $scriptname": batch procedures are not yet defined for node=$node"
endif
exit



build:
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                             BUILD SYSTEM
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
set SRCDIR = $Pbuild_source_directory
chdir $SRCDIR

setenv ESMADIR $SRCDIR:h
setenv ARCH `uname -s`

#=================================================
# create $BUILD_LOG_DIR, plus LOG and info files
#=================================================

# create build log directory
#---------------------------
if (! -d $bldlogdir) mkdir -p $bldlogdir

# copy gmh.pl script to build log directory
#------------------------------------------
if ((! -e $bldlogdir/gmh.pl) && (-e $SRCDIR/Config/gmh.pl)) then
   \cp $SRCDIR/Config/gmh.pl $bldlogdir
endif

# increment LOG and info files if previous versions exist
#--------------------------------------------------------
ls $buildlog $buildinfo >& /dev/null
if ($status == 0) then
   @ next = 0
   foreach file ( $buildlog $buildinfo $cleanlog )
      @ num = 1
      while (-e ${file}.$num)
         @ num ++
      end
      if ($num > $next) @ next = $num
   end
   set buildinfo = $bldlogdir/info.$next
   set buildlog  = $bldlogdir/LOG.$next
   set cleanlog  = $bldlogdir/cleanlog.$next
endif

# alias definitions
#------------------
alias echo2 "echo \!* |& tee    $buildinfo $buildlog"
alias echo1 "echo \!* |& tee -a $buildinfo"
alias date1 "date     |& tee -a $buildinfo"

# initialize files
#-----------------
echo "Writing LOG and info files to directory: $bldlogdir:t"
echo2 ""

#================
# set environment  
#================
source $SRCDIR/g5_modules

$SRCDIR/Assert.pl >>& $buildinfo
$SRCDIR/Assert.pl
if ($status) exit 2

# write environment info to build log
#------------------------------------
set cvstag = "tag-unknown"
if (-e $SRCDIR/CVS/Tag) set cvstag = `cat $SRCDIR/CVS/Tag | cut -dN -f2-`
if ("$tmpdir" != "") setenv TMPDIR $tmpdir
echo1 "======================================"
echo1 `uname -a`
echo1 "ESMADIR: $ESMADIR"
echo1 "BASEDIR: $BASEDIR"
echo1 "CVSTAG: $cvstag"
echo1 "SITE: $SITE"
echo1 "$sdflag"
echo1 "TMPDIR = $TMPDIR"
if ($?ESMA_FC) then
   echo1 "ESMA_FC: $ESMA_FC"
endif
if ("$debug" != "") then
   echo1 "debug: $debug"
endif
if ("$optflg" != "") then
   echo1 "optimization: $optflg"
endif
if ("$queue" != "") then
   echo1 "queue: $queue"
endif
$SRCDIR/Config/OScheck.pl -q >> $buildinfo

echo1 "======================================"
module list >>& $buildinfo
echo1 "======================================"
echo1 ""
echo1 "The following commands will check the status/success of the build:"
echo1 ""
echo1 "  cd $bldlogdir"
echo1 "  ./gmh.pl [-v] $buildlog:t"

#===============
# build system
#===============
if ( $cleanFLAG != "" ) then
    echo1 ""
    echo1 "--------------------------------------"
    date1
    echo1  "make $cleanFLAG"
    make $cleanFLAG $sdflag |& tee -a $cleanlog
    set buildstatus = $status
    echo1 "clean complete; status = $buildstatus"
    date1
    echo1 "--------------------------------------"
    time >> $buildinfo
endif

set cmd = "make $sdflag $debug$optflg --jobs=$numjobs pinstall"
echo1 "" 
echo1 ""
echo1 "--------------------------------------"
echo1 $cmd
date1
echo1  "Parallel build: $numjobs JOBS on $ncpus CPUs ... "
set echo
$cmd |& tee -a $buildlog
set buildstatus = $status

echo1 "build complete; status = $buildstatus"
date1
echo1 "--------------------------------------"
time >> $buildinfo
echo1 ""

# check build results
#--------------------
chdir $bldlogdir
ls $buildlog >& /dev/null
set logstatus = $status

if (($logstatus == 0) && (-e gmh.pl)) then
   ./gmh.pl -A $buildlog
   ./gmh.pl -A $buildlog >> $buildinfo
else
   echo "${scriptname}: >>> WARNING <<< Unable to assess success of build."
   echo "Check for existence of files, $buildlog and gmh.pl, in $bldlogdir."
endif

exit $buildstatus

usage:
cat <<EOF

usage: $0:t [ESMA_FC=fc] [flagged options]
where
    fc                 user-specified Fortran compiler

flagged options
   -debug (or -db)     compile with debug flags (BOPT=g)
   -gpu                compile with BOPT=GPU (only valid with ESMA_FC=pgfortran)
   -help (or -h)       echo usage information
   -hdf                compile with hdf libraries
   -i                  run interactively rather than queuing job
   -opt n              set optimization flag to n, where n={1,2,3,4,5}
   -q queue            send PBS job to queue
   -np                 do not prompt for responses; use defaults
   -tmpdir dir         alternate Fortran TMPDIR location
   -walltime hh:mm:ss  time to use as PBS walltime at job submittal

   -ivy                compile on Ivy Bridge nodes (NAS only)
   -sand               compile on SandyBridge nodes
   -west               compile on Westmere nodes (default)
EOF
