#! /bin/csh
# @ job_type   = parallel
# @ environment = COPY_ALL;MP_EUILIB=us
# @ job_name   = regtest
# @ output     = regtest_out
# @ error      = regtest_err
# @ node       = 1,1
# @ network.MPI    = css0,shared,us
# @ tasks_per_node = 4
# @ node_usage = shared
# @ checkpoint = no
# @ wall_clock_limit = 21600
# @ class      = com_reg
# @ ja_report  = yes
# @ queue

#	This is a script to test the bit-for-bit reproducibility of
#	the WRF model, when comparing single processor serial runs to
#	OpenMP and MPI parallel runs.  There are several regression tests
#	that are performed.  

#	Approximate time for completion on 766 MHz ev67: 2.5 hours


#	These need to be changed for your particular set of runs.  This is
#	where email gets sent.

set FAIL_MAIL = ( michalak@ucar.edu )
set GOOD_MAIL = ( michalak@ucar.edu )

#	Get the command line input

set thedate = -999
set thefile = "null"

if ( `uname` == "AIX" ) then
   set argv=( -f testit.tar)
#   set argv=( -here )
endif

if ( $#argv == 0 ) then
	echo "Please enter either a date for cvs checkout. regtest.csh -D date"
	echo " or a file name containing WRF. regtest.csh -f tarfile"
	echo " or the -ftp flag for the script to pick code off anon ftp"
	exit
endif

set theargs = 0
foreach a ( $argv )
	if ( "$a" == "-D" ) then

		rsh -n maple.mmm.ucar.edu w >& /dev/null
		if ( $status ) then
			echo "Cannot execute a remote shell on maple.mmm.ucar.edu, where the"
			echo "WRF code resides."
			echo "Please check that it is up and that you have permission to rsh"
			echo "to this host. (Create a .rhosts file)."
			ping -c 1 maple.mmm.ucar.edu
			exit 2
		endif
		setenv CVSROOT maple.mmm.ucar.edu:/data7/mp/mesouser/WRF
		
		set acquire_from = "cvs"
		set thedate = $argv[2]

	endif

	if ( "$a" == "-f" ) then

		set thefile = $argv[2]
		#	Check for absolute path, if not, make it absolute
		echo $thefile | grep '^/' > /dev/null
		if ( $status != 0 ) set thefile = `pwd`/$thefile
		set acquire_from = "filearg"

	endif

	if( "$a" == "-ftp" ) then
		set acquire_from = "ftp"
	endif

	if( "$a" == "-here" ) then
		set acquire_from = "here"
	endif

end

set echo 
set date

set start = ( `date` )

#####################################################################

#	Initial set up values

set PHYSOPTS =	( 1 2 3 )

set OPENMP = 4

unalias cd cp rm ls pushd popd mv

set CUR_DIR = `pwd`
cat >! phys_1a << EOF
 num_soil_layers                     = 5,
EOF
cat >! phys_1b << EOF
 mp_physics                          = 3,
 ra_lw_physics                       = 1,
 ra_sw_physics                       = 1,
 bl_sfclay_physics                   = 1,
 bl_surface_physics                  = 1,
 bl_pbl_physics                      = 1,
 cu_physics                          = 1,
EOF

cat >! phys_2a << EOF
 num_soil_layers                     = 4,
EOF
cat >! phys_2b << EOF
 mp_physics                          = 4,
 ra_lw_physics                       = 1,
 ra_sw_physics                       = 2,
 bl_sfclay_physics                   = 2,
 bl_surface_physics                  = 2,
 bl_pbl_physics                      = 2,
 cu_physics                          = 2,
EOF

cat >! phys_3a << EOF
 num_soil_layers                     = 4,
EOF
cat >! phys_3b << EOF
 mp_physics                          = 5,
 ra_lw_physics                       = 1,
 ra_sw_physics                       = 2,
 bl_sfclay_physics                   = 2,
 bl_surface_physics                  = 2,
 bl_pbl_physics                      = 2,
 cu_physics                          = 2,
EOF

set dataset = jun01
set dataset = jan00

#####################################################################

#####################################################################

#	Set up info for particular architectures

set ARCH = ( `uname` )

set CORES=( eh_real em_real eh_b_wave eh_quarter_ss em_b_wave em_quarter_ss )

if      ( $ARCH[1] == OSF1 ) then
	set DEF_DIR		= /mmmtmp/$user
	set TMPDIR		= .
	set MAIL		= /usr/bin/mailx
	set COMPOPTS		= ( 1 2 3 )
	set MPIRUNCOMMAND 	= ( mpirun -np 4 )
else if ( $ARCH[1] == AIX ) then
	set DEF_DIR		= $home
	set TMPDIR		= /ptmp/$user
	set MAIL		= /usr/bin/mailx
	set COMPOPTS		= ( 1 3 4 )
	set MPIRUNCOMMAND 	= ( poe )
        setenv MP_SHARED_MEMORY yes
endif

#####################################################################

#	We may have everything that we need to start.

set ok=0
if ( -e ${DEF_DIR}/regression_test/time_stamp ) then
	grep `date -u +"%j"` ${DEF_DIR}/regression_test/time_stamp >& /dev/null
	set ok = $status
	if ( $ok == 0 ) then
		cd $DEF_DIR
		pushd regression_test
		goto GOT_TODAYS_WRF_SOURCE
	endif
endif

#	First of all, in which particular directory do we start.

cd $DEF_DIR
if ( ! -d regression_test ) then
  mkdir regression_test
  set ok = $status
endif
if ( $ok != 0 ) then
	echo "Gee, I cannot make a directory in $DEF_DIR"  >! error_message
	echo `pwd`                                         >> error_message
	echo `\ls -ls`                                     >> error_message
	$MAIL -s "$DEF_DIR not writable" $FAIL_MAIL < error_message
	exit ( 1 )
else
	pushd regression_test
endif

#	Assorted ftp scripts to pick up files from WRF anonyous
#	ftp site.

cat >! ftp_script_data << EOF
user ftp ${user}@
cd /pub/wrf/test
bin
get data.tar.gz
quit
EOF

cat >! ftp_script_1 << EOF
user ftp ${user}@
cd /pub/wrf/test
bin
get time_stamp
quit
EOF

cat >! ftp_script_2 << EOF
user ftp ${user}@
cd /pub/wrf/test
bin
get wrf.tar.gz
get data.tar.gz
quit
EOF

if      ( $acquire_from == "ftp" ) then

	#	Let's get the time_stamp file, this tells us if the WRF source code is
	#	the most recent possible.
	
	ftp -n ftp.ucar.edu < ftp_script_1
	
	#	Did we get a version of the code that is recent?  We can check
	#	with the "time_stamp" file which has a UTC Julian day inside.
	
	grep `date -u +"%j"` time_stamp >& /dev/null
	set ok = $status
	if ( $ok != 0 ) then
		echo "WRF repository file not sent to anonymous ftp for `date`." >! error_message
		echo "WRF date on time stamp is " `cat time_stamp`               >> error_message
		echo "I think that today is actually " `date -u +"%j"`           >> error_message
		$MAIL -s "old WRF on ftp site" $FAIL_MAIL < error_message
		exit ( 2 )
	endif
	
	#	If we are still in the game, then the date must have been OK.
	#	That means if the source code is recent, then we get it and the data.
	#	Only the source code is placed with this time stamp.  The data may be
	#	updated periodically, but we don't care about that.
	
	ftp -n ftp.ucar.edu < ftp_script_2

else if ( $acquire_from == "cvs" ) then

	#	Checkout the most recent version of WRF from the NCAR cvs repository,
	#	and pick up the required input data from the anonymous ftp site.

	cvs checkout -D $thedate WRFV1
	find ./WRFV1 -exec touch \{\} \;
	ftp -n ftp.ucar.edu < ftp_script_data

else if ( $acquire_from == "filearg" ) then

	#	A tar file of the WRF source was provided, so that is used, along with 
	#	the required input data files from the ftp site.

	tar xvf $thefile
	ftp -n ftp.ucar.edu < ftp_script_data

endif


GOT_TODAYS_WRF_SOURCE:

#	If we have the current source and data, we may be lucky and it is
#	already unpacked.  Do those tests.

#	It takes a while to unpack the data, so lets start that in the 
#	background, and at the same time begin building the executables.

if ( ! -d WRF-data ) then
	gzip -cd data.tar.gz | tar -xf - &
endif

#	We can't put the source unzip in the background, since we rely on
#	it existing to do a compile.

if ( $acquire_from == "ftp" && ! -d WRFV1 ) then
	gzip -cd  wrf.tar.gz | tar -xf -
endif
pushd WRFV1

#	There are six WRF executables to be considered that can run in distributed
#	memory.  The 2d hills and 2d squall lines cannot be parallelized with
#	MPI, and are therefore of no consequence to this shell.

if ( -e ${DEF_DIR}/regtest.output ) rm ${DEF_DIR}/regtest.output
set first_time_in = TRUE
foreach core ( $CORES )

	#	Here we are looping over all of the various compilation configurations,
	#	such as serial only, OpenMP only, MPI only, etc.  Each architecture
	#	has its own list of these options.  We build each of the executables for
	#	this particular ${core}.
	
	foreach compopt ( $COMPOPTS )

           if ( $compopt > 0 ) then
	
                goto BUILD_REGARDLESS
		#	Did we already build this one?

		if ( ( $core == em_real ) || ( $core == eh_real ) ) then
			if      ( ( $compopt == $COMPOPTS[1] ) && \
                                  ( -e main/wrf_${core}.exe.$compopt ) && \
                                  ( -e main/real_${core}.exe.1 ) && \
                                  ( -e ${DEF_DIR}/regression_test/WRFV1/external/io_netcdf/diffwrf ) ) then
				goto GOT_THIS_EXEC
			else if ( ( $compopt != $COMPOPTS[1] ) && \
                                  ( -e main/wrf_${core}.exe.$compopt ) && \
                                  ( -e ${DEF_DIR}/regression_test/WRFV1/external/io_netcdf/diffwrf ) ) then
				goto GOT_THIS_EXEC
			endif
		else
			if      ( ( $compopt == $COMPOPTS[1] ) && \
                                  ( -e main/wrf_${core}.exe.$compopt ) && \
                                  ( -e main/ideal_${core}.exe.1 ) && \
                                  ( -e ${DEF_DIR}/regression_test/WRFV1/external/io_netcdf/diffwrf ) ) then
				goto GOT_THIS_EXEC
			else if ( ( $compopt != $COMPOPTS[1] ) && \
                                  ( -e main/wrf_${core}.exe.$compopt ) && \
                                  ( -e ${DEF_DIR}/regression_test/WRFV1/external/io_netcdf/diffwrf ) ) then
				goto GOT_THIS_EXEC
			endif
		endif

BUILD_REGARDLESS:
	
		#	The WRF configuration file works with a single integer
		#	input, which is the compiler option.  By default, option "1" is serial, 
		#	option "2" is OpenMP, and option "3" is MPI.
	
		./clean
		echo $compopt | ./configure
	
                if ( `uname` == AIX ) then
 		  if ( ( $compopt == $COMPOPTS[1] ) || ( $compopt == $COMPOPTS[3] ) ) then
			sed -e '/^OMP/d' -e '/^FCOPTIM/d' -e '/^FCDEBUG/s/#//g' ./configure.wrf >! foo ; /bin/mv foo configure.wrf
		  else
			sed -e '/^OMP/s/noauto/noauto:noopt/' \
                                         -e '/^FCOPTIM/d' -e '/^FCDEBUG/s/#//g' ./configure.wrf >! foo ; /bin/mv foo configure.wrf
		  endif
                else
 		  if ( ( $compopt == $COMPOPTS[1] ) || ( $compopt == $COMPOPTS[3] ) ) then
			sed -e '/^OMP/d' -e '/^FCOPTIM/d' -e '/^FCDEBUG/s/#//g' ./configure.wrf >! foo ; /bin/mv foo configure.wrf
		  else
			sed              -e '/^FCOPTIM/d' -e '/^FCDEBUG/s/#//g' ./configure.wrf >! foo ; /bin/mv foo configure.wrf
		  endif
		endif
	
		#	Build this executable
		
		./compile $core

		#	This is the hilarious way to build the diffing program.  Repeat the
		#	build of WRF, and you MAGICALLY get it.  This diffing program is the same for em and eh,
		#	so we only have to do this once.

		if ( $first_time_in == TRUE ) then
			./compile $core
			set first_time_in = FALSE
		endif
	
		#	Did the compile work?

		set ok = $status

		if ( $ok != 0 ) then
			echo "SUMMARY compilation    for $core           parallel $compopt FAIL" >>! ${DEF_DIR}/regtest.output
			$MAIL -s "REGRESSION FAILURE $ARCH[1]" $FAIL_MAIL < ${DEF_DIR}/regtest.output
			exit ( 3 )
		else
			echo "SUMMARY compilation    for $core           parallel $compopt PASS" >>! ${DEF_DIR}/regtest.output
			echo "-------------------------------------------------------------" >> ${DEF_DIR}/regtest.output
			mv main/wrf.exe main/wrf_${core}.exe.$compopt
			if ( ( ( $core == em_real ) || ( $core == eh_real ) ) && ( $compopt == $COMPOPTS[1] ) ) then
				mv main/real.exe main/real_${core}.exe.1
			else if ( $compopt == 1 ) then
				mv main/ideal.exe main/ideal_${core}.exe.1
			endif
		endif

		GOT_THIS_EXEC:

             endif

	end
	
	#	We have all of the executables built, now we run'em.  This is a loop
	#	over all of the various physics options for this particular
	#	${core}.  Inside the physics loop, we loop over the parallel options.
	#	This allows us to use the same WRF input files for each of the parallel
	#	choices for a single physics loop index.

	foreach phys_option ( $PHYSOPTS )
	
		#	For each of the executables, we need to run several physics
		#	options.  

		if ( ( $core == em_real ) || ( $core == eh_real ) ) then

			foreach compopt ( $COMPOPTS )
                          if ( $compopt > 0 ) then

				pushd test/$core

				#	Create the correct namelist.input file.

				cp ${CUR_DIR}/phys_${phys_option}a phys_opta
				cp ${CUR_DIR}/phys_${phys_option}b phys_optb

				sed -e '/^ mp_physics/d' -e '/^ ra_lw_physics/d' -e '/^ ra_sw_physics/d' -e '/^ bl_sfclay_physics/d' \
                                    -e '/^ bl_surface_physics/d' -e '/^ bl_pbl_physics/d' -e '/^ cu_physics/d' -e '/^ num_soil_layers/d' \
                                    -e 's/ time_step_max  *= [1-9][0-9][0-9]*/ time_step_max = 10/g' \
                                    -e 's/ time_step_count_history  *= [1-9][0-9]*/ time_step_count_history = 10/g' \
                                    -e '/ chem_opt/r ./phys_optb' -e '/ icloud/r ./phys_opta' \
                                       ./namelist.input.$dataset >! namelist.input
                                /bin/cp namelist.input $TMPDIR/namelist.input.$core.${phys_option}.$compopt

				#	If this is the first time in (for this dynamical core, the first parallel loop
				#	and the first physics loop), we need to link in the first-guess data.  This input
				#	data is only a function of the dynamical core. 

				if ( ( $phys_option == $PHYSOPTS[1] ) && ( $compopt == $COMPOPTS[1] ) ) then
					rm real_input* >& /dev/null
					set ext = `echo $core | cut -c 1-2`
					ln -s ${DEF_DIR}/regression_test/WRF-data/${dataset}_${ext}/* .
				endif

				#	If this is the serial code, generate the IC and BC.  The real.exe program is not
				#	parallelized, so the data is generated and saved for the rest of the parallel tests.
				#	This data is necessarily updated for each of the physics tests.

				if ( $compopt == $COMPOPTS[1] ) then

					#	Zap any old input data laying around.

					rm wrfinput >& /dev/null
					rm wrfbdy   >& /dev/null

					../../main/real_${core}.exe.1 >! print.out.real_${core}_Phys=${phys_option}_Parallel=${compopt}

					#	Did making the IC BC files work?

					if ( ( -e wrfinput ) && ( -e wrfbdy ) ) then
						echo "SUMMARY generate IC/BC for $core physics $phys_option parallel $compopt PASS" >>! ${DEF_DIR}/regtest.output
						echo "-------------------------------------------------------------" >> ${DEF_DIR}/regtest.output
					else
						echo "SUMMARY generate IC/BC for $core physics $phys_option parallel $compopt FAIL" >>! ${DEF_DIR}/regtest.output
						$MAIL -s "WRF FAIL making IC/BC" $FAIL_MAIL < ${DEF_DIR}/regtest.output
						exit ( 4 )
					endif
				endif
		
				#	Run the forecast for this core, physics package and parallel option

				rm $TMPDIR/wrfout_01_000000.$core.${phys_option}.$compopt >& /dev/null

				if      ( $compopt == $COMPOPTS[1] ) then
					../../main/wrf_${core}.exe.$compopt >! print.out.wrf_${core}_Phys=${phys_option}_Parallel=${compopt}.exe
				else if ( $compopt == $COMPOPTS[2] ) then
					setenv OMP_NUM_THREADS $OPENMP
                                        setenv XLSMPOPTS "parthds=4"
					../../main/wrf_${core}.exe.$compopt >! print.out.wrf_${core}_Phys=${phys_option}_Parallel=${compopt}.exe
				else if ( $compopt == $COMPOPTS[3] ) then
					setenv OMP_NUM_THREADS 1
                                        setenv XLSMPOPTS "parthds=1"
					$MPIRUNCOMMAND ../../main/wrf_${core}.exe.$compopt
					mv rsl.error.0000 print.out.wrf_${core}_Phys=${phys_option}_Parallel=${compopt}.exe
				endif

				#	Did making the forecast work, by that, we mean "is there an output file created?"

				if ( -e wrfout_01_000000 ) then
					echo "SUMMARY generate FCST  for $core physics $phys_option parallel $compopt PASS" >>! ${DEF_DIR}/regtest.output
					echo "-------------------------------------------------------------" >> ${DEF_DIR}/regtest.output
				else
					echo "SUMMARY generate FCST  for $core physics $phys_option parallel $compopt FAIL" >>! ${DEF_DIR}/regtest.output
					$MAIL -s "WRF FAIL FCST" $FAIL_MAIL < ${DEF_DIR}/regtest.output
					exit ( 5 )
				endif

				#	We have to save this output file for our biggy comparison after all of the
				#	parallel options have been considered.

				mv wrfout_01_000000 $TMPDIR/wrfout_01_000000.$core.${phys_option}.$compopt

				popd

                          endif
			end

		else

			#	The ideal cases do not have different physics options, we only
			#	run them on an "as-is" basis.  If this is not the "first" physics
			#	loop, jump to the end of the physics loop.

			if ( $phys_option != $PHYSOPTS[1] ) then
				goto BOTTOM_OF_PHYSICS_LOOP
			endif

			foreach compopt ( $COMPOPTS )
                          if ( $compopt > 0 ) then

				pushd test/$core

				sed -e 's/ time_step_max  *= [1-9][0-9][0-9]*/ time_step_max = 10/g' \
                                    -e 's/ time_step_count_history  *= [1-9][0-9]*/ time_step_count_history = 10/g' \
                                       ./namelist.input >! foo1
				mv foo1 namelist.input

				#	If this is the serial code, generate the IC and BC.  The ideal.exe program is not
				#	parallelized, so the data is generated and saved for the rest of the parallel tests.

				if ( $compopt == $COMPOPTS[1] ) then

					#	Zap any old input data laying around.

					rm wrfinput >& /dev/null
					rm wrfbdy   >& /dev/null

					../../main/ideal_${core}.exe.1 >! print.out.ideal_${core}_Parallel=${compopt}

					#	Did making the IC BC files work?

					if ( ( -e wrfinput ) && ( -e wrfbdy ) ) then
						echo "SUMMARY generate IC/BC for $core           parallel $compopt PASS" >>! ${DEF_DIR}/regtest.output
						echo "-------------------------------------------------------------" >> ${DEF_DIR}/regtest.output
					else
						echo "SUMMARY generate IC/BC for $core           parallel $compopt FAIL" >>! ${DEF_DIR}/regtest.output
						$MAIL -s "WRF FAIL making IC/BC" $FAIL_MAIL < ${DEF_DIR}/regtest.output
						exit ( 4 )
					endif
				endif
		
				#	Run the forecast for this core and parallel option

				rm $TMPDIR/wrfout_01_000000.$core.${phys_option}.$compopt >& /dev/null

				if      ( $compopt == $COMPOPTS[1] ) then
					../../main/wrf_${core}.exe.$compopt >! print.out.wrf_${core}_Parallel=${compopt}.exe
				else if ( $compopt == $COMPOPTS[2] ) then
					setenv OMP_NUM_THREADS $OPENMP
					../../main/wrf_${core}.exe.$compopt >! print.out.wrf_${core}_Parallel=${compopt}.exe
				else if ( $compopt == $COMPOPTS[3] ) then
					setenv OMP_NUM_THREADS 1
					$MPIRUNCOMMAND ../../main/wrf_${core}.exe.$compopt
					mv rsl.error.0000 print.err.wrf_${core}_Parallel=${compopt}.exe
					mv rsl.out.0000 print.out.wrf_${core}_Parallel=${compopt}.exe
				endif

				#	Did making the forecast work, by that, we mean "is there an output file created?"

				if ( -e wrfout_01_000000 ) then
					echo "SUMMARY generate FCST  for $core           parallel $compopt PASS" >>! ${DEF_DIR}/regtest.output
					echo "-------------------------------------------------------------" >> ${DEF_DIR}/regtest.output
				else
					echo "SUMMARY generate FCST  for $core           parallel $compopt FAIL" >>! ${DEF_DIR}/regtest.output
					$MAIL -s "WRF FAIL FCST" $FAIL_MAIL < ${DEF_DIR}/regtest.output
					exit ( 5 )
				endif

				#	We have to save this output file for our biggy comparison after all of the
				#	parallel options have been considered.

				mv wrfout_01_000000 $TMPDIR/wrfout_01_000000.$core.${phys_option}.$compopt

				popd

                          endif
			end

		endif

		#	If there is only a single parallel option, then we are just trying to
		#	build the serial code.  That implies no comparisons are needed.

		if ( ${#COMPOPTS} != 1 ) then

			#	All of the forecasts for this set of physics and core have been
			#	generated.  We now compare the WRF model output files to see
			#	if they are S^2D^2.
	
			pushd ${DEF_DIR}/regression_test/WRFV1/test/$core
			set DIFFWRF = ${DEF_DIR}/regression_test/WRFV1/external/io_netcdf/diffwrf
	
			#	Serial vs OpenMP
	
			/bin/rm fort.88 fort.98 >& /dev/null
			if ( ( -e $TMPDIR/wrfout_01_000000.$core.${phys_option}.$COMPOPTS[1] ) && ( -e $TMPDIR/wrfout_01_000000.$core.${phys_option}.$COMPOPTS[2] ) ) then
				$DIFFWRF $TMPDIR/wrfout_01_000000.$core.${phys_option}.$COMPOPTS[1] $TMPDIR/wrfout_01_000000.$core.${phys_option}.$COMPOPTS[2] >& /dev/null
			else
				touch fort.88 fort.98
			endif
			if ( ! -e fort.88 ) then
				echo "SUMMARY serial vs OMP  for $core physics $phys_option              PASS" >>! ${DEF_DIR}/regtest.output
				echo "-------------------------------------------------------------" >> ${DEF_DIR}/regtest.output
			else
				echo "SUMMARY serial vs OMP  for $core physics $phys_option              FAIL" >>! ${DEF_DIR}/regtest.output
				echo "-------------------------------------------------------------" >> ${DEF_DIR}/regtest.output
			endif
	
			#	Serial vs MPI
	
			/bin/rm fort.88 fort.98 >& /dev/null
			if ( ( -e $TMPDIR/wrfout_01_000000.$core.${phys_option}.$COMPOPTS[1] ) && ( -e $TMPDIR/wrfout_01_000000.$core.${phys_option}.$COMPOPTS[3] ) ) then
				$DIFFWRF $TMPDIR/wrfout_01_000000.$core.${phys_option}.$COMPOPTS[1] $TMPDIR/wrfout_01_000000.$core.${phys_option}.$COMPOPTS[3] >& /dev/null
			else
				touch fort.88 fort.98
			endif
			if ( ! -e fort.88 ) then
				echo "SUMMARY serial vs MPI  for $core physics $phys_option            PASS" >>! ${DEF_DIR}/regtest.output
				echo "-------------------------------------------------------------" >> ${DEF_DIR}/regtest.output
			else
				echo "SUMMARY serial vs MPI  for $core physics $phys_option            FAIL" >>! ${DEF_DIR}/regtest.output
				echo "-------------------------------------------------------------" >> ${DEF_DIR}/regtest.output
			endif
	
			popd

		endif

		BOTTOM_OF_PHYSICS_LOOP:
		
	end
		
end

#	How long did this take.

set end = ( `date` )
echo "Start WRF Regression: $start " >> ${DEF_DIR}/regtest.output
echo "End   WRF Regression: $end   " >> ${DEF_DIR}/regtest.output

#	We have done all of the tests, and placed the PASS FAIL labels in the
#	output file.  If there are any FAIL messages, we are in trouble.

grep FAIL ${DEF_DIR}/regtest.output
set ok = $status
if ( $ok == 0 ) then
	$MAIL -s "REGRESSION FAILURE $ARCH[1]" $FAIL_MAIL < ${DEF_DIR}/regtest.output
else
	$MAIL -s "REGRESSION SUCCESS $ARCH[1]" $GOOD_MAIL < ${DEF_DIR}/regtest.output
endif
