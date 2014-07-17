#!/usr/bin/perl -w
#   Purpose:   Convert POST grib1 control file to grib2 xml control file
#   Usage:     perl cnvt1_model_pn1_local.pl gfs_cntrl.parm >model.xml
#   03-29-2013  Guang Ping Lou: Initial code
#   08-27-2013  Guang Ping Lou: Change to local variable if both local and WMO exist.
use Text::Balanced qw/extract_multiple extract_bracketed extract_quotelike/;
use XML::LibXML;
my $doc = XML::LibXML::Document->new();
my $varname="";
my $scale=1.0;
my $one_rec1="";
my $DATSET="";
my $i=0;
print $doc->toString;
my @level=(100., 200., 300., 500., 700., 1000., 2000., 3000., 5000., 7000., 
         10000., 12500., 15000., 17500., 20000., 22500., 25000., 27500., 30000., 32500., 
         35000., 37500., 40000., 42500., 45000., 47500., 50000., 52500., 55000., 57500.,
         60000., 62500., 65000., 67500., 70000., 72500., 75000., 77500., 80000., 82500., 
         85000., 87500., 90000., 92500., 95000., 97500., 100000.);  #Pa

my @HTFD=(30.0,50.0,80.0,100.0,305.0,457.0,610.0,914.0,1524.0,1829.0,2134.0,2743.0,3658.0,4572.0,6000.0);     #flight height meters
         
#/nwprod/lib/sorc/gfsio/gfsio_module.f sigma values
##my @vcoord=(  0.0000000, 0.64200000E-03, 0.13780000E-02, 0.22199999E-02,
##  0.31830000E-02, 0.42840000E-02, 0.55439998E-02, 0.69849999E-02,
##  0.86310003E-02, 0.10511000E-01, 0.12658000E-01, 0.15107000E-01,
##  0.17901000E-01, 0.21083999E-01, 0.24707999E-01, 0.28829999E-01,
##  0.33514999E-01, 0.38830999E-01, 0.44854999E-01, 0.51670998E-01,
##  0.59370000E-01, 0.68048999E-01, 0.77808000E-01, 0.88756002E-01,
##0.10100200, 0.11465500, 0.12982300, 0.14660800, 0.16509899, 0.18537199,
##0.20748200, 0.23145400, 0.25728399, 0.28492799, 0.31430000, 0.34526899,
##0.37765899, 0.41124901, 0.44577801, 0.48095500, 0.51646298, 0.55197400,
##0.58715999, 0.62170500, 0.65531600, 0.68773103, 0.71872902, 0.74813300,
##0.77581102, 0.80167699, 0.82568502, 0.84783000, 0.86813903, 0.88666302,
##0.90347999, 0.91867799, 0.93235999, 0.94463098, 0.95560300, 0.96538502,
## 0.97408301, 0.98180002, 0.98863202, 0.99467099, 1.0000000);
# my @vcoord=(1.0000000, 0.99467099, 0.98863202, 0.98180002, 0.97408301,
#             0.96538502, 0.95560300, 0.94463098, 0.93235999, 0.91867799,
#             0.90347999, 0.88666302, 0.86813903, 0.84783000, 0.82568502,
# #            0.80167699, 0.77581102, 0.74813300, 0.71872902, 0.68773103,
#  #           0.65531600, 0.62170500, 0.58715999, 0.55197400, 0.51646298,
#            0.48095500, 0.44577801, 0.41124901, 0.37765899, 0.34526899,
#            0.31430000, 0.28492799, 0.25728399, 0.23145400, 0.20748200,
#            0.18537199, 0.16509899, 0.14660800, 0.12982300, 0.11465500,
#            0.10100200, 0.88756002E-01, 0.77808000E-01, 0.68048999E-01, 0.68048999E-01,
#            0.59370000E-01, 0.51670998E-01, 0.44854999E-01, 0.38830999E-01, 0.38830999E-01,
#            0.33514999E-01, 0.28829999E-01, 0.24707999E-01, 0.21083999E-01, 0.17901000E-01,
#            0.15107000E-01, 0.12658000E-01, 0.10511000E-01, 0.86310003E-02, 0.69849999E-02,
#            0.55439998E-02, 0.42840000E-02, 0.31830000E-02, 0.22199999E-02, 0.13780000E-02,
#            0.64200000E-03, 0.0000000 );
my @vcoord=(1.0000, .9930, .9849, .9756, .9649, .9525, .9382, .9215, .9019,
            .8790, .8521, .8209, .7853, .7454, .7016, .6546, .6052, .5545,
            .5039, .4548, .4088, .3669, .3295, .2965, .2675, .2413, .2169,
            .1937, .1712, .1496, .1290, .1094, .0912, .0743, .0588, .0448,
            .0325, .0218, .0128, .0055, 0.000) ;

#INITPOST_GFS_NEMS.f
my @SOILZ=(0, 10, 40, 100, 200);  #cm
my @SOILZ2=(10, 40, 100, 200);  #cm
my @SOILZ_SCAL=(2, 2, 2, 2);  #scale_fact_fixed_sfc1

#WRFPOST.f
my @PVL=(500, -500, 1000, -1000, 1500, -1500, 2000, -2000);
#my @PVL=(0.5,-0.5,1.0,-1.0,1.5,-1.5,2.0,-2.0)
my @PVL_SCAL=(9, 9, 9, 9, 9, 9, 9, 9);  #scale_fact_fixed_sfc1

my @SPEC_PRES=(3000., 6000., 9000., 12000., 15000., 18000.);
my @SPEC_PRES2=(0., 3000., 6000., 9000., 12000., 15000.);

my @THETA_L=(320);

my $allzeros="(00000 00000 00000 00000 00000 00000 00000 00000 00000 00000 00000 00000 00000 00000)";

my @HYBL=(  1,  2,  3,  4,  5,  6,  7,  8,  9, 10,
           11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
           21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
           31, 32, 33, 34, 35, 36, 37, 38, 39, 40) ;
          
my @buffer;
$#buffer = 1; # 3 line buffer (or whatever)
my $shortn;
my $pname;
my $SCALE1="";
my $var_fd="_ON_SPEC_HGT_LVL_ABOVE_GRND_FDHGT";
#my @pressu;
my $file=$ARGV[0];
#print "my fiel= $file\n";
$model = substr($file, 0, index($file, '_'));
#print "my model= $model\n";
$model=uc($model);
#print "my model= $model\n";

        print "    <postxml>\n";
   open ( INFILE, "< $file" );           ## Open the input file.
   ## Place pruned data into the database, record-by-record.
   while ( defined( my $one_rec = <INFILE> ) ) {
         if($one_rec =~ m/KGTYPE | IMDLTY | DATSET/) {
          if ($one_rec =~ m/DATSET/) {
            @DATS = extract_multiple(
                             $one_rec,
                               [ sub{extract_bracketed($_[0], '()')},],
                              undef,
                             1
                            );
                  $DATSET=$DATS[0];
                  $DATSET =~ s/\(|\)//g;

        print "    </paramset>\n\n";
        print "    <paramset>\n";
        print "    <datset> $DATSET </datset>\n";
            if( $model eq "NAM" || $model eq "DGEX" ) {
        print "    <grid_num>32769</grid_num>\n";;
            } else {
        print "    <grid_num>4</grid_num>\n";;
            }
        print "    <sub_center>ncep_nco</sub_center>\n";
        print "    <version_no>v2003</version_no>\n";
        print "    <local_table_vers_no>local_tab_yes1</local_table_vers_no>\n";
        print "    <sigreftime>fcst</sigreftime>\n";
        print "    <prod_status>oper</prod_status>\n";
        print "    <data_type>fcst</data_type>\n";
        print "    <gen_proc_type>fcst</gen_proc_type>\n";
        print "    <time_range_unit>hour</time_range_unit>\n";
        print "    <orig_center>nws_ncep</orig_center>\n";
            if( $model eq "NAM" || $model eq "DGEX" ) {
        print "    <gen_proc>meso_nam12km</gen_proc>\n";
            } else {
        print "    <gen_proc>$model</gen_proc>\n";
            }
        print "    <packing_method>jpeg2000</packing_method>\n";
        print "    <field_datatype>fltng_pnt</field_datatype>\n";
        print "    <comprs_type>lossless</comprs_type>\n\n";

         }
        } elsif ($one_rec =~ m/SCAL=/) {
# get the scale factor and grib1 variable name
        my @tempv = extract_multiple(
                               $one_rec,
                               [ sub{extract_bracketed($_[0], '()')},],
                               undef,
                               1
                            );               #get values inside ()
#                $SCALE=$tempv[-1];
#                $SCALE=~ s/\(|\)//g;
                $varname=$tempv[0];
                $varname=~ s/\(|\)//g;       # remove ()
                $varname =~ s/^\s+([\s\S]+)\s+$/$1/; #remove tail spaces
#                 print "varname= $varname\n";
#  match grib1 with grib2 variable short name and get variable number
      open ( TEXT_FILE, "< RQSTFLD.f" );
   while (  my $two_rec = <TEXT_FILE>  ) {
    push @buffer, $two_rec;
      shift @buffer;
                if ($two_rec =~ m/${varname}/i) {
                  my $varnum = extract_multiple(
                             $buffer[0],
                             [ sub{extract_bracketed($_[0], '()')},],
                             undef,
                             1
                            );
#             print "varnum= $varnum\n";
                  $varnum=~ s/\(|\)//g;
                  $varnum=~ s/^0*//;
# match available grib2 field
    open ( TEXT_FILE2, "< post_avblflds.xml" );
      while (my $line2 = <TEXT_FILE2>) {
                if ($line2 =~ m/post_avblfldidx>${varnum}</) {
                $shortn= <TEXT_FILE2>;
      while (my $line6 = <TEXT_FILE2>) {
                if ($line6 =~ m/<pname>/) {
                  $pname= $line6;
                 } elsif ($line6 =~ m/<scale>/) {
                  $SCALE1= $line6;
#           print " SCALE1= $SCALE1";
                  last;
               }
           }
                  last;
               }
           }

        }
}

        } elsif ($one_rec =~ m/L=\(/) {
            my @tempv = extract_multiple(
                               $one_rec,
                               [ sub{extract_bracketed($_[0], '()')},],
                               undef,
                               1
                            );
                $LEV=$tempv[0];
               if ( $LEV ne $allzeros) {
                $LEV=~ s/\(|\)//g;
                $LEV=~ s/ //g;
        print "       <param>\n";
        print "$shortn";
        print "$pname";
##########
#check if the variable is NCEP defined
                $var=$pname;
                 $var=~ s/<\/pname>//;
                 $var=~ s/<pname>//;
                  $var=~ s/ //g;
#                  $var=~ s/_/:/g;
#  split the string and get the first and second words (variable)
#        print "$var";
#        print "$shortn $var";
#         my $key1="";
#my ($key, $key2, $value) = split /\s*:\s*/, $var, 3;
#         $key1=$key;
#       if ($key eq "$model" or $key eq "AVE" or $key eq "ACM" or $key eq "BEST" 
#                      or $key eq "MAX" or $key eq "MIXED" or $key eq "UNSTABLE"
#                      or $key eq "MIN" or $key eq "GSD" or  $key eq "INST" ) {
#                    $key1=$key2;
#        print "keyout= $key, $key2\n";
#                 }
    open ( TEXT_FILE3, "< params_grib2_tbl_new" );
      while (my $line3 = <TEXT_FILE3>) {
                 if ($line3 =~ m/ 1 $var$/) {
                  print "       <table_info>NCEP</table_info>\n";
                 last;
#                 } elsif ($line3 =~ m/ [0] $var$/) {
                 }
               }
############
        if ($shortn =~ m/ON_ISOBARIC_SFC/i) {
        print "       <level>";
         $i=0;
          while ($i <= 47) {
           my $nth = substr($LEV, $i, 1);
           my $ii = $nth + 0 ;
            if( $ii != 0) {
              if($level[$i]) {
               if($i == 20 || $i == 40 ) {
               printf ("%9.1f", $level[$i]);
               print "\n";
                } else {
               printf ("%9.1f", $level[$i]);
               }
             }
          }
         $i++;
        }
        print "</level>\n";
############
        } elsif ($shortn =~ m/ON_HYBRID_LVL/i) {
         $i=0;
         my $ii=0;
          while ($i <= 60) {
           my $nth = substr($LEV, $i, 1);
              $ii = $ii + $nth;
         $i++;
               }
            if( $ii > 1) {
         
        print "       <level>";
         $i=0;
          while ($i <= 60) {
           my $nth = substr($LEV, $i, 1);
           my $ii = $nth + 0 ;
            if( $ii != 0) {
              if($HYBL[$i]) {
               printf ("%3.0f", $HYBL[$i]);
             }
          }
         $i++;
        }
        print "</level>\n";
        }
############
        } elsif ($shortn =~ m/ON_SIGMA_LVL/i) {
         $i=0;
         my $jj=0;
          while ($i <= 69) {
           my $nth = substr($LEV, $i, 1);
              $jj = $jj + $nth;
         $i++;
               }
            if( $jj > 1) {
         
        print "       <level>";
         $i=0;
          while ($i <= 69) {
           my $nth = substr($LEV, $i, 1);
           my $ii = $nth + 0;
            if( $ii != 0) {
              if($vcoord[$i]) {

               if($i == 23 || $i == 46 ) {
               printf ("%7.4f", $vcoord[$i]);
               print "\n";
                } else {
               printf ("%7.4f", $vcoord[$i]);
               }

             }
          }
         $i++;
        }
        print "</level>\n";
        }
############
        }elsif ($shortn =~ m/POT_VORT_SFC/i) {
#my @PVL=(500, -500, 1000, -1000, 1500, -1500, 2000, -2000);
#my @PVL_SCAL=(9, 9, 9, 9, 9, 9, 9, 9);  #scale_fact_fixed_sfc1
        print "       <scale_fact_fixed_sfc1>";
         $i=0;
          while ($i <= 47) {
           my $nth = substr($LEV, $i, 1);
           my $ii = $nth + 0 ;
            if( $ii != 0) {
               printf ("%3.0f",$PVL_SCAL[$i]);
               }
         $i++;
             }
        print " </scale_fact_fixed_sfc1>\n";
            
        print "       <level>";
          $i=0;
          while ($i <= 47) {
           my $nth = substr($LEV, $i, 1);
           my $ii = $nth + 0 ;
            if( $ii != 0) {
              if($PVL[$i]) {
               printf ("%9.1f", $PVL[$i]);
             }
          }
         $i++;
        }
        print "</level>\n";
        } elsif ($shortn =~ m/ISENTROPIC_LVL/i) {
#my @THETA_L=(320);
        print "       <level>";
         $i=0;
          while ($i <= 47) {
           my $nth = substr($LEV, $i, 1);
           my $ii = $nth + 0 ;
            if( $ii == 1) {
              if($HTFD[$i]) {
               printf ("%5.0f", $THETA_L[$i]);
             }
               }
         $i++;
             }
        print "</level>\n";
        } elsif ($shortn =~ m/SPEC_ALT_ABOVE_MEAN_SEA_LVL/i) {
         
        print "       <level>";
#flight height meters
##my @HTFD=(30.0,50.0,80.0,100.0,305.0,457.0,610.0,914.0,1524.0,1829.0,2134.0,2743.0,3658.0,4572.0,6000.0);
         $i=0;
          while ($i <= 47) {
           my $nth = substr($LEV, $i, 1);
           my $ii = $nth + 0 ;
            if( $ii == 1) {
              if($HTFD[$i]) {
               printf ("%5.0f", $HTFD[$i]);
             }
               }
         $i++;
             }
        print "</level>\n";
        } elsif ($shortn =~ m/ON_SPEC_PRES_ABOVE_GRND/i) {
#my @SPEC_PRES=(3000, 6000, 9000, 12000, 15000, 18000);
#my @SPEC_PRES2=(0, 3000, 6000, 9000, 12000, 15000);
        print "       <level>";
         $i=0;
          while ($i <= 47) {
           my $nth = substr($LEV, $i, 1);
           my $ii = $nth + 0 ;
            if( $ii != 0) {
              if($SPEC_PRES[$i]) {
               printf ("%8.0f",$SPEC_PRES[$i]);
             }
               }
         $i++;
             }
        print "</level>\n";
        
#      get the second level
        print "       <level2>";
         $i=0;
          while ($i <= 47) {
           my $nth = substr($LEV, $i, 1);
           my $ii = $nth + 0 ;
            if( $ii != 0) {
               printf ("%8.0f",$SPEC_PRES2[$i]);
               }
         $i++;
             }
        print "</level2>\n";
        } elsif ($shortn =~ m/DEPTH_BEL_LAND_SFC/) {
#my @SOILZ=(0, 10, 40, 100, 200);  #cm
#my @SOILZ2=(10, 40, 100, 200);  #cm
#my @SOILZ_SCAL=(2, 2, 2, 2);  #scale_fact_fixed_sfc1
        if ($shortn =~ m/DEPTH_BEL_LAND_SFC_3m/) {
           #do nothing
        } elsif ($varname=~ m/TOTAL/)  {
           #do nothing
#             last;
        } else {
        print "       <scale_fact_fixed_sfc1>";
         $i=0;
          while ($i <= 47) {
           my $nth = substr($LEV, $i, 1);
           my $ii = $nth + 0 ;
            if( $ii != 0) {
               printf ("%3.0f",$SOILZ_SCAL[$i]);
               }
         $i++;
             }
        print " </scale_fact_fixed_sfc1>\n";
            
        print "       <level>";
         $i=0;
          while ($i <= 47) {
           my $nth = substr($LEV, $i, 1);
           my $ii = $nth + 0 ;
            if( $ii == 1) {
               printf ("%5.0f",$SOILZ[$i]);
               }
         $i++;
             }
        print "</level>\n";
        
        print "       <scale_fact_fixed_sfc2>";
          $i=0;
          while ($i <= 47) {
           my $nth = substr($LEV, $i, 1);
           my $ii = $nth + 0 ;
            if( $ii != 0) {
               printf ("%3.0f",$SOILZ_SCAL[$i]);
               }
         $i++;
             }
        print " </scale_fact_fixed_sfc2>\n";
            
#      get the second level
        print "       <level2>";
         $i=0;
          while ($i <= 47) {
           my $nth = substr($LEV, $i, 1);
           my $ii = $nth + 0 ;
            if( $ii != 0) {
               printf ("%5.0f",$SOILZ2[$i]);
               }
         $i++;
             }
        print "</level2>\n";
        }   #3m end here
        }
#        print "       <scale> $SCALE </scale>\n";
           print "  $SCALE1";
        print "      </param>\n\n";
               }
#************************************
        if ($shortn =~  m/SPEC_ALT_ABOVE_MEAN_SEA_LVL/i) {
           if ($LEV =~  m/2/) {
        print "       <param>\n";
                $var=$shortn;
                 $var=~ s/SPEC_ALT_ABOVE_MEAN_SEA_LVL/SPEC_HGT_LVL_ABOVE_GRND_FDHGT/;
#     my ($key1, $value1) = split /\s*:\s*/, $var, 2;
#        print "       <shortname>$var$var_fd</shortname>\n";
        print "$var";
        print "$pname";
        print "       <level>";
#flight height meters
##my @HTFD=(30.0,50.0,80.0,100.0,305.0,457.0,610.0,914.0,1524.0,1829.0,2134.0,2743.0,3658.0,4572.0,6000.0);
         $i=0;
          while ($i <= 47) {
           my $nth = substr($LEV, $i, 1);
           my $ii = $nth + 0 ;
            if( $ii == 2) {
              if($HTFD[$i]) {
               printf ("%5.0f", $HTFD[$i]);
             }
               }
         $i++;
             }
        print "</level>\n";
#        print "       <scale> $SCALE </scale>\n";
           print "$SCALE1";
        print "       </param>\n\n";
               }
               }
#************************************
            }
}
print "   </paramset>\n";
print "   </postxml>\n";
