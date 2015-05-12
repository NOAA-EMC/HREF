#
# Define the name of the executable
#
TARGET   = ../../exec/hiresw_post

#
# CPP, Compiler, and Linker Options
#
FC       = ${COMP_MP} -compiler ${COMP}
CPP      = /lib/cpp -P
CPPFLAGS = -DLINUX 
OPTS     = -O3 -fp-model fast -assume noold_ldout_format 
FREE     = -FR
TRAPS    = # -ftrapuv  -check all  -fp-stack-check  -fstack-protector
DEBUG    = -g  # -traceback

#
# Now the hard part. NCEPPOST needs 19 libraries, WOW!!!!!
#

SFCIO_INC  = -I${SFCIO_INC4}
SFCIO_LIB  = ${SFCIO_LIB4}

BACIO_LIB  = ${BACIO_LIB4}

SIGIO_INC  = -I${SIGIO_INC4}
SIGIO_LIB  = ${SIGIO_LIB4}

WRFPATH    = /nwprod/sorc/wrf_shared.v1.1.0
WRF_INC    = -I$(WRFPATH)/external/io_quilt -I$(WRFPATH)/frame
WRF_LIB    = $(WRFPATH)/main/libwrflib.a  \
             $(WRFPATH)/frame/pack_utils.o  \
             $(WRFPATH)/frame/module_internal_header_util.o \
             $(WRFPATH)/external/io_grib1/libio_grib1.a \
             $(WRFPATH)/external/io_grib_share/libio_grib_share.a \
             $(WRFPATH)/external/io_int/libwrfio_int.a \
             $(WRFPATH)/external/io_netcdf/libwrfio_nf.a \
             $(WRFPATH)/external/esmf_time_f90/libesmf_time.a \
             $(WRFPATH)/external/RSL_LITE/librsl_lite.a

G2_INC     = -I${G2_INC4} -I${G2TMPL_INC}
G2_LIB     = ${G2TMPL_LIB} ${G2_LIB4} ${JASPER_LIB} ${PNG_LIB} ${Z_LIB}

XML_INC    = -I${XMLPARSE_INC}
XML_LIB    = ${XMLPARSE_LIB}

GFSIO_INC  = -I${GFSIO_INC4}
GFSIO_LIB  = ${GFSIO_LIB4}

IP_LIB     = ${IP_LIB4}

SP_LIB     = ${SP_LIB4}

W3_INC     = -I${W3EMC_INC4}
W3_LIB     = ${W3NCO_LIB4} ${W3EMC_LIB4}

NETCDFPATH = /usrx/local/NetCDF/3.6.3/
NETCDF_LIB = $(NETCDFPATH)/lib/libnetcdf_c++.a $(NETCDFPATH)/lib/libnetcdf.a
NETCDF_INC = -I$(NETCDFPATH)/include

#
# Assemble Options
#
FFLAGS   = $(OPTS) $(FREE) $(TRAPS) $(DEBUG) \
           $(WRF_INC) \
           $(XML_INC) \
           $(G2_INC) \
           -I$(NEMSIO_INC) \
           $(GFSIO_INC) \
           $(SIGIO_INC) \
           $(SFCIO_INC) \
           $(W3_INC) \
           -I$(CRTM_INC) \
           $(NETCDF_INC)

LIBS     = $(WRF_LIB) \
           $(XML_LIB) \
           $(G2_LIB) \
           $(NEMSIO_LIB) \
           $(GFSIO_LIB) \
           $(SIGIO_LIB) \
           $(SFCIO_LIB) \
           $(IP_LIB) \
           $(SP_LIB) \
           $(W3_LIB) \
           $(BACIO_LIB) \
           $(CRTM_LIB) \
           $(NETCDF_LIB)


OBJS =  wrf_io_flags.o getVariable.o getIVariable.o getIVariableN.o \
        getVariableB.o \
        kinds_mod.o machine.o physcons.o \
        count_recs_wrf_binary_file.o inventory_wrf_binary_file.o \
        next_buf.o retrieve_index.o ZENSUN.o CLDFRAC_ZHAO.o \
        GFSPOST.o GETGBANDSCATTER.o post_t.o \
        VRBLS2D_mod.o VRBLS3D_mod.o VRBLS4D_mod.o MASKS_mod.o PMICRPH.o SOIL_mod.o \
        CMASSI.o CTLBLK.o GRIDSPEC.o LOOKUP.o PARAMR.o RHGRD.o RQSTFLD.o \
        cuparm.o params.o svptbl.o get_postfilename.o grib2_module.o \
        SET_LVLSXML.o FILL_PSETFLD.o \
        BNDLYR.o BOUND.o CALCAPE.o CALDWP.o CALDRG.o CALHEL.o CALLCL.o \
        CALMCVG.o CALPOT.o CALPW.o CALRH.o CALRCH.o CALRH_GSD.o \
        CALSTRM.o CALTAU.o CALTHTE.o CALVIS.o CALVIS_GSD.o CALVOR.o CALWXT.o \
        CALWXT_RAMER.o CALWXT_BOURG.o CALWXT_REVISED.o CALRH_PW.o \
        CALWXT_EXPLICIT.o CALWXT_DOMINANT.o \
        CLDRAD.o CLMAX.o COLLECT.o COLLECT_LOC.o DEWPOINT.o \
        FDLVL.o FGAMMA.o FIXED.o FRZLVL.o FRZLVL2.o \
        GET_BITS.o GRIBIT.o INITPOST.o LFMFLD.o \
        MAPSSLP.o MISCLN.o MIXLEN.o MDL2P.o MDLFLD.o MPI_FIRST.o MPI_LAST.o \
        NGMFLD.o NGMSLP.o OTLFT.o OTLIFT.o SLP_new.o SLP_NMM.o EXCH.o \
        PARA_RANGE.o PROCESS.o INITPOST_NMM.o EXCH2.o \
        READCNTRL.o READ_xml.o SET_OUTFLDS.o SCLFLD.o SERVER.o SETUP_SERVERS.o \
        SMOOTH.o SURFCE.o \
        SPLINE.o TABLE.o TABLEQ.o TRPAUS.o TTBLEX.o WETBULB.o WRFPOST.o \
        CALMICT.o MICROINIT.o GPVS.o MDL2SIGMA.o \
        ETCALC.o CANRES.o CALGUST.o WETFRZLVL.o SNFRAC.o MDL2AGL.o SNFRAC_GFS.o \
        AVIATION.o DEALLOCATE.o INITPOST_NMM_BIN_MPIIO.o INITPOST_BIN.o \
        CALPBL.o MDL2SIGMA2.o INITPOST_GFS.o CALRH_GFS.o LFMFLD_GFS.o \
        CALRAD_WCLOUD_newcrtm.o MDL2THANDPV.o CALPBLREGIME.o POLEAVG.o \
        INITPOST_NEMS.o GETNEMSNDSCATTER.o ICAOHEIGHT.o INITPOST_GFS_NEMS.o \
        INITPOST_BIN_MPIIO.o GEO_ZENITH_ANGLE.o GFIP3.o GRIDAVG.o CALUPDHEL.o INITPOST_GFS_SIGIO.o \
        AllGETHERV_GSD.o MSFPS.o SELECT_CHANNELS.o ALLOCATE_ALL.o INITPOST_NEMS_MPIIO.o ASSIGNNEMSIOVAR.o

.SUFFIXES: .F .f .o .f90

.F.f:
	$(CPP) $(CPPFLAGS) $< > $*.f

$(TARGET): $(OBJS)
	$(FC) -openmp -o $@ $(OBJST) $(OBJXML) $(OBJS) $(LIBS)

.f.o:
	$(FC) $(FFLAGS) -c $<

.f90.o:
	$(FC) $(FFLAGS) -c $<


clean:
	/bin/rm -f  *.lst *.o *.mod
