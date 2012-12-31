################################################################################
# 
#     Makefile for NCEP Post
#
#     Use:
#     make         -  build the executable
#     make clean   -  start with a clean slate
#
#     The following macros will be of interest:
#
#         TARGET   - name of the executable
#         FC       - name of Fortran compiler
#         CPP      - name of CPP
#         ARCH     - architecture
#         CPPFLAGS - CPP flags
#         OPTS     - compiler code optimizations
#         LIST     - source listing
#         SMP      - threading
#         TRAPS    - runtime traps for floating point exceptions
#         PROFILE  - source code profiling ( -pg )
#         DEBUG    - -g
#         MEM      - user data area and stack size
#         MAP      - load map
#         W3LIB    - w3lib
#         BACIO    - bacio lib
#         ESSL     - ESSL library
#         MASS     - MASS library
#         HPMLIB   - hpm lib
#         SEARCH   - library search location
# 
#         This version for eta_post with more intelligent memory allocation
#         Jim Tuccillo   Feb 2001
# 
#         This version for eta_post with asynchronous I/O server.   
#         Jim Tuccillo   June 2001
#
#################################################################################
#
# Define the name of the executable
#
TARGET = ../../exec/hiresw_post
#
# CPP, Compiler, and Linker Options
#


NETCDFPATH=/usrx/local/NetCDF/3.6.3
FC       = mpfort -convert big_endian -g -traceback
CPP      = /usr/bin/cpp -P
ARCH     = 
CPPFLAGS = -DLINUX
OPTS     = -lmpi -O2 # -check all -fpe0 -ftrapuv
LIST     = 
FREE     = -FR
TRAPS    = 
PROFILE  = 
DEBUG    = 
MEM      =
MAP      =  
W3LIBDIR = /nwprod/lib
ESSL     = 
MASS     =
NCDLIBS = -L$(NETCDFPATH)/lib -lnetcdf
NCDFFLAGS = -I$(NETCDFPATH)

CRTMFFLAGS = -I/nwprod/lib/incmod/crtm

W3FLAGS = 
SFCFLAG = -I/nwprod/lib/incmod/sfcio_4

NEMSIOFLAG = -I/nwprod/lib/incmod/nemsio


WRFLIB    = ./wrflibs/libwrflib.a  ./wrflibs/libwrfio_int.a ./wrflibs/libwrfio_nf.a \
            ./wrflibs/pack_utils.o  ./wrflibs/libesmf_time.a ./wrflibs/librsl_lite.a  ./wrflibs/libio_grib1.a ./wrflibs/libio_grib_share.a

CRTMLIB = /nwprod/lib/libcrtm.a

SEARCH   =
#
# Assemble Options
#

FFLAGS   = $(OPTS) $(LIST) $(TRAPS) $(PROFILE) $(DEBUG) $(NCDFFLAGS) $(WRFFLAGS) $(CRTMFFLAGS) $(W3FLAGS) $(SFCFLAG) $(NEMSIOFLAG) $(FREE)
FFLAGSM  = $(OPTS) $(LIST) $(TRAPS) $(PROFILE) $(DEBUG)
FFLAGST  = $(OPTS) $(LIST) $(FREE) $(TRAPS) $(PROFILE) $(DEBUG) $(NCDFFLAGS) $(WRFFLAGS) $(CRTMFFLAGS) $(W3FLAGS) $(NEMSIOFLAG)
LDFLAGS  = -mkl=sequential $(MEM) $(MAP) $(SMP) $(PROFILE)
LIBS     = $(ESSL) $(MASS) $(SEARCH) $(NCDLIBS) $(WRFLIB)\
           -L/nwprod/lib/nemsio -lnemsio \
           -L/nwprod/lib \
           -lsfcio_4 -lbacio_4 -lsp_4 -lw3emc_4 -lw3nco_4 $(CRTMLIB) $(NCDLIBS)
#
# Threaded object files
#
OBJSF = gfsio_module.o
OBJSM = mersenne_twister.o
#
OBJST=	wrf_io_flags.o module_internal_header_util.o getVariable.o getIVariable.o getVariableB.o getIVariableN.o getVariableRSM.o \
	kinds_mod.o nemsio_module.o machine.o physcons.o \
	count_recs_wrf_binary_file.o inventory_wrf_binary_file.o \
	next_buf.o retrieve_index.o ZENSUN.o CLDFRAC_ZHAO.o \
	GFSPOST.o GETGBANDSCATTER.o 
#
# Non-threaded object files
#
OBJS=	VRBLS2D_mod.o VRBLS3D_mod.o VRBLS4D_mod.o MASKS_mod.o PMICRPH.o SOIL_mod.o \
        CMASSI.o CTLBLK.o GRIDSPEC.o LOOKUP.o PARAMR.o RHGRD.o RQSTFLD.o \
        cuparm.o params.o svptbl.o \
	BNDLYR.o  BOUND.o  CALCAPE.o  CALDWP.o  CALDRG.o CALHEL.o  CALLCL.o  \
	CALMCVG.o CALPOT.o  CALPW.o CALRH.o  CALRCH.o CALRH_GSD.o \
	CALSTRM.o CALTAU.o CALTHTE.o CALUPDHEL.o CALVIS.o CALVIS_GSD.o CALVOR.o CALWXT.o \
        CALWXT_RAMER.o CALWXT_BOURG.o CALWXT_REVISED.o \
        CALWXT_EXPLICIT.o CALWXT_DOMINANT.o \
	CLDRAD.o  CLMAX.o COLLECT.o  COLLECT_LOC.o \
	DEWPOINT.o \
	FDLVL.o FGAMMA.o FIXED.o  FRZLVL.o  FRZLVL2.o \
	GET_BITS.o  GRIBIT.o INITPOST.o LFMFLD.o  INITPOST_BIN.o \
	MAPSSLP.o MISCLN.o MIXLEN.o MDL2P.o MDLFLD.o MPI_FIRST.o  MPI_LAST.o \
	NGMFLD.o NGMSLP.o  OTLFT.o OTLIFT.o SLP_new.o SLP_NMM.o EXCH.o \
	PARA_RANGE.o              PROCESS.o INITPOST_NMM.o EXCH2.o \
	READCNTRL.o  SCLFLD.o  SERVER.o  SETUP_SERVERS.o SMOOTH.o SURFCE.o \
	SPLINE.o  TABLE.o  TABLEQ.o  TRPAUS.o  TTBLEX.o WETBULB.o WRFPOST.o \
        INITPOST_NMM_BIN.o CALMICT_LOC.o MICROINIT.o GPVS.o MDL2SIGMA.o \
        ETCALC.o CANRES.o CALGUST.o WETFRZLVL.o SNFRAC.o MDL2AGL.o SNFRAC_GFS.o \
	INITPOST_RSM.o AVIATION.o DEALLOCATE.o INITPOST_NMM_BIN_MPIIO_IJK.o \
        CALPBL.o MDL2SIGMA2.o INITPOST_GFS.o CALRH_GFS.o LFMFLD_GFS.o  \
	MDL2THANDPV.o CALPBLREGIME.o POLEAVG.o \
	INITPOST_NEMS.o GETNEMSNDSCATTER.o ICAOHEIGHT.o INITPOST_GFS_NEMS.o \
        INITPOST_BIN_MPIIO.o GEO_ZENITH_ANGLE.o 

SRCS= 	AVIATION.f         FDLVL.f                MICROINIT.f \
BNDLYR.f         FGAMMA.f                 MISCLN.f \
BOUND.f          FIXED.f                  MIXLEN.f \
CALCAPE.f          FRZLVL2.f              module_internal_header_util.f \
CALDRG.f         FRZLVL.f                 MPI_FIRST.f \
CALDWP.f         GEO_ZENITH_ANGLE.f       MPI_LAST.f \
CALGUST.f          GET_BITS.f        nemsio_module.f \
CALHEL.f         GETGBANDSCATTER.f          next_buf.f \
CALLCL.f         getIVariable.f      NGMFLD.f \
CALMCVG.f          getIVariableN.f          NGMSLP.f \
CALMICT_LOC.f          GETNEMSNDSCATTER.f     OTLFT.f \
CALPBL.f         getVariableB.f      OTLIFT.f \
CALPBLREGIME.f        getVariable.f         PARAMR.f \
CALPOT.f         getVariableRSM.f           params.f \
CALPW.f          gfsio_module.f      PARA_RANGE.f \
CALRAD.f         GFSPOST.f                physcons.f \
CALRAD_WCLOUD.f        GPVS.f             PMICRPH.f \
CALRAD_WCLOUD_newcrtm.f       GRIBIT_DECIMAL.f          POLEAVG.f \
CALRCH.f         GRIBIT.f                 PROCESS.f \
CALRH.f          GRIDSPEC.f          READCNTRL.f \
CALRH_GFS.f         ICAOHEIGHT.f          retrieve_index.f \
CALRH_GSD.f         INITPOST_BIN.f          RHGRD.f \
CALSTRM.f          INITPOST_BIN_MPIIO.f     RQSTFLD.f \
CALTAU.f         INITPOST.f          SCLFLD.f \
CALTHTE.f          INITPOST_GFS.f           SERVER.f \
CALUPDHEL.f         INITPOST_GFS_NEMS.f     SETUP_SERVERS.f \
CALVIS.f         INITPOST_GRIB.f          SLP_new.f \
CALVIS_GSD.f        INITPOST_NEMS.f         SLP_NMM.f \
CALVOR.f         INITPOST_NMM_BIN.f       SMOOTH.f \
CALWXT_BOURG.f        INITPOST_NMM_BIN_MPIIO.f      SNFRAC.f \
CALWXT_DOMINANT.f        INITPOST_NMM_BIN_MPIIO_IJK.f  SNFRAC_GFS.f \
CALWXT_EXPLICIT.f        INITPOST_NMM_BIN_MPIIO_IKJ.f  SOIL_mod.f \
CALWXT.f         INITPOST_NMM.f      SPLINE.f \
CALWXT_RAMER.f        INITPOST_RSM.f        SURFCE.f \
CALWXT_REVISED.f       intio_tags.f         svptbl.f \
CANRES.f         inventory_wrf_binary_file.f   TABLE.f \
CLDFRAC_ZHAO.f        kinds_mod.f           TABLEQ.f \
CLDRAD.f         LFMFLD.f                 TIMEF.f \
CLMAX.f          LFMFLD_GFS.f        TRPAUS.f \
CMASSI.f         LOOKUP.f                 TRPAUS_NAM.f \
COLLECT.f          machine.f              TTBLEX.f \
COLLECT_LOC.f        MAPSSLP.f            VRBLS2D_mod.f \
count_recs_wrf_binary_file.f  MASKS_mod.f               VRBLS3D_mod.f \
CTLBLK.f         MDL2AGL.f                VRBLS4D_mod.f \
cuparm.f         MDL2P.f                  WETBULB.f \
DEALLOCATE.f        MDL2SIGMA2.f          WETFRZLVL.f \
DEWPOINT.f         MDL2SIGMA.f       wrf_io_flags.f \
ETCALC.f         MDL2THANDPV.f       WRFPOST.f \
EXCH2.f          MDLFLD.f                 ZENSUN.f \
EXCH.f         mersenne_twister.f

#
# Includes
#
##INCLUDES= parm.tbl parmeta parmout parmsoil cuparm
#INCLUDES= parm.tbl cuparm
#
# Common Blocks
#
#COMMS=	LOOKUP.comm   RQSTFLD.comm   CTLBLK.comm  \
#        GRIDSPEC.comm CMASSI.comm RHGRD.comm

DEPS= $(COMMS) $(INCLUDES) $(SRCS)

.SUFFIXES:	.F .f

.F.f:
	$(CPP) $(CPPFLAGS) $< > $*.f

$(TARGET):	$(OBJSM) $(OBJSF) $(OBJST) $(OBJS)
	$(FC) $(LDFLAGS) -o $@ $(OBJSM) $(OBJSF) $(OBJST) $(OBJS) $(LIBS)

$(OBJS):	$(DEPS)
	$(FC) -c $(FFLAGS) $(@:.o=.f)

$(OBJST):	$(DEPS)
	$(FC) -c $(FFLAGST) $(@:.o=.f)

$(OBJSF):       %.o: %.f
	$(FC) -c $(FFLAGST) $(@:.o=.f)

$(OBJSM):       %.o: %.f
	$(FC) -c $(FFLAGSM) $(@:.o=.f)

clean:	
	/bin/rm -f  *.lst *.o *.mod gfsio_module.f
#
