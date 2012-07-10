################################################################################
# 
#     Makefile for WRFBUFR code
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
#################################################################################
#
# Define the name of the executable
#
TARGET = ../../exec/hiresw_wrfbufr
#
# CPP, Compiler, and Linker Options
#

NETLIB   = /apps/netcdf/3.6.3/intel/lib

WRFPATH  = /scratch2/portfolios/NCEPDEV/meso/save/Matthew.Pyle/wcoss_transition/sorc/hiresw_real_fcst.fd
FC       = ifort -lmpi
CPP      = /lib/cpp -P
ARCH     =
CPPFLAGS = 
OPTS     = -O0 -assume byterecl
# OPTS     = -O2 -assume byterecl
LIST     = 
FREE     = -FR
TRAPS    = 
PROFILE  = 
DEBUG    = -g -traceback -fpe0 -ftrapuv # -check all 
MEM      =
MAP      =
ESSL     =
MASS     =
W3FLAGS = -I/home/Ratko.Vasic/lib_tmp
WRFFFLAGS = -I$(WRFPATH)/external/io_quilt
NEMSIOFLAG = -I/home/Ratko.Vasic/lib_tmp/nemsio/incmod
WRFLIB    = $(WRFPATH)/main/libwrflib.a  \
            $(WRFPATH)/frame/pack_utils.o  \
            $(WRFPATH)/external/io_grib1/libio_grib1.a \
            $(WRFPATH)/external/io_grib_share/libio_grib_share.a \
            $(WRFPATH)/external/io_int/libwrfio_int.a \
            $(WRFPATH)/external/io_netcdf/libwrfio_nf.a \
            $(WRFPATH)/external/esmf_time_f90/libesmf_time.a \
            $(WRFPATH)/external/RSL_LITE/librsl_lite.a

SEARCH   =
#
# Assemble Options
#
FFLAGS   = $(OPTS) $(LIST) $(TRAPS) $(PROFILE) $(DEBUG) $(WRFFLAGS) $(W3FLAGS) $(NEMSIOFLAG)
FFLAGST  = $(OPTS) $(LIST) $(FREE) $(TRAPS) $(PROFILE) $(DEBUG) $(WRFFLAGS) $(W3FLAGS) $(NEMSIOFLAG)
LDFLAGS  = $(MEM) $(MAP) $(SMP) $(PROFILE)
LIBS     = $(ESSL) $(MASS) $(SEARCH) $(WRFLIB) \
           -L/home/Ratko.Vasic/lib_tmp/nemsio -lnemsio \
           -L/home/Ratko.Vasic/lib_tmp \
           -lw3-2.0_4  -lbacio_4 \
           -L${NETLIB} -lnetcdf
#
#
# free-format object files
#
OBJST=	module_internal_header_util.o getIVariableB.o getIVariable.o getVariableB.o getVariable.o \
        kinds_mod.o \
        count_recs_wrf_binary_file.o inventory_wrf_binary_file.o \
        next_buf.o retrieve_index.o GETNEMSANDPLACE.o
#
# fixed-format object files
#
OBJS=	VRBLS2D_mod.o VRBLS3D_mod.o MASKS_mod.o SOIL_mod.o \
 WRFBUFR.o PROF_EM.o PROF_NMM.o  PROF_NMMB.o PROF_NMM_NET.o PROF_EM_NET.o \
iw3jdn.o w3fs26.o w3movdat.o w3reddat.o gdswiz.o gdswiz01.o \
gdswiz03.o gdswiz05.o
#
# Includes
#
INCLUDES= parmsoil
#
# Common Blocks
#
COMMS=

DEPS= $(COMMS) $(INCLUDES)

.SUFFIXES:	.F .f .o

.F.f:
	$(CPP) $(CPPFLAGS) $< > $*.f

$(TARGET):	$(OBJST) $(OBJS)
	$(FC) $(LDFLAGS) -o $@ $(OBJST) $(OBJS) $(LIBS) 

$(OBJS):	$(DEPS)
	$(FC) -c $(FFLAGS) $(@:.o=.f)

$(OBJST):	$(DEPS)
	$(FC) -c $(FFLAGST) $(@:.o=.f)

clean:	
	/bin/rm -f  $(TARGET) *.lst *.o *.mod
#
