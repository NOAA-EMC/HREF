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

NETLIB=/usrx/local/netcdf.3.5.0/lib

FC       = ncepmpxlf
CPP      = /lib/cpp -P
ARCH     = auto
CPPFLAGS = 
OPTS     = -O -g -qnosave -qarch=$(ARCH) -qmaxmem=-1 -NS2000
LIST     = 
FREE     = -qfree
TRAPS    = 
PROFILE  = 
DEBUG    = 
MEM      =
MAP      = 
ESSL     =
MASS     =
WRFLIB    = ./wrflibs/libwrflib.a  ./wrflibs/libwrfio_int.a ./wrflibs/libwrfio_nf.a \
            ./wrflibs/pack_utils.o  ./wrflibs/libesmf_time.a ./wrflibs/librsl_lite.a -L${NETLIB} -lnetcdf

SEARCH   =
#
# Assemble Options
#
FFLAGS   = $(OPTS) $(LIST) $(TRAPS) $(PROFILE) $(DEBUG) $(WRFFLAGS)
FFLAGST  = $(OPTS) $(LIST) $(FREE) $(TRAPS) $(PROFILE) $(DEBUG) $(WRFFLAGS)
LDFLAGS  = $(MEM) $(MAP) $(SMP) $(PROFILE)
LIBS     = $(ESSL) $(MASS) $(SEARCH) $(WRFLIB)
#
#
# free-format object files
#
OBJST=	module_internal_header_util.o getIVariableB.o getIVariable.o getVariableB.o getVariable.o \
        kinds_mod.o \
        count_recs_wrf_binary_file.o inventory_wrf_binary_file.o \
        next_buf.o retrieve_index.o
#
# fixed-format object files
#
OBJS=	VRBLS2D_mod.o VRBLS3D_mod.o MASKS_mod.o SOIL_mod.o \
 WRFBUFR.o PROF_EM.o PROF_NMM.o PROF_NMM_NET.o PROF_EM_NET.o \
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
	$(FC) $(FFLAGS) -c $<

$(OBJST):	$(DEPS)
	$(FC) $(FFLAGST) -c $<

clean:	
	/bin/rm -f  $(TARGET) *.lst *.o *.mod
#
