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
#         CMD      - name of the executable
#         FC       - name of Fortran compiler
#         CPP      - name of CPP
#         ARCH     - architecture
#         CPPFLAGS - CPP flags
#         OPTS     - compiler code optimizations
#         TRAPS    - runtime traps for floating point exceptions
#         DEBUG    - -g
#
#################################################################################
#
# Define the name of the executable
#
CMD = ../../exec/hiresw_wrfbufr
#
# CPP, Compiler, and Linker Options
#

# FC       = ${FC} 

CPP      = /lib/cpp -P
ARCH     =
CPPFLAGS = 
OPTS     = -O2 -xAVX  -convert big_endian -assume noold_ldout_format
FREE     = -FR
TRAPS    = 
DEBUG    = -g # -traceback -fpe0 -ftrapuv # -check all 

 WRFLIB    = ./wrflibs/libwrflib.a  \
             ./wrflibs/pack_utils.o  \
             ./wrflibs/libfftpack.a \
            ./wrflibs/libio_grib1.a \
            ./wrflibs/libio_grib_share.a \
            ./wrflibs/libwrfio_int.a \
            ./wrflibs/libwrfio_nf.a \
            ./wrflibs/libesmf_time.a \
            ./wrflibs/librsl_lite.a


#
# Assemble Options
#
FFLAGS   = $(OPTS) $(TRAPS) $(DEBUG) $(WRFFLAGS) -I$(NEMSIO_INC)
FFLAGST  = $(OPTS) $(FREE) $(TRAPS) $(DEBUG) $(WRFFLAGS) -I$(NEMSIO_INC)
LDFLAGS  =
LIBS     = $(WRFLIB) \
           $(NEMSIO_LIB) \
           $(W3EMC_LIB4) $(W3NCO_LIB4) $(BACIO_LIB4) \
           $(NETCDF_LDFLAGS_F)
#
#
# free-format object files
#
OBJST= module_internal_header_util.o getIVariableB.o getIVariable.o getVariableB.o getVariable.o \
        kinds_mod.o \
        count_recs_wrf_binary_file.o inventory_wrf_binary_file.o \
        next_buf.o retrieve_index.o GETNEMSANDPLACE.o PARA_RANGE.o


#
# fixed-format object files
#
OBJS=	VRBLS2D_mod.o VRBLS3D_mod.o MASKS_mod.o SOIL_mod.o \
 WRFBUFR.o PROF_EM.o PROF_NMM.o  PROF_NMM_NET.o PROF_EM_NET.o PROF_NMMB_SERIAL.o \
iw3jdn.o w3fs26.o w3movdat.o w3reddat.o gdswiz.o gdswiz01.o \
gdswiz03.o gdswiz05.o
#
SRCS=	PROF_EM.f PROF_NMMB_SERIAL.f WRFBUFR.f
#
# Includes
#
INCLUDES= parmsoil
#

DEPS= $(INCLUDES)

.SUFFIXES:	.F .f .o

.F.f:
	$(CPP) $(CPPFLAGS) $< > $*.f

$(CMD):	$(OBJST) $(OBJS)
	$(FC) $(LDFLAGS) -o $@ $(OBJST) $(OBJS) $(LIBS) 

$(OBJS):	$(DEPS)
	$(FC) -c $(FFLAGS) $(@:.o=.f)

$(OBJST):	$(DEPS)
	$(FC) -c $(FFLAGST) $(@:.o=.f90)

clean:	
	/bin/rm -f  *.lst *.o *.mod
#
