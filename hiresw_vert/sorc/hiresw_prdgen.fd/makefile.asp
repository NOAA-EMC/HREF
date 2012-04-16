SHELL=/bin/sh
#
#
SRCS=	ADD_WMO.f PRDGEN.f CTL_RDR.f \
	w3fi63.f FNDBIT.f GET_PDS.f MAK_WMO.f PUT_GB.f w3fi71.f w3fi75.f \
	SET_O11.f SET_O14.f SET_O23.f SET_O56.f SET_TBL.f SET_WMO.f \
        EXTEND.f FILTER_SC.f FILTER_UV.f GRIB_IN.f GRIB_OUT.f INTERP_PPT.f \
        INTERP_SC.f INTERP_UV.f READ_SORT_CTL.f READ_SORT_GRIB.f TILE_OUT.f

OBJS=	ADD_WMO.o PRDGEN.o CTL_RDR.o \
	w3fi63.o FNDBIT.o GET_PDS.o MAK_WMO.o PUT_GB.o w3fi71.o w3fi75.o \
	SET_O11.o SET_O14.o SET_O23.o SET_O56.o SET_TBL.o SET_WMO.o \
        EXTEND.o FILTER_SC.o FILTER_UV.o GRIB_IN.o GRIB_OUT.o INTERP_PPT.o \
        INTERP_SC.o INTERP_UV.o READ_SORT_CTL.o READ_SORT_GRIB.o TILE_OUT.o

# Tunable parameters
#
# FC		Name of the fortran compiling system to use
# LDFLAGS	Flags to the loader
# LIBS		List of libraries
# CMD		Name of the executable
# PROFLIB	Library needed for profiling
#
FC =		xlf
#LDFLAGS =	-p
#LDFLAGS =	-qsmp
LDFLAGS =	-bmaxdata:256000000 -bmaxstack:256000000 
#LDFLAGS =	-bmaxdata:4000000000 -bmaxstack:3000000000
LIBS =		/nwprod/w3lib90/w3lib_4 /nwprod/w3lib90/gemlib_4 \
	        /nwprod/w3lib90/iplib_4 /nwprod/w3lib90/bacio_4 
CMD =		hiresw_prdgen
PROFLIB =	-lprof

# To perform the default compilation, use the first line
# To compile with flowtracing turned on, use the second line
# To compile giving profile additonal information, use the third line
# WARNING:  SIMULTANEOUSLY PROFILING AND FLOWTRACING IS NOT RECOMMENDED 
FFLAGS =         -O -qarch=pwr3 -qnosave -g -C

# Lines from here on down should not need to be changed.  They are the
# actual rules which make uses to build prdgen.
#
all:		$(CMD)

$(CMD):		$(OBJS)
	$(FC) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)

# Make the profiled version of the command and call it prdgen.prof
#
$(CMD).prof:	$(OBJS)
	$(FC) $(LDFLAGS) -o $(@) $(OBJS) $(PROFLIB) $(LIBS)

clean:
	-rm -f $(OBJS)

clobber:	clean
	-rm -f $(CMD) $(CMD).prof

void:	clobber
	-rm -f $(SRCS) makefile
