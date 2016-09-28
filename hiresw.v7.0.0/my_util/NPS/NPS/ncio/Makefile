#	NPS component makefile

include ../configure.nps

M4      = m4 -Uinclude -Uindex -Ulen

LIB     = libncio.a

OBJS    = wrf_io.o field_routines.o

all: $(LIB)

$(LIB): $(OBJS)
	$(RM) $@
	$(AR) $(ARFLAGS) $@ $(OBJS)
	$(RANLIB) $@

wrf_io.o: wrf_io.F90
	$(CPP) -DWRFIO_NCD_LARGE_FILE_SUPPORT wrf_io.F90 | $(M4) - > wrf_io.f90
	$(SFC) $(FFLAGS) $(NETCDF_INC) -c -o $@ wrf_io.f90

clean:
	$(RM) -f *.o *.mod *.M $(LIB)

superclean: clean
