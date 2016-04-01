#	NPS component makefile

include ../configure.nps

LIB     = libdio.a
DIODUMP = diodump.exe

OBJS    = dio.o fbioc.o wrfheader.o

all: $(LIB) $(DIODUMP)

$(LIB): $(OBJS)
	$(RM) $@
	$(AR) $(ARFLAGS) $@ $(OBJS)
	$(RANLIB) $@

$(DIODUMP): diodump.f90 $(LIB)
	$(SFC) $(FFLAGS) -o $@ diodump.f90 $(LIB)
	
dio.o: dio.f90
	$(SFC) $(FFLAGS) -c -o $@ dio.f90

wrfheader.o: wrfheader.f90
	$(SFC) $(FFLAGS) -c -o $@ wrfheader.f90

fbioc.o: fbioc.c
	$(SCC) $(CPPFLAGS) -c -o $@ fbioc.c

clean:
	$(RM) -f *.o *.mod *.M $(LIB) $(DIODUMP)

superclean: clean
