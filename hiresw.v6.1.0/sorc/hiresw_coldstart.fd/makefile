SHELL=/bin/sh
#----------------------------------------------------------------
# makefile:
#    prgmmr: george gayno   ORG: NP2    DATE: 2005-09-15
#
# usage:
#    type "make"
#
# remarks: 
#    this program works best with 8 byte floats!!
#----------------------------------------------------------------

LIBS =  ${NEMSIO_LIB} ${W3NCO_LIBd} ${BACIO_LIB4} ${IP_LIBd} ${SFCIO_LIB4} \
        ${SP_LIBd}  ${NETCDF_LDFLAGS}

FCOMP=${COMP_MPI}
FFLAGS = -O0 -I${LANDSFCUTIL_INCd} ${NETCDF_INCLUDE} \
          -I${SFCIO_INC4} -I${NEMSIO_INC} \
          -convert big_endian -r8 -i4 -FR
LFLAGS = -openmp
FPPFLAGS=-fpp -DGFS=0 -save-temps


EXE = ../../exec/hiresw_coldstart

OBJS = program_setup.o \
       coldstart_wrf.o \
       read_data.o \
       interp_data.o \
       write_data.o \
       surface_chgres.o \
       machine.o

$(EXE): ${OBJS}
	$(FCOMP) $(FFLAGS) ${LFLAGS} -o ${EXE} ${OBJS} \
	${LANDSFCUTIL_LIBd} $(LIBS)

coldstart_wrf.o: program_setup.o read_data.o interp_data.o write_data.o coldstart_wrf.f
	${FCOMP} ${FFLAGS} -c coldstart_wrf.f

program_setup.o: program_setup.f
	${FCOMP} ${FFLAGS} -c program_setup.f

machine.o: machine.f
	${FCOMP} ${FFLAGS} -c machine.f

surface_chgres.o: machine.o surface_chgres.F
	${FCOMP} ${FFLAGS} ${FPPFLAGS} -c surface_chgres.F

read_data.o: program_setup.o surface_chgres.o read_data.f
	${FCOMP} ${FFLAGS} -c read_data.f

interp_data.o: program_setup.o read_data.o surface_chgres.o interp_data.f
	${FCOMP} ${FFLAGS} -c interp_data.f

write_data.o: interp_data.o program_setup.o write_data.f
	${FCOMP} ${FFLAGS} -c write_data.f

clean:
	rm -f *.o *.mod surface_chgres.i

clobber:
	rm -f *.o *.mod surface_chgres.i ${EXE}
