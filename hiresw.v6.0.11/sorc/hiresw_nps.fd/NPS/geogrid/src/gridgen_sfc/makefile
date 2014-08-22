include ../../../configure.nps

LIBS    = ${LANDSFCUTIL_LIBS} ${W3_LIBS} ${IP_LIBS}

FLAGS = ${FFLAGS} ${R8} ${LANDSFCUTIL_INC} ${G2_INCMOD}

TARGETLIB = libgridgen_sfc.a

OBJS = calc_latlons.o \
       calc_tiles.o \
       find_bounds_ll.o \
       gaussian_reduced.o \
       green.o \
       grib_latlons.o \
       init_grib1.o \
       init_grib2.o \
       gridgen_sfc.o \
       interp_tiles.o \
       interp_to_mdl.o \
       leaf_area_index.o \
       lsmask_orog.o  \
       max_snow_albedo.o \
       mpimod.o \
       native_endianness.o \
       program_setup.o \
       roughness.o \
       slope_type.o \
       smooth.o \
       snow_free_albedo.o \
       soil_substrate.o \
       soil_vegtype_tile.o \
       sort.o

all: $(TARGETLIB)

${TARGETLIB}:	${OBJS}
	$(AR) ${TARGETLIB} ${OBJS}

calc_latlons.o: program_setup.o mpimod.o calc_latlons.f
	${FC} ${FLAGS} -c calc_latlons.f

calc_tiles.o: mpimod.o lsmask_orog.o calc_tiles.f
	${FC} ${FLAGS} -c calc_tiles.f

find_bounds_ll.o: program_setup.o mpimod.o find_bounds_ll.f
	${FC} ${FLAGS} -c find_bounds_ll.f

gaussian_reduced.o: program_setup.o gaussian_reduced.f
	${FC} ${FLAGS} -c gaussian_reduced.f

green.o: program_setup.o green.f
	${FC} ${FLAGS} -c green.f

gridgen_sfc.o: program_setup.o init_grib1.o init_grib2.o calc_latlons.o \
               lsmask_orog.o mpimod.o soil_vegtype_tile.o gridgen_sfc.f
	${FC} ${FLAGS} -c gridgen_sfc.f

init_grib1.o: program_setup.o calc_latlons.o init_grib1.f
	${FC} ${FLAGS} -c init_grib1.f

init_grib2.o: program_setup.o calc_latlons.o init_grib2.f
	${FC} ${FLAGS} -c init_grib2.f

grib_latlons.o: init_grib1.o init_grib2.o calc_latlons.o mpimod.o program_setup.o grib_latlons.f
	${FC} ${FLAGS} -c grib_latlons.f

interp_tiles.o: init_grib1.o init_grib2.o program_setup.o mpimod.o calc_latlons.o lsmask_orog.o interp_tiles.f
	${FC} ${FLAGS} -c interp_tiles.f

interp_to_mdl.o: native_endianness.o mpimod.o program_setup.o lsmask_orog.o \
                 calc_latlons.o init_grib1.o init_grib2.o interp_to_mdl.f
	${FC} ${FLAGS} -c interp_to_mdl.f

leaf_area_index.o: calc_latlons.o lsmask_orog.o program_setup.o init_grib1.o init_grib2.o leaf_area_index.f
	${FC} ${FLAGS} -c leaf_area_index.f

lsmask_orog.o: native_endianness.o program_setup.o init_grib1.o init_grib2.o calc_latlons.o mpimod.o lsmask_orog.f
	${FC} ${FLAGS} -c lsmask_orog.f

max_snow_albedo.o: program_setup.o max_snow_albedo.f
	${FC} ${FLAGS} -c max_snow_albedo.f

mpimod.o: mpimod.f
	${FC} ${FLAGS} -c mpimod.f

native_endianness.o: native_endianness.f
	${FC} ${FLAGS} -c native_endianness.f

program_setup.o: mpimod.o program_setup.f
	${FC} ${FLAGS} -c program_setup.f

roughness.o: mpimod.o soil_vegtype_tile.o program_setup.o lsmask_orog.o init_grib1.o init_grib2.o roughness.f
	${FC} ${FLAGS} -c roughness.f

slope_type.o: program_setup.o slope_type.f
	${FC} ${FLAGS} -c slope_type.f

smooth.o: smooth.f
	${FC} ${FLAGS} -c smooth.f

snow_free_albedo.o: program_setup.o snow_free_albedo.f
	${FC} ${FLAGS} -c snow_free_albedo.f

soil_substrate.o: native_endianness.o init_grib1.o init_grib2.o program_setup.o calc_latlons.o \
                  lsmask_orog.o mpimod.o soil_substrate.f
	${FC} ${FLAGS} -c soil_substrate.f

soil_vegtype_tile.o: native_endianness.o init_grib1.o init_grib2.o program_setup.o lsmask_orog.o \
                     interp_tiles.o mpimod.o soil_vegtype_tile.f
	${FC} ${FLAGS} -c soil_vegtype_tile.f

sort.o: sort.f
	${FC} ${FLAGS} -c sort.f

clean:
	${RM} *.o *.mod ${TARGETLIB}
