include ../../configure.nps

OBJS = cio.o wrf_debug.o bitarray_module.o constants_module.o despace.o geogrid.o gridinfo_module.o hash_module.o interp_module.o list_module.o llxy_module.o misc_definitions_module.o module_debug.o module_map_utils.o output_module.o parallel_module.o process_tile_module.o process_ncep_module.o proc_point_module.o queue_module.o read_geogrid.o smooth_module.o source_data_module.o sfc_grib2nc.o

all:
	clear ;
	@echo " "
	@echo "go up two directories and type compile to build WPS"
	@echo " "
	@echo " "

GRIDGEN_SFC_LIBS = gridgen_sfc/libgridgen_sfc.a

geogrid.exe: $(GRIDGEN_SFC_LIBS) $(OBJS)
	$(FC) $(LDFLAGS) -o $@ $(OBJS) $(DIO_LIBS) $(NCIO_LIBS) $(GRIDGEN_SFC_LIBS) $(LANDSFCUTIL_LIBS) $(G2_LIBS) $(COMPRESSION_LIBS) $(W3D_LIBS) $(IP_LIBS) $(SP_LIBS) $(BACIO_LIBS) $(NETCDF_LIB)

$(GRIDGEN_SFC_LIBS):
	( cd gridgen_sfc ; $(MAKE) )

bitarray_module.o: module_debug.o

cio.o:

constants_module.o:

despace.o:

geogrid.o: bitarray_module.o constants_module.o hash_module.o gridinfo_module.o llxy_module.o misc_definitions_module.o module_debug.o output_module.o parallel_module.o process_tile_module.o process_ncep_module.o proc_point_module.o queue_module.o smooth_module.o source_data_module.o list_module.o sfc_grib2nc.o $(GRIDGEN_SFC_LIBS)

gridinfo_module.o: constants_module.o misc_definitions_module.o module_debug.o

hash_module.o:

interp_module.o: bitarray_module.o misc_definitions_module.o module_debug.o queue_module.o

list_module.o: module_debug.o

llxy_module.o: gridinfo_module.o module_debug.o module_map_utils.o misc_definitions_module.o list_module.o

misc_definitions_module.o:

module_debug.o: parallel_module.o

module_map_utils.o: constants_module.o misc_definitions_module.o module_debug.o

output_module.o: gridinfo_module.o llxy_module.o misc_definitions_module.o module_debug.o parallel_module.o source_data_module.o

parallel_module.o:

process_tile_module.o: bitarray_module.o constants_module.o hash_module.o gridinfo_module.o interp_module.o llxy_module.o misc_definitions_module.o module_debug.o module_map_utils.o output_module.o parallel_module.o proc_point_module.o queue_module.o smooth_module.o source_data_module.o

process_ncep_module.o: bitarray_module.o constants_module.o hash_module.o gridinfo_module.o interp_module.o llxy_module.o misc_definitions_module.o module_debug.o module_map_utils.o output_module.o parallel_module.o proc_point_module.o queue_module.o smooth_module.o source_data_module.o

proc_point_module.o: bitarray_module.o hash_module.o interp_module.o llxy_module.o misc_definitions_module.o module_debug.o source_data_module.o

one_record_to_ij.o:

sfc_grib2nc.o:

queue_module.o: module_debug.o

read_geogrid.o:

smooth_module.o: parallel_module.o

source_data_module.o: despace.o gridinfo_module.o hash_module.o list_module.o llxy_module.o misc_definitions_module.o module_debug.o

wrf_debug.o: cio.o

clean:
	( cd gridgen_sfc ; $(MAKE) clean )
	$(RM) $(OBJS) *.mod
	$(RM) geogrid.exe 

superclean:	clean
