include ../../configure.nps

FFLAGS=$(FFLAGS) $(NEMSIO_INC)

OBJS = module_data.o nemsinterp.o ingest_metgrid.o parallel_module.o module_vinterp_routines.o process_domain_module.o module_date_pack.o gridinfo_module.o misc_definitions_module.o  module_assorted_definitions.o module_boundary.o output_nemsinit.o

all: 
	clear ;
	@echo " "
	@echo "go up two directories and type compile to build NPS"
	@echo " "
	@echo " "

nemsinterp.exe: $(OBJS)
	$(FC) $(LDFLAGS) -o $@ $(OBJS) $(DIO_LIBS) $(NEMSIO_LIBS) $(W3_LIBS) $(BACIO_LIBS)

module_data.o:

ingest_metgrid.o: module_data.o

module_vinterp_routines.o: module_data.o

module_date_pack.o:

nemsinterp.o: gridinfo_module.o  parallel_module.o process_domain_module.o

gridinfo_module.o: misc_definitions_module.o module_data.o

parallel_module.o:

misc_definitions_module.o:

process_domain_module.o: module_date_pack.o gridinfo_module.o parallel_module.o ingest_metgrid.o module_data.o module_vinterp_routines.o module_assorted_definitions.o module_boundary.o output_nemsinit.o

module_assorted_definitions.o: module_data.o 

module_boundary.o : module_data.o parallel_module.o

output_nemsinit.o : module_data.o parallel_module.o

clean:
	$(RM) $(OBJS) *.mod
	$(RM) nemsinterp.exe

superclean:	clean
