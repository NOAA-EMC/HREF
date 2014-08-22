#
# Makefile for ESMA components.
#
# REVISION HISTORY:
#
# 3mar2004  Zaslavsky  Initial imlementation.
# 20Oct2004  da Silva  Standardization
#

# Make sure ESMADIR is defined
# ----------------------------
ifndef ESMADIR
       ESMADIR := ../../..
endif

# Compilation rules, flags, etc
# -----------------------------
  include $(ESMADIR)/Config/ESMA_base.mk  # Generic stuff
  include $(ESMADIR)/Config/ESMA_arch.mk  # System dependencies
  include $(ESMADIR)/Config/GMAO_base.mk  # System dependencies


#                  ---------------------
#                  Standard ESMA Targets
#                  ---------------------

THIS := $(shell basename `pwd`)
LIB  = lib$(THIS).a

# Handle stubs for NCEP implementarion of GOCART Stand alone
# ----------------------------------------------------------
  ifeq ($(GOCART_MODE),stub)
     BINS = ut_Registry.x
     CSHS = 
     SRCS := Chem_RegistryMod---stub.F90
     LIBS = $(LIB)
  else
     BINS = Chem_Aod.x Chem_Aod3d.x ctl_crst.x Chem_BundleToG5rs.x 
     CSHS = extinction_calculator.csh
     SRCS = Chem_RegistryMod.F90 Chem_ArrayMod.F90 \
            Chem_BundleMod.F90 Chem_Mod.F90 Chem_InitMod.F90 \
            Chem_MieTableMod.F90 Chem_MieMod.F90 Chem_AodMod.F90
     LIBS = $(LIB) $(LIB_EU) $(LIB_GFIO) \
            $(LIB_MFHDF3) $(LIB_SDF) \
            $(LIB_ESMF) $(LIB_SYS) $(LIB_MPI) 
  endif


ifeq ( $(wildcard $(INC_MAPL_BASE)), $(null) ) 
      DGEOS5 =
else
      MOD_DIRS += $(INC_ESMF) $(INC_MAPL_BASE) $(INC_GMAO_MPEU) 
      DGEOS5 = $(D)GEOS5
endif


#esma_install install: $(LIB) $(BINS)
esma_install install: $(LIB)
	$(MKDIR) $(ESMALIB) $(ESMAETC) $(ESMAINC)/$(THIS) $(ESMABIN)
	$(CP) -p *.a             $(ESMALIB)
	$(CP) -p *.rc            $(ESMAETC)
	$(CP) -p *.mod           $(ESMAINC)/$(THIS)
	$(CP) -p $(CSHS) $(ESMABIN)

esma_clean esma_distclean clean:
	$(RM) *~ *.[aoxd] *.[Mm][Oo][Dd] *.x

esma_doc doc:
	@echo "Target $@ not implemented yet in `pwd`"


esma_help help:
	@echo "Standard ESMA targets:"
	@echo "% make esma_install    (builds and install under ESMADIR)"
	@echo "% make esma_clean      (removes deliverables: *.[aox], etc)"
	@echo "% make esma_distclean  (leaves in the same state as cvs co)"
	@echo "% make esma_doc        (generates PDF, installs under ESMADIR)"
	@echo "% make esma_help       (this message)"
	@echo "Environment:"
	@echo "      ESMADIR = $(ESMADIR)"
	@echo "      BASEDIR = $(BASEDIR)"
	@echo "         ARCH = $(ARCH)"
	@echo "         SITE = $(SITE)"
	@echo "        FREAL = $(FREAL)"

#                  --------------------
#                  User Defined Targets
#                  --------------------
OBJS := $(addsuffix .o, $(basename $(SRCS)))
DEPS := $(addsuffix .d, $(basename $(SRCS))) \
        $(addsuffix .d, $(basename $(BINS))) 

BIG_ENDIAN =
THIS_GFIO = GMAO_gfio_r4
#FOPT = $(FOPT3)

ifeq ($(ESMA_REAL),$(FREAL8))
	THIS_GFIO = GMAO_gfio_r8
	FREAL = $(FREAL8)
else
	THIS_GFIO = GMAO_gfio_r4
	FREAL = $(FREAL4)
endif

MOD_DIRS += . $(INC_MAPL_BASE) $(INC_MPEU) $(INC_MFHDF3) 

USER_FFLAGS = $(BIG_ENDIAN) $(DGEOS5)
USER_FMODS  = $(foreach dir,$(MOD_DIRS),$(M)$(dir)) 
USER_FINCS  = $(foreach dir,$(MOD_DIRS),$(I)$(dir)) 

vpath % $(MOD_DIRS)

$(LIB) lib : $(DEPS) $(OBJS)
	$(RM) $(LIB)
	$(AR) $(AR_FLAGS) $(LIB) $(OBJS)

%.x : $(LIB) %.o 
	$(LD) $(LDFLAGS) -o $@ $*.o $(LIBS)

# Hack to prevent remaking dep files during cleaning
# --------------------------------------------------
  ifneq ($(findstring clean,$(MAKECMDGOALS)),clean)
    -include $(DEPS)
  endif

  -include $(ESMADIR)/Config/ESMA_post.mk  # ESMA additional targets, macros

#.
