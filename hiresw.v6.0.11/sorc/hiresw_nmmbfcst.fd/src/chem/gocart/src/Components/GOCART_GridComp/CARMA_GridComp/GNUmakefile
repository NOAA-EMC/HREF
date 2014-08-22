#
# Makefile for ESMA components.
#
# REVISION HISTORY:
#
# 3mar2004  Zaslavsky  Initial imlementation.
# 20oct2004  da Silva  Standardization.
#

# Make sure ESMADIR is defined
# ----------------------------
ifndef ESMADIR
       ESMADIR := $(PWD)/../../..
endif

# Compilation rules, flags, etc
# -----------------------------
  include $(ESMADIR)/Config/ESMA_base.mk  # Generic stuff
  include $(ESMADIR)/Config/ESMA_arch.mk  # System dependencies
  include $(ESMADIR)/Config/GMAO_base.mk  # GMAO stuff

#                  ---------------------
#                  Standard ESMA Targets
#                  ---------------------

THIS := $(shell basename `pwd`)
NAME := $(patsubst %_GridComp,%,$(THIS))

  # All fvChem is a single library

#esma_install install: $(AGCS) $(LIB_THIS) 
esma_install install: $(ACGS) $(LIB_THIS) 
	$(MKDIR) $(ESMALIB) $(ESMAETC) $(INC_CHEM) $(ESMABIN)
	$(CP) -p *.rc        $(ESMAETC)
	$(CP) -p *.h *.mod   $(INC_CHEM)

esma_clean clean:
	$(RM) $(ACGS) *~ *.[aox] *.[Mm][Oo][Dd]

esma_distclean distclean:
	$(RM) $(ACGS) *~ *.[aoxd] *.[Mm][Oo][Dd]

esma_doc doc:
	@$(PROTEX) $(PROTEX_FLAGS) *GridComp*.[fF]* > $(ESMADOC)/$(THIS).tex


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

#                  --------------------
#                  User Defined Targets
#                  --------------------

ifeq ($(DOING_GEOS5),TRUE)
  DGEOS5 = $(D)GEOS5
endif

ifeq ($(ESMA_REAL),$(FREAL8))
	FREAL = $(FREAL8)
        SINGLE = 
else
	FREAL = $(FREAL4)
        SINGLE = $(D)SINGLE
endif

SPMD = $(D)SPMD
DEBUG = $(D)DEBUG

SRCS := carma_precision_mod.F90 \
        carma_constants_mod.F90 \
        carma_types_mod.F90 \
        carma_main_mod.F90 \
        carma_bins.F90 \
        prtsep.F90 \
        setuperr.F90 \
        setupaer.F90 \
        setupbins.F90 \
        setupvf.F90 \
        initnew.F90 \
        initaer.F90 \
        hydrostat.F90 \
        vaporp.F90 \
        supersat.F90 \
        initgas.F90 \
        initatm.F90 \
        init.F90 \
        prestep.F90 \
        smallconc.F90 \
        step.F90 \
        newstate.F90 \
        vertical.F90 \
        vertadv.F90 \
        vertdif.F90 \
        versol.F90 \
        setupckern.F90 \
        setupcoag.F90 \
        microslow.F90 \
        coagl.F90 \
        coagp.F90 \
        csolve.F90 \
        $(THIS)Mod.F90
OBJS := $(addsuffix .o, $(basename $(SRCS)))
ACGS := #$(NAME)_ExportSpec___.h $(NAME)_GetPointer___.h $(NAME)_History___.rc
DEPS := $(addsuffix .d, $(basename $(SRCS)))

INC_DIRS = . $(INC_MAPL_BASE) 
MOD_DIRS = . $(INC_ESMF) $(INC_MPEU) $(INC_PILGRIM) \
             $(INC_DIRS) $(INC_GEOS_SHARED) $(ESMAINC)/MAPL_Base \
             $(INC_CHEM_BASE) $(INC_CHEM_SHARED) $(INC_CHEM) 

USER_FDEFS  = $(DGEOS5) $(DEBUG) $(SINGLE)
USER_FFLAGS = $(BIG_ENDIAN) 
USER_FMODS  = $(foreach dir,$(MOD_DIRS),$(M)$(dir)) 
USER_FINCS  = $(foreach dir,$(INC_DIRS),$(I)$(dir)) 

vpath % $(MOD_DIRS)

#FOPT = $(FOPT3)

$(LIB_THIS) lib : $(ACGS) $(OBJS)
	$(AR) $(AR_FLAGS) $(LIB_THIS) $(OBJS)

$(ACGS) : $(NAME)_Registry.rc $(ACG)
	@$(ACG) $(ACG_FLAGS) -C -N GOCART $(NAME)_Registry.rc

$(DEPS) : $(ACGS)

# Hack to prevent remaking dep files during cleaning
# --------------------------------------------------
  ifneq ($(findstring clean,$(MAKECMDGOALS)),clean)
    -include $(DEPS)
  endif

#.

  -include $(ESMADIR)/Config/ESMA_post.mk  # ESMA additional targets, macros

