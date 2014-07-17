#
# Makefile for ESMA components.
#
# REVISION HISTORY:
#
# 3mar2004  Zaslavsky  Initial imlementation.
# 20oct2004  da Silva  Standardization.
# 14Mar2013  Lu        Add NEMS option
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
  include $(ESMADIR)/Config/NCEP_base.mk  # NCEP stuff


#                  ---------------------
#                  Standard ESMA Targets
#                  ---------------------

THIS := $(shell basename `pwd`)
NAME := $(patsubst %_GridComp,%,$(THIS))

  # All fvChem is a single library

esma_install install: $(AGCS) $(LIB_THIS) 
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

ifeq ($(DOING_NEMS),TRUE)
  DNEMS = $(D)NEMS
endif

SPMD = $(D)SPMD
DEBUG = $(D)DEBUG

SRCS := $(THIS)Mod.F90
OBJS := $(addsuffix .o, $(basename $(SRCS)))
ACGS := $(NAME)_ExportSpec___.h $(NAME)_GetPointer___.h $(NAME)_History___.rc $(NAME)_DeclarePointer___.h
DEPS := $(addsuffix .d, $(basename $(SRCS)))

INC_DIRS = . $(INC_MAPL_BASE) $(ESMAINC)/MAPL_Base
MOD_DIRS = . $(INC_ESMF) $(INC_MPEU) $(INC_PILGRIM) \
             $(INC_DIRS) $(INC_GEOS_SHARED) \
             $(INC_CHEM_BASE) $(INC_CHEM_SHARED) $(INC_CHEM) 

##USER_FDEFS  = $(DGEOS5) $(DEBUG)
USER_FDEFS  = $(DGEOS5) $(DEBUG) ${DNEMS}
USER_FFLAGS = $(BIG_ENDIAN) 
USER_FMODS  = $(foreach dir,$(MOD_DIRS),$(M)$(dir)) 
USER_FINCS  = $(foreach dir,$(INC_DIRS),$(I)$(dir)) 

vpath % $(MOD_DIRS)

#FOPT = $(FOPT3)

$(LIB_THIS) lib : $(ACGS) $(OBJS)
	$(AR) $(AR_FLAGS) $(LIB_THIS) $(OBJS)

$(ACGS) : $(NAME)_Registry.rc $(ACG)
	@$(ACG) $(ACG_FLAGS) -B -C -N GOCART $(NAME)_Registry.rc

$(DEPS) : $(ACGS)

# Hack to prevent remaking dep files during cleaning
# --------------------------------------------------
  ifneq ($(findstring clean,$(MAKECMDGOALS)),clean)
    -include $(DEPS)
  endif

#.

  -include $(ESMADIR)/Config/ESMA_post.mk  # ESMA additional targets, macros

