#
# Makefile for ESMA components.
#
# REVISION HISTORY:
#
# 20oct2004  da Silva  First ESMA version.
# 23Aug2006  da Silva  Driver for making either MAPL or GEOS_Base.
# 26Aug2008  Stassi    No longer builds GEOS_Base; merged with MAPL_base.mk
# 27Oct2008  da SIlva  Some clean up for ESMF v3
# 03oct2011  Lu        Add NEMS option
#
# -------------------------------------------------------------------------


# Make sure ESMADIR is defined
# ----------------------------
ifndef ESMADIR
       ESMADIR := $(shell pwd)/../../..
endif

THIS := $(shell basename `pwd`)
LIB  = lib$(THIS).a

# Compilation rules, flags, etc
# -----------------------------
  include $(ESMADIR)/Config/ESMA_base.mk  # Generic stuff
  include $(ESMADIR)/Config/ESMA_arch.mk  # System dependencies
  include $(ESMADIR)/Config/GMAO_base.mk
  include $(ESMADIR)/Config/NCEP_base.mk  # NCEP stuff


#                  ---------------------
#                  Standard ESMA Targets
#                  ---------------------

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
	@echo "         SITE = $(SITE) "
	@echo "         THIS = $(THIS) "

esma_install install: $(LIB)
	$(MKDIR) $(ESMALIB) $(ESMAINC)/$(THIS)
	$(CP) -p *.a                       $(ESMALIB)
	$(CP) -p *.[Mm][Oo][Dd] $(IINCS)   $(ESMAINC)/$(THIS)
	$(CP) -p *.pl                      $(ESMABIN) 

esma_clean clean:
	-$(RM) *~ *.[aox] *.[Mm][Oo][Dd] \
          *.tex *.dvi *.ps *.pdf *.aux *.toc *.log 

esma_distclean distclean: clean
	-$(RM) *~ *.tex *.d


#                  --------------------
#                  User Defined Targets
#                  --------------------
ifeq ($(DOING_NEMS),TRUE)
  DNEMS = $(D)NEMS
endif

INCS :=  MAPL_ErrLog.h            MAPL_ErrLogMain.h      MAPL_Generic.h \
         MAPL_Exceptions.h

CODE :=  allgather.code           bcast.code             recv.code      \
         allgatherv.code          gather.code            scatter.code\
         arraygather.code         sun.code               send.code\
         arrayscatter.code        read_parallel.code     sendrecv.code\
         allreducemin.code        allreducemax.code      allreducesum.code

SRCS :=  ESMFL_Mod.P90            MAPL_HeapMod.F90       MAPL_VarSpecMod.F90\
         MAPL_Base.F90            MAPL_InterpMod.F90     hinterp.F\
         MAPL_CFIO.F90            MAPL_IO.P90            MAPL_HorzTransform.F90\
         MAPL_Comms.P90           MAPL_LocStreamMod.F90  MAPL_Mod.F90\
         MAPL_Constants.F90       MAPL_Profiler.F90      sort.c\
         MAPL_GenericCplComp.F90  MAPL_Sort.F90\
         MAPL_Generic.F90         MAPL_sun_uc.P90        MAPL_SatVapor.F90\
         MAPL_HistoryGridComp.F90 MAPL_Cap.F90\
         memuse.c                 MAPL_MemUtils.F90

TESTS = utCFIO_Bundle.x utCFIO_Array.x utCFIO_Nbits.x 
IINCS = $(wildcard MAPL_*.h)

OBJS := $(addsuffix .o, $(basename $(SRCS)))
DEPS := $(addsuffix .d, $(basename $(SRCS)))

INC_DIRS = . $(INC_ESMF) $(INC_MPI) /usr/include
MOD_DIRS = $(INC_DIRS) $(INC_CFIO) $(INC_MPEU)

USER_FINCS  = $(foreach dir,$(INC_DIRS),$(I)$(dir)) 
USER_FMODS  = $(foreach dir,$(MOD_DIRS),$(M)$(dir)) 
##USER_FFLAGS = $(D)MAPL  $(D)ESMF_2_2_2_or_newer $(D)DO_COMMAS
USER_FFLAGS = $(D)MAPL  $(D)ESMF_2_2_2_or_newer $(D)DO_COMMAS  ${DNEMS}

FREAL = $(FREAL4) # for now, require 32 bit reals (R4)
THIS_CFIO = MAPL_cfio_r4

vpath % $(MOD_DIRS) tests TeX

$(LIB) lib : $(DEPS) $(OBJS)
	$(AR) $(AR_FLAGS) $(LIB) $(OBJS)

%.x : $(LIB) %.o
	$(FC) $(LDFLAGS) -o $@ $*.o $(LIB) \
              $(LIB_ESMF) $(LIB_CFIO) \
              $(wildcard $(LIB_MFHDF3)) $(LIB_SDF)\
              $(LIB_MPI) $(LIB_SYS)

test: $(TESTS)
	  @ argv="$(TESTS)" ;\
	  for test in $$argv; do			 \
	    ( echo " "; echo "---- Starting test $$test"; \
              mpirun -np 4 $$test ) \
	  done

#                  --------------------
#                     Documentation
#                  --------------------

PDF_DOCS = MAPL_UsersGuide.pdf  \
           MAPL_Base.pdf       \
           MAPL_Cap.pdf        \
           MAPL_CFIO.pdf       \
           MAPL_Exceptions.pdf \
           MAPL_HistoryGridComp.pdf       

esma_doc doc: $(PDF_DOCS)

UG_SRCS = MAPL_Generic.F90 MAPL_GenericCplComp.F90  \
          MAPL_sun_uc.P90 MAPL_SatVapor.F90

MAPL_UsersGuide.tex: $(UG_SRCS)
	$(PROTEX) -g -b -f $(UG_SRCS) > MAPL_UsersGuide___.tex
	$(CAT) TeX/MAPL_Intro.tex MAPL_UsersGuide___.tex > MAPL_UsersGuide.tex

# Hack to prevent remaking dep files during cleaning
# --------------------------------------------------
  ifneq ($(findstring clean,$(MAKECMDGOALS)),clean)
    -include $(DEPS)
  endif

  -include $(ESMADIR)/Config/ESMA_post.mk  # ESMA additional targets, macros

#.
