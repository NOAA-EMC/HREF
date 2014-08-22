#
# Earth System Modeling Applications (ESMA) post processing makefile fragment.
# Optionally, include this at the end of your makefile.
#
# REVISION HISTORY:
#
# 15Dec2006  da Silva  First Crack
#--------------------------------------------------------------------------

#                     -----------------
#                     Parallel Install
#                     -----------------

LOCAL_INSTALL = local_install

PINSTALL_DIRS = $(foreach dir,$(SUBDIRS),$(dir)_install) 
ifeq ($(SUBDIRS),$(null))
	PINSTALL_TARGET = install
else
	PINSTALL_TARGET = $(LOCAL_INSTALL)
endif

pinstall: $(PINSTALL_DIRS) 
	$(MAKE) -e $(PINSTALL_TARGET)

%_install: 
	@$(ESMA_TIMER_BEG)
	$(MAKE) -e -C $* pinstall
	@$(ESMA_TIMER_END)

pinstall_skip:
	@echo "Skipping local_install in `pwd`"

