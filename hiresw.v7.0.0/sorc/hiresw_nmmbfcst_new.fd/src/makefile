SHELL = /bin/sh

export TOP := $(shell pwd)

# check if configure.nems file exists

exist := $(wildcard ./conf/configure.nems)
ifneq ($(strip $(exist)),)
  include ./conf/configure.nems
else
  $(error Error: !!!!!! Configure file is missing. Please create ./conf/configure.nems  by running ./configure !!!!!!)
endif

# conditionally read in the paths for external components

ifndef NEMSAppBuilder
  exist := $(wildcard ./conf/externals.nems)
  ifneq ($(strip $(exist)),)
    include ./conf/externals.nems
  else
    $(error Error: !!!!!! Externals file is missing. Please create ./conf/externals.nems  by running ./configure !!!!!!)
  endif
endif

# Need a variable that contains only a comma for some of the operations below
COMMA := ,

# external ATM dependencies ####################################################
ifneq (,$(findstring satm,$(ATM)))
  include $(satm_mk)
  DEP_FRONTS    := $(DEP_FRONTS) -DFRONT_SATM=$(ESMF_DEP_FRONT)
  DEP_INCS      := $(DEP_INCS) $(addprefix -I, $(ESMF_DEP_INCPATH))
  DEP_CMPL_OBJS := $(DEP_CMPL_OBJS) $(ESMF_DEP_CMPL_OBJS)
  DEP_LINK_OBJS := $(DEP_LINK_OBJS) $(ESMF_DEP_LINK_OBJS)
  DEP_SHRD_PATH := $(DEP_SHRD_PATH) $(addprefix -L, $(ESMF_DEP_SHRD_PATH)) $(addprefix -Wl$(COMMA)-rpath$(COMMA), $(ESMF_DEP_SHRD_PATH))
  DEP_SHRD_LIBS := $(DEP_SHRD_LIBS) $(addprefix -l, $(ESMF_DEP_SHRD_LIBS))
endif
ifneq (,$(findstring xatm,$(ATM)))
  include $(xatm_mk)
  DEP_FRONTS    := $(DEP_FRONTS) -DFRONT_XATM=$(ESMF_DEP_FRONT)
  DEP_INCS      := $(DEP_INCS) $(addprefix -I, $(ESMF_DEP_INCPATH))
  DEP_CMPL_OBJS := $(DEP_CMPL_OBJS) $(ESMF_DEP_CMPL_OBJS)
  DEP_LINK_OBJS := $(DEP_LINK_OBJS) $(ESMF_DEP_LINK_OBJS)
  DEP_SHRD_PATH := $(DEP_SHRD_PATH) $(addprefix -L, $(ESMF_DEP_SHRD_PATH)) $(addprefix -Wl$(COMMA)-rpath$(COMMA), $(ESMF_DEP_SHRD_PATH))
  DEP_SHRD_LIBS := $(DEP_SHRD_LIBS) $(addprefix -l, $(ESMF_DEP_SHRD_LIBS))
endif

# external OCN dependencies ####################################################
ifneq (,$(findstring socn,$(OCN)))
  include $(socn_mk)
  DEP_FRONTS    := $(DEP_FRONTS) -DFRONT_SOCN=$(ESMF_DEP_FRONT)
  DEP_INCS      := $(DEP_INCS) $(addprefix -I, $(ESMF_DEP_INCPATH))
  DEP_CMPL_OBJS := $(DEP_CMPL_OBJS) $(ESMF_DEP_CMPL_OBJS)
  DEP_LINK_OBJS := $(DEP_LINK_OBJS) $(ESMF_DEP_LINK_OBJS)
  DEP_SHRD_PATH := $(DEP_SHRD_PATH) $(addprefix -L, $(ESMF_DEP_SHRD_PATH)) $(addprefix -Wl$(COMMA)-rpath$(COMMA), $(ESMF_DEP_SHRD_PATH))
  DEP_SHRD_LIBS := $(DEP_SHRD_LIBS) $(addprefix -l, $(ESMF_DEP_SHRD_LIBS))
endif
ifneq (,$(findstring xocn,$(OCN)))
  include $(xocn_mk)
  DEP_FRONTS    := $(DEP_FRONTS) -DFRONT_XOCN=$(ESMF_DEP_FRONT)
  DEP_INCS      := $(DEP_INCS) $(addprefix -I, $(ESMF_DEP_INCPATH))
  DEP_CMPL_OBJS := $(DEP_CMPL_OBJS) $(ESMF_DEP_CMPL_OBJS)
  DEP_LINK_OBJS := $(DEP_LINK_OBJS) $(ESMF_DEP_LINK_OBJS)
  DEP_SHRD_PATH := $(DEP_SHRD_PATH) $(addprefix -L, $(ESMF_DEP_SHRD_PATH)) $(addprefix -Wl$(COMMA)-rpath$(COMMA), $(ESMF_DEP_SHRD_PATH))
  DEP_SHRD_LIBS := $(DEP_SHRD_LIBS) $(addprefix -l, $(ESMF_DEP_SHRD_LIBS))
endif
ifneq (,$(findstring hycom,$(OCN)))
  include $(hycom_mk)
  DEP_FRONTS    := $(DEP_FRONTS) -DFRONT_HYCOM=$(ESMF_DEP_FRONT)
  DEP_INCS      := $(DEP_INCS) $(addprefix -I, $(ESMF_DEP_INCPATH))
  DEP_CMPL_OBJS := $(DEP_CMPL_OBJS) $(ESMF_DEP_CMPL_OBJS)
  DEP_LINK_OBJS := $(DEP_LINK_OBJS) $(ESMF_DEP_LINK_OBJS)
  DEP_SHRD_PATH := $(DEP_SHRD_PATH) $(addprefix -L, $(ESMF_DEP_SHRD_PATH)) $(addprefix -Wl$(COMMA)-rpath$(COMMA), $(ESMF_DEP_SHRD_PATH))
  DEP_SHRD_LIBS := $(DEP_SHRD_LIBS) $(addprefix -l, $(ESMF_DEP_SHRD_LIBS))
endif
ifneq (,$(findstring mom5,$(OCN)))
  include $(mom5_mk)
  DEP_FRONTS    := $(DEP_FRONTS) -DFRONT_MOM5=$(ESMF_DEP_FRONT)
  DEP_INCS      := $(DEP_INCS) $(addprefix -I, $(ESMF_DEP_INCPATH))
  DEP_CMPL_OBJS := $(DEP_CMPL_OBJS) $(ESMF_DEP_CMPL_OBJS)
  DEP_LINK_OBJS := $(DEP_LINK_OBJS) $(ESMF_DEP_LINK_OBJS)
  DEP_SHRD_PATH := $(DEP_SHRD_PATH) $(addprefix -L, $(ESMF_DEP_SHRD_PATH)) $(addprefix -Wl$(COMMA)-rpath$(COMMA), $(ESMF_DEP_SHRD_PATH))
  DEP_SHRD_LIBS := $(DEP_SHRD_LIBS) $(addprefix -l, $(ESMF_DEP_SHRD_LIBS))
endif
ifneq (,$(findstring pom,$(OCN)))
  include $(pom_mk)
  DEP_FRONTS    := $(DEP_FRONTS) -DFRONT_POM=$(ESMF_DEP_FRONT)
  DEP_INCS      := $(DEP_INCS) $(addprefix -I, $(ESMF_DEP_INCPATH))
  DEP_CMPL_OBJS := $(DEP_CMPL_OBJS) $(ESMF_DEP_CMPL_OBJS)
  DEP_LINK_OBJS := $(DEP_LINK_OBJS) $(ESMF_DEP_LINK_OBJS)
  DEP_SHRD_PATH := $(DEP_SHRD_PATH) $(addprefix -L, $(ESMF_DEP_SHRD_PATH)) $(addprefix -Wl$(COMMA)-rpath$(COMMA), $(ESMF_DEP_SHRD_PATH))
  DEP_SHRD_LIBS := $(DEP_SHRD_LIBS) $(addprefix -l, $(ESMF_DEP_SHRD_LIBS))
endif
################################################################################

# external SEAICE dependencies #################################################
ifneq (,$(findstring sice,$(ICE)))
  include $(sice_mk)
  DEP_FRONTS    := $(DEP_FRONTS) -DFRONT_SICE=$(ESMF_DEP_FRONT)
  DEP_INCS      := $(DEP_INCS) $(addprefix -I, $(ESMF_DEP_INCPATH))
  DEP_CMPL_OBJS := $(DEP_CMPL_OBJS) $(ESMF_DEP_CMPL_OBJS)
  DEP_LINK_OBJS := $(DEP_LINK_OBJS) $(ESMF_DEP_LINK_OBJS)
  DEP_SHRD_PATH := $(DEP_SHRD_PATH) $(addprefix -L, $(ESMF_DEP_SHRD_PATH)) $(addprefix -Wl$(COMMA)-rpath$(COMMA), $(ESMF_DEP_SHRD_PATH))
  DEP_SHRD_LIBS := $(DEP_SHRD_LIBS) $(addprefix -l, $(ESMF_DEP_SHRD_LIBS))
endif
ifneq (,$(findstring xice,$(ICE)))
  include $(xice_mk)
  DEP_FRONTS    := $(DEP_FRONTS) -DFRONT_XICE=$(ESMF_DEP_FRONT)
  DEP_INCS      := $(DEP_INCS) $(addprefix -I, $(ESMF_DEP_INCPATH))
  DEP_CMPL_OBJS := $(DEP_CMPL_OBJS) $(ESMF_DEP_CMPL_OBJS)
  DEP_LINK_OBJS := $(DEP_LINK_OBJS) $(ESMF_DEP_LINK_OBJS)
  DEP_SHRD_PATH := $(DEP_SHRD_PATH) $(addprefix -L, $(ESMF_DEP_SHRD_PATH)) $(addprefix -Wl$(COMMA)-rpath$(COMMA), $(ESMF_DEP_SHRD_PATH))
  DEP_SHRD_LIBS := $(DEP_SHRD_LIBS) $(addprefix -l, $(ESMF_DEP_SHRD_LIBS))
endif
ifneq (,$(findstring cice,$(ICE)))
  include $(cice_mk)
  DEP_FRONTS    := $(DEP_FRONTS) -DFRONT_CICE=$(ESMF_DEP_FRONT)
  DEP_INCS      := $(DEP_INCS) $(addprefix -I, $(ESMF_DEP_INCPATH))
  DEP_CMPL_OBJS := $(DEP_CMPL_OBJS) $(ESMF_DEP_CMPL_OBJS)
  DEP_LINK_OBJS := $(DEP_LINK_OBJS) $(ESMF_DEP_LINK_OBJS)
  DEP_SHRD_PATH := $(DEP_SHRD_PATH) $(addprefix -L, $(ESMF_DEP_SHRD_PATH)) $(addprefix -Wl$(COMMA)-rpath$(COMMA), $(ESMF_DEP_SHRD_PATH))
  DEP_SHRD_LIBS := $(DEP_SHRD_LIBS) $(addprefix -l, $(ESMF_DEP_SHRD_LIBS))
endif
################################################################################

# external WAV dependencies ####################################################
ifneq (,$(findstring swav,$(WAV)))
  include $(swav_mk)
  DEP_FRONTS    := $(DEP_FRONTS) -DFRONT_SWAV=$(ESMF_DEP_FRONT)
  DEP_INCS      := $(DEP_INCS) $(addprefix -I, $(ESMF_DEP_INCPATH))
  DEP_CMPL_OBJS := $(DEP_CMPL_OBJS) $(ESMF_DEP_CMPL_OBJS)
  DEP_LINK_OBJS := $(DEP_LINK_OBJS) $(ESMF_DEP_LINK_OBJS)
  DEP_SHRD_PATH := $(DEP_SHRD_PATH) $(addprefix -L, $(ESMF_DEP_SHRD_PATH)) $(addprefix -Wl$(COMMA)-rpath$(COMMA), $(ESMF_DEP_SHRD_PATH))
  DEP_SHRD_LIBS := $(DEP_SHRD_LIBS) $(addprefix -l, $(ESMF_DEP_SHRD_LIBS))
endif
ifneq (,$(findstring xwav,$(WAV)))
  include $(xwav_mk)
  DEP_FRONTS    := $(DEP_FRONTS) -DFRONT_XWAV=$(ESMF_DEP_FRONT)
  DEP_INCS      := $(DEP_INCS) $(addprefix -I, $(ESMF_DEP_INCPATH))
  DEP_CMPL_OBJS := $(DEP_CMPL_OBJS) $(ESMF_DEP_CMPL_OBJS)
  DEP_LINK_OBJS := $(DEP_LINK_OBJS) $(ESMF_DEP_LINK_OBJS)
  DEP_SHRD_PATH := $(DEP_SHRD_PATH) $(addprefix -L, $(ESMF_DEP_SHRD_PATH)) $(addprefix -Wl$(COMMA)-rpath$(COMMA), $(ESMF_DEP_SHRD_PATH))
  DEP_SHRD_LIBS := $(DEP_SHRD_LIBS) $(addprefix -l, $(ESMF_DEP_SHRD_LIBS))
endif
ifneq (,$(findstring ww3,$(WAV)))
  include $(ww3_mk)
  DEP_FRONTS    := $(DEP_FRONTS) -DFRONT_WW3=$(ESMF_DEP_FRONT)
  DEP_INCS      := $(DEP_INCS) $(addprefix -I, $(ESMF_DEP_INCPATH))
  DEP_CMPL_OBJS := $(DEP_CMPL_OBJS) $(ESMF_DEP_CMPL_OBJS)
  DEP_LINK_OBJS := $(DEP_LINK_OBJS) $(ESMF_DEP_LINK_OBJS)
  DEP_SHRD_PATH := $(DEP_SHRD_PATH) $(addprefix -L, $(ESMF_DEP_SHRD_PATH)) $(addprefix -Wl$(COMMA)-rpath$(COMMA), $(ESMF_DEP_SHRD_PATH))
  DEP_SHRD_LIBS := $(DEP_SHRD_LIBS) $(addprefix -l, $(ESMF_DEP_SHRD_LIBS))
endif
################################################################################

# external LND dependencies ####################################################
ifneq (,$(findstring slnd,$(LND)))
  include $(slnd_mk)
  DEP_FRONTS    := $(DEP_FRONTS) -DFRONT_SLND=$(ESMF_DEP_FRONT)
  DEP_INCS      := $(DEP_INCS) $(addprefix -I, $(ESMF_DEP_INCPATH))
  DEP_CMPL_OBJS := $(DEP_CMPL_OBJS) $(ESMF_DEP_CMPL_OBJS)
  DEP_LINK_OBJS := $(DEP_LINK_OBJS) $(ESMF_DEP_LINK_OBJS)
  DEP_SHRD_PATH := $(DEP_SHRD_PATH) $(addprefix -L, $(ESMF_DEP_SHRD_PATH)) $(addprefix -Wl$(COMMA)-rpath$(COMMA), $(ESMF_DEP_SHRD_PATH))
  DEP_SHRD_LIBS := $(DEP_SHRD_LIBS) $(addprefix -l, $(ESMF_DEP_SHRD_LIBS))
endif
ifneq (,$(findstring xlnd,$(LND)))
  include $(xlnd_mk)
  DEP_FRONTS    := $(DEP_FRONTS) -DFRONT_XLND=$(ESMF_DEP_FRONT)
  DEP_INCS      := $(DEP_INCS) $(addprefix -I, $(ESMF_DEP_INCPATH))
  DEP_CMPL_OBJS := $(DEP_CMPL_OBJS) $(ESMF_DEP_CMPL_OBJS)
  DEP_LINK_OBJS := $(DEP_LINK_OBJS) $(ESMF_DEP_LINK_OBJS)
  DEP_SHRD_PATH := $(DEP_SHRD_PATH) $(addprefix -L, $(ESMF_DEP_SHRD_PATH)) $(addprefix -Wl$(COMMA)-rpath$(COMMA), $(ESMF_DEP_SHRD_PATH))
  DEP_SHRD_LIBS := $(DEP_SHRD_LIBS) $(addprefix -l, $(ESMF_DEP_SHRD_LIBS))
endif
ifneq (,$(findstring lis,$(LND)))
  include $(lis_mk)
  DEP_FRONTS    := $(DEP_FRONTS) -DFRONT_LIS=$(ESMF_DEP_FRONT)
  DEP_INCS      := $(DEP_INCS) $(addprefix -I, $(ESMF_DEP_INCPATH))
  DEP_CMPL_OBJS := $(DEP_CMPL_OBJS) $(ESMF_DEP_CMPL_OBJS)
  DEP_LINK_OBJS := $(DEP_LINK_OBJS) $(ESMF_DEP_LINK_OBJS)
  DEP_SHRD_PATH := $(DEP_SHRD_PATH) $(addprefix -L, $(ESMF_DEP_SHRD_PATH)) $(addprefix -Wl$(COMMA)-rpath$(COMMA), $(ESMF_DEP_SHRD_PATH))
  DEP_SHRD_LIBS := $(DEP_SHRD_LIBS) $(addprefix -l, $(ESMF_DEP_SHRD_LIBS))
endif
################################################################################

# external IPM dependencies ####################################################
ifneq (,$(findstring sipm,$(IPM)))
  include $(sipm_mk)
  DEP_FRONTS    := $(DEP_FRONTS) -DFRONT_SIPM=$(ESMF_DEP_FRONT)
  DEP_INCS      := $(DEP_INCS) $(addprefix -I, $(ESMF_DEP_INCPATH))
  DEP_CMPL_OBJS := $(DEP_CMPL_OBJS) $(ESMF_DEP_CMPL_OBJS)
  DEP_LINK_OBJS := $(DEP_LINK_OBJS) $(ESMF_DEP_LINK_OBJS)
  DEP_SHRD_PATH := $(DEP_SHRD_PATH) $(addprefix -L, $(ESMF_DEP_SHRD_PATH)) $(addprefix -Wl$(COMMA)-rpath$(COMMA), $(ESMF_DEP_SHRD_PATH))
  DEP_SHRD_LIBS := $(DEP_SHRD_LIBS) $(addprefix -l, $(ESMF_DEP_SHRD_LIBS))
endif
ifneq (,$(findstring xipm,$(IPM)))
  include $(xipm_mk)
  DEP_FRONTS    := $(DEP_FRONTS) -DFRONT_XIPM=$(ESMF_DEP_FRONT)
  DEP_INCS      := $(DEP_INCS) $(addprefix -I, $(ESMF_DEP_INCPATH))
  DEP_CMPL_OBJS := $(DEP_CMPL_OBJS) $(ESMF_DEP_CMPL_OBJS)
  DEP_LINK_OBJS := $(DEP_LINK_OBJS) $(ESMF_DEP_LINK_OBJS)
  DEP_SHRD_PATH := $(DEP_SHRD_PATH) $(addprefix -L, $(ESMF_DEP_SHRD_PATH)) $(addprefix -Wl$(COMMA)-rpath$(COMMA), $(ESMF_DEP_SHRD_PATH))
  DEP_SHRD_LIBS := $(DEP_SHRD_LIBS) $(addprefix -l, $(ESMF_DEP_SHRD_LIBS))
endif
ifneq (,$(findstring ipe,$(IPM)))
  include $(ipe_mk)
  DEP_FRONTS    := $(DEP_FRONTS) -DFRONT_IPE=$(ESMF_DEP_FRONT)
  DEP_INCS      := $(DEP_INCS) $(addprefix -I, $(ESMF_DEP_INCPATH))
  DEP_CMPL_OBJS := $(DEP_CMPL_OBJS) $(ESMF_DEP_CMPL_OBJS)
  DEP_LINK_OBJS := $(DEP_LINK_OBJS) $(ESMF_DEP_LINK_OBJS)
  DEP_SHRD_PATH := $(DEP_SHRD_PATH) $(addprefix -L, $(ESMF_DEP_SHRD_PATH)) $(addprefix -Wl$(COMMA)-rpath$(COMMA), $(ESMF_DEP_SHRD_PATH))
  DEP_SHRD_LIBS := $(DEP_SHRD_LIBS) $(addprefix -l, $(ESMF_DEP_SHRD_LIBS))
endif
################################################################################

# external HYD dependencies ####################################################
ifneq (,$(findstring shyd,$(HYD)))
  include $(shyd_mk)
  DEP_FRONTS    := $(DEP_FRONTS) -DFRONT_SHYD=$(ESMF_DEP_FRONT)
  DEP_INCS      := $(DEP_INCS) $(addprefix -I, $(ESMF_DEP_INCPATH))
  DEP_CMPL_OBJS := $(DEP_CMPL_OBJS) $(ESMF_DEP_CMPL_OBJS)
  DEP_LINK_OBJS := $(DEP_LINK_OBJS) $(ESMF_DEP_LINK_OBJS)
  DEP_SHRD_PATH := $(DEP_SHRD_PATH) $(addprefix -L, $(ESMF_DEP_SHRD_PATH)) $(addprefix -Wl$(COMMA)-rpath$(COMMA), $(ESMF_DEP_SHRD_PATH))
  DEP_SHRD_LIBS := $(DEP_SHRD_LIBS) $(addprefix -l, $(ESMF_DEP_SHRD_LIBS))
endif
ifneq (,$(findstring xhyd,$(HYD)))
  include $(xhyd_mk)
  DEP_FRONTS    := $(DEP_FRONTS) -DFRONT_XHYD=$(ESMF_DEP_FRONT)
  DEP_INCS      := $(DEP_INCS) $(addprefix -I, $(ESMF_DEP_INCPATH))
  DEP_CMPL_OBJS := $(DEP_CMPL_OBJS) $(ESMF_DEP_CMPL_OBJS)
  DEP_LINK_OBJS := $(DEP_LINK_OBJS) $(ESMF_DEP_LINK_OBJS)
  DEP_SHRD_PATH := $(DEP_SHRD_PATH) $(addprefix -L, $(ESMF_DEP_SHRD_PATH)) $(addprefix -Wl$(COMMA)-rpath$(COMMA), $(ESMF_DEP_SHRD_PATH))
  DEP_SHRD_LIBS := $(DEP_SHRD_LIBS) $(addprefix -l, $(ESMF_DEP_SHRD_LIBS))
endif
ifneq (,$(findstring wrfhydro,$(HYD)))
  include $(wrfhydro_mk)
  DEP_FRONTS    := $(DEP_FRONTS) -DFRONT_WRFHYDRO=$(ESMF_DEP_FRONT)
  DEP_INCS      := $(DEP_INCS) $(addprefix -I, $(ESMF_DEP_INCPATH))
  DEP_CMPL_OBJS := $(DEP_CMPL_OBJS) $(ESMF_DEP_CMPL_OBJS)
  DEP_LINK_OBJS := $(DEP_LINK_OBJS) $(ESMF_DEP_LINK_OBJS)
  DEP_SHRD_PATH := $(DEP_SHRD_PATH) $(addprefix -L, $(ESMF_DEP_SHRD_PATH)) $(addprefix -Wl$(COMMA)-rpath$(COMMA), $(ESMF_DEP_SHRD_PATH))
  DEP_SHRD_LIBS := $(DEP_SHRD_LIBS) $(addprefix -l, $(ESMF_DEP_SHRD_LIBS))
endif
################################################################################

CPPFLAGS += $(DEP_FRONTS)

TARGET = ../exe/NEMS.x

INCS = -Iatmos \
       -Ichem \
       -Iatmos/share \
       -IENS_Cpl \
       -Iatmos/nmm \
       -Iatmos/fim \
       -I$(gfsdir) \
       -Iatmos/phys \
       -Iatmos/gen \
       $(DEP_INCS)

MAIN = MAIN_NEMS.o

OBJS = module_MEDIATOR.o \
       module_MEDIATOR_SpaceWeather.o \
       module_EARTH_INTERNAL_STATE.o \
       module_EARTH_GRID_COMP.o \
       module_NEMS_INTERNAL_STATE.o \
       module_NEMS_GRID_COMP.o

COBJS = # c_code.o

LIBS = atmos/libatmos.a                 \
       atmos/nmm/libnmm.a               \
       $(gfsdir)/libgfs.a               \
       atmos/io/libio.a                 \
       atmos/gen/libgen.a               \
       atmos/fim/libfimtop.a            \
       $(gfsdir)/libutil/gfs_libutil.a  \
       $(gfsdir)/phys/gfs_physics.a     \
       atmos/phys/libphys.a             \
       $(gfsdir)/dyn/gfs_dynamics.a     \
       atmos/share/libshare.a           \
       atmos/post/libpost.a             \
       $(DEP_LINK_OBJS)                 \
       ENS_Cpl/ENS_Cpl.a

LIBS_NMM = atmos/libatmos.a             \
           atmos/nmm/libnmm.a           \
           $(gfsdir)/libgfs.a           \
           atmos/fim/libfimtop.a        \
           atmos/gen/libgen.a           \
           atmos/phys/libphys.a         \
           atmos/share/libshare.a       \
           atmos/post/libpost.a         \
           $(DEP_LINK_OBJS)             \
           ENS_Cpl/ENS_Cpl.a

LIBS_GEN = atmos/libatmos.a             \
           atmos/nmm/libnmm.a           \
           atmos/io/libio.a             \
           $(gfsdir)/libgfs.a           \
           atmos/fim/libfimtop.a        \
           atmos/gen/libgen.a           \
           atmos/phys/libphys.a         \
           atmos/share/libshare.a       \
           atmos/post/libpost.a         \
           $(DEP_LINK_OBJS)             \
           ENS_Cpl/ENS_Cpl.a

#TODO:  DRY these settings, they are redundant with atmos/fim/makefile and 
#TODO:  atmos/fim/FIMsrc ...  
SMS      = /gpfs/t3/global/save/wx20tbh/SMS/SMS_r237
SMSLIB   = $(SMS)/lib/libsms.a
# libraries created during build inside atmos/fim/FIM_src_devccs
FIMSRCLIBS = atmos/fim/FIMsrc_devccs/lib/libfim.a      \
             atmos/fim/FIMsrc_devccs/lib/libwrfio.a    \
             atmos/fim/FIMsrc_devccs/lib/libgribio.a   \
             atmos/fim/FIMsrc_devccs/lib/libvlint.a    \
             atmos/fim/FIMsrc_devccs/lib/libsp_4.a     \
             atmos/fim/FIMsrc_devccs/lib/libslint.a    \
             atmos/fim/FIMsrc_devccs/lib/libsigio_4.a  \
             atmos/fim/FIMsrc_devccs/lib/libsfcio_4.a  \
             atmos/fim/FIMsrc_devccs/lib/libw3_4.a     \
             atmos/fim/FIMsrc_devccs/lib/libbacio_4.a  \
             atmos/fim/FIMsrc_devccs/lib/libcntl.a     \
             atmos/fim/FIMsrc_devccs/lib/libsysshare.a

LIBS_FIM = atmos/libatmos.a                     \
           atmos/nmm/libnmm.a                   \
           $(gfsdir)/libgfs.a                   \
           atmos/fim/libfimtop.a                \
           $(SMSLIB) $(FIMSRCLIBS)              \
           atmos/gen/libgen.a                   \
           atmos/share/libshare.a               \
           $(DEP_LINK_OBJS)                     \
           ENS_Cpl/ENS_Cpl.a                    \
           -L/nwprod/lib -lnetcdf_64

MAKEFILE = makefile

#
# GOCART specific compilation variables/flags
#
GOCART_MODE=stub
ESMADIR=chem/gocart
include $(ESMADIR)/src/Config/NCEP_base.mk

ifeq ($(GOCART_MODE),stub)
	LIBGOCART=
else
	LIBGOCART=$(LIB_GOCART)
endif
export GOCART_MODE


#
# log info
#
WHOAMI  := $(shell whoami)
CMP_DATE := $(shell date)
SVN_REV := $(shell svnversion -n .)
SVN_URL := $(shell svn info | grep URL | cut -f 2 -d ' ')
CMP_YEAR := $(shell date +"%Y" )
CMP_JD := $(shell date +"%j" )

CPPFLAGS += -D'SVN_INFO="($(WHOAMI)) $(CMP_DATE) r$(SVN_REV) $(SVN_URL)"'

ifdef CMP_YEAR
  CPPFLAGS += -D'CMP_YEAR=$(CMP_YEAR)'
endif
ifdef CMP_JD
  CPPFLAGS += -D'CMP_JD=$(CMP_JD)'
endif

################################################################################
#
# Ask user which target he/she wants to build
#
################################################################################
all:
	@echo
	@echo " gmake nmm_gsm ----------------------------- NMM and GSM - without the GOCART"
	@echo " gmake gen --------------------------------- GEN "
	@echo " gmake gsm_gen_post GOCART_MODE=full ------- GEN and GSM - with the GOCART and post"
	@echo " gmake nmm_gsm GOCART_MODE=full ------------ NMM and GSM - with the GOCART"
	@echo " gmake nmm --------------------------------- NMM "
	@echo " gmake nmm_post ---------------------------- NMM - with post"
	@echo " gmake gsm --------------------------------- GSM - without the GOCART"
	@echo " gmake gsm_post ---------------------------- GSM - with post without the GOCART"
	@echo " gmake gsm GOCART_MODE=full ---------------- GSM - with the GOCART"
	@echo " gmake fim --------------------------------- FIM "
#	@echo " gmake fim_gsm ----------------------------- FIM and GSM - without the GOCART"
#	@echo " gmake fim_gsm GOCART_MODE=full ------------ FIM and GSM - with the GOCART"
	@echo
	@echo "In order to build NEMS with external atmosphere components, all of the above targets support"
	@echo "the optional ATM variable. Currently supported values are: \"satm\", \"xatm\"."
	@echo
	@echo "In order to build NEMS with ocean components, all of the above targets support"
	@echo "the optional OCN variable. Currently supported values are: \"socn\", \"xocn\", \"mom5\", \"hycom\", \"pom\"."
	@echo
	@echo "In order to build NEMS with sea-ice components, all of the above targets support"
	@echo "the optional ICE variable. Currently supported values are: \"sice\", \"xice\", \"cice\"."
	@echo
	@echo "In order to build NEMS with wave components, all of the above targets support"
	@echo "the optional WAV variable. Currently supported values are: \"swav\", \"xwav\", \"ww3\"."
	@echo
	@echo "In order to build NEMS with land components, all of the above targets support"
	@echo "the optional LND variable. Currently supported values are: \"slnd\", \"xlnd\", \"lis\"."
	@echo
	@echo "In order to build NEMS with ionosphere-plasmasphere components, all of the above targets support"
	@echo "the optional IPM variable. Currently supported values are: \"sipm\", \"xipm\", \"ipe\"."
	@echo
	@echo "In order to build NEMS with hydrology components, all of the above targets support"
	@echo "the optional HYD variable. Currently supported values are: \"shyd\", \"xhyd\", \"wrfhydro\"."
	@echo
	@echo "Specifying multiple values is supported!"
	@echo "E.g. \"gmake nmm_gsm ATM=satm,xatm OCN=socn,xocn,mom5,hycom ICE=sice,xice,cice WAV=swav,xwav LND=slnd,xlnd IPM=sipm,xipm HYD=shyd,xhyd\"."
	@echo

################################################################################
#
# NMM
#
################################################################################

nmm: nmm_libs $(OBJS) $(COBJS) $(MAIN)
	$(FC) $(FFLAGS_NMM) $(SMP) -o $(TARGET) $(MAIN) $(OBJS) $(COBJS) $(LIBS_NMM) $(EXTLIBS) 
	@echo "$(TARGET) is created for NMM core."

nmm_libs:
	cd atmos/share         && gmake $(J)
	cd atmos/phys          && gmake $(J) gocart_stub
	cd atmos/nmm           && gmake $(J)
	cd $(gfsdir)           && gmake stub
	cd atmos/gen           && gmake stub
	cd atmos/fim           && gmake stub
	cd atmos/post          && gmake stub
	cd atmos               && gmake $(J)
	cd ENS_Cpl             && gmake stub

################################################################################
#
# NMM with post
#
################################################################################

nmm_post: nmm_post_libs $(OBJS) $(COBJS) $(MAIN)
	$(FC) $(FFLAGS_NMM) $(SMP) -o $(TARGET) $(MAIN) $(OBJS) $(COBJS) $(LIBS_NMM) $(EXTLIBS_POST) 
	@echo "$(TARGET) is created for NMM core."

nmm_post_libs:
	cd atmos/share         && gmake $(J)
	cd atmos/phys          && gmake $(J) gocart_stub
	cd atmos/nmm           && gmake $(J)
	cd $(gfsdir)           && gmake stub
	cd atmos/gen           && gmake stub
	cd atmos/fim           && gmake stub
	cd atmos/post          && gmake $(J) gfs_stub
	cd atmos               && gmake $(J)
	cd ENS_Cpl             && gmake stub

################################################################################
#
# GSM
#
################################################################################

gsm: gsm_libs $(OBJS) $(COBJS) $(MAIN)
	$(FC) $(FFLAGS_GFS) $(SMP) -o $(TARGET) $(MAIN) $(OBJS) $(COBJS) $(LIBS) $(LIBGOCART) $(gfsdir)/phys/gfs_physics.a $(gfsdir)/dyn/gfs_dynamics.a $(EXTLIBS)
	@echo "NEMS.x successfully created for GSM core."

gsm_libs:
ifeq ($(GOCART_MODE),full)
	cd chem/gocart/src     && gmake install GOCART_MODE=$(GOCART_MODE)
endif
	cd atmos/share         && gmake
	cd atmos/phys          && gmake gocart_$(GOCART_MODE)
	cd atmos/nmm           && gmake stub
	cd atmos/gen           && gmake stub
	cd atmos/io            && gmake
	cd $(gfsdir)           && gmake gocart_$(GOCART_MODE)
	cd atmos/fim           && gmake stub
	cd atmos/post          && gmake stub
	cd atmos               && gmake
	cd ENS_Cpl             && gmake

################################################################################
#
# GSM with post
#
################################################################################

gsm_post: gsm_post_libs $(OBJS) $(COBJS) $(MAIN)
	$(FC) $(FFLAGS_GFS) $(SMP) -o $(TARGET) $(MAIN) $(OBJS) $(COBJS) $(LIBS) $(LIBGOCART) $(gfsdir)/phys/gfs_physics.a $(gfsdir)/dyn/gfs_dynamics.a $(EXTLIBS_POST)
	@echo "NEMS.x successfully created for GSM core."

gsm_post_libs:
ifeq ($(GOCART_MODE),full)
	cd chem/gocart/src     && gmake install GOCART_MODE=$(GOCART_MODE)
endif
	cd atmos/share         && gmake
	cd atmos/phys          && gmake gocart_$(GOCART_MODE)
	cd atmos/nmm           && gmake stub
	cd atmos/gen           && gmake stub
	cd atmos/io            && gmake
	cd $(gfsdir)           && gmake gocart_$(GOCART_MODE)
	cd atmos/fim           && gmake stub
	cd atmos/post          && gmake nmm_stub
	cd atmos               && gmake
	cd ENS_Cpl             && gmake

################################################################################
#
# GEN
#
################################################################################

gen: gen_libs $(OBJS) $(COBJS) $(MAIN)
	$(FC) $(FFLAGS_GFS) $(SMP) -o $(TARGET) $(MAIN) $(COBJS) $(OBJS) $(LIBS_GEN) $(EXTLIBS)
	@echo "NEMS.x successfully created for GEN core."

gen_libs:
	cd atmos/share         && gmake
	cd atmos/phys          && gmake
	cd atmos/nmm           && gmake stub
	cd $(gfsdir)     && gmake stub
	cd atmos/fim           && gmake stub
	cd atmos/gen           && gmake
	cd atmos/post          && gmake stub
	cd atmos               && gmake
	cd ENS_Cpl             && gmake


################################################################################
#
# GEN, NMM and GSM
#
################################################################################

nmm_gsm_gen: nmm_gsm_gen_libs $(OBJS) $(COBJS) $(MAIN)
	$(FC) $(FFLAGS_GFS) $(SMP) -o $(TARGET) $(MAIN) $(OBJS) $(COBJS) $(LIBS) $(LIBGOCART) $(gfsdir)/phys/gfs_physics.a $(gfsdir)/dyn/gfs_dynamics.a $(EXTLIBS)
	@echo "NEMS.x successfully created for GEN, NMM and GSM core."

nmm_gsm_gen_libs:
ifeq ($(GOCART_MODE),full)
	cd chem/gocart/src     && gmake install GOCART_MODE=$(GOCART_MODE)
endif
	cd atmos/share         && gmake
	cd atmos/phys          && gmake gocart_$(GOCART_MODE)
	cd atmos/nmm           && gmake
	cd atmos/io            && gmake
	cd $(gfsdir)           && gmake gocart_$(GOCART_MODE)
	cd atmos/gen           && gmake 
	cd atmos/fim           && gmake stub
	cd atmos/post          && gmake stub
	cd atmos               && gmake
	cd ENS_Cpl             && gmake

################################################################################
#
# GEN and GSM with post
#
################################################################################

gsm_gen_post: gsm_gen_post_libs $(OBJS) $(COBJS) $(MAIN)
	$(FC) $(FFLAGS_GFS) $(SMP) -o $(TARGET) $(MAIN) $(OBJS) $(COBJS) $(LIBS) $(LIBGOCART) $(gfsdir)/phys/gfs_physics.a $(gfsdir)/dyn/gfs_dynamics.a $(EXTLIBS_POST)
	@echo "NEMS.x successfully created for GEN and GSM core."

gsm_gen_post_libs:
ifeq ($(GOCART_MODE),full)
	cd chem/gocart/src     && gmake install GOCART_MODE=$(GOCART_MODE)
endif
	cd atmos/share         && gmake
	cd atmos/phys          && gmake gocart_$(GOCART_MODE)
	cd atmos/nmm           && gmake stub
	cd atmos/io            && gmake
	cd $(gfsdir)           && gmake gocart_$(GOCART_MODE)
	cd atmos/gen           && gmake
	cd atmos/fim           && gmake stub
	cd atmos/post          && gmake nmm_stub
	cd atmos               && gmake
	cd ENS_Cpl             && gmake

################################################################################
#
# NMM and GSM
#
################################################################################

nmm_gsm: nmm_gsm_libs $(OBJS) $(COBJS) $(MAIN)
	$(FC) $(FFLAGS_GFS) $(SMP) -o $(TARGET) $(MAIN) $(COBJS) $(OBJS) $(LIBS) $(LIBGOCART) $(gfsdir)/phys/gfs_physics.a $(gfsdir)/dyn/gfs_dynamics.a $(EXTLIBS)
	@echo "NEMS.x successfully created for NMM and GSM core."

nmm_gsm_libs:
ifeq ($(GOCART_MODE),full)
	cd chem/gocart/src     && gmake install GOCART_MODE=$(GOCART_MODE)
endif
	cd atmos/share         && gmake
	cd atmos/phys          && gmake gocart_$(GOCART_MODE)
	cd atmos/nmm           && gmake
	cd atmos/io            && gmake
	cd $(gfsdir)           && gmake gocart_$(GOCART_MODE)
	cd atmos/fim           && gmake stub
	cd atmos/gen           && gmake stub
	cd atmos/post          && gmake stub
	cd atmos               && gmake
	cd ENS_Cpl             && gmake

################################################################################
#
# FIM
#
################################################################################

# TODO:  Change to re-use NEMS standard $(EXTLIBS) instead of FIM versions 
# TODO:  currently in $(LIBS_FIM) .  
fim: fim_libs $(OBJS) $(MAIN)
	$(FC) $(FFLAGS_GFS) $(SMP) -o $(TARGET) $(MAIN) $(OBJS) $(LIBS_FIM) $(ESMF_LIB)
	@echo "NEMS.x successfully created for FIM core."

fim_libs:
	cd atmos/share         && gmake
	cd atmos/nmm           && gmake stub
	cd $(gfsdir)     && gmake stub 
	cd atmos/fim           && gmake
	cd atmos/gen           && gmake stub
	cd atmos/post          && gmake stub
	cd atmos               && gmake
	cd ENS_Cpl             && gmake

################################################################################
#
# FIM and GSM
#TBH:  not yet integrated with FIM build
#
################################################################################

fim_gsm: fim_gsm_libs $(OBJS) $(MAIN)
#	$(FC) $(FFLAGS_GFS) $(SMP) -o $(TARGET) $(MAIN) $(OBJS) $(LIBS) $(EXTLIBS)
#	@echo "NEMS.x successfully created for FIM and GSM core."
	@echo "ERROR:  Cannot build FIM with other cores yet.  nems.x is NOT created for FIM and GSM core."
	@exit -1

fim_gsm_libs:
	cd atmos/share         && gmake
	cd atmos/nmm           && gmake stub
	cd atmos/io            && gmake
	cd $(gfsdir)           && gmake
	cd atmos/fim           && gmake
	cd atmos/gen           && gmake stub
	cd atmos/post          && gmake stub
	cd atmos               && gmake
	cd ENS_Cpl             && gmake

################################################################################
#
# Clean everything
#
################################################################################

clean:
	cd atmos               && gmake clean
	cd ENS_Cpl             && gmake clean
	cd chem                && gmake clean GOCART_MODE=full
	$(RM) -f *.f90 *.lst *.o *.mod lm map $(TARGET)

.SUFFIXES:
.SUFFIXES: .F90 .f90 .o

.F90.f90:
	$(CPP) $(CPPFLAGS) $< > $*.f90

$(COBJS) : %.o: %.c 
	$(CC) -c $*.c

$(OBJS): %.o: %.f90
	$(FC) $(FFLAGS) $(INCS) -c $*.f90
	$(RM) $*.f90

MAIN_NEMS.o: %.o: %.f90
	$(FC) $(FFLAGS) $(INCS) -c $*.f90
	$(RM) $*.f90

