
include       $(ESMFMKFILE)
ESMF_INC    = $(ESMF_F90COMPILEPATHS)
ESMF_LIB    = $(ESMF_F90LINKPATHS) $(ESMF_F90LINKRPATHS) $(ESMF_F90ESMFLINKLIBS)

gfs=gsm
gfsdir=atmos/$(gfs)

NWPROD_LIB ?= $(TOP)/../../lib

NETCDF_INC  = -I$(NWPROD_LIB)/netcdf/include
NETCDF_LIB  = -L$(NWPROD_LIB)/netcdf/lib -lnetcdff -lnetcdf

NEMSIO_INC  = -I$(NWPROD_LIB)/incmod/nemsio
NEMSIO_LIB  = -L$(NWPROD_LIB) -lnemsio

W3_LIB      = -L$(NWPROD_LIB) -lw3nco_4 -lw3emc_4
BACIO_LIB   = -L$(NWPROD_LIB) -lbacio_4
SP_LIB      = -L$(NWPROD_LIB) -lsp_4
SYS_LIB     =

EXTLIBS     = $(NEMSIO_LIB) \
              $(W3_LIB) \
              $(BACIO_LIB) \
              $(SP_LIB) \
              $(ESMF_LIB) \
              $(NETCDF_LIB) \
              $(SYS_LIB)

FC          = mpif90
FPP         = -cpp
FREE        = -ffree-form
FIXED       = -ffixed-form
R8          = -fdefault-real-8 -fdefault-double-8

FINCS       = $(ESMF_INC) $(NEMSIO_INC) $(NETCDF_INC)
TRAPS       =

FFLAGS      = $(TRAPS) $(FINCS) -fopenmp -fconvert=big-endian -fno-range-check -ffree-line-length-256

OPTS_NMM    = -O3
OPTS_GFS    = -O3
OPTS_GEN    = -O3
OPTS_FIM    = -O3

FFLAGS_NMM  = $(OPTS_NMM) $(FFLAGS)
FFLAGS_GFS  = $(OPTS_GFS) $(FFLAGS) $(FREE)
FFLAGS_GFSF = $(OPTS_GFS) $(FFLAGS) $(FIXED)
FFLAGS_GEN  = $(OPTS_GEN) $(FFLAGS)
FFLAGS_FIM  = $(OPTS_FIM) $(FFLAGS)

CPP         = /lib/cpp -P -traditional
CPPFLAGS    = -DCHNK_RRTM=8

AR          = ar
ARFLAGS     = -r

RM          = rm
