#!/bin/sh
FC =   ncepxlf
SRCS =  ensmeangrib.f 
OBJS =  ensmeangrib.o 
LIBS = -L/nwprod/lib -lw3_4 -lbacio_4 
LDFLAGS =
CMD  = hiresw_ensmeangrib

$(CMD):   $(OBJS)
	$(FC) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)

