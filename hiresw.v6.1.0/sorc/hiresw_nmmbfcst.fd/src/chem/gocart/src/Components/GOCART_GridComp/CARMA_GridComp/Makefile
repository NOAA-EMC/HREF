# Yippee!  This is my very own makefile.
# Jamison A. Smith, AKA JAS
# April 26, 2007

FORTRAN=	ifort
#FORTRAN=	pathf90
FORTRAN=	gfortran
#FORTRAN=	g95

PACKAGE =	CARMA
TGZ =		CARMA.tgz

#CPPFLAGS=	-DSINGLE        # for single precision
#CPPFLAGS += -DDEBUG         # for debugging statements
#CPPFLAGS=	-DCOAGTEST -DSINGLE
#CPPFLAGS=	-DFALLTEST -DSINGLE
#CPPFLAGS=	-DSSFALLTEST -DSINGLE
#CPPFLAGS=	-DCLDICE 
CPPFLAGS=	-DBCOC -DSINGLE

default:
	$(FORTRAN) $(CPPFLAGS) -c carma_precision_mod.F90
	$(FORTRAN) $(CPPFLAGS) -c carma_constants_mod.F90
	$(FORTRAN) $(CPPFLAGS) -c carma_types_mod.F90
	$(FORTRAN) $(CPPFLAGS) -c carma_main_mod.F90
	$(FORTRAN) $(CPPFLAGS) -c carma_bins.F90
	$(FORTRAN) $(CPPFLAGS) -c SLOD_ChemMod.F90
	$(FORTRAN) $(CPPFLAGS) -c SLOD.F90
	$(FORTRAN) $(CPPFLAGS) -c SLOD_Coupler.F90
	$(FORTRAN) $(CPPFLAGS) -c prtsep.F90
	$(FORTRAN) $(CPPFLAGS) -c setuperr.F90
	$(FORTRAN) $(CPPFLAGS) -c setupaer.F90
	$(FORTRAN) $(CPPFLAGS) -c setupbins.F90
	$(FORTRAN) $(CPPFLAGS) -c setupvf.F90
	$(FORTRAN) $(CPPFLAGS) -c initnew.F90
	$(FORTRAN) $(CPPFLAGS) -c initaer.F90
	$(FORTRAN) $(CPPFLAGS) -c hydrostat.F90
	$(FORTRAN) $(CPPFLAGS) -c vaporp.F90
	$(FORTRAN) $(CPPFLAGS) -c supersat.F90
	$(FORTRAN) $(CPPFLAGS) -c initgas.F90
	$(FORTRAN) $(CPPFLAGS) -c initatm.F90
	$(FORTRAN) $(CPPFLAGS) -c init.F90
	$(FORTRAN) $(CPPFLAGS) -c prestep.F90
	$(FORTRAN) $(CPPFLAGS) -c smallconc.F90
	$(FORTRAN) $(CPPFLAGS) -c step.F90
	$(FORTRAN) $(CPPFLAGS) -c newstate.F90
	$(FORTRAN) $(CPPFLAGS) -c vertical.F90
	$(FORTRAN) $(CPPFLAGS) -c vertadv.F90
	$(FORTRAN) $(CPPFLAGS) -c vertdif.F90
	$(FORTRAN) $(CPPFLAGS) -c versol.F90
	$(FORTRAN) $(CPPFLAGS) -c setupckern.F90
	$(FORTRAN) $(CPPFLAGS) -c setupcoag.F90
	$(FORTRAN) $(CPPFLAGS) -c microslow.F90
	$(FORTRAN) $(CPPFLAGS) -c coagl.F90
	$(FORTRAN) $(CPPFLAGS) -c coagp.F90
	$(FORTRAN) $(CPPFLAGS) -c csolve.F90
	$(FORTRAN) $(CPPFLAGS) -o SLOD.exe *.o

clean:
	/bin/rm -f *.exe *.o *.mod
	/bin/rm -f *.out carma.p
	/bin/rm -f falltest.txt coagtest.txt bcoctest.txt cldicetest.txt

tar:
	/bin/rm -rf $(PACKAGE)
	mkdir $(PACKAGE)
	cp Makefile $(PACKAGE)
	cp CHANGES $(PACKAGE)
	cp -r CVS $(PACKAGE)
	cp *.h $(PACKAGE)
	cp *.F90 $(PACKAGE)
	cp *.pro $(PACKAGE)
	cp cvsnotes.txt $(PACKAGE)
	tar czvf $(TGZ) $(PACKAGE)
	/bin/rm -rf $(PACKAGE)
