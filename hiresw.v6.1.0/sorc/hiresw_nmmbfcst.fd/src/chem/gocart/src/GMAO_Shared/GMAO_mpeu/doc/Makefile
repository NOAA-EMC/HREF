
RM              = /bin/rm -f
LATEX           = latex
DVIPS           = dvips

ALLINC		= AppendixPrologue.tex			\
		  BookeepingMemoryAllocation.tex	\
		  ErrorHandling.tex			\
		  FileInputOuput.tex			\
		  Miscellaneous.tex			\
		  MpiModuleInterface.tex		\
		  PackageOverview.tex			\
		  References.tex			\
		  ResourceManagement.tex		\
		  RevisionHistory.tex			\
		  SortingTool.tex			\
		  StringManipulation.tex		\
		  Timing.tex


mpeu.dvi:	mpeu.tex $(ALLINC)
	$(LATEX) mpeu.tex
	$(LATEX) mpeu.tex

mpeu.ps:	mpeu.dvi
	$(DVIPS) -o $@ mpeu.dvi

doc:	mpeu.ps

clean:
	$(RM) *.aux *.dvi *.log *.toc
