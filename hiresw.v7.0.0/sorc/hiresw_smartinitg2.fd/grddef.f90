MODULE grddef
!=======================================================================
!  Create TYPE to store grid and run definitions
!=======================================================================
   TYPE GINFO
     INTEGER IMAX,JMAX,KMAX,FHR,CYC,DATE,HOUR,ITOT,OGRD
     LOGICAL LCYCON,LHR12,LNEST
     CHARACTER*4 REGION
   END TYPE ginfo
END MODULE grddef


