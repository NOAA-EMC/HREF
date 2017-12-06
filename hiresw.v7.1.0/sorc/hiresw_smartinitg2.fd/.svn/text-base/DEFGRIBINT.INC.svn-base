!=======================================================================
!  Set interface for routines used in several subprograms
!=======================================================================
INTERFACE
   SUBROUTINE GRIBIT(ID,RITEHD,GRID,GDIN,LUNOUT,DECI)
      use grddef
      PARAMETER (MXBIT=16,LENPDS=28,LENGDS=32)
      PARAMETER (SMALL=1.E-6,SPVAL=9.9E10)
      TYPE (GINFO),INTENT(IN) :: GDIN
      LOGICAL RITEHD
      CHARACTER*1  CDOT,CCOL
      CHARACTER*4  RESTHR
      CHARACTER*6  ,PROJ,DATSET
      CHARACTER*10  DESCR2,DESCR3
      CHARACTER*28 PDS
      CHARACTER*50 ENVAR
      CHARACTER*80 FNAME
      INTEGER IBDSFL(9),IGDS(18),ID(25)
      INTEGER ICENT,IYY,IMM,IDD,IHRST,DATE,OGRD
      CHARACTER*1, ALLOCATABLE    :: KBUF(:)
      REAL,        ALLOCATABLE    :: HOLDGRID(:,:)
      REAL,        INTENT(INOUT)  :: GRID(:,:)
      INTEGER,     ALLOCATABLE    :: IBMAP(:,:),IGRD(:,:)
      LOGICAL  NEWFILE
      INTEGER IH(5),LUNOUT
   END SUBROUTINE gribit
END INTERFACE
