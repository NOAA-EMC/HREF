   SUBROUTINE GETGRIB(ISNOW,IZR,IIP,IRAIN,VEG,WETFRZ,  &
   P03M,P06M,P12M,SN03,SN06,S3REF01,S3REF10,S3REF50,S6REF01,  &
   S6REF10,S6REF50,S12REF01,S12REF10,S12REF50, THOLD,DHOLD,GDIN,&
   VALIDPT,HAVESREF,GFLD_S,GFLD8_S)

    use grddef
    use aset3d
    use aset2d
    use rdgrib
    USE GRIB_MOD
    USE pdstemplates


!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .
! SUBPROGRAM:    GETGRIB    CREATES NDFD FILES 
!   PRGRMMR: MANIKIN           ORG: W/NP22     DATE: 06-09-14
!
! ABSTRACT:
!   READS GRIB FILE for smartinit downscaling

!   Precip read are conditioned on whether on or off cycle run
!   ON-CYCLE GRIB FILES have 6,12 hour precip buckets
!   OFF-CYCLE GRIB FILES have 3 hr precip buckets

! PROGRAM HISTORY LOG:
!   06-09-14  G MANIKIN  - ADAPT CODE TO NAM 
!   12-10-01  J.MCQUEEN  - Reduced code thru use of rdhdrs,setvar
!                          subroutines
!   12-10-01             - Combined on and off-cycle reads into getgrib
!   14-10-27  M.PYLE     - Changes to process GRIB2

! USAGE:    CALL SMARTINIT 
!   INPUT ARGUMENT LIST:

!   OUTPUT ARGUMENT LIST:
!     NONE

!   OUTPUT FILES:
!     NONE

        IMPLICIT NONE

      TYPE (GINFO) :: GDIN
      INTEGER JPDS(200),JGDS(200),KPDS(200),KGDS(200)
      INTEGER YEAR,MON,DAY,IHR,DATE,IFHR,HAVESREF
      INTEGER:: NUMVAL, IMAX, JMAX, KMAX, NUMLEV

      INTEGER :: LUB,LUI,J,JDISC,JPDTN,JGDTN
      INTEGER,DIMENSION(:) :: JIDS(200),JPDT(200),JGDT(200)
      LOGICAL :: UNPACK
      INTEGER :: K,IRET, IGDNUM, IGDNUM2, IGDNUM3, IGDNUMSN3, NUMVAL2
      INTEGER :: NUMVAL3, NUMVALSN3, IGDNUM5
      INTEGER :: LUG6PI, IGDNUM6, NUMVAL6, IGDNUMSN6, NUMVALSN6
      INTEGER :: IGDNUM12, NUMVAL12, IGDNUMT, NUMVALT, LUGTI, IT, KRET
      INTEGER :: ITOT, KK, NUMVP, NUMVS, IFHR4, IFHR12, KT, LUGTA, LUGTB
      INTEGER :: IIH, LL, M, N, I, ISTAT, KF, ISSREF
      TYPE(GRIBFIELD):: GFLD, GFLD8, GFLD_S, GFLD8_S


      INTEGER, PARAMETER :: MBUF=2000000
      CHARACTER CBUF(MBUF)
      CHARACTER*80 FNAME
      CHARACTER*4 DUM1
      LOGICAL*1 LCYCON,LHR3,LHR6,LHR12,LFULL,LANL,LLIMITED
      LOGICAL LNEST   ! for nests
      INTEGER JENS(200),KENS(200),CYC

!   REAL,        ALLOCATABLE   :: GRID(:)
!   LOGICAL*1,   ALLOCATABLE   :: MASK(:)
!-----------------------------------------------------------------------------------------

!  TYPE(ISET), INTENT(INOUT) :: iprcp
   INTEGER, INTENT(INOUT) :: ISNOW(:,:),IZR(:,:),IIP(:,:),IRAIN(:,:)
        REAL, allocatable,dimension(:,:) :: RTYPE
    

!  TYPE PCPSET, INTENT(INOUT) :: prcp
   REAL,    INTENT(INOUT) :: P03M(:,:),P06M(:,:),P12M(:,:),SN03(:,:),SN06(:,:)
   REAL,    INTENT(INOUT) :: WETFRZ(:,:)
   REAL,    INTENT(INOUT) :: THOLD(:,:,:),DHOLD(:,:,:)

   REAL,    INTENT(INOUT) :: VEG(:,:)

!  TYPE POPSET, INTENT(INOUT) :: pop
   REAL,    INTENT(INOUT) :: S3REF01(:,:),S3REF10(:,:),S3REF50(:,:)
   REAL,    INTENT(INOUT) :: S6REF01(:,:),S6REF10(:,:),S6REF50(:,:)
   REAL,    INTENT(INOUT) :: S12REF01(:,:),S12REF10(:,:),S12REF50(:,:)

   LOGICAL, INTENT(INOUT) :: VALIDPT(:,:) 

   INTEGER :: IHROFF, FHR, LUGB, LUGI, LUGB2, LUGI2, LUGP3, LUGP3I
   INTEGER :: LUGS3, LUGS3I, LUGP6, LUGP6I, LUGS6, LUGS6I
   INTEGER :: LUGP12, LUGP12i, LUGT, LUGT1, LUGT2, LUGT3, LUGT4, LUGT5
   INTEGER :: LUGT1I, LUGT2I, LUGT3I, LUGT4I, LUGT5I

   LOGICAL :: LHR9

!-----------------------------------------------------------------------------------------

!    09-2012 JTM : Modified I/O for WCOSS fort. file name nomenclature
!                  ENVVAR not needed
!                  Introduced RDHDRS and SETVAR routines to eliminate redundancies
!    10-2012 JTM : Added dynamic allocation for arrays
!    11-2012 JTM : Added options for hi, ak, pr domains
!    12-2012 JTM : Added options for conusnest (validpt sets)

      IHROFF=0;LHR12=.FALSE.; LHR6=.FALSE.; LHR3=.FALSE.
      LFULL=.FALSE.;LANL=.FALSE.;LLIMITED=.FALSE.;LCYCON=.FALSE.

      FHR=GDIN%FHR;IFHR=FHR;CYC=GDIN%CYC;LNEST=GDIN%LNEST
      LHR9=.false.

      IF (IFHR.EQ.0) THEN
        LANL=.TRUE.
      ELSE
        IF (MOD(IFHR,3).EQ.0) THEN 
          LFULL=.TRUE.
        writE(0,*) 'set LFULL to true for IFHR: ', IFHR
        ELSE
          LLIMITED=.TRUE.
        ENDIF
      ENDIF


        IGDNUMT=0

        write(0,*) 'inside GETGRIB'
        write(0,*) 'LNEST: ', LNEST
       
!    SET LOGICALS FOR when to read precip or max/min from special files

!                    NON-Nest(on)      NON-Nest(off)     NESTS
!----------------------------------------------------------------
!    F3,15, 27            X              3 hr prcp        X
!    other mod(3h)    3 hr prcp          3 hr prcp        X
!    mod(6 fhrs)         X              3,6 hr prcp      6 hr prcp
!    mod(12 fhrs)     3,6 hr prcp        3,6 hr prcp      6,12 hr prcp
!    MAX/MIN 12 hrs   prev 11 hrs        prev 11 hrs
!----------------------------------------------------------------

      IF (CYC.EQ.12.OR.CYC.EQ.00) LCYCON=.TRUE.

!     Set full, sref and special precip file unit numbers
      LUGB=11; LUGI=12   !DEFAULT MODEL FULL GRIB FILE UNITS
      LUGB2=13; LUGI2=14 !DEFAULT SREF POP GRIB FILE UNITS
      if (.not.lanl ) then
       LUGP3=11;  LUGP3i=12
       LUGS3=11;  LUGS3i=12
       LUGP6=11;  LUGP6i=12
       LUGS6=11;  LUGS6i=12
       LUGP12=11; LUGP12i=12

       IF(MOD(IFHR,3).EQ.0) LHR3=.TRUE.  
       IF(MOD(IFHR,6).EQ.0) LHR6=.TRUE.
       IF(LCYCON) THEN
         IF(MOD(IFHR,12).EQ.9)  LHR9=.TRUE.
         IF(MOD(IFHR,12).EQ.0) LHR12=.TRUE.
       ELSE
         IF(IFHR.GT.6 .AND. MOD(IFHR-6,12).EQ.0) LHR12=.TRUE.
       ENDIF
      
!     Set precip unit numbers for nests
       IF (lnest) THEN
         LUGP6=15;LUGP6i=16
         LUGS6=17;LUGS6i=18
         LUGP12=19;LUGP12i=20
         LHR9=.FALSE.   ! nests have 3 hour precip in std parent grid (01-28-13, JTM)
       else
         IF(LCYCON) THEN 
           IF(MOD(IFHR,12).NE.3) THEN  !all 3 hrs except 3,15,27,39,51,63,75
             LUGP3=15; LUGP3i=16  
             LUGS3=17; LUGS3i=18   
           ENDIF
           IF(LHR12) THEN
             LUGP6=17; LUGP6i=18
             LUGS3=19; LUGS3i=20 
             LUGS6=21; LUGS6i=22
           ENDIF
         ELSE
           LHR9=.FALSE.    ! off-cycle have 3 hr precip in std parent grid
           IF (LHR6) THEN    
             LUGP6=15; LUGP6i=16   !SPECIAL PRECIP FILE
             LUGS6=17; LUGS6i=18
           ENDIF
           IF (LHR12) LUGP12=19;LUGP12i=20   
         ENDIF
        endif  !lnest
      endif  !lanl

      print *, 'IFHR',IFHR,'LHR3',LHR3,'LHR6',LHR6,'LHR12',LHR12
      P06M=0.0; P12M=0.0

!     SET MAX/MIN FILE UNIT NUMBERS
!     FOR 12-hr on-cycle TIMES, WE NEED 3 AND 6-HR BUCKETS AND MAX/MIN TEMP
!     DATA FOR THE PREVIOUS 11 HOURS
      IF(LHR12) THEN
       LUGT1=23
       IF (.not.LCYCON .or. lnest) LUGT1=21
       LUGT2=LUGT1+1
       LUGT3=LUGT1+2
       LUGT4=LUGT1+3
       LUGT5=LUGT1+4
       LUGT1I=LUGT1+5
       LUGT2I=LUGT1+6
       LUGT3I=LUGT1+7
       LUGT4I=LUGT1+8
       LUGT5I=LUGT1+9
        print *,'====================================================='
        print *, 'Read  11 hrs of MAX,MIN TEMP', IFHR,lugt1,lugt2,lugt3
        print *, 'Read  3 hr precip from unit',lugp3,lugs3
        print *, 'Read  6 hr precip from unit',lugp6,lugs6
        print *, 'Read  12 hr precip from unit',lugp12
        print *,'====================================================='

      ELSE IF(LHR6.OR.LHR9) THEN
!      However Off-Hour cycle runs do not have 6 hour buckets
         LUGT1=19; LUGT2=20; LUGT1I=21; LUGT2I=22
       print *,'======================================================='
       print *, 'Read previous 2 hrs of  MAX,MIN TEMP', IFHR, lugt1,lugt2
       print *, 'Read  3 hr precip from unit',lugp3,lugs3
       if(lhr6)  print *,'Read 6 hr precip from unit',lugp6,lugs6
       print *,'======================================================='

      ELSE IF(LHR3) THEN
        LUGT1=15;LUGT2=16; LUGT1I=17; LUGT2I=18
       print *,'================================================================='
       print *, 'Read previous 2 hours of MAX,MIN TEMP ', IFHR,lugt1,lugt2
       print *, 'Read 3 hr prcp from std grid ',lugp3,lugs3
       print *,'================================================================='
      ELSE

!      IN-BETWEEN HOURS DON'T NEED ANYTHING FANCY
      print *,'====================================================='
       print *, 'IN-Between HOURS, small change ', IFHR
       print *,'====================================================='
      ENDIF

        write(0,*) 'here a'

      OPEN(49,file='DATE',form='formatted')
      READ(49,200) DUM1,GDIN%DATE
      DATE=GDIN%DATE
      CLOSE(49)
 200  FORMAT(A4,2X,I10)
      year=int(date/1000000)
      mon=int(int(mod(date,1000000)/100)/100)
      day=int(mod(date,10000)/100)
      ihr=mod(date,100)
      print *, 'date ', DATE,YEAR,MON,DAY,IHR 
        write(0,*) 'here b'

!==========================================================
!     READ INDEX FILE TO GET GRID SPECS
!==========================================================
        write(0,*) 'here c - call RDHDRS'
      CALL RDHDRS_g2(LUGB,LUGI,IGDNUM,GDIN,NUMVAL)
      IMAX=GDIN%IMAX;JMAX=GDIN%JMAX;KMAX=GDIN%KMAX
      NUMLEV=GDIN%KMAX
      ITOT=IMAX*JMAX
      print *,gdin%imax,jmax,kmax,numlev,itot
        write(0,*) 'here d - past RDHDRS'
        write(0,*) 'see NUMLEV: ', NUMLEV

      if (lfull) then

        if (HAVESREF .eq. 1) then
      print *, ' READING SREF HDRS',LUGB2,LUGI2
      CALL RDHDRS_g2(LUGB2,LUGI2,IGDNUM2,GDIN,NUMVAL2)
        endif

        write(0,*) 'here e - past RDHDRS'

! GSM  READ 3-HR PRECIP AND SNOW FILES WHICH ARE NEEDED
!      IF NOT A 3-HR ACCUMULATION TIME (F15,F27,F39...) 
!      OR AN "OFF-TIME" (F13,F14,F16....)

        write(0,*) 'READ 3-hr precip HDRS from Unit ', LUGP3,LUGP3I
      print *, 'READ 3-hr precip HDRS from Unit ', LUGP3,LUGP3I
      CALL RDHDRS_g2(LUGP3,LUGP3I,IGDNUM3,GDIN,NUMVAL3)
        write(0,*) 'here f - past RDHDRS'
      print *, 'READ 3-hr snow HDRS from Unit ', LUGS3,LUGS3I
      CALL RDHDRS_g2(LUGS3,LUGS3I,IGDNUMSN3,GDIN,NUMVALSN3)
        write(0,*) 'here g - past RDHDRS'
      IGDNUM5=IGDNUMSN3

!     READ 6-HR PRECIP/SNOW FILES AT F12,F24,F36.....
!     OR 12-hr PRECIP FOR OFF-CYCLE RUNS
      IF (LHR6.OR.LHR9.OR.LHR12) THEN
        print *, 'READING 6 hr precip HDR from Unit ', LUGP6,LUGP6I
        write(0,*) 'here ga - '
        CALL RDHDRS_g2(LUGP6,LUGP6I,IGDNUM6,GDIN,NUMVAL6)
        write(0,*) 'here gb - '
        print *, 'READING 6 hr SNOW HDR from Unit ', LUGS6,LUGS6I
        CALL RDHDRS_g2(LUGS6,LUGS6I,IGDNUMSN6,GDIN,NUMVALSN6)
        write(0,*) 'here gc - '
        IF(LHR12) THEN
          print *, 'READING 12 hr precip HDR from Unit ', LUGP12,LUGP12I
        write(0,*) 'here gd - '
          CALL RDHDRS_g2(LUGP12,LUGP12I,IGDNUM12,GDIN,NUMVAL12)
        write(0,*) 'here ge - '
        ENDIF
      ENDIF
        write(0,*) 'here h '

!==================================================================
! GSM  READ TEMPERATURE HDR FILES FOR 12-HR MIN/MAX
!      READ INDEX FILE TO GET GRID SPECS
!==================================================================
!     GET GRID NUMBER FROM PDS AND PROCESS GRIB FILE
!     NOTE: WE'LL ASSUME THE GRID NUMBER IS THE SAME FOR
!     ALL OF THESE MIN/MAX FILES AND NOT DO THIS FOR EACH

      print *, "Reading min/max Temp HDR from UNIT:",LUGT1, LUGT1I
      CALL RDHDRS_g2(LUGT1,LUGT1I,IGDNUMT,GDIN,NUMVALT)
        write(0,*) 'past RDHDRS_g2(aa) call'

!     Fill the max/min T/Td holders with 0's to
!      1) account for this array at times other than f12,24,36...
!      2) temporarily fill the 12th time slot, since we have only 11 here
      THOLD=0.
      DHOLD=0.

      print *, "Reading min/max Temp HDR from UNIT:",LUGT2, LUGT2I, NUMVALT
      CALL RDHDRS_g2(LUGT2,LUGT2I,IGDNUMT,GDIN,NUMVALT)
        write(0,*) 'past RDHDRS_g2(a) call'

      IF (LHR12) THEN
        LUGT=LUGT3    
        LUGTI=LUGT3I   
        DO  IT=3,5
          print *, "Reading min/max Temp  UNIT:",IT, LUGT, LUGTI
          CALL RDHDRS_g2(LUGT,LUGTI,IGDNUMT,GDIN,NUMVALT)
        write(0,*) 'past RDHDRS_g2(b) call'
          LUGT=LUGT+1
          LUGTI=LUGTI+1
        ENDDO 
      ENDIF
      endif !LFULL

!  get sfc height 
!tst      ALLOCATE (GRID(ITOT),MASK(ITOT),STAT=kret)
!tst      print *,'GRID ALLOCATED',ITOT,' kret',kret

        JIDS=-9999
        JPDTN=-1
        JPDT=-9999
        JGDTN=-1
        JGDT=-9999

       JPDT(2) = 005
       JPDT(10) = 1
       JPDTN   = 0
       JDISC = 0

        write(0,*) 'call SETVAR_g2 for ZSFC'
        write(0,*) 'NUMVAL into SETVAR_g2: ', NUMVAL

! is it SREF data?
        ISSREF=0

        J=0
      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K, &
                     KPDS,KGDS,MASK,GRID,ZSFC,GFLD_S,ISSREF,IRET,ISTAT)

        write(0,*) 'GFLD_S%ibmap: ', GFLD_S%ibmap
        
      WHERE (ZSFC < 0.0) ZSFC=0.0

        write(0,*) 'minval(zsfc),maxval(zsfc): ', minval(zsfc),maxval(zsfc)

! get surface pressure

       JPDT(1) = 003
       JPDT(2) = 000
       JPDT(10) = 1
       JPDTN   = 0

        write(0,*) 'call for pressure'
        J=0
      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,& 
                     KPDS,KGDS,MASK,GRID,PSFC,GFLD,ISSREF,IRET,ISTAT)

        if (IRET .eq. 0) then
        write(0,*) 'minval(psfc),maxval(psfc); ', minval(psfc),maxval(psfc)
        endif


! get 4 INTEGER precip types 
      if (lfull) then

! snow

        JIDS=-9999
        JPDT=-9999
        JGDTN=-1
        JGDT=-9999
        JDISC=0

        JPDTN=0
       JPDT(1) = 001
       JPDT(2) = 195 ! 036
       JPDT(10) = 1
        UNPACK=.true.
        J=0
       
        allocate(RTYPE(IMAX,JMAX))

!      CALL GETGB2(LUGB,LUGI,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDTN,JGDT, &
!                  UNPACK,K,GFLD,IRET)

        J=0
      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,& 
                     KPDS,KGDS,MASK,GRID,RTYPE,GFLD,ISSREF,IRET,ISTAT)

        write(0,*) 'IRET from snow cat: ', IRET

        do J=1,JMAX
        do I=1,IMAX
        ISNOW(I,J)=int(RTYPE(I,J))
        enddo
        enddo


!      IF(IRET.EQ.0) THEN
!        DO KK = 1, ITOT
!          IF(MOD(KK,IMAX).EQ.0) THEN
!            M=IMAX
!            N=INT(KK/IMAX)
!          ELSE
!            M=MOD(KK,IMAX)
!            N=INT(KK/IMAX) + 1
!          ENDIF
!          ISNOW(M,N) = GFLD%FLD(KK)
!        ENDDO
!        WRITE(6,*) 'FOUND SNOW CATEGORY'
!      ELSE
!        ENDDO
!        WRITE(6,*) 'FOUND SNOW CATEGORY'
!      ELSE
!       WRITE(6,*)'COULD NOT UNPACK VARB(a)',JPDT(1:10),IRET
!       ISTAT = IRET
!      ENDIF

! ice pellets
       JPDT(1) = 001
       JPDT(2) = 194 ! 035
       JPDT(10) = 1

        write(0,*) 'to ice pellets'

        J=0
      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,& 
                     KPDS,KGDS,MASK,GRID,RTYPE,GFLD,ISSREF,IRET,ISTAT)

        write(0,*) 'IRET from snow cat: ', IRET

        do J=1,JMAX
        do I=1,IMAX
        IIP(I,J)=int(RTYPE(I,J))
        enddo
        enddo

!      CALL GETGB2(LUGB,LUGI,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDTN,JGDT, &
!                  UNPACK,K,GFLD,IRET)
!      IF(IRET.EQ.0) THEN
!        DO KK = 1, ITOT
!          IF(MOD(KK,IMAX).EQ.0) THEN
!            M=IMAX
!            N=INT(KK/IMAX)
!          ELSE
!            M=MOD(KK,IMAX)
!            N=INT(KK/IMAX) + 1
!          ENDIF
!          IIP(M,N) = GFLD%FLD(KK)
!        ENDDO
!        WRITE(6,*) 'FOUND ICE PELLETS CATEGORY'
!      ELSE
!       WRITE(6,*)'COULD NOT UNPACK VARB(b)',JPDT(1:10),IRET
!       ISTAT = IRET
!      ENDIF
!
! frz rain
       JPDT(1) = 001
       JPDT(2) = 193 ! 034
       JPDT(10) = 1

!      CALL GETGB2(LUGB,LUGI,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDTN,JGDT, &
!                  UNPACK,K,GFLD,IRET)
!
!      IF(IRET.EQ.0) THEN
!        DO KK = 1, ITOT
!          IF(MOD(KK,IMAX).EQ.0) THEN
!            M=IMAX
!            N=INT(KK/IMAX)
!          ELSE
!            M=MOD(KK,IMAX)
!            N=INT(KK/IMAX) + 1
!          ENDIF
!          IZR(M,N) = GFLD%FLD(KK)
!        ENDDO
!        WRITE(6,*) 'FOUND FREEZING RAIN CATEGORY'
!      ELSE
!       WRITE(6,*)'COULD NOT UNPACK VARB(c)',JPDT(1:10),IRET
!       ISTAT = IRET
!      ENDIF

        J=0
      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,& 
                     KPDS,KGDS,MASK,GRID,RTYPE,GFLD,ISSREF,IRET,ISTAT)

        write(0,*) 'IRET from freezing rain cat: ', IRET

        do J=1,JMAX
        do I=1,IMAX
        IZR(I,J)=int(RTYPE(I,J))
        enddo
        enddo

! rain
       JPDT(1) = 001
       JPDT(2) = 192 ! 034
       JPDT(10) = 1

!      CALL GETGB2(LUGB,LUGI,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDTN,JGDT, &
!                  UNPACK,K,GFLD,IRET)
!      IF(IRET.EQ.0) THEN
!        DO KK = 1, ITOT
!          IF(MOD(KK,IMAX).EQ.0) THEN
!            M=IMAX
!            N=INT(KK/IMAX)
!          ELSE
!            M=MOD(KK,IMAX)
!            N=INT(KK/IMAX) + 1
!          ENDIF
!          IRAIN(M,N) = GFLD%FLD(KK)
!        ENDDO
!        WRITE(6,*) 'FOUND RAIN CATEGORY'
!      ELSE
!       WRITE(6,*)'COULD NOT UNPACK VARB(d)',JPDT(1:10),IRET
!       ISTAT = IRET
!      ENDIF

        J=0
      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,& 
                     KPDS,KGDS,MASK,GRID,RTYPE,GFLD,ISSREF,IRET,ISTAT)

        write(0,*) 'IRET from freezing rain cat: ', IRET

        do J=1,JMAX
        do I=1,IMAX
        IRAIN(I,J)=int(RTYPE(I,J))
        enddo
        enddo

      endif !lfull
    
      if (lfull.or.lanl) then
! lowest wet bulb zero level
        write(0,*) 'here before SETVAR for lowest wet bulb zero'


       JPDT(1) = -9999
       JPDT(2) = 005
       JPDT(10) = 245

        J=0
      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,& 
                     KPDS,KGDS,MASK,GRID,WETFRZ,GFLD,ISSREF,IRET,ISTAT)

        write(0,*) 'IRET for lowest wet bulb zero level: ', IRET

! visibility 
       JPDT(1) = 19
       JPDT(2) = 000
       JPDT(10) = 1
        J=0
      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,& 
                     KPDS,KGDS,MASK,GRID,VIS,GFLD,ISSREF,IRET,ISTAT)

        write(0,*) 'IRET for vis: ', IRET

      endif

! 2-m temp

       JPDT(1) = 0
       JPDT(2) = 000
       JPDT(10) = 103
       JPDT(12) = 2
        J=0
      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,& 
                     KPDS,KGDS,MASK,GRID,T2,GFLD,ISSREF,IRET,ISTAT)

        write(0,*) 'IRET for t2m: ', IRET

!        print*, 'T2(251,100): ', T2(251,100)
!        print*, 'T2(253,131): ', T2(253,131)

! 2-m spec hum
       JPDT(1) = 1
       JPDT(2) = 000
       JPDT(10) = 103
       JPDT(12) = 2
        J=0
      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,& 
                     KPDS,KGDS,MASK,GRID,Q2,GFLD,ISSREF,IRET,ISTAT)

        write(0,*) 'IRET for q2m: ', IRET

! 2-m dew point 

       JPDT(1) = 0
       JPDT(2) = 006
       JPDT(10) = 103
       JPDT(12) = 2
        J=0
      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,& 
                     KPDS,KGDS,MASK,GRID,D2,GFLD,ISSREF,IRET,ISTAT)

        write(0,*) 'IRET for td2m: ', IRET

! 10-m U

       JPDT(1) = 2
       JPDT(2) = 002
       JPDT(10) = 103
       JPDT(12) = 10

        J=0
      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,& 
                     KPDS,KGDS,MASK,GRID,U10,GFLD,ISSREF,IRET,ISTAT)

! 10-m V

       JPDT(1) = 2
       JPDT(2) = 003
       JPDT(10) = 103
       JPDT(12) = 10
        J=0

      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,& 
                     KPDS,KGDS,MASK,GRID,V10,GFLD,ISSREF,IRET,ISTAT)

        write(0,*) 'U10(1,1),V10(1,1): ', U10(1,1),V10(1,1)


! vegetation fraction

!! 225 is vegetation type, not vegetation fraction

       JDISC    = 2
       JPDT(1)  = 0
       JPDT(2)  = 198
       JPDT(10) = -9999
       JPDT(12) = -9999

        J=0
      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,& 
                     KPDS,KGDS,MASK,GRID,VEG,GFLD,ISSREF,IRET,ISTAT)

        write(0,*) 'VEG(1,1): ', VEG(1,1)


! Best Liftex Index 
      if (lfull.or.lanl) then

       JDISC=0
        JPDT=-9999
       JPDT(1) = 7
       JPDT(2) = 193
       JPDT(10) = -9999
        J=0
      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,& 
                     KPDS,KGDS,MASK,GRID,BLI,GFLD,ISSREF,IRET,ISTAT)
        write(0,*) 'IRET  for BLI: ', IRET, minval(BLI),MAXVAL(BLI)


      endif

!====================================================================
!      READ PRECIP FROM FULL (LUGB=11) or SPECIAL GRIB FILE (unit 15-21)
!  STANDARD FULL GRIB FILE PRECIP 
!      For on-Cycles it will have either a 3-hr, 6-hr, 9-hr, or 12-hr accumulation
!      For off-Cycles,  3 and 12 hr accumulations are in full grib file
!====================================================================
      if (lfull) then
      JPDS=-1;J=0
      NUMVP=NUMVAL;NUMVS=NUMVAL
      IF (LHR3) THEN

! Read 3-hr Precip 

       JDISC=0
       JPDTN=8
       JPDT(1) = 1
       JPDT(2) = 8
       JPDT(10) = -9999
       JPDT(12) = -9999
        J=0
      CALL SETVAR_g2(LUGP3,LUGP3I,NUMVP,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,& 
                     KPDS,KGDS,MASK,GRID,P03M,GFLD8_S,ISSREF,IRET,ISTAT)

! 3-hr Snow 

       JDISC=0
       JPDTN=8
       JPDT(1) = 1
       JPDT(2) = 13
       JPDT(10) = -9999
        J=0
      CALL SETVAR_g2(LUGS3,LUGS3I,NUMVS,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,& 
                     KPDS,KGDS,MASK,GRID,SN03,GFLD,ISSREF,IRET,ISTAT)

      ENDIF

!   ON-CYC : 6 hr buckets at 12 Fhrs in special file
!   OFF-CYC: All 6 hr buckets are in special file
      IF (LHR6.OR.LHR12) THEN

! Read 6-hr Precip 

       JPDTN=8
       JPDT(1) = 1
       JPDT(2) = 8
       JPDT(10) = -9999
        J=0
      CALL SETVAR_g2(LUGP6,LUGP6I,NUMVP,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,& 
                     KPDS,KGDS,MASK,GRID,P06M,GFLD,ISSREF,IRET,ISTAT)


!     6-hr snow 

       JPDTN=8
       JPDT(1) = 1
       JPDT(2) = 13
       JPDT(10) = -9999
        J=0
      CALL SETVAR_g2(LUGS6,LUGS6I,NUMVS,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,& 
                     KPDS,KGDS,MASK,GRID,SN06,GFLD,ISSREF,IRET,ISTAT)

! Read  12-hr Precip
        IF(LHR12) THEN
          print *, LCYCON,IFHR,'READ 12 hr PRECIP from file ',LUGP12,IGDNUM12

       JPDTN=8
       JPDT(1) = 1
       JPDT(2) = 8
       JPDT(10) = -9999
        J=0
      CALL SETVAR_g2(LUGP12,LUGP12I,NUMVP,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,& 
                     KPDS,KGDS,MASK,GRID,P12M,GFLD,ISSREF,IRET,ISTAT)
        ENDIF
      ENDIF

!  READ min/max temperature values for previous 2 hours
      print *, 'Reading max/min for previous 2 hours',LUGT1,LUGT2,IGDNUMT
       JPDTN=0
       JPDT(1) = 0
       JPDT(2) = 000
       JPDT(10) = 103
       JPDT(12) = 2
        J=0
      CALL SETVAR_g2(LUGT1,LUGT1I,NUMVALT,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,& 
                     KPDS,KGDS,MASK,GRID,THOLD(:,:,2),GFLD,ISSREF,IRET,ISTAT)

!        print*, 'THOLD(251,100,2): ', THOLD(251,100,2)
!        print*, 'THOLD(253,131,2): ', THOLD(253,131,2)

       JPDT(1) = 0
       JPDT(2) = 006
       JPDT(10) = 103
       JPDT(12) = 2
        J=0
      CALL SETVAR_g2(LUGT1,LUGT1I,NUMVALT,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,& 
                     KPDS,KGDS,MASK,GRID,DHOLD(:,:,2),GFLD,ISSREF,IRET,ISTAT)


       JPDT(1) = 0
       JPDT(2) = 000
       JPDT(10) = 103
       JPDT(12) = 2
        J=0
      CALL SETVAR_g2(LUGT2,LUGT2I,NUMVALT,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,& 
                     KPDS,KGDS,MASK,GRID,THOLD(:,:,3),GFLD,ISSREF,IRET,ISTAT)

       JPDT(1) = 0
       JPDT(2) = 006
       JPDT(10) = 103
       JPDT(12) = 2
        J=0
      CALL SETVAR_g2(LUGT2,LUGT2I,NUMVALT,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,& 
                     KPDS,KGDS,MASK,GRID,DHOLD(:,:,3),GFLD,ISSREF,IRET,ISTAT)

! Get min/max temperature values for full 12-hr period for F12,24...
      IF (LHR12) THEN
       IFHR4=IFHR-3
       IFHR12=IFHR-11
       KT=4
       LUGTA=LUGT3
       LUGTB=LUGT3I
       DO IIH=IFHR4,IFHR12,-1
         IF (IIH.LE.IFHR-6) THEN
           LUGTA=LUGT4
           LUGTB=LUGT4I
         ENDIF
         IF (IIH.LE.IFHR-9) THEN
           LUGTA=LUGT5
           LUGTB=LUGT5I
         ENDIF
 
         print *, 'READING TEMP for hr', IIH, LUGTA,LUGTB,KT

       JPDT(1) = 0
       JPDT(2) = 000
       JPDT(10) = 103
       JPDT(12) = 2
        J=0
      CALL SETVAR_g2(LUGTA,LUGTB,NUMVALT,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,& 
                     KPDS,KGDS,MASK,GRID,THOLD(:,:,KT),GFLD,ISSREF,IRET,ISTAT)

         print *, 'READING  DPT for hr', IIH, LUGTA,LUGTB,KT 

       JPDT(1) = 0
       JPDT(2) = 006
       JPDT(10) = 103
       JPDT(12) = 2
        J=0
      CALL SETVAR_g2(LUGTA,LUGTB,NUMVALT,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,& 
                     KPDS,KGDS,MASK,GRID,DHOLD(:,:,KT),GFLD,ISSREF,IRET,ISTAT)

         KT=KT+1

       ENDDO
      ENDIF
      endif !lfull

        write(0,*) 'here 27'

!   get the vertical profile of pressure 
      print *,'READ UPPER LEVEL fields from unit ', LUGB,'KMAX',KMAX

        JDISC=0

!        JIDS=-9999
        JPDTN=0
        JPDT=-9999
        JGDTN=-1
        JGDT=-9999

      DO LL=1,KMAX

       JPDT(1) = 003
       JPDT(2) = 000
       JPDT(10) = 105
       JPDT(12) = LL

        J=0
      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,& 
                     KPDS,KGDS,MASK,GRID,PMID(:,:,LL),GFLD,ISSREF,IRET,ISTAT)

        if (IRET .eq. 0) then
        write(0,*) 'PMID(1,1,LL): ', LL, PMID(1,1,LL)
        endif

        if (IRET .ne. 0) then
        write(0,*) 'IRET from pressure column : ', IRET
              STOP
        endif

!mptest       J=K
      ENDDO

!   get the vertical profile of height 
      J=0
      DO LL=1,KMAX  

       JPDT(1) = 003
       JPDT(2) = 005
       JPDT(10) = 105
       JPDT(12) = LL

        J=0
      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,&
                     KPDS,KGDS,MASK,GRID,HGHT(:,:,LL),GFLD,ISSREF,IRET,ISTAT)
        if (IRET .eq. 0) then
        write(0,*) 'HGHT(1,1,LL): ', HGHT(1,1,LL)
        endif

!mptest       J=K
      ENDDO


!   get the vertical profile of temperature
      J=0
      DO LL=1,KMAX  
!mptest       J=K

       JPDT(1) = 000
       JPDT(2) = 000
       JPDT(10) = 105
       JPDT(12) = LL

        J=0
      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,&
                     KPDS,KGDS,MASK,GRID,T(:,:,LL),GFLD,ISSREF,IRET,ISTAT)
        write(0,*) 'T(1,1,LL): ', T(1,1,LL)

      ENDDO


! note points that are within bitmap
       VALIDPT=.TRUE.
       WHERE(T(:,:,1).LE.10. .or. T(:,:,1) .ge. 9.e20) VALIDPT = .FALSE.

! JTM 01-28-13: Added check for where previous temps are not at validpts
       do i=1,imax
       do j=1,jmax

!        if (I .eq. 251 .and. J .eq. 100) then
!        do LL=1,KMAX
!        print*, 'T(251,100,LL): ', T(251,100,LL)
!        enddo
!        endif

         if(validpt(i,j).and.T(i,j,1).le.10) then 
            print *,' Inconsistent valid pt at :', i,j,' Temperature=',T(i,j,1)
! not needed  validpt(i,j)=.false.
         endif
       enddo
       enddo
       print *,'VALIDPT=',validpt(20,20),'max/min Temp at lvl 1',maxval(T),minval(T)

!   get the vertical profile of q
      J=0
      DO LL=1,KMAX   
       JPDT(1) = 001
       JPDT(2) = 000
       JPDT(10) = 105
       JPDT(12) = LL

      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,&
                     KPDS,KGDS,MASK,GRID,Q(:,:,LL),GFLD,ISSREF,IRET,ISTAT)
        J=K
        write(0,*) 'Q(50,50,LL): ', Q(50,50,LL)


      ENDDO

!   get the vertical profile of u 
      J=0
      DO LL=1,KMAX  
       JPDT(1) = 002
       JPDT(2) = 002
       JPDT(10) = 105
       JPDT(12) = LL

      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,&
                     KPDS,KGDS,MASK,GRID,UWND(:,:,LL),GFLD,ISSREF,IRET,ISTAT)

       J=K
        if (I .eq. 293 .and. J .eq. 132) then
        write(0,*) 'I,J, LL, UWND(I,J,LL): ', I,J, LL, UWND(I,J,LL)
        endif

      ENDDO

!   get the vertical profile of v
      J=0
      DO LL=1,KMAX  
       JPDT(1) = 002
       JPDT(2) = 003
       JPDT(10) = 105
       JPDT(12) = LL

      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,&
                     KPDS,KGDS,MASK,GRID,VWND(:,:,LL),GFLD,ISSREF,IRET,ISTAT)
       J=K
        if (I .eq. 293 .and. J .eq. 132) then
        write(0,*) 'I,J, LL, VWND(I,J,LL): ', I,J, LL, VWND(I,J,LL)
        endif
      ENDDO
        write(0,*) 'is llimited true....will avoid T850, etc: ', llimited
      if (llimited) return

!   get the vertical profile of cloud fraction for non-nests
      if (.not. lnest) then
      J=0
      DO LL=1,KMAX  
        write(0,*) 'vertical column of cloud fraction'
       JPDT(1) = 006
       JPDT(2) = 032
       JPDT(10) = 105
       JPDT(12) = LL

      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,&
                     KPDS,KGDS,MASK,GRID,CFR(:,:,LL),GFLD,ISSREF,IRET,ISTAT)
       J=K



      ENDDO
      endif

!   950 mb temperature

       JPDT=-9999
       JPDT(1) = 000
       JPDT(2) = 000
       JPDT(10) = 100
       JPDT(12) = 95000

       write(0,*) '950 T get'

      J=0
      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,&
                     KPDS,KGDS,MASK,GRID,T950,GFLD,ISSREF,IRET,ISTAT)

!   850 mb temperature

       JPDT(12) = 85000
      J=0
      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,&
                     KPDS,KGDS,MASK,GRID,T850,GFLD,ISSREF,IRET,ISTAT)

!   700 mb temperature

       JPDT(12) = 70000
      J=0
      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,&
                     KPDS,KGDS,MASK,GRID,T700,GFLD,ISSREF,IRET,ISTAT)

!   500 mb temperature

       JPDT(12) = 50000
      J=0
      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,&
                     KPDS,KGDS,MASK,GRID,T500,GFLD,ISSREF,IRET,ISTAT)

!   850 mb RH

       JPDT(1) = 001
       JPDT(2) = 001
       JPDT(10) = 100
       JPDT(12) = 85000

      J=0
      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,&
                     KPDS,KGDS,MASK,GRID,RH850,GFLD,ISSREF,IRET,ISTAT)

        write(0,*) 'min/max of RH850: ', minval(RH850),maxval(RH850)

!   700 mb RH

       JPDT(12) = 70000

      J=0
      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,&
                     KPDS,KGDS,MASK,GRID,RH700,GFLD,ISSREF,IRET,ISTAT)
        write(0,*) 'min/max of RH700: ', minval(RH700),maxval(RH700)

        write(0,*) 'T850, T700, T500: ', T850(1,1), T700(1,1), T500(1,1)
        write(0,*) 'RH850, RH700: ', RH850(1,1), RH700(1,1)

!  sfc wind gust 

       JPDT(1) = 002
       JPDT(2) = 022
       JPDT(10) = -9999
       JPDT(12) = -9999

      J=0
      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,&
                     KPDS,KGDS,MASK,GRID,GUST,GFLD,ISSREF,IRET,ISTAT)


! composite reflectivity
          
       JPDT(1) = 16
       JPDT(2) = 196
       JPDT(10) = -9999
       JPDT(12) = -9999

      J=0
      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,&
                     KPDS,KGDS,MASK,GRID,REFC,GFLD,ISSREF,IRET,ISTAT)

        write(0,*) 'here good'

      if (lanl) return

!     nests already have computed cld fracs...
      if (lnest) then

       JPDT(1) = 6
       JPDT(2) = 01
       JPDT(10) = -9999
       JPDT(12) = -9999
        write(0,*) 'cloud fraction'

      J=0
      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,&
                     KPDS,KGDS,MASK,GRID,TCLD,GFLD,ISSREF,IRET,ISTAT)

        write(0,*) 'IRET from SETVAR_g2 for TCLD ', IRET

        write(0,*) 'maxval(TCLD): ', maxval(TCLD)


!        JPDS(5) = 73
!        JPDS(6) = 214
!        CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,LCLD,GFLD,ISSREF,IRET,ISTAT)
!        JPDS(5) = 74
!        JPDS(6) = 224
!        CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,MCLD,GFLD,ISSREF,IRET,ISTAT)
!        JPDS(5) = 75
!        JPDS(6) = 234
!        CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,HCLD,GFLD,ISSREF,IRET,ISTAT)
       endif 

        if (HAVESREF .eq. 1) then
! is it SREF data?
        ISSREF=1

        write(0,*) 'to read of SREF precip'
!  READ SREF precip
      print*; print *,'READ SREF Precip Probs', LUGB2

! 3-hr probability of .01"

!      J=0     !J= number of records to skip in SREFPCP file
!      JPDS=-1;JGDS=-1
!      JPDS(3) = IGDNUM2
!      JPDS(5) = 191 
!      JPDS(6) = 001
!      CALL SETVAR(LUGB2,LUGI2,NUMVAL2,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,S3REF01,GFLD,ISSREF,IRET,ISTAT)

        JIDS=-9999
        JPDTN=-1
        JPDT=-9999
        JGDTN=-1
        JGDT=-9999

       J=0
       JPDT(2) = 008
       JPDT(10) = 1
       JDISC = 0

        write(0,*) 'LUGB2, LUGI2 for SREF prob: ', LUGB2, LUGI2
      CALL SETVAR_g2(LUGB2,LUGI2,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K, &
                     KPDS,KGDS,MASK,GRID,S3REF01,GFLD,ISSREF,IRET,ISTAT)

        write(0,*) 'IRET from SETVAR_g2: ', IRET

        
        write(0,*) 'min,max S3REF01: ', minval(S3REF01),maxval(S3REF01)
        write(0,*) 'sum(S3REF01): ', sum(S3REF01)

! probability of .1"
!      J = 2
!      JPDS=-1;JGDS=-1
!      JPDS(3) = IGDNUM2
!      JPDS(5) = 191 
!      JPDS(6) = 001
!      CALL SETVAR(LUGB2,LUGI2,NUMVAL2,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,S3REF10,GFLD,ISSREF,IRET,ISTAT)

       J = 2
       JPDT(2) = 008
       JPDT(10) = 1
       JDISC = 0

      CALL SETVAR_g2(LUGB2,LUGI2,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K, &
                     KPDS,KGDS,MASK,GRID,S3REF10,GFLD,ISSREF,IRET,ISTAT)
      IF(IRET .NE. 0 )RETURN

! probability of 0.5"
!      J = 4
!      JPDS=-1;JGDS=-1
!      JPDS(3) = IGDNUM2
!      JPDS(5) = 191 
!      JPDS(6) = 001
!      CALL SETVAR(LUGB2,LUGI2,NUMVAL2,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,S3REF50,GFLD,ISSREF,IRET,ISTAT)

       J = 4
       JPDT(2) = 008
       JPDT(10) = 1
       JDISC = 0

      CALL SETVAR_g2(LUGB2,LUGI2,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K, &
                     KPDS,KGDS,MASK,GRID,S3REF50,GFLD,ISSREF,IRET,ISTAT)

        write(0,*) 'minval(S3REF50),maxval(S3REF50): ', &
                    minval(S3REF50),maxval(S3REF50)

      IF (IFHR .EQ. 3) THEN
        IF (.NOT.LCYCON) THEN
          print *, 'FHR=3 so zero 6 and 12-hr sref probabilities'
          S6REF01(:,:) = 0.0
          S6REF10(:,:) = 0.0
          S6REF50(:,:) = 0.0
          S12REF01(:,:) = 0.0
          S12REF10(:,:) = 0.0
          S12REF50(:,:) = 0.0
        ENDIF
       print *, 'bailing out of sref pcp early IFHR=',IFHR
       RETURN
      ENDIF

! 6-hr probability of 0.01"
!      J = 5
!      JPDS=-1;JGDS=-1
!      JPDS(3) = IGDNUM2
!      JPDS(5) = 191 
!      JPDS(6) = 001
!      CALL SETVAR(LUGB2,LUGI2,NUMVAL2,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,S6REF01,GFLD,ISSREF,IRET,ISTAT)

       J = 5
       JPDT(2) = 008
       JPDT(10) = 1
       JDISC = 0

      CALL SETVAR_g2(LUGB2,LUGI2,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K, &
                     KPDS,KGDS,MASK,GRID,S6REF01,GFLD,ISSREF,IRET,ISTAT)


!      JPDS(3) = IGDNUM2
!      JPDS(5) = 191 
!      JPDS(6) = 001
!      CALL SETVAR(LUGB2,LUGI2,NUMVAL2,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,S6REF10,GFLD,ISSREF,IRET,ISTAT)

       J = J+2
       JPDT(2) = 008
       JPDT(10) = 1
       JDISC = 0

      CALL SETVAR_g2(LUGB2,LUGI2,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K, &
                     KPDS,KGDS,MASK,GRID,S6REF10,GFLD,ISSREF,IRET,ISTAT)

! 6-hr probability of 0.5"
!      J = J+2
!      JPDS=-1;JGDS=-1
!      JPDS(3) = IGDNUM2
!      JPDS(5) = 191 
!      JPDS(6) = 001
!      CALL SETVAR(LUGB2,LUGI2,NUMVAL2,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,S6REF50,GFLD,ISSREF,IRET,ISTAT)

       J = J+2
       JPDT(2) = 008
       JPDT(10) = 1
       JDISC = 0

      CALL SETVAR_g2(LUGB2,LUGI2,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K, &
                     KPDS,KGDS,MASK,GRID,S6REF50,GFLD,ISSREF,IRET,ISTAT)

! 12-hr probability of 0.01"
      J = 10
        IF (IFHR .EQ. 6 .OR. IFHR .EQ. 9) THEN
        print *, 'FHR=6 or 9 so 12-hr sref probabilities not available'
          S12REF01 = 0.0
          S12REF10 = 0.0
          S12REF50 = 0.0
          RETURN
        ENDIF

!      CALL SETVAR(LUGB2,LUGI2,NUMVAL2,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,S12REF01,GFLD,ISSREF,IRET,ISTAT) 

       J = 10
       JPDT(2) = 008

      CALL SETVAR_g2(LUGB2,LUGI2,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K, &
                     KPDS,KGDS,MASK,GRID,S12REF01,GFLD,ISSREF,IRET,ISTAT)
! 12-hr probability of 0.1"
!      J = J+2 
!      CALL SETVAR(LUGB2,LUGI2,NUMVAL2,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,S12REF10,GFLD,ISSREF,IRET,ISTAT)
       J = J+2
       JPDT(2) = 008

      CALL SETVAR_g2(LUGB2,LUGI2,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K, &
                     KPDS,KGDS,MASK,GRID,S12REF10,GFLD,ISSREF,IRET,ISTAT)

! 12-hr probability of 0.5"
!      J = J+2
!      CALL SETVAR(LUGB2,LUGI2,NUMVAL2,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,S12REF50,GFLD,ISSREF,IRET,ISTAT)

       J = J+2
       JPDT(2) = 008

      CALL SETVAR_g2(LUGB2,LUGI2,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K, &
                     KPDS,KGDS,MASK,GRID,S12REF50,GFLD,ISSREF,IRET,ISTAT)

! is it SREF data?
        ISSREF=0
        else
        write(6,*) 'SKIPPED SREF READS'

        endif

      RETURN 
      END SUBROUTINE getgrib
