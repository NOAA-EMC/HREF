       SUBROUTINE CALWXT_DOMINANT(PREC,RAIN,FREEZR,SLEET,SNOW,
     &         DOMR,DOMZR,DOMIP,DOMS)
C
C     WRITTEN: 24 AUGUST 2005, G MANIKIN 
C      
C     THIS ROUTINE TAKES THE PRECIP TYPE SOLUTIONS FROM DIFFERENT
C       ALGORITHMS AND SUMS THEM UP TO GIVE A DOMINANT TYPE
C
!      INCLUDE "parmeta"
      INCLUDE "CTLBLK.comm"
C
      PARAMETER (NALG=5)
      PARAMETER (PTHRESH=0.000004)
C    INPUT:
      REAL PREC(IM,jsta_2l:jend_2u)
      DIMENSION DOMS(IM,JM),DOMR(IM,JM),DOMZR(IM,JM),DOMIP(IM,JM)
      DIMENSION RAIN(IM,JM,NALG),SNOW(IM,JM,NALG),
     &   SLEET(IM,JM,NALG),FREEZR(IM,JM,NALG)
C
      print* , 'into dominant'
!$omp  parallel do
      DO J=JSTA,JEND
      DO I=1,IM
        DOMR(I,J) = 0.
        DOMS(I,J) = 0.
        DOMZR(I,J) = 0.
        DOMIP(I,J) = 0.
      ENDDO
      ENDDO
C
!$omp  parallel do
!$omp& private(totsn,totip,totr,totzr)
      DO 800 J=JSTA,JEND
      DO 800 I=1,IM
C   SKIP THIS POINT IF NO PRECIP THIS TIME STEP
       IF (PREC(I,J).LE.PTHRESH) GOTO 800
       TOTSN = 0
       TOTIP = 0
       TOTR  = 0
       TOTZR = 0 
C   LOOP OVER THE NUMBER OF DIFFERENT ALGORITHMS THAT ARE USED
       DO 820 L = 1, NALG
        IF (RAIN(I,J,L).GT. 0) THEN
           TOTR = TOTR + 1
           GOTO 830
        ENDIF

        IF (SNOW(I,J,L).GT. 0) THEN
           TOTSN = TOTSN + 1
           GOTO 830
        ENDIF

        IF (SLEET(I,J,L).GT. 0) THEN
           TOTIP = TOTIP + 1
           GOTO 830
        ENDIF

        IF (FREEZR(I,J,L).GT. 0) THEN
           TOTZR = TOTZR + 1
           GOTO 830
        ENDIF
 830    CONTINUE
 820    CONTINUE

C   TIES ARE BROKEN TO FAVOR THE MOST DANGEROUS FORM OF PRECIP
C     FREEZING RAIN > SNOW > SLEET > RAIN 
        IF (TOTSN .GT. TOTIP) THEN
         IF (TOTSN .GT. TOTZR) THEN
          IF (TOTSN .GE. TOTR) THEN
           DOMS(I,J) = 1
           GOTO 800 
          ELSE
           DOMR(I,J) = 1 
           GOTO 800 
          ENDIF
         ELSE IF (TOTZR .GE. TOTR) THEN
          DOMZR(I,J) = 1
          GOTO 800 
         ELSE
          DOMR(I,J) = 1
          GOTO 800 
         ENDIF 
        ELSE IF (TOTIP .GT. TOTZR) THEN
         IF (TOTIP .GE. TOTR) THEN
          DOMIP(I,J) = 1
          GOTO 800 
         ELSE
          DOMR(I,J) = 1
          GOTO 800 
         ENDIF
        ELSE IF (TOTZR .GE. TOTR) THEN
         DOMZR(I,J) = 1
         GOTO 800 
         ELSE
          DOMR(I,J) = 1
          GOTO 800 
         ENDIF
 800  CONTINUE 
      RETURN
      END
