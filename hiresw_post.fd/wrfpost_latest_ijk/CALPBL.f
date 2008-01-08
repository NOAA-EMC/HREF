      SUBROUTINE CALPBL(PBLRI)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    CALPBL COMPUTES PBL HEIGHT BASED ON BULK RCH NUMBER
C     
C ABSTRACT:  
C   THIS ROUTINE COMPUTES THE BULK RICHARDSON NUMBER
C   AND PBL HEIGHT ABOVE SURFACE
C   .     
C     
C PROGRAM HISTORY LOG:
C   06-05-04  M TSIDULKO 
C   
C USAGE:    CALL CALPBL(PBLRI)
C   INPUT ARGUMENT LIST:
C
C   OUTPUT ARGUMENT LIST: 
C     PBLRI  - PBL HEIGHT ABOVE GROUND
C     
C   OUTPUT FILES:
C     NONE
C     
C   SUBPROGRAMS CALLED:
C     UTILITIES:
C       NONE
C     LIBRARY:
C       COMMON   - 
C                  CTLBLK
C     
C   ATTRIBUTES:
C     LANGUAGE: FORTRAN
C     MACHINE : 
C$$$  
C
      use vrbls3d
      use vrbls2d
      use masks
C     
C     INCLUDE,DERIVE,SET PARAMETERS.
C     
!      INCLUDE "parmeta"
!      PARAMETER(LM1=LM-1)
      INCLUDE "params"
C
C     INCLUDE COMMON BLOCKS.
C
      INCLUDE "CTLBLK.comm"
C     
C     DECLARE VARIABLES.
C     
      REAL, ALLOCATABLE :: THV(:,:,:)
      
      INTEGER IFRSTLEV(IM,jsta_2l:jend_2u)
     *       ,ICALPBL(IM,jsta_2l:jend_2u)
     *       ,LVLP(IM,jsta_2l:jend_2u)
      REAL PBLRI(IM,jsta_2l:jend_2u)
     *       ,RIF(IM,jsta_2l:jend_2u)
     *       ,RIBP(IM,jsta_2l:jend_2u)
     *       ,UBOT1(IM,jsta_2l:jend_2u)
     *       ,VBOT1(IM,jsta_2l:jend_2u)
     *       ,ZBOT1(IM,jsta_2l:jend_2u)
     *       ,THVBOT1(IM,jsta_2l:jend_2u)
     
C
C     
C*************************************************************************
C     START CALRCHB HERE.
C     
      ALLOCATE ( THV(IM,JSTA_2L:JEND_2U,LM) )

C     INITIALIZE ARRAYS.
C
!$omp  parallel do
        DO J=JSTA,JEND
        DO I=1,IM
          PBLRI(I,J)=SPVAL
        ENDDO
        ENDDO
C
C     COMPUTE VIRTUAL POTENTIAL TEMPERATURE.
C
!$omp  parallel do
!$omp& private(ape)
      DO L=LM,1,-1
        DO J=JSTA,JEND
        DO I=1,IM
          APE=(H10E5/PMID(I,J,L))**CAPA
          THV(I,J,L)=(Q(I,J,L)*D608+H1)*T(I,J,L)*APE
        ENDDO
        ENDDO
      ENDDO
C
C     COMPUTE BULK RICHARDSON NUMBER AS CODED IN GFS MODEL
C     AND RAOBS FOR VERIFICATION
C
!$omp  parallel do
!$omp& private(uhkl,ulkl,vhkl,vlkl,rib,ubot,utop,vbot,vtop,
!$omp&         betta,ricr,ustarr,wmin,tvhtop,ztop,
!$omp&         wndsl,wndslp,betta,ricr,ustarr,wmin 
!$omp&       ,IFRSTLEV
!$omp&       ,ICALPBL
!$omp&       ,LVLP
!$omp&       ,RIF
!$omp&       ,RIBP
!$omp&       ,UBOT1
!$omp&       ,VBOT1
!$omp&       ,ZBOT1
!$omp&       ,THVBOT1)

        DO J=JSTA_M,JEND_M
        DO I=2,IM-1
           IFRSTLEV(I,J) = 0
           LVLP(I,J) = LM
           ICALPBL(I,J)=0
        ENDDO
        ENDDO

      DO L = LM,2,-1

        BETTA = 100.
        RICR = 0.25
        USTARR = 0.1
        WMIN = 0.01
C
        call exch(VTM(1,jsta_2l,L))
        call exch(UH(1,jsta_2l,L))
	call exch(VH(1,jsta_2l,L))
	         
        DO J=JSTA_M,JEND_M
        DO I=2,IM-1
C
          RIF(I,J) = 0.
          IF(IFRSTLEV(I,J).EQ.0) THEN
            RIBP(I,J)=RIF(I,J)
          ENDIF

          IF(MODELNAME .EQ. 'NCAR'.OR.MODELNAME.EQ.'RSM')THEN
           UHKL=UH(I,J,L)
           ULKL=UH(I,J,L+1)
           VHKL=VH(I,J,L)
           VLKL=VH(I,J,L+1)
          ELSE IF(MODELNAME .EQ. 'NMM')THEN
           IE=I+MOD(J+1,2) 
           IW=I+MOD(J+1,2)-1
C
C         WE NEED (U,V) WINDS AT A MASS POINT.  FOUR POINT
C         AVERAGE (U,V) WINDS TO MASS POINT.  NORMALIZE FOUR
C         POINT AVERAGE BY THE ACTUAL NUMBER OF (U,V) WINDS
C         USED IN THE AVERAGING.  VTM=1 IF WIND POINT IS
C         ABOVE GROUND.  VTM=0 IF BELOW GROUND.
C
           WNDSL=VTM(I,J-1,L)+VTM(IW,J,L)+VTM(IE,J,L)+VTM(I,J+1,L)
           WNDSLP=VTM(I,J-1,L-1)+VTM(IW,J,L-1)+
     1           VTM(IE,J,L-1)+VTM(I,J+1,L-1)
           IF(WNDSL.EQ.0..OR.WNDSLP.EQ.0.)GO TO 10
           UBOT=(UH(I,J-1,L)+UH(IW,J,L)+UH(IE,J,L)+UH(I,J+1,L))/WNDSL
           UTOP=(UH(I,J-1,L-1)+UH(IW,J,L-1)+UH(IE,J,L-1)+
     1          UH(I,J+1,L-1))/WNDSLP
           VBOT=(VH(I,J-1,L)+VH(IW,J,L)+VH(IE,J,L)+VH(I,J+1,L))/WNDSL
           VTOP=(VH(I,J-1,L-1)+VH(IW,J,L-1)+VH(IE,J,L-1)+
     1           VH(I,J+1,L-1))/WNDSLP
          END IF

          IF(IFRSTLEV(I,J).EQ.0) THEN
            UBOT1(I,J)=UBOT
            VBOT1(I,J)=VBOT
            ZBOT1(I,J)=ZMID(I,J,L)
            THVBOT1(I,J)=THV(I,J,L)
            IFRSTLEV(I,J)=1
          ENDIF

          THVTOP=THV(I,J,L-1)
          ZTOP=ZMID(I,J,L-1)

C     
C         COMPUTE BULK RICHARDSON NUMBER.
C     
C  FOLLOWING VOGELEZANG AND HOLTSLAG (1996):

          WDL2 = (UTOP-UBOT1(I,J))**2 + (VTOP-VBOT1(I,J))**2 + WMIN**2
          RIB=(G/THVBOT1(I,J))*(THVTOP-THVBOT1(I,J))*
     .       (ZTOP-ZBOT1(I,J))/(WDL2+BETTA*(USTARR**2))

C     
C         COMPUTE PBL HEIGHT
C     
C --------------------------------------------------------------------
C  IF BULK RICHARDSON NUMBER (RIB) EXCEEDS THE CRITICAL RICHARDSON
C  NUMBER (RICR), DETERMINE ABL HEIGHT USING LINEAR INTERPOLATION
C  BETWEEN HEIGHTS, AND PREVIOUS (RIBP) AND CURRENT (RIB) BULK
C  RICHARDSON NUMBERS.  L IS BOUNDARY-LAYER TOP LEVEL NUMBER.
C --------------------------------------------------------------------
            IF (RIB.GE.RICR.AND.ICALPBL(I,J).EQ.0) THEN
             PBLRI(I,J)=ZMID(I,J,L)+
c-   *                  (ZMID(I,J,L-1)-ZMID(I,J,LVLP(I,J)))*
     *                  (ZMID(I,J,L-1)-ZMID(I,J,L))*
     *                   (RICR-RIBP(I,J))/(RIB-RIBP(I,J))
             ICALPBL(I,J)=1

c-------- Extract surface height -----------------------------------

             PBLRI(I,J)=PBLRI(I,J)-FIS(I,J)*GI

            ENDIF
            
            RIBP(I,J)=RIB
            LVLP(I,J)=L-1

C
 10      CONTINUE
        ENDDO
        ENDDO
      ENDDO
C     
      DEALLOCATE (THV)
C     END OF ROUTINE.
C     
      RETURN
      END

