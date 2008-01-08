      SUBROUTINE CALRCH(EL,RICHNO)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    CALRCH      COMPUTES GRD RCH NUMBER
C   PRGRMMR: TREADON         ORG: W/NP2      DATE: 93-10-11
C     
C ABSTRACT:  
C   THIS ROUTINE COMPUTES THE GRADIENT RICHARDSON NUMBER
C   AS CODED IN ETA MODEL SUBROUTINE PROFQ2.F.
C   FIX TO AVOID UNREASONABLY SMALL ANEMOMETER LEVEL WINDS.
C   .     
C     
C PROGRAM HISTORY LOG:
C   93-10-11  RUSS TREADON
C   98-06-17  T BLACK - CONVERSION FROM 1-D TO 2-D
C   00-01-04  JIM TUCCILLO - MPI VERSION
C   01-10-22  H CHUANG - MODIFIED TO PROCESS HYBRID MODEL OUTPUT
C   02-01-15  MIKE BALDWIN - WRF VERSION
C   05-02-25  H CHUANG - ADD COMPUTATION FOR NMM E GRID
C   05-07-07  BINBIN ZHOU - ADD RSM FOR A GRID  
C   
C USAGE:    CALL CALRCH(EL,RICHNO)
C   INPUT ARGUMENT LIST:
C     EL      - MIXING LENGTH SCALE.
C
C   OUTPUT ARGUMENT LIST: 
C     RICHNO  - GRADIENT RICHARDSON NUMBER.
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
C     MACHINE : CRAY C-90
C$$$  
C
      use vrbls3d
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
      REAL EL(IM,jsta_2l:jend_2u,LM),
     *     RICHNO(IM,jsta_2l:jend_2u,LM)
      REAL, ALLOCATABLE :: THV(:,:,:)
C
C     
C*************************************************************************
C     START CALRCH HERE.
C     
      ALLOCATE ( THV(IM,JSTA_2L:JEND_2U,LM) )
C     INITIALIZE ARRAYS.
C     
!$omp  parallel do
      DO L = 1,LM
        DO J=JSTA,JEND
        DO I=1,IM
          RICHNO(I,J,L)=SPVAL
        ENDDO
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
C     COMPUTE GRADIENT RICHARDSON NUMBER AS CODED IN ETA MODEL
C     SUBROUTINE PROFQ2.F.  OUTER LOOP OVER THE VERTICAL. 
C     INTTER LOOP OVER THE HORIZONTAL.
C
!$omp  parallel do
!$omp& private(cs,ct,dthvkl,dukl,dvkl,dzkl,elkl,elklsq,
!$omp&         q2kl,qroot,rdzkl,ri,uhkl,ulkl,vhkl,vlkl,
!$omp&         wndsl,wndslp)
      DO L = 1,LM1
C
        call exch(VTM(1,jsta_2l,L))
        call exch(UH(1,jsta_2l,L))
	call exch(VH(1,jsta_2l,L))
	         
        DO J=JSTA_M,JEND_M
        DO I=2,IM-1
C
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
           WNDSLP=VTM(I,J-1,L+1)+VTM(IW,J,L+1)+
     1           VTM(IE,J,L+1)+VTM(I,J+1,L+1)
           IF(WNDSL.EQ.0..OR.WNDSLP.EQ.0.)GO TO 10
           UHKL=(UH(I,J-1,L)+UH(IW,J,L)+UH(IE,J,L)+UH(I,J+1,L))/WNDSL
           ULKL=(UH(I,J-1,L+1)+UH(IW,J,L+1)+UH(IE,J,L+1)+
     1          UH(I,J+1,L+1))/WNDSLP
           VHKL=(VH(I,J-1,L)+VH(IW,J,L)+VH(IE,J,L)+VH(I,J+1,L))/WNDSL
           VLKL=(VH(I,J-1,L+1)+VH(IW,J,L+1)+VH(IE,J,L+1)+
     1           VH(I,J+1,L+1))/WNDSLP
          END IF

          DZKL=ZINT(I,J,L)-ZINT(I,J,L+1)
          RDZKL=1./DZKL
          Q2KL=AMAX1(Q2(I,J,L),0.00001)
          QROOT=SQRT(Q2KL)
          ELKL=EL(I,J,L)
          ELKL=AMAX1(ELKL,EPSQ2)
          ELKLSQ=ELKL*ELKL
          DTHVKL=THV(I,J,L)-THV(I,J,L+1)
          DUKL=(UHKL-ULKL)
          DVKL=(VHKL-VLKL)
          CS=(DUKL*RDZKL)**2+(DVKL*RDZKL)**2
C     
C         COMPUTE GRADIENT RICHARDSON NUMBER.
C     
          IF(CS.LE.1.E-8)THEN
C
C         WIND SHEAR IS VANISHINGLY SO SET RICHARDSON
C         NUMBER TO POST PROCESSOR SPECIAL VALUE.
C
            RICHNO(I,J,L)=SPVAL
C
          ELSE
C
C         WIND SHEAR LARGE ENOUGH TO USE RICHARDSON NUMBER.
C
            CT=-1.*G*BETA*DTHVKL*RDZKL
            RI=-CT/CS
            RICHNO(I,J,L)=RI
          ENDIF
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

