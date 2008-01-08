
      SUBROUTINE CALLLWS(U,V,H,LLWS)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    CALLLWS       COMPUTES Low Level Wind Shear (0-2000feet) 
C   PRGRMMR: Binbin Zhou      /NCEP/EMC  DATE: 2005-08-16       
C     
C ABSTRACT:  
C    This program computes the low level wind shear(LLWS) over 0-2000 feet (0-609.5m)
c    layer. But because 10m wind represent sfc wind, 10-619.5 m layer
c    is used. (NOAA/NWS Instruction 10-813, 2004)
c
c    Definition: LLWS(Z1,Z2) is vector difference of wind at z1 and z2
c          where Z1 = 10m   + Surface height
c                Z2 = 619.5 + Surface height
c
c    Algorithm: since Z2 is not defined in the model, so,
c           first thing is searching Z2  to see which layers 
c               it is located(ie between which two pressure levels), 
c           then find the wind vector (U2,V2)at Z2 by interpolating with 
c               the wind vectors of the at pressure levels above and below
c           then compute the vector difference between Z2 and Z1 (ie U10,V10)
c
c
c
c                               
c      ----------------------------------------- K2-1 ---------------------
c                            ^
c                            |
c                            |
c                            |            
c                 ____       |  _____ Z2, U2=interpo[U(K2),U(K2-1)]
c                  ^         |            V2=interpo[V(K2),V(K2-1)]
c                  |         |                       
c      ------------|---------|------------------ K2 ------------------------
c                  |         |
c                  |         |DH=SUM of all layers between K1-1 & K2-1 
c                  |         |                                            .              
c                  |609.5m   |                                            .
c                  |(2000ft) |                                            .
c                  |         v
c      ------------|---------------------------------------------------- LSM-2
c                  |               ^
c                  |               |ZH1   
c                  |               |
c                 o-o 10m       ___v__ Z1,U10,V10                 
c       FIS    ....|.....          ^
c        ^   .            .        |
c      --|-------------------------|------------ K1 -------------------- LSM-1
c        | .                .      |
c        |.                  .     |
c       .|                    ...  |
c      --|-------------------------|------------------------------------- LSM
c      . |                         |
c     ////////////////////////////////////////////////////////////////// Sea Level
c
c
C USAGE:    CALL CALLLWS(U,V,H,LLWS)
C   INPUT ARGUMENT LIST:
C     U     - U wind profile (m/s) (at pressure level)
C     V     - V wind (m/s)         (at pressure level)
C     H     - Height (m)           (at pressure level)
C
C   OUTPUT ARGUMENT LIST: 
C     LLWS  - Low level wind shear (Knots/2000ft) 
C     
C   OUTPUT FILES:
C     NONE
C     
C   SUBPROGRAMS CALLED:
C     UTILITIES:
C     LIBRARY:
C       NONE
C     
C   ATTRIBUTES:
C     LANGUAGE: FORTRAN 90/77
C     MACHINE : BLUE AT NCEP
C$$$  
C
      USE vrbls2d

!      INCLUDE "parmeta"
      INCLUDE "params"
      INCLUDE "CTLBLK.comm"
      INCLUDE "GRIDSPEC.comm"
C     
C     DECLARE VARIABLES.
C     
      REAL,DIMENSION(IM,JM,LSM),INTENT(IN)    :: U,V,H
      REAL,DIMENSION(IM,JM),INTENT(INOUT)     :: LLWS
      REAL    :: Z1,Z2,HZ1,DH,U2,V2,W2,RT 
      INTEGER :: K1,K2 

C***************************************************************
C
C

      DO 100 J=JSTA,JEND
        DO I=1,IM
 
          Z1 = 10.0 + FIS(I,J)*GI                              !Height of 10m levels geographic height (from sea level)
          
          IF(Z1.LT.H(I,J,LSM)) THEN                            !First search location of 10m wind level
            K1 = LSM + 1                                       !to see it is in which pressure levels
          ELSE
            DO LP = LSM,2,-1                                   !If not found, keep searching upward                              
             IF(Z1.GE.H(I,J,LP).AND.Z1.LT.H(I,J,LP-1)) THEN
               K1 = LP 
             END IF
            END DO
          END IF

          HZ1 = H(I,J,K1-1) - Z1                                !Distance between K1-1 and 10m level
 
          DH = 0.0

          IF((HZ1+10).GT.609.6) THEN                            !Then, search 2000ft(609.6m) location
            U2= U10(I,J) + (U(I,J,K1-1)-U10(I,J))*599.6/HZ1     !found it between K1-1 and K1, then linear
            V2= V10(I,J) + (V(I,J,K1-1)-V10(I,J))*599.6/HZ1     !interpolate to get wind at 2000ft U2,V2     
            Z2= FIS(I,J)*GI + 609.6
          ELSE                                                 !otherwise, keep on search upward
            DO LP = K1-1,2,-1
             DH=DH+(H(I,J,LP-1) - H(I,J,LP))
             IF((DH+HZ1+10).gt.609.6) THEN                      !found the 2000ft level 
               Z2=FIS(I,J)*GI+609.6   
               RT=(Z2-H(I,J,LP))/(H(I,J,LP-1)-H(I,J,LP))
               U2=U(I,J,LP)+RT*(U(I,J,LP-1)-U(I,J,LP))
               V2=V(I,J,LP)+RT*(V(I,J,LP-1)-V(I,J,LP))
               K2=LP
               GO TO 610
              END IF
             END DO
            END IF

610       LLWS(I,J)=SQRT((U2-U10(I,J))**2+(V2-V10(I,J))**2)/    !computer vector difference
     +              609.6 * 1.943*609.6                         !unit: knot/2000ft

 
        ENDDO
 
100   CONTINUE     

      RETURN
      END


      SUBROUTINE CALICING (T1,RH,OMGA, ICING)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    CALICING       COMPUTES In-Flight Icing
C   PRGRMMR: Binbin Zhou      /NCEP/EMC  DATE: 2005-08-16       
C     
C ABSTRACT:  
C    This program computes the in-flight icing condition
c    with the T-RH-OMGA algorithm provided by S. Silberberg of
C    NCEP/AWC (improved new version)
C 
C    According to S. Silberberg, Icing happens in following 
C    situation:
C       (1) -22C < T < 0C to      
C       (2)  RH > 70 %
C       (3) Ascent air, OMGA < 0 
C       (4) Equivalent Potential Vorticity (EPV) < 0
C       (5) Cloud water if SLD (supercooled large droplet)
C
C    Current version dosn't consider SLD, so cloud water           
C    is not used. EPV computation is not available for current
C    NCEP/EMC models(NAM, WRF, RSM), so EPV is also not
C    used
C
C USAGE:    CALL CALICING(T1,RH,OMGA,ICING)
C   INPUT ARGUMENT LIST:
C     T1     - TEMPERATURE (K)
C     RH     - RELATIVE HUMIDITY  (DECIMAL FORM)
C     OMGA   - Vertical velocity (Pa/sec)
C
C   OUTPUT ARGUMENT LIST: 
C     ICING     - ICING CONDITION (1 or 0)
C     
C   OUTPUT FILES:
C     NONE
C     
C   SUBPROGRAMS CALLED:
C     UTILITIES:
C     LIBRARY:
C       NONE
C     
C   ATTRIBUTES:
C     LANGUAGE: FORTRAN 90/77
C     MACHINE : BLUE AT NCEP
C$$$  
C
!      INCLUDE "parmeta"
      INCLUDE "CTLBLK.comm"
C     
C     DECLARE VARIABLES.
C     
      REAL, DIMENSION(IM,JM), INTENT(IN)    :: T1,RH,OMGA
      REAL, DIMENSION(IM,JM), INTENT(INOUT) :: ICING 
C***************************************************************
C
C

      DO J=JSTA,JEND
        DO I=1,IM

         IF(OMGA(I,J).LT.0.0 .AND.
     +      (T1(I,J).LE.273.0.AND.T1(I,J).GE.251.0) 
     +        .AND. RH(I,J).GE.70.0) THEN

           ICING(I,J) = 1.0

         ELSE

           ICING(I,J) = 0.0

         END IF
 

        ENDDO
      ENDDO

      RETURN
      END


      SUBROUTINE CALCAT(U,V,H,U_OLD,V_OLD,H_OLD,CAT)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    CALCAT       COMPUTES Clear Air Turbulence Index 
C   PRGRMMR: Binbin Zhou      /NCEP/EMC  DATE: 2005-08-16       
C     
C ABSTRACT:  
C    This program computes the Clear Air Turbulence condition
C    which is expressed as Index with Ellrod Algorithm 
C    (Gary P. Ellrod: Wea. and Forecast,1992) and Ri number 
C    suggested by S. Silberberg of AWC. But Ri number is still
C    not classified into 3 level CAT, so current version does
C    not use Ri as suggested by S. Silberberg
C
C PROGRAM HISTORY LOG:
C
C   05-09-19  H CHUANG - MODIFIED TO COMPUTE GRADIENTS FOR BOTH A AND E GRIDS
C
C
C    According to Ellrod, the CAT is classied into 3 levels (index)
C    Light:  CAT = 1     
C    Middle: CAT = 2
C    Severe: CAT = 3
C    No CAT: CAT = 0 
C
C USAGE:    CALL CALCAT(U,V,H,L,CAT)
C   INPUT ARGUMENT LIST:
C     U     - U wind profile (m/s) (at pressure level)
C     V     - V wind (m/s)         (at pressure level)
C     H     - Height (m)           (at pressure level)
C     L     - # of pressure level
C
C   OUTPUT ARGUMENT LIST: 
C     CAT     - CAT Index
C     
C   OUTPUT FILES:
C     NONE
C     
C   SUBPROGRAMS CALLED:
C     UTILITIES:
C     LIBRARY:
C       NONE
C     
C   ATTRIBUTES:
C     LANGUAGE: FORTRAN 90/77
C     MACHINE : BLUE AT NCEP
C$$$  
      use masks
C
!      INCLUDE "parmeta"
      INCLUDE "CTLBLK.comm"
      INCLUDE "GRIDSPEC.comm"
C     
C     DECLARE VARIABLES.
C     
      REAL,DIMENSION(IM,JM),INTENT(IN)    :: U,V,H,U_OLD,V_OLD,H_OLD
C      INTEGER,INTENT(IN)                      :: L
      REAL,DIMENSION(IM,JM),INTENT(INOUT)     :: CAT

      REAL  DSH, DST, DEF, CVG, VWS, TRBINDX
      INTEGER  DXVAL, DYVAL, IHE(JM),IHW(JM)

C***************************************************************
C
C
      DO J=JSTA_2L,JEND_2U
       IF(MODELNAME .EQ. 'NCAR' .OR.MODELNAME.EQ.'RSM')THEN
        IHW(J)=-1
        IHE(J)=1 
       ELSE
        IHW(J)=-MOD(J,2)
        IHE(J)=IHW(J)+1
       END IF	
      ENDDO

      call exch_f(U)
      call exch_f(V)
      call exch_f(U_OLD)
      call exch_f(V_OLD)
      call exch_f(H)
      call exch_f(H_OLD)

      DO 100 J=JSTA_M,JEND_M
        DO I=2,IM-1
!         IF(J.EQ.JM. or. J.eq.1) GOTO 100
           
          DSH = (V(I+IHE(J),J) - V(I+IHW(J),J))*10000./(2*DX(I,J))       !dsh=dv/dx+du/dy
     +        + (U(I,J+1) - U(I,J-1))*10000./(2*DY(I,J))

          DST = (U(I+IHE(J),J) - U(I+IHW(J),J))*10000./(2*DX(I,J))       !dst=du/dx-dv/dy
     +        - (V(I,J+1) - V(I,J-1))*10000./(2*DY(I,J))

          DEF = SQRT (DSH*DSH + DST*DST)

          CVG = -( (U(I+IHE(J),J) - U(I+IHW(J),J))*10000./(2*DX(I,J))    !cvg=-(du/dx+dv/dy)
     +            +(V(I,J+1) - V(I,J-1))*10000./(2*DY(I,J)) )

          IF(MODELNAME .EQ. 'NCAR' .OR.MODELNAME.EQ.'RSM')THEN
           VWS = ( SQRT(U_OLD(I,J)**2+V_OLD(I,J)**2 ) -           !vws=d|U|/dz
     +            SQRT(U(I,J)**2+V(I,J)**2 )   ) *
     +            1000.0/(H_OLD(I,J) - H(I,J))
          else
           if((H_OLD(I+IHE(J),J) - H(I+IHE(J),J)).lt.1.e-10)print*,
     +  'problem in calcat',i+IHE(J),j,H_OLD(I+IHE(J),J)
     +, H(I+IHE(J),J)
           if((H_OLD(I+IHW(J),J) - H(I+IHW(J),J)).lt.1.e-10)print*,
     +  'problem in calcat',i+IHW(J),j,H_OLD(I+IHW(J),J)
     +, H(I+IHW(J),J)
           if((H_OLD(I,J-1) - H(I,J-1)).lt.1.e-10)print*,
     +  'problem in calcat',i,j-1,H_OLD(I,J-1), H(I,J-1)
           if((H_OLD(I,J+1) - H(I,J+1)).lt.1.e-10)print*,
     +  'problem in calcat',i,j+1,H_OLD(I,J+1), H(I,J+1)
	   VWS1 = ( SQRT(U_OLD(I+IHE(J),J)**2+V_OLD(I+IHE(J),J)**2 ) -           !vws=d|U|/dz
     +            SQRT(U(I+IHE(J),J)**2+V(I+IHE(J),J)**2 )   ) *
     +            1000.0/(H_OLD(I+IHE(J),J) - H(I+IHE(J),J))
           VWS2 = ( SQRT(U_OLD(I+IHW(J),J)**2+V_OLD(I+IHW(J),J)**2 ) -           !vws=d|U|/dz
     +            SQRT(U(I+IHW(J),J)**2+V(I+IHW(J),J)**2 )   ) *
     +            1000.0/(H_OLD(I+IHW(J),J) - H(I+IHW(J),J))
           VWS3 = ( SQRT(U_OLD(I,J-1)**2+V_OLD(I,J-1)**2 ) -           !vws=d|U|/dz
     +            SQRT(U(I,J-1)**2+V(I,J-1)**2 )   ) *
     +            1000.0/(H_OLD(I,J-1) - H(I,J-1))
           VWS4 = ( SQRT(U_OLD(I,J+1)**2+V_OLD(I,J+1)**2 ) -           !vws=d|U|/dz
     +            SQRT(U(I,J+1)**2+V(I,J+1)**2 )   ) *
     +            1000.0/(H_OLD(I,J+1) - H(I,J+1))
           VWS=(VWS1+VWS2+VWS3+VWS4)/4.0
	  END IF  
           
          TRBINDX = ABS(VWS)*(DEF + ABS(CVG))
         
          IF(TRBINDX.LE.4.) CAT(I,J) = 0.0
          IF(TRBINDX.LE.8. .AND. TRBINDX.GT.4.) CAT(I,J)=1.0
          IF(TRBINDX.LE.12. .AND. TRBINDX.GT.8.) CAT(I,J)=2.0
          IF(TRBINDX.GT.12.) CAT(I,J)=3.0
        
 
        ENDDO
 
100   CONTINUE     

      RETURN
      END


      SUBROUTINE CALCEILING (CLDZ,TCLD,CEILING)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    CALCEILING       COMPUTES Ceiling
C   PRGRMMR: Binbin Zhou      /NCEP/EMC  DATE: 2005-08-18       
C     
C ABSTRACT:  
C    This program computes the ceiling
C    Definition: Ceiling is the cloud base height for cloud fraction > 50%
C    The cloud base is from sea level in the model, while ceiling
C    is from surface. If no ceiling, set ceiling height = 20000 m
C
C USAGE:    CALL CALCEILING (CLDZ,TCLD,CEILING)
C   INPUT ARGUMENT LIST:
C     CLDZ   - CLOUD BASE HEIGHT from sea level(M)
C     TCLD   - TOTAL CLOUD FRACTION (%)
C
C   OUTPUT ARGUMENT LIST: 
C     CEILING - CEILING HEIGHT from surface (m)
C     
C   OUTPUT FILES:
C     NONE
C     
C   SUBPROGRAMS CALLED:
C     UTILITIES:
C     LIBRARY:
C       NONE
C     
C   ATTRIBUTES:
C     LANGUAGE: FORTRAN 90/77
C     MACHINE : BLUE AT NCEP
C$$$  
C

      USE vrbls2d

!      INCLUDE "parmeta"
      INCLUDE "params"
      INCLUDE "CTLBLK.comm"
C     
C     DECLARE VARIABLES.
C     
      REAL, DIMENSION(IM,JM), INTENT(IN)    :: CLDZ, TCLD
      REAL, DIMENSION(IM,JM), INTENT(INOUT) :: CEILING
C***************************************************************
C
C

      DO J=JSTA,JEND
        DO I=1,IM

         IF(TCLD(I,J).GE.50.) THEN
           CEILING(I,J)=CLDZ(I,J) - FIS(I,J)*GI
         ELSE
           CEILING(I,J)=20000.0
         END IF

         IF(CEILING(I,J).LT.0.0) CEILING(I,J)=20000.0

        ENDDO
      ENDDO

      RETURN
      END


      SUBROUTINE CALFLTCND (CEILING,FLTCND)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    CALFLTCND   COMPUTES Ceiling
C   PRGRMMR: Binbin Zhou      /NCEP/EMC  DATE: 2005-08-18       
C     
C ABSTRACT:  
C    This program computes the flight condition restriction 
C    which is defined as follow (NOAA/NWS/Instruction for TAF, 2004):
C  
C                Ceiling(feet)             Visibility(miles)   FLTCND
C      LIFR        < 200           and/or      < 1               1
C      IFR      >= 500 to <  1000  and/or     >=1 to <  3        2
C      MVFR     >=1000 to <= 3000  and/or     >=3 to <= 5        3
C      VFR         > 3000                       > 5              5
C
C
C USAGE:    CALL CALFLTCND(CEILING,FLTCND)
C   INPUT ARGUMENT LIST:
C     CEILING - CEILING HEIGHT from surface (m)
C     NOTE: VIS - Visibility is passed through COMMON /VISB/
C
C   OUTPUT ARGUMENT LIST: 
C     FLTCND - FLIGHT CONDITION CATERGORY     
C     
C   OUTPUT FILES:
C     NONE
C     
C   SUBPROGRAMS CALLED:
C     UTILITIES:
C     LIBRARY:
C       NONE
C     
C   ATTRIBUTES:
C     LANGUAGE: FORTRAN 90/77
C     MACHINE : BLUE AT NCEP
C$$$  
C
      use vrbls2d
!      INCLUDE "parmeta"
      INCLUDE "CTLBLK.comm"
C     
C     DECLARE VARIABLES.
C     
      REAL, DIMENSION(IM,JM), INTENT(IN)    :: CEILING
      REAL, DIMENSION(IM,JM), INTENT(INOUT) :: FLTCND
!      REAL  CEIL,VISI,VIS(IM,JM) 
      REAL  CEIL,VISI

!      COMMON /VISB/VIS
C***************************************************************
C
C

      DO J=JSTA,JEND
        DO I=1,IM
 
          CEIL = CEILING(I,J) * 3.2808               !from m -> feet
          VISI = VIS(I,J) / 1609.0                   !from m -> miles       

          IF(CEIL.LT.500.0 .OR. VISI.LT.1.0 ) THEN
             FLTCND(I,J) = 1.0

          ELSE IF( (CEIL.GE.500.AND.CEIL.LT.1000.0) .OR.
     +              (VISI.GE.1.0.AND.VISI.LT.3.0) ) THEN
             FLTCND(I,J) = 2.0

          ELSE IF( (CEIL.GE.1000.AND.CEIL.LE.3000.0) .OR.
     +              (VISI.GE.3.0.AND.VISI.LE.5.0) ) THEN
             FLTCND(I,J) = 3.0

          ELSE IF( CEIL.GT.3000.0  .OR. VISI.GT.5.0) THEN
             FLTCND(I,J) = 4.0

          END IF

        ENDDO
      ENDDO

      RETURN
      END
