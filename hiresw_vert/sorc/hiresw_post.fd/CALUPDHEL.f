      SUBROUTINE CALUPDHEL(UPDHEL)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    CALUPDHEL      COMPUTES UPDRAFT HELICITY
!   PRGRMMR: PYLE            ORG: W/NP2      DATE: 07-10-22       
!     
! ABSTRACT:  
!     THIS ROUTINE COMPUTES THE UPDRAFT HELICITY
!   .     
!     
! PROGRAM HISTORY LOG:
!   07-10-22  M PYLE - based on SPC Algorithm courtesy of David Bright
!   11-01-11  M Pyle - converted to F90 for unified post

!     
! USAGE:    CALL CALUPDHEL(UPDHEL)
!
!   INPUT ARGUMENT LIST:
!     NONE
!
!   OUTPUT ARGUMENT LIST: 
!     UPDHEL   - UPDRAFT HELICITY (M^2/S^2)
!     
!   OUTPUT FILES:
!     NONE
!     
!   SUBPROGRAMS CALLED:
!     UTILITIES:
!       NONE
!     LIBRARY:
!       COMMON   - CTLBLK
!     
!   ATTRIBUTES:
!     LANGUAGE: FORTRAN
!     MACHINE : CRAY C-90
!$$$  
!     
!
      use vrbls2d
      use vrbls3d
      use masks
!
!     INCLUDE ETA GRID DIMENSIONS.  SET/DERIVE OTHER PARAMETERS.
!     
!      INCLUDE "parmeta"
!      INCLUDE "params"

      use params_mod
      use ctlblk_mod
      use gridspec_mod

      REAL, PARAMETER:: HLOWER=2000.
      REAL, PARAMETER:: HUPPER=5000.
!
!     DECLARE COMMONS.
!      INCLUDE "CTLBLK.comm"
!      INCLUDE "GRIDSPEC.comm"
!
!     DECLARE VARIABLES.
!     
!      LOGICAL RUN,FIRST,RESTRT,SIGMA,OLDRD,STRD
      REAL ZMIDLOC
      REAL :: HTSFC(IM,JM),UPDHEL(IM,JM)
      INTEGER IHE(JM),IHW(JM)
!        INTEGER DXVAL,DYVAL,CENLAT,CENLON,TRUELAT1,TRUELAT2
!        INTEGER LATSTART,LONSTART,LATLAST,LONLAST 
!     
!***************************************************************************
!     START CALUPDHEL HERE.
!     

      DO J=JSTA_2L,JEND_2U
      DO I=1,IM
        UPDHEL(I,J) = D00
      ENDDO
      ENDDO

      DO J=JSTA_2L,JEND_2U
        IHW(J)=-MOD(J,2)
        IHE(J)=IHW(J)+1
      ENDDO

!     Integrate (w * relative vorticity * dz) over the 2 km to
!     5 km AGL depth.

!	initial try without horizontal averaging

      DO J=JSTA_M,JEND_M
      DO I=1,IM
        HTSFC(I,J)=ZINT(I,J,NINT(LMH(I,J)))
      ENDDO
      ENDDO

      DO J=JSTA_M,JEND_M
      DO I=2,IM-1

       R2DX   = 1./(2.*DX(I,J))
       R2DY   = 1./(2.*DY(I,J))

       l_loop: DO L=1,LM
        ZMIDLOC=0.5*(ZINT(I,J,L)+ZINT(I,J,L+1))

        IF ( (ZMIDLOC - HTSFC(I,J)) .ge. HLOWER  .AND.   &
     &       (ZMIDLOC - HTSFC(I,J)) .le. HUPPER ) THEN
           DZ=(ZINT(I,J,L)-ZINT(I,J,L+1))

           IF (WH(I,J,L) .lt. 0) THEN

!          ANY DOWNWARD MOTION IN 2-5 km LAYER KILLS COMPUTATION AND
!          SETS RESULTANT UPDRAFT HELICTY TO ZERO

              UPDHEL(I,J)=0.
              EXIT l_loop

           ENDIF

         IF(MODELNAME .EQ. 'NCAR' .OR.MODELNAME.EQ.'RSM')THEN
           DVDX   = (VH(I+1,J,L)-VH(I-1,J,L))*R2DX
           DUDY   = (UH(I,J+1,L)-UH(I,J-1,L))*R2DY
         ELSE
           DVDX   = (VH(I+IHE(J),J,L)-VH(I+IHW(J),J,L))*R2DX
           DUDY   = (UH(I,J+1,L)-UH(I,J-1,L))*R2DY
         ENDIF

         UPDHEL(I,J)=UPDHEL(I,J)+(DVDX-DUDY)*WH(I,J,L)*DZ

        ENDIF
      ENDDO l_loop
      ENDDO
      ENDDO

!
!     
!     END OF ROUTINE.
!     
      RETURN
      END
