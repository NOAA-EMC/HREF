!----------------------------------------------------------------------
      subroutine vadjust(VALIDPT,VEG_NDFD,U,V,HTOPO,DX,DY,IM,JM,gdin)
!----------------------------------------------------------------------

! --- FROM CALMET   Version: 5.8        Level: 050328                 ADJUST

!      THIS ROUTINE ADJUSTS SURFACE WINDS FOR TERRAIN EFFECTS
!     INPUTS:  U (R ARRAY)     - GRIDDED X-DIRECTION WIND COMPONENTS
!              V (R ARRAY)     - GRIDDED Y-DIRECTION WIND COMPONENTS
!              HTOPO (R ARRAY) - GRIDDED TERRAIN HEIGHTS
!              HBAR (R ARRAY)  - MODEL LEVELS HEIGHTS
!              UB (R ARRAY)    - U-COMPONENT BOUNDARY VALUES
!              VB (R ARRAY)    - V-COMPONENT BOUNDARY VALUES
!       Parameters: MXNX, MXNY, MXNZ, MXNZP1

!     OUTPUTS:  U (R ARRAY) - X-DIRECTION WIND COMPONENTS WITH
!                             ADJUSTED SURFACE LAYER WINDS
!               V (R ARRAY) - Y-DIRECTION WIND COMPONENTS WITH
!                             ADJUSTED SURFACE LAYER WINDS

    use constants
    use grddef
    use aset2d
    use aset3d

    LOGICAL, INTENT(IN) :: VALIDPT(:,:)
    REAL, INTENT(IN) :: VEG_NDFD(:,:)
    REAL, INTENT(INOUT) :: U(:,:),V(:,:)
    REAL, INTENT(IN) :: HTOPO(:,:),DX,DY
    TYPE (GINFO)        :: GDIN
    REAL, ALLOCATABLE   :: PHI(:,:,:)
    real HBAR,DXI,DYI,FX,FY,HTOIM1,HTOJM1,HTOIP1,HTOJP1,DHDX,DHDY, &
         DXSQ,DYSQ,DSQ,FACT,ERROR,ERR,EPSI,OVREL,XX,YY,XOLD,DSCALE, &
         AVEERR
    integer itmax,ii,jj,kk,idir,it, NCOUNT
    real :: tstart, tend

    INTERFACE
    SUBROUTINE setphibnd(validpt,nx,ny,phi)
!==========================================================
!     Set PHI at validpt boundaries
!==========================================================
      LOGICAL, INTENT(IN) :: VALIDPT(:,:)
      REAL, INTENT(INOUT) :: PHI(:,:,:)
      INTEGER, INTENT(IN) :: NX,NY
     END SUBROUTINE setphibnd
    END INTERFACE

!     ITERATION CRITERIA
      DATA ITMAX,EPSI,OVREL/75,0.02,1.5/

        call cpu_time(tstart)
      KK = 1
      NX=IM;NY=JM
      ALLOCATE (PHI(NX,NY,2),STAT=kret)
      print *,'============================================================'
      print *,'VADJUST:  DX  DY  NX NY', DX,DY,NX,NY

!     COMPUTE TERRAIN GRADIENTS AND INITIAL POTENTIAL
      PHI=0.1
      DXI=0.5/DX
      DYI=0.5/DY
      print *,'DXI DYI',DXI,DYI
      print *,'NDFD Topo', MINVAL(HTOPO),MAXVAL(HTOPO)
      print *,'MDL Topo', MINVAL(ZSFC),MAXVAL(ZSFC)
      print *,'U ', MINVAL(U),MAXVAL(U)
      print *,'V ', MINVAL(V),MAXVAL(V)

      do j=2,ny-1
      do i=2,nx-1
       if(validpt(i,j)) then
         HBAR=HGHT(I,J,1)
         if (HBAR .LT. 1)HBAR=1.0
         FX=DXI/HBAR
         FY=DYI/HBAR
         HTOIM1=HTOPO(I,J)
         HTOJM1=HTOPO(I,J)
         HTOIP1=HTOPO(I,J)
         HTOJP1=HTOPO(I,J)
         IF(validpt(i-1,j)) HTOIM1=HTOPO(I-1,J)
         IF(validpt(i+1,j)) HTOIP1=HTOPO(I+1,J)
         IF(validpt(i,j-1)) HTOJM1=HTOPO(I,J-1)
         IF(validpt(i,j+1)) HTOJP1=HTOPO(I,J+1)

         DHDX=(HTOIP1-HTOIM1)*FX
         DHDY=(HTOJP1-HTOJM1)*FY

!TEST         PHI(I,J,2)=(U(I,J)*DHDX+V(I,J)*DHDY)*DSCALE
         PHI(I,J,2)=(U(I,J)*DHDX+V(I,J)*DHDY)
         if (abs(PHI(i,j,2)).gt.100.) then 
!           print *, '==================================================='
            print *,i,j,'PHI Large',phi(i,j,2),validpt(i,j),DSCALE,U(I,J),ZMAX
!            print *,'DSCALE',dscale, ' ZMAX',ZMAX
!            print *, 'FX',FX,'DHDX', DHDX, 'DHDY', DHDY
!            print *, 'HGHT', HGHT(I,J,1)  
!            print *, 'HTOIP,IM',HTOIP1,HTOIM1
!            print *, 'HTOJP,JM',HTOJP1,HTOJM1
!            print *,' U, V', U(i,j),V(i,j)
!           print *, '==================================================='
         endif
!         if (abs(U(i,j)).gt.100.) print *, i,j,'U LARGE', U(i,j)
!         if (abs(V(i,j)).gt.100.) print *, i,j,'V LARGE', V(i,j)

!     CALCULATE THE VERTICAL VELOCITY DUE TO TOPOGRAPHIC EFFECTS (JTM)
!          WTOPO=U(I,J)*DHDX+V(I,J)*DHDY
!          PHI(I,J,1)=WTOPO
!          TEST assume terrain following winds --no topo effects
         PHI(I,J,1)=0.1
       else
         PHI(I,J,1)=0.1
         PHI(I,J,2)=0.1
       endif
      enddo
      enddo
        call cpu_time(tend)
        write(0,*) 'elapsed to past first block: ', tend-tstart

!     SET BOUNDARY VALUES FOR PHI
      call setphibnd(validpt,nx,ny,phi)
      print *,'VADJUST PHI 1 IC :POIS ', MINVAL(PHI(:,:,1)),MAXVAL(PHI(:,:,1))
      print *,'VADJUST PHI 2 IC :', MINVAL(PHI(:,:,2)),MAXVAL(PHI(:,:,2))

        call cpu_time(tend)
        write(0,*) 'elapsed to past setphibnd: ', tend-tstart
!     SOLVE POISSON EQUATION BY GAUSS-SEIDEL METHOD FOR
!     VELOCITY POTENTIAL

      DXSQ=DX*DX
      DYSQ=DY*DY
      DSQ=DXSQ*DYSQ
      FACT=1.0/(2.0*(DXSQ+DYSQ))
      DO 100 IT=1,ITMAX
          ERROR=-1.0E+09
          AVEERR=0.
          NCOUNT=0
        DO 90 IDIR=1,4
          do jj=2,ny-1
          do ii=2,nx-1

            SELECT CASE (IDIR)
             CASE (1)
              I=II
              J=JJ
             CASE(2)  
              I=NX-II+1
              J=JJ
             CASE (3) 
              I=II
              J=NY-JJ+1
             CASE(4)
              I=NX-II+1
              J=NY-JJ+1
            END SELECT

           if(validpt(i,j)) then
             XOLD=PHI(I,J,KK)
             PHIIM1=PHI(I,J,KK)
             PHIJM1=PHI(I,J,KK)
             PHIIP1=PHI(I,J,KK)
             PHIJP1=PHI(I,J,KK)
             IF(validpt(i-1,j)) PHIIM1=PHI(I-1,J,KK)
             IF(validpt(i,j-1)) PHIJM1=PHI(I,J-1,KK)
             IF(validpt(i+1,j)) PHIIP1=PHI(I+1,J,KK)
             IF(validpt(i,j+1)) PHIJP1=PHI(I,J+1,KK)

             XX=DYSQ*(PHIIP1+PHIIM1)
             YY=DXSQ*(PHIJP1+PHIJM1)
!==================================================================================
             PHI(I,J,KK) = (1.-OVREL)*PHI(I,J,KK)+ OVREL*FACT*(XX+YY-DSQ*PHI(I,J,2))
!==================================================================================
           else
             PHI(I,J,KK)=0.1;XOLD=0.1;PHIIM1=0.1;PHIJM1=0.1;PHIIP1=.1;PHIJP1=0.1
             PHI(I,J,2)=0.1
           endif
           IF(ABS(XOLD).GE.1.0E-10) THEN     
             ERR=ABS((PHI(I,J,KK)-XOLD)/XOLD)
             ERROR=AMAX1(ERR,ERROR)
             AVEERR=AVEERR+ERR
             NCOUNT=NCOUNT+1
           ENDIF
         enddo
         enddo 
   90   CONTINUE
!        IF (ERROR.LE.EPSI) exit
        IF (AVEERR/NCOUNT .LE.EPSI) THEN
         write(0,*) 'exiting after IT, ERROR: ', IT, ERROR
         write(0,*) 'exiting after IT, AVEERR: ', IT, AVEERR/NCOUNT
         exit
        ENDIF
	write(0,*) 'IT, ERROR: ', IT, ERROR
        write(0,*) 'IT, AVEERR: ', IT, AVEERR/NCOUNT
        if (mod(IT,10) .eq. 0) then
        call cpu_time(tend)
        write(0,*) 'elapsed to this point: ', tend-tstart
        endif
  100 CONTINUE
      print *,'VADJUST :' ,IT,IDIR, ' XOLD',XOLD,'ERROR',ERROR

! Set PHI at validpt boundaries
      call setphibnd(validpt,nx,ny,phi)

      print *,'VADJUST PHI 1 :POIS ', MINVAL(PHI(:,:,1)),MAXVAL(PHI(:,:,1))
      print *,'VADJUST PHI 2 :', MINVAL(PHI(:,:,2)),MAXVAL(PHI(:,:,2))

!     COMPUTE WIND COMPONENTS FROM VELOCITY POTENTIAL
      do j=2,ny-1
      do i=2,nx-1
        if (validpt(i,j)) then
          PHIIM1=PHI(I,J,KK)
          PHIJM1=PHI(I,J,KK)
          PHIIP1=PHI(I,J,KK)
          PHIJP1=PHI(I,J,KK)
          IF(validpt(i-1,j)) PHIIM1=PHI(I-1,J,KK)
          IF(validpt(i,j-1)) PHIJM1=PHI(I,J-1,KK)
          IF(validpt(i+1,j)) PHIIP1=PHI(I+1,J,KK)
          IF(validpt(i,j+1)) PHIJP1=PHI(I,J+1,KK)
          UOLD=U(I,J)
          VOLD=V(I,J)

!  DSCALE based on difference in terrain
!  Using model hght,HGHT, since it is not less than or equal to 0.
!  NEED to check if HGHT is geopotential or just model level hgt
          H1=HGHT(I,J,1)-ZSFC(I,J)
          ZMAX=AMAX1(HGHT(I,J,1),HTOPO(I,J))
          if (VEG_NDFD(I,J) .LE. 0. .or. VEG_NDFD(I,J) .EQ. 16) then
            DSCALE=0.0
          else 
            DSCALE=ABS(HGHT(I,J,1) - (HTOPO(I,J)+H1))/ABS(ZMAX)
!            if (i.eq.300) then
!              if (j.ge.300.and.j.le.400) then
!                print *, DSCALE, H1, hght(i,j,1), htopo(i,j)
!              endif
!            endif
          endif
          DSCALE=AMIN1(DSCALE,1.0)
          DSCALE=AMAX1(DSCALE,0.0)
          U(I,J)=(PHIIP1-PHIIM1)*DXI*DSCALE+U(I,J)
          V(I,J)=(PHIJP1-PHIJM1)*DYI*DSCALE+V(I,J)
          diffi=U(i,j)-UOLD
          diffj=V(i,j)-VOLD

          if (abs(diffi).gt.10. ) then
            if(diffi.gt.10) diffi=10
            if(diffi.lt.-10) diffi=-10
            U(I,J)=UOLD+diffi
            print *, i,j,'DIFFU', diffi,diffj,'U ',UOLD, U(I,J),'PHI:',PHIIP1,PHIIM1,'SCAL:',DSCALE
          endif
          if (abs(diffj).gt.10.)  then
            if(diffj.gt.10) diffj=10
            if(diffj.lt.-10) diffj=-10
            V(I,J)=VOLD+diffj
            print *, i,j,'DIFFV', diffi,diffj,'U ',VOLD, V(I,J),'PHI:',PHIIP1,PHIIM1,'SCAL:',DSCALE
          endif

        endif
      enddo
      enddo
      print *,'VADJUST UWND  :',  MINVAL(U(:,:)),MAXVAL(U(:,:))
      print *,'VADJUST VWND  :',  MINVAL(V(:,:)),MAXVAL(V(:,:))

      RETURN
      END

      SUBROUTINE setphibnd(validpt,nx,ny,phi)
!==========================================================
!     Set PHI at validpt boundaries
!==========================================================
      LOGICAL, INTENT(IN) :: VALIDPT(:,:)
      REAL, INTENT(INOUT) :: PHI(:,:,:)
      INTEGER, INTENT(IN) :: NX,NY

      do kp=1,2

      PHIAVG=SUM(PHI(:,:,kp))/(NX*NY)
      do j=2,ny-1
      do i=2,nx-1
       if (.not.validpt(i,j))then
          if (validpt(i+1,j)) then
           PHI(I,J,kp)=PHI(I+1,J,kp)
          elseif (validpt(i,j+1)) then
           PHI(I,J,kp)=PHI(I,J+1,kp)
          elseif (validpt(i+1,j+1)) then
           PHI(I,J,kp)=PHI(I+1,J+1,kp)
          elseif (validpt(i-1,j)) then
           PHI(I,J,kp)=PHI(I-1,J,kp)
          elseif (validpt(i,j-1)) then
           PHI(I,J,kp)=PHI(I,J-1,kp)
          elseif (validpt(i-1,j-1)) then
           PHI(I,J,kp)=PHI(I-1,J-1,kp)
          elseif (validpt(i-1,j+1)) then
           PHI(I,J,kp)=PHI(I-1,J+1,kp)
          elseif (validpt(i+1,j-1)) then
           PHI(I,J,kp)=PHI(I+1,J-1,kp)
          else
           PHI(I,J,kp)=PHIAVG
          endif
        endif
      enddo
      enddo

!     Set PHI at domain edge boundaries
      PHIAVG=SUM(PHI(:,:,kp))/(NX*NY)
      print *, kp,'PHIAVG ',PHIAVG
      do j=1,ny
        if(validpt(2,j)) then
           PHI(1,J,kp)=PHI(2,J,kp)
        else
           PHI(1,J,kp)=PHIAVG
        endif
        if(validpt(NX-1,j)) then
           PHI(NX,j,kp)=PHI(NX-1,j,kp)
        else
           PHI(NX,J,kp)=PHIAVG
        endif
      enddo
      do i=1,nx
        if(validpt(i,1)) then
           PHI(i,1,kp)=PHI(2,1,kp)
        else
           PHI(i,1,kp)=PHIAVG
        endif
        if(validpt(i,ny-1)) then
           PHI(i,ny,kp)=PHI(i,ny-1,kp)
        else
           PHI(i,ny,kp)=PHIAVG
        endif
      enddo
     enddo

      return
      end
