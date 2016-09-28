MODULE assorted_definitions

        USE module_data
        USE parallel_module

CONTAINS

!-------------------------------------------------------------------


	SUBROUTINE extra_nems_fields( domnum, gridin, gridout )

	implicit none

          TYPE(input_vars), INTENT(INOUT) :: gridin
          TYPE(output_vars), INTENT(INOUT) :: gridout

          INTEGER, INTENT(IN):: DOMNUM

        INTEGER:: ITS, ITE, IDS, IDE, IMS, IME
        INTEGER:: JTS, JTE, JDS, JDE, JMS, JME
        INTEGER:: KTS, KTE, KDS, KDE, KMS, KME
        INTEGER:: II, JJ, ITER, itermax
        INTEGER:: I, J, K, L, KCOUNT, Irad

        LOGICAL :: GLOBAL, ncep_processing, print_diag
        REAL:: cur_smc, aposs_smc, eres
        REAL:: RSNOW, SNOFAC, SEAICESUM

        INTEGER:: numsoil

	REAL, PARAMETER:: SALP=2.60
        REAL, PARAMETER:: SNUP=0.040
        REAL, PARAMETER:: DTR=0.01745329
        REAL, PARAMETER:: TG0=258.16
        REAL, PARAMETER:: TGA=30.0
        REAL, PARAMETER:: TWOM=.00014584
        REAL, PARAMETER:: Z0LAND=0.10
        REAL, PARAMETER:: Z0SEA=0.001

        REAL:: VGZ0TBL(24)
        DATA VGZ0TBL / 1.00,  0.07,  0.07,  0.07,  0.07,  0.15,        &
     &                 0.08,  0.03,  0.05,  0.86,  0.80,  0.85,             &
     &                 2.65,  1.09,  0.80,  0.001, 0.04,  0.05,             &
     &                 0.01,  0.04,  0.06,  0.05,  0.03,  0.001/

        REAL:: VGZ0TBL_IGBP(20)

        DATA VGZ0TBL_IGBP / 1.800, 2.653, 0.854, 2.467, 1.966, 0.050, &
                            0.030, 0.856, 0.856, 0.080, 0.040, 0.170, &
                            1.000, 0.500, 0.011, 0.011, 0.001, 0.076, &
                            0.050, 0.030 /


        REAL, ALLOCATABLE:: ADUM2D(:,:), SM_OLD(:,:), SICE_OLD(:,:)
        REAL, ALLOCATABLE:: TG_ALT(:,:),             SM_G(:,:), SICE_G(:,:)
        REAL, ALLOCATABLE::                          SM_L(:,:), SICE_L(:,:)

        REAL:: TPH0, TPH0D, WBD, SBD, DLMD, DPHD
        REAL:: WB, SB, DLM, DPH, TDLM, TDPH, WBI, SBI
        REAL:: ROW_MID, COL_MID, DPHD_HALF, DLMD_HALF
        REAL:: EBI, ANBI, STPH0, CTPH0
        REAL:: TLMD, TPHD, TLM, TPH, STPH, CTPH
        REAL:: TERM1, FP, APH

! ------------------

	GLOBAL=gridout%GLOBAL
        ncep_processing=gridout%ncep_processing

	numsoil=gridin%numsoil


        ITS=gridin%ITS
        ITE=gridin%ITE
        IDS=gridin%IDS
        IDE=gridin%IDE
        JTS=gridin%JTS
        JTE=gridin%JTE
        JDS=gridin%JDS
        JDE=gridin%JDE

        IMS=gridin%IMS
        IME=gridin%IME
        JMS=gridin%JMS
        JME=gridin%JME

	KDS=gridin%KDS
	KDE=gridin%KDE

        if (ITS .eq. 1 .and. JTS .eq. 1) then
          print_diag=.true.
        else
          print_diag=.false.
        endif

	allocate(gridout%RTDPTH(KDE-1))

	ALLOCATE(gridout%EPSR(IMS:IME,JMS:JME))
	ALLOCATE(gridout%SR(IMS:IME,JMS:JME)) ; gridout%SR=0.
	ALLOCATE(gridout%SI(IMS:IME,JMS:JME))
	ALLOCATE(gridout%SH2O(IMS:IME,numsoil,JMS:JME))
	ALLOCATE(gridout%Z0(IMS:IME,JMS:JME))
	ALLOCATE(gridout%USTAR(IMS:IME,JMS:JME)) ; gridout%USTAR=0.1
	ALLOCATE(gridout%SNO(IMS:IME,JMS:JME))
	ALLOCATE(gridout%SST(IMS:IME,JMS:JME)) ; gridout%SST=0.
	ALLOCATE(gridout%TG(IMS:IME,JMS:JME)) ; gridout%TG=0.
        ALLOCATE(gridout%STC(IMS:IME,numsoil,JMS:JME))
        ALLOCATE(gridout%SMC(IMS:IME,numsoil,JMS:JME))
	ALLOCATE(gridout%IVGTYP(IMS:IME,JMS:JME))
	ALLOCATE(gridout%ISLTYP(IMS:IME,JMS:JME))
	ALLOCATE(gridout%CMC(IMS:IME,JMS:JME)) ; gridout%CMC=0.
        ALLOCATE(TG_ALT(IMS:IME,JMS:JME))
!!! 
        if (print_diag) then
        write(0,*) 'SM top of assorted routine'
        DO J=min(jde-1,jte),jts,min(-1,-(jte-jts)/15)
          write(0,635) (gridout%SM(i,J),I=its,ite,max(1,(ite-its)/10))
        END DO
        endif

        DO j = jts, MIN(jte,jde-1)
          DO i = its, MIN(ite,ide-1)

!           if (ncep_processing) then
              gridout%TG(I,J)=gridin%SOILTEMP(I,J)
              if (gridout%SM(i,J) .gt. 0.5 .and. gridout%TG(I,J) .lt. 180.) then
               gridout%TG(I,J)=273.15
              endif

!           endif

           gridout%SI(I,J)=0.
 	   gridout%SICE(I,J)=gridin%SEAICE(I,J)
           IF(gridout%SM(I,J).GT.0.9) THEN

              IF (gridout%SICE(I,J) .gt. 0) then
                 gridout%SI(I,J)=1.0
              ENDIF
!                                                            SEA
              gridout%EPSR(I,J)=.97
              gridout%ALBEDO(I,J)=.06
              gridout%ALBASE(I,J)=.06

              IF(gridout%SI (I,J).GT.0.    ) THEN    !    SEA-ICE
                 gridout%SM(I,J)=0.
                 gridout%SI(I,J)=0.
                 gridout%SICE(I,J)=1.
                 gridout%ALBEDO(I,J)=.65
                 gridout%ALBASE(I,J)=.65
              ENDIF
          ELSE   !                                           LAND
              gridout%SI(I,J)=5.0*gridout%WEASD(I,J)/1000.
              gridout%EPSR(I,J)=1.0
              gridout%SICE(I,J)=0.
              gridout%SNO(I,J)=gridout%SI(I,J)*.20
          ENDIF
         ENDDO
        ENDDO

! DETERMINE ALBEDO OVER LAND

        if (print_diag) then
	write(0,*) 'maxval gridout%MXSNAL: ', maxval(gridout%MXSNAL)
	write(0,*) 'maxval gridout%ALBASE: ', maxval(gridout%ALBASE)
	write(0,*) 'maxval gridout%ALBEDO: ', maxval(gridout%ALBEDO)
        endif

        DO j = jts, MIN(jte,jde-1)
          DO i = its, MIN(ite,ide-1)
           IF(gridout%SM(I,J).LT.0.9.AND.gridout%SICE(I,J).LT.0.9) THEN
! SNOWFREE ALBEDO
               IF ( (gridout%SNO(I,J) .EQ. 0.0) .OR. &
                  (  gridout%ALBASE(I,J) .GE. gridout%MXSNAL(I,J))) THEN
                  gridout%ALBEDO(I,J) = gridout%ALBASE(I,J)
               ELSE
! MODIFY ALBEDO IF SNOWCOVER (DIFFERS IF < or > THAN THRESHOLD SNUP)
                 IF (gridout%SNO(I,J) .LT. SNUP) THEN
                   RSNOW = gridout%SNO(I,J)/SNUP
                   SNOFAC = 1. - ( EXP(-SALP*RSNOW) - RSNOW*EXP(-SALP))
                 ELSE
                   SNOFAC = 1.0
                 ENDIF

!      write(0,*) 'I,J, SNO, SNUP, SNOFAC:: ', I,J, gridout%SNO(I,J), SNUP, SNOFAC

! CALCULATE ALBEDO ACCOUNTING FOR SNOWDEPTH AND VGFRCK
                 gridout%ALBEDO(I,J) = gridout%ALBASE(I,J) + &
                 (1.0-gridout%VEGFRA(I,J))*SNOFAC*         &
                 (gridout%MXSNAL(I,J)-gridout%ALBASE(I,J))
!	if (gridout%ALBEDO(I,J) .gt. .74999 .or. gridout%ALBEDO(I,J) .lt. 0.) then
!      write(0,*) 'I,J, ALBASE, ALBEDO: ', I,J, gridout%ALBASE(I,J), gridout%ALBEDO(I,J),gridout%VEGFRA(I,J)
!	endif
               ENDIF
           ENDIF
           gridout%SI(I,J)=5.0*gridout%WEASD(I,J)
           gridout%SNO(I,J)=gridout%WEASD(I,J)
          ENDDO
        ENDDO

  637   format(50(f2.0))

	ALLOCATE(SM_OLD(IMS:IME,JMS:JME)) ; SM_OLD=0.
	ALLOCATE(SICE_OLD(IMS:IME,JMS:JME)) ; SICE_OLD=0.

       if (print_diag) then
        write(0,*) 'starting sea ice on patch'
      DO J=JTE,JTS,min(-JTE/35,-1)
         write(0,637) (gridout%SICE(I,J),I=ITS,ITE,max(1,ITE/30))
      END DO
        endif

        eres=111.2*((gridout%DPHD+gridout%DLMD)/2.)
        itermax=max(1,int(15./eres))
        itermax=min(itermax,10)

      DO ITER=1,itermax
        write(0,*) 'START ITER: ', ITER, 'of: ', itermax
         call exchange_halo_r(gridout%SM, &
                              IMS, IME, JMS, JME, 1, 1, &
                              ITS, ITE, JTS, JTE, 1, 1)

         call exchange_halo_r(gridout%SICE, &
                              IMS, IME, JMS, JME, 1, 1, &
                              ITS, ITE, JTS, JTE, 1, 1)
         SM_OLD=gridout%SM
         SICE_OLD=gridout%SICE

         DO J = max(JTS,JDS+1), min(JTE,JDE-2)
          DO I = max(ITS,IDS+1), min(ITE,IDE-2)

            IF (SM_OLD(I,J) .eq. 1.) THEN

! any sea ice around point in question?
               SEAICESUM=SICE_OLD(I+1,J)+SICE_OLD(I,J+1)+ &
                         SICE_OLD(I-1,J)+SICE_OLD(I,J-1)+ &
                         SICE_OLD(I+1,J+1)+SICE_OLD(I+1,J-1) + &
                         SICE_OLD(I-1,J+1)+SICE_OLD(I-1,J-1)

               IF (SEAICESUM .ge. 1. .and. SEAICESUM .lt. 5.) THEN
                  IF ((SICE_OLD(I+1,J).eq.0 .and. SM_OLD(I+1,J).eq.0) .OR. &
                      (SICE_OLD(I-1,J).eq.0 .and. SM_OLD(I-1,J).eq.0) .OR. &
                      (SICE_OLD(I,J-1).eq.0 .and. SM_OLD(I,J-1).eq.0) .OR. &
                      (SICE_OLD(I,J+1).eq.0 .and. SM_OLD(I,J+1).eq.0) .OR. &
                      (SICE_OLD(I+1,J+1).eq.0 .and. SM_OLD(I+1,J+1).eq.0) .OR. &
                      (SICE_OLD(I+1,J-1).eq.0 .and. SM_OLD(I+1,J-1).eq.0) .OR. &
                      (SICE_OLD(I-1,J+1).eq.0 .and. SM_OLD(I-1,J+1).eq.0) .OR. &
                      (SICE_OLD(I-1,J-1).eq.0 .and. SM_OLD(I-1,J-1).eq.0)) THEN

!                     SEA ICE AND A SURROUNDING LAND POINT - CONVERT TO SEA ICE
                      write(0,*) 'making seaice (1): ', I,J
 	              gridout%SICE(I,J)=1.0
                      gridout%SM(I,J)=0.
                      gridout%STC(I,:,J)=271.16
                      gridout%SMC(I,:,J)=0.
                      gridout%SI(I,J)=0.
                      gridout%ALBEDO(I,J)=.65
                      gridout%ALBASE(I,J)=.65
                  ENDIF

               ELSEIF (SEAICESUM .ge. 5) THEN

!                 WATER POINT SURROUNDED BY ICE  - CONVERT TO SEA ICE
                  write(0,*) 'making seaice (2): ', I,J
	          gridout%SICE(I,J)=1.0
                  gridout%STC(I,:,J)=271.16
                  gridout%SMC(I,:,J)=0.
                  gridout%SM(I,J)=0.
                  gridout%SI(I,J)=0.
                  gridout%ALBEDO(I,J)=.65
                  gridout%ALBASE(I,J)=.65

               ENDIF

            ENDIF

          ENDDO
         ENDDO

!       North pole ice
	if (gridout%GLOBAL .and. DOMNUM .eq. 1 .and. JTE .eq. JDE-1) then
	write(0,*) 'making ice for global domain'
	J=JDE-1
        do I=ITS,min(ITE,IDE-1)
        gridout%SICE(I,J)=1.0
        gridout%SM(I,J)=0.
        gridout%SI(I,J)=0.
        gridout%ALBEDO(I,J)=.65
        gridout%ALBASE(I,J)=.65
        enddo
        endif

        write(0,*) 'END ITER: ', ITER

      ENDDO  ! ITER

       if (print_diag) then
        write(0,*) 'revised sea ice on patch'
      DO J=JTE,JTS,min(-JTE/35,-1)
         write(0,637) (gridout%SICE(I,J),I=ITS,ITE,max(1,ITE/30))
      END DO
        endif

      DEALLOCATE(SM_OLD, SICE_OLD)

! this block meant to guarantee land/sea agreement between SM and landmask

       DO j = jts, MIN(jte,jde-1)
         DO i = its, MIN(ite,ide-1)
          IF (gridout%SM(I,J) .gt. 0.5) THEN
                gridin%LANDMASK(I,J)=0.0
          ELSEIF (gridout%SM(I,J) .eq. 0 .and. gridout%SICE(I,J) .eq. 1) then
                gridin%LANDMASK(I,J)=0.0
          ELSEIF (gridout%SM(I,J) .lt. 0.5 .and. gridout%SICE(I,J) .eq. 0) then
                gridin%LANDMASK(I,J)=1.0
          ELSE 
                write(0,*) 'missed point in landmask definition ' , I,J
                gridin%LANDMASK(I,J)=0.0
          ENDIF
   	  IF (gridout%SICE(I,J) .gt. 0.5 .and. gridout%TSK(I,J) .eq. 0. .and. gridout%SST(I,J) .gt. 0.) THEN
            gridout%TSK(I,J)=gridout%SST(I,J)
            gridout%SST(I,J)=0.
          ENDIF
        ENDDO
      ENDDO



       IF ( ncep_processing .or. gridin%SOILCAT(its,jts) .GT. 0.5 ) THEN
          DO j = jts, MIN(jde-1,jte)
             DO i = its, MIN(ide-1,ite)
                gridout%ISLTYP(i,j) = NINT( gridin%SOILCAT(i,j) )
              IF (gridout%SM(I,J) .eq. 1) THEN
                gridout%ISLTYP(I,J)=14
              ENDIF
!	write(0,*) 'I,J, SM, ISLTYP:: ', I,J,gridout%SM(I,J),gridout%ISLTYP(I,J)

             ENDDO
          ENDDO

	if ( .NOT. ncep_processing ) then
          DO j = jts, MIN(jde-1,jte)
            DO i = its, MIN(ide-1,ite)
              CALL find_secondary_cat(gridin%SOILCTOP, 14, I,J, gridout%ISLTYP, &
                              IMS, IME, JMS, JME, 16 )
              IF (gridout%SM(I,J) .eq. 1) THEN
                gridout%ISLTYP(I,J)=14
              ENDIF
            END DO
          END DO
        endif
       END IF

       IF ( ncep_processing .or. gridin%VEGCAT(its,jts) .GT. 0.5 ) THEN
          DO j = jts, MIN(jde-1,jte)
            DO i = its, MIN(ide-1,ite)
              gridout%IVGTYP(i,j) = NINT( gridin%VEGCAT(i,j) )
	      IF (gridout%SM(I,J) .eq. 1) THEN
	IF (.not. gridout%use_igbp ) THEN
                gridout%IVGTYP(I,J)=16  ! USGS water
        ELSE
                gridout%IVGTYP(I,J)=17  ! IGBP water
        ENDIF
              ENDIF
            END DO
          END DO

	if ( .NOT. ncep_processing ) then
          DO j = jts, MIN(jde-1,jte)
            DO i = its, MIN(ide-1,ite)
	IF (.not. gridout%use_igbp ) THEN
	      CALL find_secondary_cat(gridin%LANDUSEF, 16, I,J, gridout%IVGTYP, &
                                IMS, IME, JMS, JME, 24 )
        ELSE
	      CALL find_secondary_cat(gridin%LANDUSEF, 17, I,J, gridout%IVGTYP, &
                                IMS, IME, JMS, JME, 20 )
        ENDIF

	      IF (gridout%SM(I,J) .eq. 1) THEN
                 IF (.not. gridout%use_igbp ) THEN
                   gridout%IVGTYP(I,J)=16  ! USGS water
                 ELSE
                   gridout%IVGTYP(I,J)=17  ! IGBP water
                 ENDIF
              ENDIF

           END DO
          END DO
         endif
       END IF

        if (print_diag) then
	write(0,*) 'IVGTYP'
        DO J=min(jde-1,jte),jts,min(-(jte-jts)/25,-1)
          write(0,625) (gridout%IVGTYP(i,J),I=its,ite,max(1,(ite-its)/25))
        END DO

	write(0,*) 'ISLTYP'
        DO J=min(jde-1,jte),jts,min(-(jte-jts)/25,-1)
          write(0,625) (gridout%ISLTYP(i,J),I=its,ite,max(1,(ite-its)/25))
        END DO
        endif

  625	format(40(I2,1x))

        DO J = jts, MIN(jde-1,jte)
          DO I = its, MIN(ide-1,ite)

            IF (gridout%SICE(I,J) .eq. 0) THEN
              IF (gridin%LANDMASK(I,J) .gt. 0.5 .and. gridout%SM(I,J) .eq. 1.0) THEN
                 write(0,*) 'land mask and SM both > 0.5: ', &
                             I,J,gridin%LANDMASK(I,J),gridout%SM(I,J)
                 gridout%SM(I,J)=0.
              ELSEIF (gridin%LANDMASK(I,J) .lt. 0.5 .and. gridout%SM(I,J) .eq. 0.0) THEN
                 write(0,*) 'land mask and SM both < 0.5: ', &
                             I,J, gridin%LANDMASK(I,J),gridout%SM(I,J)
                 gridout%SM(I,J)=1.
              ENDIF
            ELSE
              IF (gridin%LANDMASK(I,J) .gt. 0.5 .and. gridout%SM(I,J)+gridout%SICE(I,J) .eq. 1) then
                write(0,*) 'landmask says LAND, SM/SICE say SEAICE: ', I,J
              ENDIF
            ENDIF

          ENDDO
        ENDDO

        DO J = jts, MIN(jde-1,jte)
          DO I = its, MIN(ide-1,ite)
            IF (gridout%SICE(I,J) .eq. 1.0) THEN
              IF (.not. gridout%use_igbp ) THEN
                gridout%IVGTYP(I,J)=24
              ELSE
                gridout%IVGTYP(I,J)=15
              ENDIF
              gridout%ISLTYP(I,J)=16
            ENDIF 

              IF (.not. gridout%use_igbp ) THEN
            IF (gridout%IVGTYP(I,J) .eq. 16 .and. gridout%ISLTYP(I,J) .ne. 14) 	THEN
              write(0,*) 'inconsistent at I,J with SM:: ', I,J, gridout%SM(I,J)
            ENDIF
              ELSE ! IGBP
            IF (gridout%IVGTYP(I,J) .eq. 17 .and. gridout%ISLTYP(I,J) .ne. 14) 	THEN
              write(0,*) 'inconsistent at I,J with SM:: ', I,J, gridout%SM(I,J)
            ENDIF
              ENDIF
          ENDDO
        ENDDO

        DO J = jts, MIN(jde-1,jte)
          DO I = its, MIN(ide-1,ite)
            IF (gridout%SM(I,J) .lt. 0.5) THEN
              gridout%SST(I,J)=0.
            ENDIF
            IF (gridout%SM(I,J) .gt. 0.5) THEN
              IF (gridout%SST(I,J) .eq. 0) THEN
                gridout%SST(I,J)=gridout%TSK(I,J)
              ENDIF
!mp              gridout%TSK(I,J)=0.
            ENDIF
!mp            IF ( (gridout%TSK(I,J)+gridout%SST(I,J)) .lt. 200. .or. &
!mp                 (gridout%TSK(I,J)+gridout%SST(I,J)) .gt. 350. ) THEN
!mp                  write(0,*) 'I,J, trouble SM, NMM_TSK,SST ',I,J, gridout%SM(I,J),gridout%TSK(I,J),gridout%SST(I,J)
!mp            ENDIF
          ENDDO
        ENDDO

        if (print_diag) then
        write(0,*) 'SM'
        DO J=min(jde-1,jte),jts,min(-1,-(jte-jts)/15)
          write(0,635) (gridout%SM(i,J),I=its,ite,max(1,(ite-its)/10))
        END DO

        write(0,*) 'TSK'
        DO J=min(jde-1,jte),jts,min(-1,-(jte-jts)/15)
          write(0,635) (gridout%TSK(I,J),I=ITS,min(ide-1,ite),max(1,(ite-its)/10))
        END DO
        endif

  635   format(20(f5.1,1x))

!!!!!!!!!!!!!!!!!!!!!!!!!

      DLMD=gridout%DLMD
      DPHD=gridout%DPHD
      TPH0D=gridout%TPH0D
      TPH0=TPH0D*DTR

      WBD=-((ide-2)/2)*DLMD
      SBD=-((jde-1)/2)*DPHD

      ROW_MID=((JDE-1)+ 1.0) * 0.5 
      COL_MID=((IDE-1)+ 1.0) * 0.5 
      DPHD_HALF=0.5*DPHD
      DLMD_HALF=0.5*DLMD

      WB= WBD*DTR
      SB= SBD*DTR
      DLM=DLMD*DTR
      DPH=DPHD*DTR

      TDLM=DLM+DLM
      TDPH=DPH+DPH
      WBI=WB+TDLM
      SBI=SB+TDPH

      EBI=WB+(ide-2)*TDLM
      ANBI=SB+(jde-2)*DPH
      STPH0=SIN(TPH0)
      CTPH0=COS(TPH0)

      DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,MIN(ITE,IDE-1)
           TLMD=(I-COL_MID)*DLMD+DLMD_HALF   ! velocity point transformed lat/lon
           TPHD=(J-ROW_MID)*DPHD+DPHD_HALF
           TLM = TLMD*DTR
           TPH = TPHD*DTR
           
           STPH=SIN(TPH)
           CTPH=COS(TPH)
           TERM1=(STPH0*CTPH*COS(TLM)+CTPH0*STPH)
           FP=TWOM*(TERM1)
         ENDDO
      ENDDO

      DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,MIN(ITE,IDE-1)
           TLMD=(I-COL_MID)*DLMD   ! mass point transformed lat/lon
           TPHD=(J-ROW_MID)*DPHD
           TLM = TLMD*DTR
           TPH = TPHD*DTR

           STPH=SIN(TPH)
           CTPH=COS(TPH)
           TERM1=(STPH0*CTPH*COS(TLM)+CTPH0*STPH)
           APH=ASIN(TERM1)
           TG_ALT(I,J)=TG0+TGA*COS(APH)-gridout%FIS(I,J)/3333.
         ENDDO
      ENDDO

      DO J = jts, MIN(jde-1,jte)
        DO I = its, MIN(ide-1,ite)
           IF (gridout%TG(I,J) .lt. 180.) THEN   ! only use TG_ALT definition if
                                                 ! not getting TG from elsewhere
              gridout%TG(I,J)=TG_ALT(I,J)
           ENDIF
        END DO
      END DO

      DEALLOCATE(TG_ALT)
	
!!!   zero out TSK at water points again

      DO J = jts, MIN(jde-1,jte)
        DO I = its, MIN(ide-1,ite)
          IF (gridout%SM(I,J) .gt. 0.5) THEN
!            gridout%TSK(I,J)=0.
          ENDIF
        END DO
      END DO

!!    check on STC/SMC

       DO J = jts, MIN(jde-1,jte)
         DO L = 1, numsoil
           DO I = its, MIN(ide-1,ite)

             gridout%STC(I,L,J)=gridin%STC_WPS(I,J,L)
	     IF (gridout%STC(I,L,J) .lt. 0. .or. gridout%STC(I,L,J) .gt. 350.) THEN
               write(0,*) 'bad STC definition....I,L,J, STC: ', I,L,J, gridout%STC(I,L,J)
	       IF (J .gt. jts .and. J .lt. MIN(jde-1,jte) .and. &
                   I .gt. its .and. I .lt. MIN(ide-1,ite)) THEN
                 gridout%STC(I,L,J)=max(gridin%STC_WPS(I+1,J,L), gridin%STC_WPS(I-1,J,L), &
                                        gridin%STC_WPS(I,J+1,L),gridin%STC_WPS(I,J-1,L))
                 write(0,*) 'gridout%STC(I,L,J) now: ',L, gridout%STC(I,L,J)
               ELSE
                 gridout%STC(I,L,J)=283.16
               ENDIF
             ENDIF

             gridout%SMC(I,L,J)=gridin%SMC_WPS(I,J,L)
             IF (gridout%SMC(I,L,J) .lt. 0 .or. gridout%SMC(I,L,J) .gt. 1.1) THEN
               write(0,*) 'bad SMC definition...I,L,J, SMC: ', I,L,J, gridout%SMC(I,L,J)
               IF (J .gt. jts .and. J .lt. MIN(jde-1,jte) .and. I .gt. its .and. I .lt. MIN(ide-1,ite)) THEN
                 gridout%SMC(I,L,J)=max(gridin%SMC_WPS(I+1,J,L), gridin%SMC_WPS(I-1,J,L), &
                                        gridin%SMC_WPS(I,J+1,L), gridin%SMC_WPS(I,J-1,L))
               ELSE
                 gridout%SMC(I,L,J)=0.2
               ENDIF
             ENDIF

           ENDDO
         ENDDO
       ENDDO

       DO J = jts, MIN(jde-1,jte)
         DO I = its, MIN(ide-1,ite)
           IF (gridout%SICE(I,J) .eq. 1.0) THEN 
             DO L = 1, numsoil
               gridout%STC(I,L,J)=271.16   
             END DO
               gridout%TG(I,J)=271.16
           END IF
           IF (gridout%SM(I,J) .eq. 1.0) THEN
             DO L = 1, numsoil
               gridout%STC(I,L,J)=273.16    ! TG value used by Eta/NMM
             END DO
               gridout%TG(I,J)=273.16
           END IF
         END DO
       END DO

	do Irad=1,5
        if (print_diag) write(0,*) 'start radius: ', Irad
       DO J = jts, MIN(jde-1,jte)
         DO I = its, MIN(ide-1,ite)

           IF (gridout%SM(I,J) .eq. 0. .and. gridout%STC(I,1,J) .eq. 0 .or. gridout%SMC(I,1,J) .eq. 0.) THEN
!             write(0,*) 'initially troublesome SM,STC,SMC value: ', I,J,gridout%SM(I,J),  &
!                         gridout%STC(I,1,J),gridout%SMC(I,1,J)
             DO JJ=J-Irad,J+Irad
              DO L=1, numsoil
               DO II=I-Irad,I+Irad
                 IF (II .ge. its .and. II .le. MIN(ide-1,ite) .and. &
                     JJ .ge. jts .and. JJ .le. MIN(jde-1,jte)) THEN
	         IF ( gridout%SM(II,JJ) .eq. 0) THEN
                   gridout%STC(I,L,J)=amax1(gridout%STC(I,L,J),gridout%STC(II,L,JJ))
                   cur_smc=gridout%SMC(I,L,J)
                   IF ( gridout%SMC(II,L,JJ) .gt. 0.005 .and. gridout%SMC(II,L,JJ) .lt. 1.0) THEN
                     aposs_smc=gridout%SMC(II,L,JJ)

                     IF ( cur_smc .eq. 0 ) THEN
                       cur_smc=aposs_smc
                       gridout%SMC(I,L,J)=cur_smc
                     ELSE
                       cur_smc=amin1(cur_smc,aposs_smc)
                       cur_smc=amin1(cur_smc,aposs_smc)
                       gridout%SMC(I,L,J)=cur_smc
                     ENDIF

                   ENDIF
                 ENDIF ! SM=0
                 ENDIF ! bounds check
               ENDDO
              ENDDO
             ENDDO
	     if (gridout%STC(I,1,J) .gt. 200 .and. gridout%SMC(I,1,J) .gt. 0.) then
             write(0,*) 'STC, SMC(1) now: ',I,J,  gridout%STC(I,1,J),gridout%SMC(I,1,J)
	     endif
           ENDIF

           IF (IRAD .eq. 5 .and. gridout%STC(I,1,J) .eq. 0) THEN
              write(0,*) 'BOGUSING DUE TO STILL troublesome STC value: ', I,J, &
                                         gridout%STC(I,1,J),gridout%SMC(I,1,J)
               gridout%STC(I,:,J)=280.
               if (gridout%SMC(I,1,J) .eq. 0) THEN
               gridout%SMC(I,:,J)=0.1
               endif
           ENDIF

           if (gridout%SMC(I,1,J) .eq. 0 .and. gridout%SM(I,J) .eq. 0 .and. gridout%SICE(I,J) .ne. 1.0) then
!             write(0,*) ' HAVE ZERO SMC value at a land point: ', I,J
	     if (gridout%STC(I,1,J) .eq. 271.16) then
!	write(0,*) 'make it sea ice: ', I, J
             gridout%SMC(I,:,J)=0.0
             gridout%SICE(I,J)=1.0
!             else
             endif
           ENDIF

         ENDDO
       ENDDO

	if (IRAD .eq. 5) then
       DO J = jts, MIN(jde-1,jte)
         DO I = its, MIN(ide-1,ite)
            if (gridout%SMC(I,1,J) .le. .001 .and. gridout%SICE(I,J) .ne. 1.) then
	    write(0,*) 'have dry soil at a non-ice point: ', I,J, gridout%SMC(I,1,J)
            endif
         ENDDO
       ENDDO
	endif

        enddo

!hardwire soil stuff for time being

        gridout%RTDPTH=0.
        gridout%RTDPTH(1)=0.1
        gridout%RTDPTH(2)=0.3
        gridout%RTDPTH(3)=0.6

        gridout%SLDPTH=0.
        gridout%SLDPTH(1)=0.1
        gridout%SLDPTH(2)=0.3
        gridout%SLDPTH(3)=0.6
        gridout%SLDPTH(4)=1.0

	gridout%DZSOIL(1)=0.1
	gridout%DZSOIL(2)=0.3
	gridout%DZSOIL(3)=0.6
	gridout%DZSOIL(4)=1.0

        if (print_diag) then
        write(0,*) 'STC(1)'
        DO J=min(jde-1,jte),jts,min(-1,-(jte-jts)/15)
          write(0,635) (gridout%STC(I,1,J),I=its,min(ite,ide-1),max((ite-its)/12,1))
        ENDDO

        write(0,*) 'SMC(1)'
        DO J=min(jde-1,jte),jts,min(-1,-(jte-jts)/15)
          write(0,635) (gridout%SMC(I,1,J),I=its,min(ite,ide-1),max((ite-its)/12,1))
        ENDDO
        endif

!        DO J = jts, MIN(jde-1,jte)
!          DO i=  ITS, MIN(IDE-1,ITE)
!           IF (gridout%SM(I,J) .eq. 0 .and. gridout%SMC(I,1,J) .gt. 0.5 .and. gridout%SICE(I,J) .eq. 0) THEN
!             write(0,*) 'very moist on land point: ', I,J,gridout%SMC(I,1,J)
!           ENDIF
!          ENDDO
!        ENDDO

        CALL NMM_SH2O(IMS,IME,JMS,JME,ITS,min(ITE,IDE-1),JTS,min(JTE,JDE-1),4,gridout%ISLTYP, &
                               gridout%SM,gridout%SICE,gridout%STC,gridout%SMC,gridout%SH2O)

!! must be a better place to put this, but will eliminate "phantom"
!! wind points here (no wind point on eastern boundary)
	IF (.NOT. GLOBAL) THEN
        IF (   abs(IDE-1-ITE) .eq. 1 ) THEN ! along eastern boundary
          write(0,*) 'zero phantom winds'
          DO K=1,KDE-1
            DO J=JDS,JDE-1   ! every row for B-grid
              IF (J .ge. JTS .and. J .le. JTE) THEN
                gridout%u(IDE-1,K,J)=0.
                gridout%v(IDE-1,K,J)=0.
              ENDIF
            ENDDO
          ENDDO 
        ENDIF
        ENDIF

	IF (.not. gridout%use_igbp ) THEN
          DO j = jts, min(jte,jde-1)
            DO i = its, min(ite,ide-1)
              gridout%Z0(I,J)    = VGZ0TBL(gridout%IVGTYP(I,J))
              IF (gridout%SM(I,J) .eq. 1) THEN
                gridout%Z0(I,J) = gridout%Z0(I,J) + Z0SEA
              ELSE
                IF (gridout%IVGTYP(I,J) .ne. 24) then  ! 
                   gridout%Z0(I,J) = gridout%Z0(I,J) + Z0LAND
                ENDIF
              ENDIF
            ENDDO
          ENDDO
        ELSE
          DO j = jts, min(jte,jde-1)
            DO i = its, min(ite,ide-1)
              gridout%Z0(I,J)    = VGZ0TBL_IGBP(gridout%IVGTYP(I,J))
              IF (gridout%SM(I,J) .eq. 1) THEN
                gridout%Z0(I,J) = gridout%Z0(I,J) + Z0SEA
              ELSE
                IF (gridout%IVGTYP(I,J) .ne. 15) then  ! 
                   gridout%Z0(I,J) = gridout%Z0(I,J) + Z0LAND
                ENDIF
              ENDIF
            ENDDO
          ENDDO
        ENDIF

        if (print_diag) then
          write(0,*) 'Z0 leaving module_initialize_real'
          DO J=JME,JMS,min(-(JME-JMS)/20,-1)
          write(0,635) (gridout%Z0(I,J),I=IMS,IME,max((IME-IMS)/14,1))
          ENDDO

        write(0,*) 'leaving extra_nems_fields'
	write(0,*) 'min/max of albedo leaving extra_nems_fields: ', minval(gridout%albedo),maxval(gridout%albedo)
        endif

!	call summary()
      
!==================================================================================

      RETURN

   END SUBROUTINE extra_nems_fields

!!!!!!!!!!!!!
!------------------------------------------------------

	SUBROUTINE find_secondary_cat(INPUT_DATA, AVOID, I,J, OUTPUT_DATA, &
                                      IMIN, IMAX, JMIN, JMAX, NCAT )

	INTEGER :: I, J, IMIN, IMAX, JMIN, JMAX, K
        INTEGER :: AVOID, NCAT

        REAL :: rmax
        REAL :: INPUT_DATA( IMIN:IMAX,JMIN:JMAX,NCAT)
        INTEGER :: OUTPUT_DATA(IMIN:IMAX,JMIN:JMAX)

        rmax=-1.

        do K=1,NCAT

	if (K .ne. AVOID .and. INPUT_DATA(I,J,K) .gt. rmax) then
	rmax=INPUT_DATA(I,J,K)
        OUTPUT_DATA(I,J)=K
	endif

	enddo


        END SUBROUTINE find_secondary_cat



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!--------------------------------------------------------------------
!-----------------------------------------------------------------------

      SUBROUTINE NMM_SH2O(IMS,IME,JMS,JME,ISTART,IM,JSTART,JM,&
                        NSOIL,ISLTPK, &
                        SM,SICE,STC,SMC,SH2O)

!!        INTEGER, PARAMETER:: NSOTYP=9
!        INTEGER, PARAMETER:: NSOTYP=16
        INTEGER, PARAMETER:: NSOTYP=19 !!!!!!!!MAYBE???

        REAL :: PSIS(NSOTYP),BETA(NSOTYP),SMCMAX(NSOTYP)
        REAL :: STC(IMS:IME,NSOIL,JMS:JME), &
                SMC(IMS:IME,NSOIL,JMS:JME)
        REAL :: SH2O(IMS:IME,NSOIL,JMS:JME),SICE(IMS:IME,JMS:JME),&
                SM(IMS:IME,JMS:JME)
        REAL :: HLICE,GRAV,T0,BLIM
        INTEGER :: ISLTPK(IMS:IME,JMS:JME)
        CHARACTER(LEN=132) :: message

! Constants used in cold start SH2O initialization
      DATA HLICE/3.335E5/,GRAV/9.81/,T0/273.15/
      DATA BLIM/5.5/
!      DATA PSIS /0.04,0.62,0.47,0.14,0.10,0.26,0.14,0.36,0.04/
!      DATA BETA /4.26,8.72,11.55,4.74,10.73,8.17,6.77,5.25,4.26/
!      DATA SMCMAX /0.421,0.464,0.468,0.434,0.406, &
!                  0.465,0.404,0.439,0.421/

        
!!!      NOT SURE...PSIS=SATPSI, BETA=BB??

        DATA PSIS /0.069, 0.036, 0.141, 0.759, 0.759, 0.355,   &
                   0.135, 0.617, 0.263, 0.098, 0.324, 0.468,   &
                   0.355, 0.000, 0.069, 0.036, 0.468, 0.069, 0.069  /

        DATA BETA/2.79,  4.26,  4.74,  5.33,  5.33,  5.25,    &
                  6.66,  8.72,  8.17, 10.73, 10.39, 11.55,    &
                  5.25,  0.00,  2.79,  4.26, 11.55, 2.79, 2.79 /

        DATA SMCMAX/0.339, 0.421, 0.434, 0.476, 0.476, 0.439,  &
                    0.404, 0.464, 0.465, 0.406, 0.468, 0.468,  &
                    0.439, 1.000, 0.200, 0.421, 0.468, 0.200, 0.339/

        DO K=1,NSOIL
         DO J=JSTART,JM
          DO I=ISTART,IM
!tst
        IF (SMC(I,K,J) .gt. SMCMAX(ISLTPK(I,J))) then
  if (K .eq. 1) then
!    write(0,*) 'I,J,reducing SMC from ' ,I,J,SMC(I,K,J), 'to ', SMCMAX(ISLTPK(I,J))
  endif
        SMC(I,K,J)=SMCMAX(ISLTPK(I,J))
        ENDIF
!tst

        IF ( (SM(I,J) .lt. 0.5) .and. (SICE(I,J) .lt. 0.5) ) THEN

        IF (ISLTPK(I,J) .gt. 19) THEN
                WRITE(0,*) 'FORCING ISLTPK(a) at : ', I,J
                ISLTPK(I,J)=9
        ELSEIF (ISLTPK(I,J) .le. 0) then
                WRITE(0,*) 'FORCING ISLTPK(b) at : ', I,J
                ISLTPK(I,J)=1
        ENDIF


! cold start:  determine liquid soil water content (SH2O)
! SH2O <= SMC for T < 273.149K (-0.001C)

           IF (STC(I,K,J) .LT. 273.149) THEN

! first guess following explicit solution for Flerchinger Eqn from Koren
! et al, JGR, 1999, Eqn 17 (KCOUNT=0 in FUNCTION FRH2O).

              BX = BETA(ISLTPK(I,J))
              IF ( BETA(ISLTPK(I,J)) .GT. BLIM ) BX = BLIM

        if ( GRAV*(-PSIS(ISLTPK(I,J))) .eq. 0 ) then
        write(0,*) 'TROUBLE I,J, isltpk, psis: ', I,J, isltpk(I,J), psis(isltpk(I,J))
        endif

        if (BX .eq. 0 .or. STC(I,K,J) .eq. 0) then
                write(0,*) 'TROUBLE2 -- I,J,BX,SM, STC,SMC,SICE: ', I,J,BX,SM(I,J),STC(I,K,J),SMC(I,K,J),SICE(I,J)
        endif
              FK = (((HLICE/(GRAV*(-PSIS(ISLTPK(I,J)))))* &
                  ((STC(I,K,J)-T0)/STC(I,K,J)))** &
                  (-1/BX))*SMCMAX(ISLTPK(I,J))
              IF (FK .LT. 0.02) FK = 0.02
              SH2O(I,K,J) = MIN ( FK, SMC(I,K,J) )
! ----------------------------------------------------------------------
! now use iterative solution for liquid soil water content using
! FUNCTION FRH2O (from the Eta "NOAH" land-surface model) with the
! initial guess for SH2O from above explicit first guess.

              SH2O(I,K,J)=FRH2O_init(STC(I,K,J),SMC(I,K,J),SH2O(I,K,J), &
                         SMCMAX(ISLTPK(I,J)),BETA(ISLTPK(I,J)), &
                         PSIS(ISLTPK(I,J)))

            ELSE ! above freezing
              SH2O(I,K,J)=SMC(I,K,J)
            ENDIF

        ELSE   ! water point
              SH2O(I,K,J)=SMC(I,K,J)
        ENDIF ! test on land/ice/sea
        IF (SH2O(I,K,J) .gt. SMCMAX(ISLTPK(I,J))) THEN
          write(0,*) 'SH2O > THAN SMCMAX ', I,J,SH2O(I,K,J),SMCMAX(ISLTPK(I,J)),SMC(I,K,J)
        ENDIF

         ENDDO
        ENDDO
       ENDDO

        END SUBROUTINE NMM_SH2O

!-------------------------------------------------------------------

      FUNCTION FRH2O_init(TKELV,SMC,SH2O,SMCMAX,B,PSIS)

      IMPLICIT NONE

! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!  PURPOSE:  CALCULATE AMOUNT OF SUPERCOOLED LIQUID SOIL WATER CONTENT
!  IF TEMPERATURE IS BELOW 273.15K (T0).  REQUIRES NEWTON-TYPE ITERATION
!  TO SOLVE THE NONLINEAR IMPLICIT EQUATION GIVEN IN EQN 17 OF
!  KOREN ET AL. (1999, JGR, VOL 104(D16), 19569-19585).
! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
! New version (JUNE 2001): much faster and more accurate newton iteration
! achieved by first taking log of eqn cited above -- less than 4
! (typically 1 or 2) iterations achieves convergence.  Also, explicit
! 1-step solution option for special case of parameter Ck=0, which reduces
! the original implicit equation to a simpler explicit form, known as the
! ""Flerchinger Eqn". Improved handling of solution in the limit of
! freezing point temperature T0.
!
! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
! INPUT:
!
!   TKELV.........Temperature (Kelvin)
!   SMC...........Total soil moisture content (volumetric)
!   SH2O..........Liquid soil moisture content (volumetric)
!   SMCMAX........Saturation soil moisture content (from REDPRM)
!   B.............Soil type "B" parameter (from REDPRM)
!   PSIS..........Saturated soil matric potential (from REDPRM)
!
! OUTPUT:
!   FRH2O.........supercooled liquid water content.
! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      REAL B
      REAL BLIM
      REAL BX
      REAL CK
      REAL DENOM
      REAL DF
      REAL DH2O
      REAL DICE
      REAL DSWL
      REAL ERROR
      REAL FK
      REAL FRH2O_init
      REAL GS
      REAL HLICE
      REAL PSIS
      REAL SH2O
      REAL SMC
      REAL SMCMAX
      REAL SWL
      REAL SWLK
      REAL TKELV
      REAL T0

      INTEGER NLOG
      INTEGER KCOUNT
      PARAMETER (CK=8.0)
!      PARAMETER (CK=0.0)
      PARAMETER (BLIM=5.5)
!      PARAMETER (BLIM=7.0)
      PARAMETER (ERROR=0.005)

      PARAMETER (HLICE=3.335E5)
      PARAMETER (GS = 9.81)
      PARAMETER (DICE=920.0)
      PARAMETER (DH2O=1000.0)
      PARAMETER (T0=273.15)

!  ###   LIMITS ON PARAMETER B: B < 5.5  (use parameter BLIM)  ####
!  ###   SIMULATIONS SHOWED IF B > 5.5 UNFROZEN WATER CONTENT  ####
!  ###   IS NON-REALISTICALLY HIGH AT VERY LOW TEMPERATURES    ####
!  ################################################################
!
      BX = B
      IF ( B .GT. BLIM ) BX = BLIM
! ------------------------------------------------------------------

! INITIALIZING ITERATIONS COUNTER AND ITERATIVE SOLUTION FLAG.
      NLOG=0
      KCOUNT=0

! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! C  IF TEMPERATURE NOT SIGNIFICANTLY BELOW FREEZING (T0), SH2O = SMC
! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


      IF (TKELV .GT. (T0 - 1.E-3)) THEN

        FRH2O_init=SMC

      ELSE

! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
       IF (CK .NE. 0.0) THEN

! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! CCCCCCCCC OPTION 1: ITERATED SOLUTION FOR NONZERO CK CCCCCCCCCCC
! CCCCCCCCCCCC IN KOREN ET AL, JGR, 1999, EQN 17 CCCCCCCCCCCCCCCCC

! INITIAL GUESS FOR SWL (frozen content)
        SWL = SMC-SH2O
! KEEP WITHIN BOUNDS.
         IF (SWL .GT. (SMC-0.02)) SWL=SMC-0.02
         IF(SWL .LT. 0.) SWL=0.
! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! C  START OF ITERATIONS
! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        DO WHILE (NLOG .LT. 10 .AND. KCOUNT .EQ. 0)
         NLOG = NLOG+1
         DF = ALOG(( PSIS*GS/HLICE ) * ( ( 1.+CK*SWL )**2. ) * &
             ( SMCMAX/(SMC-SWL) )**BX) - ALOG(-(TKELV-T0)/TKELV)
         DENOM = 2. * CK / ( 1.+CK*SWL ) + BX / ( SMC - SWL )
         SWLK = SWL - DF/DENOM
! BOUNDS USEFUL FOR MATHEMATICAL SOLUTION.
         IF (SWLK .GT. (SMC-0.02)) SWLK = SMC - 0.02
         IF(SWLK .LT. 0.) SWLK = 0.
! MATHEMATICAL SOLUTION BOUNDS APPLIED.
         DSWL=ABS(SWLK-SWL)
         SWL=SWLK
! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! CC IF MORE THAN 10 ITERATIONS, USE EXPLICIT METHOD (CK=0 APPROX.)
! CC WHEN DSWL LESS OR EQ. ERROR, NO MORE ITERATIONS REQUIRED.
! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
         IF ( DSWL .LE. ERROR )  THEN
           KCOUNT=KCOUNT+1
         END IF
        END DO
! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! C  END OF ITERATIONS
! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! BOUNDS APPLIED WITHIN DO-BLOCK ARE VALID FOR PHYSICAL SOLUTION.
        FRH2O_init = SMC - SWL

! CCCCCCCCCCCCCCCCCCCCCCCC END OPTION 1 CCCCCCCCCCCCCCCCCCCCCCCCCCC

       ENDIF

       IF (KCOUNT .EQ. 0) THEN
!         Print*,'Flerchinger used in NEW version. Iterations=',NLOG

! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! CCCCC OPTION 2: EXPLICIT SOLUTION FOR FLERCHINGER EQ. i.e. CK=0 CCCCCCCC
! CCCCCCCCCCCCC IN KOREN ET AL., JGR, 1999, EQN 17  CCCCCCCCCCCCCCC

        FK=(((HLICE/(GS*(-PSIS)))*((TKELV-T0)/TKELV))**(-1/BX))*SMCMAX
! APPLY PHYSICAL BOUNDS TO FLERCHINGER SOLUTION
        IF (FK .LT. 0.02) FK = 0.02
        FRH2O_init = MIN ( FK, SMC )

! CCCCCCCCCCCCCCCCCCCCCCCCC END OPTION 2 CCCCCCCCCCCCCCCCCCCCCCCCCC

       ENDIF

      ENDIF

        RETURN

      END FUNCTION FRH2O_init


!--------------------------------------------------------------------

END MODULE assorted_definitions
