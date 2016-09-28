MODULE vinterp_routines

   USE module_data
   USE parallel_module

   INTEGER :: internal_time_loop

CONTAINS

!-------------------------------------------------------------------


	SUBROUTINE vinterp_driver(grid , gridout, w,  metgrid_levels_in, lnsh, lnsv )

	implicit none

          TYPE(input_vars), INTENT(INOUT) :: grid
          TYPE(output_vars), INTENT(INOUT) :: gridout
          TYPE(work_vars)                  :: w

        REAL, ALLOCATABLE, DIMENSION(:) :: Y2S, Pvec, Qvec, XOLD, DOLD, Z1D, P1D
        REAL, ALLOCATABLE, DIMENSION(:) :: DFL, DFRLG, eta_levels

        REAL, ALLOCATABLE,DIMENSION(:,:):: ADUM2D,SNOWC,HT,TG_ALT, &
                                           PDVP,PSFC_OUTV,ALBBCK,SHDMIN,SHDMAX, &
                                           PSFC_OUT,HGT_HOLD

         REAL  :: dummypint(grid%IMS:grid%IME,gridout%LM+1,&
                                             grid%JMS:grid%JME)

!        REAL, ALLOCATABLE,DIMENSION(:,:,:):: RWMR_input, CLWMR_input, &
!                                             SNMR_input, CICE_input, RIMEF_input

!        INTEGER, ALLOCATABLE, DIMENSION(:):: KHL2,KVL2,KHH2,KVH2, &
!                                             KHLA,KHHA,KVLA,KVHA

!        REAL, ALLOCATABLE, DIMENSION(:):: DXJ,WPDARJ,CPGFUJ,CURVJ, &
!                                          FCPJ,FDIVJ,EMJ,EMTJ,FADJ, &
!                                          HDACJ,DDMPUJ,DDMPVJ

         REAL, ALLOCATABLE:: DXV(:)
         REAL             :: DYV

	INTEGER :: num_metgrid_levels, kold, icount
        INTEGER :: ITS, ITE, JTS, JTE, KTS, KTE
        INTEGER :: IDS, IDE, JDS, JDE, KDS, KDE
        INTEGER :: IMS, IME, JMS, JME, KMS, KME
        INTEGER :: I,J,K,ILOOK,JLOOK, L, metgrid_levels_in

        INTEGER:: numsoil, LNSH,LNSV

        LOGICAL:: hyb_coor, GLOBAL, direct_temp, ncep_processing
        LOGICAL:: print_it, auto_def, spectral, wrt_water

        REAL, SAVE :: PDTOP

        REAL, PARAMETER :: DEGRAD=57.2957795
        REAL, PARAMETER :: G=9.81
!        REAL, PARAMETER :: pt=5000.
!        REAL, PARAMETER :: ptsgm=42000.

 	REAL  :: pt, ptsgm

	ncep_processing=gridout%ncep_processing
	direct_temp=gridout%direct_temp
        GLOBAL=gridout%global 
        numsoil=grid%numsoil
        spectral=gridout%spectral

        num_metgrid_levels = metgrid_levels_in

	if (spectral) num_metgrid_levels = num_metgrid_levels - 1

!!
	ITS=grid%ITS
	ITE=grid%ITE
	IDS=grid%IDS
	IDE=grid%IDE
	IMS=grid%IMS
	IME=grid%IME
	JTS=grid%JTS
	JTE=grid%JTE
	JDS=grid%JDS
	JDE=grid%JDE
	JMS=grid%JMS
	JME=grid%JME

        if (its .eq. 1 .and. jts .eq. 1) then 
          print_it=.true.
        else
          print_it=.false.
        endif

        if (print_it) then
        write(0,*) 'num_metgrid_levels start of vinterp_driver: ', num_metgrid_levels
        write(0,*) 'metgrid_levels_in start of vinterp_driver: ', metgrid_levels_in
        endif

        KDS=1
	KDE=gridout%LM+1
        KTS=KDS
	KTE=gridout%LM+1
        KMS=KDS
	KME=gridout%LM

	grid%KDS=KDS
	grid%KDE=KDE

!	if (grid%first_time) then
	allocate(Y2S(num_metgrid_levels+2))
	allocate(Pvec(num_metgrid_levels+2))
	allocate(Qvec(num_metgrid_levels+2))
	allocate(XOLD(1:num_metgrid_levels))
	allocate(DOLD(1:num_metgrid_levels))
	allocate(Z1D(KDS:KDE))
	allocate(P1D(KDS:KDE))
        ALLOCATE(DFL(KDS:KDE))
        ALLOCATE(DFRLG(KDS:KDE))
        ALLOCATE(eta_levels(KDS:KDE))
!	endif

!!! allocate target NEMS arrays

	IF (.not. ALLOCATED(gridout%PD)) THEN

           ALLOCATE(gridout%ETA1(KTS:KTE)) !mod
           ALLOCATE(gridout%ETA2(KTS:KTE)) !mod
           ALLOCATE(gridout%AETA1(KMS:KME)) !mod
           ALLOCATE(gridout%AETA2(KMS:KME)) !mod
           ALLOCATE(gridout%DETA1(KMS:KME)) !mod
           ALLOCATE(gridout%DETA2(KMS:KME)) !mod
           ALLOCATE(gridout%NL_ETALEVS(KTS:KTE)) !mod
!           ALLOCATE(gridout%DZSOIL(KTE)) ; gridout%DZSOIL=0.
!           ALLOCATE(gridout%SLDPTH(KTE))
           ALLOCATE(gridout%DZSOIL(grid%numsoil)) ; gridout%DZSOIL=0.
           ALLOCATE(gridout%SLDPTH(grid%numsoil))

           ALLOCATE(gridout%TSK(IMS:IME,JMS:JME))
           ALLOCATE(gridout%SM(IMS:IME,JMS:JME))
           ALLOCATE(gridout%SICE(IMS:IME,JMS:JME))
           ALLOCATE(gridout%WEASD(IMS:IME,JMS:JME))
           ALLOCATE(gridout%FIS(IMS:IME,JMS:JME))
           ALLOCATE(gridout%PD(IMS:IME,JMS:JME))
           ALLOCATE(gridout%STDVTOPO(IMS:IME,JMS:JME))
           ALLOCATE(gridout%VEGFRA(IMS:IME,JMS:JME))
           ALLOCATE(gridout%ALBEDO(IMS:IME,JMS:JME))
           ALLOCATE(gridout%ALBASE(IMS:IME,JMS:JME))
           ALLOCATE(gridout%MXSNAL(IMS:IME,JMS:JME))
           ALLOCATE(gridout%T(IMS:IME,JMS:JME,KMS:KME))
           ALLOCATE(gridout%Q(IMS:IME,JMS:JME,KMS:KME)) ; gridout%Q=0.
           ALLOCATE(gridout%CWM(IMS:IME,JMS:JME,KMS:KME))
           ALLOCATE(gridout%U(IMS:IME,JMS:JME,KMS:KME))
           ALLOCATE(gridout%V(IMS:IME,JMS:JME,KMS:KME))
           allocate(gridout%pint_out(IMS:IME,KDS:KDE,JMS:JME))

           allocate(w%P3D_OUT(IMS:IME,KDS:KDE-1,JMS:JME))
           allocate(w%model_Z(IMS:IME,KDS:KDE,JMS:JME))
           allocate(w%P3DV_OUT(IMS:IME,KDS:KDE-1,JMS:JME))
           allocate(w%P3DV_IN(IMS:IME,JMS:JME,num_metgrid_levels))
           ALLOCATE(w%qtmp(IMS:IME,num_metgrid_levels,JMS:JME))
           ALLOCATE(w%qtmp2(IMS:IME,JMS:JME,num_metgrid_levels))

           ALLOCATE(w%SNMR_input(IMS:IME,JMS:JME,KMS:KME))
           ALLOCATE(w%CLWMR_input(IMS:IME,JMS:JME,KMS:KME))
           ALLOCATE(w%CICE_input(IMS:IME,JMS:JME,KMS:KME))
           ALLOCATE(w%RWMR_input(IMS:IME,JMS:JME,KMS:KME))
           ALLOCATE(w%rimef_input(IMS:IME,JMS:JME,KMS:KME))

!	   ALLOCATE(gridout%GLAT(IMS:IME,JMS:JME))
!	   ALLOCATE(gridout%GLON(IMS:IME,JMS:JME))

           gridout%CWM=0.
           gridout%SM=0.
           gridout%SICE=0.
           gridout%STDVTOPO=0.

	ENDIF

!	write(0,*) 'size(w%p3d_out): ', size(w%p3d_OUT,DIM=1),size(w%p3d_out,dim=2),size(w%p3d_out,dim=3)
!	write(0,*) 'size(w%model_z): ', size(w%model_z,DIM=1),size(w%model_z,dim=2),size(w%model_z,dim=3)
!	write(0,*) 'size(w%qtmp2): ', size(w%qtmp2,DIM=1),size(w%qtmp2,dim=2),size(w%qtmp2,dim=3)

!!! end allocate

	IF (grid%GHT(its,jts,10) .lt. grid%GHT(its,jts,11)) THEN
           if (print_it) write(0,*) 'normal ground up file order'
           hyb_coor=.false.
        ELSE
           hyb_coor=.true.
           if (print_it) write(0,*) 'reverse the order of coordinate'
           CALL reverse_vert_coord(grid%GHT, 2, num_metgrid_levels &
     &,                            IDS,IDE,JDS,JDE,KDS,KDE        &
     &,                            IMS,IME,JMS,JME,KMS,KME        &
     &,                            ITS,ITE,JTS,JTE,KTS,KTE ) 

           CALL reverse_vert_coord(grid%PRES, 2, num_metgrid_levels &
     &,                            IDS,IDE,JDS,JDE,KDS,KDE        &
     &,                            IMS,IME,JMS,JME,KMS,KME        &
     &,                            ITS,ITE,JTS,JTE,KTS,KTE ) 

           CALL reverse_vert_coord(grid%TT, 2, num_metgrid_levels &
     &,                            IDS,IDE,JDS,JDE,KDS,KDE        &
     &,                            IMS,IME,JMS,JME,KMS,KME        &
     &,                            ITS,ITE,JTS,JTE,KTS,KTE ) 

           CALL reverse_vert_coord(grid%UU, 2, num_metgrid_levels &
     &,                            IDS,IDE,JDS,JDE,KDS,KDE        &
     &,                            IMS,IME,JMS,JME,KMS,KME        &
     &,                            ITS,ITE,JTS,JTE,KTS,KTE ) 

           CALL reverse_vert_coord(grid%VV, 2, num_metgrid_levels &
     &,                            IDS,IDE,JDS,JDE,KDS,KDE        &
     &,                            IMS,IME,JMS,JME,KMS,KME        &
     &,                            ITS,ITE,JTS,JTE,KTS,KTE ) 

           CALL reverse_vert_coord(grid%RH, 2, num_metgrid_levels &
     &,                            IDS,IDE,JDS,JDE,KDS,KDE        &
     &,                            IMS,IME,JMS,JME,KMS,KME        &
     &,                            ITS,ITE,JTS,JTE,KTS,KTE ) 

        ENDIF 


	IF (hyb_coor) THEN 
                           ! limit extreme deviations from source model topography
                           ! due to potential for nasty extrapolation/interpolation issues
                           !
          if (print_it) write(0,*) 'min, max of grid%HGT_M before adjust: ', minval(grid%HGT_M), maxval(grid%HGT_M)
          ICOUNT=0
          DO J=JTS,min(JTE,JDE-1)
           DO I=ITS,min(ITE,IDE-1)
             IF ((grid%HGT_M(I,J) - grid%GHT(I,J,2)) .LT. -150.) THEN
               grid%HGT_M(I,J)=grid%GHT(I,J,2)-150.
               IF (ICOUNT .LT. 20) THEN
                 write(0,*) 'increasing NMM topo toward RUC  ', I,J
                 ICOUNT=ICOUNT+1
               ENDIF
             ELSEIF ((grid%HGT_M(I,J) - grid%GHT(I,J,2)) .GT. 150.) THEN
               grid%HGT_M(I,J)=grid%GHT(I,J,2)+150.
               IF (ICOUNT .LT. 20) THEN
                 write(0,*) 'decreasing NMM topo toward RUC ', I,J
                 ICOUNT=ICOUNT+1
               ENDIF
             ENDIF
           END DO
          END DO
          if (print_it) write(0,*) 'min, max of grid%HGT_M after correct: ', minval(grid%HGT_M), maxval(grid%HGT_M)
        ENDIF

	IF ( .not. GLOBAL .and. .not. ncep_processing) THEN
         if (print_it) write(0,*) 'HGT_M extremes into boundary_smooth: ', minval(grid%HGT_M),maxval(grid%HGT_M)
	

	  ALLOCATE(HGT_HOLD(IDS:IDE-1,JDS:JDE-1))

          DO J=JTS,min(JTE,JDE-1)
           DO I=ITS,min(ITE,IDE-1)
           HGT_HOLD(I,J)=grid%HGT_M(I,J)
	if (HGT_HOLD(I,J) .gt. 6000.) then
	write(0,*) 'bad HGT...I,J, HGT_HOLD(I,J): ', I,J, HGT_HOLD(I,J)
	endif

           ENDDO
          ENDDO

      CALL boundary_smooth(grid%HGT_M,grid%landmask, grid, 12 , 12 &
     &,                          IDS,IDE,JDS,JDE,KDS,KDE             &
     &,                          IMS,IME,JMS,JME,KMS,KME             &
     &,                          ITS,ITE,JTS,JTE,KTS,KTE ) 

	icount=0
          DO J=JTS,min(JTE,JDE-1)
           DO I=ITS,min(ITE,IDE-1)
           if ( abs( HGT_HOLD(I,J) - grid%HGT_M(I,J)) .gt. 150.) then
             icount=icount+1
	     if (icount .lt. 10) then
             write(0,*) 'smoothing changed topo from : ',I,J, HGT_HOLD(I,J), ' to: ', grid%HGT_M(I,J) 
	     endif
           endif
           ENDDO
          ENDDO

	write(0,*) 'boundary topography smoothing changed: ', icount, ' points by 150+ meters'

!	write(0,*) 'HGT_M extremes out of boundary_smooth: ', minval(grid%HGT_M),maxval(grid%HGT_M)


	DEALLOCATE(HGT_HOLD)
        ENDIF



!	write(0,*) 'into definition block1'
!	write(0,*) 'size(grid%TT):: ', size(grid%TT,dim=1),size(grid%TT,dim=2),size(grid%TT,dim=3)

       DO j = jts, MIN(jte,jde-1)
         DO i = its, MIN(ite,ide-1)
           if (grid%LANDMASK(I,J) .eq. 1.0) gridout%SM(I,J)=0.
           if (grid%LANDMASK(I,J) .eq. 0.0) gridout%SM(I,J)=1.
	if (grid%SKINTEMP(I,J) .gt. 0.) then
	   gridout%TSK(I,J)=grid%SKINTEMP(I,J)
        else
           gridout%TSK(I,J)=grid%TT(I,J,1) ! stopgap measure
        endif
!
!           gridout%GLAT(I,J)=grid%XLAT_M(I,J)*DEGRAD
!           gridout%GLON(I,J)=grid%XLONG_M(I,J)*DEGRAD

           gridout%WEASD(I,J)=grid%SNOW(I,J)
           gridout%SICE(I,J)=grid%SEAICE(I,J)

	if (I .eq. 1 .and. J .eq. 1) then
	write(0,*) 'xlat_m(1,1), xlong_m(1,1):: ', grid%xlat_m(1,1),grid%xlong_m(1,1)
	endif

         ENDDO
       ENDDO


! First item is to define the target vertical coordinate

!     num_metgrid_levels = grid%num_metgrid_levels

      eta_levels(1:kde) = gridout%coord_levs(1:kde)

!     ptsgm             = model_config_rec%ptsgm
!     p_top_requested = grid%p_top_requested
!     pt=p_top_requested

	pt=gridout%PT
	ptsgm=gridout%PTSGM

        if (print_it) write(0,*) 'pt, ptsgm NOT FROM PARAMETER:: ', pt, ptsgm

	if (grid%first_time) then


!!!! NEED SOMETHING TO INDICATE IT IS THE FIRST TIME

        if (gridout%vcoord .eq. 0 .and. eta_levels(1) .ne. 0.0) then
          write(0,*) '*************************************** '
          write(0,*) '**  eta_levels appears not to be specified in the'
          write(0,*) '**  namelist.  We will call compute_nmm_levels to'
          write(0,*) '**  define layer thicknesses.  These levels should'
          write(0,*) '**  be reasonable for running the model, but may'
          write(0,*) '**  not be ideal for the simulation being made.  '
          write(0,*) '** Consider defining your own levels by specifying'
          write(0,*) '**  eta_levels in the namelist.  '
          write(0,*) '**************************************** '

          CALL compute_nmm_levels(KDE,pt,eta_levels)

          DO L=1,KDE
            write(0,*) 'L, eta_levels(L) returned :: ', L,eta_levels(L)
          ENDDO

        endif


!!!!!  branch for different coordinate options

        if (gridout%vcoord .ne. 0 .and. eta_levels(1) .eq. 0.0) then
           auto_def=.false.
        if (print_it) write(0,*) 'auto_def is false'
        else
        if (print_it) write(0,*) 'auto_def is true'
           auto_def=.true.
        endif

      CALL define_nmm_vertical_coord (kde-1, ptsgm,gridout%LPT2, pt,pdtop, eta_levels, &
                                      gridout%ETA1,gridout%DETA1,gridout%AETA1,             &
                                      gridout%ETA2,gridout%DETA2,gridout%AETA2, DFL, DFRLG, &
                                      gridout%vcoord, print_it, auto_def    ) 

        if (print_it) write(0,*) 'pt, pdtop: ', pt, pdtop

	gridout%pdtop=pdtop

	gridout%nl_etalevs(1:KDE)=eta_levels(1:KDE)

!       DO L=KDS,KDE-1
!        DETA(L)=eta_levels(L)-eta_levels(L+1)
!       ENDDO

	endif


        if (.NOT. allocated(PDVP))     allocate(PDVP(IMS:IME,JMS:JME))
        if (.NOT. allocated(PSFC_OUTV)) allocate(PSFC_OUTV(IMS:IME,JMS:JME))
        if (.NOT. allocated(PSFC_OUT)) allocate(PSFC_OUT(IMS:IME,JMS:JME))

        
       DO j = jts, MIN(jte,jde-1)
         DO i = its, MIN(ite,ide-1)
           gridout%FIS(I,J)=grid%HGT_M(I,J)*g
!
	IF ( grid%PRES(I,J,1) .ne. 200100. .AND.  (grid%HGT_M(I,J) .eq. grid%GHT(I,J,1)) .AND. grid%HGT_M(I,J) .ne. 0) THEN
	  IF (mod(I,10) .eq. 0 .and. mod(J,10) .eq. 0) THEN
	    write(0,*) 'grid%HGT_M and grid%SOILHGT to swap  ::: ', &
                          I,J, grid%HGT_M(I,J),grid%SOILHGT(I,J)
          ENDIF
!                IF ( ( flag_soilhgt.EQ. 1 ) ) THEN 
                 grid%GHT(I,J,1)=grid%SOILHGT(I,J)
!                ENDIF
        ENDIF

         ENDDO
       ENDDO

        Ilook=105
	Jlook=15

	if (num_metgrid_levels .eq. 0) STOP


!	write(0,*) 'HGT_M into surfacep ', ITS, ITE, JTS, JTE
!	do J=JTE,JTS,min(-(JTE-JTS)/15,-1)
!	write(0,533) J,(grid%HGT_M(I,J),I=ITS,ITE,max(1,(ITE-ITS)/10))
!	enddo
  533	format('HGT_M: ',I4,15(f5.0,1x))

	if (.not. spectral) then
      CALL compute_nmm_surfacep (grid%HGT_M, grid%GHT, grid%PRES , grid%TT               &
     &,                          psfc_out, num_metgrid_levels  &
     &,                          IDS,IDE,JDS,JDE,KDS,KDE             &
     &,                          IMS,IME,JMS,JME,KMS,KME             &
     &,                          ITS,ITE,JTS,JTE,KTS,KTE,gridout%spectral,Ilook,Jlook,print_it ) ! H points
       else
      CALL compute_nmm_surfacep (grid%HGT_M, grid%GHT, grid%PINT , grid%TT               &
     &,                          psfc_out, num_metgrid_levels  &
     &,                          IDS,IDE,JDS,JDE,KDS,KDE             &
     &,                          IMS,IME,JMS,JME,KMS,KME             &
     &,                          ITS,ITE,JTS,JTE,KTS,KTE,gridout%spectral,Ilook,Jlook,print_it ) ! H points
       endif


	if (.not. global .and. (PT .eq. 0 .or. PDTOP .eq. 0)) then
	write(0,*) 'bad PT or PDTOP into compute_3d_pressure: ', PT, PDTOP
	STOP
	endif

        if (print_it) write(0,*) 'ITS, JTS, psfc_out extremes: ', ITS, JTS, minval(psfc_out),maxval(psfc_out)


      CALL compute_3d_pressure (psfc_out,gridout%AETA1,gridout%AETA2   &
     &,            gridout%ETA1,gridout%ETA2                           &
     &,            pdtop,pt,gridout%pd,w%p3d_out,gridout%pint_out        &
     &,            IDS,IDE,JDS,JDE,KDS,KDE             &
     &,            IMS,IME,JMS,JME,KMS,KME             &
     &,            ITS,ITE,JTS,JTE,KTS,KTE, gridout%vcoord, print_it )


	
        if (print_it) then
	if (grid%gtype .ne. 'A') then
	write(0,*) 'at PDVP definition'
	write(0,*) 'JTE, JDE-1: ', JTE, JDE-1
	write(0,*) 'ITE, IDE-1: ', ITE, IDE-1
        else
        write(0,*) 'will interpolate winds on mass points for A grid'
        endif
        endif

         call exchange_halo_r(psfc_out, &
                              IMS, IME, JMS, JME, 1, 1, &
                              ITS, ITE, JTS, JTE, 1, 1)

         call exchange_halo_r(gridout%PD, &
                              IMS, IME, JMS, JME, 1, 1, &
                              ITS, ITE, JTS, JTE, 1, 1)

         call exchange_halo_r(grid%PRES, &
                              IMS, IME, JMS, JME, 1, num_metgrid_levels, &
                              ITS, ITE, JTS, JTE, 1, num_metgrid_levels)


!!! AGRID

	if (grid%gtype .ne. 'A') then

       do K=1,num_metgrid_levels
	do J=JTS,min(JTE,JDE-1)
         do I=ITS,min(ITE,IDE-1)

         IF (K .eq. KTS) THEN

! believe that on B grid nearly all PDVP definitions can be treated the same

            IF (J .lt. JDE-1 .and. I .lt. IDE-1) THEN
              PDVP(I,J) = 0.25*(gridout%PD(I,J)+gridout%PD(I+1,J)+gridout%PD(I,J+1)+gridout%PD(I+1,J+1))
              PSFC_OUTV(I,J)= 0.25*( PSFC_OUT(I,J)+PSFC_OUT(I+1,J)+ &
                                     PSFC_OUT(I,J+1)+PSFC_OUT(I+1,J+1) )
            ELSEIF (J .eq. JDE-1 .and. I .ne. IDE-1) THEN
              PDVP(I,J) = 0.5*(gridout%PD(I,J)+gridout%PD(I+1,J))
              PSFC_OUTV(I,J) = 0.5*(PSFC_OUT(I,J)+PSFC_OUT(I+1,J))
            ELSEIF (I .eq. IDE-1) THEN
              PDVP(I,J) = gridout%PD(I,J) 
              PSFC_OUTV(I,J) = PSFC_OUT(I,J) 
            ENDIF
          ENDIF
	
            IF (J .lt. JDE-1 .and. I .lt. IDE-1) THEN
              w%P3DV_IN(I,J,K)=0.25*( grid%PRES(I,J,K)+grid%PRES(I+1,J,K) + &
                                    grid%PRES(I,J+1,K)+grid%PRES(I+1,J+1,K) )
            ELSEIF (J .eq. JDE-1 .and. I .ne. IDE-1) THEN
              w%P3DV_IN(I,J,K)=0.5*(grid%PRES(I,J,K)+grid%PRES(I+1,J,K))
            ELSEIF (I .eq. IDE-1) THEN
              w%P3DV_IN(I,J,K)=grid%PRES(I,J,K)
            ENDIF

         enddo
        enddo
       enddo

	endif

!!! AGRID

        if (print_it) write(0,*) 'past loops, direct_temp : ', direct_temp

!	if (ITE .eq. IDE-1 .and. JTE .eq. JDE-1) then
!	write(0,*) 'PSFC_OUTV(IDE-1,JDE-1), PDVP, w%P3DV_IN: ', PSFC_OUTV(IDE-1,JDE-1), PDVP(IDE-1,JDE-1), w%p3dv_in(IDE-1,JDE-1,5)
!	endif

	if (direct_temp) then

!	if (ITS .eq. 1 .and. JTS .eq. 1) then
!	do K=1,num_metgrid_levels
!	write(0,*) 'K, grid%PRES, grid%TT: ', K, grid%PRES(Ilook,Jlook,K),grid%TT(Ilook,Jlook,K)
!	enddo
!	endif


	if (spectral) then
      CALL interp_press2press_lin(grid%PRES, w%p3d_out        &
     &,            grid%TT, gridout%T,num_metgrid_levels-1          &
     &,            .TRUE.,.FALSE.,.TRUE.               & ! extrap, ignore_lowest, t_field
     &,            IDS,IDE,JDS,JDE,KDS,KDE             &
     &,            IMS,IME,JMS,JME,KMS,KME             &
     &,            ITS,ITE,JTS,JTE,KTS,KTE, grid%first_time, LNSH )
        else
      CALL interp_press2press_lin(grid%PRES, w%p3d_out        &
     &,            grid%TT, gridout%T,num_metgrid_levels          &
     &,            .TRUE.,.TRUE.,.TRUE.               & ! extrap, ignore_lowest, t_field
     &,            IDS,IDE,JDS,JDE,KDS,KDE             &
     &,            IMS,IME,JMS,JME,KMS,KME             &
     &,            ITS,ITE,JTS,JTE,KTS,KTE, grid%first_time, LNSH )
        endif

	if (ITS .le. Ilook .and. ITE .ge. Ilook .and. JTS .le. Jlook .and. JTE .ge. Jlook) then
	write(0,*) 'past interp_press2press_lin again'
	do K=KDS,KDE-1
	write(0,*) 'K, w%p3d_out, T_direct: ',K,w%p3d_out(Ilook,K,Jlook),gridout%T(Ilook,Jlook,K)
	enddo
	endif


	else

	if (ITS .le. Ilook .and. ITE .ge. Ilook .and. JTS .le. Jlook .and. JTE .ge. Jlook) then
	do L=1,num_metgrid_levels
	write(0,*) 'L, grid%GHT,grid%PRES, T: ', L, grid%GHT(Ilook,Jlook,L),grid%PRES(Ilook,Jlook,L),grid%TT(Ilook,Jlook,L)
	enddo
        endif


	Y2S=0.
	Pvec=0.
	Qvec=0.


       DO J=JTS,min(JTE,JDE-1)
         DO I=ITS,min(ITE,IDE-1)

	 do L=KDS,KDE
	 p1d(L)=gridout%pint_out(I,KDE-L+1,J)
	if ( I .eq. Ilook  .and. J .eq. Jlook) then
	write(0,*) 'Lout, p1d(L): ', I,J,L, p1d(L)
	endif
	 enddo

	IF ( (grid%GHT(I,J,2)-grid%HGT_M(I,J)) .gt. 1.0) THEN

	if ( (p1d(1)-grid%PRES(I,J,2)) .gt. 500.) then ! big gap, define extra level
        kold=num_metgrid_levels
	else
	kold=num_metgrid_levels-1
	endif

	 do L=2,num_metgrid_levels
	if (.not. spectral) then
	 xold(L-1)=grid%PRES(I,J,num_metgrid_levels-L+2)
	else
	 xold(L-1)=grid%PINT(I,J,num_metgrid_levels-L+2)
        endif
         dold(L-1)=grid%GHT(I,J,num_metgrid_levels-L+2)
         if ( (I .eq. Ilook .or. I .eq. Ilook+1) .and. J .eq. Jlook) then
	    write(0,*) 'I,J,L-1, xold, dold: ', I,J,L-1, xold(L-1), dold(L-1), KOLD
         endif
         enddo

!       basically use computed surface pressure as another data point

	dold(kold)=grid%HGT_M(I,J)
!mp	xold(kold)=gridout%pint_out(I,1,J)
	xold(kold)=p1d(1)
	if ( (I .eq. Ilook .or. I .eq. Ilook+1) .and. J .eq. Jlook) then
	write(0,*) 'I,J,L, xold, dold: ', I,J,kold, xold(kold),dold(kold),KOLD
	endif

        CALL spline(kold,xold,dold,y2s,KDE,p1d,z1d,pvec,qvec)

        ELSE

	kold=num_metgrid_levels-1

	 do L=2,num_metgrid_levels
	 xold(L-1)=grid%PRES(I,J,num_metgrid_levels-L+2)
         dold(L-1)=grid%GHT(I,J,num_metgrid_levels-L+2)
	if ( (I .eq. Ilook .or. I .eq. Ilook+1) .and. J .eq. Jlook) then
	  write(0,*) 'I,J, L-1, xold, dold: ', I,J,L-1, xold(L-1), dold(L-1), KOLD
        endif
	enddo

        CALL spline(kold,xold,dold,y2s,KDE,p1d,z1d,pvec,qvec)

	endif

	 do L=KDS,KDE
	 w%model_Z(I,L,J)=z1d(KDE-L+1)

!	if ( (I .eq. Ilook .or. I .eq. Ilook+1) .and. J .eq. Jlook) then
!	write(0,*) 'I,J,L,w%model_Z(I,J),gridout%pint_out(I,J): ',I,J,L, w%model_Z(I,L,J),gridout%pint_out(I,L,J)
!	endif

	 enddo

	 ENDDO
       ENDDO

	

!	do L=KDS,KDE
!	write(0,*) 'L, w%model_Z(1,L,1),gridout%pint_out(1,L,1): ', L, w%model_Z(1,L,1),gridout%pint_out(1,L,1)
!	enddo

!!! NOTE:  model_Z has interface heights

      CALL heights_to_temps(w%model_Z, gridout%T, gridout%pint_out,w%p3d_out         &
     &,            IDS,IDE,JDS,JDE,KDS,KDE             &
     &,            IMS,IME,JMS,JME,KMS,KME             &
     &,            ITS,ITE,JTS,JTE,KTS,KTE,Ilook,Jlook )

!	write(0,*) 'gridout%T(1,1,1) out of heights_to_temps: ', gridout%T(1,1,1)


! bogus a top level temperature
	if (PT .eq. 0.) then
	do J=JTS,min(JTE,JDE-1)
         do I=ITS,min(ITE,IDE-1)
	 gridout%T(I,J,1)=gridout%T(I,J,2)+5.
         enddo
        enddo
        endif


	endif

	deallocate(Y2S,Pvec,Qvec,XOLD,DOLD,Z1D,P1D, DFL, DFRLG, eta_levels)


!!! AGRID
	if (grid%gtype .ne. 'A') then
!        if (print_it) then
!	write(0,*) 'ITS, JTS, psfc_outv extremes: ', ITS, JTS, minval(psfc_outv),maxval(psfc_outv)
!	write(0,*) 'INTO COMPUTE_3D_PRESSURE for V points'
!	endif
      CALL compute_3d_pressure (psfc_outv,gridout%AETA1,gridout%AETA2   &
     &,            gridout%ETA1,gridout%ETA2                           &
     &,            pdtop,pt,pdvp,w%p3dv_out,dummypint   &  
     &,            IDS,IDE,JDS,JDE,KDS,KDE             &
     &,            IMS,IME,JMS,JME,KMS,KME             &
     &,            ITS,ITE,JTS,JTE,KTS,KTE,gridout%vcoord, print_it )
        endif

!!! AGRID



       if (internal_time_loop .eq. 1 .and. ITS .le. Ilook .and. ITE .ge. Ilook & 
             .and. JTS .le. Jlook .and. JTE .ge. Jlook) then
        do L=1,num_metgrid_levels-1
	write(0,*) 'L, w%p3dv_in, grid%UU,grid%VV: ', L, w%p3dv_in(Ilook,Jlook,L),grid%UU(Ilook,Jlook,L),grid%VV(Ilook,Jlook,L)
        enddo
        endif


!!! AGRID
	if (grid%gtype .ne. 'A') then

        if (.not. spectral) then
      CALL interp_press2press_lin(w%p3dv_in, w%p3dv_out       &
     &,            grid%UU, gridout%U,num_metgrid_levels            &
     &,            .FALSE.,.TRUE.,.FALSE.              &
     &,            IDS,IDE,JDS,JDE,KDS,KDE             &
     &,            IMS,IME,JMS,JME,KMS,KME             &
     &,            ITS,ITE,JTS,JTE,KTS,KTE, grid%first_time, LNSV )

      CALL interp_press2press_lin(w%p3dv_in, w%p3dv_out         &
     &,            grid%VV, gridout%V,num_metgrid_levels        &
     &,            .FALSE.,.TRUE.,.FALSE.              &
     &,            IDS,IDE,JDS,JDE,KDS,KDE             &
     &,            IMS,IME,JMS,JME,KMS,KME             &
     &,            ITS,ITE,JTS,JTE,KTS,KTE, grid%first_time, LNSV )
       else

      CALL interp_press2press_lin(w%p3dv_in, w%p3dv_out        &
     &,            grid%UU, gridout%U,num_metgrid_levels-1          &
     &,            .FALSE.,.FALSE.,.FALSE.              &
     &,            IDS,IDE,JDS,JDE,KDS,KDE             &
     &,            IMS,IME,JMS,JME,KMS,KME             &
     &,            ITS,ITE,JTS,JTE,KTS,KTE, grid%first_time, LNSV )

      CALL interp_press2press_lin(w%p3dv_in, w%p3dv_out        &
     &,            grid%VV, gridout%V,num_metgrid_levels-1          &
     &,            .FALSE.,.FALSE.,.FALSE.              &
     &,            IDS,IDE,JDS,JDE,KDS,KDE             &
     &,            IMS,IME,JMS,JME,KMS,KME             &
     &,            ITS,ITE,JTS,JTE,KTS,KTE, grid%first_time, LNSV )
       endif

       endif

	if (.not. global .and. grid%gtype .eq. 'B') then

	if (gridout%boundary_flux) then
	call bgrid_boundary_winds(gridout%pd, gridout%U, gridout%V, &
     &                            w%p3dv_in,  grid%UU, grid%VV,     &
     &                            gridout%DETA1, gridout%DETA2,pdtop,pt,     &
     &                            IDS,IDE,JDS,JDE,KDS,KDE,          &
     &                            IMS,IME,JMS,JME,KMS,KME,          &
     &                            ITS,ITE,JTS,JTE,KTS,KTE,LNSV,num_metgrid_levels,print_it )

	endif

	if (gridout%no_flux) then
	call bgrid_boundary_zeroflux(gridout%pd, gridout%U, gridout%V, &
     &                            gridout%DETA1, gridout%DETA2,pdtop,pt,     &
     &                            IDS,IDE,JDS,JDE,KDS,KDE,          &
     &                            IMS,IME,JMS,JME,KMS,KME,          &
     &                            ITS,ITE,JTS,JTE,KTS,KTE)
	endif

	endif


!!! AGRID
	if (internal_time_loop .eq. 1 .and. ITS .le. Ilook .and. ITE .ge. Ilook &
             .and. JTS .le. Jlook .and. JTE .ge. Jlook) then
       do L=KTS,KTE-1
	write(0,*) 'L, w%p3dv_out, U,V: ', L, w%p3dv_out(Ilook,L,Jlook),&
                           gridout%U(Ilook,L,Jlook),gridout%V(Ilook,L,Jlook)
       end do
        endif


       IF (hyb_coor) THEN
!	write(0,*) 'SKIPPING wind_adjust'

!!! AGRID
	if (grid%gtype .ne. 'A') then
       CALL wind_adjust(w%p3dv_in,w%p3dv_out,grid%UU,grid%VV,gridout%U,gridout%V  &
     &,                 num_metgrid_levels,5000.        &
     &,                 IDS,IDE,JDS,JDE,KDS,KDE           &
     &,                 IMS,IME,JMS,JME,KMS,KME           &
     &,                 ITS,ITE,JTS,JTE,KTS,KTE )
        endif
!!! AGRID

	if (internal_time_loop .eq. 1) then
       do L=KTS,KTE-1
	write(0,*) 'post adjust L, w%p3dv_out, U,V: ', L, w%p3dv_out(Ilook,L,Jlook),gridout%U(Ilook,L,Jlook),gridout%V(Ilook,L,Jlook)
       end do
         endif
       ENDIF


	 if ( .not. GLOBAL) THEN

!!! AGRID
	if (grid%gtype .ne. 'A') then
        IF (   abs(IDE-1-ITE) .le. 1 ) THEN ! along eastern boundary
          write(0,*) 'zero phantom winds at IDE-1:', IDE-1
          DO K=1,KDE-1
            DO J=JDS,JDE-1   ! every row for B-grid
              IF (J .ge. JTS .and. J .le. JTE) THEN
                gridout%U(IDE-1,J,K)=0.
                gridout%V(IDE-1,J,K)=0.
              ENDIF
            ENDDO
          ENDDO 
        ENDIF
        endif
!!! AGRID

        ENDIF

!         IF ( flag_qv .NE. 1 ) THEN

        if (.not. spectral) then

!	 IF (num_metgrid_levels .eq. 27) then ! assume GFS (imperfect)
!         wrt_water = .false.
! 	 ELSE
         wrt_water = .true.
!         ENDIF

            CALL rh_to_mxrat (grid%RH, grid%TT, grid%PRES, w%qtmp , wrt_water , &
                        ids , ide , jds , jde , 1 , num_metgrid_levels , &
                        ims , ime , jms , jme , 1 , num_metgrid_levels , &
                        its , ite , jts , jte , 1 , num_metgrid_levels )



       do K=1,num_metgrid_levels
	do J=JTS,min(JTE,JDE-1)
         do I=ITS,min(ITE,IDE-1)
           w%QTMP2(I,J,K)=w%QTMP(I,K,J)/(1.0+w%QTMP(I,K,J))
         end do
        end do
       end do

      CALL interp_press2press_log(grid%PRES, w%p3d_out        &
     &,            w%QTMP2, gridout%Q,num_metgrid_levels          &
     &,            .FALSE.,.TRUE.                      &
     &,            IDS,IDE,JDS,JDE,KDS,KDE             &
     &,            IMS,IME,JMS,JME,KMS,KME             &
     &,            ITS,ITE,JTS,JTE,KTS,KTE, grid%first_time, LNSH )

       else

      CALL interp_press2press_log(grid%PRES, w%p3d_out        &
     &,            grid%SPECHUMD, gridout%Q,num_metgrid_levels-1          &
     &,            .FALSE.,.FALSE.                      &
     &,            IDS,IDE,JDS,JDE,KDS,KDE             &
     &,            IMS,IME,JMS,JME,KMS,KME             &
     &,            ITS,ITE,JTS,JTE,KTS,KTE, grid%first_time, LNSH )

      endif


	if (maxval(grid%CLWMR) .gt. 0. .AND. gridout%do_clouds) then

!   assume have fields defined to create an initial CWM
!
!   allocate the *_input fields

	write(0,*) 'DOING CLOUDS'
!
!        ALLOCATE(RWMR_input(IMS:IME,JMS:JME,KMS:KME))
!        ALLOCATE(CLWMR_input(IMS:IME,JMS:JME,KMS:KME))
!        ALLOCATE(SNMR_input(IMS:IME,JMS:JME,KMS:KME))
!        ALLOCATE(CICE_input(IMS:IME,JMS:JME,KMS:KME))
!        ALLOCATE(RIMEF_input(IMS:IME,JMS:JME,KMS:KME))

	if (maxval(grid%FRIMEF) .gt. 1.e-12) then
      CALL interp_press2press_lin(grid%PRES, w%p3d_out        &
     &,            grid%FRIMEF, w%rimef_input,num_metgrid_levels          &
     &,            .false.,.TRUE.,.false.               & ! extrap, ignore_lowest, t_field
     &,            IDS,IDE,JDS,JDE,KDS,KDE             &
     &,            IMS,IME,JMS,JME,KMS,KME             &
     &,            ITS,ITE,JTS,JTE,KTS,KTE, grid%first_time, LNSH )
           else
	write(0,*) 'max input FRIMEF tiny, set w%rimef_input to zero'
	w%rimef_input=0.
        endif

!----------------------------------------------
	write(0,*) 'maxval RWMR: ', maxval(grid%RWMR)
	if (maxval(grid%RWMR) .gt. 1.e-12) then
      CALL interp_press2press_log(grid%PRES, w%p3d_out        &
     &,            grid%RWMR, w%rwmr_input,num_metgrid_levels          &
     &,            .false.,.TRUE.               & ! extrap, ignore_lowest
     &,            IDS,IDE,JDS,JDE,KDS,KDE             &
     &,            IMS,IME,JMS,JME,KMS,KME             &
     &,            ITS,ITE,JTS,JTE,KTS,KTE, grid%first_time, LNSH )
	write(0,*) 'maxval rwmr_input: ', maxval(w%rwmr_input)
           else
	write(0,*) 'max input RWMR tiny, set w%rwmr_input to zero'
	w%rwmr_input=0.
        endif


!----------------------------------------------
	write(0,*) 'maxval SNMR: ', maxval(grid%SNMR)
	if (maxval(grid%SNMR) .gt. 1.e-12) then
      CALL interp_press2press_log(grid%PRES, w%p3d_out        &
     &,            grid%SNMR, w%snmr_input,num_metgrid_levels          &
     &,            .false.,.TRUE.               & ! extrap, ignore_lowest
     &,            IDS,IDE,JDS,JDE,KDS,KDE             &
     &,            IMS,IME,JMS,JME,KMS,KME             &
     &,            ITS,ITE,JTS,JTE,KTS,KTE, grid%first_time, LNSH )
	write(0,*) 'maxval snmr_input: ', maxval(w%snmr_input)
           else
	write(0,*) 'max input SNMR tiny, set w%snmr_input to zero'
	w%snmr_input=0.
	endif

!----------------------------------------------
	write(0,*) 'maxval CICE: ', maxval(grid%CICE)
        if (maxval(grid%CICE) .gt. 1.e-12) then
      CALL interp_press2press_log(grid%PRES, w%p3d_out        &
     &,            grid%CICE, w%cice_input,num_metgrid_levels          &
     &,            .false.,.TRUE.               & ! extrap, ignore_lowest
     &,            IDS,IDE,JDS,JDE,KDS,KDE             &
     &,            IMS,IME,JMS,JME,KMS,KME             &
     &,            ITS,ITE,JTS,JTE,KTS,KTE, grid%first_time, LNSH )
	write(0,*) 'maxval cice_input: ', maxval(w%cice_input)
        else
	write(0,*) 'max input CICE tiny, set w%cice_input to zero'
	w%cice_input=0.
	endif

!----------------------------------------------
	write(0,*) 'maxval CLWMR: ', maxval(grid%CLWMR)
        if (maxval(grid%CLWMR) .gt. 1.e-12) then
      CALL interp_press2press_log(grid%PRES, w%p3d_out        &
     &,            grid%CLWMR, w%clwmr_input,num_metgrid_levels          &
     &,            .false.,.TRUE.               & ! extrap, ignore_lowest
     &,            IDS,IDE,JDS,JDE,KDS,KDE             &
     &,            IMS,IME,JMS,JME,KMS,KME             &
     &,            ITS,ITE,JTS,JTE,KTS,KTE, grid%first_time, LNSH )
	else
	write(0,*) 'max input CLWMR tiny, set w%clwmr_input to zero'
	w%clwmr_input=0.
	endif



	write(0,*) 'J limits CWM definition: ', JTS,min(JDE-1,JTE)
	write(0,*) 'K limits CWM definition: ', KDS, KDE-1
	write(0,*) 'I limits CWM definition: ', ITS,min(IDE-1,ITE)

       do K=KDS,KDE-1
        do J=JTS,min(JDE-1,JTE)
          do I=ITS,min(IDE-1,ITE)

! cloud stuff
! prevent negative values

        IF (w%CLWMR_input(I,J,K) .lt. 1.e-11) w%CLWMR_input(I,J,K)=0.
!         CLWMR_input(I,J,K)=0.
        IF (w%SNMR_input(I,J,K) .lt. 1.e-11) w%SNMR_input(I,J,K)=0.
        IF (w%RWMR_input(I,J,K) .lt. 1.e-11) w%RWMR_input(I,J,K)=0.
        IF (w%CICE_input(I,J,K) .lt. 1.e-11) w%CICE_input(I,J,K)=0.
! prevent negative values

        gridout%CWM(I,J,K)= w%SNMR_input(I,J,K) + w%CLWMR_input(I,J,K) + &
                            w%RWMR_input(I,J,K) + w%CICE_input(I,J,K)

!	if (I .eq. 883 .and. J .eq. 433) then
!	write(0,*) 'K, SNMR, CLWMR, RWMR, CICE...CWM: ', K, SNMR_input(I,J,K),  CLWMR_input(I,J,K), RWMR_input(I,J,K), CICE_input(I,J,K), gridout%CWM(I,J,K)
!	endif

        if (gridout%CWM(I,J,K) .lt. 0 .or. gridout%CWM(I,J,K) .gt. 50.e-2) then
        write(0,*) 'strange CWM...I,J,K,CWM: ', I,J,K,gridout%CWM(I,J,K)
        endif

          enddo
         enddo
        enddo

	write(0,*) 'maxval(CWM): ', maxval(gridout%CWM)
        else   ! not clouds

       do K=KDS,KDE-1
        do J=JTS,min(JDE-1,JTE)
          do I=ITS,min(IDE-1,ITE)
           gridout%CWM(I,J,K)=0.
          enddo
        enddo
       enddo


	endif

	if (.NOT. direct_temp) then

!! devirtualize temperatures
         do K=KDS,KDE-1
	do J=JTS,min(JTE,JDE-1)
          do I=ITS,min(ITE,IDE-1)
	if (I .eq. Ilook .and. J .eq. Jlook) then
	write(0,*) 'K, virtual T:: ', K, gridout%T(I,J,K), gridout%Q(I,J,K)
	endif
           gridout%T(I,J,K)=gridout%T(I,J,K)/(1.0+0.608*gridout%Q(I,J,K))

	if (I .eq. Ilook .and. J .eq. Jlook) then
	write(0,*) 'K, devirtualized T:: ', K, gridout%T(I,J,K)
	endif

          enddo
         enddo
        enddo

       endif

!	write(0,*) 'gridout%q(1,1,1): ', gridout%q(1,1,1)
!	write(0,*) 'gridout%t(1,1,1): ', gridout%t(1,1,1)


!         END IF  ! flav_qv

         !  Get the monthly values interpolated to the current date 
         !  for the traditional monthly
         !  fields of green-ness fraction and background albedo.

!WRF
         CALL monthly_interp_to_date ( grid%greenfrac , grid%current_date//'.0000' , gridout%vegfra , &
                                      ids , ide , jds , jde , kds , kde , &
                                      ims , ime , jms , jme , kms , kme , &
                                      its , ite , jts , jte , kts , kte )
         if ( ncep_processing ) then
	do J=JTS,min(JTE,JDE-1)
          do I=ITS,min(ITE,IDE-1)
          gridout%vegfra(I,J)=gridout%vegfra(I,J)/100.
          enddo
        enddo
         endif

	allocate(albbck(IMS:IME,JMS:JME)) ; albbck=0.
	allocate(shdmin(IMS:IME,JMS:JME))
	allocate(shdmax(IMS:IME,JMS:JME))

!WRF
	write(0,*) 'grid%current_date: ', grid%current_date
         if ( ncep_processing ) then

         CALL quarterly_interp_to_date( grid%albedo12m , grid%current_date//'.0000' , albbck , &
                                       ids , ide , jds , jde , kds , kde , &
                                       ims , ime , jms , jme , kms , kme , &
                                       its , ite , jts , jte , kts , kte )
         if (print_it) then
          write(0,*) 'min,max of albbck from quarterly interp: ', minval(albbck),maxval(albbck)
         endif

         else

         CALL monthly_interp_to_date ( grid%albedo12m , grid%current_date//'.0000' , albbck , &
                                       ids , ide , jds , jde , kds , kde , &
                                       ims , ime , jms , jme , kms , kme , &
                                       its , ite , jts , jte , kts , kte )
         if (print_it) then
          write(0,*) 'min,max of albbck from monthly interp: ', minval(albbck),maxval(albbck)
         endif

         endif

         !  Get the min/max of each i,j for the monthly green-ness fraction.

!WRF
         CALL monthly_min_max ( grid%greenfrac , shdmin , shdmax , &
                                ids , ide , jds , jde , kds , kde , &
                                ims , ime , jms , jme , kms , kme , &
                                its , ite , jts , jte , kts , kte )

         !  The model expects the green-ness values in percent, not fraction.

         DO j = jts, MIN(jte,jde-1)
           DO i = its, MIN(ite,ide-1)
!!              vegfra(i,j) = vegfra(i,j) * 100.
              shdmax(i,j) = shdmax(i,j) * 100.
              shdmin(i,j) = shdmin(i,j) * 100.
!              VEGFRC(I,J)=VEGFRA(I,J)
           END DO
         END DO

         !  The model expects the albedo fields as 
         !  a fraction, not a percent.  Set the water values to 8%.

         DO j = jts, MIN(jte,jde-1)
           DO i = its, MIN(ite,ide-1)
              if (albbck(i,j) .lt. 5.) then
!                  write(0,*) 'reset albedo to 8%...  I,J,albbck:: ', I,J,albbck(I,J)
                  albbck(I,J)=8.
              endif
              albbck(i,j) = albbck(i,j) / 100.
              grid%snoalb(i,j) = grid%snoalb(i,j) / 100.
              IF ( grid%landmask(i,j) .LT. 0.5 ) THEN
                 albbck(i,j) = 0.08
                 grid%snoalb(i,j) = 0.08
              END IF

              gridout%albase(i,j)=albbck(i,j)

!	if (mod(I,10) .eq. 0 .and. mod(J,10) .eq. 0) then
!	write(0,*) 'I, J, gridout%albedo(I,J): ', I, J, gridout%albedo(I,J)
!	endif
              gridout%mxsnal(i,j)=grid%snoalb(i,j)
           END DO
         END DO

!  new deallocs
        deallocate(albbck,shdmin,shdmax)

!	write(0,*) 'gridout%q(1,1,1) into check on temps: ', gridout%q(1,1,1)
!	write(0,*) 'gridout%t(1,1,1) into check on temps: ', gridout%t(1,1,1)

	if (grid%first_time) then

!!! check on temps
        DO l = kds, kde-1
         DO j = jts, MIN(jte,jde-1)
           DO i = its, MIN(ite,ide-1)
	     if (gridout%T(I,J,L) .lt. 180.) then
			write(0,*) 'modifying to 180 at I,J,L: ', I,J,L, gridout%T(I,J,L)
			gridout%T(I,J,L)=180.
	     endif
	     if (gridout%T(I,J,L) .gt. 330.) then
			write(0,*) 'modifying to 330 at I,J,L: ', I,J,L, gridout%T(I,J,L)
			gridout%T(I,J,L)=330.
	     endif

	if (I .eq. 1 .and. J .eq. 1) then
	write(0,*) 'I,J,L, P3DV_OUT, U(I,J,L),V(I,J,L): ', I,J,L, w%P3DV_OUT(I,L,J),gridout%U(I,J,L),gridout%V(I,J,L)	
	endif

           ENDDO
         ENDDO
        ENDDO


	endif

!	write(0,*) 'gridout%q(1,1,1) end vert interp: ', gridout%q(1,1,1)
        if (print_it) write(0,*) 'end of vertical interpolation'

	END SUBROUTINE vinterp_driver


!------------------------------------------------------


  SUBROUTINE define_nmm_vertical_coord ( LM, PTSGM, LPT2, PT, PDTOP,HYBLEVS, &
                                         SG1,DSG1,SGML1,         &
                                         SG2,DSG2,SGML2,DFL, DFRLG, vcoord_opt, print_it, auto_define  )

        IMPLICIT NONE

!        USE module_model_constants

!!! certain physical parameters here probably don't need to be defined, as defined
!!! elsewhere within WRF.  Done for initial testing purposes.

        INTEGER ::  LM, L, vcoord_opt
        INTEGER, intent(out):: LPT2
        REAL    ::  PTSGM, PT, PL, PT2, PDTOP
        REAL    ::  RGOG, PSIG,PHYB,PHYBM
        REAL, PARAMETER  :: Rd           =  287.04  ! J deg{-1} kg{-1}
        REAL, PARAMETER :: CP=1004.6,GAMMA=.0065,PRF0=101325.,T0=288.
        REAL, PARAMETER :: g=9.81

        REAL, DIMENSION(LM)   :: DSG,DSG1,DSG2
        REAL, DIMENSION(LM)   :: SGML1,SGML2
        REAL, DIMENSION(LM+1) :: SG1,SG2,HYBLEVS,DFL,DFRLG

        LOGICAL :: print_it, auto_define

!	LOGICAL, parameter:: PRINT_IT=.true.

        CHARACTER(LEN=132)    :: message


	if (VCOORD_OPT .eq. 0) then

!        LPT2=LM+1
         LPT2=1
       

        write(0,*) 'pt= ', pt

!        DO L=LM+1,1,-1
        DO L=1,LM+1
          pl=HYBLEVS(L)*(101325.-pt)+pt
          if(pl.lt.ptSGm) LPT2=l
        ENDDO

      IF(LPT2.gt.1) THEN
        pt2=HYBLEVS(LPT2)*(101325.-pt)+pt
      ELSE
        pt2=pt
      ENDIF

      write(0,*) '*** Sigma system starts at ',pt2,' Pa, from level ',LPT2

      pdtop=pt2-pt

        DSG=-99.

      DO L=1,LM
        DSG(L)=HYBLEVS(L+1)-HYBLEVS(L)
      ENDDO

        DSG1=0.
        DSG2=0.

!      DO L=LM,1,-1
      DO L=1,LM

       IF(L.LT.LPT2) then
        DSG1(L)=DSG(L)
       ELSE
        DSG2(L)=DSG(L)
       ENDIF

      ENDDO

        SGML1=-99.
        SGML2=-99.

!       IF(LPT2.le.LM+1) THEN
       IF(LPT2.gt.1) THEN

!        DO L=LM+1,LPT2,-1
        DO L=1,LPT2
        SG2(L)=0.
        ENDDO

       DO L=LPT2,LM
        SG2(L+1)=SG2(L)+DSG2(L)
       ENDDO
	write(0,*) 'SG2(LM+1): ', SG2(LM+1)

        DO L=LPT2+1,LM
	write(0,*) 'L, SG2 was: ', L, SG2(L)
        SG2(L)=SG2(L)/SG2(LM+1)
	write(0,*) 'L, SG2 post renormalize: ', L, SG2(L)
        ENDDO 
        SG2(LM+1)=1.

       DO L=LPT2+1,LM
        DSG2(L)=SG2(L+1)-SG2(L)
        SGML2(l)=(SG2(l)+SG2(l+1))*0.5
       ENDDO

      ENDIF

      DO L=1,LPT2
        DSG2(L)=0.
        SGML2(L)=0.
      ENDDO

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        SG1(1)=0.

      DO L=1,LPT2
       SG1(L+1)=SG1(L)+DSG1(L)
      ENDDO

      DO L=1,LPT2
       SG1(L)=SG1(L)/SG1(LPT2+1)
      ENDDO

        SG1(LPT2+1)=1.

       do l=LPT2+2,LM+1
        SG1(l)=1.
       enddo


      DO L=1,LPT2-1
       DSG1(L)=SG1(L+1)-SG1(L)
       SGML1(L)=(SG1(L)+SG1(L+1))*0.5
      ENDDO

      DO L=LPT2,LM+1
               DSG1(L)=0.
               SGML1(L)=1.
      ENDDO

 1000 format('l,hyblevs,psig,SG1,SG2,phyb,phybm')
 1100 format(' ',i4,f7.4,f10.2,2f7.4,2f10.2)

      write(0,1000)

      do l=1,LM+1
        psig=HYBLEVS(L)*(101325.-pt)+pt
        phyb=SG1(l)*pdtop+SG2(l)*(101325.-pdtop-pt)+pt
        if(l.lt.LM+1) then
          phybm=SGML1(l)*pdtop+SGML2(l)*(101325.-pdtop-pt)+pt
        else
          phybm=-99.
        endif

        write(0,1100) l,HYBLEVS(L),psig &
                      ,SG1(l),SG2(l),phyb,phybm
      enddo


  632   format(f9.6)

       write(0,*) 'SG1'
       do L=1,LM+1
       write(0,632) SG1(L)
       enddo

       write(0,*) 'SG2'
       do L=1,LM+1
       write(0,632) SG2(L)
       enddo

       write(0,*) 'DSG1'
       do L=1,LM
       write(0,632) DSG1(L)
       enddo

       write(0,*) 'DSG2'
       do L=1,LM
       write(0,632) DSG2(L)
       enddo

       write(0,*) 'SGML1'
       do L=1,LM
       write(0,632) SGML1(L)
       enddo

       write(0,*) 'SGML2'
       do L=1,LM
       write(0,632) SGML2(L)
       enddo

      rgog=(rd*gamma)/g
      DO L=1,LM+1
        DFL(L)=g*T0*(1.-((pt+SG1(L)*pdtop+SG2(L)*(101325.-pt2)) &
                       /101325.)**rgog)/gamma
        DFRLG(L)=DFL(L)/g
!       write(0,*) 'L, DFL(L): ', L, DFL(L)
      ENDDO

	elseif (VCOORD_OPT .eq. 1) then
        call vcgenerator(LM,pt,ptsgm,pdtop,lpt2,hyblevs,sg1,dsg1,sgml1,sg2,dsg2,sgml2, &
                         print_it, auto_define)
	elseif (VCOORD_OPT .eq. 2) then
        call gfsgenerator(LM,pt,ptsgm,pdtop,lpt2,hyblevs,sg1,dsg1,sgml1,sg2,dsg2,sgml2, &
                          print_it, auto_define)
	elseif (VCOORD_OPT .eq. 3) then
        call salgenerator(LM,pt,ptsgm,pdtop,lpt2,hyblevs,sg1,dsg1,sgml1,sg2,dsg2,sgml2, &
                          print_it, auto_define)
	else
	write(0,*) 'UNKNOWN VERTICAL COORDINATE NUMBER: ' , VCOORD_OPT
	STOP
	endif

        write(0,*) 'end define_nmm_vertical_coord with LPT2: ', LPT2

  END SUBROUTINE define_nmm_vertical_coord

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE compute_nmm_surfacep ( TERRAIN_HGT_T, Z3D_IN, PRESS3D_IN, T3D_IN   &
     &,                             psfc_out,generic           & 
     &,                             IDS,IDE,JDS,JDE,KDS,KDE             &
     &,                             IMS,IME,JMS,JME,KMS,KME             &
     &,                             ITS,ITE,JTS,JTE,KTS,KTE,spectral, Ilook,Jlook, print_diag )

	
       IMPLICIT NONE

       real, allocatable:: dum2d(:,:),DUM2DB(:,:)
       
       integer :: IDS,IDE,JDS,JDE,KDS,KDE
       integer :: IMS,IME,JMS,JME,KMS,KME
       integer :: ITS,ITE,JTS,JTE,KTS,KTE,Ilook,Jlook
       integer :: I,J,II,generic,L,KINSERT,K,bot_lev,LL
       integer :: IHE(JMS:JME),IHW(JMS:JME), loopinc,iloopinc
	
       real :: TERRAIN_HGT_T(IMS:IME,JMS:JME)
       real :: Z3D_IN(IMS:IME,JMS:JME,generic)
       real :: T3D_IN(IMS:IME,JMS:JME,generic)
       real :: PRESS3D_IN(IMS:IME,JMS:JME,generic)
       real :: PSFC_IN(IMS:IME,JMS:JME),TOPO_IN(IMS:IME,JMS:JME)
       real :: psfc_out(IMS:IME,JMS:JME),rincr(IMS:IME,JMS:JME)
       real :: dif1,dif2,dif3,dif4,dlnpdz,BOT_INPUT_HGT,BOT_INPUT_PRESS,dpdz,rhs
       real :: zin(generic),pin(generic)

       character (len=132) :: message
	
       logical :: DEFINED_PSFC(IMS:IME,JMS:JME), DEFINED_PSFCB(IMS:IME,JMS:JME), spectral
       logical, intent(in) :: print_diaG

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!	write(0,*) 'size(TERRAIN_HGT_T):: ', size(TERRAIN_HGT_T,dim=1),size(TERRAIN_HGT_T,dim=2)
!	write(0,*) 'what are JMS, JME here??? : ', JMS, JME
!	write(0,*) 'JTS, min(JTE,JDE-1): ', JTS, min(JTE,JDE-1)

       DO j = JMS, JME
          IHE(J)=MOD(J+1,2)
          IHW(J)=IHE(J)-1
       ENDDO

       DO J=JMS,JME
       DO I=IMS,IME
          DEFINED_PSFC(I,J)=.FALSE.
          DEFINED_PSFCB(I,J)=.FALSE.
        IF (PRESS3D_IN(I,J,1) .ne. 200100.) THEN
          PSFC_IN(I,J)=PRESS3D_IN(I,J,1)
          TOPO_IN(I,J)=Z3D_IN(I,J,1)
        ELSE
          PSFC_IN(I,J)=PRESS3D_IN(I,J,2)
          TOPO_IN(I,J)=Z3D_IN(I,J,2)
        ENDIF
       ENDDO
       ENDDO

        if (print_diag) then
	write(0,*) 'terrain_hgt_t in surfacep compute ', IMS,IME,JMS,JME
	do J=JME,JMS,min(-(JME-JMS)/20,-1)
	write(0,535) J,(TERRAIN_HGT_T(I,J),I=IMS,IME,max(1,(IME-IMS)/12))
	enddo
        endif

        if (print_diag) then
	write(0,*) 'z3d_in(3) at same points:'
	do J=JME,JMS,min(-(JME-JMS)/20,-1)
	write(0,535) J,(Z3D_IN(I,J,3),I=IMS,IME,max(1,(IME-IMS)/12))
	enddo
        endif

  535	format(I4,' ::: ',18(f5.0,1x))

! input surface pressure smoothing over the ocean - still needed for NAM?

        II_loop: do II=1,8

        CYCLE II_loop   ! avoiding it

	do J=JTS+1,min(JTE,JDE-1)-1
         do I=ITS+1,min(ITE,IDE-1)-1
         rincr(I,J)=0.

       if (PSFC_IN(I,J) .gt. 100000.          .and. &
           PSFC_IN(I+IHE(J),J+1) .gt. 100000. .and. &
           PSFC_IN(I+IHE(J),J-1) .gt. 100000. .and. &
           PSFC_IN(I+IHW(J),J+1) .gt. 100000. .and. &
           PSFC_IN(I+IHW(J),J-1) .gt. 100000. ) then

       dif1=abs(PSFC_IN(I,J)-PSFC_IN(I+IHE(J),J+1))
       dif2=abs(PSFC_IN(I,J)-PSFC_IN(I+IHE(J),J-1))
       dif3=abs(PSFC_IN(I,J)-PSFC_IN(I+IHW(J),J+1))
       dif4=abs(PSFC_IN(I,J)-PSFC_IN(I+IHW(J),J-1))

        if (max(dif1,dif2,dif3,dif4) .lt. 200. .and. TOPO_IN(I,J).le. 0.5 .and. &
            TOPO_IN(I+IHE(J),J+1) .le. 0.5 .and. &
            TOPO_IN(I+IHW(J),J+1) .le. 0.5 .and. &
            TOPO_IN(I+IHE(J),J-1) .le. 0.5 .and. &
            TOPO_IN(I+IHW(J),J-1) .lt. 0.5) then

        rincr(I,J)=0.125*( 4.*PSFC_IN(I,J)+ &
                            PSFC_IN(I+IHE(J),J+1)+PSFC_IN(I+IHE(J),J-1)+ &
                            PSFC_IN(I+IHW(J),J+1)+PSFC_IN(I+IHW(J),J-1) ) &
                          - PSFC_IN(I,J)

!        if (rincr(I,J) .ne. 0 .and. abs(rincr(I,J)) .gt. 20.) then
!          write(0,*) 'II, I,J,rincr: ', II, I,J,rincr(I,J)
!        endif

         endif
         endif

        ENDDO
        ENDDO

       DO J=JTS+1,min(JTE,JDE-1)-1
         DO I=ITS+1,min(ITE,IDE-1)-1
           PSFC_IN(I,J)=PSFC_IN(I,J) + rincr(I,J)
         ENDDO
       ENDDO

!        write(0,*) ' -------------------------------------------------- '

         end do II_loop

       ALLOCATE(DUM2D(IMS:IME,JMS:JME))
     
       DO J=JMS,JME
        DO I=IMS,IME
         DUM2D(I,J)=-9.
        END DO
       END DO

       DO J=JTS,min(JTE,JDE-1)
        I_loop: DO I=ITS,min(ITE,IDE-1)

         IF (PSFC_IN(I,J) .eq. 0.) THEN
           write(0,*) 'QUITTING BECAUSE I,J, PSFC_IN: ', I,J,PSFC_IN(I,J)

	STOP
         ENDIF

         BOT_INPUT_PRESS=PSFC_IN(I,J)
         BOT_INPUT_HGT=TOPO_IN(I,J)


        IF (I .eq. Ilook .AND. J .eq. Jlook) THEN

!	   write(0,*) ' TERRAIN_HGT_T: ', I,J, TERRAIN_HGT_T(I,J)
	   write(0,*) ' PSFC_IN, TOPO_IN: ', &
                            I, J, PSFC_IN(I,J),TOPO_IN(I,J)

           DO L=1,generic
	     write(0,*) ' L,PRESS3D_IN, Z3D_IN: ', &
                             I,J,L, PRESS3D_IN(I,J,L),Z3D_IN(I,J,L)
           END DO
         ENDIF

  short_loop:    DO L=1,generic-1
        IF ( PRESS3D_IN(i,j,L) .gt. PSFC_IN(I,J)   .AND.  &
             Z3D_IN(I,J,L) .lt. TERRAIN_HGT_T(I,J) .AND.  &
             Z3D_IN(I,J,L+1) .gt. TERRAIN_HGT_T(I,J) ) THEN
!
          BOT_INPUT_PRESS=PRESS3D_IN(i,j,L)
          BOT_INPUT_HGT=Z3D_IN(I,J,L)
          EXIT short_loop
         ENDIF
      END DO short_loop

!!!!!!!!!!!!!!!!!!!!!! START HYDRO CHECK

	if (.not. spectral) then

       IF ( PRESS3D_IN(i,j,1) .ne. 200100. .AND. &
          (PSFC_IN(I,J) .gt. PRESS3D_IN(i,j,2) .OR. &
           TOPO_IN(I,J) .lt. Z3D_IN(I,J,2)) ) THEN        ! extrapolate downward

         IF (J .eq. JTS .AND. I .eq. ITS) THEN
            if (print_diag) write(0,*) 'hydro check - should only be for isobaric input'
         ENDIF

	 IF (Z3D_IN(I,J,2) .ne. TOPO_IN(I,J)) THEN
           dpdz=(PRESS3D_IN(i,j,2)-PSFC_IN(I,J))/(Z3D_IN(I,J,2)-TOPO_IN(I,J))
           rhs=-9.81*((PRESS3D_IN(i,j,2)+ PSFC_IN(I,J))/2.)/(287.04* T3D_IN(I,J,2))

	   IF ( abs(PRESS3D_IN(i,j,2)-PSFC_IN(I,J)) .gt. 290.) THEN
             IF (dpdz .lt. 1.05*rhs .OR. dpdz .gt. 0.95*rhs) THEN
!                IF (mod(I,5).eq.0 .AND. mod(J,5).eq.0) THEN
!                    write(0,*) 'I,J,P(2),Psfc,Z(2),Zsfc: ', &
!                    I,J,PRESS3D_IN(i,j,2),PSFC_IN(I,J),Z3D_IN(I,J,2),TOPO_IN(I,J)
!                ENDIF
	      CYCLE I_loop
             ENDIF 

           ENDIF 

         ELSE ! z(2) equals TOPO_IN

	  IF (PRESS3D_IN(i,j,2) .eq. PSFC_IN(I,J)) THEN
!	    write(0,*) 'all equal at I,J: ', I,J
          ELSE
!           write(0,*) 'heights equal, pressures not: ', &
!                           PRESS3D_IN(i,j,2), PSFC_IN(I,J)
	    CYCLE I_loop
	  ENDIF

         ENDIF
       
         IF ( abs(PRESS3D_IN(i,j,2)-PSFC_IN(I,J)) .gt. 290.) THEN
           IF (PRESS3D_IN(i,j,2) .lt. PSFC_IN(I,J) .and. &
                          Z3D_IN(I,J,2) .lt. TOPO_IN(I,J)) THEN
            write(0,*) 'surface data mismatch(a) at I,J: ', I,J
	     CYCLE I_loop
           ELSEIF (PRESS3D_IN(i,j,2) .gt. PSFC_IN(I,J) .AND.  &
                  Z3D_IN(I,J,2) .gt. TOPO_IN(I,J)) THEN
             write(0,*) 'surface data mismatch(b) at I,J: ', I,J
             CYCLE I_loop
           ENDIF
         ENDIF 
       ENDIF

!!!!!!! loop over a few more levels

        DO L=3,6
          IF ( PRESS3D_IN(i,j,1) .ne. 200100. .AND. &
             (((PSFC_IN(I,J)-PRESS3D_IN(i,j,L)) .lt. 400.) .OR. &
               TOPO_IN(I,J) .lt. Z3D_IN(I,J,L))) then
                 
	    IF (Z3D_IN(I,J,L) .ne. TOPO_IN(I,J)) THEN
              dpdz=(PRESS3D_IN(i,j,L)-PSFC_IN(I,J))/ &
                   (Z3D_IN(I,J,L)-TOPO_IN(I,J))
              rhs=-9.81*((PRESS3D_IN(i,j,L)+ PSFC_IN(I,J))/2.)/ &
                        (287.04*T3D_IN(I,J,L))
              IF ( abs(PRESS3D_IN(i,j,L)-PSFC_IN(I,J)) .gt. 290.) THEN
                IF (dpdz .lt. 1.05*rhs .or. dpdz .gt. 0.95*rhs) THEN
!                  IF (mod(I,5).eq.0 .AND. mod(J,5).eq.0) THEN
!                  write(0,*) 'I,J,L,Piso,Psfc,Ziso,Zsfc: ', &
!                                    I,J,L,PRESS3D_IN(i,j,L),PSFC_IN(I,J),&
!                                    Z3D_IN(I,J,L),TOPO_IN(I,J)
!		  ENDIF
	          CYCLE I_loop
                ENDIF 
              ENDIF
            ELSE
	      IF (PRESS3D_IN(i,j,2) .eq. PSFC_IN(I,J)) THEN
!	        write(0,*) 'all equal at I,J: ', I,J
              ELSE 
	        CYCLE I_loop
              ENDIF
            ENDIF
          ENDIF

	  IF ( abs(PRESS3D_IN(i,j,L)-PSFC_IN(I,J)) .gt. 290.) THEN
            IF (PRESS3D_IN(i,j,L) .lt. PSFC_IN(I,J) .AND. &
                    Z3D_IN(I,J,L) .lt. TOPO_IN(I,J)) THEN
              CYCLE I_loop
            ELSEIF (PRESS3D_IN(i,j,L) .gt. PSFC_IN(I,J) .AND.  &
                    Z3D_IN(I,J,L) .gt. TOPO_IN(I,J)) THEN
             CYCLE I_loop
            ENDIF 
          ENDIF 
        END DO

	endif ! spectral

!!!!!!!!!!!!!!!!!!!!!! END HYDRO CHECK

           IF (I .eq. Ilook .and. J .eq. Jlook) THEN
	write(0,*) 'enter this section, TERRAIN_HGT_T, BOT_INPUT_HGT: ', TERRAIN_HGT_T(I,J), BOT_INPUT_HGT
           ENDIF

        IF (TERRAIN_HGT_T(I,J) .eq. BOT_INPUT_HGT ) THEN
           dum2d(I,J)=BOT_INPUT_PRESS
           DEFINED_PSFC(I,J)=.TRUE.
           IF (I .eq. Ilook .and. J .eq. Jlook) THEN
	   write(0,*) 'TERRAIN_HGT_T .eq. BOT_INPUT_HGT, set dum2d to: ', I,J, dum2d(I,J)
           ENDIF

	  IF (BOT_INPUT_HGT .ne. 0. .and. (BOT_INPUT_HGT-INT(BOT_INPUT_HGT) .ne. 0.) ) THEN
	    write(0,*) 'with BOT_INPUT_HGT: ', BOT_INPUT_HGT, &
                             'set dum2d to bot_input_pres: ', I,J,dum2d(I,J)
          ENDIF

        ELSEIF (TERRAIN_HGT_T(I,J) .lt. BOT_INPUT_HGT ) THEN

!         target is below lowest possible input...extrapolate

          IF ( BOT_INPUT_PRESS-PRESS3D_IN(I,J,2) .gt. 500. ) THEN
            dlnpdz= (log(BOT_INPUT_PRESS)-log(PRESS3D_IN(i,j,2)) ) / &
                     (BOT_INPUT_HGT-Z3D_IN(i,j,2))
            IF (I .eq. Ilook .and. J .eq. Jlook) THEN
              write(0,*) 'I,J,dlnpdz(a): ', I,J,dlnpdz
            ENDIF

          ELSE

!! thin layer and/or just have lowest level - difference with 3rd level data
            IF ( abs(BOT_INPUT_PRESS - PRESS3D_IN(i,j,3)) .gt. 290. ) THEN

              dlnpdz= (log(BOT_INPUT_PRESS)-log(PRESS3D_IN(i,j,3)) ) / &
                      (BOT_INPUT_HGT-Z3D_IN(i,j,3))

              IF (I .eq. Ilook .and. J .eq. Jlook) then
               write(0,*) 'p diff: ', BOT_INPUT_PRESS, PRESS3D_IN(i,j,3)
               write(0,*) 'z diff: ', BOT_INPUT_HGT, Z3D_IN(i,j,3)
              ENDIF
	
            ELSE

!! Loop up to level 7 looking for a sufficiently thick layer

              FIND_THICK:  DO LL=4,7
               IF( abs(BOT_INPUT_PRESS - PRESS3D_IN(i,j,LL)) .gt. 290.) THEN
                 dlnpdz= (log(BOT_INPUT_PRESS)-log(PRESS3D_IN(i,j,LL)) ) / &
                   (BOT_INPUT_HGT-Z3D_IN(i,j,LL))
                EXIT FIND_THICK
               ENDIF 
              END DO FIND_THICK

            ENDIF
        
          ENDIF

        dum2d(I,J)= exp(log(BOT_INPUT_PRESS) + dlnpdz * &
                        (TERRAIN_HGT_T(I,J) - BOT_INPUT_HGT) )

           DEFINED_PSFC(I,J)=.TRUE.

           IF (I .eq. Ilook .and. J .eq. Jlook) THEN
	   write(0,*) 'here(b) set dum2d to: ', I,J, dum2d(I,J)
           ENDIF

        ELSE ! target level bounded by input levels

          c_loop:  DO L=2,generic-1
            IF (TERRAIN_HGT_T(I,J) .gt. Z3D_IN(i,j,L) .AND. &
                  TERRAIN_HGT_T(I,J) .lt. Z3D_IN(i,j,L+1) ) THEN
               dlnpdz= (log(PRESS3D_IN(i,j,l))-log(PRESS3D_IN(i,j,L+1)) ) / &
                       (Z3D_IN(i,j,l)-Z3D_IN(i,j,L+1))
               dum2d(I,J)= log(PRESS3D_IN(i,j,l)) +   &
                           dlnpdz * (TERRAIN_HGT_T(I,J) - Z3D_IN(i,j,L) )
               dum2d(i,j)=exp(dum2d(i,j))
           DEFINED_PSFC(I,J)=.TRUE.
           EXIT c_loop
           IF (I .eq. Ilook .and. J .eq. Jlook) THEN
	     write(0,*) 'here(c) set dum2d to: ', I,J, Dum2d(I,J)
           ENDIF
            ENDIF
          ENDDO c_loop

!!! account for situation where BOT_INPUT_HGT < TERRAIN_HGT_T < Z3D_IN(:,2,:)
          IF (dum2d(I,J) .eq. -9 .AND. BOT_INPUT_HGT .lt. TERRAIN_HGT_T(I,J) &
              .AND. TERRAIN_HGT_T(I,J) .lt. Z3D_IN(I,J,2)) then

            IF (mod(I,50) .eq. 0 .AND. mod(J,50) .eq. 0) THEN
              write(0,*) 'I,J,BOT_INPUT_HGT, bot_pres, TERRAIN_HGT_T: ',  &
                 I,J,BOT_INPUT_HGT, BOT_INPUT_PRESS, TERRAIN_HGT_T(I,J)
            ENDIF

            dlnpdz= (log(PSFC_IN(i,j))-log(PRESS3D_IN(i,j,2)) ) / &
                    (TOPO_IN(i,j)-Z3D_IN(i,j,2))
            dum2d(I,J)= log(PSFC_IN(i,j)) +   &
                        dlnpdz * (TERRAIN_HGT_T(I,J) - TOPO_IN(i,j) )
            dum2d(i,j)= exp(dum2d(i,j))
           DEFINED_PSFC(I,J)=.TRUE.
           IF (I .eq. Ilook .and. J .eq. Jlook) THEN
	     write(0,*) 'here(d) set dum2d to: ', I,J, Dum2d(I,J)
           ENDIF
          ENDIF

          IF (dum2d(I,J) .eq. -9.) THEN
            write(0,*) 'must have flukey situation in new ', I,J
            write(0,*) 'I,J,BOT_INPUT_HGT, bot_pres, TERRAIN_HGT_T: ',  &
                       I,J,BOT_INPUT_HGT, BOT_INPUT_PRESS, TERRAIN_HGT_T(I,J)

            e_loop:  DO L=1,generic-1
              IF ( TERRAIN_HGT_T(I,J) .eq. Z3D_IN(i,j,L) ) THEN
! problematic with HGT_M substitution for "input" surface height?
                dum2d(i,j)=PRESS3D_IN(I,J,L)
                DEFINED_PSFC(I,J)=.TRUE.
           IF (I .eq. Ilook .and. J .eq. Jlook) THEN
	     write(0,*) 'here(e) set dum2d to: ', I,J, Dum2d(I,J)
           ENDIF
                EXIT e_loop
              ENDIF
            ENDDO e_loop

            IF ( TERRAIN_HGT_T(I,J) .eq. TOPO_IN(I,J)) THEN
              dum2d(I,J)=PSFC_IN(I,J)
              DEFINED_PSFC(I,J)=.TRUE.
           IF (I .eq. Ilook .and. J .eq. Jlook) THEN
	     write(0,*) 'here(f) set dum2d to: ', I,J, Dum2d(I,J)
           ENDIF
             write(0,*) 'matched input topo, psfc: ', I,J,TOPO_IN(I,J),PSFC_IN(I,J)
            ENDIF

!            IF (dum2d(I,J) .eq. -9.) THEN
!            ENDIF 

          ENDIF

	if (.not. defined_psfc(i,J)) then
	write(0,*) 'switching to true here'
          DEFINED_PSFC(I,J)=.TRUE.
        endif

	  IF (I .eq. Ilook .AND. J .eq. Jlook) THEN
	    write(0,*) 'newstyle psfc: ', I,J,dum2d(I,J)
          ENDIF

        ENDIF 

	if (.not. DEFINED_PSFC(I,J)) then
!	write(0,*) 'new style undefined at: ', I,J
	endif

        ENDDO I_loop
        ENDDO

        if (print_diag) then
        write(0,*) 'psfc points (new style)'
	loopinc=max( (JTE-JTS)/20,1)
	iloopinc=max( (ITE-ITS)/10,1)
        DO J=min(JTE,JDE-1),JTS,-loopinc
          write(0,633) (dum2d(I,J)/100.,I=ITS,min(ITE,IDE-1),iloopinc)
        END DO
        write(0,*) 'PSFC extremes (new style): ',  minval(dum2d,MASK=DEFINED_PSFC),maxval(dum2d,MASK=DEFINED_PSFC)
        endif

  633   format(35(f5.0,1x))

!         IF (minval(dum2d,MASK=DEFINED_PSFC) .lt. 40000. .or. maxval(dum2d,MASK=DEFINED_PSFC) .gt. 110000.) THEN
!        ENDIF

!! "traditional" isobaric only approach ------------------------------------------------

       ALLOCATE (DUM2DB(IMS:IME,JMS:JME))
       DO J=JMS,JME
        DO I=IMS,IME
         DUM2DB(I,J)=-9.
        END DO
       END DO

       DO J=JTS,min(JTE,JDE-1)
       DO I=ITS,min(ITE,IDE-1)

        IF (TERRAIN_HGT_T(I,J) .lt. Z3D_IN(i,j,2)) THEN ! targ below lowest

          IF ( abs(PRESS3D_IN(i,j,2)-PRESS3D_IN(i,j,3)) .gt. 290.) THEN
            dlnpdz= (log(PRESS3D_IN(i,j,2))-log(PRESS3D_IN(i,j,3)) ) / &
                    (Z3D_IN(i,j,2)-Z3D_IN(i,j,3))
          ELSE
            dlnpdz= (log(PRESS3D_IN(i,j,2))-log(PRESS3D_IN(i,j,4)) ) / &
                    (Z3D_IN(i,j,2)-Z3D_IN(i,j,4))
          ENDIF

          DUM2DB(I,J)= exp( log(PRESS3D_IN(i,j,2)) + dlnpdz * &
                           (TERRAIN_HGT_T(I,J) - Z3D_IN(i,j,2)) )

	  IF (I .eq. Ilook .and. J .eq. Jlook) THEN
	    write(0,*) 'I,K, trad: dlnpdz, press_in(2), terrain_t, Z3D_IN(2): ', I,J,dlnpdz, &
                             PRESS3D_IN(i,j,2), TERRAIN_HGT_T(I,J), Z3D_IN(i,j,2)
          ENDIF

          DEFINED_PSFCB(i,j)=.true.

        ELSEIF (TERRAIN_HGT_T(I,J) .gt. Z3D_IN(i,j,2)) THEN ! target level bounded by input levels

        loop_2b: DO L=2,generic-1
          IF (TERRAIN_HGT_T(I,J) .gt. Z3D_IN(i,j,L) .AND. &
              TERRAIN_HGT_T(I,J) .lt. Z3D_IN(i,j,L+1) ) THEN

            dlnpdz= (log(PRESS3D_IN(i,j,l))-log(PRESS3D_IN(i,j,L+1)) ) / &
                    (Z3D_IN(i,j,l)-Z3D_IN(i,j,L+1))

            DUM2DB(I,J)= log(PRESS3D_IN(i,j,l)) +   &
                         dlnpdz * (TERRAIN_HGT_T(I,J) - Z3D_IN(i,j,L) )
            DUM2DB(i,j)=exp(DUM2DB(i,j))

           IF (I .eq. Ilook .and. J .eq. Jlook) THEN
	     write(0,*) 'L, L+1, p3d_in(L), p3d_in(L+1), z3d_in(L), z3d_in(L+1): ', L, L+1, PRESS3D_IN(i,j,l), PRESS3D_IN(i,j,L+1), Z3D_IN(i,j,l), Z3D_IN(i,j,L+1)
	     write(0,*) 'TERRAIN_HGT_T(I,J) , Z3D_IN(i,j,L): ', TERRAIN_HGT_T(I,J) , Z3D_IN(i,j,L)
	     write(0,*) 'here(2b) set dum2db to: ', I,J, Dum2db(I,J)
           ENDIF

	    DEFINED_PSFCB(i,j)=.true.

            IF (DUM2DB(I,J) .lt. 13000.) THEN
              write(0,*) 'I,J,L,terrain,Z3d(L),z3d(L+1),p3d(L),p3d(l+1): ', I,J,L, &
                                TERRAIN_HGT_T(I,J),Z3D_IN(I,J,L),Z3D_IN(I,J,L+1),PRESS3D_IN(I,J,L), &
                                PRESS3D_IN(I,J,L+1)
            ENDIF
            EXIT loop_2b
          ENDIF
        ENDDO loop_2b

        ELSEIF (TERRAIN_HGT_T(I,J) .eq. Z3D_IN(i,j,2)) THEN
          DUM2DB(i,j)=PRESS3D_IN(I,J,2)
           IF (I .eq. Ilook .and. J .eq. Jlook) THEN
	     write(0,*) 'here(2c) set dum2db to: ', I,J, Dum2db(I,J)
           ENDIF
	  DEFINED_PSFCB(i,j)=.true.
        ENDIF

        IF (DUM2DB(I,J) .eq. -9.) THEN
          write(0,*) 'must have flukey situation in trad ', I,J
          loop_2d: DO L=1,generic-1
            IF ( TERRAIN_HGT_T(I,J) .eq. Z3D_IN(i,j,L) ) THEN
              DUM2DB(i,j)=PRESS3D_IN(I,J,L)
           IF (I .eq. Ilook .and. J .eq. Jlook) THEN
	     write(0,*) 'here(2d) set dum2db to: ', I,J, Dum2db(I,J)
           ENDIF
              DEFINED_PSFCB(i,j)=.true.
              EXIT loop_2d
            ENDIF
          ENDDO loop_2d
        ENDIF

        IF (DUM2DB(I,J) .eq. -9.) THEN
          write(0,*) 'HOPELESS PSFC, I QUIT'
        ENDIF

	if (I .eq. Ilook .and. J .eq. Jlook) THEN
	  write(0,*) ' traditional psfc: ', I,J,DUM2DB(I,J)
        ENDIF

       ENDDO
       ENDDO

!       write(0,*) 'psfc points (traditional)'
!       DO J=min(JTE,JDE-1),JTS,-loopinc
!         write(0,633) (DUM2DB(I,J)/100.,I=its,min(ite,IDE-1),iloopinc)
!       ENDDO

!       write(0,*) 'PSFC extremes (traditional): ', minval(DUM2DB,MASK=DEFINED_PSFCB),maxval(DUM2DB,MASK=DEFINED_PSFCB)

!       IF (minval(DUM2DB,MASK=DEFINED_PSFCB) .lt. 40000. .or. maxval(DUM2DB,MASK=DEFINED_PSFCB) .gt. 108000.) THEN
!       ENDIF

!!!!! end traditional

       DO J=JTS,min(JTE,JDE-1)
       DO I=ITS,min(ITE,IDE-1)
         IF (DEFINED_PSFCB(I,J) .and. DEFINED_PSFC(I,J)) THEN

          IF (  abs(dum2d(I,J)-DUM2DB(I,J)) .gt. 400.) THEN
	     write(0,*) 'BIG DIFF I,J, dum2d, DUM2DB: ', I,J,dum2d(I,J),DUM2DB(I,J)
          ENDIF

!! do we have enough confidence in new style to give it more than 50% weight?
          psfc_out(I,J)=0.5*(dum2d(I,J)+DUM2DB(I,J))
         ELSEIF (DEFINED_PSFC(I,J)) THEN
           psfc_out(I,J)=dum2d(I,J)
         ELSEIF (DEFINED_PSFCB(I,J)) THEN
           psfc_out(I,J)=DUM2DB(I,J)
         ELSE
	   write(0,*) 'I,J,dum2d,DUM2DB: ', I,J,dum2d(I,J),DUM2DB(I,J)
	   write(0,*) 'I,J,DEFINED_PSFC(I,J),DEFINED_PSFCB(I,J): ', I,J,DEFINED_PSFC(I,J),DEFINED_PSFCB(I,J)
         ENDIF

	IF (I .eq. Ilook .AND. J .eq. Jlook) THEN
	  write(0,*) ' combined psfc: ', I,J,psfc_out(I,J)
        ENDIF

	IF (psfc_out(I,J) .lt. 50000. .or. psfc_out(I,J) .gt. 108000.) THEN
	  write(0,*) 'strange combo on psfc_out, terrain_hgt_t: ', I,J, psfc_out(I,J), terrain_hgt_t(I,J)
	  write(0,*) 'DEFINED_PSFC, dum2d: ', DEFINED_PSFC(I,J),dum2d(I,J)
	  write(0,*) 'DEFINED_PSFCB, DUM2DB: ', DEFINED_PSFCB(I,J),DUM2DB(I,J)

!	if (terrain_hgt_t(I,J) .gt. 0 .and. terrain_hgt_t(I,J) .lt. 5000.) then
!        else
!          write(0,*) 'will let strange psfc pass because surface topo is: ', terrain_hgt_t(I,J)
!        endif

	ENDIF

       ENDDO
       ENDDO

       if (print_diag) then
       write(0,*) 'psfc points (final combined)'
       DO J=min(JTE,JDE-1),JTS,-loopinc
         write(0,633) (psfc_out(I,J)/100.,I=its,min(ite,IDE-1),iloopinc)
       ENDDO
       endif
	
	deallocate(dum2d,dum2db)

	END SUBROUTINE compute_nmm_surfacep

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      SUBROUTINE compute_3d_pressure(psfc_out,SGML1,SGML2,               &
                                     SG1,SG2,pdtop,pt                    &
     &,                              pd,p3d_o,pint_o                 &
     &,                              IDS,IDE,JDS,JDE,KDS,KDE             &
     &,                              IMS,IME,JMS,JME,KMS,KME             &
     &,                              ITS,ITE,JTS,JTE,KTS,KTE,vcoord,print_diag)


        REAL, INTENT(IN) :: psfc_out(IMS:IME,JMS:JME)
        REAL, INTENT(IN) :: SGML1(KDE-1),SGML2(KDE-1),pdtop,pt
        REAL, INTENT(IN) :: SG1(KDE),SG2(KDE)
        LOGICAL, INTENT(IN) :: print_diag

!        REAL, INTENT(OUT):: p3d_o(IMS:IME,KDS:KDE-1,JMS:JME)
!        REAL, INTENT(OUT):: pint_o(IMS:IME,KDS:KDE,JMS:JME)
!        REAL, INTENT(OUT):: PD(IMS:IME,JMS:JME)

        REAL :: p3d_o(IMS:IME,KDS:KDE-1,JMS:JME)
        REAL :: pint_o(IMS:IME,KDS:KDE,JMS:JME)
        REAL :: PD(IMS:IME,JMS:JME)
         
        INTEGER          :: IDS,IDE,JDS,JDE,KDS,KDE
        INTEGER          :: IMS,IME,JMS,JME,KMS,KME
        INTEGER          :: ITS,ITE,JTS,JTE,KTS,KTE  
        INTEGER          :: vcoord

        CHARACTER (len=132) :: message

!	write(0,*) 'pdtop, pt, psfc_out(1,1): ', pdtop, pt, psfc_out(1,1)

        DO J=JTS,min(JTE,JDE-1)
          DO I=ITS,min(ITE,IDE-1)

          IF (vcoord .eq. 0) then
             PD(I,J)=psfc_out(I,J)-PDTOP-PT
          ELSE
             PD(I,J)=psfc_out(I,J)-PT
          ENDIF

          ENDDO
        ENDDO

        if (print_diag) then
	write(0,*) 'IMS,IME,JMS,JME: ', IMS,IME,JMS,JME
	write(0,*) 'size(p3d_o): ', size(p3d_o,dim=1),size(p3d_o,dim=2), size(p3d_o,dim=3)
	write(0,*) 'size(pd): ', size(pd,dim=1),size(pd,dim=2)
        endif

        DO J=JTS,min(JTE,JDE-1)
         DO K=KDS,KDE-1
          DO I=ITS,min(ITE,IDE-1)
           p3d_o(I,K,J)=PD(I,J)*SGML2(K)+PDTOP*SGML1(K)+PT
	IF (p3d_o(I,K,J) .ge. psfc_out(I,J) .or. p3d_o(I,K,J) .le. pt) THEN
        write(0,*) 'problematic I,K,J,p3d_o: ', I,K,J,p3d_o(I,K,J),pt,psfc_out(I,J)
        write(0,*) 'SGML2(K): ', SGML2(K)
        write(0,*) 'SGML1(K): ', SGML1(K)
        write(0,*) 'PD, PDTOP, PT: ', PD(I,J),PDTOP, PT
	STOP
 	ENDIF
          ENDDO
         ENDDO
        ENDDO

        DO J=JTS,min(JTE,JDE-1)
         DO K=KDS,KDE
          DO I=ITS,min(ITE,IDE-1)
           pint_o(I,K,J)=PD(I,J)*SG2(K)+PDTOP*SG1(K)+PT

	if (pint_o(I,K,J)-psfc_out(I,J) .gt. 500.) then
!	IF (pint_o(I,K,J) .gt. psfc_out(I,J) .or. pint_o(I,K,J) .lt. pt) THEN
           write(0,*) 'problematic I,K,J,pint_o: ', I,K,J,pint_o(I,K,J),pt,psfc_out(I,J)
 	ENDIF

          ENDDO
         ENDDO
        ENDDO

	END SUBROUTINE compute_3d_pressure

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE interp_press2press_lin(press_in,press_out, &
                                    data_in, data_out,generic          &
     &,                             extrapolate,ignore_lowest,TFIELD    &
     &,                             IDS,IDE,JDS,JDE,KDS,KDE             &
     &,                             IMS,IME,JMS,JME,KMS,KME             &
     &,                             ITS,ITE,JTS,JTE,KTS,KTE, first, LNSH )

    ! Interpolates data from one set of pressure surfaces to
    ! another set of pressures

    INTEGER                            :: IDS,IDE,JDS,JDE,KDS,KDE
    INTEGER                            :: IMS,IME,JMS,JME,KMS,KME
    INTEGER                            :: ITS,ITE,JTS,JTE,KTS,KTE,generic

!    REAL, INTENT(IN)                   :: press_in(IMS:IME,generic,JMS:JME)
    REAL, INTENT(IN)                   :: press_in(IMS:IME,JMS:JME,generic)
    REAL, INTENT(IN)                   :: press_out(IMS:IME,KDS:KDE-1,JMS:JME)
!    REAL, INTENT(IN)                   :: data_in(IMS:IME,generic,JMS:JME)
    REAL, INTENT(IN)                   :: data_in(IMS:IME,JMS:JME,generic)
    REAL, INTENT(OUT)                  :: data_out(IMS:IME,JMS:JME,KMS:KME)
    LOGICAL, INTENT(IN)                :: extrapolate, ignore_lowest, TFIELD
    LOGICAL                            :: col_smooth, first

    INTEGER                            :: i,j, Ilook,Jlook
    INTEGER                            :: k,kk,lnsh
    REAL                               :: desired_press
    REAL                               :: dvaldlnp,dlnp,tadiabat,tiso

    REAL, PARAMETER                    :: ADIAFAC=9.81/1004.
    REAL, PARAMETER                    :: TSTEXTRAPFAC=.0065



    data_out(:,:,:) = -99999.9


    Ilook=5
    Jlook=5

    IF (ignore_lowest) then
       LMIN=2
    ELSE
       LMIN=1
    ENDIF

        

        if (ITS .eq. 1 .and. JTS .eq. 1) then
	write(0,*) 'interpolating for ', LNSH, ' boundary rows'
	endif


    DO j = JTS, min(JTE,JDE-1)
     test_i: DO i = ITS, min(ITE,IDE-1)

     IF (.not. first) THEN

!        IF (J .ne. JDS .and. J .ne. JDE-1 .and. J .ne. JDE-2 .and. &
!          I .ne. IDS .and. I .ne. IDE-1 .and. I .ne. IDE-2 ) THEN

        IF (J .ge. JDS+lnsh+1 .and. J .le. JDE-2-lnsh  .and. &
            I .ge. IDS+LNSH+1 .and. I .le. IDE-2-LNSH  ) THEN

!! not near boundary
          CYCLE test_i

	if (mod(I,15) .eq. 0 .and. mod(J,15) .eq. 0) then
	write(0,*) 'made it past the boundary skip in _lin vinterp'
	endif

        ENDIF
     ENDIF


       col_smooth=.false.

        output_loop: DO k = KDS,KDE-1

          desired_press = press_out(i,k,j)

	if (I .eq. Ilook .and. J .eq. Jlook) write(0,*) 'K, desired_press: ', K, desired_press

	if (col_smooth .and. TFIELD .and. K-1 .ge. KDS) then
        if (desired_press .lt. press_in(i,j,LMIN) .and.  &
            press_out(i,k-1,j) .gt. press_in(i,j,LMIN)) then
          MAX_SMOOTH=K
!         write(0,*) 'I,J, MAX_SMOOTH: ', I,J, MAX_SMOOTH
        endif
	endif

! keep track of where the extrapolation begins

          IF (desired_press .GT. press_in(i,j,LMIN)) THEN
           IF (TFIELD .and. K .eq. 1  .and. (desired_press - press_in(i,j,LMIN)) .gt. 3000.) then
            col_smooth=.TRUE.   ! due to large extrapolation distance
           ENDIF


            IF ((desired_press - press_in(i,j,LMIN)).LT. 50.) THEN ! 0.5 mb
               data_out(i,j,k) = data_in(i,j,LMIN)
            ELSE
              IF (extrapolate) THEN
                ! Extrapolate downward because desired P level is below
                ! the lowest level in our input data.  Extrapolate using simple
                ! 1st derivative of value with respect to ln P for the bottom 2
                ! input layers.

                ! Add a check to make sure we are not using the gradient of
                ! a very thin layer

                if (TFIELD) then
                  tiso=0.5*(data_in(i,j,1)+data_in(i,j,2))
                endif


                IF ( (press_in(i,j,LMIN)-press_in(i,j,LMIN+1)) .GT. 500.) THEN ! likely isobaric data
                  dlnp     = log(press_in(i,j,LMIN))-log(press_in(i,j,LMIN+1))
                  dvaldlnp = (data_in(i,j,LMIN) - data_in(i,j,LMIN+1)) / dlnp
                ELSE                                                           ! assume terrain following
                  dlnp     = log(press_in(i,j,LMIN))-log(press_in(i,j,LMIN+5))
                  dvaldlnp = (data_in(i,j,LMIN) - data_in(i,j,LMIN+5)) / dlnp
                ENDIF
                data_out(i,j,k) = data_in(i,j,LMIN) + dvaldlnp * &
                               ( log(desired_press)-log(press_in(i,j,LMIN)) )

	if (I .eq. Ilook .and. J .eq. Jlook) then
		write(0,*) 'K, extrap data_out: ', K, data_out(i,J,K)
	endif

        if (TFIELD .and. data_out(i,j,k) .lt. tiso-0.2) then

! restrict slope to -1K/10 hPa
          dvaldlnp=max(dvaldlnp, -1.0/ &
                                log( press_in(i,j,LMIN) / &
                                   ( press_in(i,j,LMIN)-1000.)  ))

          data_out(I,J,K)= data_in(i,j,LMIN) + dvaldlnp * &
                               ( log(desired_press)-log(press_in(i,j,LMIN)) )

        elseif (TFIELD .and. data_out(i,j,k) .gt. tiso+0.2) then

! restrict slope to +0.8K/10 hPa
          dvaldlnp=min(dvaldlnp, 0.8/ &
                                log( press_in(i,j,LMIN) / &
                                   ( press_in(i,j,LMIN)-1000.)  ))

          data_out(I,J,K)= data_in(i,j,LMIN) + dvaldlnp * &
                               ( log(desired_press)-log(press_in(i,j,LMIN)) )

         endif

              ELSE
                data_out(i,j,k) = data_in(i,j,LMIN)
              ENDIF
            ENDIF
          ELSE IF (desired_press .LT. press_in(i,j,generic)) THEN
            IF ( (press_in(i,j,generic) - desired_press) .LT. 10.) THEN
               data_out(i,j,k) = data_in(i,j,generic)
            ELSE
              IF (extrapolate) THEN
                ! Extrapolate upward
                IF ((press_in(i,j,generic-1)-press_in(i,j,generic)).GT.50.) THEN
                  dlnp    =log(press_in(i,j,generic))-log(press_in(i,j,generic-1))
                  dvaldlnp=(data_in(i,j,generic)-data_in(i,j,generic-1))/dlnp
                ELSE
                  dlnp    =log(press_in(i,j,generic))-log(press_in(i,j,generic-2))
                  dvaldlnp=(data_in(i,j,generic)-data_in(i,j,generic-2))/dlnp
                ENDIF
                data_out(i,j,k) =  data_in(i,j,generic) + &
                  dvaldlnp * (log(desired_press)-log(press_in(i,j,generic)))
              ELSE
                data_out(i,j,k) = data_in(i,j,generic)
              ENDIF
            ENDIF
          ELSE
            ! We can trap between two levels and linearly interpolate

            input_loop:  DO kk = LMIN, generic-1
              IF (desired_press .EQ. press_in(i,j,kk) )THEN
                data_out(i,j,k) = data_in(i,j,kk)
                EXIT input_loop
              ELSE IF ( (desired_press .LT. press_in(i,j,kk)) .AND. &
                        (desired_press .GT. press_in(i,j,kk+1)) ) THEN

!       do trapped in lnp

         dlnp = log(press_in(i,j,kk)) - log(press_in(i,j,kk+1))
         dvaldlnp = (data_in(i,j,kk)-data_in(i,j,kk+1))/dlnp
         data_out(i,j,k) = data_in(i,j,kk+1)+ &
                           dvaldlnp*(log(desired_press)-log(press_in(i,j,kk+1)))
	if (I .eq. Ilook .and. J .eq. Jlook) then
		write(0,*) 'K, interp data_out: ', K, data_out(i,J,K)
	endif

                EXIT input_loop
              ENDIF

            ENDDO input_loop
          ENDIF
        ENDDO output_loop

        if (col_smooth) then
       do K=max(KDS,MAX_SMOOTH-4),MAX_SMOOTH+4
       data_out(I,J,K)=0.5*(data_out(I,J,K)+data_out(I,J,K+1))
       enddo
        endif

      ENDDO test_i
    ENDDO
  END SUBROUTINE interp_press2press_lin

!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE interp_press2press_lin_inter(press_in,press_out, &
                                    data_in, data_out,generic          &
     &,                             extrapolate,ignore_lowest,TFIELD    & 
     &,                             IDS,IDE,JDS,JDE,KDS,KDE             &
     &,                             IMS,IME,JMS,JME,KMS,KME             &
     &,                             ITS,ITE,JTS,JTE,KTS,KTE, Ilook,Jlook )

    ! Interpolates data from one set of pressure surfaces to
    ! another set of pressures

    INTEGER                            :: IDS,IDE,JDS,JDE,KDS,KDE
    INTEGER                            :: IMS,IME,JMS,JME,KMS,KME,Ilook,Jlook
    INTEGER                            :: ITS,ITE,JTS,JTE,KTS,KTE,generic

!    REAL, INTENT(IN)                   :: press_in(IMS:IME,generic,JMS:JME)
    REAL, INTENT(IN)                   :: press_in(IMS:IME,JMS:JME,generic)
    REAL, INTENT(IN)                   :: press_out(IMS:IME,KDS:KDE,JMS:JME)
!    REAL, INTENT(IN)                   :: data_in(IMS:IME,generic,JMS:JME)
    REAL, INTENT(IN)                   :: data_in(IMS:IME,JMS:JME,generic)
    REAL, INTENT(OUT)                  :: data_out(IMS:IME,KDS:KDE,JMS:JME)
    LOGICAL, INTENT(IN)                :: extrapolate, ignore_lowest, TFIELD
    LOGICAL                            :: col_smooth

    INTEGER                            :: i,j
    INTEGER                            :: k,kk
    REAL                               :: desired_press
    REAL                               :: dvaldlnp,dlnp,tadiabat,tiso
    REAL                               :: dvaldp,dp


    data_out(:,:,:) = -99999.9

    IF (ignore_lowest) then
       LMIN=2
    ELSE
       LMIN=1
    ENDIF

    DO j = JTS, min(JTE,JDE-1)
      DO i = ITS, min(ITE,IDE-1)

	if (I .eq. Ilook .and. J .eq. Jlook) then
	write(0,*) 'extrapolate, ignore_lowest: ', extrapolate, ignore_lowest
	write(0,*) 'I,J, press_in(1), press_in(2): ', I,J,press_in(I,J,1),press_in(I,J,2)
	write(0,*) 'I,J, data_in(1), data_in(2): ', I,J,data_in(I,J,1),data_in(I,J,2)
	endif

!	if ( (press_in(I,J,1)- press_in(I,J,2)) .gt. 2000.) then
!          LMIN=1
!        else
!          LMIN=2
!        endif


        output_loop: DO k = KDS,KDE

          desired_press = press_out(i,k,j)

!	if (I .eq. ITS .and. J .eq. JTS) then
!	write(0,*) 'trying to define for K= ', K, desired_press
!	endif

	if (I .eq. Ilook .and. J .eq. Jlook) then
	write(0,*) 'K, desired pressure is: ', K, desired_press
	endif


          IF (desired_press .GT. press_in(i,j,LMIN)) THEN

	if (I .eq. Ilook .and. J .eq. Jlook) then
	write(0,*) 'in extrapolation possibility region ', desired_press, press_in(i,j,LMIN)
	endif
	
            IF ((desired_press - press_in(i,j,LMIN)).LT. 50.) THEN ! 0.5 mb
               data_out(i,k,j) = data_in(i,j,LMIN)
            ELSE
              IF (extrapolate) THEN
                ! Extrapolate downward because desired P level is below
                ! the lowest level in our input data.  Extrapolate using simple
                ! 1st derivative of value with respect to ln P for the bottom 2
                ! input layers.

                ! Add a check to make sure we are not using the gradient of
                ! a very thin layer


                IF ( (press_in(i,j,LMIN)-press_in(i,j,LMIN+1)) .GT. 500.) THEN ! likely isobaric data
                  dlnp     = log(press_in(i,j,LMIN))-log(press_in(i,j,LMIN+1))
                  dp       = press_in(i,j,LMIN)-press_in(i,j,LMIN+1)
                  dvaldlnp = (data_in(i,j,LMIN) - data_in(i,j,LMIN+1)) / dlnp
                  dvaldp   = (data_in(i,j,LMIN) - data_in(i,j,LMIN+1)) / dp
                ELSE                                                           ! assume terrain following
                  dlnp     = log(press_in(i,j,LMIN))-log(press_in(i,j,LMIN+5))
                  dp       = press_in(i,j,LMIN)-press_in(i,j,LMIN+5)
                  dvaldlnp = (data_in(i,j,LMIN) - data_in(i,j,LMIN+5)) / dlnp
                  dvaldp   = (data_in(i,j,LMIN) - data_in(i,j,LMIN+5)) / dp
                ENDIF
!                data_out(i,k,j) = data_in(i,j,LMIN) + dvaldlnp * &
!                               ( log(desired_press)-log(press_in(i,j,LMIN)) )

                data_out(i,k,j) = data_in(i,j,LMIN) + dvaldp * &
                               ( desired_press-press_in(i,j,LMIN) )

	if (I .eq. Ilook .and. J .eq. Jlook) then
	write(0,*) 'extrapolated K, data_out: ', K, data_out(I,K,J)
	endif

              ELSE
                data_out(i,k,j) = data_in(i,j,LMIN)
	if (I .eq. Ilook .and. J .eq. Jlook) then
	write(0,*) 'set to LMIN value K, data_out: ', K, data_out(I,K,J)
	endif
              ENDIF
            ENDIF
          ELSE IF (desired_press .LT. press_in(i,j,generic)) THEN
            IF ( (press_in(i,j,generic) - desired_press) .LT. 10.) THEN
               data_out(i,k,j) = data_in(i,j,generic)
            ELSE
              IF (extrapolate) THEN
                ! Extrapolate upward
                IF ((press_in(i,j,generic-1)-press_in(i,j,generic)).GT.50.) THEN
                  dlnp    =log(press_in(i,j,generic))-log(press_in(i,j,generic-1))
                  dvaldlnp=(data_in(i,j,generic)-data_in(i,j,generic-1))/dlnp
                ELSE
                  dlnp    =log(press_in(i,j,generic))-log(press_in(i,j,generic-2))
                  dvaldlnp=(data_in(i,j,generic)-data_in(i,j,generic-2))/dlnp
                ENDIF
                data_out(i,k,j) =  data_in(i,j,generic) + &
                  dvaldlnp * (log(desired_press)-log(press_in(i,j,generic)))
              ELSE
                data_out(i,k,j) = data_in(i,j,generic)
              ENDIF
            ENDIF
          ELSE
            ! We can trap between two levels and linearly interpolate

            input_loop_2:  DO kk = LMIN, generic
              IF (desired_press .EQ. press_in(i,j,kk) )THEN
                data_out(i,k,j) = data_in(i,j,kk)
                CYCLE output_loop
              ENDIF
              END DO input_loop_2

            input_loop:  DO kk = LMIN, generic-1
              IF (desired_press .EQ. press_in(i,j,kk) )THEN
                data_out(i,k,j) = data_in(i,j,kk)
                EXIT input_loop
              ELSE IF ( (desired_press .LT. press_in(i,j,kk)) .AND. &
                        (desired_press .GT. press_in(i,j,kk+1)) ) THEN

!       do trapped in lnp

         dlnp = log(press_in(i,j,kk)) - log(press_in(i,j,kk+1))
         dp   = press_in(i,j,kk) - press_in(i,j,kk+1)
         dvaldlnp = (data_in(i,j,kk)-data_in(i,j,kk+1))/dlnp
         dvaldp   = (data_in(i,j,kk)-data_in(i,j,kk+1))/dp
!         data_out(i,k,j) = data_in(i,j,kk+1)+ &
!                           dvaldlnp*(log(desired_press)-log(press_in(i,j,kk+1)))

         data_out(i,k,j) = data_in(i,j,kk+1)+ &
                           dvaldp*(desired_press-press_in(i,j,kk+1))

	if (I .eq. Ilook .and. J .eq. Jlook) then
	write(0,*) 'trapped K, data_out:: ', K, desired_press, data_out(I,K,J)
	endif


                EXIT input_loop
              ENDIF

            ENDDO input_loop
          ENDIF
        ENDDO output_loop

      ENDDO
    ENDDO
  END SUBROUTINE interp_press2press_lin_inter


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE wind_adjust(press_in,press_out, &
                                    U_in, V_in,U_out,V_out           &
     &,                             generic,depth_replace    & 
     &,                             IDS,IDE,JDS,JDE,KDS,KDE             &
     &,                             IMS,IME,JMS,JME,KMS,KME             &
     &,                             ITS,ITE,JTS,JTE,KTS,KTE )

    INTEGER                            :: IDS,IDE,JDS,JDE,KDS,KDE
    INTEGER                            :: IMS,IME,JMS,JME,KMS,KME
    INTEGER                            :: ITS,ITE,JTS,JTE,KTS,KTE,generic
    INTEGER                            :: MAXLIN,MAXLOUT

    REAL, INTENT(IN)                   :: press_in(IMS:IME,JMS:JME,generic)
    REAL, INTENT(IN)                   :: press_out(IMS:IME,KDS:KDE-1,JMS:JME)
    REAL, INTENT(IN)                   :: U_in(IMS:IME,JMS:JME,generic)
    REAL, INTENT(IN)                   :: V_in(IMS:IME,JMS:JME,generic)
    REAL, INTENT(INOUT)                :: U_out(IMS:IME,KMS:KME,JMS:JME)
    REAL, INTENT(INOUT)                :: V_out(IMS:IME,KMS:KME,JMS:JME)
    REAL                               :: p1d_in(generic)
    REAL                               :: p1d_out(KDS:KDE-1)

    DO j = JTS, min(JTE,JDE-1)
      DO i = ITS, min(ITE,IDE-1)

!        IF (press_out(I,1,J) .lt. press_in(I,J,2)) then
         IF(  (press_in(I,J,2)-press_out(I,1,J)) .gt. 200.) then

        U_out(I,1,J)=U_in(I,J,2)
        V_out(I,1,J)=V_in(I,J,2)

   INLOOP: DO L=2,generic
        IF (  (press_in(I,J,2)-press_in(I,J,L)) .lt. depth_replace) THEN
          p1d_in(L)=(press_in(I,J,2)-press_in(I,J,L))
          MAXLIN=L
        ELSE
          EXIT INLOOP
        ENDIF 
    END DO INLOOP

   OUTLOOP: DO L=KDS,KDE-1
        IF (  (press_out(I,1,J)-press_out(I,L,J)) .lt. depth_replace) THEN
          p1d_out(L)=(press_out(I,1,J)-press_out(I,L,J))
          MAXLOUT=L
        ELSE
          EXIT OUTLOOP
        ENDIF 
    END DO OUTLOOP

!        IF ((press_in(I,J,2)-press_out(I,1,J)) .gt. 2200.) then
!         do L=2,MAXLIN
!	 write(0,*) 'I,J,L, p1d_in: ', I,J,L,p1d_in(L), U_in(I,J,L),V_in(I,J,L)
!         enddo
!         do L=1,MAXLOUT
!	 write(0,*) 'I,J,L, p1d_out: ', I,J,L,p1d_out(L),U_out(I,L,J),V_out(I,L,J)
!         enddo
!        ENDIF


        DO L=1,MAXLOUT
	ptarg=p1d_out(L)

    FINDLOOP:   DO LL=2,MAXLIN

         if (p1d_in(LL) .lt. ptarg .and. p1d_in(LL+1) .gt. ptarg) then

           dlnp=log(p1d_in(LL))-log(p1d_in(LL+1))
           dudlnp=(U_in(I,J,LL)-U_in(I,J,LL+1))/dlnp
           dvdlnp=(V_in(I,J,LL)-V_in(I,J,LL+1))/dlnp
           U_out(I,L,J)=U_in(I,J,LL)+dudlnp*(log(ptarg)-log(p1d_in(LL)))
           V_out(I,L,J)=V_in(I,J,LL)+dvdlnp*(log(ptarg)-log(p1d_in(LL)))

!        IF ((press_in(I,J,2)-press_out(I,1,J)) .gt. 2200.) then
!	 write(0,*) 'rev I,J,L, p1d_out: ', I,J,L,p1d_out(L),U_out(I,L,J),V_out(I,L,J)
!        ENDIF

           EXIT FINDLOOP
         endif

   END DO FINDLOOP
        END DO   ! MAXLOUT loop
           

        ENDIF

      ENDDO
    ENDDO



  END SUBROUTINE wind_adjust
!--------------------------------------------------------------------

  SUBROUTINE interp_press2press_log(press_in,press_out, &
                                    data_in, data_out, generic          &
     &,                             extrapolate,ignore_lowest           & 
     &,                             IDS,IDE,JDS,JDE,KDS,KDE             &
     &,                             IMS,IME,JMS,JME,KMS,KME             &
     &,                             ITS,ITE,JTS,JTE,KTS,KTE, first, lnsh )

    ! Interpolates ln(data) from one set of pressure surfaces to
    ! another set of pressures

    INTEGER                            :: IDS,IDE,JDS,JDE,KDS,KDE
    INTEGER                            :: IMS,IME,JMS,JME,KMS,KME
    INTEGER                            :: ITS,ITE,JTS,JTE,KTS,KTE,generic

    REAL, INTENT(IN)                   :: press_in(IMS:IME,JMS:JME,generic)
    REAL, INTENT(IN)                   :: press_out(IMS:IME,KDS:KDE-1,JMS:JME)
    REAL, INTENT(INOUT)                   :: data_in(IMS:IME,JMS:JME,generic)
    REAL, INTENT(OUT)                  :: data_out(IMS:IME,JMS:JME,KMS:KME)
    LOGICAL, INTENT(IN)                :: extrapolate, ignore_lowest, first

    INTEGER                            :: i,j
    INTEGER                            :: k,kk,lnsh
    REAL                               :: desired_press
    REAL                               :: dlnvaldlnp,dlnp



!!    LNSH=7

	if (its .eq. 1 .and. jts .eq. 1) write(0,*) 'have LNSH in interp_press2press_log: ', LNSH
    data_out(:,:,:) = -99999.9

      DO K=1,generic
      DO j = JTS, min(JTE,JDE-1)
      DO i = ITS, min(ITE,IDE-1)
        DATA_IN(I,J,K)=max(DATA_in(I,J,K),1.e-13)
      ENDDO
      ENDDO
      ENDDO


    IF (ignore_lowest) then
       LMIN=2
    ELSE
       LMIN=1
    ENDIF

    DO j = JTS, min(JTE,JDE-1)
     test_i:  DO i = ITS, min(ITE,IDE-1)

      IF (internal_time_loop .gt. 1) THEN
        IF (J .ge. JDS+lnsh .and. J .le. JDE-1-lnsh  .and. &
            I .ge. IDS+LNSH .and. I .le. IDE-1-LNSH  ) THEN
!! not on boundary
          CYCLE test_i
        ENDIF
      ENDIF


        output_loop: DO k = KDS,KDE-1

          desired_press = press_out(i,k,j)

          IF (desired_press .GT. press_in(i,j,LMIN)) THEN

            IF ((desired_press - press_in(i,j,LMIN)).LT. 10.) THEN ! 0.1 mb
               data_out(i,j,k) = data_in(i,j,LMIN)
            ELSE
              IF (extrapolate) THEN
                ! Extrapolate downward because desired P level is below
                ! the lowest level in our input data.  Extrapolate using simple
                ! 1st derivative of value with respect to ln P for the bottom 2
                ! input layers.

                ! Add a check to make sure we are not using the gradient of
                ! a very thin layer

                IF ( (press_in(i,j,LMIN)-press_in(i,j,LMIN+1)) .GT. 100.) THEN
                  dlnp     = log(press_in(i,j,LMIN))-log(press_in(i,j,LMIN+1))
                  dlnvaldlnp = ( log(data_in(i,j,LMIN)) - log(data_in(i,j,LMIN+1)) ) / dlnp

                ELSE

                  dlnp     = log(press_in(i,j,LMIN))-log(press_in(i,j,LMIN+2))
                  dlnvaldlnp = (log(data_in(i,j,LMIN)) - log(data_in(i,j,LMIN+2))) / dlnp

                ENDIF

                data_out(i,j,k) = exp(log(data_in(i,j,LMIN)) + dlnvaldlnp * &
                               ( log(desired_press)-log(press_in(i,j,LMIN))))
              ELSE
                data_out(i,j,k) = data_in(i,j,LMIN)
              ENDIF
            ENDIF
          ELSE IF (desired_press .LT. press_in(i,j,generic)) THEN
            IF ( (press_in(i,j,generic) - desired_press) .LT. 10.) THEN
               data_out(i,j,k) = data_in(i,j,generic)
            ELSE
              IF (extrapolate) THEN
                ! Extrapolate upward
                IF ((press_in(i,j,generic-1)-press_in(i,j,generic)).GT.50.) THEN
                  dlnp    =log(press_in(i,j,generic))-log(press_in(i,j,generic-1))
                  dlnvaldlnp=(log(data_in(i,j,generic))-log(data_in(i,j,generic-1)))/dlnp
                ELSE
                  dlnp    =log(press_in(i,j,generic))-log(press_in(i,j,generic-2))
                  dlnvaldlnp=(log(data_in(i,j,generic))-log(data_in(i,j,generic-2)))/dlnp
                ENDIF
                data_out(i,j,k) =  exp(log(data_in(i,j,generic)) + &
                          dlnvaldlnp * (log(desired_press)-log(press_in(i,j,generic))))
              ELSE
                data_out(i,j,k) = data_in(i,j,generic)
              ENDIF
            ENDIF
          ELSE
            ! We can trap between two levels and linearly interpolate

            input_loop:  DO kk = LMIN, generic-1
              IF (desired_press .EQ. press_in(i,j,kk) )THEN
                data_out(i,j,k) = data_in(i,j,kk)
                EXIT input_loop
              ELSE IF ( (desired_press .LT. press_in(i,j,kk)) .AND. &
                        (desired_press .GT. press_in(i,j,kk+1)) ) THEN

!       do trapped in lnp

         dlnp = log(press_in(i,j,kk)) - log(press_in(i,j,kk+1))
         dlnvaldlnp = (log(data_in(i,j,kk))-log(data_in(i,j,kk+1)))/dlnp
         data_out(i,j,k) = exp(log(data_in(i,j,kk+1))+ &
                          dlnvaldlnp*(log(desired_press)-log(press_in(i,j,kk+1))))

                EXIT input_loop

              ENDIF

            ENDDO input_loop
          ENDIF
        ENDDO output_loop
      ENDDO test_i
    ENDDO
  END SUBROUTINE interp_press2press_log

!-------------------------------------------------------------------
   SUBROUTINE rh_to_mxrat (rh, t, p, q , wrt_liquid , &
                           ids , ide , jds , jde , kds , kde , &
                           ims , ime , jms , jme , kms , kme , &
                           its , ite , jts , jte , kts , kte )

      IMPLICIT NONE

      INTEGER , INTENT(IN)        :: ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte

      LOGICAL , INTENT(IN)        :: wrt_liquid

!      REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(IN)     :: p , t
!      REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(INOUT)  :: rh
      REAL , DIMENSION(ims:ime,jms:jme,kms:kme) , INTENT(IN)     :: p , t
      REAL , DIMENSION(ims:ime,jms:jme,kms:kme) , INTENT(INOUT)  :: rh
      REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(OUT)    :: q

      !  Local vars

      INTEGER                     :: i , j , k

      REAL                        :: ew , q1 , t1

      REAL,         PARAMETER     :: T_REF       = 0.0
      REAL,         PARAMETER     :: MW_AIR      = 28.966
      REAL,         PARAMETER     :: MW_VAP      = 18.0152

      REAL,         PARAMETER     :: A0       = 6.107799961
      REAL,         PARAMETER     :: A1       = 4.436518521e-01
      REAL,         PARAMETER     :: A2       = 1.428945805e-02
      REAL,         PARAMETER     :: A3       = 2.650648471e-04
      REAL,         PARAMETER     :: A4       = 3.031240396e-06
      REAL,         PARAMETER     :: A5       = 2.034080948e-08
      REAL,         PARAMETER     :: A6       = 6.136820929e-11

      REAL,         PARAMETER     :: ES0 = 6.1121

      REAL,         PARAMETER     :: C1       = 9.09718
      REAL,         PARAMETER     :: C2       = 3.56654
      REAL,         PARAMETER     :: C3       = 0.876793
      REAL,         PARAMETER     :: EIS      = 6.1071
      REAL                        :: RHS
      REAL,         PARAMETER     :: TF       = 273.16
      REAL                        :: TK

      REAL                        :: ES
      REAL                        :: QS
      REAL,         PARAMETER     :: EPS         = 0.622
      REAL,         PARAMETER     :: SVP1        = 0.6112
      REAL,         PARAMETER     :: SVP2        = 17.67
      REAL,         PARAMETER     :: SVP3        = 29.65
      REAL,         PARAMETER     :: SVPT0       = 273.15

      !  This subroutine computes mixing ratio (q, kg/kg) from basic variables
      !  pressure (p, Pa), temperature (t, K) and relative humidity (rh, 1-100%).
      !  The reference temperature (t_ref, C) is used to describe the temperature
      !  at which the liquid and ice phase change occurs.

         DO k = kts , kte
      DO j = jts , MIN ( jde-1 , jte )
            DO i = its , MIN (ide-1 , ite )
!mp                  rh(i,j,k) = MIN ( MAX ( rh(i,j,k) ,  1. ) , 100. )
                  rh(i,j,k) = MIN ( MAX ( rh(i,j,k) ,  .001 ) , 100. )
            END DO
         END DO
      END DO

      IF ( wrt_liquid ) THEN
            DO k = kts , kte
         DO j = jts , MIN ( jde-1 , jte )
               DO i = its , MIN (ide-1 , ite )
                  es=svp1*10.*EXP(svp2*(t(i,j,k)-svpt0)/(t(i,j,k)-svp3))
                  qs=eps*es/(p(i,j,k)/100.-es)
                  q(i,k,j)=MAX(.01*rh(i,j,k)*qs,0.0)
               END DO
            END DO
         END DO

      ELSE
            DO k = kts , kte
         DO j = jts , MIN ( jde-1 , jte )
               DO i = its , MIN (ide-1 , ite )

                  t1 = t(i,j,k) - 273.16

                  !  Obviously dry.

                  IF ( t1 .lt. -200. ) THEN
                     q(i,k,j) = 0

                  ELSE

                     !  First compute the ambient vapor pressure of water

                     IF ( ( t1 .GE. t_ref ) .AND. ( t1 .GE. -47.) ) THEN    ! liq phase ESLO
                        ew = a0 + t1 * (a1 + t1 * (a2 + t1 * (a3 + t1 * (a4 + t1 * (a5 + t1 * a6)))))

                     ELSE IF ( ( t1 .GE. t_ref ) .AND. ( t1 .LT. -47. ) ) then !liq phas poor ES
                        ew = es0 * exp(17.67 * t1 / ( t1 + 243.5))

                     ELSE
                        tk = t(i,j,k)
                        rhs = -c1 * (tf / tk - 1.) - c2 * alog10(tf / tk) +  &
                               c3 * (1. - tk / tf) +      alog10(eis)
                        ew = 10. ** rhs

                     END IF

                     !  Now sat vap pres obtained compute local vapor pressure

                     ew = MAX ( ew , 0. ) * rh(i,j,k) * 0.01

                     !  Now compute the specific humidity using the partial vapor
                     !  pressures of water vapor (ew) and dry air (p-ew).  The
                     !  constants assume that the pressure is in hPa, so we divide
                     !  the pressures by 100.

                     q1 = mw_vap * ew
                     q1 = q1 / (q1 + mw_air * (p(i,j,k)/100. - ew))

                     q(i,k,j) = q1 / (1. - q1 )

                  END IF

               END DO
            END DO
         END DO

      END IF

   END SUBROUTINE rh_to_mxrat

!--=------------------------------------------------------------------

      SUBROUTINE  boundary_smooth(h, landmask, grid, nsmth , nrow &
     &,                          IDS,IDE,JDS,JDE,KDS,KDE             &
     &,                          IMS,IME,JMS,JME,KMS,KME             &
     &,                          ITS,ITE,JTS,JTE,KTS,KTE ) 

	implicit none

      TYPE (input_vars)          :: grid 

        integer:: nsmth,nrow
        real::    h(IMS:IME,JMS:JME),landmask(IMS:IME,JMS:JME)
        real ::   h_old(IMS:IME,JMS:JME)
        real ::   hbms(IDS:IDE-1,JDS:JDE-1)
        real ::   hse(IDS:IDE-1,JDS:JDE-1)
        real ::   hne(IDS:IDE-1,JDS:JDE-1)
        integer :: IDS,IDE,JDS,JDE,KDS,KDE
        integer :: IMS,IME,JMS,JME,KMS,KME
        integer :: ITS,ITE,JTS,JTE,KTS,KTE
        integer :: IPS,IPE,JPS,JPE,KPS,KPE
        integer :: ihl, ihh, m2l, ibas,jmelin
        integer :: I,J,KS,IOFFSET,JSTART,JEND

	ips=its
        ipe=ite
	jps=jts
        jpe=jte
	kps=kts
        kpe=kte


!!!! what was the purpose of +1 at the top and -1 at the bottom????

!mp	ide=ide+1
!mp     jde=jde+1

	write(0,*) 'boundary smooth ite,ide: ', ite,ide
	write(0,*) 'boundary smooth jte,jde: ', jte,jde
	write(0,*) 'boundary smooth ims,ime: ', ims,ime

        do J=JTS,min(JTE,JDE-1)
         do I=ITS,min(ITE,IDE-1)
           hbms(I,J)=landmask(I,J)
         enddo
        enddo

        jmelin=(JDE-1)-nrow+1
        ibas=nrow/2
        m2l=mod(nrow,2)

         ihl=IDS+ibas
         ihh=(IDE-1)-ibas
	
	write(0,*) 'ihl, ihh: ', ihl, ihh

        do j=jts,min(jte,jde-1)

!         ihl=ibas+mod(j,2)+m2l*mod(J+1,2)
!         ihh=(IDE-1)-ibas-m2l*mod(J+1,2)

         do i=its,min(ite,ide-1)
          if (I .ge. ihl .and. I .le. ihh .and. J .ge. nrow .and. J .le. jmelin) then
           hbms(I,J)=0.
          endif
         end do
        end do

  634	format(30(f3.0,1x))

        do KS=1,nsmth

         grid%HGT_M=h

         call exchange_halo_r(grid%HGT_M, &
                              IMS, IME, JMS, JME, 1, 1, &
                              ITS, ITE, JTS, JTE, 1, 1)

         h=grid%HGT_M
         h_old=grid%HGT_M

	write(0,*) 'loop J: ',  JTS,min(JTE,JDE-1)
	write(0,*) 'loop I: ',  ITS,min(ITE,IDE-1)

! reinstated

          do J=JTS,min(JTE,JDE-1)
           do I=ITS, min(ITE,IDE-1)
              if (I .gt. IDS .and. J .gt. JDS .and. J .lt. JDE-1 .and. I .lt. IDE-1) then
                h(i,j)= ( h_old(i+1,j) + h_old(i-1,j) + h_old(i,j-1) + h_old(i,j+1) - &
                        4. *h_old(i,j) )*hbms(i,j)*0.125+h_old(i,j)
              endif
           enddo
          enddo

!       special treatment for four corners

        if (hbms(1,1) .eq. 1 .and. ITS .le. 1 .and. JTS .le. 1) then
        h(1,1)=0.75*h(1,1)+0.05*h(2,2)+  &
                                 0.10*(h(2,1)+h(1,2))
        endif

        if (hbms(IDE-1,1) .eq. 1 .and. ITE .ge. IDE-2 .and. JTS .le. 1) then
        h(IDE-1,1)=0.75*h(IDE-1,1)+0.05*h(IDE-1-1,2)+ &
                                 0.10*(h(IDE-1-1,1)+h(IDE-1,2))
        endif

        if (hbms(1,JDE-1) .eq. 1 .and. ITS .le. 1 .and. JTE .ge. JDE-2) then
        h(1,JDE-1)=0.75*h(1,JDE-1)+0.05*h(2,JDE-1-1)+ &
                                 0.10*(h(2,JDE-1)+h(1,JDE-1-1))
        endif

        if (hbms(IDE-1,JDE-1) .eq. 1 .and. ITE .ge. IDE-2 .and. JTE .ge. JDE-2) then
        h(IDE-1,JDE-1)=0.75*h(IDE-1,JDE-1)+0.05*h(IDE-1-1,JDE-1-1)+ &
                                 0.10*(h(IDE-1-1,JDE-1)+h(IDE-1,JDE-1-1))
        endif

        do J=JMS,JME
         do I=IMS,IME
         grid%HGT_M(I,J)=h(I,J)
         enddo 
        enddo

         call exchange_halo_r(grid%HGT_M, &
                              IMS, IME, JMS, JME, 1, 1, &
                              ITS, ITE, JTS, JTE, 1, 1)
        do J=JMS,JME
         do I=IMS,IME
         h(I,J)=grid%HGT_M(I,J)
         h_old(I,J)=grid%HGT_M(I,J)
         enddo 
        enddo


!       S bound
	if (JTS .eq. JDS) then
        J=JTS

        do I=ITS,ITE
        if (I .ge. IDS+1 .and. I .le. IDE-2) then

        if (hbms(I,J) .eq. 1) then
        h(I,J)=0.7*h_old(I,J)+0.1*( h_old(I-1,J)+h_old(I+1,J)+h_old(I,J+1) )
	if (I .eq. 1 .and. (J .eq. 2 .or. J .eq. 50) ) then
	   write(0,*) 'A modified I,J,h(I,J): ', I,J,h(I,J)
        endif
        endif

        endif
        enddo

        endif

!       N bound
        if (JTE .ge. JDE-3) then
        J=JDE-1
	write(0,*) 'J for N boundary: ', J
         do I=ITS,ITE
          if (hbms(I,J) .eq. 1 .and. I .ge. IDS+1 .and. I .le. IDE-2) then
           h(I,J)=0.7*h_old(I,J)+0.1*(h_old(I-1,J)+h_old(I+1,J)+h_old(I,J-1))
	if (I .eq. 1 .and. (J .eq. 2 .or. J .eq. 50) ) then
	   write(0,*) 'B modified I,J,h(I,J): ', I,J,h(I,J)
        endif
          endif
         enddo
	endif

!       W bound
        if (ITS .eq. IDS) then
         I=ITS
         do J=JTS,JTE
          if (hbms(I,J) .eq. 1 .and. J .ge. JDS+1 .and. J .le. JDE-2) then
           h(I,J)=0.7*h_old(I,J)+0.1*(h_old(I,J+1)+h_old(I,J-1)+h_old(I+1,J))
	if (I .eq. 1 .and. (J .eq. 2 .or. J .eq. 50) ) then
	   write(0,*) 'C modified I,J,h(I,J): ', I,J,h(I,J)
        endif
          endif
         enddo
	endif

!       E bound
        if (ITE .ge. IDE-3) then
         I=min(ITE,IDE-1)
         do J=JTS,JTE 
          if (hbms(I,J) .eq. 1  .and. J .ge. JDS+1 .and. J .le. JDE-2) then
           h(I,J)=0.7*h_old(I,J)+0.1*(h_old(I,J+1)+h_old(I,J-1)+h_old(I-1,J))
          endif
         enddo
	endif



        enddo   ! end ks loop

	write(0,*) 'IMS,IME,JMS,JME for halo exchange: ', IMS,IME,JMS,JME
        do J=JMS,JME
         do I=IMS,IME
         grid%HGT_M(I,J)=h(I,J)
         enddo 
        enddo

         call exchange_halo_r(grid%HGT_M, &
                              IMS, IME, JMS, JME, 1, 1, &
                              ITS, ITE, JTS, JTE, 1, 1)
        do J=JMS,JME
         do I=IMS,IME
         h(I,J)=grid%HGT_M(I,J)
         h_old(I,J)=grid%HGT_M(I,J)
        enddo 
        enddo


! extra smoothing along inner boundary

          if (JTS .eq. JDS) then
           if (ITE .eq. IDE-1) then
              IOFFSET=1
           else
              IOFFSET=0
           endif
!  Southern Boundary
        write(0,*) 'souther inner boundary over: ', its,min(ITE,IDE-1)-IOFFSET
           do i=its,min(ITE,IDE-1)-IOFFSET
	if (I .ne. 1) then
             h(i,2)=0.25*(h_old(i,1)+h_old(i+1,2)+ &
                          h_old(i,3)+h_old(i-1,2))
	endif
           enddo
          endif


          if (JTE .eq. JDE-1) then
           if (ITE .ge. IDE-3) then
              IOFFSET=1
           else
              IOFFSET=0
           endif
           do i=its,min(ITE,IDE-1)-IOFFSET
	if (I .ne. IDS) then
             h(i,(JDE-1)-1)=0.25*(h_old(i,(JDE-1)-2)+h_old(i-1,(JDE-1)-1)+ &
                                h_old(i,JDE-1)+h_old(i+1,JDE-1-1))
	if (I .eq. 1 .and. (JDE-2 .eq. 2 .or. JDE-2 .eq. 50) ) then
	   write(0,*) 'E modified I,J,h(I,J): ', I,JDE-2,h(I,JDE-2)
	   write(0,*) 'h_old(i,(JDE-1)-2),h_old(i-1,(JDE-1)-1): ', h_old(i,(JDE-1)-2),h_old(i-1,(JDE-1)-1)
	   write(0,*) 'h_old(i,JDE-1),h_old(i+1,JDE-1-1): ', h_old(i,JDE-1),h_old(i+1,JDE-1-1)
        endif
	endif
           enddo
          endif

        
           if (JTS .eq. 1) then
             JSTART=2
           else
             JSTART=JTS
           endif

           if (JTE .ge. JDE-3) then
             JEND=(JDE-1)-2
           else
             JEND=JTE
           endif

          if (ITS .eq. IDS) then

!  Western Boundary
!mp         do j=JSTART,JEND,2
          do j=JSTART,JEND
            h(2,j)=0.25*(h_old(2,j-1)+h_old(1,j)+ &
                         h_old(2,j+1)+h_old(3,j))

          enddo
          endif
	

          if (ITE .eq. IDE-1) then
!  Eastern Boundary
!mp          do j=JSTART,JEND,2
	write(0,*) 'inner, eastern boundary from: ', JSTART, JEND
          do j=JSTART,JEND
            h(IDE-1-1,j)=0.25*(h_old((IDE-1)-1,j-1)+h_old((IDE-1)-2,j)+ &
                              h_old((IDE-1)-1,j+1)+h_old((IDE-1),j))
          enddo
          endif

!mp          ide=ide-1
!mp          jde=jde-1


       END SUBROUTINE boundary_smooth

!--------------------------------------------------------------------

  SUBROUTINE reverse_vert_coord  ( field, start_z, end_z                &
     &,                             IDS,IDE,JDS,JDE,KDS,KDE             &
     &,                             IMS,IME,JMS,JME,KMS,KME             &
     &,                             ITS,ITE,JTS,JTE,KTS,KTE )

	IMPLICIT NONE

        INTEGER , INTENT(IN)        :: ids , ide , jds , jde , kds , kde , &
                                       ims , ime , jms , jme , kms , kme , &
                                       its , ite , jts , jte , kts , kte,  &
                                       start_z, end_z
        REAL, INTENT(INOUT)         :: field(IMS:IME,JMS:JME,end_z) 
! local

        INTEGER                     :: I,J,L
        REAL, ALLOCATABLE           :: dum3d(:,:,:)

        allocate(dum3d(IMS:IME,JMS:JME,end_z))

        DO L=start_z,end_z
          DO J=jts,min(jte,jde-1)
            DO I=its,min(ite,ide-1)
	      dum3d(I,J,L)=field(I,J,end_z-L+start_z)
            END DO
          END DO
        END DO

        DO L=start_z,end_z
          DO J=jts,min(jte,jde-1)
            DO I=its,min(ite,ide-1)
              field(I,J,L)=dum3d(I,J,L)
            END DO
          END DO
        END DO
       
        DEALLOCATE(dum3d)

        END SUBROUTINE reverse_vert_coord

!--------------------------------------------------------------------

      SUBROUTINE heights_to_temps(Z, T, pint, pmid     &
     &,            IDS,IDE,JDS,JDE,KDS,KDE             &
     &,            IMS,IME,JMS,JME,KMS,KME             &
     &,            ITS,ITE,JTS,JTE,KTS,KTE, Ilook,Jlook )

	IMPLICIT NONE

	INTEGER:: IDS,IDE,JDS,JDE,KDS,KDE
        INTEGER:: IMS,IME,JMS,JME,KMS,KME
	INTEGER:: ITS,ITE,JTS,JTE,KTS,KTE
        INTEGER:: I,J,K,Ilook,Jlook

        real, parameter:: r_d=287.04
        real, parameter:: g=9.81

        REAL, INTENT(IN):: Z(IMS:IME,KDS:KDE,JMS:JME)
        REAL, INTENT(IN):: PINT(IMS:IME,KDS:KDE,JMS:JME)
        REAL, INTENT(IN):: PMID(IMS:IME,KDS:KDE-1,JMS:JME)
        REAL, INTENT(OUT):: T(IMS:IME,JMS:JME,KMS:KME)

        DO j = JTS, min(JTE,JDE-1)
         DO k = KDS, KDE-1
          DO i = ITS, min(ITE,IDE-1)


!! 	if test just to prevent taking log(0)
	if (PINT(I,K,J) .eq. 0) then
          T(I,J,K)=-(PMID(I,K,J)*G/R_d) * &
                    (Z(I,K,J)-Z(I,K+1,J))/(PINT(I,K,J)-PINT(I,K+1,J))
	else
          T(I,J,K)=-(G/R_d) * &
                    (Z(I,K,J)-Z(I,K+1,J))/(log(PINT(I,K,J))-log(PINT(I,K+1,J)))
        endif

	if (I .eq. 1 .and. J .eq. JDE/2) then
	write(0,*) 'K, PMID, PINT(K),PINT(K+1): ', K, PMID(I,K,J),PINT(I,K,J),PINT(I,K+1,J)	
	write(0,*) 'K, Z(K),Z(K+1): ', K, Z(I,K,J),Z(I,K+1,J)
	endif

	if (I .eq. 1 .and. J .eq. JDE/2) then
	write(0,*) 'I,J,K, T(I,J,K): ',I,J,K, T(I,J,K)
	endif

          ENDDO
         ENDDO
        ENDDO

      END SUBROUTINE heights_to_temps

!--------------------------------------------------------------------

        SUBROUTINE compute_nmm_levels(ninterface, ptop, eta_levels)

!        USE module_model_constants

        IMPLICIT NONE

        character(len=132):: message
        integer        ::  ninterface,Lthick,L
        real, parameter:: gamma=.0065
        real, parameter:: t_stand=288.
        real, parameter:: p_stand=101325.
        real, parameter:: r_d=287.04
        real, parameter:: g=9.81

        real           ::  maxdz_compute, ptop
        real           ::  plower,pupper,tlay, sum

        real             :: eta_levels(ninterface)
        real, allocatable:: Z(:)
        real, allocatable:: deta_levels_spline(:)

        logical:: print_pbl_warn

!----------------------------------------------------

        allocate(Z(ninterface))
        allocate(deta_levels_spline(ninterface-1))

        CALL compute_eta_spline(ninterface-1,deta_levels_spline,ptop)

        sum=0.
        DO L=1,ninterface-1
          sum=sum+deta_levels_spline(L)
        ENDDO

        eta_levels(ninterface)=1.00
        eta_levels(1)=0.00

        DO L=2,ninterface
          eta_levels(L)=eta_levels(L-1)+deta_levels_spline(L-1)
        ENDDO

        DO L=2,ninterface-1
          eta_levels(L)=0.5*(eta_levels(L))+0.25*(eta_levels(L-1)+eta_levels(L+1))
        ENDDO

        Z(1)=0.
        maxdz_compute=0.
        print_pbl_warn=.false.

        DO L=2,ninterface
          tlay=max( t_stand-gamma*Z(L-1), 216.5)
          plower=ptop+(p_stand-ptop)*eta_levels(L)
          pupper=ptop+(p_stand-ptop)*eta_levels(L-1)
          Z(L)=Z(L-1)+(tlay*r_d/g)*(log(plower)-log(pupper))

          if (plower .gt. 85000. .and. pupper .lt. 85000. .and. L .lt. 10) then
            print_pbl_warn=.true.
          endif

          write(0,*) 'L, eta(l), pupper, Z(L): ', L, eta_levels(L),pupper,Z(L)

          if (Z(L)-Z(L-1) .gt. maxdz_compute) then
            Lthick=L
          endif

          maxdz_compute=max(maxdz_compute,Z(L)-Z(L-1))
        ENDDO

        if (print_pbl_warn) then
          write(0,*) 'WARNING - PBL MAY BE POORLY RESOLVED WITH NUMBER OF VERTICAL LEVELS'
          write(0,*) '        - CONSIDER INCREASING THE VERTICAL RESOLUTION'
        endif

        write(0,*) 'thickest layer was: ', maxdz_compute , 'meters thick at level: ', Lthick

        END SUBROUTINE compute_nmm_levels

!---------------------------

     SUBROUTINE compute_eta_spline(LM, dsg, ptop)

     IMPLICIT NONE

     real:: dsg(LM), ptop, sum, rsum
     real, allocatable:: xold(:),dold(:)
     real, allocatable:: xnew(:),sgm(:)
     real, allocatable:: pps(:),qqs(:),y2s(:)
     integer nlev,LM,L,KOLD

    IF (LM .ge. 46) THEN
     KOLD=9
     allocate(xold(KOLD))
     allocate(dold(KOLD))

     xold(1)=.00
     dold(1)=.006
     xold(2)=.13
     dold(2)=.009
     xold(3)=.19
     dold(3)=.012
     xold(4)=.30
     dold(4)=.036
     xold(5)=.42
     dold(5)=.041
     xold(6)=.56
     dold(6)=.040
     xold(7)=.69
     dold(7)=.018

     if (ptop .ge. 2000.) then
      xold(8)=.90
      dold(8)=.012
      xold(9)=1.0
      dold(9)=.006
     else
      xold(8)=.90
      dold(8)=.008
      xold(9)=1.0
      dold(9)=.003
     endif

    ELSE

     KOLD=8
     allocate(xold(KOLD))
     allocate(dold(KOLD))

     xold(1)=.00
     dold(1)=.006
     xold(2)=.18
     dold(2)=.015
     xold(3)=.32
     dold(3)=.035
     xold(4)=.50
     dold(4)=.040
     xold(5)=.68
     dold(5)=.030
     xold(6)=.75
     dold(6)=.017
     xold(7)=.85
     dold(7)=.012

     if (ptop .ge. 2000.) then
      xold(8)=1.0
      dold(8)=.013
     else
      xold(8)=1.0
      dold(8)=.008
     endif

    ENDIF

        allocate(xnew(lm))
        allocate(sgm(lm+1))
        allocate(pps(lm))
        allocate(qqs(lm))
        allocate(y2s(lm))

    DO L=1,LM
       xnew(l)=float(l-1)/float(lm-1)
    ENDDO

    y2s=0.

        write(0,*) 'call spline with KOLD: ', KOLD
        write(0,*) 'call spline with XOLD: ', XOLD
        write(0,*) 'call spline with DOLD: ', DOLD
        write(0,*) 'call spline with XNEW: ', xnew

    CALL spline(kold,xold,dold,y2s,lm,xnew,dsg,pps,qqs)

        write(0,*) 'from spline...dsg: ', dsg

    sum=0.
    DO l=1,lm
       sum=sum+dsg(l)
    ENDDO


    rsum=1./sum
    sgm(1)=0.

    DO L=1,lm-1
     dsg(l)=dsg(l)*rsum
     sgm(l+1)=sgm(l)+dsg(l)
    ENDDO
    sgm(lm+1)=1.
    dsg(lm)=sgm(lm+1)-sgm(lm)

    END SUBROUTINE compute_eta_spline

   SUBROUTINE monthly_min_max ( field_in , field_min , field_max , &
                      ids , ide , jds , jde , kds , kde , &
                      ims , ime , jms , jme , kms , kme , &
                      its , ite , jts , jte , kts , kte )

   !  Plow through each month, find the max, min values for each i,j.

      IMPLICIT NONE

      INTEGER , INTENT(IN)        :: ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte

      REAL , DIMENSION(ims:ime,jms:jme,12) , INTENT(IN)  :: field_in
      REAL , DIMENSION(ims:ime,   jms:jme) , INTENT(OUT) :: field_min , field_max

      !  Local vars

      INTEGER :: i , j , l
      REAL :: minner , maxxer

      DO j = jts , MIN(jde-1,jte)
         DO i = its , MIN(ide-1,ite)
            minner = field_in(i,j,1)
            maxxer = field_in(i,j,1)
            DO l = 2 , 12
               IF ( field_in(i,j,l) .LT. minner ) THEN
                  minner = field_in(i,j,l)
               END IF
               IF ( field_in(i,j,l) .GT. maxxer ) THEN
                  maxxer = field_in(i,j,l)
               END IF
            END DO
            field_min(i,j) = minner
            field_max(i,j) = maxxer
         END DO
      END DO

   END SUBROUTINE monthly_min_max
!---------------------------------------------------------------------
   SUBROUTINE monthly_interp_to_date ( field_in , date_str , field_out , &
                      ids , ide , jds , jde , kds , kde , &
                      ims , ime , jms , jme , kms , kme , &
                      its , ite , jts , jte , kts , kte )

   !  Linrarly in time interpolate data to a current valid time.  The data is
   !  assumed to come in "monthly", valid at the 15th of every month.

      IMPLICIT NONE

      INTEGER , INTENT(IN)        :: ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte

      CHARACTER (LEN=24) , INTENT(IN) :: date_str
      REAL , DIMENSION(ims:ime,jms:jme,12) , INTENT(IN)  :: field_in
      REAL , DIMENSION(ims:ime,   jms:jme) , INTENT(OUT) :: field_out

      !  Local vars

      INTEGER :: i , j , l
      INTEGER , DIMENSION(0:13) :: middle
      INTEGER :: target_julyr , target_julday , target_date
      INTEGER :: julyr , julday , int_month, next_month
      REAL :: gmt
      CHARACTER (LEN=4) :: yr
      CHARACTER (LEN=2) :: mon , day15


      WRITE(day15,FMT='(I2.2)') 15
      DO l = 1 , 13
         WRITE(mon,FMT='(I2.2)') l
         CALL get_julgmt ( date_str(1:4)//'-'//mon//'-'//day15//'_'//'00:00:00.0000' , julyr , julday , gmt )
         middle(L) = julyr*1000 + julday
      END DO

      l = 0
      middle(l) = middle( 1) - 30

      l = 13
      middle(l) = middle(12) + 30

      CALL get_julgmt ( date_str , target_julyr , target_julday , gmt )
      target_date = target_julyr * 1000 + target_julday

      find_month : DO l = 0 , 12
         IF ( ( middle(l) .LT. target_date ) .AND. ( middle(l+1) .GE. target_date ) ) THEN
            DO j = jts , MIN ( jde-1 , jte )
               DO i = its , MIN (ide-1 , ite )
                  int_month = MOD ( l , 12 )
                  IF ( int_month .EQ. 0 ) int_month=12

        IF (int_month == 12) THEN
            next_month=1
        ELSE
            next_month=int_month+1
        ENDIF

                  field_out(i,j) =  ( field_in(i,j,next_month) * ( target_date - middle(l)   ) + &
                                      field_in(i,j,int_month  ) * ( middle(l+1) - target_date ) ) / &
                                    ( middle(l+1) - middle(l) )

               END DO
            END DO
            EXIT find_month
         END IF
      END DO find_month

   END SUBROUTINE monthly_interp_to_date

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE quarterly_interp_to_date( field_in , date_str , field_out , &
                      ids , ide , jds , jde , kds , kde , &
                      ims , ime , jms , jme , kms , kme , &
                      its , ite , jts , jte , kts , kte )

   !  Linrarly in time interpolate data to a current valid time.  The data is
   !  assumed to come in "quarterly", valid at the 15th of every month.

      IMPLICIT NONE

      INTEGER , INTENT(IN)        :: ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte

      CHARACTER (LEN=24) , INTENT(IN) :: date_str
      REAL , DIMENSION(ims:ime,jms:jme,4) , INTENT(IN)  :: field_in
      REAL , DIMENSION(ims:ime,   jms:jme) , INTENT(OUT) :: field_out

      !  Local vars

      INTEGER :: i , j , l, LL
      INTEGER , DIMENSION(0:5) :: middle
      INTEGER :: target_julyr , target_julday , target_date
      INTEGER :: julyr , julday , int_quart, next_quart
      REAL :: gmt
      CHARACTER (LEN=4) :: yr
      CHARACTER (LEN=2) :: mon , day30


      WRITE(day30,FMT='(I2.2)') 30
      LL=0
      DO l = 1 , 10, 3
      LL=LL+1
         WRITE(mon,FMT='(I2.2)') l
         CALL get_julgmt ( date_str(1:4)//'-'//mon//'-'//day30//'_'//'00:00:00.0000' , julyr , julday , gmt )
         middle(LL) = julyr*1000 + julday
      END DO

      l = 0
      middle(l) = middle( 1) - 90

      l = 5
      middle(l) = middle(4) + 90

      CALL get_julgmt ( date_str , target_julyr , target_julday , gmt )
      target_date = target_julyr * 1000 + target_julday
      find_quarter : DO l = 0 , 4
         IF ( ( middle(l) .LT. target_date ) .AND. ( middle(l+1) .GE. target_date ) ) THEN
                  int_quart = MOD ( l , 4 )
                  IF ( int_quart .EQ. 0 ) int_quart = 4

        IF (int_quart == 4) THEN
            next_quart=1
        ELSE
            next_quart=int_quart+1
        ENDIF

            DO j = jts , MIN ( jde-1 , jte )
               DO i = its , MIN (ide-1 , ite )
                  field_out(i,j) =  ( field_in(i,j,next_quart) * ( target_date - middle(l)   ) + &
                                      field_in(i,j,int_quart  ) * ( middle(l+1) - target_date ) ) / &
                                    ( middle(l+1) - middle(l) )
               END DO
            END DO
            EXIT find_quarter
         END IF
      END DO find_quarter

   END SUBROUTINE quarterly_interp_to_date

! -------------------------------------------------------------------
   SUBROUTINE get_julgmt(date_str,julyr,julday,gmt)
     IMPLICIT NONE
! Arguments
     CHARACTER (LEN=24) , INTENT(IN) :: date_str
     INTEGER, INTENT(OUT  ) :: julyr
     INTEGER, INTENT(OUT  ) :: julday
     REAL   , INTENT(OUT  ) :: gmt
! Local
     INTEGER :: ny , nm , nd , nh , ni , ns , nt
     INTEGER :: my1, my2, my3, monss
     INTEGER, DIMENSION(12) :: mmd
     DATA MMD/31,28,31,30,31,30,31,31,30,31,30,31/
     CALL split_date_char ( date_str , ny , nm , nd , nh , ni , ns , nt )
     GMT=nh+FLOAT(ni)/60.+FLOAT(ns)/3600.
     MY1=MOD(ny,4)
     MY2=MOD(ny,100)
     MY3=MOD(ny,400)
     IF(MY1.EQ.0.AND.MY2.NE.0.OR.MY3.EQ.0)MMD(2)=29
     JULDAY=nd
     JULYR=ny
     DO MONSS=1,nm-1
       JULDAY=JULDAY+MMD(MONSS)
     ENDDO
   END SUBROUTINE get_julgmt
!---------------------------------------------------------

   SUBROUTINE split_date_char ( date , century_year , month , day , hour , minute , second , ten_thousandth)

      IMPLICIT NONE

      !  Input data.

      CHARACTER(LEN=24) , INTENT(IN) :: date

      !  Output data.

      INTEGER , INTENT(OUT) :: century_year , month , day , hour , minute , second , ten_thousandth

      READ(date,FMT='(    I4)') century_year
      READ(date,FMT='( 5X,I2)') month
      READ(date,FMT='( 8X,I2)') day
      READ(date,FMT='(11X,I2)') hour
      READ(date,FMT='(14X,I2)') minute
      READ(date,FMT='(17X,I2)') second
      READ(date,FMT='(20X,I4)') ten_thousandth

   END SUBROUTINE split_date_char

   SUBROUTINE init_module_date_time
   END SUBROUTINE init_module_date_time
!--------------------------------------------------------

     subroutine spline(NOLD,XOLD,YOLD,Y2,NNEW,XNEW,YNEW,P,Q)
!   ********************************************************************
!   *                                                                  *
!   *  THIS IS A ONE-DIMENSIONAL CUBIC SPLINE FITTING ROUTINE          *
!   *  PROGRAMED FOR A SMALL SCALAR MACHINE.                           *
!   *                                                                  *
!   *  PROGRAMER Z. JANJIC                                             *
!   *                                                                  *
!   *  NOLD - NUMBER OF GIVEN VALUES OF THE FUNCTION.  MUST BE GE 3.   *
!   *  XOLD - LOCATIONS OF THE POINTS AT WHICH THE VALUES OF THE       *
!   *         FUNCTION ARE GIVEN.  MUST BE IN ASCENDING ORDER.         *
!   *  YOLD - THE GIVEN VALUES OF THE FUNCTION AT THE POINTS XOLD.     *
!   *  Y2   - THE SECOND DERIVATIVES AT THE POINTS XOLD.  IF NATURAL   *
!   *         SPLINE IS FITTED Y2(1)=0. AND Y2(NOLD)=0. MUST BE        *
!   *         SPECIFIED.                                               *
!   *  NNEW - NUMBER OF VALUES OF THE FUNCTION TO BE CALCULATED.       *
!   *  XNEW - LOCATIONS OF THE POINTS AT WHICH THE VALUES OF THE       *
!   *         FUNCTION ARE CALCULATED.  XNEW(K) MUST BE GE XOLD(1)     *
!   *         AND LE XOLD(NOLD).                                       *
!   *  YNEW - THE VALUES OF THE FUNCTION TO BE CALCULATED.             *
!   *  P, Q - AUXILIARY VECTORS OF THE LENGTH NOLD-2.                  *
!   *                                                                  *
!   ********************************************************************
!
!   LOG:
!
!     JOVIC - July 2008 - fixed incorrectly dimensioned arrays,
!     PYLE                and do loop leading to out of bound array
!                         reference
!------
!
!     PYLE - June 2007 - eliminated use of GO TO statements.
!
!-----------------------------------------------------------------------
      IMPLICIT NONE
!-----------------------------------------------------------------------
      INTEGER,INTENT(IN) :: NNEW,NOLD
      REAL,DIMENSION(NOLD),INTENT(IN) :: XOLD,YOLD
      REAL,DIMENSION(NNEW),INTENT(IN)  :: XNEW
      REAL,DIMENSION(NNEW),INTENT(OUT) :: YNEW
      REAL,DIMENSION(NOLD+2),INTENT(INOUT) :: P,Q,Y2
!
      INTEGER :: K,K1,K2,KOLD,NOLDM1, K2_hold, K_hold
      REAL :: AK,BK,CK,DEN,DX,DXC,DXL,DXR,DYDXL,DYDXR                   &
     &       ,RDX,RTDXC,X,XK,XSQ,Y2K,Y2KP1
!-----------------------------------------------------------------------

      NOLDM1=NOLD-1

      DXL=XOLD(2)-XOLD(1)
      DXR=XOLD(3)-XOLD(2)
      DYDXL=(YOLD(2)-YOLD(1))/DXL
      DYDXR=(YOLD(3)-YOLD(2))/DXR
      RTDXC=0.5/(DXL+DXR)

      P(1)= RTDXC*(6.*(DYDXR-DYDXL)-DXL*Y2(1))
      Q(1)=-RTDXC*DXR

      K=3
      first_loop: DO K=3,NOLD-1
        DXL=DXR
        DYDXL=DYDXR
        DXR=XOLD(K+1)-XOLD(K)
        DYDXR=(YOLD(K+1)-YOLD(K))/DXR
        DXC=DXL+DXR
        DEN=1./(DXL*Q(K-2)+DXC+DXC)
        P(K-1)= DEN*(6.*(DYDXR-DYDXL)-DXL*P(K-2))
        Q(K-1)=-DEN*DXR
      END DO first_loop

      DO K=NOLDM1,2,-1
         Y2(K)=P(K-1)+Q(K-1)*Y2(K+1)
         K_hold=K
      END DO

      K=K_hold

!-----------------------------------------------------------------------
      second_loop:  DO K1=1,NNEW
        XK=XNEW(K1)
        third_loop:  DO K2=2,NOLD

          IF(XOLD(K2)>XK)THEN
            KOLD=K2-1
            K2_hold=K2
            exit third_loop
          ENDIF
        K2_hold=K2
        END DO third_loop

        IF (XOLD(K2_hold) .le. XK) THEN
          YNEW(K1)=YOLD(NOLD)
          CYCLE second_loop
        ENDIF

        IF (K1 .eq. 1 .or. K .ne. KOLD) THEN
          K=KOLD
          Y2K=Y2(K)
          Y2KP1=Y2(K+1)
          DX=XOLD(K+1)-XOLD(K)
          RDX=1./DX
          AK=.1666667*RDX*(Y2KP1-Y2K)
          BK=0.5*Y2K
          CK=RDX*(YOLD(K+1)-YOLD(K))-.1666667*DX*(Y2KP1+Y2K+Y2K)
!        write(0,*) 'RDX, Y2KP1, Y2K, AK: ', RDX, Y2KP1, Y2K, AK
!        write(0,*) 'YOLD(K+1),YOLD(K), DX: ', YOLD(K+1),YOLD(K), DX
        ENDIF

        X=XK-XOLD(K)
        XSQ=X*X
        YNEW(K1)=AK*XSQ*X+BK*XSQ+CK*X+YOLD(K)
!        write(0,*) 'K1, AK, XSQ, X: ', K1, AK, XSQ, X
!        write(0,*) 'K1, BK, CK, K, YOLD(K): ', K1, BK, CK, K, YOLD(K)
!       write(0,*) '(b) K1, YNEW(K1): ', K1, YNEW(K1)

      END DO second_loop

      END SUBROUTINE SPLINE

!--------------------------------------------------------------------

!--------------------------------------------------------------------

   SUBROUTINE init_module_initialize
   END SUBROUTINE init_module_initialize

!---------------------------------------------------------------------

   subroutine vcgenerator(LM,pt,ptsgm,pdtop,lpt2,sgm,sg1,dsg1,sgml1,sg2,dsg2,sgml2,&
                          print_it,auto_define)
!     *************************************************************
!     *                                                           *
!     *  hybrid vertical coordinate generator                     *
!     *  programer z.janjic, ncep 2008                            *
!     *                                                           *
!     *************************************************************
!-----------------------------------------------------------------------
implicit none
!-----------------------------------------------------------------------
! include 'modelgrid.inc'
!-----------------------------------------------------------------------
real(kind=4),parameter:: &
 g=9.8,r=287.04,gor=g/r

integer(kind=4):: &
 kold,l,lpt2,lsal,LM

real(kind=4) &
 bsg,es,pdtop,pl,pbot,pt2,rp,rs,rsg,rsum,sum,ptsgm,pt

real(kind=4),dimension(1:10):: &
 xold,dold,y2s,pps,qqs

real(kind=4),dimension(1:lm):: &
 dsg,dsg1,dsg2,sgml1,sgml2,xnew

real(kind=4),dimension(1:lm+1):: &
 a,b,sg1,sg2,sgm,sgwork

logical :: print_it, auto_define

!-----------------------------------------------------------------------
!      print*,'*** Hi, this is your vertical coordinate generator.'
!--set rel. thicknesses of ref. sigma layers at significant points------

	if ( auto_define ) then
	if (print_it) write(0,*) 'spline defining the layer interfaces'
      xold(1)=.00
      xold(2)=.30 !.25
      xold(3)=.40 !.35
      xold(4)=.55 !.50
      xold(5)=.80
      xold(6)=1.
!
      dold(1)=.002
      dold(2)=.008
      dold(3)=.010
      dold(4)=.018
      dold(5)=.008
      dold(6)=.0025
!
!      dold(1)=.002
!      dold(2)=.008
!      dold(3)=.01
!      dold(4)=.016
!      dold(5)=.008
!      dold(6)=.002
!
      kold=6
!
      do l=1,lm
        xnew(l)=float(l-1)/float(lm-1)
      enddo
!
      do l=1,10
        y2s(l)=0.
      enddo
!
      call spline(kold,xold,dold,y2s,lm,xnew,dsg,pps,qqs)
!
      sum=0.
      do l=1,lm
        sum=sum+dsg(l)
      enddo
!
      rsum=1./sum
      sgm(1)=0.
      do l=1,lm-1
        dsg(l)=dsg(l)*rsum
        sgm(l+1)=sgm(l)+dsg(l)
      enddo
      sgm(lm+1)=1.
      dsg(lm)=sgm(lm+1)-sgm(lm)
!--print reference sigma and thicknesses of the layers------------------
      if (print_it) then
      print*,' reference sigma, l and sigma increase top down'
      do l=1,lm+1
        print*,'l=',l,' sgm(l)=',sgm(l)
      enddo
      do l=1,lm
        print*,'l=',l,' dsg(l)=',dsg(l)
      enddo
	endif

      else  ! reverse order of provided levels

	do L=1,LM+1
!        sgwork(LM+2-L)=sgm(L)
	write(0,*) 'ALREADY HAVE L, SGM(L): ',  L, SGM(L)
        enddo

!	do L=1,LM+1
!        sgm(L)=sgwork(L)
!        enddo

      endif ! auto_define
!-----------------------------------------------------------------------
      pbot=101300. ! reference mslp
      lpt2=0. ! interface below pressure range, # of pressure layers + 1
!
      do l=1,lm+1
        pl=sgm(l)*(pbot-pt)+pt
        if(pl.lt.ptsgm) lpt2=l
      enddo
!
      if(lpt2.gt.0) then
	if (print_it) write(0,*) 'LPT2, SGM(LPT2): ', LPT2, SGM(LPT2)
        pt2=sgm(lpt2)*(101300.-pt)+pt ! transition point
      else
        pt2=pt                        ! there are no pressure layers
      endif
      if (print_it) print*,'*** Mixed system starts at ',pt2,' Pa, from level ',lpt2
      pdtop=pt2-pt 
!--pressure range-------------------------------------------------------
      do l=1,lpt2
        a(l)=sgm(l)/sgm(lpt2)*pdtop+pt
        b(l)=0.
      enddo
!--mixed range----------------------------------------------------------
      lsal=0 !1 ! # of sangster-arakawa-lamb layers at the botom
      rp=2.2 !2. !2.2
      rs=1.2 !1.1 !1.2 !1. !1.35
      es=9. !10. !5. !10. !5.
      do l=lpt2+1,lm+1-lsal
        bsg=(sgm(l)-sgm(lpt2))/(1.-sgm(lpt2))
        rsg=rp+(rs-rp)/atan(es)*atan(es*bsg)
        b(l)=bsg**rsg
        a(l)=(sgm(l)-b(l))*(pbot-pt)+pt
      enddo
      do l=lm+1-lsal+1,lm+1
        b(l)=(sgm(l)-sgm(lpt2))/(1.-sgm(lpt2))
        a(l)=(sgm(l)-b(l))*(pbot-pt)+pt
      enddo
!--redefined equivalent sigma-------------------------------------------
      do l=1,lm+1
        sgm(l)=(a(l)-pt+b(l)*(pbot-pt))/(pbot-pt)
      if (print_it) print*, 'equivalent sgm(',l,')=',sgm(l)
      enddo
!--define NMM's sg1 and sg2---------------------------------------------
      do l=1,lm+1
        sg1(l)=(a(l)-pt)/pdtop
        sg2(l)=b(l)
      enddo
!--define NMM's dsg1, dsg2, sgml1, sgml2--------------------------------
      do l=1,lm
        dsg1(l)=sg1(l+1)-sg1(l)
        dsg2(l)=sg2(l+1)-sg2(l)
        sgml1(l)=(sg1(l)+sg1(l+1))*0.5
        sgml2(l)=(sg2(l)+sg2(l+1))*0.5
      enddo
!--print resulting vertical discretization------------------------------
      if (print_it) then
      do l=1,lm+1
        print*, '   sg1(',l,')=',  sg1(l),'   sg2(',l,')=',  sg2(l)
      enddo
      do l=1,lm
        print*, '  dsg1(',l,')=', dsg1(l),'  dsg2(',l,')=', dsg2(l)
      enddo
      do l=1,lm
        print*, ' sgml1(',l,')=',sgml1(l),' sgml2(',l,')=',sgml2(l)
      enddo
!--test surface pressures-----------------------------------------------
      do l=1,lm+1
        print*,a(l),b(l),a(l)+b(l)*(100000.-pt)
      enddo
      print*,'midlayer pressures, 100000 pa'
      do l=1,lm
        print*,(a(l+1)+b(l+1)*(100000.-pt)+a(l)+b(l)*(100000.-pt))*0.5
      enddo
      print*,'layer thhicknesses, 100000 pa'
      do l=1,lm
        print*,a(l+1)+b(l+1)*(100000.-pt)-a(l)-b(l)*(100000.-pt)
      enddo
      print*,'midlayer pressures, 75000 pa'
      do l=1,lm
        print*,(a(l+1)+b(l+1)*(75000.-pt)+a(l)+b(l)*(75000.-pt))*0.5
      enddo
      print*,'layer thhicknesses, 75000 pa'
      do l=1,lm
        print*,a(l+1)+b(l+1)*(75000.-pt)-a(l)-b(l)*(75000.-pt)
      enddo
      print*,'midlayer pressures, 50000 pa'
      do l=1,lm
        print*,(a(l+1)+b(l+1)*(50000.-pt)+a(l)+b(l)*(50000.-pt))*0.5
      enddo
      print*,'layer thhicknesses, 50000 pa'
      do l=1,lm
        print*,a(l+1)+b(l+1)*(50000.-pt)-a(l)-b(l)*(50000.-pt)
      enddo
      endif
!-----------------------------------------------------------------------
!      open(unit=1,file='../output/dsg' &
!          ,status='unknown',form='unformatted')
!      write(1) pdtop,lpt2,sgm,sg1,dsg1,sgml1,sg2,dsg2,sgml2
!      close(1)
!-----------------------------------------------------------------------
     end subroutine vcgenerator

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------


   subroutine salgenerator(LM,pt,ptsgm,pdtop,lpt2,sgm,sg1,dsg1,sgml1,sg2,dsg2,sgml2,&
                           print_it,auto_define)
!     *************************************************************
!     *                                                           *
!     *  sal hybrid vertical coordinate generator                 *
!     *  programer z.janjic, ncep 2008                            *
!     *                                                           *
!     *************************************************************
!-----------------------------------------------------------------------
implicit none
!-----------------------------------------------------------------------
! include 'modelgrid.inc'
!-----------------------------------------------------------------------
real(kind=4),parameter:: &
 g=9.8,r=287.04,gor=g/r

integer(kind=4):: &
 kold,l,lpt2,lsal,LM

real(kind=4) &
 bsg,es,pdtop,pl,pbot,pt2,rp,rs,rsg,rsum,sum,pt,ptsgm

real(kind=4),dimension(1:10):: &
 xold,dold,y2s,pps,qqs

real(kind=4),dimension(1:lm):: &
 dsg,dsg1,dsg2,sgml1,sgml2,xnew

real(kind=4),dimension(1:lm+1):: &
 a,b,sg1,sg2,sgm

logical :: print_it, auto_define
!-----------------------------------------------------------------------
!      print*,'*** Hi, this is your vertical coordinate generator.'
!--set rel. thicknesses of ref. sigma layers at significant points------

	if (auto_define) then
      xold(1)=.00
      xold(2)=.30 !.25
      xold(3)=.40 !.35
      xold(4)=.55 !.50
      xold(5)=.80
      xold(6)=1.
!
      dold(1)=.002
      dold(2)=.008
      dold(3)=.008
      dold(4)=.016
      dold(5)=.008
      dold(6)=.0025
!
!      dold(1)=.002
!      dold(2)=.008
!      dold(3)=.01
!      dold(4)=.016
!      dold(5)=.008
!      dold(6)=.002
!
      kold=6
!
      do l=1,lm
        xnew(l)=float(l-1)/float(lm-1)
      enddo
!
      do l=1,10
        y2s(l)=0.
      enddo
!
      call spline(kold,xold,dold,y2s,lm,xnew,dsg,pps,qqs)
!
      sum=0.
      do l=1,lm
        sum=sum+dsg(l)
      enddo
!
      rsum=1./sum
      sgm(1)=0.
      do l=1,lm-1
        dsg(l)=dsg(l)*rsum
        sgm(l+1)=sgm(l)+dsg(l)
      enddo
      sgm(lm+1)=1.
      dsg(lm)=sgm(lm+1)-sgm(lm)
!--print reference sigma and thicknesses of the layers------------------
     if (print_it) then
      print*,' reference sigma, l and sigma increase top down'
      do l=1,lm+1
        print*,'l=',l,' sgm(l)=',sgm(l)
      enddo
      do l=1,lm
        print*,'l=',l,' dsg(l)=',dsg(l)
      enddo
      endif

      endif ! auto_define
!-----------------------------------------------------------------------
      pbot=101300. ! reference mslp
      lpt2=0. ! interface below pressure range, # of pressure layers + 1
!
      do l=1,lm+1
        pl=sgm(l)*(pbot-pt)+pt
        if(pl.lt.ptsgm) lpt2=l
      enddo
!
      if(lpt2.gt.0) then
        pt2=sgm(lpt2)*(101300.-pt)+pt ! transition point
      else
        pt2=pt                        ! there are no pressure layers
      endif
      print*,'*** Mixed system starts at ',pt2,' Pa, from level ',lpt2
      pdtop=pt2-pt 
!--pressure range-------------------------------------------------------
      do l=1,lpt2
        a(l)=sgm(l)/sgm(lpt2)*pdtop+pt
        b(l)=0.
      enddo
!--sigma range----------------------------------------------------------
      do l=lpt2+1,lm+1
        b(l)=(sgm(l)-sgm(lpt2))/(1.-sgm(lpt2))
        a(l)=(sgm(l)-b(l))*(pbot-pt)+pt
      enddo
!--define NMM's sg1 and sg2---------------------------------------------
      do l=1,lm+1
        sg1(l)=(a(l)-pt)/pdtop
        sg2(l)=b(l)
      enddo
!--define NMM's dsg1, dsg2, sgml1, sgml2--------------------------------
      do l=1,lm
        dsg1(l)=sg1(l+1)-sg1(l)
        dsg2(l)=sg2(l+1)-sg2(l)
        sgml1(l)=(sg1(l)+sg1(l+1))*0.5
        sgml2(l)=(sg2(l)+sg2(l+1))*0.5
      enddo
!--print resulting vertical discretization------------------------------
      if (print_it) then
      do l=1,lm+1
        print*, '   sg1(',l,')=',  sg1(l),'   sg2(',l,')=',  sg2(l)
      enddo
      do l=1,lm
        print*, '  dsg1(',l,')=', dsg1(l),'  dsg2(',l,')=', dsg2(l)
      enddo
      do l=1,lm
        print*, ' sgml1(',l,')=',sgml1(l),' sgml2(',l,')=',sgml2(l)
      enddo
!--test surface pressures-----------------------------------------------
      do l=1,lm+1
        print*,a(l),b(l),a(l)+b(l)*(100000.-pt)
      enddo
      print*,'midlayer pressures, 100000 pa'
      do l=1,lm
        print*,(a(l+1)+b(l+1)*(100000.-pt)+a(l)+b(l)*(100000.-pt))*0.5
      enddo
      print*,'layer thhicknesses, 100000 pa'
      do l=1,lm
        print*,a(l+1)+b(l+1)*(100000.-pt)-a(l)-b(l)*(100000.-pt)
      enddo
      print*,'midlayer pressures, 75000 pa'
      do l=1,lm
        print*,(a(l+1)+b(l+1)*(75000.-pt)+a(l)+b(l)*(75000.-pt))*0.5
      enddo
      print*,'layer thhicknesses, 75000 pa'
      do l=1,lm
        print*,a(l+1)+b(l+1)*(75000.-pt)-a(l)-b(l)*(75000.-pt)
      enddo
      print*,'midlayer pressures, 50000 pa'
      do l=1,lm
        print*,(a(l+1)+b(l+1)*(50000.-pt)+a(l)+b(l)*(50000.-pt))*0.5
      enddo
      print*,'layer thhicknesses, 50000 pa'
      do l=1,lm
        print*,a(l+1)+b(l+1)*(50000.-pt)-a(l)-b(l)*(50000.-pt)
      enddo

      endif
!-----------------------------------------------------------------------
!      open(unit=1,file='../output/dsg' &
!          ,status='unknown',form='unformatted')
!      write(1) pdtop,lpt2,sgm,sg1,dsg1,sgml1,sg2,dsg2,sgml2
!      close(1)
!-----------------------------------------------------------------------
     end subroutine salgenerator

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

   subroutine gfsgenerator(LM,pt,ptsgm,pdtop,lpt2,sgm,sg1,dsg1,sgml1,sg2,dsg2,sgml2,&
                           print_it, auto_define)
!-----------------------------------------------------------------------
implicit none
!--nmm variables--------------------------------------------------------
! include 'modelgrid.inc'
!
integer(kind=4):: &
 kflip,l,lpt2,LM

real(kind=4) &
 pdtop,pt2,pt,ptsgm

real(kind=4),dimension(1:10):: &
 xold,dold,y2s,pps,qqs

real(kind=4),dimension(1:lm):: &
 dsg,dsg1,dsg2,sgml1,sgml2,xnew

real(kind=4),dimension(1:lm+1):: &
 a,b,sg1,sg2,sgm

logical :: print_it, auto_define

!--gfs variables--------------------------------------------------------
integer levs,lupp,k
real pbot,psig,ppre,pupp,ptop,dpbot,dpsig,dppre,dpupp,dptop,pmin
real,allocatable:: ak(:),bk(:)
!-----------------------------------------------------------------------
!write(0,*) 'Enter levs,lupp,pbot,psig,ppre,pupp,ptop,dpbot,dpsig,dppre,dpupp,dptop'
!read *,levs,lupp,pbot,psig,ppre,pupp,ptop,dpbot,dpsig,dppre,dpupp,dptop
!-----------------------------------------------------------------------
      levs=lm ! # of layers
!-----------------------------------------------------------------------
      allocate(ak(0:levs),bk(0:levs))
!-----------------------------------------------------------------------
      lupp=levs
      pbot=100000.-pt ! surface pressure-pt
      psig=99400.-pt ! lowest interface above surface-pt
      ppre=ptsgm-pt ! pressure-hybrid transition point-pt
      pupp=0.
      ptop=0.
      dpbot=500.
      dpsig=1200.
      dppre=18000.
      dpupp=60.
      dptop=60.
!-----------------------------------------------------------------------
      call akbkgen &
      (levs,lupp,pbot,psig,ppre &
      ,pupp,ptop,dpbot,dpsig,dppre,dpupp,dptop &
      ,pmin,ak,bk)
!-----------------------------------------------------------------------
	if (print_it) then
      write(0,*) 'pmin=',pmin
      print '(2i6)',2,levs
      print '(f12.3,f12.8)',(ak(k),bk(k),k=0,levs)
        endif
!--flip gfs's ak, bk into nmm's a, b------------------------------------
      do l=1,lm+1
        kflip=lm+1-l
        a(l)=ak(kflip)+pt ! to account for reduced pbot
        b(l)=bk(kflip)
      enddo
!--retrieve required nmm parameters-------------------------------------
      do l=1,lm+1
        if(b(l).gt.1.e-7) then
          exit
        else
          b(l)=0.
          lpt2=l
        endif
      enddo
!
      pt2=a(lpt2)
      pdtop=pt2-pt
      print*,'*** Mixed system starts at ',pt2,' Pa, from level ',lpt2
      print*, 'pdtop within gfs code: ', pdtop
!
      do l=1,lm+1
        sgm(l)=(a(l)-pt+b(l)*(pbot-pt))/(pbot-pt) 
	if (print_it) then
        print*, '   sgm(',l,')=',sgm(l)
	endif
      enddo
!--define NMM's sg1 and sg2---------------------------------------------
      do l=1,lm+1
        sg1(l)=(a(l)-pt)/pdtop
        sg2(l)=b(l)
      enddo
!--define NMM's dsg1, dsg2, sgml1, sgml2--------------------------------
      do l=1,lm
        dsg1(l)=sg1(l+1)-sg1(l)
        dsg2(l)=sg2(l+1)-sg2(l)
        sgml1(l)=(sg1(l)+sg1(l+1))*0.5
        sgml2(l)=(sg2(l)+sg2(l+1))*0.5
      enddo
!--print resulting vertical discretization------------------------------
      if (print_it) then
      do l=1,lm+1
        print*, '   sg1(',l,')=',  sg1(l),'   sg2(',l,')=',  sg2(l)
      enddo
      do l=1,lm
        print*, '  dsg1(',l,')=', dsg1(l),'  dsg2(',l,')=', dsg2(l)
      enddo
      do l=1,lm
        print*, ' sgml1(',l,')=',sgml1(l),' sgml2(',l,')=',sgml2(l)
      enddo
!--test surface pressures-----------------------------------------------
      do l=1,lm+1
        print*,a(l),b(l),a(l)+b(l)*(100000.-pt)
      enddo
      print*,'midlayer pressures, 100000 pa'
      do l=1,lm
        print*,(a(l+1)+b(l+1)*(100000.-pt)+a(l)+b(l)*(100000.-pt))*0.5
      enddo
      print*,'layer thhicknesses, 100000 pa'
      do l=1,lm
        print*,a(l+1)+b(l+1)*(100000.-pt)-a(l)-b(l)*(100000.-pt)
      enddo
      print*,'midlayer pressures, 75000 pa'
      do l=1,lm
        print*,(a(l+1)+b(l+1)*(75000.-pt)+a(l)+b(l)*(75000.-pt))*0.5
      enddo
      print*,'layer thhicknesses, 75000 pa'
      do l=1,lm
        print*,a(l+1)+b(l+1)*(75000.-pt)-a(l)-b(l)*(75000.-pt)
      enddo
      print*,'midlayer pressures, 50000 pa'
      do l=1,lm
        print*,(a(l+1)+b(l+1)*(50000.-pt)+a(l)+b(l)*(50000.-pt))*0.5
      enddo
      print*,'layer thhicknesses, 50000 pa'
      do l=1,lm
        print*,a(l+1)+b(l+1)*(50000.-pt)-a(l)-b(l)*(50000.-pt)
      enddo
       endif
!-----------------------------------------------------------------------
!      open(unit=1,file='../output/dsg' &
!          ,status='unknown',form='unformatted')
!      write(1) pdtop,lpt2,sgm,sg1,dsg1,sgml1,sg2,dsg2,sgml2
!      close(1)
!-----------------------------------------------------------------------
     end subroutine gfsgenerator


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!$$$  Subprogram documentation block
!
! Subprogram:    akbkgen     Generates hybrid coordinate interface profiles
!   Prgmmr: Iredell    Org: W/NP23      Date: 2008-08-01
!
! Abstract: This subprogram generates hybrid coordinate interface profiles
!   from a few given parameters. The hybrid coordinate is intended to start
!   out at the bottom in pure sigma and end up at the top in pure pressure,
!   with a smooth transition in between. The pressure thickness is close to
!   quadratic in pressure, with maximum thicknesses in the middle of the domain.
!   The coordinate pressure will have continuous second derivatives in level.
!
!   The hybrid coordinate is returned in terms of vectors AK and BK, where
!   the interface pressure is defined as A+B*ps where ps is surface pressure
!   Thus A=0 in regions of pure sigma and B=0 in regions of pure sigma.
!   At the bottom, A(0)=0 and B(0)=1 so that surface pressure is the bottom
!   boundary condition, while at the top, A(levs)=ptop and B(levs)=0 so that
!   the constant top pressure (which can be zero) is the top boundary condition.
!
!   The procedure for the calculation is described in the remarks section below.
!
! Program history log:
!   2008-08-01  Mark Iredell
!
! Usage:    call akbkgen(levs,lupp,pbot,psig,ppre,pupp,ptop,&
!                        dpbot,dpsig,dppre,dpupp,dptop,pmin,ak,bk)
!   Input argument list:
!     levs     integer number of levels
!     lupp     integer number of levels below pupp
!     pbot     real nominal surface pressure (Pa)
!     psig     real nominal pressure where coordinate changes
!              from pure sigma (Pa)
!     ppre     real nominal pressure where coordinate changes
!              to pure pressure (Pa)
!     pupp     real nominal pressure where coordinate changes
!              to upper atmospheric profile (Pa)
!     ptop     real pressure at top (Pa)
!     dpbot    real coordinate thickness at bottom (Pa)
!     dpsig    real thickness of zone within which coordinate changes
!              to pure sigma (Pa)
!     dppre    real thickness of zone within which coordinate changes
!              to pure pressure (Pa)
!     dpupp    real coordinate thickness at pupp (Pa)
!     dptop    real coordinate thickness at top (Pa)
!   Output argument list:
!     pmin     real minimum surface pressure (Pa)
!     ak       real(0:levs) a coordinate values, bottom to top (Pa)
!     bk       real(0:levs) b coordinate values, bottom to top ()
!
! Subprograms called:
!     ludcmp   lower and upper triangular decomposition
!     lubksb   lower and upper triangular back substitution
!
! Attributes:
!   Language: Fortran 90
!
! Remarks:
!   Example: Create the operational GFS 64-level hybrid coordinate.
!      call akbkgen(64,100000.,500.,60.,7000.,18000.,99400.,1200.,pmin,ak,bk)
!
!   Graphical description of parameters and zones:
!     ptop  ---  -----  ----------------------
!           ...  dptop
!           ---         zone U (upper atmos)
!           ...
!     pupp  ---  -----  ----------------------
!           ...  dpupp
!           ---  -----
!           ...         zone P (pure pressure)
!           ---
!           ...
!     ppre  ---  -----  ----------------------
!           ...
!           ---  dppre  zone T1 (transition 1)
!           ...
!           ---  -----  ----------------------
!           ...
!           ---
!           ...         zone T2 (transition 2)
!           ---
!           ...
!           ---  -----  ----------------------
!           ...
!           ---  dpsig  zone T3 (transition 3)
!           ...
!     psig  ---  -----  ----------------------
!           ...
!           ---  -----  zone S (pure sigma)
!           ...  dpbot
!     pbot  ---  -----  ----------------------
!
!   Detailed procedure description:
!     STEP 1.
!   The pressure profile is computed with respect to the given reference
!   surface pressure pbot. For this surface pressure, the 'sigma' thicknesses
!   dsig are assumed to be proportional to a quadratic polynomial in sigma sig
!   with zero intercepts sig1 and sig2 somewhere below and above the model
!   domain, respectively. That is,
!     dsig ~ (sig2-sig)*(sig-sig1)*dk
!   Integrating this differential equation gives
!     sig = (sig1*exp(c1*k+c2)+sig2)/(exp(c1*k+c2)+1)
!   The required boundary conditions sig(0)=1 and sig(levs)=0
!   fix the proportionality and integration constants c1 and c2.
!   The two crossing parameters (sig1 and sig2) are determined
!   by two input sigma thickness conditions dsig/dk at the bottom and top
!   which are respectively given as dpbot/(pbot-ptop) and dptop/(pbot-ptop).
!   The crossing parameters are computed using Newton-Raphson iteration.
!   This procedure fixes the pressure profile for surface pressure pbot.
!     STEP 2.
!   The pressure profile is computed with respect to a minimum surface pressure.
!   This minimum surface pressure pmin is yet to be determined.
!   Divide the profile into zones:
!     zone P (pure pressure) from ptop to ppre
!     zone T1 (transition 1) from ppre to ppre+dppre
!     zone T2 (transition 2) from ppre+dppre to psig-dpsig
!     zone T3 (transition 3) from psig-dpsig to psig
!     zone S (pure "sigma") from psig to pmin
!     (here sigma=p/ps so that d(ln(p))/dk is horizontally uniform)
!   The pressure profile in the pure pressure zone P is set from step 1.
!   The pressure thicknesses in zone T1 is set to be quadratic in level k.
!   The pressure thicknesses in zone T2 is set to be linear in level k.
!   The pressure thicknesses in zone T3 is set to be quadratic in level k.
!   The pressure profile in the pure sigma zone S is also set from step 1.
!   Thus there are nine unknowns:
!     the 3 polynomial coefficients in zone T1
!     the 2 polynomial coefficients in zone T2
!     the 3 polynomial coefficients in zone T3
!     and the 1 minimum surface pressure.
!   The nine conditions to determine these unknowns are:
!     the thickness and its derivative match at zone P and T1 boundary
!     the thickness and its derivative match at zone T1 and T2 boundary
!     the thickness and its derivative match at zone T2 and T3 boundary
!     the thickness and its derivative match at zone T3 and S boundary
!     the sum of the thicknesses of zones T1, T2, T3, and S is pmin-ppre
!   The unknowns are computed using standard linear decomposition.
!   This procedure fixes the pressure profile for surface pressure pmin.
!     STEP 3.
!   For an arbitrary surface pressure, the pressure profile is an linear
!   combination of the pressure profiles for surface pressures pbot and pmin
!     p(psfc)=p(pbot)*(psfc-pmin)/(pbot-pmin)+p(pmin)*(pbot-psfc)/(pbot-pmin)
!   from which the hybrid coordinate profiles ak and bk are found such that
!     p(psfc)=ak+bk*psfc
!
!$$$
subroutine akbkgen(levs,lupp,pbot,psig,ppre,pupp,ptop,&
                   dpbot,dpsig,dppre,dpupp,dptop,pmin,ak,bk)
  implicit none
  integer,intent(in):: levs,lupp
  real,intent(in):: pbot,psig,ppre,pupp,ptop
  real,intent(in):: dpbot,dpsig,dppre,dpupp,dptop
  real,intent(out):: pmin,ak(0:levs),bk(0:levs)
  integer,parameter:: lo=100,li=10  ! outer and inner N-R iterations
  real pdif        ! thickness from pbot to ptop
  real delb        ! delta sig at bot
  real delt        ! delta sig at top
  real sig1        ! crossing parameter 1
  real sig2        ! crossing parameter 2
  real c1          ! proportionality constant
  real c2          ! integration constant
  real sig         ! sig variable
  real dsig        ! delta sig variable
  real delbio0     ! initial guess at delta sig at bot
  real deltio0     ! initial guess at delta sig at top
  real delbio      ! guess at delta sig at bot
  real deltio      ! guess at delta sig at top
  real c1sig1      ! d(c1)/d(sig1)
  real c1sig2      ! d(c1)/d(sig2)
  real p(2)        ! rhs in N-R iteration
  real fjac(2,2)   ! lhs in N-R iteration
  integer indx(2)  ! permutations in N-R iteration
  real ppred       ! pressure at T1-T2 boundary
  real spre        ! sig at P-T1 boundary
  real spred       ! sig at T1-T2 boundary
  real rkpre       ! level at P-T1 boundary
  real rkpred      ! level at T1-T2 boundary
  real pkpre       ! dp/dk at P-T1 boundary
  real pkkpre      ! d2p/dk2 at P-T1 boundary
  real psigd       ! pressure at T2-T3 boundary
  real ssig        ! sig at T3-S boundary
  real ssigd       ! sig at T2-T3 boundary
  real rksig       ! level at T3-S boundary
  real rksigd      ! level at T2-T3 boundary
  real pksig       ! dp/dk at T3-S boundary
  real pkksig      ! d2p/dk2 at T3-S boundary
  real p2sig       ! pressure at T3-S boundary for pmin surface pressure
  real p2sigd      ! pressure at T2-T3 boundary for pmin surface pressure
  real p2pred      ! pressure at T1-T2 boundary for pmin surface pressure
  real x2(9)       ! rhs in linear solver
  real a2(9,9)     ! lhs in linear solver
  integer indx2(9) ! permutations in linear solver
  real pkupp       ! dp/dk at U-P boundary
  real pkkupp      ! d2p/dk2 at U-P boundary
  real x3(3)       ! rhs in linear solver
  real a3(3,3)     ! lhs in linear solver
  integer indx3(3) ! permutations in linear solver
  real p1          ! pressure variable for pbot surface pressure
  real p2          ! pressure variable for pmin surface pressure
  real d           ! determinant permutation
  integer io,ii,k
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  STEP 1.
  pdif=pbot-pupp
  delb=dpbot/pdif
  delt=dpupp/pdif
   write(9,*) 'pdif,delb,delt=',pdif,delb,delt
  sig1=1+delb
  sig2=-delt
  c1=log(-sig2*(1-sig1)/sig1/(sig2-1))/lupp
  c2=log((sig2-1)/(1-sig1))
  sig=1
  dsig=(sig2-sig)*(sig-sig1)*c1/(sig1-sig2)
  delbio0=-dsig
  sig=0
  dsig=(sig2-sig)*(sig-sig1)*c1/(sig1-sig2)
  deltio0=-dsig
!  Newton-Raphson iterations
  do io=1,lo
    delbio=delbio0+(delb-delbio0)*io/lo
    deltio=deltio0+(delt-deltio0)*io/lo
    do ii=1,li
      c1sig1=-1/(sig1*(1-sig1)*lupp)
      c1sig2=-1/(sig2*(sig2-1)*lupp)
      sig=1
      dsig=(sig2-sig)*(sig-sig1)*c1/(sig1-sig2)
      p(1)=-delbio-dsig
      fjac(1,1)=((-c1*(sig+sig2)+(sig-sig1)*c1sig1*(sig1+sig2)) &
                *(sig2-sig)/(sig1+sig2)**2)
      fjac(1,2)=((c1*(sig+sig1)+(sig2-sig)*c1sig2*(sig1+sig2)) &
                *(sig-sig1)/(sig1+sig2)**2)
      sig=0
      dsig=(sig2-sig)*(sig-sig1)*c1/(sig1-sig2)
      p(2)=-deltio-dsig
      fjac(2,1)=((-c1*(sig+sig2)+(sig-sig1)*c1sig1*(sig1+sig2)) &
                *(sig2-sig)/(sig1+sig2)**2)
      fjac(2,2)=((c1*(sig+sig1)+(sig2-sig)*c1sig2*(sig1+sig2)) &
                *(sig-sig1)/(sig1+sig2)**2)
      call ludcmp(fjac,2,2,indx,d)
      call lubksb(fjac,2,2,indx,p)
      sig1=sig1+p(1)
      sig2=sig2+p(2)
      c1=log(-sig2*(1-sig1)/sig1/(sig2-1))/lupp
      c2=log((sig2-1)/(1-sig1))
   !write(9,*) 'io,ii,sig1,sig2,c1,c2=',io,ii,sig1,sig2,c1,c2
    enddo
  enddo
   write(9,*) 'sig1,sig2,c1,c2=',sig1,sig2,c1,c2
   write(9,*) 'pktop=',pdif*c1*(sig2-0)*(0-sig1)/(sig1-sig2)
   k=0
   sig=(sig1*exp(c1*k+c2)+sig2)/(exp(c1*k+c2)+1)
   write(9,*) 'pk0=',pdif*c1*(sig2-sig)*(sig-sig1)/(sig1-sig2)
   k=59
   sig=(sig1*exp(c1*k+c2)+sig2)/(exp(c1*k+c2)+1)
   write(9,*) 'pk59=',pdif*c1*(sig2-sig)*(sig-sig1)/(sig1-sig2)
   !stop 1
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  STEP 2.
!  Compute minimum surface pressure
  ppred=ppre+dppre
  spre=(ppre-pupp)/pdif
  spred=(ppred-pupp)/pdif
  rkpre=(log((spre-sig2)/(sig1-spre))-c2)/c1
  rkpred=(log((spred-sig2)/(sig1-spred))-c2)/c1
  pkpre=pdif*c1*(sig2-spre)*(spre-sig1)/(sig1-sig2)
  pkkpre=pkpre*c1*(sig2+sig1-2*spre)/(sig1-sig2)
  psigd=psig-dpsig
  ssig=(psig-pupp)/pdif
  ssigd=(psigd-pupp)/pdif
  rksig=(log((ssig-sig2)/(sig1-ssig))-c2)/c1
  rksigd=(log((ssigd-sig2)/(sig1-ssigd))-c2)/c1
  pksig=pdif*c1*(sig2-ssig)*(ssig-sig1)/(sig1-sig2)
  pkksig=pksig*c1*(sig2+sig1-2*ssig)/(sig1-sig2)
   write(9,*) 'psig,ssig,rksig=',psig,ssig,rksig
  x2=0
  a2=0
  x2(1)=pkpre
  a2(1,1)=1
  a2(1,2)=rkpre
  a2(1,3)=rkpre**2
  x2(2)=pkkpre
  a2(2,2)=1
  a2(2,3)=2*rkpre
  a2(3,1)=1
  a2(3,2)=rkpred
  a2(3,3)=rkpred**2
  a2(3,4)=-1
  a2(3,5)=-rkpred
  a2(4,2)=1
  a2(4,3)=2*rkpred
  a2(4,5)=-1
  a2(5,4)=-1
  a2(5,5)=-rksigd
  a2(5,6)=1
  a2(5,7)=rksigd
  a2(5,8)=rksigd**2
  a2(6,5)=-1
  a2(6,7)=1
  a2(6,8)=2*rksigd
  a2(7,6)=1
  a2(7,7)=rksig
  a2(7,8)=rksig**2
  a2(7,9)=-pksig/pbot
  a2(8,7)=1
  a2(8,8)=2*rksig
  a2(8,9)=-pkksig/pbot
  x2(9)=ppre
  a2(9,1)=(rkpre-rkpred)
  a2(9,2)=(rkpre**2-rkpred**2)/2
  a2(9,3)=(rkpre**3-rkpred**3)/3
  a2(9,4)=(rkpred-rksigd)
  a2(9,5)=(rkpred**2-rksigd**2)/2
  a2(9,6)=(rksigd-rksig)
  a2(9,7)=(rksigd**2-rksig**2)/2
  a2(9,8)=(rksigd**3-rksig**3)/3
  a2(9,9)=psig/pbot
  call ludcmp(a2,9,9,indx2,d)
  call lubksb(a2,9,9,indx2,x2)
  pmin=x2(9)
   write(9,*) 'pmin=',pmin
  p2sig=psig/pbot*pmin
  p2sigd=p2sig &
        +x2(6)*(rksigd-rksig) &
        +x2(7)*(rksigd**2-rksig**2)/2 &
        +x2(8)*(rksigd**3-rksig**3)/3
  p2pred=p2sigd &
        +x2(4)*(rkpred-rksigd) &
        +x2(5)*(rkpred**2-rksigd**2)/2
   !stop 2
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  STEP 3.
  if(lupp.lt.levs) then
    pkupp=pdif*c1*(sig2-0)*(0-sig1)/(sig1-sig2)
    pkkupp=pkupp*c1*(sig2+sig1-2*0)/(sig1-sig2)
    x3=0
    a3=0
    x3(1)=pkupp
    a3(1,1)=pupp
    x3(2)=pkkupp*pupp-pkupp**2
    a3(2,2)=pupp**2
    x3(3)=log((ptop+dptop)/pupp)
    a3(3,1)=levs-1-lupp
    a3(3,2)=(levs-1-lupp)**2/2
    a3(3,3)=(levs-1-lupp)**3/3
     write(9,*) 'pre-x3=',x3
     !stop 31
    call ludcmp(a3,3,3,indx3,d)
     write(9,*) 'at stop 32. a3=',a3
     write(9,*) 'at stop 32. x3=',x3
     !stop 32
    call lubksb(a3,3,3,indx3,x3)
     write(9,*) 'post-x3=',x3
     !stop 3
  endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  STEP 4.
!  Compute hybrid interface values
  ak(0)=0
  bk(0)=1
  do k=1,levs-1
    if(k.ge.lupp) then
      p1=pupp*exp(x3(1)*(k-lupp)+x3(2)*(k-lupp)**2/2+x3(3)*(k-lupp)**3/3)
    else
      p1=(sig1*exp(c1*k+c2)+sig2)/(exp(c1*k+c2)+1)*pdif+pupp
    endif
    if(k.ge.rkpre) then
      p2=p1
    elseif(k.ge.rkpred) then
      p2=p2pred+x2(1)*(k-rkpred) &
               +x2(2)*(k**2-rkpred**2)/2 &
               +x2(3)*(k**3-rkpred**3)/3
    elseif(k.ge.rksigd) then
      p2=p2sigd+x2(4)*(k-rksigd) &
               +x2(5)*(k**2-rksigd**2)/2
    elseif(k.ge.rksig) then
      p2=p2sig+x2(6)*(k-rksig) &
              +x2(7)*(k**2-rksig**2)/2 &
              +x2(8)*(k**3-rksig**3)/3
    else
      p2=p1/pbot*pmin
    endif
    ak(k)=(p2*pbot-p1*pmin)/(pbot-pmin)
    bk(k)=(p1-p2)/(pbot-pmin)
  enddo
  ak(levs)=ptop
  bk(levs)=0
end subroutine
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!-----------------------------------------------------------------------
!$$$  Subprogram documentation block
!
! Subprogram:    ludcmp      lower and upper triangular decomposition
!   Prgmmr: Iredell    Org: W/NP23      Date: 2008-08-01
!
! Abstract: This subprogram decomposes a matrix into a product of
!   lower and upper triangular matrices.
!
! Program history log:
!   2008-08-01  Mark Iredell
!
! Usage:    call ludcmp(a,n,np,indx,d)
!   Input argument list:
!     a        real(np,np) matrix (will be overwritten)
!     n        integer order of matrix
!     np       integer dimension of matrix
!
!   Output argument list:
!     a        real(np,np) LU-decomposed matrix
!              (U is upper part of A, including diagonal;
!               L is lower part of A, with 1 as diagonal;
!               L*U equals original A after permuting)
!     indx     integer(n) pivot indices
!              (original A rows are permuted in order i with indx(i))
!     d        real determinant permutation (1 or -1, or 0 if singular)
!              (determinant is output diagonal product times d)
!
! Attributes:
!   Language: Fortran 90
!
!$$$
subroutine ludcmp(a,n,np,indx,d)
  implicit none
  integer,intent(in):: n,np
  real,intent(inout):: a(np,np)
  integer,intent(out):: indx(n)
  real,intent(out):: d
  integer i,j,k,imax
  real aamax,sum,dum
  real vv(n)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  d=1
  do i=1,n
    aamax=0
    do j=1,n
      if(abs(a(i,j))>aamax) aamax=abs(a(i,j))
    enddo
    if(aamax==0) then
      d=0
      return
    endif
    vv(i)=1/aamax
  enddo
  do j=1,n
    do i=1,j-1
      sum=a(i,j)
      do k=1,i-1
        sum=sum-a(i,k)*a(k,j)
      enddo
      a(i,j)=sum
    enddo
    aamax=0.
    do i=j,n
      sum=a(i,j)
      do k=1,j-1
        sum=sum-a(i,k)*a(k,j)
      enddo
      a(i,j)=sum
      dum=vv(i)*abs(sum)
      if(dum>=aamax) then
        imax=i
        aamax=dum
      endif
    enddo
    if (j/=imax)then
      do k=1,n
        dum=a(imax,k)
        a(imax,k)=a(j,k)
        a(j,k)=dum
      enddo
      d=-d
      vv(imax)=vv(j)
    endif
    indx(j)=imax
    if(a(j,j)==0) then
      d=0
      return
    endif
    if(j/=n)then
      dum=1/a(j,j)
      do i=j+1,n
        a(i,j)=a(i,j)*dum
      enddo
    endif
  enddo
end subroutine
!-------------------------------------------------------------------------------
!$$$  Subprogram documentation block
!
! Subprogram:    lubksb      lower and upper triangular back substitution
!   Prgmmr: Iredell    Org: W/NP23      Date: 2008-08-01
!
! Abstract: This subprogram back substitutes to solve decomposed
!   lower and upper triangular matrices as outputted by ludcmp.
!
! Program history log:
!   2008-08-01  Mark Iredell
!
! Usage:    call lubksb(a,n,np,indx,b)
!   Input argument list:
!     a        real(np,np) LU-decomposed matrix
!              (from ludcmp)
!     n        integer order of matrix
!     np       integer dimension of matrix
!     indx     integer(n) pivot indices
!              (from ludcmp)
!     b        real(n) rhs vector of linear problem (will be overwritten)
!
!   Output argument list:
!     b        real(n) solution of linear problem
!              (original A times output B equals original B)
!
! Attributes:
!   Language: Fortran 90
!
!$$$
subroutine lubksb(a,n,np,indx,b)
  implicit none
  integer,intent(in):: n,np
  real,intent(in):: a(np,np)
  integer,intent(in):: indx(n)
  real,intent(inout):: b(n)
  integer i,j,ii,ll
  real sum
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ii=0
  do i=1,n
    ll=indx(i)
    sum=b(ll)
    b(ll)=b(i)
    if (ii/=0)then
      do j=ii,i-1
        sum=sum-a(i,j)*b(j)
      enddo
    elseif(sum/=0) then
      ii=i
    endif
    b(i)=sum
  enddo
  do i=n,1,-1
    sum=b(i)
    do j=i+1,n
      sum=sum-a(i,j)*b(j)
    enddo
    b(i)=sum/a(i,i)
  enddo
end subroutine

! ------------------------------------------------------------------------

      SUBROUTINE bgrid_boundary_winds (pd, u, v, psl_in, u_in, v_in, &
     &                            dsg1, dsg2, PDTOP,pt,             &
     &                            IDS,IDE,JDS,JDE,KDS,KDE,          &
     &                            IMS,IME,JMS,JME,KMS,KME,          &
     &                            ITS,ITE,JTS,JTE,KTS,KTE,LNSV, num_metgrid_levels, print_diag  )

	REAL, INTENT(IN):: PD(IMS:IME,JMS:JME)
       REAL, INTENT(IN):: PSL_IN(IMS:IME,JMS:JME,num_metgrid_levels)
       REAL, INTENT(IN):: U_IN(IMS:IME,JMS:JME,num_metgrid_levels)
       REAL, INTENT(IN):: V_IN(IMS:IME,JMS:JME,num_metgrid_levels)
       REAL, INTENT(IN):: dsg1(KME),dsg2(KME),PDTOP,PT
       REAL, INTENT(INOUT):: U(IMS:IME,JMS:JME,KMS:KME),V(IMS:IME,JMS:JME,KMS:KME)
       REAL            :: ptop, psfc_inc
       REAL, PARAMETER :: tweak=100. ! allowance in Pa
       INTEGER         :: ldmin,ldmax,I,J,LNSV, IEND
       LOGICAL, parameter::  full_col=.false.
       LOGICAL :: print_diag
!       LOGICAL, parameter::  full_col=.true.

!! PSL_IN allocated IMS:IME,JMS:JME, but only defined for tile dimensions

        do J=JTS,min(JTE,JDE-1)
        do I=ITS,min(ITE,IDE-1)
        if (PSL_IN(I,J,1) .lt. PSL_IN(I,J,2)) then
          call sort_3(PSL_IN(I,J,:),U_IN(I,J,:),V_IN(I,J,:),num_metgrid_levels)
        endif
        enddo
        enddo

!-------------southern boundary-----------------------------------------

        if (print_diag) write(0,*) 'KME, PDTOP, PT: ', KME, PDTOP, PT

        ptop=pt+(dsg1(1)*pdtop)*0.5   ! pressure middle top model layer

        if (print_diag) write(0,*) 'ptop is: ', ptop

          amdvel=0.

   s_boundary:	if (JTS .eq. 1) then

       do J=1,LNSV
        DO I=ITS,min(ITE,IDE-2)
          sdph=0.
          spfp=0.
          spfh=0.
          pdxy=(pd(i,j)+pd(i,j+1)+pd(i+1,j)+pd(i+1,j+1))*0.25
          psfc_inc=(dsg1(KME)*pdtop+dsg2(KME)*pdxy)*0.5

        ldmin=1

    k_loop1:   do ld=ldmin,num_metgrid_levels
	if ( mod(PSL_IN(I,J,ld),100.) .ne. 0) then
          ldmax=ld
          exit k_loop1
        endif
              end do k_loop1

!
          do ld=ldmin,num_metgrid_levels-1

	if (.not. full_col) then

        if (ld .ge. ldmax) then

          spfp=(V_IN(I,J,ld)+V_IN(I,J,ld+1))* &
               (PSL_IN(I,J,ld)-PSL_IN(I,J,ld+1))*0.5+spfp

          endif

        else

              spfp=(V_IN(I,J,ld)+V_IN(I,J,ld+1))* &
            (PSL_IN(I,J,ld)-PSL_IN(I,J,ld+1))*0.5+spfp

        endif

          enddo
!
! ====================
!
          do l=1,KME
      dphl=(dsg1(l)*pdtop+dsg2(l)*pdxy)
      sdph=dphl+sdph
      spfh=v(i,j,l)*dphl+spfh
          enddo
!
      dvel=(spfp-spfh)/sdph

      amdvel=max(abs(dvel),amdvel)
!
          do l=1,KME
      v(i,j,l)=v(i,j,l)+dvel
          enddo

        END DO 
       END DO ! j loop
      if (print_diag) print*,'south max dvel=',amdvel

	endif s_boundary
!
!-------------northern boundary----------------------------------------
!
   n_boundary:	if (JTE .ge. JDE-2) then

	JEND=min(JTE,JDE-1)

          amdvel=0.
       do J=1,LNSV
        do i=ITS,min(ITE,IDE-2)
          sdph=0.
          spfp=0.
          spfh=0.
          pdxy=(pd(i,JEND-J)+pd(i,JEND-J+1)+pd(i+1,JEND-J)+pd(i+1,JEND-J+1))*0.25
          psfc_inc=(dsg1(KME)*pdtop+dsg2(KME)*pdxy)*0.5
!
	ldmin=1

    k_loop2:   do ld=ldmin,num_metgrid_levels
	if ( mod(PSL_IN(I,JEND-J,ld),100.) .ne. 0) then
          ldmax=ld
          exit k_loop2
        endif
              end do k_loop2

          do ld=ldmin,num_metgrid_levels-1
	if (.not. full_col) then
!          if( PSL_IN(I,JEND-J,ld) .le. pdxy+pt+tweak .and. &
!              PSL_IN(I,JEND-J,ld+1) .ge. pt ) then
        if (ld .ge. ldmax) then
      spfp=(V_IN(I,JEND-J,ld)+V_IN(I,JEND-J,ld+1))* &
           (PSL_IN(I,JEND-J,ld)-PSL_IN(I,JEND-J,ld+1))*0.5+spfp
          endif
        else
      spfp=(V_IN(I,JEND-J,ld)+V_IN(I,JEND-J,ld+1))* &
           (PSL_IN(I,JEND-J,ld)-PSL_IN(I,JEND-J,ld+1))*0.5+spfp
        endif

          enddo
!
! ==================
!
          do l=1,KME
      dphl=(dsg1(l)*pdtop+dsg2(l)*pdxy)
      sdph=dphl+sdph
      spfh=v(i,JEND-J,l)*dphl+spfh
          enddo
!
      dvel=(spfp-spfh)/sdph
	if (abs(dvel) .gt. amdvel) then
        amdvel=max(abs(dvel),amdvel)
	endif

!
          do l=1,KME
      v(i,JEND-J,l)=v(i,JEND-J,l)+dvel
          enddo
              enddo
     end do ! j loop
       if (print_diag) print*,'north max dvel=',amdvel
       endif n_boundary
!
!-------------western boundary-----------------------------------------
!
  w_boundary:	if (ITS .eq. 1)  then

	JEND=min(JTE,JDE-2)

          amdvel=0.
            do i=1,LNSV
              do j=JTS,JEND
          sdph=0.
          spfp=0.
          spfh=0.
          pdxy=(pd(I,j)+pd(I+1,j)+pd(I,j+1)+pd(I+1,j+1))*0.25
          psfc_inc=(dsg1(KME)*pdtop+dsg2(KME)*pdxy)*0.5
!
	ldmin=1
    k_loop3:   do ld=ldmin,num_metgrid_levels
	if ( mod(PSL_IN(I,J,ld),100.) .ne. 0) then
          ldmax=ld
          exit k_loop3
        endif
              end do k_loop3
!
          do ld=ldmin,num_metgrid_levels-1
	if (.not. full_col) then
!          if( PSL_IN(I,J,ld) .le. pdxy+pt+tweak .and. &
!              PSL_IN(I,J,ld+1) .ge. pt) then

        if (ld .ge. ldmax) then
      spfp=(u_in(I,j,ld)+u_in(I,j,ld+1))*  &
           (PSL_IN(I,J,ld)-PSL_IN(I,J,ld+1))*0.5+spfp
          endif
       else
      spfp=(u_in(I,j,ld)+u_in(I,j,ld+1))*  &
           (PSL_IN(I,J,ld)-PSL_IN(I,J,ld+1))*0.5+spfp
       endif
          enddo
!
! =======================
!
          do l=1,KME
      dphl=(dsg1(l)*pdtop+dsg2(l)*pdxy)
      sdph=dphl+sdph
      spfh=u(I,j,l)*dphl+spfh
          enddo
!
      dvel=(spfp-spfh)/sdph
      amdvel=max(abs(dvel),amdvel)
!
          do l=1,KME
      u(I,j,l)=u(I,j,l)+dvel
          enddo
              enddo
      end do ! I loop
      if (print_diag) print*,'west max dvel=',amdvel

	endif w_boundary
!-------------eastern boundary-----------------------------------------

  e_boundary:  if (ITE .ge. IDE-2) then


	IEND=min(ITE,IDE-1)
	JEND=min(JTE,JDE-2)

          amdvel=0.

            do i=1,LNSV
              do j=JTS,JEND
          sdph=0.
          sdph_in=0.
          spfp=0.
          spfh=0.
          pdxy=(pd(IEND-I,j)+pd(IEND-I+1,j)+pd(IEND-I,j+1)+pd(IEND-I+1,j+1))*0.25
          psfc_inc=(dsg1(KME)*pdtop+dsg2(KME)*pdxy)*0.5

	ldmin=1

    k_loop4:   do ld=ldmin,num_metgrid_levels
	if ( mod(PSL_IN(IEND-I,J,ld),100.) .ne. 0) then
          ldmax=ld
          exit k_loop4
        endif
              end do k_loop4

          do ld=ldmin,num_metgrid_levels-1

	if (.not. full_col) then

        if (ld .ge. ldmax) then
      sdph_in=sdph_in+(PSL_IN(IEND-I,j,ld)-PSL_IN(IEND-I,j,ld+1))


      spfp=(u_in(IEND-I,j,ld)+u_in(IEND-I,j,ld+1))* &
     &     (PSL_IN(IEND-I,j,ld)-PSL_IN(IEND-I,j,ld+1))*0.5+spfp
        endif

        else  ! full_col

      sdph_in=sdph_in+(PSL_IN(IEND-I,j,ld)-PSL_IN(IEND-I,j,ld+1))
      spfp=(u_in(IEND-I,j,ld)+u_in(IEND-I,j,ld+1))* &
     &     (PSL_IN(IEND-I,j,ld)-PSL_IN(IEND-I,j,ld+1))*0.5+spfp
        endif
          enddo
!
! =======================
!
          do l=1,KME
      dphl=(dsg1(l)*pdtop+dsg2(l)*pdxy)
      sdph=dphl+sdph
      spfh=u(IEND-I,j,l)*dphl+spfh
          enddo
!
      dvel=(spfp-spfh)/sdph

      amdvel=max(abs(dvel),amdvel)

!
          do l=1,KME
      u(IEND-I,j,l)=u(IEND-I,j,l)+dvel
          enddo ! l loop
          enddo ! j loop
          end do ! I loop
      if (print_diag) print*,'east max dvel=',amdvel

     endif e_boundary

	end subroutine bgrid_boundary_winds

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      SUBROUTINE bgrid_boundary_zeroflux (pd, u, v, &
     &                            dsg1, dsg2, PDTOP,pt,             &
     &                            IDS,IDE,JDS,JDE,KDS,KDE,          &
     &                            IMS,IME,JMS,JME,KMS,KME,          &
     &                            ITS,ITE,JTS,JTE,KTS,KTE )

       REAL, INTENT(IN):: PD(IMS:IME,JMS:JME) 
       REAL, INTENT(IN):: dsg1(KME),dsg2(KME),PDTOP,PT
       REAL, INTENT(INOUT):: U(IMS:IME,JMS:JME,KMS:KME),V(IMS:IME,JMS:JME,KMS:KME)
       INTEGER         :: NN
       REAL :: dvelsum

!-------------southern boundary-----------------------------------------

	write(0,*) 'KME, PDTOP, PT: ', KME, PDTOP, PT

!	do NN=1,15
        sdph=0.
        spfh=0.

   s_boundary:	if (JTS .eq. 1) then

	

        DO I=ITS,min(ITE,IDE-1)-1
          pdxy=(pd(i,1)+pd(i,2)+pd(i+1,1)+pd(i+1,2))*0.25
          do l=1,KME
            dphl=(dsg1(l)*pdtop+dsg2(l)*pdxy)
            sdph=dphl+sdph
            spfh=v(i,1,l)*dphl+spfh
          enddo
        END DO 

	endif s_boundary
!	write(0,*) 'sdph, spfh after s_boundary: ', sdph, spfh
!-------------northern boundary----------------------------------------
   n_boundary:	if (JTE .ge. JDE-2) then

	JEND=min(JTE,JDE-1)

              do i=ITS,min(ITE,IDE-2)
          pdxy=(pd(i,JEND-1)+pd(i,JEND)+pd(i+1,JEND-1)+pd(i+1,JEND))*0.25
!
          do l=1,KME
            dphl=(dsg1(l)*pdtop+dsg2(l)*pdxy)
            sdph=dphl+sdph
            spfh=-v(i,JEND-1,l)*dphl+spfh
          enddo
!
              enddo

       endif n_boundary
!	write(0,*) 'sdph, spfh after n_boundary: ', sdph, spfh
!-------------western boundary-----------------------------------------

  w_boundary:	if (ITS .eq. 1)  then

	JEND=min(JTE,JDE-2)

              do j=JTS,JEND
          pdxy=(pd(1,j)+pd(2,j)+pd(1,j+1)+pd(2,j+1))*0.25
!
          do l=1,KME
             dphl=(dsg1(l)*pdtop+dsg2(l)*pdxy)
             sdph=dphl+sdph
             spfh=u(1,j,l)*dphl+spfh
          enddo
!
              enddo

	endif w_boundary
!	write(0,*) 'sdph, spfh after w_boundary: ', sdph, spfh
!-------------eastern boundary-----------------------------------------

  e_boundary:  if (ITE .ge. IDE-2) then

	IEND=min(ITE,IDE-1)
	JEND=min(JTE,JDE-2)

              do j=JTS,JEND
          pdxy=(pd(IEND-1,j)+pd(IEND,j)+pd(IEND-1,j+1)+pd(IEND,j+1))*0.25
!
          do l=1,KME
             dphl=(dsg1(l)*pdtop+dsg2(l)*pdxy)
             sdph=dphl+sdph
             spfh=-u(IEND-1,j,l)*dphl+spfh
          enddo

              enddo

	write(0,*) 'sdph, spfh after e_boundary (done top): ', sdph, spfh
     endif e_boundary

	write(0,*) 'spfh/sdph - possible dvel: ', spfh/sdph

	dvel=1.0*spfh/sdph

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	

	spfh=0.

   s_boundary2:	if (JTS .eq. 1) then

        DO I=ITS,min(ITE,IDE-1)-1
          pdxy=(pd(i,1)+pd(i,2)+pd(i+1,1)+pd(i+1,2))*0.25
          do L=1,KME
            dphl=(dsg1(L)*pdtop+dsg2(L)*pdxy)
            v(i,1,L)=v(i,1,L)-dvel
            spfh=v(i,1,L)*dphl+spfh
          enddo
        END DO 

!	write(0,*) 'spfh end S: ', spfh

	endif s_boundary2
!-------------northern boundary----------------------------------------

   n_boundary2:	if (JTE .ge. JDE-2) then

	JEND=min(JTE,JDE-1)

              do i=ITS,min(ITE,IDE-2)
          pdxy=(pd(i,JEND-1)+pd(i,JEND)+pd(i+1,JEND-1)+pd(i+1,JEND))*0.25
!
          do l=1,KME
            dphl=(dsg1(L)*pdtop+dsg2(L)*pdxy)
            v(i,JEND-1,L)=v(i,JEND-1,L)+dvel
            spfh=-v(i,JEND-1,l)*dphl+spfh
          enddo
!
              enddo

!	write(0,*) 'spfh end N: ', spfh
       endif n_boundary2
!-------------western boundary-----------------------------------------

  w_boundary2:	if (ITS .eq. 1)  then

	JEND=min(JTE,JDE-2)

              do j=JTS,JEND
          pdxy=(pd(1,j)+pd(2,j)+pd(1,j+1)+pd(2,j+1))*0.25
!
          do l=1,KME
             dphl=(dsg1(L)*pdtop+dsg2(L)*pdxy)
             u(1,J,L)=u(1,J,L)-dvel
             spfh=u(1,J,L)*dphl+spfh
          enddo
!
              enddo

!	write(0,*) 'spfh end W: ', spfh
	endif w_boundary2
!-------------eastern boundary-----------------------------------------

  e_boundary2:  if (ITE .ge. IDE-2) then

	IEND=min(ITE,IDE-1)
	JEND=min(JTE,JDE-2)

              do j=JTS,JEND
          pdxy=(pd(IEND-1,j)+pd(IEND,j)+pd(IEND-1,j+1)+pd(IEND,j+1))*0.25
!
          do l=1,KME
             dphl=(dsg1(L)*pdtop+dsg2(L)*pdxy)
             u(IEND-1,J,L)=u(IEND-1,J,L)+dvel
             spfh=-u(IEND-1,j,L)*dphl+spfh
          enddo

              enddo

	write(0,*) 'spfh end E (all done): ', spfh
     endif e_boundary2


!	enddo ! NN loop
        	
	end subroutine bgrid_boundary_zeroflux

! --------------------------------------------------------------------------

        subroutine sort_3(pres,uwind,vwind,nlev)

        integer:: J, nlev
        real :: pres(nlev), uwind(nlev), vwind(nlev)
        real :: pres_n(nlev), uwind_n(nlev), vwind_n(nlev)

        new_spot=1

    find_slot:  do L=2,nlev-1 !changed to avoid out of bounds on pres
        if (pres(1) .le. pres(L) .and. pres(1) .ge. pres(L+1)) then
           new_spot=L
           exit find_slot
        endif
        end do find_slot

        do L=1,new_spot-1
        pres_n(L)=pres(L+1)
        uwind_n(L)=uwind(L+1)
        vwind_n(L)=vwind(L+1)
        enddo

        pres_n(new_spot)=pres(1)
        uwind_n(new_spot)=uwind(1)
        vwind_n(new_spot)=vwind(1)

        do L=new_spot+1,nlev
          pres_n(L)=pres(L)
          uwind_n(L)=uwind(L)
          vwind_n(L)=vwind(L)
        enddo

        do L=1,nlev
          pres(L)=pres_n(L)
          uwind(L)=uwind_n(L)
          vwind(L)=vwind_n(L)
        enddo

        end subroutine sort_3

END MODULE vinterp_routines
