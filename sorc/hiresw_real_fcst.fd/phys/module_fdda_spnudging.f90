



MODULE module_fdda_spnudging

  USE module_dm , ONLY : ntasks_x, ntasks_y, local_communicator_x, local_communicator_y, data_order_xzy
  USE module_wrf_error , ONLY : wrf_err_message

CONTAINS



   SUBROUTINE spectral_nudging(grid,itimestep,dt,xtime,id,analysis_interval, end_fdda_hour, &
               if_no_pbl_nudging_uv, if_no_pbl_nudging_t, if_no_pbl_nudging_ph,&
               if_zfac_uv, k_zfac_uv, dk_zfac_uv, & 
               if_zfac_t, k_zfac_t, dk_zfac_t, &
               if_zfac_ph, k_zfac_ph, dk_zfac_ph, &
               guv, gt, gph, if_ramping, dtramp_min, xwavenum, ywavenum, &
               u3d,v3d,th3d,ph3d,                 &
               u_ndg_old,v_ndg_old,t_ndg_old,ph_ndg_old,       &
               u_ndg_new,v_ndg_new,t_ndg_new,ph_ndg_new,       &
               RUNDGDTEN,RVNDGDTEN,RTHNDGDTEN,RPHNDGDTEN,&
               pblh, ht, z, z_at_w,                             &
               ids,ide, jds,jde, kds,kde,                           &
               ims,ime, jms,jme, kms,kme,                           &
               i_start,i_end, j_start,j_end, kts,kte, num_tiles, &
               ips,ipe,jps,jpe,kps,kpe,                   &
               imsx,imex,jmsx,jmex,kmsx,kmex,                    &
               ipsx,ipex,jpsx,jpex,kpsx,kpex,                   &
               imsy,imey,jmsy,jmey,kmsy,kmey,                    &
               ipsy,ipey,jpsy,jpey,kpsy,kpey                   )



   USE module_state_description
   USE module_domain, ONLY : domain


   implicit none

































   TYPE(domain) , TARGET          :: grid

   INTEGER,  INTENT(IN)   ::      itimestep, analysis_interval, end_fdda_hour

   INTEGER,  INTENT(IN)   ::      if_no_pbl_nudging_uv, if_no_pbl_nudging_t, &
                                  if_no_pbl_nudging_ph
   INTEGER,  INTENT(IN)   ::      if_zfac_uv, if_zfac_t, if_zfac_ph
   INTEGER,  INTENT(IN)   ::      k_zfac_uv,  k_zfac_t, k_zfac_ph
   INTEGER,  INTENT(IN)   ::      dk_zfac_uv,  dk_zfac_t, dk_zfac_ph
   INTEGER,  INTENT(IN)   ::      if_ramping
   INTEGER,  INTENT(IN)   ::      xwavenum,ywavenum

   INTEGER , INTENT(IN)   ::      id
   REAL,     INTENT(IN)   ::      DT, xtime, dtramp_min

   INTEGER,  INTENT(IN)   ::      ids,ide, jds,jde, kds,kde, &
                                  ims,ime, jms,jme, kms,kme, &
                                  kts,kte, num_tiles,        &
                                  ips,ipe,jps,jpe,kps,kpe,   &
                                  imsx,imex,jmsx,jmex,kmsx,kmex,   &
                                  ipsx,ipex,jpsx,jpex,kpsx,kpex,   &
                                  imsy,imey,jmsy,jmey,kmsy,kmey,   &
                                  ipsy,ipey,jpsy,jpey,kpsy,kpey 

   INTEGER, DIMENSION(num_tiles), INTENT(IN) ::                   &
  &                                    i_start,i_end,j_start,j_end
 
   REAL,     DIMENSION( ims:ime, kms:kme, jms:jme ), &
             INTENT(IN)   ::                  ph3d, &
                                              th3d, &
                                                 z, &
                                            z_at_w

   REAL,     DIMENSION( ims:ime, kms:kme, jms:jme ), &
             INTENT(INOUT)   ::           rundgdten, &
                                          rvndgdten, &
                                         rthndgdten, &
                                         rphndgdten


   REAL,     DIMENSION( ims:ime, kms:kme, jms:jme ), &
             INTENT(INOUT)   ::           u_ndg_old, &
                                          v_ndg_old, &
                                          t_ndg_old, &
                                          ph_ndg_old, &
                                          u_ndg_new, &
                                          v_ndg_new, &
                                          t_ndg_new, &
                                          ph_ndg_new

   REAL,     DIMENSION( ims:ime, kms:kme, jms:jme ), &
             INTENT(IN)   ::                    u3d, &
                                                v3d

   REAL,  DIMENSION( ims:ime, jms:jme ), INTENT(IN) :: pblh, &
                                                         ht

   REAL, INTENT(IN)    :: guv, gt ,gph

   INTEGER             :: its,ite, jts,jte,ij
   INTEGER             :: i, j, k, itsu, jtsv, itf, jtf, ktf, i0, k0, j0
   REAL                :: xtime_old, xtime_new, coef, val_analysis
   INTEGER             :: kpbl, dbg_level

   REAL                :: zpbl, zagl, zagl_bot, zagl_top, tfac, actual_end_fdda_min
   REAL, DIMENSION( ips:ipe, kps:kpe, jps:jpe, 4 ) :: wpbl  
   REAL, DIMENSION( kps:kpe, 4 )                   :: wzfac 

   LOGICAL , EXTERNAL  :: wrf_dm_on_monitor

   CHARACTER (LEN=256) :: message


   END SUBROUTINE spectral_nudging



SUBROUTINE spectral_nudging_filter_3dx( f, nwave,            &
                            ids, ide, jds, jde, kds, kde,    &
                            ims, ime, jms, jme, kms, kme,    &
                            its, ite, jts, jte, kts, kte )

  IMPLICIT NONE

  INTEGER ,       INTENT(IN   ) :: nwave
  INTEGER ,       INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                   ims, ime, jms, jme, kms, kme, &
                                   its, ite, jts, jte, kts, kte

  REAL , DIMENSION( ims:ime , kms:kme, jms:jme ) , INTENT(INOUT) ::  f

  REAL , DIMENSION(1:ide-ids+1,1:kte-kts+1) :: sheet
  INTEGER ::  i, j, j_end, k, nx, ny

  
  
  
  

  
  IF ((its /= ids) .OR. (ite /= ide)) THEN
     WRITE ( wrf_err_message , * ) 'module_spectral_nudging: 3d: (its /= ids) or (ite /= ide)',its,ids,ite,ide
     CALL wrf_error_fatal3("",176,&
TRIM( wrf_err_message ) )
  END IF


  nx = ide-ids+1 
  ny = kte-kts+1 
  j_end = MIN(jte, jde-1)
  IF (j_end == jde-1) j_end = jde
  DO j = jts, j_end

        DO k=kts,kte
        DO i=ids,ide-1
           sheet(i-ids+1,k-kts+1) = f(i,k,j)
        END DO
           sheet(ide,k-kts+1) = 0.
        END DO

        CALL spectral_nudging_filter_fft_2d_ncar(nx,ny,nwave,sheet)

        DO k=kts,kte
           DO i=ids,ide
              f(i,k,j) = sheet(i-ids+1,k-kts+1)
           END DO
        END DO
  END DO 

END SUBROUTINE spectral_nudging_filter_3dx


SUBROUTINE spectral_nudging_filter_3dy( f, nwave,   &
                            ids, ide, jds, jde, kds, kde,    &
                            ims, ime, jms, jme, kms, kme,    &
                            its, ite, jts, jte, kts, kte )

  IMPLICIT NONE

  INTEGER ,       INTENT(IN   ) :: nwave
  INTEGER ,       INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                   ims, ime, jms, jme, kms, kme, &
                                   its, ite, jts, jte, kts, kte

  REAL , DIMENSION( ims:ime , kms:kme, jms:jme ) , INTENT(INOUT) ::  f

  REAL , DIMENSION(1:jde-jds+1,1:kte-kts+1) :: sheet
  INTEGER ::  i, j, i_end, k, nx, ny

  
  
  
  

  
  IF ((jts /= jds) .OR. (jte /= jde)) THEN
     WRITE ( wrf_err_message , * ) 'module_spectral_nudging: 3d: (jts /= jds) or (jte /= jde)',jts,ids,ite,ide
     CALL wrf_error_fatal3("",231,&
TRIM( wrf_err_message ) )
  END IF


  nx = jde-jds+1
  ny = kte-kts+1 
  i_end = MIN(ite, ide-1)
  IF (i_end == ide-1) i_end = ide
  DO i = its, i_end

        DO k=kts,kte
        DO j=jds,jde
           sheet(j-jds+1,k-kts+1) = f(i,k,j)
        END DO
           sheet(jde,k-kts+1) = 0.
        END DO

        CALL spectral_nudging_filter_fft_2d_ncar(nx,ny,nwave,sheet)

        DO k=kts,kte
           DO j=jds,jde
              f(i,k,j) = sheet(j-jds+1,k-kts+1)
           END DO
        END DO
  END DO 

END SUBROUTINE spectral_nudging_filter_3dy



SUBROUTINE spectral_nudging_filter_fft_2d_ncar(nx,ny,nwave,fin)
  IMPLICIT NONE
  INTEGER , INTENT(IN) :: nx, ny, nwave
  REAL , DIMENSION(nx,ny), INTENT(INOUT) :: fin

  INTEGER :: i, j
  REAL, dimension(nx,ny) :: fp

  INTEGER :: lensave, ier, nh, n1
  INTEGER :: lot, jump, n, inc, lenr, lensav, lenwrk
  REAL, DIMENSION(nx+15) :: wsave
  REAL, DIMENSION(nx,ny) :: work





  n = nx
  lot = ny
  lensav = n+15
  inc = 1
  lenr = nx*ny
  jump = nx
  lenwrk = lenr





  call rfftmi(n,wsave,lensav,ier)
  IF(ier /= 0) THEN
    write(0,*) ' error in rfftmi ',ier
  END IF



  call rfftmf( lot, jump, n, inc, fin, lenr, wsave, lensav, work, lenwrk, ier )
  IF(ier /= 0) THEN
    write(0,*) ' error in rfftmf ',ier
  END IF

  if(MOD(n,2) == 0) then
    nh = n/2 - 1
  else
    nh = (n-1)/2
  end if




  fp = 1.

  DO j=1,ny
     DO i=nwave+1,nh
         fp(2*i-1,j) = 0.
         fp(2*i,j) = 0.
     ENDDO
  ENDDO

  DO j=1,ny
    DO i=1,nx
      fin(i,j) = fp(i,j)*fin(i,j)
    ENDDO
  ENDDO



  call rfftmb( lot, jump, n, inc, fin, lenr, wsave, lensav, work, lenwrk, ier )
  IF(ier /= 0) THEN
    write(0,*) ' error in rfftmb ',ier
  END IF

END SUBROUTINE spectral_nudging_filter_fft_2d_ncar



   SUBROUTINE fddaspnudginginit(id,rundgdten,rvndgdten,rthndgdten,rphndgdten, &
               run_hours,  &
               if_no_pbl_nudging_uv, if_no_pbl_nudging_t, if_no_pbl_nudging_ph, &
               if_zfac_uv, k_zfac_uv, dk_zfac_uv, &
               if_zfac_t, k_zfac_t, dk_zfac_t, &
               if_zfac_ph, k_zfac_ph, dk_zfac_ph, &               
               guv, gt, gph, if_ramping, dtramp_min, end_fdda_hour, &
               xwavenum,ywavenum,                          &
                      restart, allowed_to_read,                    &
                      ids, ide, jds, jde, kds, kde,                &
                      ims, ime, jms, jme, kms, kme,                &
                      its, ite, jts, jte, kts, kte                 )

   IMPLICIT NONE


   INTEGER , INTENT(IN)         ::  id
   LOGICAL, INTENT(IN)          ::  restart, allowed_to_read
   INTEGER, INTENT(IN)          ::  ids, ide, jds, jde, kds, kde, &
                                    ims, ime, jms, jme, kms, kme, &
                                    its, ite, jts, jte, kts, kte
   REAL, DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(OUT) :: &
                                                       rundgdten, &
                                                       rvndgdten, &
                                                      rthndgdten, &
                                                      rphndgdten
   INTEGER,  INTENT(IN)   ::      run_hours
   INTEGER,  INTENT(IN)   ::      if_no_pbl_nudging_uv, if_no_pbl_nudging_t, &
                                  if_no_pbl_nudging_ph, end_fdda_hour
   INTEGER,  INTENT(IN)   ::      if_zfac_uv, if_zfac_t, if_zfac_ph
   INTEGER,  INTENT(IN)   ::      k_zfac_uv,  k_zfac_t,  k_zfac_ph
   INTEGER,  INTENT(IN)   ::      dk_zfac_uv,  dk_zfac_t,  dk_zfac_ph
   INTEGER,  INTENT(IN)   ::      if_ramping
   INTEGER,  INTENT(IN)   ::      xwavenum,ywavenum
   REAL,     INTENT(IN)   ::      dtramp_min
   REAL, INTENT(IN)       ::      guv, gt, gph
   REAL                   ::      actual_end_fdda_min

   INTEGER :: i, j, k

   LOGICAL , EXTERNAL     ::      wrf_dm_on_monitor

   CHARACTER (LEN=256) :: message

   IF ( wrf_dm_on_monitor() ) THEN

     IF( guv > 0.0) THEN
       WRITE(message,'(a,i1,a,e12.4,a,i4,a,i4)') &
           'D0',id,' Spectral nudging for wind is turned on and Guv= ', guv,' xwave= ',xwavenum,' ywavenum= ',ywavenum
       CALL wrf_message(TRIM(message))
     ELSE IF( guv < 0.0 ) THEN
       CALL wrf_error_fatal3("",389,&
'In grid FDDA, Guv must be positive.')
     ELSE
       WRITE(message,'(a,i1,a,e12.4)') &
           'D0',id,' Spectral nudging for wind is turned off and Guv= ', guv
       CALL wrf_message(TRIM(message))
     ENDIF

     IF( gt > 0.0) THEN
       WRITE(message,'(a,i1,a,e12.4,a,i4,a,i4)') &
           'D0',id,' Spectral nudging for temperature is turned on and Gt= ', gt,' xwave= ',xwavenum,' ywavenum= ',ywavenum
       CALL wrf_message(TRIM(message))
     ELSE IF( gt < 0.0 ) THEN
       CALL wrf_error_fatal3("",402,&
'In grid FDDA, Gt must be positive.')
     ELSE
       WRITE(message,'(a,i1,a,e12.4)') &
           'D0',id,' Spectral nudging for temperature is turned off and Gt= ', gt
       CALL wrf_message(TRIM(message))
     ENDIF

     IF( gph > 0.0) THEN
       WRITE(message,'(a,i1,a,e12.4,a,i4,a,i4)') &
         'D0',id,' Spectral nudging for geopotential is turned on and Gph= ', gph,' xwave= ',xwavenum,' ywavenum= ',ywavenum
       CALL wrf_message(TRIM(message))
     ELSE IF( gph < 0.0 ) THEN
       CALL wrf_error_fatal3("",415,&
'In grid FDDA, Gph must be positive.')
     ELSE
       WRITE(message,'(a,i1,a,e12.4)') &
         'D0',id,' Spectral nudging for geopotential is turned off and Gph= ', gph
       CALL wrf_message(TRIM(message))
     ENDIF

     IF( guv > 0.0 .AND. if_no_pbl_nudging_uv == 1 ) THEN
        WRITE(message,'(a,i1,a)') &
           'D0',id,' Spectral nudging for wind is turned off within the PBL.'
        CALL wrf_message(TRIM(message))
             IF( dk_zfac_uv < 1 ) CALL wrf_error_fatal3("",427,&
'In spectral nudging, dk_zfac_uv must be greater or equal than 1.')
     ELSEIF( guv > 0.0 .AND. if_zfac_uv == 1 ) THEN
        WRITE(message,'(a,i1,a,i3)') &
           'D0',id,' Spectral nudging for wind is turned off below layer', k_zfac_uv
        CALL wrf_message(TRIM(message))
             IF( dk_zfac_uv < 1 ) CALL wrf_error_fatal3("",433,&
'In spectral nudging, dk_zfac_uv must  be greater or equal than 1.')       
     ENDIF


     IF( gt > 0.0 .AND. if_no_pbl_nudging_t == 1 ) THEN
        WRITE(message,'(a,i1,a)') &
           'D0',id,' Spectral nudging for temperature is turned off within the PBL.'
        CALL wrf_message(TRIM(message))
             IF( dk_zfac_t < 1 ) CALL wrf_error_fatal3("",442,&
'In spectral nudging, dk_zfac_t must be greater or equal than 1.')
     ELSEIF( gt > 0.0 .AND. if_zfac_t == 1 ) THEN
        WRITE(message,'(a,i1,a,i3)') &
           'D0',id,' Spectral nudging for temperature is turned off below layer', k_zfac_t
        CALL wrf_message(TRIM(message))
            IF( dk_zfac_t < 1 ) CALL wrf_error_fatal3("",448,&
'In spectral nudging, dk_zfac_t must be greater or equal than 1.')
     ENDIF


     IF( gph > 0.0 .AND. if_no_pbl_nudging_ph == 1 ) THEN
        WRITE(message,'(a,i1,a)') &
         'D0',id,' Spectral nudging for geopotential is turned off within the PBL.'
        CALL wrf_message(TRIM(message))
            IF( dk_zfac_ph < 1 ) CALL wrf_error_fatal3("",457,&
'In spectral nudging, dk_zfac_ph must be greater or equal than 1.')
     ELSEIF( gph > 0.0 .AND. if_zfac_ph == 1 ) THEN
        WRITE(message,'(a,i1,a,i3)') &
          'D0',id,' Spectral nudging for geopotential is turned off below layer', &
           k_zfac_ph
        CALL wrf_message(TRIM(message))
            IF( dk_zfac_ph < 1 ) CALL wrf_error_fatal3("",464,&
'In spectral nudging, dk_zfac_ph must be greater or equal than 1.')
     ENDIF

     IF( if_ramping == 1 .AND. ABS(dtramp_min) > 0.0 ) THEN
       IF( dtramp_min <= 0.0 ) THEN
         actual_end_fdda_min = end_fdda_hour*60.0
       ELSE
         actual_end_fdda_min = end_fdda_hour*60.0 + ABS(dtramp_min)
       ENDIF

       IF( actual_end_fdda_min <= run_hours*60. ) THEN
          WRITE(message,'(a,i1,a)') &
            'D0',id,' Spectral nudging is ramped down near the end of the nudging period,'
          CALL wrf_message(TRIM(message))

          WRITE(message,'(a,f6.2,a,f6.2,a)') &
             '      starting at ', (actual_end_fdda_min - ABS(dtramp_min))/60.0,&
             'h, ending at ', actual_end_fdda_min/60.0,'h.'
          CALL wrf_message(TRIM(message))
       ENDIF
     ENDIF

   ENDIF

   IF(.not.restart) THEN
     DO j = jts,jte
     DO k = kts,kte
     DO i = its,ite
        rundgdten(i,k,j) = 0.
        rvndgdten(i,k,j) = 0.
        rthndgdten(i,k,j) = 0.
        rphndgdten(i,k,j) = 0.
     ENDDO
     ENDDO
     ENDDO
   ENDIF

   END SUBROUTINE fddaspnudginginit


END MODULE module_fdda_spnudging
