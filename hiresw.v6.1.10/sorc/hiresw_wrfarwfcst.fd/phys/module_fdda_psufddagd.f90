







MODULE module_fdda_psufddagd

CONTAINS



   SUBROUTINE fddagd(itimestep,dx,dt,xtime,  &
               id,analysis_interval, end_fdda_hour, &
               if_no_pbl_nudging_uv, if_no_pbl_nudging_t, if_no_pbl_nudging_q, &
               if_zfac_uv, k_zfac_uv, if_zfac_t, k_zfac_t, if_zfac_q, k_zfac_q, &
               guv, gt, gq, if_ramping, dtramp_min,  &

               grid_sfdda, &
               analysis_interval_sfc, end_fdda_hour_sfc, guv_sfc, gt_sfc, gq_sfc, &
               rinblw, &

               u3d,v3d,th3d,t3d,                 &
               qv3d,     &
               p3d,pi3d,                &
               u_ndg_old,v_ndg_old,t_ndg_old,q_ndg_old,mu_ndg_old,       &
               u_ndg_new,v_ndg_new,t_ndg_new,q_ndg_new,mu_ndg_new,       &
     u10_ndg_old, v10_ndg_old, t2_ndg_old, th2_ndg_old, q2_ndg_old, &
     rh_ndg_old, psl_ndg_old, ps_ndg_old, tob_ndg_old, odis_ndg_old, &
     u10_ndg_new, v10_ndg_new, t2_ndg_new, th2_ndg_new, q2_ndg_new, &
     rh_ndg_new, psl_ndg_new, ps_ndg_new, tob_ndg_new, odis_ndg_new, &
               RUNDGDTEN,RVNDGDTEN,RTHNDGDTEN,RQVNDGDTEN,RMUNDGDTEN,&
               pblh, ht, regime, znt, z, z_at_w,                             &
               ids,ide, jds,jde, kds,kde,                           &
               ims,ime, jms,jme, kms,kme,                           &
               its,ite, jts,jte, kts,kte                        )


   implicit none











































   INTEGER,  INTENT(IN)   ::      itimestep, analysis_interval, end_fdda_hour
   INTEGER,  INTENT(IN)   ::      analysis_interval_sfc, end_fdda_hour_sfc
   INTEGER,  INTENT(IN)   ::      grid_sfdda

   INTEGER,  INTENT(IN)   ::      if_no_pbl_nudging_uv, if_no_pbl_nudging_t, &
                                  if_no_pbl_nudging_q
   INTEGER,  INTENT(IN)   ::      if_zfac_uv, if_zfac_t, if_zfac_q
   INTEGER,  INTENT(IN)   ::      k_zfac_uv,  k_zfac_t,  k_zfac_q
   INTEGER,  INTENT(IN)   ::      if_ramping

   INTEGER , INTENT(IN)   ::      id
   REAL,     INTENT(IN)   ::      DT, dx, xtime, dtramp_min

   INTEGER,  INTENT(IN)   ::      ids,ide, jds,jde, kds,kde, &
                                  ims,ime, jms,jme, kms,kme, &
                                  its,ite, jts,jte, kts,kte
 
   REAL,     DIMENSION( ims:ime, kms:kme, jms:jme ), &
             INTENT(IN)   ::                   qv3d, &
                                               p3d, &
                                              pi3d, &
                                              th3d, &
                                               t3d, &
                                                 z, &
                                            z_at_w

   REAL,     DIMENSION( ims:ime, kms:kme, jms:jme ), &
             INTENT(INOUT)   ::           rundgdten, &
                                          rvndgdten, &
                                         rthndgdten, &
                                         rqvndgdten

   REAL,     DIMENSION( ims:ime, jms:jme ), &
             INTENT(INOUT)   ::          rmundgdten

   REAL,     DIMENSION( ims:ime, kms:kme, jms:jme ), &
             INTENT(IN)      ::           u_ndg_old, &
                                          v_ndg_old, &
                                          t_ndg_old, &
                                          q_ndg_old, &
                                          u_ndg_new, &
                                          v_ndg_new, &
                                          t_ndg_new, &
                                          q_ndg_new
                                                           
   REAL,       DIMENSION( ims:ime, jms:jme ),            &   
               INTENT(IN)       ::                       u10_ndg_old,  &
                                                         v10_ndg_old,  &
                                                         t2_ndg_old,   &
                                                         th2_ndg_old,  &
                                                         q2_ndg_old,   &
                                                         rh_ndg_old,   &
                                                         psl_ndg_old,  &
                                                         ps_ndg_old,   &
                                                         u10_ndg_new,  &
                                                         v10_ndg_new,  &
                                                         t2_ndg_new,   &
                                                         th2_ndg_new,  &
                                                         q2_ndg_new,   &
                                                         rh_ndg_new,   &
                                                         psl_ndg_new,  &
                                                         ps_ndg_new

   REAL,       DIMENSION( ims:ime, jms:jme ),            &   
               INTENT(IN)       ::                       tob_ndg_old,  &
                                                         tob_ndg_new

   REAL,     DIMENSION( ims:ime, jms:jme ), &
             INTENT(INOUT) ::   mu_ndg_old, &
                                mu_ndg_new

   REAL,       DIMENSION( ims:ime, jms:jme ),            &   
               INTENT(IN)       ::                       odis_ndg_old, odis_ndg_new

   REAL,     DIMENSION( ims:ime, kms:kme, jms:jme ), &
             INTENT(IN)   ::                    u3d, &
                                                v3d

   REAL,  DIMENSION( ims:ime, jms:jme ), INTENT(IN) :: pblh, &
                                                       ht, &
                                                       regime, &
                                                       znt

   REAL, INTENT(IN)    :: guv, gt, gq
   REAL, INTENT(IN)    :: guv_sfc, gt_sfc, gq_sfc, rinblw

   INTEGER             :: i, j, k, itsu, jtsv, itf, jtf, ktf, i0, k0, j0
   REAL                :: xtime_old, xtime_new, coef, val_analysis
   INTEGER             :: kpbl, dbg_level

   REAL                :: zpbl, zagl, zagl_bot, zagl_top, tfac, actual_end_fdda_min
   REAL, DIMENSION( its:ite, kts:kte, jts:jte, 4 ) :: wpbl  
   REAL, DIMENSION( kts:kte, 4 )                   :: wzfac 

   LOGICAL , EXTERNAL  :: wrf_dm_on_monitor

   CHARACTER (LEN=256) :: message
   INTEGER :: int4

   int4 = 1  

   actual_end_fdda_min = end_fdda_hour*60.0
   IF( if_ramping == 1 .AND. dtramp_min > 0.0 ) &
       actual_end_fdda_min = end_fdda_hour*60.0 + ABS(dtramp_min)
   IF( xtime > actual_end_fdda_min ) THEN


     DO j = jts, jte
     DO k = kts, kte
     DO i = its, ite
       RUNDGDTEN(i,k,j) = 0.0
       RVNDGDTEN(i,k,j) = 0.0
       RTHNDGDTEN(i,k,j) = 0.0
       RQVNDGDTEN(i,k,j) = 0.0
       IF( k .EQ. kts ) RMUNDGDTEN(i,j) = 0.
     ENDDO
     ENDDO
     ENDDO
     RETURN
   ENDIF

   IF( analysis_interval <= 0 )CALL wrf_error_fatal3("<stdin>",204,&
'In grid FDDA, gfdda_interval_m must be > 0')
   xtime_old = FLOOR(xtime/analysis_interval) * analysis_interval * 1.0
   xtime_new = xtime_old + analysis_interval
   IF( int4 == 1 ) THEN
     coef = (xtime-xtime_old)/(xtime_new-xtime_old)
   ELSE
     coef = 1.0          
   ENDIF

   IF ( wrf_dm_on_monitor()) THEN

     CALL get_wrf_debug_level( dbg_level )

     IF( xtime-xtime_old < 0.5*dt/60.0 ) THEN

       IF( xtime < end_fdda_hour*60.0 ) THEN
         WRITE(message,'(a,i1,a,f10.3,a)') &
          'D0',id,' 3-D analysis nudging reads new data at time = ', xtime, ' min.'
         CALL wrf_message( TRIM(message) )
         WRITE(message,'(a,i1,a,2f8.2,a)') &
          'D0',id,' 3-D analysis nudging bracketing times = ', xtime_old, xtime_new, ' min.'
         CALL wrf_message( TRIM(message) )
       ENDIF

       actual_end_fdda_min = end_fdda_hour*60.0
       IF( if_ramping == 1 .AND. dtramp_min > 0.0 ) &
           actual_end_fdda_min = end_fdda_hour*60.0 + ABS(dtramp_min)

       IF( dbg_level .GE. 10 .AND. xtime <= actual_end_fdda_min ) THEN

         i0 = (ite-its)/2+its
         j0 = (jte-jts)/2+jts 

         IF( guv > 0.0 ) THEN
           DO k = kts, kte
             WRITE(message,'(a,i1,a,3i4,a,f10.4,a,f10.4)') &
               '    D0',id,' sample 3-D analysis values at i,k,j=', i0, k, j0, &
               ' u_ndg_old=', u_ndg_old(i0,k,j0), ' u_ndg_new=', u_ndg_new(i0,k,j0)
             CALL wrf_message( TRIM(message) )
           ENDDO
           WRITE(message,'(a,i1,a,3i4,a,f10.4,a,f10.4)') &
             '    D0',id,' sample 3-D analysis values at i,k,j=', i0, k, j0, &
             ' mu_ndg_old=', mu_ndg_old(i0,j0), ' mu_ndg_new=', mu_ndg_new(i0,j0)
           CALL wrf_message( TRIM(message) )
           DO k = kts, kte
             WRITE(message,'(a,i1,a,3i4,a,f10.4,a,f10.4)') &
               '    D0',id,' sample 3-D analysis values at i,k,j=', i0, k, j0, &
               ' v_ndg_old=', v_ndg_old(i0,k,j0), ' v_ndg_new=', v_ndg_new(i0,k,j0)
             CALL wrf_message( TRIM(message) )
           ENDDO
         ENDIF

         IF( gt > 0.0 ) THEN
           DO k = kts, kte
             WRITE(message,'(a,i1,a,3i4,a,f10.4,a,f10.4)') &
               '    D0',id,' sample 3-D analysis values at i,k,j=', i0, k, j0, &
               ' t_ndg_old=', t_ndg_old(i0,k,j0), ' t_ndg_new=', t_ndg_new(i0,k,j0)
             CALL wrf_message( TRIM(message) )
           ENDDO
         ENDIF

         IF( gq > 0.0 ) THEN
           DO k = kts, kte
             WRITE(message,'(a,i1,a,3i4,a,f10.4,a,f10.4)') &
               '    D0',id,' sample 3-D analysis values at i,k,j=', i0, k, j0, &
               ' q_ndg_old=', q_ndg_old(i0,k,j0), ' q_ndg_new=', q_ndg_new(i0,k,j0)
             CALL wrf_message( TRIM(message) )
           ENDDO
         ENDIF

        IF( int4 == 1 ) then
           WRITE(message,'(a,i1,a)') '    D0',id, &
              ' 3-D nudging towards the temporally interpolated analysis'
         ELSE
           WRITE(message,'(a,i1,a)') '    D0',id, &
              ' 3-D nudging towards the target analysis'
         ENDIF

       ENDIF
     ENDIF
   ENDIF

   jtsv=MAX0(jts,jds+1)
   itsu=MAX0(its,ids+1)

   jtf=MIN0(jte,jde-1)
   ktf=MIN0(kte,kde-1)
   itf=MIN0(ite,ide-1)






   wpbl(:,:,:,:) = 1.0

   IF( if_no_pbl_nudging_uv == 1 .OR. grid_sfdda == 1 ) THEN

     DO j=jts,jtf 
     DO i=itsu,itf

       kpbl = 1
       zpbl = 0.5 * ( pblh(i-1,j) + pblh(i,j) )

       loop_ku: DO k=kts,ktf 

         zagl_bot = 0.5 * ( z_at_w(i-1,k,  j)-ht(i-1,j) + z_at_w(i,k,  j)-ht(i,j) )
         zagl_top = 0.5 * ( z_at_w(i-1,k+1,j)-ht(i-1,j) + z_at_w(i,k+1,j)-ht(i,j) )
         IF( zpbl >= zagl_bot .AND. zpbl < zagl_top ) THEN
           kpbl = k
           EXIT loop_ku
         ENDIF
       ENDDO loop_ku

       DO k=kts,ktf 
         IF( k <= kpbl   ) wpbl(i, k, j, 1) = 0.0
         IF( k == kpbl+1 ) wpbl(i, k, j, 1) = 0.1
         IF( k >  kpbl+1 ) wpbl(i, k, j, 1) = 1.0
       ENDDO

     ENDDO
     ENDDO

     DO i=its,itf
     DO j=jtsv,jtf

       kpbl = 1
       zpbl = 0.5 * ( pblh(i,j-1) + pblh(i,j) )

       loop_kv: DO k=kts,ktf

         zagl_bot = 0.5 * ( z_at_w(i,k,  j-1)-ht(i,j-1) + z_at_w(i,k,  j)-ht(i,j) )
         zagl_top = 0.5 * ( z_at_w(i,k+1,j-1)-ht(i,j-1) + z_at_w(i,k+1,j)-ht(i,j) )
         IF( zpbl >= zagl_bot .AND. zpbl < zagl_top ) THEN
           kpbl = k
           EXIT loop_kv
         ENDIF
       ENDDO loop_kv

       DO k=kts,ktf
         IF( k <= kpbl   ) wpbl(i, k, j, 2) = 0.0
         IF( k == kpbl+1 ) wpbl(i, k, j, 2) = 0.1
         IF( k >  kpbl+1 ) wpbl(i, k, j, 2) = 1.0
       ENDDO

     ENDDO
     ENDDO

   ENDIF

   IF( if_no_pbl_nudging_t == 1 .OR. grid_sfdda == 1 ) THEN
   
     DO j=jts,jtf
     DO i=its,itf

       kpbl = 1
       zpbl = pblh(i,j)
        
       loop_kt: DO k=kts,ktf

         zagl_bot = z_at_w(i,k,  j)-ht(i,j)
         zagl_top = z_at_w(i,k+1,j)-ht(i,j)
         IF( zpbl >= zagl_bot .AND. zpbl < zagl_top ) THEN
           kpbl = k
           EXIT loop_kt
         ENDIF
       ENDDO loop_kt

       DO k=kts,ktf
         IF( k <= kpbl   ) wpbl(i, k, j, 3) = 0.0
         IF( k == kpbl+1 ) wpbl(i, k, j, 3) = 0.1
         IF( k >  kpbl+1 ) wpbl(i, k, j, 3) = 1.0
       ENDDO 
        
     ENDDO
     ENDDO

   ENDIF

   IF( if_no_pbl_nudging_q == 1 .OR. grid_sfdda == 1 ) THEN
   
     DO j=jts,jtf
     DO i=its,itf

       kpbl = 1
       zpbl = pblh(i,j)
          
       loop_kq: DO k=kts,ktf

         zagl_bot = z_at_w(i,k,  j)-ht(i,j)
         zagl_top = z_at_w(i,k+1,j)-ht(i,j)
         IF( zpbl >= zagl_bot .AND. zpbl < zagl_top ) THEN
           kpbl = k
           EXIT loop_kq
         ENDIF
       ENDDO loop_kq

       DO k=kts,ktf
         IF( k <= kpbl   ) wpbl(i, k, j, 4) = 0.0
         IF( k == kpbl+1 ) wpbl(i, k, j, 4) = 0.1
         IF( k >  kpbl+1 ) wpbl(i, k, j, 4) = 1.0
       ENDDO 
            
     ENDDO  
     ENDDO
        
   ENDIF







   wzfac(:,:) = 1.0

   IF( if_zfac_uv == 1 ) THEN

     DO j=jts,jtf
     DO i=itsu,itf
     DO k=kts,ktf
       IF( k <= k_zfac_uv   ) wzfac(k, 1:2) = 0.0
       IF( k == k_zfac_uv+1 ) wzfac(k, 1:2) = 0.1
       IF( k >  k_zfac_uv+1 ) wzfac(k, 1:2) = 1.0
     ENDDO
     ENDDO
     ENDDO

   ENDIF

   IF( if_zfac_t == 1 ) THEN

     DO j=jts,jtf
     DO i=itsu,itf
     DO k=kts,ktf
       IF( k <= k_zfac_t   ) wzfac(k, 3) = 0.0
       IF( k == k_zfac_t+1 ) wzfac(k, 3) = 0.1
       IF( k >  k_zfac_t+1 ) wzfac(k, 3) = 1.0
     ENDDO
     ENDDO
     ENDDO

   ENDIF

   IF( if_zfac_q == 1 ) THEN
       
     DO j=jts,jtf
     DO i=itsu,itf 
     DO k=kts,ktf
       IF( k <= k_zfac_q   ) wzfac(k, 4) = 0.0
       IF( k == k_zfac_q+1 ) wzfac(k, 4) = 0.1 
       IF( k >  k_zfac_q+1 ) wzfac(k, 4) = 1.0
     ENDDO  
     ENDDO
     ENDDO

   ENDIF













   tfac = 1.0

   IF( if_ramping == 1 .AND. ABS(dtramp_min) > 0.0 ) THEN
 
     IF( dtramp_min <= 0.0 ) THEN
       actual_end_fdda_min = end_fdda_hour*60.0
     ELSE
       actual_end_fdda_min = end_fdda_hour*60.0 + dtramp_min
     ENDIF

     IF( xtime < actual_end_fdda_min-ABS(dtramp_min) )THEN 
       tfac = 1.0
     ELSEIF( xtime >= actual_end_fdda_min-ABS(dtramp_min) .AND. xtime <= actual_end_fdda_min )THEN
       tfac = ( actual_end_fdda_min - xtime ) / ABS(dtramp_min)
       IF( dtramp_min > 0.0 ) coef = (xtime-xtime_old+analysis_interval)/(analysis_interval*1.0)
     ELSE                                                     
       tfac = 0.0
     ENDIF

   ENDIF                                                  



   IF( grid_sfdda == 1 ) THEN
     CALL SFDDAGD(itimestep,dx,dt,xtime, id, &
     analysis_interval_sfc, end_fdda_hour_sfc, guv_sfc, gt_sfc, gq_sfc, &
     rinblw, &
               u3d,v3d,th3d,t3d,                 &
               qv3d,     &
               p3d,pi3d,        &
     u10_ndg_old, v10_ndg_old, t2_ndg_old, th2_ndg_old, q2_ndg_old, &
     rh_ndg_old, psl_ndg_old, ps_ndg_old, tob_ndg_old, odis_ndg_old,  &
     u10_ndg_new, v10_ndg_new, t2_ndg_new, th2_ndg_new, q2_ndg_new, &
     rh_ndg_new, psl_ndg_new, ps_ndg_new, tob_ndg_new, odis_ndg_new,  &
     RUNDGDTEN,RVNDGDTEN,RTHNDGDTEN,RQVNDGDTEN,RMUNDGDTEN,&
     pblh, ht, regime, znt, z, z_at_w,                             &
     ids,ide, jds,jde, kds,kde,                           &
     ims,ime, jms,jme, kms,kme,                           &
     its,ite, jts,jte, kts,kte, wpbl, wzfac, if_ramping, dtramp_min, &
     actual_end_fdda_min, tfac )
   ENDIF



   DO j=jts,jtf
   DO k=kts,ktf
   DO i=itsu,itf
     val_analysis = u_ndg_old(i,k,j) *( 1.0 - coef ) + u_ndg_new(i,k,j) * coef
     RUNDGDTEN(i,k,j) = RUNDGDTEN(i,k,j) + guv * wpbl(i,k,j,1) * wzfac(k,1) * tfac * &
                         ( val_analysis - u3d(i,k,j) )
   ENDDO
   ENDDO
   ENDDO

   DO j=jtsv,jtf
   DO k=kts,ktf
   DO i=its,itf
     val_analysis = v_ndg_old(i,k,j) *( 1.0 - coef ) + v_ndg_new(i,k,j) * coef
     RVNDGDTEN(i,k,j) = RVNDGDTEN(i,k,j) + guv * wpbl(i,k,j,2) * wzfac(k,2) * tfac * &
                       ( val_analysis - v3d(i,k,j) )
   ENDDO
   ENDDO
   ENDDO

   DO j=jts,jtf
   DO k=kts,ktf
   DO i=its,itf
     val_analysis = t_ndg_old(i,k,j) *( 1.0 - coef ) + t_ndg_new(i,k,j) * coef
     RTHNDGDTEN(i,k,j) = RTHNDGDTEN(i,k,j) +   gt * wpbl(i,k,j,3) * wzfac(k,3) * tfac * &
                          ( val_analysis - th3d(i,k,j) + 300.0 )

     val_analysis = q_ndg_old(i,k,j) *( 1.0 - coef ) + q_ndg_new(i,k,j) * coef
     RQVNDGDTEN(i,k,j) = RQVNDGDTEN(i,k,j) + gq * wpbl(i,k,j,4) * wzfac(k,4) * tfac * &
                          ( val_analysis - qv3d(i,k,j) )
   ENDDO
   ENDDO
   ENDDO

   END SUBROUTINE fddagd


   SUBROUTINE sfddagd(itimestep,dx,dt,xtime,  &
               id, analysis_interval_sfc, end_fdda_hour_sfc, &
               guv_sfc, gt_sfc, gq_sfc, rinblw,  &
               u3d,v3d,th3d,t3d,                 &
               qv3d,     &
               p3d,pi3d,                &
               u10_ndg_old, v10_ndg_old, t2_ndg_old, th2_ndg_old, q2_ndg_old, &
               rh_ndg_old, psl_ndg_old, ps_ndg_old, tob_ndg_old, odis_ndg_old,  &
               u10_ndg_new, v10_ndg_new, t2_ndg_new, th2_ndg_new, q2_ndg_new, &
               rh_ndg_new, psl_ndg_new, ps_ndg_new, tob_ndg_new, odis_ndg_new,  &
               RUNDGDTEN,RVNDGDTEN,RTHNDGDTEN,RQVNDGDTEN,RMUNDGDTEN, &
               pblh, ht, regime, znt, z, z_at_w,                             &
               ids,ide, jds,jde, kds,kde,                           &
               ims,ime, jms,jme, kms,kme,                           &
               its,ite, jts,jte, kts,kte, wpbl, wzfac, if_ramping, dtramp_min, &
               actual_end_fdda_min, tfac)


   USE module_model_constants
 
   implicit none












































   INTEGER,  INTENT(IN)   ::      itimestep, analysis_interval_sfc, end_fdda_hour_sfc

   INTEGER , INTENT(IN)   ::      id
   REAL,     INTENT(IN)   ::      dx,DT, xtime

   INTEGER,  INTENT(IN)   ::      ids,ide, jds,jde, kds,kde, &
                                  ims,ime, jms,jme, kms,kme, &
                                  its,ite, jts,jte, kts,kte
 
   REAL,     DIMENSION( ims:ime, kms:kme, jms:jme ), &
             INTENT(IN)   ::                   qv3d, &
                                               p3d, &
                                              pi3d, &
                                              th3d, &
                                               t3d, &
                                                 z, &
                                            z_at_w

   REAL,     DIMENSION( ims:ime, kms:kme, jms:jme ), &
             INTENT(INOUT)   ::           rundgdten, &
                                          rvndgdten, &
                                         rthndgdten, &
                                         rqvndgdten

   REAL,     DIMENSION( ims:ime, jms:jme ), &
             INTENT(INOUT)   ::          rmundgdten

   REAL,       DIMENSION( ims:ime, jms:jme ),            &
               INTENT(IN)       ::                       u10_ndg_old,  &
                                                         v10_ndg_old,  &
                                                         t2_ndg_old,   &
                                                         th2_ndg_old,  &
                                                         q2_ndg_old,   &
                                                         rh_ndg_old,   &
                                                         psl_ndg_old,  &
                                                         ps_ndg_old,   &
                                                         u10_ndg_new,  &
                                                         v10_ndg_new,  &
                                                         t2_ndg_new,   &
                                                         th2_ndg_new,  &
                                                         q2_ndg_new,   &
                                                         rh_ndg_new,   &
                                                         psl_ndg_new,  &
                                                         ps_ndg_new

   REAL,       DIMENSION( ims:ime, jms:jme ),            &
               INTENT(IN)       ::                       tob_ndg_old,  &
                                                         tob_ndg_new

   REAL,     DIMENSION( ims:ime, kms:kme, jms:jme ), &
             INTENT(IN)   ::                    u3d, &
                                                v3d

   REAL,       DIMENSION( ims:ime, jms:jme ),            &
               INTENT(IN)         ::                    odis_ndg_old, odis_ndg_new

   REAL,  DIMENSION( ims:ime, jms:jme ), INTENT(IN) :: pblh, &
                                                       ht,   &
                                                       regime, &
                                                       znt

   REAL, INTENT(IN)    :: guv_sfc, gt_sfc, gq_sfc, rinblw

   INTEGER             :: i, j, k, itsu, jtsv, itf, jtf, ktf, i0, j0
   REAL                :: xtime_old_sfc, xtime_new_sfc, coef, val_analysis, es
   INTEGER             :: kpbl, dbg_level

   REAL                :: zpbl, zagl, zagl_bot, zagl_top, tfac, actual_end_fdda_min

   REAL, DIMENSION( its:ite, kts:kte, jts:jte, 4 ), &
         INTENT(IN)                                   :: wpbl  
   REAL, DIMENSION( kts:kte, 4 ),  &
         INTENT(IN)                                   :: wzfac 
   REAL, DIMENSION( its:ite, jts:jte)                 :: wndcor_u, wndcor_v
   REAL, DIMENSION( its-2:ite+2, jts-2:jte+2)         :: blw_old, blw_new
   REAL, DIMENSION( its:ite, kts:kte, jts:jte)        :: qsat

   REAL                                               :: m, b=1.8, blw, rindx, x

   REAL :: difz, wr14, wr1z, wr24, wr2z, wndfac, reg, znt0
   INTEGER,  INTENT(IN)   :: if_ramping
   REAL, INTENT(IN)       :: dtramp_min

   LOGICAL , EXTERNAL     :: wrf_dm_on_monitor

   CHARACTER (LEN=256)    :: message
   INTEGER :: iwinds, idd, iqsat, int4

   iwinds = 1    
                 
                 
   idd = 1       
   iqsat = 1     
   int4 = 1      

   IF( analysis_interval_sfc <= 0 )CALL wrf_error_fatal3("<stdin>",716,&
'In grid sfc FDDA, sgfdda_interval_m must be > 0')
   xtime_old_sfc = FLOOR(xtime/analysis_interval_sfc) * analysis_interval_sfc * 1.0
   xtime_new_sfc = xtime_old_sfc + analysis_interval_sfc
   IF( int4 == 1 ) THEN
     coef = (xtime-xtime_old_sfc)/(xtime_new_sfc-xtime_old_sfc)  
   ELSE
     coef = 1.0          
   ENDIF

   IF ( wrf_dm_on_monitor()) THEN

     CALL get_wrf_debug_level( dbg_level )

     IF( xtime-xtime_old_sfc < 0.5*dt/60.0 ) THEN

       IF( xtime < end_fdda_hour_sfc*60.0 ) THEN
         WRITE(message,'(a,i1,a,f10.3,a)') &
          'D0',id,' surface analysis nudging reads new data at time = ', xtime, ' min.'
         CALL wrf_message( TRIM(message) )
         WRITE(message,'(a,i1,a,2f8.2,a)') &
          'D0',id,' surface analysis nudging bracketing times = ', xtime_old_sfc, xtime_new_sfc, ' min.'
         CALL wrf_message( TRIM(message) )
       ENDIF

       IF( dbg_level .GE. 10 .AND. xtime <= actual_end_fdda_min ) THEN

         i0 = (ite-its)/2+its
         j0 = (jte-jts)/2+jts 

         IF( guv_sfc > 0.0 ) THEN
           WRITE(message,'(a,i1,a,2i4,a,f10.4,a,f10.4)') &
             '    D0',id,' sample surface analysis values at i,j=', i0, j0, &
             ' u10_ndg_old=', u10_ndg_old(i0,j0), ' u10_ndg_new=', u10_ndg_new(i0,j0)
           CALL wrf_message( TRIM(message) )
           WRITE(message,'(a,i1,a,2i4,a,f10.4,a,f10.4)') &
             '    D0',id,' sample surface analysis values at i,j=', i0, j0, &
             ' v10_ndg_old=', v10_ndg_old(i0,j0), ' v10_ndg_new=', v10_ndg_new(i0,j0)
           CALL wrf_message( TRIM(message) )
         ENDIF

         IF( gt_sfc > 0.0 ) THEN
           WRITE(message,'(a,i1,a,2i4,a,f10.4,a,f10.4)') &
             '    D0',id,' sample surface analysis values at i,j=', i0, j0, &
             ' th2_ndg_old=', th2_ndg_old(i0,j0), ' th2_ndg_new=', th2_ndg_new(i0,j0)
           CALL wrf_message( TRIM(message) )
         ENDIF

         IF( gq_sfc > 0.0 ) THEN
           WRITE(message,'(a,i1,a,2i4,a,f10.4,a,f10.4)') &
             '    D0',id,' sample surface analysis values at i,j=', i0, j0, &
             ' q2_ndg_old=', q2_ndg_old(i0,j0), ' q2_ndg_new=', q2_ndg_new(i0,j0)
           CALL wrf_message( TRIM(message) )
         ENDIF

         IF( iwinds ==  1 ) &
           WRITE(message,'(a,i1,a)') '    D0',id, &
              ' surface wind analysis s scaled to the lowest model level, if dz1 > 10m and REGIME=4.'

         IF( idd ==  1 ) &
           WRITE(message,'(a,i1,a)') '    D0',id, &
              ' obs data density is used for additional weighting function'

         IF( iqsat ==  1 ) &
           WRITE(message,'(a,i1,a)') '    D0',id, &
              ' super saturation is not allowed for q analysis'

         IF( int4 ==  1 ) then
           WRITE(message,'(a,i1,a)') '    D0',id, &
              ' surface nudging towards the temporally interpolated analysis'
         ELSE
           WRITE(message,'(a,i1,a)') '    D0',id, &
              ' surface nudging towards the target analysis'
         ENDIF

       ENDIF
     ENDIF
   ENDIF


   jtsv=MAX0(jts,jds+1)
   itsu=MAX0(its,ids+1)

   jtf=MIN0(jte,jde-1)
   ktf=MIN0(kte,kde-1)
   itf=MIN0(ite,ide-1)






   IF( iwinds == 1 ) THEN
     wndcor_u(:,:) = 1.0
     DO j=jts,jtf
     DO i=itsu,itf
       reg =  0.5 * ( regime(i-1,  j) + regime(i,  j) )
       difz = 0.5 * ( z(i-1,1,j) - ht(i-1,j)  &
                    + z(i,  1,j) - ht(i,  j)    ) 
       IF( reg > 3.5 .AND. difz > 10.0 ) THEN
         znt0 = 0.5 * (    znt(i-1,  j) +    znt(i,  j) )
         IF( znt0 <= 0.2) THEN
           wndcor_u(i,j) = 1.0+0.320*znt0**0.2
         ELSE
           wndcor_u(i,j) = 1.169+0.315*znt0
         ENDIF

         wr14 = log(40.0/0.05)
         wr1z = log(difz/0.05)
         wr24 = log(40.0/1.0)
         wr2z = log(difz/1.0)
         wndfac = 0.5*(WR1Z/WR14+WR2Z/WR24) 
         wndcor_u(i,j) = wndfac*wndcor_u(i,j)
       ENDIF
     ENDDO
     ENDDO

     IF ( wrf_dm_on_monitor()) THEN
       IF( xtime-xtime_old_sfc < 0.5*dt/60.0 ) THEN
         IF( dbg_level .GE. 10 .AND. xtime <= actual_end_fdda_min ) THEN
           i0 = (ite-its)/2+its
           j0 = (jte-jts)/2+jts
           WRITE(message,'(a,i1,a,2i4,a,f10.4)') &
             '    D0',id,' sample wndcor_u values at i,j=', i0, j0, &
             ' wndcor_u=', wndcor_u(i0,j0)
           CALL wrf_message( TRIM(message) )
         ENDIF
       ENDIF
     ENDIF
   ELSE 
     wndcor_u(:,:) = 1.0
   ENDIF

   IF( iwinds == 1 ) THEN
     wndcor_v(:,:) = 1.0
     DO j=jtsv,jtf
     DO i=its,itf
       reg =  0.5 * ( regime(i,  j-1) + regime(i,  j) )
       difz = 0.5 * ( z(i,1,j-1) - ht(i,j-1)  &
                  +   z(i,1,j  ) - ht(i,j  )    )
       IF( reg > 3.5 .AND. difz > 10.0 ) THEN 
         znt0 = 0.5 * (    znt(i,  j-1) +    znt(i,  j) )
         IF( znt0 <= 0.2) THEN
           wndcor_v(i,j) = 1.0+0.320*znt0**0.2
         ELSE
           wndcor_v(i,j) = 1.169+0.315*znt0
         ENDIF
       
         wr14 = log(40.0/0.05)
         wr1z = log(difz/0.05)
         wr24 = log(40.0/1.0)
         wr2z = log(difz/1.0)
         wndfac = 0.5*(WR1Z/WR14+WR2Z/WR24)
           wndcor_v(i,j) = wndfac*wndcor_v(i,j)
       ENDIF
     ENDDO
     ENDDO
     IF ( wrf_dm_on_monitor()) THEN
       IF( xtime-xtime_old_sfc < 0.5*dt/60.0 ) THEN
         IF( dbg_level .GE. 10 .AND. xtime <= actual_end_fdda_min ) THEN
           i0 = (ite-its)/2+its
           j0 = (jte-jts)/2+jts
           WRITE(message,'(a,i1,a,2i4,a,f10.4)') &
             '    D0',id,' sample wndcor_v values at i,j=', i0, j0, &
             ' wndcor_v=', wndcor_v(i0,j0)
           CALL wrf_message( TRIM(message) )
         ENDIF
       ENDIF
     ENDIF
   ELSE 
     wndcor_v(:,:) = 1.0
   ENDIF




   IF( iqsat == 1 ) THEN
     DO j=jts,jtf
     DO k=kts,ktf
     DO i=its,itf
       es = SVP1*EXP(SVP2*(t3d(i,k,j)-SVPT0)/(t3d(i,k,j)-SVP3))  * 10.0    
       qsat(i,k,j) = EP_2*es/(p3d(i,k,j)/100.0-es)
     ENDDO
     ENDDO
     ENDDO

     IF ( wrf_dm_on_monitor()) THEN
       IF( xtime-xtime_old_sfc < 0.5*dt/60.0 ) THEN
         IF( dbg_level .GE. 10 .AND. xtime <= actual_end_fdda_min ) THEN
           i0 = (ite-its)/2+its
           j0 = (jte-jts)/2+jts 
           DO k = kts, kte
             WRITE(message,'(a,i1,a,3i4,a,f10.4,a,f10.4)') &
               '    D0',id,' sample moisture values (g/kg) at i,k,j=', i0, k, j0, &
               ' qv3d=', qv3d(i0,k,j0)*1000.0, ' qsat=', qsat(i0,k,j0)*1000.0
             CALL wrf_message( TRIM(message) )
           ENDDO
         ENDIF
       ENDIF
     ENDIF
   ENDIF



   IF( idd == 1 ) THEN

     IF( rinblw < 0.001 ) THEN
       IF ( wrf_dm_on_monitor()) THEN
         WRITE(message,'(a)') 'Error in rinblw, please specify a reasonable value ***'
         CALL wrf_message( TRIM(message) )
       ENDIF
       CALL wrf_error_fatal3("<stdin>",927,&
'In grid FDDA')
     ENDIF

     rindx = rinblw*1000.0/dx
     m = -0.8*2.0/rindx

     DO j=MAX(jts-2,jds),MIN(jte+2,jde-1)
     DO i=MAX(its-2,ids),MIN(ite+2,ide-1)
       IF( odis_ndg_old(i,j) < 0.5*rinblw ) THEN
         blw_old(i,j) = 1.0
       ELSE
         x = min( odis_ndg_old(i,j)*1000./dx, rindx )
         blw_old(i,j) = m * x + b
       ENDIF

       IF( odis_ndg_new(i,j) < 0.5*rinblw ) THEN
         blw_new(i,j) = 1.0
       ELSE
         x = min( odis_ndg_new(i,j)*1000./dx, rindx )
         blw_new(i,j) = m * x + b
       ENDIF
     ENDDO
     ENDDO


     CALL smther(blw_old, its-2,ite+2, jts-2,jte+2, 1, &
                 MAX(its-1,ids+1), MIN(ite+1,ide-2), MAX(jts-1,jds+1), MIN(jte+1,jde-2))
     CALL smther(blw_new, its-2,ite+2, jts-2,jte+2, 1, &
                 MAX(its-1,ids+1), MIN(ite+1,ide-2), MAX(jts-1,jds+1), MIN(jte+1,jde-2))

     WHERE ( blw_old > 1.0)
        blw_old = 1.0
     END WHERE
     WHERE ( blw_new > 1.0)
        blw_new = 1.0
     END WHERE
     WHERE ( blw_old < 0.0)
        blw_old = 0.0
     END WHERE
     WHERE ( blw_new < 0.0)
        blw_new = 0.0
     END WHERE

     IF ( wrf_dm_on_monitor()) THEN
       IF( xtime-xtime_old_sfc < 0.5*dt/60.0 ) THEN
         IF( dbg_level .GE. 10 .AND. xtime <= actual_end_fdda_min ) THEN
           i0 = (ite-its)/2+its
           j0 = (jte-jts)/2+jts
           WRITE(message,'(a,i1,a,2i4,4(a,f10.4))') &
             '    D0',id,' sample blw values at i,j=', i0, j0, &
             ' odis_ndg_old=', odis_ndg_old(i0,j0), ' km odis_ndg_new=', odis_ndg_new(i0,j0), &
             ' km  blw_old=', blw_old(i0,j0), ' blw_new=', blw_new(i0,j0)
           CALL wrf_message( TRIM(message) )
         ENDIF
       ENDIF
     ENDIF

   ENDIF



   IF( xtime >= actual_end_fdda_min-ABS(dtramp_min) .AND. xtime <= actual_end_fdda_min &
       .AND. dtramp_min > 0.0 .AND. if_ramping == 1 )  &
    coef = (xtime-xtime_old_sfc+analysis_interval_sfc)/(analysis_interval_sfc*1.0)




   DO j=jts,jtf
   DO k=kts,ktf
   DO i=itsu,itf
     IF( idd == 1 ) THEN
       blw = 0.5* (blw_old(i-1,j)+blw_old(i,j)) * ( 1.0 - coef ) &
           + 0.5* (blw_new(i-1,j)+blw_new(i,j)) * coef
     ELSE
       blw = 1.0
     ENDIF
     val_analysis = u10_ndg_old(i,j) *( 1.0 - coef ) + u10_ndg_new(i,j) * coef
     val_analysis = val_analysis * wndcor_u(i,j)
     RUNDGDTEN(i,k,j) = guv_sfc * (1.0-wpbl(i,k,j,1)) * wzfac(k,1) * tfac * blw * &
                         ( val_analysis - u3d(i,1,j) )
   ENDDO
   ENDDO
   ENDDO

   DO j=jtsv,jtf
   DO k=kts,ktf 
   DO i=its,itf
     IF( idd == 1 ) THEN
       blw = 0.5* (blw_old(i,j-1)+blw_old(i,j)) * ( 1.0 - coef ) &
           + 0.5* (blw_new(i,j-1)+blw_new(i,j)) * coef
     ELSE
       blw = 1.0
     ENDIF
     val_analysis = v10_ndg_old(i,j) *( 1.0 - coef ) + v10_ndg_new(i,j) * coef
     val_analysis = val_analysis * wndcor_v(i,j)
     RVNDGDTEN(i,k,j) = guv_sfc * (1.0-wpbl(i,k,j,2)) * wzfac(k,2) * tfac * blw * &
                       ( val_analysis - v3d(i,1,j) )
   ENDDO
   ENDDO
   ENDDO

   DO j=jts,jtf
   DO k=kts,ktf
   DO i=its,itf
     IF( idd == 1 ) THEN
       blw = blw_old(i,j) * ( 1.0 - coef ) + blw_new(i,j) * coef
     ELSE
       blw = 1.0
     ENDIF
     val_analysis = th2_ndg_old(i,j) *( 1.0 - coef ) + th2_ndg_new(i,j) * coef
     RTHNDGDTEN(i,k,j) = gt_sfc * (1.0-wpbl(i,k,j,3)) * wzfac(k,3) * tfac * blw * &
                          ( val_analysis - th3d(i,1,j))

     val_analysis = q2_ndg_old(i,j) *( 1.0 - coef ) + q2_ndg_new(i,j) * coef
     IF( iqsat ==  1 .AND. val_analysis > qsat(i,k,j) ) val_analysis = qsat(i,k,j)
     RQVNDGDTEN(i,k,j) = gq_sfc * (1.0-wpbl(i,k,j,4)) * wzfac(k,4) * tfac * blw * &
                          ( val_analysis - qv3d(i,k,j) )
   ENDDO
   ENDDO
   ENDDO

   END SUBROUTINE sfddagd



   SUBROUTINE fddagdinit(id,rundgdten,rvndgdten,rthndgdten,rqvndgdten,rmundgdten,&
               run_hours,  &
               if_no_pbl_nudging_uv, if_no_pbl_nudging_t, if_no_pbl_nudging_q, &
               if_zfac_uv, k_zfac_uv, if_zfac_t, k_zfac_t, if_zfac_q, k_zfac_q, &
               guv, gt, gq, if_ramping, dtramp_min, end_fdda_hour, &
               grid_sfdda, guv_sfc, gt_sfc, gq_sfc,                &
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
                                                      rqvndgdten
   INTEGER,  INTENT(IN)   ::      run_hours
   INTEGER,  INTENT(IN)   ::      if_no_pbl_nudging_uv, if_no_pbl_nudging_t, &
                                  if_no_pbl_nudging_q, end_fdda_hour
   INTEGER,  INTENT(IN)   ::      if_zfac_uv, if_zfac_t, if_zfac_q
   INTEGER,  INTENT(IN)   ::      k_zfac_uv,  k_zfac_t,  k_zfac_q
   INTEGER,  INTENT(IN)   ::      if_ramping, grid_sfdda
   REAL,     INTENT(IN)   ::      dtramp_min
   REAL, INTENT(IN)       ::      guv, gt, gq
   REAL, INTENT(IN)       ::      guv_sfc, gt_sfc, gq_sfc
   REAL                   ::      actual_end_fdda_min

   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(OUT) :: rmundgdten
   INTEGER :: i, j, k

   LOGICAL , EXTERNAL     ::      wrf_dm_on_monitor

   CHARACTER (LEN=256) :: message

   IF ( wrf_dm_on_monitor() ) THEN  

     IF( guv > 0.0 ) THEN
       WRITE(message,'(a,i1,a,e12.4)') &
           'D0',id,' 3-D analysis nudging for wind is applied and Guv= ', guv
       CALL wrf_message(TRIM(message))
     ELSE IF( guv < 0.0 ) THEN
       CALL wrf_error_fatal3("<stdin>",1103,&
'In grid FDDA, Guv must be positive.')
     ELSE 
       WRITE(message,'(a,i1,a,e12.4)') &
           'D0',id,' 3-D analysis nudging for wind is not applied and Guv= ', guv
       CALL wrf_message(TRIM(message))
     ENDIF

     IF( gt > 0.0 ) THEN
       WRITE(message,'(a,i1,a,e12.4)') &
           'D0',id,' 3-D analysis nudging for temperature is applied and Gt= ', gt
       CALL wrf_message(TRIM(message))
     ELSE IF( gt < 0.0 ) THEN
       CALL wrf_error_fatal3("<stdin>",1116,&
'In grid FDDA, Gt must be positive.')
     ELSE 
       WRITE(message,'(a,i1,a,e12.4)') &
           'D0',id,' 3-D analysis nudging for temperature is not applied and Gt= ', gt
       CALL wrf_message(TRIM(message))
     ENDIF

     IF( gq > 0.0 ) THEN
       WRITE(message,'(a,i1,a,e12.4)') &
         'D0',id,' 3-D analysis nudging for water vapor mixing ratio is applied and Gq= ', gq
       CALL wrf_message(TRIM(message))
     ELSE IF( gq < 0.0 ) THEN
       CALL wrf_error_fatal3("<stdin>",1129,&
'In grid FDDA, Gq must be positive.')
     ELSE
       WRITE(message,'(a,i1,a,e12.4)') &
         'D0',id,' 3-D analysis nudging for water vapor mixing ratio is not applied and Gq= ', gq
       CALL wrf_message(TRIM(message))
     ENDIF

     IF( guv > 0.0 .AND. if_no_pbl_nudging_uv == 1 ) THEN
        WRITE(message,'(a,i1,a)') &
           'D0',id,' 3-D analysis nudging for wind is turned off within the PBL.'
        CALL wrf_message(TRIM(message))
     ENDIF

     IF( gt > 0.0 .AND. if_no_pbl_nudging_t == 1 ) THEN
        WRITE(message,'(a,i1,a)') &
           'D0',id,' 3-D analysis nudging for temperature is turned off within the PBL.'
        CALL wrf_message(TRIM(message))
     ENDIF

     IF( gq > 0.0 .AND. if_no_pbl_nudging_q == 1 ) THEN
        WRITE(message,'(a,i1,a)') &
         'D0',id,' 3-D analysis nudging for water vapor mixing ratio is turned off within the PBL.'
        CALL wrf_message(TRIM(message))
     ENDIF

     IF( guv > 0.0 .AND. if_zfac_uv == 1 ) THEN
        WRITE(message,'(a,i1,a,i3)') &
           'D0',id,' 3-D analysis nudging for wind is turned off below layer', k_zfac_uv
        CALL wrf_message(TRIM(message))
     ENDIF

     IF( gt > 0.0 .AND. if_zfac_t == 1 ) THEN
        WRITE(message,'(a,i1,a,i3)') &
           'D0',id,' 3-D analysis nudging for temperature is turned off below layer', k_zfac_t
        CALL wrf_message(TRIM(message))
     ENDIF

     IF( gq > 0.0 .AND. if_zfac_q == 1 ) THEN
        WRITE(message,'(a,i1,a,i3)') &
          'D0',id,' 3-D analysis nudging for water vapor mixing ratio is turned off below layer', &
           k_zfac_q
        CALL wrf_message(TRIM(message))
     ENDIF

     IF( grid_sfdda ==1 ) THEN
       IF( guv_sfc > 0.0 ) THEN
         WRITE(message,'(a,i1,a,e12.4)') &
             'D0',id,' surface analysis nudging for wind is applied and Guv_sfc= ', guv_sfc
         CALL wrf_message(TRIM(message))
       ELSE IF( guv_sfc < 0.0 ) THEN
         CALL wrf_error_fatal3("<stdin>",1180,&
'In grid FDDA, Guv_sfc must be positive.')
       ELSE
         WRITE(message,'(a,i1,a,e12.4)') &
             'D0',id,' surface analysis nudging for wind is not applied and Guv_sfc= ', guv_sfc
         CALL wrf_message(TRIM(message))
       ENDIF

       IF( gt_sfc > 0.0 ) THEN
         WRITE(message,'(a,i1,a,e12.4)') &
             'D0',id,' surface analysis nudging for temperature is applied and Gt_sfc= ', gt_sfc
         CALL wrf_message(TRIM(message))
       ELSE IF( gt_sfc < 0.0 ) THEN
         CALL wrf_error_fatal3("<stdin>",1193,&
'In grid FDDA, Gt_sfc must be positive.')
       ELSE
         WRITE(message,'(a,i1,a,e12.4)') &
             'D0',id,' surafce analysis nudging for temperature is not applied and Gt_sfc= ', gt_sfc
         CALL wrf_message(TRIM(message))
       ENDIF

       IF( gq_sfc > 0.0 ) THEN
         WRITE(message,'(a,i1,a,e12.4)') &
           'D0',id,' surface analysis nudging for water vapor mixing ratio is applied and Gq_sfc= ', gq_sfc
         CALL wrf_message(TRIM(message))
       ELSE IF( gq_sfc < 0.0 ) THEN
         CALL wrf_error_fatal3("<stdin>",1206,&
'In grid FDDA, Gq_sfc must be positive.')
       ELSE
         WRITE(message,'(a,i1,a,e12.4)') &
           'D0',id,' surface analysis nudging for water vapor mixing ratio is not applied and Gq_sfc= ', gq_sfc
         CALL wrf_message(TRIM(message))
       ENDIF

     ENDIF

     IF( if_ramping == 1 .AND. ABS(dtramp_min) > 0.0 ) THEN
       IF( dtramp_min <= 0.0 ) THEN
         actual_end_fdda_min = end_fdda_hour*60.0
       ELSE
         actual_end_fdda_min = end_fdda_hour*60.0 + ABS(dtramp_min)
       ENDIF

       IF( actual_end_fdda_min <= run_hours*60. ) THEN
          WRITE(message,'(a,i1,a)') &
            'D0',id,' analysis nudging is ramped down near the end of the nudging period,'
          CALL wrf_message(TRIM(message))

          WRITE(message,'(a,f6.2,a,f6.2,a)') &
             '      starting at ', (actual_end_fdda_min - ABS(dtramp_min))/60.0, &
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
        rqvndgdten(i,k,j) = 0.
        if(k.eq.kts) rmundgdten(i,j) = 0.
     ENDDO
     ENDDO
     ENDDO
   ENDIF

   END SUBROUTINE fddagdinit


  SUBROUTINE smther(slab, idimst, idimnd, jdimst, jdimnd, npass, ist, ind, jst, jnd)








    IMPLICIT NONE

    INTEGER :: idimst, idimnd, jdimst, jdimnd, npass, ist, ind, jst, jnd
    INTEGER :: i, j, k, kk
    REAL    :: avg
    REAL, DIMENSION(idimst:idimnd, jdimst:jdimnd) :: SLAB
    REAL, DIMENSION(idimst:idimnd, jdimst:jdimnd) :: SLAB_ORIG
    REAL, DIMENSION(2)                            :: XNU

    IF(NPASS.EQ.0)RETURN
    XNU(1)=0.50
    XNU(2)=-0.52
    DO K=1,NPASS
      KK = 2 - MOD(K,2)

      DO J=JDIMST,JDIMND
        DO I=IDIMST,IDIMND
           SLAB_ORIG(I,J) = SLAB(I,J)
        END DO
      END DO

      DO J=JST,JND
        DO I=IST,IND
          AVG = ( SLAB_ORIG(I+1,J  ) + &
                  SLAB_ORIG(I-1,J  ) + &
                  SLAB_ORIG(I  ,J+1) + &
                  SLAB_ORIG(I  ,J-1) ) * 0.25
          SLAB(I,J)=SLAB_ORIG(I,J)+XNU(KK)*(AVG - SLAB_ORIG(I,J))
        ENDDO
      ENDDO

    ENDDO

  END SUBROUTINE smther


END MODULE module_fdda_psufddagd
