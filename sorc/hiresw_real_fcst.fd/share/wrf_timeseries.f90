





SUBROUTINE calc_ts_locations( grid )

   USE module_domain
   USE module_configure
   USE module_dm
   USE module_llxy

   IMPLICIT NONE

   
   TYPE (domain), INTENT(INOUT) :: grid

   
   LOGICAL, EXTERNAL :: wrf_dm_on_monitor
   INTEGER, EXTERNAL :: get_unused_unit

   
   INTEGER :: ntsloc_temp
   INTEGER :: i, k, iunit
   REAL :: ts_rx, ts_ry, ts_xlat, ts_xlong, ts_hgt
   REAL :: known_lat, known_lon
   CHARACTER (LEN=132) :: message
   TYPE (PROJ_INFO) :: ts_proj
   TYPE (grid_config_rec_type) :: config_flags

   INTEGER :: ids, ide, jds, jde, kds, kde,        &
              ims, ime, jms, jme, kms, kme,        &
              ips, ipe, jps, jpe, kps, kpe,        &
              imsx, imex, jmsx, jmex, kmsx, kmex,  &
              ipsx, ipex, jpsx, jpex, kpsx, kpex,  &
              imsy, imey, jmsy, jmey, kmsy, kmey,  &
              ipsy, ipey, jpsy, jpey, kpsy, kpey


   IF ( grid%ntsloc .LE. 0 ) RETURN

      CALL get_ijk_from_grid ( grid ,                               &
                               ids, ide, jds, jde, kds, kde,        &
                               ims, ime, jms, jme, kms, kme,        &
                               ips, ipe, jps, jpe, kps, kpe,        &
                               imsx, imex, jmsx, jmex, kmsx, kmex,  &
                               ipsx, ipex, jpsx, jpex, kpsx, kpex,  &
                               imsy, imey, jmsy, jmey, kmsy, kmey,  &
                               ipsy, ipey, jpsy, jpey, kpsy, kpey )
   
      CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )
   
      
      CALL map_init(ts_proj)
   
      IF (ips <= 1 .AND. 1 <= ipe .AND. &
          jps <= 1 .AND. 1 <= jpe) THEN
         known_lat = grid%xlat(1,1)
         known_lon = grid%xlong(1,1)
      ELSE
         known_lat = 9999.
         known_lon = 9999.
      END IF
      known_lat = wrf_dm_min_real(known_lat)
      known_lon = wrf_dm_min_real(known_lon)
   
      
      IF (config_flags%map_proj == PROJ_MERC) THEN
         CALL map_set(PROJ_MERC, ts_proj,               &
                      truelat1 = config_flags%truelat1, &
                      lat1     = known_lat,             &
                      lon1     = known_lon,             &
                      knowni   = 1.,                    &
                      knownj   = 1.,                    &
                      dx       = config_flags%dx)
   
      
      ELSE IF (config_flags%map_proj == PROJ_LC) THEN
      CALL map_set(PROJ_LC, ts_proj,                  &
                      truelat1 = config_flags%truelat1,  &
                      truelat2 = config_flags%truelat2,  &
                      stdlon   = config_flags%stand_lon, &
                      lat1     = known_lat,              &
                      lon1     = known_lon,              &
                      knowni   = 1.,                     &
                      knownj   = 1.,                     &
                      dx       = config_flags%dx)
   
      
      ELSE IF (config_flags%map_proj == PROJ_PS) THEN
         CALL map_set(PROJ_PS, ts_proj,                  &
                      truelat1 = config_flags%truelat1,  &
                      stdlon   = config_flags%stand_lon, &
                      lat1     = known_lat,              &
                      lon1     = known_lon,              &
                      knowni   = 1.,                     &
                      knownj   = 1.,                     &
                      dx       = config_flags%dx)
   
      
      ELSE IF (config_flags%map_proj == PROJ_CASSINI) THEN
         CALL map_set(PROJ_CASSINI, ts_proj,                            &
                      latinc   = grid%dy*360.0/(2.0*EARTH_RADIUS_M*PI), &
                      loninc   = grid%dx*360.0/(2.0*EARTH_RADIUS_M*PI), & 
                      lat1     = known_lat,                             &
                      lon1     = known_lon,                             &


                      lat0     = 90.0,                                  &
                      lon0     = 0.0,                                   &
                      knowni   = 1.,                                    &
                      knownj   = 1.,                                    &
                      stdlon   = config_flags%stand_lon)

      
      ELSE IF (config_flags%map_proj == PROJ_ROTLL) THEN
         CALL map_set(PROJ_ROTLL, ts_proj,                      &

                      ixdim    = grid%e_we-1,                   &
                      jydim    = grid%e_sn-1,                   &
                      phi      = real(grid%e_sn-2)*grid%dy/2.0, &
                      lambda   = real(grid%e_we-2)*grid%dx,     &
                      lat1     = config_flags%cen_lat,          &
                      lon1     = config_flags%cen_lon,          &
                      latinc   = grid%dy,                       &
                      loninc   = grid%dx,                       &
                      stagger  = HH)
   
      END IF
   
      
      IF (.NOT. grid%have_calculated_tslocs) THEN
         grid%have_calculated_tslocs = .TRUE.
         WRITE(message, '(A43,I3)') 'Computing time series locations for domain ', grid%id
         CALL wrf_message(message)
   
         ntsloc_temp = 0
         DO k=1,grid%ntsloc
   
            CALL latlon_to_ij(ts_proj, grid%lattsloc(k), grid%lontsloc(k), ts_rx, ts_ry)

            ntsloc_temp = ntsloc_temp + 1
            grid%itsloc(ntsloc_temp) = NINT(ts_rx)
            grid%jtsloc(ntsloc_temp) = NINT(ts_ry)
            grid%id_tsloc(ntsloc_temp) = k
   
            
            IF (grid%itsloc(ntsloc_temp) < ids .OR. grid%itsloc(ntsloc_temp) > ide .OR. &
                grid%jtsloc(ntsloc_temp) < jds .OR. grid%jtsloc(ntsloc_temp) > jde) THEN
               ntsloc_temp = ntsloc_temp - 1
   
            END IF
   
         END DO
   
         grid%next_ts_time = 1
   
         grid%ntsloc_domain = ntsloc_temp
   
         DO k=1,grid%ntsloc_domain
   
            
            IF (grid%itsloc(k) < ips .OR. grid%itsloc(k) > ipe .OR. &
                grid%jtsloc(k) < jps .OR. grid%jtsloc(k) > jpe) THEN
               ts_xlat  = 1.E30
               ts_xlong = 1.E30
               ts_hgt   = 1.E30
            ELSE
               ts_xlat  = grid%xlat(grid%itsloc(k),grid%jtsloc(k))
               ts_xlong = grid%xlong(grid%itsloc(k),grid%jtsloc(k))
            END IF
            ts_xlat  = wrf_dm_min_real(ts_xlat)
            ts_xlong = wrf_dm_min_real(ts_xlong)
            ts_hgt   = wrf_dm_min_real(ts_hgt)
   
            IF ( wrf_dm_on_monitor() ) THEN

               iunit = get_unused_unit()
               IF ( iunit <= 0 ) THEN
                  CALL wrf_error_fatal3("",181,&
'Error in calc_ts_locations: could not find a free Fortran unit.')
               END IF

               WRITE(grid%ts_filename(k),'(A)') TRIM(grid%nametsloc(grid%id_tsloc(k)))//'.d00.TS'
               i = LEN_TRIM(grid%ts_filename(k))
               WRITE(grid%ts_filename(k)(i-4:i-3),'(I2.2)') grid%id
               OPEN(UNIT=iunit, FILE=TRIM(grid%ts_filename(k)), FORM='FORMATTED', STATUS='REPLACE')
               WRITE(UNIT=iunit, &
                     FMT='(A26,I2,I3,A6,A2,F7.3,A1,F8.3,A3,I4,A1,I4,A3,F7.3,A1,F8.3,A2)') &
                     grid%desctsloc(grid%id_tsloc(k))//' ', grid%id, grid%id_tsloc(k), &
                     ' '//grid%nametsloc(grid%id_tsloc(k)), &
                     ' (', grid%lattsloc(grid%id_tsloc(k)), ',', grid%lontsloc(grid%id_tsloc(k)), ') (', &
                     grid%itsloc(k), ',', grid%jtsloc(k), ') (', &
                     ts_xlat, ',', ts_xlong, ') '
               CLOSE(UNIT=iunit)
            END IF
         END DO
   
      END IF

END SUBROUTINE calc_ts_locations


SUBROUTINE calc_ts( grid )

   USE module_domain
   USE module_model_constants

   IMPLICIT NONE

   
   TYPE (domain), INTENT(INOUT) :: grid

   LOGICAL, EXTERNAL :: wrf_dm_on_monitor

   
   INTEGER :: i, k, mm, n, ix, iy, rc
   REAL :: earth_u, earth_v, output_t, output_q, clw, xtime_minutes
   REAL, ALLOCATABLE, DIMENSION(:) :: p8w

   
       
       
   LOGICAL, PARAMETER :: ts_model_level = .FALSE.  

   IF ( grid%ntsloc_domain .LE. 0 ) RETURN


   n = grid%next_ts_time

   ALLOCATE(p8w(grid%sm32:grid%em32))

   DO i=1,grid%ntsloc_domain

      ix = grid%itsloc(i)
      iy = grid%jtsloc(i)
  
      IF (grid%sp31 <= ix .AND. ix <= grid%ep31 .AND. &
          grid%sp33 <= iy .AND. iy <= grid%ep33) THEN
       
         IF (ts_model_level) THEN
   
            
            
            
            earth_u = grid%u(ix,1,iy)
            earth_v = grid%v(ix,1,iy)
            output_t = grid%t(ix,1,iy)
            output_q = grid%moist(ix,1,iy,P_QV)
   
         ELSE
   
            
            
            
            earth_u = grid%u10(ix,iy)
            earth_v = grid%v10(ix,iy)
            output_q = grid%qsfc(ix,iy)
            output_t = grid%t2(ix,iy)
   
         END IF
   
   
         CALL domain_clock_get( grid, minutesSinceSimulationStart=xtime_minutes )
         grid%ts_hour(n,i) = xtime_minutes / 60.
         grid%ts_u(n,i)    = earth_u
         grid%ts_v(n,i)    = earth_v
         grid%ts_t(n,i)    = output_t
         grid%ts_q(n,i)    = output_q
         grid%ts_psfc(n,i) = grid%psfc(ix,iy)
         grid%ts_tsk(n,i)  = grid%nmm_tsk(ix,iy)
         grid%ts_tslb(n,i) = grid%tslb(ix,1,iy)
   
      ELSE
  
         grid%ts_hour(n,i) = 1.E30
         grid%ts_u(n,i)    = 1.E30
         grid%ts_v(n,i)    = 1.E30
         grid%ts_t(n,i)    = 1.E30
         grid%ts_q(n,i)    = 1.E30
         grid%ts_psfc(n,i) = 1.E30
         grid%ts_tsk(n,i)  = 1.E30
         grid%ts_tslb(n,i) = 1.E30
   
      END IF
   END DO

   DEALLOCATE(p8w)
 
   grid%next_ts_time = grid%next_ts_time + 1

   IF ( grid%next_ts_time > grid%ts_buf_size ) CALL write_ts(grid)

END SUBROUTINE calc_ts


SUBROUTINE write_ts( grid )

   USE module_domain
   USE module_dm

   IMPLICIT NONE

   
   TYPE (domain), INTENT(INOUT) :: grid

   LOGICAL, EXTERNAL :: wrf_dm_on_monitor
   INTEGER, EXTERNAL :: get_unused_unit

   
   INTEGER :: i, n, ix, iy, iunit
   REAL, ALLOCATABLE, DIMENSION(:,:) :: ts_buf

   IF ( grid%ntsloc_domain .LE. 0 ) RETURN


   ALLOCATE(ts_buf(grid%ts_buf_size,grid%max_ts_locs))

   ts_buf(:,:) = grid%ts_hour(:,:)
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_hour(:,:),grid%ts_buf_size*grid%max_ts_locs)

   ts_buf(:,:) = grid%ts_u(:,:)
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_u(:,:),grid%ts_buf_size*grid%max_ts_locs)

   ts_buf(:,:) = grid%ts_v(:,:)
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_v(:,:),grid%ts_buf_size*grid%max_ts_locs)

   ts_buf(:,:) = grid%ts_t(:,:)
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_t(:,:),grid%ts_buf_size*grid%max_ts_locs)

   ts_buf(:,:) = grid%ts_q(:,:)
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_q(:,:),grid%ts_buf_size*grid%max_ts_locs)

   ts_buf(:,:) = grid%ts_psfc(:,:)
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_psfc(:,:),grid%ts_buf_size*grid%max_ts_locs)


   ts_buf(:,:) = grid%ts_tsk(:,:)
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_tsk(:,:),grid%ts_buf_size*grid%max_ts_locs)

   ts_buf(:,:) = grid%ts_tslb(:,:)
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_tslb(:,:),grid%ts_buf_size*grid%max_ts_locs)

   DEALLOCATE(ts_buf)

   IF ( wrf_dm_on_monitor() ) THEN

      iunit = get_unused_unit()
      IF ( iunit <= 0 ) THEN
         CALL wrf_error_fatal3("",351,&
'Error in write_ts: could not find a free Fortran unit.')
      END IF

      DO i=1,grid%ntsloc_domain

         ix = grid%itsloc(i)
         iy = grid%jtsloc(i)

         OPEN(UNIT=iunit, FILE=TRIM(grid%ts_filename(i)), STATUS='unknown', POSITION='append', FORM='formatted')

         DO n=1,grid%next_ts_time - 1

            WRITE(UNIT=iunit,FMT='(i2,f13.6,i5,i5,i5,1x,7(f13.5,1x))')  &
                              grid%id, grid%ts_hour(n,i),        &
                              grid%id_tsloc(i), ix, iy,          &
                              grid%ts_t(n,i),                    &
                              grid%ts_q(n,i),                    &
                              grid%ts_u(n,i),                    &
                              grid%ts_v(n,i),                    &
                              grid%ts_psfc(n,i),                 &
                              grid%ts_tsk(n,i),                  &
                              grid%ts_tslb(n,i)
         END DO

         CLOSE(UNIT=iunit)

      END DO

   END IF

   grid%next_ts_time = 1

END SUBROUTINE write_ts


