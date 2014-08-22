


MODULE module_alloc_space_2
CONTAINS
   SUBROUTINE alloc_space_field_core_2 ( grid, id, setinitval_in , tl_in , inter_domain_in , num_bytes_allocated , &
                                  sd31, ed31, sd32, ed32, sd33, ed33, &
                                  sm31 , em31 , sm32 , em32 , sm33 , em33 , &
                                  sp31 , ep31 , sp32 , ep32 , sp33 , ep33 , &
                                  sp31x, ep31x, sp32x, ep32x, sp33x, ep33x, &
                                  sp31y, ep31y, sp32y, ep32y, sp33y, ep33y, &
                                  sm31x, em31x, sm32x, em32x, sm33x, em33x, &
                                  sm31y, em31y, sm32y, em32y, sm33y, em33y )
      USE module_domain_type
      USE module_configure, ONLY : model_config_rec, grid_config_rec_type, in_use_for_config, model_to_grid_config_rec
      USE module_scalar_tables
      IMPLICIT NONE
      TYPE(domain) , POINTER :: grid
      INTEGER , INTENT(IN) :: id
      INTEGER , INTENT(IN) :: setinitval_in
      INTEGER , INTENT(IN) :: sd31, ed31, sd32, ed32, sd33, ed33
      INTEGER , INTENT(IN) :: sm31, em31, sm32, em32, sm33, em33
      INTEGER , INTENT(IN) :: sp31, ep31, sp32, ep32, sp33, ep33
      INTEGER , INTENT(IN) :: sp31x, ep31x, sp32x, ep32x, sp33x, ep33x
      INTEGER , INTENT(IN) :: sp31y, ep31y, sp32y, ep32y, sp33y, ep33y
      INTEGER , INTENT(IN) :: sm31x, em31x, sm32x, em32x, sm33x, em33x
      INTEGER , INTENT(IN) :: sm31y, em31y, sm32y, em32y, sm33y, em33y
      INTEGER , INTENT(IN) :: tl_in
      LOGICAL , INTENT(IN) :: inter_domain_in
      INTEGER(KIND=8) , INTENT(INOUT) :: num_bytes_allocated
      INTEGER idum1, idum2, spec_bdy_width
      REAL initial_data_value
      CHARACTER (LEN=256) message
      INTEGER tl
      LOGICAL inter_domain
      INTEGER setinitval
      INTEGER sr_x, sr_y
      INTEGER ierr
      INTEGER :: loop
      TYPE ( grid_config_rec_type ) :: config_flags
      INTEGER :: k_start , k_end, its, ite, jts, jte
      INTEGER :: ids , ide , jds , jde , kds , kde , &
                                         ims , ime , jms , jme , kms , kme , &
                                         ips , ipe , jps , jpe , kps , kpe
      INTEGER :: sids , side , sjds , sjde , skds , skde , &
                                         sims , sime , sjms , sjme , skms , skme , &
                                         sips , sipe , sjps , sjpe , skps , skpe
      INTEGER :: imsx, imex, jmsx, jmex, kmsx, kmex, &
                              ipsx, ipex, jpsx, jpex, kpsx, kpex, &
                              imsy, imey, jmsy, jmey, kmsy, kmey, &
                              ipsy, ipey, jpsy, jpey, kpsy, kpey
      data_ordering : SELECT CASE ( model_data_order )
         CASE ( DATA_ORDER_XYZ )
             ids = sd31 ; ide = ed31 ; jds = sd32 ; jde = ed32 ; kds = sd33 ; kde = ed33 ;
             ims = sm31 ; ime = em31 ; jms = sm32 ; jme = em32 ; kms = sm33 ; kme = em33 ;
             ips = sp31 ; ipe = ep31 ; jps = sp32 ; jpe = ep32 ; kps = sp33 ; kpe = ep33 ;
             imsx = sm31x ; imex = em31x ; jmsx = sm32x ; jmex = em32x ; kmsx = sm33x ; kmex = em33x ;
             ipsx = sp31x ; ipex = ep31x ; jpsx = sp32x ; jpex = ep32x ; kpsx = sp33x ; kpex = ep33x ;
             imsy = sm31y ; imey = em31y ; jmsy = sm32y ; jmey = em32y ; kmsy = sm33y ; kmey = em33y ;
             ipsy = sp31y ; ipey = ep31y ; jpsy = sp32y ; jpey = ep32y ; kpsy = sp33y ; kpey = ep33y ;
         CASE ( DATA_ORDER_YXZ )
             ids = sd32 ; ide = ed32 ; jds = sd31 ; jde = ed31 ; kds = sd33 ; kde = ed33 ;
             ims = sm32 ; ime = em32 ; jms = sm31 ; jme = em31 ; kms = sm33 ; kme = em33 ;
             ips = sp32 ; ipe = ep32 ; jps = sp31 ; jpe = ep31 ; kps = sp33 ; kpe = ep33 ;
             imsx = sm32x ; imex = em32x ; jmsx = sm31x ; jmex = em31x ; kmsx = sm33x ; kmex = em33x ;
             ipsx = sp32x ; ipex = ep32x ; jpsx = sp31x ; jpex = ep31x ; kpsx = sp33x ; kpex = ep33x ;
             imsy = sm32y ; imey = em32y ; jmsy = sm31y ; jmey = em31y ; kmsy = sm33y ; kmey = em33y ;
             ipsy = sp32y ; ipey = ep32y ; jpsy = sp31y ; jpey = ep31y ; kpsy = sp33y ; kpey = ep33y ;
         CASE ( DATA_ORDER_ZXY )
             ids = sd32 ; ide = ed32 ; jds = sd33 ; jde = ed33 ; kds = sd31 ; kde = ed31 ;
             ims = sm32 ; ime = em32 ; jms = sm33 ; jme = em33 ; kms = sm31 ; kme = em31 ;
             ips = sp32 ; ipe = ep32 ; jps = sp33 ; jpe = ep33 ; kps = sp31 ; kpe = ep31 ;
             imsx = sm32x ; imex = em32x ; jmsx = sm33x ; jmex = em33x ; kmsx = sm31x ; kmex = em31x ;
             ipsx = sp32x ; ipex = ep32x ; jpsx = sp33x ; jpex = ep33x ; kpsx = sp31x ; kpex = ep31x ;
             imsy = sm32y ; imey = em32y ; jmsy = sm33y ; jmey = em33y ; kmsy = sm31y ; kmey = em31y ;
             ipsy = sp32y ; ipey = ep32y ; jpsy = sp33y ; jpey = ep33y ; kpsy = sp31y ; kpey = ep31y ;
         CASE ( DATA_ORDER_ZYX )
             ids = sd33 ; ide = ed33 ; jds = sd32 ; jde = ed32 ; kds = sd31 ; kde = ed31 ;
             ims = sm33 ; ime = em33 ; jms = sm32 ; jme = em32 ; kms = sm31 ; kme = em31 ;
             ips = sp33 ; ipe = ep33 ; jps = sp32 ; jpe = ep32 ; kps = sp31 ; kpe = ep31 ;
             imsx = sm33x ; imex = em33x ; jmsx = sm32x ; jmex = em32x ; kmsx = sm31x ; kmex = em31x ;
             ipsx = sp33x ; ipex = ep33x ; jpsx = sp32x ; jpex = ep32x ; kpsx = sp31x ; kpex = ep31x ;
             imsy = sm33y ; imey = em33y ; jmsy = sm32y ; jmey = em32y ; kmsy = sm31y ; kmey = em31y ;
             ipsy = sp33y ; ipey = ep33y ; jpsy = sp32y ; jpey = ep32y ; kpsy = sp31y ; kpey = ep31y ;
         CASE ( DATA_ORDER_XZY )
             ids = sd31 ; ide = ed31 ; jds = sd33 ; jde = ed33 ; kds = sd32 ; kde = ed32 ;
             ims = sm31 ; ime = em31 ; jms = sm33 ; jme = em33 ; kms = sm32 ; kme = em32 ;
             ips = sp31 ; ipe = ep31 ; jps = sp33 ; jpe = ep33 ; kps = sp32 ; kpe = ep32 ;
             imsx = sm31x ; imex = em31x ; jmsx = sm33x ; jmex = em33x ; kmsx = sm32x ; kmex = em32x ;
             ipsx = sp31x ; ipex = ep31x ; jpsx = sp33x ; jpex = ep33x ; kpsx = sp32x ; kpex = ep32x ;
             imsy = sm31y ; imey = em31y ; jmsy = sm33y ; jmey = em33y ; kmsy = sm32y ; kmey = em32y ;
             ipsy = sp31y ; ipey = ep31y ; jpsy = sp33y ; jpey = ep33y ; kpsy = sp32y ; kpey = ep32y ;
         CASE ( DATA_ORDER_YZX )
             ids = sd33 ; ide = ed33 ; jds = sd31 ; jde = ed31 ; kds = sd32 ; kde = ed32 ;
             ims = sm33 ; ime = em33 ; jms = sm31 ; jme = em31 ; kms = sm32 ; kme = em32 ;
             ips = sp33 ; ipe = ep33 ; jps = sp31 ; jpe = ep31 ; kps = sp32 ; kpe = ep32 ;
             imsx = sm33x ; imex = em33x ; jmsx = sm31x ; jmex = em31x ; kmsx = sm32x ; kmex = em32x ;
             ipsx = sp33x ; ipex = ep33x ; jpsx = sp31x ; jpex = ep31x ; kpsx = sp32x ; kpex = ep32x ;
             imsy = sm33y ; imey = em33y ; jmsy = sm31y ; jmey = em31y ; kmsy = sm32y ; kmey = em32y ;
             ipsy = sp33y ; ipey = ep33y ; jpsy = sp31y ; jpey = ep31y ; kpsy = sp32y ; kpey = ep32y ;
      END SELECT data_ordering
      CALL model_to_grid_config_rec ( id , model_config_rec , config_flags )
      CALL nl_get_sr_x( id , sr_x )
      CALL nl_get_sr_y( id , sr_y )
      tl = tl_in
      inter_domain = inter_domain_in
      CALL get_initial_data_value ( initial_data_value )
      setinitval = setinitval_in
      CALL nl_get_spec_bdy_width( 1, spec_bdy_width )
IF(in_use_for_config(id,'xlat'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%xlat(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",167,&
    'frame/module_domain.f: Failed to allocate grid%xlat(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%xlat=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'xlat'
  grid%tail_statevars%DataName = 'XLAT'
  grid%tail_statevars%Description = 'LATITUDE, SOUTH IS NEGATIVE'
  grid%tail_statevars%Units = 'degree_north'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%xlat
  grid%tail_statevars%streams(1) = 503316483
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%xlat(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",217,&
    'frame/module_domain.f: Failed to allocate grid%xlat(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'xlong'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%xlong(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",226,&
    'frame/module_domain.f: Failed to allocate grid%xlong(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%xlong=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'xlong'
  grid%tail_statevars%DataName = 'XLONG'
  grid%tail_statevars%Description = 'LONGITUDE, WEST IS NEGATIVE'
  grid%tail_statevars%Units = 'degree_east'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%xlong
  grid%tail_statevars%streams(1) = 503316483
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%xlong(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",276,&
    'frame/module_domain.f: Failed to allocate grid%xlong(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'xlat_u'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%xlat_u(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",285,&
    'frame/module_domain.f: Failed to allocate grid%xlat_u(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%xlat_u=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'xlat_u'
  grid%tail_statevars%DataName = 'XLAT_U'
  grid%tail_statevars%Description = 'LATITUDE, SOUTH IS NEGATIVE'
  grid%tail_statevars%Units = 'degree_north'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = 'X'
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%xlat_u
  grid%tail_statevars%streams(1) = 234881027
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = ide
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( ide, ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east_stag'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%xlat_u(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",335,&
    'frame/module_domain.f: Failed to allocate grid%xlat_u(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'xlong_u'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%xlong_u(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",344,&
    'frame/module_domain.f: Failed to allocate grid%xlong_u(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%xlong_u=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'xlong_u'
  grid%tail_statevars%DataName = 'XLONG_U'
  grid%tail_statevars%Description = 'LONGITUDE, WEST IS NEGATIVE'
  grid%tail_statevars%Units = 'degree_east'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = 'X'
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%xlong_u
  grid%tail_statevars%streams(1) = 234881027
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = ide
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( ide, ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east_stag'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%xlong_u(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",394,&
    'frame/module_domain.f: Failed to allocate grid%xlong_u(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'xlat_v'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%xlat_v(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",403,&
    'frame/module_domain.f: Failed to allocate grid%xlat_v(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%xlat_v=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'xlat_v'
  grid%tail_statevars%DataName = 'XLAT_V'
  grid%tail_statevars%Description = 'LATITUDE, SOUTH IS NEGATIVE'
  grid%tail_statevars%Units = 'degree_north'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = 'Y'
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%xlat_v
  grid%tail_statevars%streams(1) = 234881027
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = jde
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( jde, jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north_stag'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%xlat_v(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",453,&
    'frame/module_domain.f: Failed to allocate grid%xlat_v(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'xlong_v'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%xlong_v(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",462,&
    'frame/module_domain.f: Failed to allocate grid%xlong_v(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%xlong_v=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'xlong_v'
  grid%tail_statevars%DataName = 'XLONG_V'
  grid%tail_statevars%Description = 'LONGITUDE, WEST IS NEGATIVE'
  grid%tail_statevars%Units = 'degree_east'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = 'Y'
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%xlong_v
  grid%tail_statevars%streams(1) = 234881027
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = jde
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( jde, jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north_stag'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%xlong_v(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",512,&
    'frame/module_domain.f: Failed to allocate grid%xlong_v(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'albedo').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%albedo(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",521,&
    'frame/module_domain.f: Failed to allocate grid%albedo(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%albedo=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'albedo'
  grid%tail_statevars%DataName = 'ALBEDO'
  grid%tail_statevars%Description = 'ALBEDO'
  grid%tail_statevars%Units = '-'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%albedo
  grid%tail_statevars%streams(1) = 1
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%albedo(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",571,&
    'frame/module_domain.f: Failed to allocate grid%albedo(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'clat'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%clat(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",580,&
    'frame/module_domain.f: Failed to allocate grid%clat(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%clat=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'clat'
  grid%tail_statevars%DataName = 'CLAT'
  grid%tail_statevars%Description = 'COMPUTATIONAL GRID LATITUDE, SOUTH IS NEGATIVE'
  grid%tail_statevars%Units = 'degree_north'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%clat
  grid%tail_statevars%streams(1) = 234881025
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%clat(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",630,&
    'frame/module_domain.f: Failed to allocate grid%clat(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'albbck').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%albbck(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",639,&
    'frame/module_domain.f: Failed to allocate grid%albbck(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%albbck=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'albbck'
  grid%tail_statevars%DataName = 'ALBBCK'
  grid%tail_statevars%Description = 'BACKGROUND ALBEDO'
  grid%tail_statevars%Units = ''
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%albbck
  grid%tail_statevars%streams(1) = 771751937
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%albbck(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",689,&
    'frame/module_domain.f: Failed to allocate grid%albbck(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'embck').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%embck(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",698,&
    'frame/module_domain.f: Failed to allocate grid%embck(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%embck=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'embck'
  grid%tail_statevars%DataName = 'EMBCK'
  grid%tail_statevars%Description = 'BACKGROUND EMISSIVITY'
  grid%tail_statevars%Units = ''
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%embck
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%embck(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",748,&
    'frame/module_domain.f: Failed to allocate grid%embck(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'emiss').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%emiss(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",757,&
    'frame/module_domain.f: Failed to allocate grid%emiss(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%emiss=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'emiss'
  grid%tail_statevars%DataName = 'EMISS'
  grid%tail_statevars%Description = 'SURFACE EMISSIVITY'
  grid%tail_statevars%Units = ''
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%emiss
  grid%tail_statevars%streams(1) = 1
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%emiss(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",807,&
    'frame/module_domain.f: Failed to allocate grid%emiss(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'snotime').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%snotime(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",816,&
    'frame/module_domain.f: Failed to allocate grid%snotime(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%snotime=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'snotime'
  grid%tail_statevars%DataName = 'SNOTIME'
  grid%tail_statevars%Description = 'SNOTIME'
  grid%tail_statevars%Units = ''
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%snotime
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%snotime(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",866,&
    'frame/module_domain.f: Failed to allocate grid%snotime(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'noahres').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%noahres(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",875,&
    'frame/module_domain.f: Failed to allocate grid%noahres(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%noahres=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'noahres'
  grid%tail_statevars%DataName = 'NOAHRES'
  grid%tail_statevars%Description = 'RESIDUAL OF THE NOAH SURFACE ENERGY BUDGET'
  grid%tail_statevars%Units = 'W m{-2}'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .FALSE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%noahres
  grid%tail_statevars%streams(1) = 1
  grid%tail_statevars%streams(2) = 0
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%noahres(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",925,&
    'frame/module_domain.f: Failed to allocate grid%noahres(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'cldefi').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%cldefi(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",934,&
    'frame/module_domain.f: Failed to allocate grid%cldefi(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%cldefi=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'cldefi'
  grid%tail_statevars%DataName = 'CLDEFI'
  grid%tail_statevars%Description = 'precipitation efficiency in BMJ SCHEME'
  grid%tail_statevars%Units = ''
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%cldefi
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%cldefi(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",984,&
    'frame/module_domain.f: Failed to allocate grid%cldefi(1,1).  ')
  endif
ENDIF
  IF (.NOT.grid%is_intermediate) THEN
   ALLOCATE( grid%tail_statevars%next )
   grid%tail_statevars => grid%tail_statevars%next
   NULLIFY( grid%tail_statevars%next )
   grid%tail_statevars%ProcOrient = '  '
   grid%tail_statevars%VarName = 'stepra'
   grid%tail_statevars%DataName = 'STEPRA'
   grid%tail_statevars%Description = 'NUMBER OF FUNDAMENTAL TIMESTEPS BETWEEN RADIATION CALLS'
   grid%tail_statevars%Units = ''
   grid%tail_statevars%Type = 'i'
   grid%tail_statevars%Ntl = 0
   grid%tail_statevars%Restart = .TRUE.
   grid%tail_statevars%Ndim = 0
   grid%tail_statevars%scalar_array = .FALSE.
   grid%tail_statevars%ifield_0d => grid%stepra
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097152
  ENDIF
IF ( setinitval .EQ. 3 ) grid%stepra=0
IF(in_use_for_config(id,'rublten').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%rublten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",1012,&
    'frame/module_domain.f: Failed to allocate grid%rublten(sm31:em31,sm32:em32,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rublten=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'rublten'
  grid%tail_statevars%DataName = 'RUBLTEN'
  grid%tail_statevars%Description = 'COUPLED X WIND TENDENCY DUE TO PBL PARAMETERIZATION'
  grid%tail_statevars%Units = 'Pa m s-2'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XZY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 3
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_3d => grid%rublten
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = kds
  grid%tail_statevars%ed2 = (kde-1)
  grid%tail_statevars%sd3 = jds
  grid%tail_statevars%ed3 = (jde-1)
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = kms
  grid%tail_statevars%em2 = kme
  grid%tail_statevars%sm3 = jms
  grid%tail_statevars%em3 = jme
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = kps
  grid%tail_statevars%ep2 = MIN( (kde-1), kpe )
  grid%tail_statevars%sp3 = jps
  grid%tail_statevars%ep3 = MIN( (jde-1), jpe )
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'bottom_top'
  grid%tail_statevars%dimname3 = 'south_north'
  ENDIF
ELSE
  ALLOCATE(grid%rublten(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",1062,&
    'frame/module_domain.f: Failed to allocate grid%rublten(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'rvblten').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%rvblten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",1071,&
    'frame/module_domain.f: Failed to allocate grid%rvblten(sm31:em31,sm32:em32,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rvblten=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'rvblten'
  grid%tail_statevars%DataName = 'RVBLTEN'
  grid%tail_statevars%Description = 'COUPLED Y WIND TENDENCY DUE TO PBL PARAMETERIZATION'
  grid%tail_statevars%Units = 'Pa m s-2'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XZY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 3
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_3d => grid%rvblten
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = kds
  grid%tail_statevars%ed2 = (kde-1)
  grid%tail_statevars%sd3 = jds
  grid%tail_statevars%ed3 = (jde-1)
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = kms
  grid%tail_statevars%em2 = kme
  grid%tail_statevars%sm3 = jms
  grid%tail_statevars%em3 = jme
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = kps
  grid%tail_statevars%ep2 = MIN( (kde-1), kpe )
  grid%tail_statevars%sp3 = jps
  grid%tail_statevars%ep3 = MIN( (jde-1), jpe )
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'bottom_top'
  grid%tail_statevars%dimname3 = 'south_north'
  ENDIF
ELSE
  ALLOCATE(grid%rvblten(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",1121,&
    'frame/module_domain.f: Failed to allocate grid%rvblten(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'rthblten').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%rthblten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",1130,&
    'frame/module_domain.f: Failed to allocate grid%rthblten(sm31:em31,sm32:em32,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rthblten=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'rthblten'
  grid%tail_statevars%DataName = 'RTHBLTEN'
  grid%tail_statevars%Description = 'COUPLED THETA TENDENCY DUE TO PBL PARAMETERIZATION'
  grid%tail_statevars%Units = 'Pa K s-1'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XZY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 3
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_3d => grid%rthblten
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = kds
  grid%tail_statevars%ed2 = (kde-1)
  grid%tail_statevars%sd3 = jds
  grid%tail_statevars%ed3 = (jde-1)
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = kms
  grid%tail_statevars%em2 = kme
  grid%tail_statevars%sm3 = jms
  grid%tail_statevars%em3 = jme
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = kps
  grid%tail_statevars%ep2 = MIN( (kde-1), kpe )
  grid%tail_statevars%sp3 = jps
  grid%tail_statevars%ep3 = MIN( (jde-1), jpe )
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'bottom_top'
  grid%tail_statevars%dimname3 = 'south_north'
  ENDIF
ELSE
  ALLOCATE(grid%rthblten(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",1180,&
    'frame/module_domain.f: Failed to allocate grid%rthblten(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'rqvblten').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%rqvblten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",1189,&
    'frame/module_domain.f: Failed to allocate grid%rqvblten(sm31:em31,sm32:em32,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rqvblten=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'rqvblten'
  grid%tail_statevars%DataName = 'RQVBLTEN'
  grid%tail_statevars%Description = 'COUPLED Q_V TENDENCY DUE TO PBL PARAMETERIZATION'
  grid%tail_statevars%Units = 'Pa kg kg-1 s-1'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XZY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 3
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_3d => grid%rqvblten
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = kds
  grid%tail_statevars%ed2 = (kde-1)
  grid%tail_statevars%sd3 = jds
  grid%tail_statevars%ed3 = (jde-1)
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = kms
  grid%tail_statevars%em2 = kme
  grid%tail_statevars%sm3 = jms
  grid%tail_statevars%em3 = jme
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = kps
  grid%tail_statevars%ep2 = MIN( (kde-1), kpe )
  grid%tail_statevars%sp3 = jps
  grid%tail_statevars%ep3 = MIN( (jde-1), jpe )
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'bottom_top'
  grid%tail_statevars%dimname3 = 'south_north'
  ENDIF
ELSE
  ALLOCATE(grid%rqvblten(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",1239,&
    'frame/module_domain.f: Failed to allocate grid%rqvblten(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'rqcblten').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%rqcblten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",1248,&
    'frame/module_domain.f: Failed to allocate grid%rqcblten(sm31:em31,sm32:em32,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rqcblten=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'rqcblten'
  grid%tail_statevars%DataName = 'RQCBLTEN'
  grid%tail_statevars%Description = 'COUPLED Q_C TENDENCY DUE TO PBL PARAMETERIZATION'
  grid%tail_statevars%Units = 'Pa kg kg-1 s-1'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XZY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 3
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_3d => grid%rqcblten
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = kds
  grid%tail_statevars%ed2 = (kde-1)
  grid%tail_statevars%sd3 = jds
  grid%tail_statevars%ed3 = (jde-1)
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = kms
  grid%tail_statevars%em2 = kme
  grid%tail_statevars%sm3 = jms
  grid%tail_statevars%em3 = jme
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = kps
  grid%tail_statevars%ep2 = MIN( (kde-1), kpe )
  grid%tail_statevars%sp3 = jps
  grid%tail_statevars%ep3 = MIN( (jde-1), jpe )
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'bottom_top'
  grid%tail_statevars%dimname3 = 'south_north'
  ENDIF
ELSE
  ALLOCATE(grid%rqcblten(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",1298,&
    'frame/module_domain.f: Failed to allocate grid%rqcblten(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'rqiblten').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%rqiblten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",1307,&
    'frame/module_domain.f: Failed to allocate grid%rqiblten(sm31:em31,sm32:em32,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rqiblten=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'rqiblten'
  grid%tail_statevars%DataName = 'RQIBLTEN'
  grid%tail_statevars%Description = 'COUPLED Q_I TENDENCY DUE TO PBL PARAMETERIZATION'
  grid%tail_statevars%Units = 'Pa kg kg-1 s-1'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XZY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 3
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_3d => grid%rqiblten
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = kds
  grid%tail_statevars%ed2 = (kde-1)
  grid%tail_statevars%sd3 = jds
  grid%tail_statevars%ed3 = (jde-1)
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = kms
  grid%tail_statevars%em2 = kme
  grid%tail_statevars%sm3 = jms
  grid%tail_statevars%em3 = jme
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = kps
  grid%tail_statevars%ep2 = MIN( (kde-1), kpe )
  grid%tail_statevars%sp3 = jps
  grid%tail_statevars%ep3 = MIN( (jde-1), jpe )
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'bottom_top'
  grid%tail_statevars%dimname3 = 'south_north'
  ENDIF
ELSE
  ALLOCATE(grid%rqiblten(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",1357,&
    'frame/module_domain.f: Failed to allocate grid%rqiblten(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'rqniblten').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%rqniblten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",1366,&
    'frame/module_domain.f: Failed to allocate grid%rqniblten(sm31:em31,sm32:em32,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rqniblten=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'rqniblten'
  grid%tail_statevars%DataName = 'RQNIBLTEN'
  grid%tail_statevars%Description = 'COUPLED Q_NI TENDENCY DUE TO PBL PARAMETERIZATION'
  grid%tail_statevars%Units = 'Pa    kg-1 s-1'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XZY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 3
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_3d => grid%rqniblten
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = kds
  grid%tail_statevars%ed2 = (kde-1)
  grid%tail_statevars%sd3 = jds
  grid%tail_statevars%ed3 = (jde-1)
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = kms
  grid%tail_statevars%em2 = kme
  grid%tail_statevars%sm3 = jms
  grid%tail_statevars%em3 = jme
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = kps
  grid%tail_statevars%ep2 = MIN( (kde-1), kpe )
  grid%tail_statevars%sp3 = jps
  grid%tail_statevars%ep3 = MIN( (jde-1), jpe )
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'bottom_top'
  grid%tail_statevars%dimname3 = 'south_north'
  ENDIF
ELSE
  ALLOCATE(grid%rqniblten(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",1416,&
    'frame/module_domain.f: Failed to allocate grid%rqniblten(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'flx4').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%flx4(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",1425,&
    'frame/module_domain.f: Failed to allocate grid%flx4(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%flx4=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'flx4'
  grid%tail_statevars%DataName = 'FLX4'
  grid%tail_statevars%Description = 'sensible heat from canopy'
  grid%tail_statevars%Units = 'W m{-2}'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .FALSE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%flx4
  grid%tail_statevars%streams(1) = 1
  grid%tail_statevars%streams(2) = 0
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%flx4(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",1475,&
    'frame/module_domain.f: Failed to allocate grid%flx4(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'fvb').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%fvb(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",1484,&
    'frame/module_domain.f: Failed to allocate grid%fvb(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%fvb=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'fvb'
  grid%tail_statevars%DataName = 'FVB'
  grid%tail_statevars%Description = 'fraction of vegetation with snow below'
  grid%tail_statevars%Units = ''
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .FALSE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%fvb
  grid%tail_statevars%streams(1) = 1
  grid%tail_statevars%streams(2) = 0
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%fvb(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",1534,&
    'frame/module_domain.f: Failed to allocate grid%fvb(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'fbur').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%fbur(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",1543,&
    'frame/module_domain.f: Failed to allocate grid%fbur(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%fbur=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'fbur'
  grid%tail_statevars%DataName = 'FBUR'
  grid%tail_statevars%Description = 'fraction of vegetation covered by snow'
  grid%tail_statevars%Units = ''
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .FALSE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%fbur
  grid%tail_statevars%streams(1) = 1
  grid%tail_statevars%streams(2) = 0
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%fbur(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",1593,&
    'frame/module_domain.f: Failed to allocate grid%fbur(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'fgsn').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%fgsn(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",1602,&
    'frame/module_domain.f: Failed to allocate grid%fgsn(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%fgsn=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'fgsn'
  grid%tail_statevars%DataName = 'FGSN'
  grid%tail_statevars%Description = 'fraction of ground covered by snow'
  grid%tail_statevars%Units = ''
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .FALSE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%fgsn
  grid%tail_statevars%streams(1) = 1
  grid%tail_statevars%streams(2) = 0
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%fgsn(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",1652,&
    'frame/module_domain.f: Failed to allocate grid%fgsn(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'isnowxy'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%isnowxy(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",1661,&
    'frame/module_domain.f: Failed to allocate grid%isnowxy(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%isnowxy=0
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'isnowxy'
  grid%tail_statevars%DataName = 'ISNOW'
  grid%tail_statevars%Description = 'no. of snow layer'
  grid%tail_statevars%Units = 'm3 m-3'
  grid%tail_statevars%Type = 'i'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%ifield_2d => grid%isnowxy
  grid%tail_statevars%streams(1) = 167772161
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%isnowxy(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",1711,&
    'frame/module_domain.f: Failed to allocate grid%isnowxy(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'tvxy'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%tvxy(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",1720,&
    'frame/module_domain.f: Failed to allocate grid%tvxy(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tvxy=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'tvxy'
  grid%tail_statevars%DataName = 'TV'
  grid%tail_statevars%Description = 'vegetation leaf temperature'
  grid%tail_statevars%Units = 'K'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%tvxy
  grid%tail_statevars%streams(1) = 167772161
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%tvxy(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",1770,&
    'frame/module_domain.f: Failed to allocate grid%tvxy(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'tgxy'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%tgxy(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",1779,&
    'frame/module_domain.f: Failed to allocate grid%tgxy(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tgxy=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'tgxy'
  grid%tail_statevars%DataName = 'TG'
  grid%tail_statevars%Description = 'bulk ground temperature'
  grid%tail_statevars%Units = 'K'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%tgxy
  grid%tail_statevars%streams(1) = 167772161
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%tgxy(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",1829,&
    'frame/module_domain.f: Failed to allocate grid%tgxy(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'canicexy'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%canicexy(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",1838,&
    'frame/module_domain.f: Failed to allocate grid%canicexy(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%canicexy=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'canicexy'
  grid%tail_statevars%DataName = 'CANICE'
  grid%tail_statevars%Description = 'intercepted ice mass'
  grid%tail_statevars%Units = 'mm'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%canicexy
  grid%tail_statevars%streams(1) = 167772161
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%canicexy(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",1888,&
    'frame/module_domain.f: Failed to allocate grid%canicexy(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'canliqxy'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%canliqxy(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",1897,&
    'frame/module_domain.f: Failed to allocate grid%canliqxy(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%canliqxy=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'canliqxy'
  grid%tail_statevars%DataName = 'CANLIQ'
  grid%tail_statevars%Description = 'intercepted liquid water'
  grid%tail_statevars%Units = 'mm'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%canliqxy
  grid%tail_statevars%streams(1) = 167772161
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%canliqxy(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",1947,&
    'frame/module_domain.f: Failed to allocate grid%canliqxy(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'eahxy'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%eahxy(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",1956,&
    'frame/module_domain.f: Failed to allocate grid%eahxy(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%eahxy=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'eahxy'
  grid%tail_statevars%DataName = 'EAH'
  grid%tail_statevars%Description = 'canopy air vapor pressure'
  grid%tail_statevars%Units = 'pa'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%eahxy
  grid%tail_statevars%streams(1) = 167772161
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%eahxy(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",2006,&
    'frame/module_domain.f: Failed to allocate grid%eahxy(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'tahxy'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%tahxy(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",2015,&
    'frame/module_domain.f: Failed to allocate grid%tahxy(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tahxy=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'tahxy'
  grid%tail_statevars%DataName = 'TAH'
  grid%tail_statevars%Description = 'canopy air temperature'
  grid%tail_statevars%Units = 'K'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%tahxy
  grid%tail_statevars%streams(1) = 167772161
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%tahxy(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",2065,&
    'frame/module_domain.f: Failed to allocate grid%tahxy(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'cmxy'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%cmxy(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",2074,&
    'frame/module_domain.f: Failed to allocate grid%cmxy(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%cmxy=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'cmxy'
  grid%tail_statevars%DataName = 'CM'
  grid%tail_statevars%Description = 'surf. exchange coeff. for momentum'
  grid%tail_statevars%Units = 'm/s'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%cmxy
  grid%tail_statevars%streams(1) = 167772161
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%cmxy(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",2124,&
    'frame/module_domain.f: Failed to allocate grid%cmxy(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'chxy'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%chxy(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",2133,&
    'frame/module_domain.f: Failed to allocate grid%chxy(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%chxy=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'chxy'
  grid%tail_statevars%DataName = 'CH'
  grid%tail_statevars%Description = 'surf. exchange coeff. for heat'
  grid%tail_statevars%Units = 'm/s'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%chxy
  grid%tail_statevars%streams(1) = 167772161
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%chxy(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",2183,&
    'frame/module_domain.f: Failed to allocate grid%chxy(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'fwetxy'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%fwetxy(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",2192,&
    'frame/module_domain.f: Failed to allocate grid%fwetxy(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%fwetxy=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'fwetxy'
  grid%tail_statevars%DataName = 'FWET'
  grid%tail_statevars%Description = 'wetted or snowed canopy fraction'
  grid%tail_statevars%Units = '-'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%fwetxy
  grid%tail_statevars%streams(1) = 167772161
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%fwetxy(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",2242,&
    'frame/module_domain.f: Failed to allocate grid%fwetxy(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'sneqvoxy'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%sneqvoxy(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",2251,&
    'frame/module_domain.f: Failed to allocate grid%sneqvoxy(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sneqvoxy=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'sneqvoxy'
  grid%tail_statevars%DataName = 'SNEQVO'
  grid%tail_statevars%Description = 'snow mass at last time step'
  grid%tail_statevars%Units = 'mm'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%sneqvoxy
  grid%tail_statevars%streams(1) = 167772161
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%sneqvoxy(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",2301,&
    'frame/module_domain.f: Failed to allocate grid%sneqvoxy(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'alboldxy'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%alboldxy(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",2310,&
    'frame/module_domain.f: Failed to allocate grid%alboldxy(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%alboldxy=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'alboldxy'
  grid%tail_statevars%DataName = 'ALBOLD'
  grid%tail_statevars%Description = 'snow albedo at last timestep'
  grid%tail_statevars%Units = '-'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%alboldxy
  grid%tail_statevars%streams(1) = 167772161
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%alboldxy(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",2360,&
    'frame/module_domain.f: Failed to allocate grid%alboldxy(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'qsnowxy'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%qsnowxy(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",2369,&
    'frame/module_domain.f: Failed to allocate grid%qsnowxy(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%qsnowxy=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'qsnowxy'
  grid%tail_statevars%DataName = 'QSNOWXY'
  grid%tail_statevars%Description = 'snowfall on the ground'
  grid%tail_statevars%Units = 'mm/s'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%qsnowxy
  grid%tail_statevars%streams(1) = 167772161
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%qsnowxy(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",2419,&
    'frame/module_domain.f: Failed to allocate grid%qsnowxy(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'wslakexy'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%wslakexy(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",2428,&
    'frame/module_domain.f: Failed to allocate grid%wslakexy(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%wslakexy=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'wslakexy'
  grid%tail_statevars%DataName = 'WSLAKE'
  grid%tail_statevars%Description = 'lake water storage'
  grid%tail_statevars%Units = 'mm'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%wslakexy
  grid%tail_statevars%streams(1) = 167772161
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%wslakexy(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",2478,&
    'frame/module_domain.f: Failed to allocate grid%wslakexy(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'zwtxy'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%zwtxy(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",2487,&
    'frame/module_domain.f: Failed to allocate grid%zwtxy(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%zwtxy=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'zwtxy'
  grid%tail_statevars%DataName = 'ZWT'
  grid%tail_statevars%Description = 'water table depth'
  grid%tail_statevars%Units = 'm'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%zwtxy
  grid%tail_statevars%streams(1) = 167772161
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%zwtxy(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",2537,&
    'frame/module_domain.f: Failed to allocate grid%zwtxy(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'waxy'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%waxy(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",2546,&
    'frame/module_domain.f: Failed to allocate grid%waxy(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%waxy=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'waxy'
  grid%tail_statevars%DataName = 'WA'
  grid%tail_statevars%Description = 'water in the acquifer'
  grid%tail_statevars%Units = 'mm'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%waxy
  grid%tail_statevars%streams(1) = 167772161
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%waxy(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",2596,&
    'frame/module_domain.f: Failed to allocate grid%waxy(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'wtxy'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%wtxy(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",2605,&
    'frame/module_domain.f: Failed to allocate grid%wtxy(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%wtxy=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'wtxy'
  grid%tail_statevars%DataName = 'WT'
  grid%tail_statevars%Description = 'groundwater storage'
  grid%tail_statevars%Units = 'mm'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%wtxy
  grid%tail_statevars%streams(1) = 167772161
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%wtxy(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",2655,&
    'frame/module_domain.f: Failed to allocate grid%wtxy(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'tsnoxy'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((model_config_rec%num_snow_layers)-(1)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%tsnoxy(sm31:em31,1:model_config_rec%num_snow_layers,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",2664,&
    'frame/module_domain.f: Failed to allocate grid%tsnoxy(sm31:em31,1:model_config_rec%num_snow_layers,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tsnoxy=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'tsnoxy'
  grid%tail_statevars%DataName = 'TSNO'
  grid%tail_statevars%Description = 'snow temperature'
  grid%tail_statevars%Units = 'K'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XZY'
  grid%tail_statevars%Stagger = 'Z'
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 3
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_3d => grid%tsnoxy
  grid%tail_statevars%streams(1) = 167772161
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = 1
  grid%tail_statevars%ed2 = config_flags%num_snow_layers
  grid%tail_statevars%sd3 = jds
  grid%tail_statevars%ed3 = (jde-1)
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = 1
  grid%tail_statevars%em2 = config_flags%num_snow_layers
  grid%tail_statevars%sm3 = jms
  grid%tail_statevars%em3 = jme
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = 1
  grid%tail_statevars%ep2 = config_flags%num_snow_layers
  grid%tail_statevars%sp3 = jps
  grid%tail_statevars%ep3 = MIN( (jde-1), jpe )
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'snow_layers_stag'
  grid%tail_statevars%dimname3 = 'south_north'
  ENDIF
ELSE
  ALLOCATE(grid%tsnoxy(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",2714,&
    'frame/module_domain.f: Failed to allocate grid%tsnoxy(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'zsnsoxy'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((model_config_rec%num_snso_layers)-(1)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%zsnsoxy(sm31:em31,1:model_config_rec%num_snso_layers,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",2723,&
    'frame/module_domain.f: Failed to allocate grid%zsnsoxy(sm31:em31,1:model_config_rec%num_snso_layers,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%zsnsoxy=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'zsnsoxy'
  grid%tail_statevars%DataName = 'ZSNSO'
  grid%tail_statevars%Description = 'layer-bottom depth from snow surf'
  grid%tail_statevars%Units = 'm'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XZY'
  grid%tail_statevars%Stagger = 'Z'
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 3
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_3d => grid%zsnsoxy
  grid%tail_statevars%streams(1) = 167772161
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = 1
  grid%tail_statevars%ed2 = config_flags%num_snso_layers
  grid%tail_statevars%sd3 = jds
  grid%tail_statevars%ed3 = (jde-1)
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = 1
  grid%tail_statevars%em2 = config_flags%num_snso_layers
  grid%tail_statevars%sm3 = jms
  grid%tail_statevars%em3 = jme
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = 1
  grid%tail_statevars%ep2 = config_flags%num_snso_layers
  grid%tail_statevars%sp3 = jps
  grid%tail_statevars%ep3 = MIN( (jde-1), jpe )
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'snso_layers_stag'
  grid%tail_statevars%dimname3 = 'south_north'
  ENDIF
ELSE
  ALLOCATE(grid%zsnsoxy(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",2773,&
    'frame/module_domain.f: Failed to allocate grid%zsnsoxy(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'snicexy'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((model_config_rec%num_snow_layers)-(1)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%snicexy(sm31:em31,1:model_config_rec%num_snow_layers,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",2782,&
    'frame/module_domain.f: Failed to allocate grid%snicexy(sm31:em31,1:model_config_rec%num_snow_layers,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%snicexy=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'snicexy'
  grid%tail_statevars%DataName = 'SNICE'
  grid%tail_statevars%Description = 'snow layer ice'
  grid%tail_statevars%Units = 'mm'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XZY'
  grid%tail_statevars%Stagger = 'Z'
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 3
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_3d => grid%snicexy
  grid%tail_statevars%streams(1) = 167772161
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = 1
  grid%tail_statevars%ed2 = config_flags%num_snow_layers
  grid%tail_statevars%sd3 = jds
  grid%tail_statevars%ed3 = (jde-1)
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = 1
  grid%tail_statevars%em2 = config_flags%num_snow_layers
  grid%tail_statevars%sm3 = jms
  grid%tail_statevars%em3 = jme
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = 1
  grid%tail_statevars%ep2 = config_flags%num_snow_layers
  grid%tail_statevars%sp3 = jps
  grid%tail_statevars%ep3 = MIN( (jde-1), jpe )
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'snow_layers_stag'
  grid%tail_statevars%dimname3 = 'south_north'
  ENDIF
ELSE
  ALLOCATE(grid%snicexy(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",2832,&
    'frame/module_domain.f: Failed to allocate grid%snicexy(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'snliqxy'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((model_config_rec%num_snow_layers)-(1)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%snliqxy(sm31:em31,1:model_config_rec%num_snow_layers,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",2841,&
    'frame/module_domain.f: Failed to allocate grid%snliqxy(sm31:em31,1:model_config_rec%num_snow_layers,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%snliqxy=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'snliqxy'
  grid%tail_statevars%DataName = 'SNLIQ'
  grid%tail_statevars%Description = 'snow layer liquid'
  grid%tail_statevars%Units = 'mm'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XZY'
  grid%tail_statevars%Stagger = 'Z'
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 3
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_3d => grid%snliqxy
  grid%tail_statevars%streams(1) = 167772161
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = 1
  grid%tail_statevars%ed2 = config_flags%num_snow_layers
  grid%tail_statevars%sd3 = jds
  grid%tail_statevars%ed3 = (jde-1)
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = 1
  grid%tail_statevars%em2 = config_flags%num_snow_layers
  grid%tail_statevars%sm3 = jms
  grid%tail_statevars%em3 = jme
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = 1
  grid%tail_statevars%ep2 = config_flags%num_snow_layers
  grid%tail_statevars%sp3 = jps
  grid%tail_statevars%ep3 = MIN( (jde-1), jpe )
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'snow_layers_stag'
  grid%tail_statevars%dimname3 = 'south_north'
  ENDIF
ELSE
  ALLOCATE(grid%snliqxy(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",2891,&
    'frame/module_domain.f: Failed to allocate grid%snliqxy(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'lfmassxy'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%lfmassxy(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",2900,&
    'frame/module_domain.f: Failed to allocate grid%lfmassxy(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%lfmassxy=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'lfmassxy'
  grid%tail_statevars%DataName = 'LFMASS'
  grid%tail_statevars%Description = 'leaf mass'
  grid%tail_statevars%Units = 'g/m2'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%lfmassxy
  grid%tail_statevars%streams(1) = 167772161
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%lfmassxy(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",2950,&
    'frame/module_domain.f: Failed to allocate grid%lfmassxy(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'rtmassxy'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%rtmassxy(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",2959,&
    'frame/module_domain.f: Failed to allocate grid%rtmassxy(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rtmassxy=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'rtmassxy'
  grid%tail_statevars%DataName = 'RTMASS'
  grid%tail_statevars%Description = 'mass of fine roots'
  grid%tail_statevars%Units = 'g/m2'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%rtmassxy
  grid%tail_statevars%streams(1) = 167772161
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%rtmassxy(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",3009,&
    'frame/module_domain.f: Failed to allocate grid%rtmassxy(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'stmassxy'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%stmassxy(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",3018,&
    'frame/module_domain.f: Failed to allocate grid%stmassxy(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%stmassxy=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'stmassxy'
  grid%tail_statevars%DataName = 'STMASS'
  grid%tail_statevars%Description = 'stem mass'
  grid%tail_statevars%Units = 'g/m2'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%stmassxy
  grid%tail_statevars%streams(1) = 167772161
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%stmassxy(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",3068,&
    'frame/module_domain.f: Failed to allocate grid%stmassxy(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'woodxy'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%woodxy(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",3077,&
    'frame/module_domain.f: Failed to allocate grid%woodxy(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%woodxy=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'woodxy'
  grid%tail_statevars%DataName = 'WOOD'
  grid%tail_statevars%Description = 'mass of wood'
  grid%tail_statevars%Units = 'g/m2'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%woodxy
  grid%tail_statevars%streams(1) = 167772161
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%woodxy(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",3127,&
    'frame/module_domain.f: Failed to allocate grid%woodxy(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'stblcpxy'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%stblcpxy(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",3136,&
    'frame/module_domain.f: Failed to allocate grid%stblcpxy(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%stblcpxy=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'stblcpxy'
  grid%tail_statevars%DataName = 'STBLCP'
  grid%tail_statevars%Description = 'stable carbon pool'
  grid%tail_statevars%Units = 'g/m2'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%stblcpxy
  grid%tail_statevars%streams(1) = 167772161
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%stblcpxy(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",3186,&
    'frame/module_domain.f: Failed to allocate grid%stblcpxy(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'fastcpxy'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%fastcpxy(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",3195,&
    'frame/module_domain.f: Failed to allocate grid%fastcpxy(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%fastcpxy=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'fastcpxy'
  grid%tail_statevars%DataName = 'FASTCP'
  grid%tail_statevars%Description = 'short-lived carbon'
  grid%tail_statevars%Units = 'g/m2'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%fastcpxy
  grid%tail_statevars%streams(1) = 167772161
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%fastcpxy(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",3245,&
    'frame/module_domain.f: Failed to allocate grid%fastcpxy(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'xsaixy'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%xsaixy(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",3254,&
    'frame/module_domain.f: Failed to allocate grid%xsaixy(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%xsaixy=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'xsaixy'
  grid%tail_statevars%DataName = 'XSAI'
  grid%tail_statevars%Description = 'stem area index'
  grid%tail_statevars%Units = '-'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%xsaixy
  grid%tail_statevars%streams(1) = 167772161
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%xsaixy(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",3304,&
    'frame/module_domain.f: Failed to allocate grid%xsaixy(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'taussxy').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%taussxy(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",3313,&
    'frame/module_domain.f: Failed to allocate grid%taussxy(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%taussxy=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'taussxy'
  grid%tail_statevars%DataName = 'TAUSS'
  grid%tail_statevars%Description = 'non-dimensional snow age'
  grid%tail_statevars%Units = ''
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%taussxy
  grid%tail_statevars%streams(1) = 1
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%taussxy(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",3363,&
    'frame/module_domain.f: Failed to allocate grid%taussxy(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'t2mvxy'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%t2mvxy(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",3372,&
    'frame/module_domain.f: Failed to allocate grid%t2mvxy(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%t2mvxy=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 't2mvxy'
  grid%tail_statevars%DataName = 'T2V'
  grid%tail_statevars%Description = '2 meter temperature over canopy'
  grid%tail_statevars%Units = 'K'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%t2mvxy
  grid%tail_statevars%streams(1) = 167772161
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%t2mvxy(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",3422,&
    'frame/module_domain.f: Failed to allocate grid%t2mvxy(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'t2mbxy'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%t2mbxy(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",3431,&
    'frame/module_domain.f: Failed to allocate grid%t2mbxy(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%t2mbxy=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 't2mbxy'
  grid%tail_statevars%DataName = 'T2B'
  grid%tail_statevars%Description = '2 meter temperature over bare ground'
  grid%tail_statevars%Units = 'K'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%t2mbxy
  grid%tail_statevars%streams(1) = 167772161
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%t2mbxy(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",3481,&
    'frame/module_domain.f: Failed to allocate grid%t2mbxy(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'q2mvxy'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%q2mvxy(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",3490,&
    'frame/module_domain.f: Failed to allocate grid%q2mvxy(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%q2mvxy=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'q2mvxy'
  grid%tail_statevars%DataName = 'Q2V'
  grid%tail_statevars%Description = '2 meter mixing ratio over canopy'
  grid%tail_statevars%Units = 'kg kg-1'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%q2mvxy
  grid%tail_statevars%streams(1) = 167772161
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%q2mvxy(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",3540,&
    'frame/module_domain.f: Failed to allocate grid%q2mvxy(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'q2mbxy'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%q2mbxy(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",3549,&
    'frame/module_domain.f: Failed to allocate grid%q2mbxy(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%q2mbxy=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'q2mbxy'
  grid%tail_statevars%DataName = 'Q2B'
  grid%tail_statevars%Description = '2 meter mixing ratio over bare ground'
  grid%tail_statevars%Units = 'kg kg-1'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%q2mbxy
  grid%tail_statevars%streams(1) = 167772161
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%q2mbxy(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",3599,&
    'frame/module_domain.f: Failed to allocate grid%q2mbxy(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'tradxy'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%tradxy(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",3608,&
    'frame/module_domain.f: Failed to allocate grid%tradxy(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tradxy=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'tradxy'
  grid%tail_statevars%DataName = 'TRAD'
  grid%tail_statevars%Description = 'surface radiative temperature'
  grid%tail_statevars%Units = 'K'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%tradxy
  grid%tail_statevars%streams(1) = 167772161
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%tradxy(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",3658,&
    'frame/module_domain.f: Failed to allocate grid%tradxy(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'neexy'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%neexy(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",3667,&
    'frame/module_domain.f: Failed to allocate grid%neexy(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%neexy=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'neexy'
  grid%tail_statevars%DataName = 'NEE'
  grid%tail_statevars%Description = 'net ecosystem exchange'
  grid%tail_statevars%Units = 'g/m2/s CO2'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%neexy
  grid%tail_statevars%streams(1) = 167772161
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%neexy(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",3717,&
    'frame/module_domain.f: Failed to allocate grid%neexy(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'gppxy'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%gppxy(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",3726,&
    'frame/module_domain.f: Failed to allocate grid%gppxy(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%gppxy=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'gppxy'
  grid%tail_statevars%DataName = 'GPP'
  grid%tail_statevars%Description = 'gross primary productivity'
  grid%tail_statevars%Units = 'g/m2/s C'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%gppxy
  grid%tail_statevars%streams(1) = 167772161
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%gppxy(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",3776,&
    'frame/module_domain.f: Failed to allocate grid%gppxy(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'nppxy'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%nppxy(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",3785,&
    'frame/module_domain.f: Failed to allocate grid%nppxy(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%nppxy=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'nppxy'
  grid%tail_statevars%DataName = 'NPP'
  grid%tail_statevars%Description = 'net primary productivity'
  grid%tail_statevars%Units = 'g/m2/s C'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%nppxy
  grid%tail_statevars%streams(1) = 167772161
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%nppxy(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",3835,&
    'frame/module_domain.f: Failed to allocate grid%nppxy(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'fvegxy'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%fvegxy(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",3844,&
    'frame/module_domain.f: Failed to allocate grid%fvegxy(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%fvegxy=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'fvegxy'
  grid%tail_statevars%DataName = 'FVEG'
  grid%tail_statevars%Description = 'Noah-MP vegetation fraction'
  grid%tail_statevars%Units = ''
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%fvegxy
  grid%tail_statevars%streams(1) = 167772161
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%fvegxy(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",3894,&
    'frame/module_domain.f: Failed to allocate grid%fvegxy(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'qinxy'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%qinxy(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",3903,&
    'frame/module_domain.f: Failed to allocate grid%qinxy(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%qinxy=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'qinxy'
  grid%tail_statevars%DataName = 'QIN'
  grid%tail_statevars%Description = 'groundwater recharge'
  grid%tail_statevars%Units = 'mm/s'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%qinxy
  grid%tail_statevars%streams(1) = 167772161
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%qinxy(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",3953,&
    'frame/module_domain.f: Failed to allocate grid%qinxy(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'runsfxy'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%runsfxy(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",3962,&
    'frame/module_domain.f: Failed to allocate grid%runsfxy(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%runsfxy=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'runsfxy'
  grid%tail_statevars%DataName = 'RUNSF'
  grid%tail_statevars%Description = 'surface runoff'
  grid%tail_statevars%Units = 'mm/s'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%runsfxy
  grid%tail_statevars%streams(1) = 167772161
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%runsfxy(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",4012,&
    'frame/module_domain.f: Failed to allocate grid%runsfxy(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'runsbxy'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%runsbxy(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",4021,&
    'frame/module_domain.f: Failed to allocate grid%runsbxy(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%runsbxy=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'runsbxy'
  grid%tail_statevars%DataName = 'RUNSB'
  grid%tail_statevars%Description = 'subsurface runoff'
  grid%tail_statevars%Units = 'mm/s'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%runsbxy
  grid%tail_statevars%streams(1) = 167772161
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%runsbxy(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",4071,&
    'frame/module_domain.f: Failed to allocate grid%runsbxy(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'ecanxy'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%ecanxy(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",4080,&
    'frame/module_domain.f: Failed to allocate grid%ecanxy(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ecanxy=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'ecanxy'
  grid%tail_statevars%DataName = 'ECAN'
  grid%tail_statevars%Description = 'evaporation of intercepted water'
  grid%tail_statevars%Units = 'mm/s'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%ecanxy
  grid%tail_statevars%streams(1) = 167772161
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%ecanxy(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",4130,&
    'frame/module_domain.f: Failed to allocate grid%ecanxy(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'edirxy'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%edirxy(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",4139,&
    'frame/module_domain.f: Failed to allocate grid%edirxy(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%edirxy=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'edirxy'
  grid%tail_statevars%DataName = 'EDIR'
  grid%tail_statevars%Description = 'ground surface evaporation rate'
  grid%tail_statevars%Units = 'mm/s'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%edirxy
  grid%tail_statevars%streams(1) = 167772161
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%edirxy(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",4189,&
    'frame/module_domain.f: Failed to allocate grid%edirxy(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'etranxy'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%etranxy(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",4198,&
    'frame/module_domain.f: Failed to allocate grid%etranxy(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%etranxy=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'etranxy'
  grid%tail_statevars%DataName = 'ETRAN'
  grid%tail_statevars%Description = 'transpiration rate'
  grid%tail_statevars%Units = 'mm/s'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%etranxy
  grid%tail_statevars%streams(1) = 167772161
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%etranxy(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",4248,&
    'frame/module_domain.f: Failed to allocate grid%etranxy(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'fsaxy'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%fsaxy(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",4257,&
    'frame/module_domain.f: Failed to allocate grid%fsaxy(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%fsaxy=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'fsaxy'
  grid%tail_statevars%DataName = 'FSA'
  grid%tail_statevars%Description = 'total absorbed solar radiation'
  grid%tail_statevars%Units = 'W/m2'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%fsaxy
  grid%tail_statevars%streams(1) = 167772161
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%fsaxy(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",4307,&
    'frame/module_domain.f: Failed to allocate grid%fsaxy(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'firaxy'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%firaxy(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",4316,&
    'frame/module_domain.f: Failed to allocate grid%firaxy(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%firaxy=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'firaxy'
  grid%tail_statevars%DataName = 'FIRA'
  grid%tail_statevars%Description = 'total net longwave rad'
  grid%tail_statevars%Units = 'W/m2'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%firaxy
  grid%tail_statevars%streams(1) = 167772161
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%firaxy(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",4366,&
    'frame/module_domain.f: Failed to allocate grid%firaxy(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'aparxy'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%aparxy(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",4375,&
    'frame/module_domain.f: Failed to allocate grid%aparxy(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%aparxy=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'aparxy'
  grid%tail_statevars%DataName = 'APAR'
  grid%tail_statevars%Description = 'photosyn active energy by canopy'
  grid%tail_statevars%Units = 'W/m2'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%aparxy
  grid%tail_statevars%streams(1) = 167772161
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%aparxy(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",4425,&
    'frame/module_domain.f: Failed to allocate grid%aparxy(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'psnxy'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%psnxy(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",4434,&
    'frame/module_domain.f: Failed to allocate grid%psnxy(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%psnxy=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'psnxy'
  grid%tail_statevars%DataName = 'PSN'
  grid%tail_statevars%Description = 'total photosynthesis'
  grid%tail_statevars%Units = 'umol co2/m2/s'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%psnxy
  grid%tail_statevars%streams(1) = 167772161
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%psnxy(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",4484,&
    'frame/module_domain.f: Failed to allocate grid%psnxy(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'savxy'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%savxy(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",4493,&
    'frame/module_domain.f: Failed to allocate grid%savxy(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%savxy=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'savxy'
  grid%tail_statevars%DataName = 'SAV'
  grid%tail_statevars%Description = 'solar rad absorbed by veg'
  grid%tail_statevars%Units = 'W/m2'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%savxy
  grid%tail_statevars%streams(1) = 167772161
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%savxy(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",4543,&
    'frame/module_domain.f: Failed to allocate grid%savxy(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'sagxy'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%sagxy(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",4552,&
    'frame/module_domain.f: Failed to allocate grid%sagxy(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sagxy=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'sagxy'
  grid%tail_statevars%DataName = 'SAG'
  grid%tail_statevars%Description = 'solar rad absorbed by ground'
  grid%tail_statevars%Units = 'W/m2'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%sagxy
  grid%tail_statevars%streams(1) = 167772161
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%sagxy(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",4602,&
    'frame/module_domain.f: Failed to allocate grid%sagxy(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'rssunxy'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%rssunxy(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",4611,&
    'frame/module_domain.f: Failed to allocate grid%rssunxy(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rssunxy=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'rssunxy'
  grid%tail_statevars%DataName = 'RSSUN'
  grid%tail_statevars%Description = 'sunlit stomatal resistance'
  grid%tail_statevars%Units = 's/m'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%rssunxy
  grid%tail_statevars%streams(1) = 167772161
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%rssunxy(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",4661,&
    'frame/module_domain.f: Failed to allocate grid%rssunxy(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'rsshaxy'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%rsshaxy(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",4670,&
    'frame/module_domain.f: Failed to allocate grid%rsshaxy(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rsshaxy=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'rsshaxy'
  grid%tail_statevars%DataName = 'RSSHA'
  grid%tail_statevars%Description = 'shaded stomatal resistance'
  grid%tail_statevars%Units = 's/m'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%rsshaxy
  grid%tail_statevars%streams(1) = 167772161
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%rsshaxy(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",4720,&
    'frame/module_domain.f: Failed to allocate grid%rsshaxy(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'bgapxy'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%bgapxy(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",4729,&
    'frame/module_domain.f: Failed to allocate grid%bgapxy(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%bgapxy=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'bgapxy'
  grid%tail_statevars%DataName = 'BGAP'
  grid%tail_statevars%Description = 'between canopy gap'
  grid%tail_statevars%Units = 'fraction'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%bgapxy
  grid%tail_statevars%streams(1) = 167772161
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%bgapxy(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",4779,&
    'frame/module_domain.f: Failed to allocate grid%bgapxy(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'wgapxy'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%wgapxy(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",4788,&
    'frame/module_domain.f: Failed to allocate grid%wgapxy(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%wgapxy=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'wgapxy'
  grid%tail_statevars%DataName = 'WGAP'
  grid%tail_statevars%Description = 'within canopy gap'
  grid%tail_statevars%Units = 'fraction'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%wgapxy
  grid%tail_statevars%streams(1) = 167772161
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%wgapxy(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",4838,&
    'frame/module_domain.f: Failed to allocate grid%wgapxy(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'tgvxy'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%tgvxy(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",4847,&
    'frame/module_domain.f: Failed to allocate grid%tgvxy(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tgvxy=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'tgvxy'
  grid%tail_statevars%DataName = 'TGV'
  grid%tail_statevars%Description = 'ground temp. under canopy'
  grid%tail_statevars%Units = 'K'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%tgvxy
  grid%tail_statevars%streams(1) = 167772161
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%tgvxy(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",4897,&
    'frame/module_domain.f: Failed to allocate grid%tgvxy(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'tgbxy'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%tgbxy(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",4906,&
    'frame/module_domain.f: Failed to allocate grid%tgbxy(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tgbxy=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'tgbxy'
  grid%tail_statevars%DataName = 'TGB'
  grid%tail_statevars%Description = 'bare ground temperature'
  grid%tail_statevars%Units = 'K'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%tgbxy
  grid%tail_statevars%streams(1) = 167772161
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%tgbxy(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",4956,&
    'frame/module_domain.f: Failed to allocate grid%tgbxy(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'chvxy'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%chvxy(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",4965,&
    'frame/module_domain.f: Failed to allocate grid%chvxy(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%chvxy=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'chvxy'
  grid%tail_statevars%DataName = 'CHV'
  grid%tail_statevars%Description = 'vegetated heat exchange coefficient'
  grid%tail_statevars%Units = 'm/s'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%chvxy
  grid%tail_statevars%streams(1) = 167772161
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%chvxy(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",5015,&
    'frame/module_domain.f: Failed to allocate grid%chvxy(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'chbxy'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%chbxy(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",5024,&
    'frame/module_domain.f: Failed to allocate grid%chbxy(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%chbxy=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'chbxy'
  grid%tail_statevars%DataName = 'CHB'
  grid%tail_statevars%Description = 'bare-ground heat exchange coefficient'
  grid%tail_statevars%Units = 'm/s'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%chbxy
  grid%tail_statevars%streams(1) = 167772161
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%chbxy(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",5074,&
    'frame/module_domain.f: Failed to allocate grid%chbxy(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'shgxy'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%shgxy(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",5083,&
    'frame/module_domain.f: Failed to allocate grid%shgxy(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%shgxy=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'shgxy'
  grid%tail_statevars%DataName = 'SHG'
  grid%tail_statevars%Description = 'sensible heat flux: ground to canopy'
  grid%tail_statevars%Units = 'W/m2'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%shgxy
  grid%tail_statevars%streams(1) = 167772161
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%shgxy(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",5133,&
    'frame/module_domain.f: Failed to allocate grid%shgxy(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'shcxy'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%shcxy(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",5142,&
    'frame/module_domain.f: Failed to allocate grid%shcxy(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%shcxy=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'shcxy'
  grid%tail_statevars%DataName = 'SHC'
  grid%tail_statevars%Description = 'sensible heat flux: leaf to canopy'
  grid%tail_statevars%Units = 'W/m2'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%shcxy
  grid%tail_statevars%streams(1) = 167772161
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%shcxy(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",5192,&
    'frame/module_domain.f: Failed to allocate grid%shcxy(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'shbxy'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%shbxy(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",5201,&
    'frame/module_domain.f: Failed to allocate grid%shbxy(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%shbxy=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'shbxy'
  grid%tail_statevars%DataName = 'SHB'
  grid%tail_statevars%Description = 'sensible heat flux: bare grnd to atmo'
  grid%tail_statevars%Units = 'W/m2'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%shbxy
  grid%tail_statevars%streams(1) = 167772161
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%shbxy(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",5251,&
    'frame/module_domain.f: Failed to allocate grid%shbxy(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'evgxy'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%evgxy(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",5260,&
    'frame/module_domain.f: Failed to allocate grid%evgxy(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%evgxy=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'evgxy'
  grid%tail_statevars%DataName = 'EVG'
  grid%tail_statevars%Description = 'latent heat flux: ground to canopy'
  grid%tail_statevars%Units = 'W/m2'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%evgxy
  grid%tail_statevars%streams(1) = 167772161
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%evgxy(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",5310,&
    'frame/module_domain.f: Failed to allocate grid%evgxy(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'evbxy'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%evbxy(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",5319,&
    'frame/module_domain.f: Failed to allocate grid%evbxy(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%evbxy=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'evbxy'
  grid%tail_statevars%DataName = 'EVB'
  grid%tail_statevars%Description = 'latent heat flux: bare grnd to atmo'
  grid%tail_statevars%Units = 'W/m2'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%evbxy
  grid%tail_statevars%streams(1) = 167772161
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%evbxy(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",5369,&
    'frame/module_domain.f: Failed to allocate grid%evbxy(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'ghvxy'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%ghvxy(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",5378,&
    'frame/module_domain.f: Failed to allocate grid%ghvxy(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ghvxy=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'ghvxy'
  grid%tail_statevars%DataName = 'GHV'
  grid%tail_statevars%Description = 'heat flux into soil: under canopy'
  grid%tail_statevars%Units = 'W/m2'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%ghvxy
  grid%tail_statevars%streams(1) = 167772161
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%ghvxy(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",5428,&
    'frame/module_domain.f: Failed to allocate grid%ghvxy(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'ghbxy'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%ghbxy(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",5437,&
    'frame/module_domain.f: Failed to allocate grid%ghbxy(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ghbxy=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'ghbxy'
  grid%tail_statevars%DataName = 'GHB'
  grid%tail_statevars%Description = 'heat flux into soil: bare fraction'
  grid%tail_statevars%Units = 'W/m2'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%ghbxy
  grid%tail_statevars%streams(1) = 167772161
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%ghbxy(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",5487,&
    'frame/module_domain.f: Failed to allocate grid%ghbxy(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'irgxy'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%irgxy(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",5496,&
    'frame/module_domain.f: Failed to allocate grid%irgxy(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%irgxy=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'irgxy'
  grid%tail_statevars%DataName = 'IRG'
  grid%tail_statevars%Description = 'net longwave at below canopy surface'
  grid%tail_statevars%Units = 'W/m2'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%irgxy
  grid%tail_statevars%streams(1) = 167772161
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%irgxy(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",5546,&
    'frame/module_domain.f: Failed to allocate grid%irgxy(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'ircxy'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%ircxy(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",5555,&
    'frame/module_domain.f: Failed to allocate grid%ircxy(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ircxy=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'ircxy'
  grid%tail_statevars%DataName = 'IRC'
  grid%tail_statevars%Description = 'net longwave in canopy'
  grid%tail_statevars%Units = 'W/m2'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%ircxy
  grid%tail_statevars%streams(1) = 167772161
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%ircxy(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",5605,&
    'frame/module_domain.f: Failed to allocate grid%ircxy(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'irbxy'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%irbxy(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",5614,&
    'frame/module_domain.f: Failed to allocate grid%irbxy(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%irbxy=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'irbxy'
  grid%tail_statevars%DataName = 'IRB'
  grid%tail_statevars%Description = 'net longwave at bare fraction surface'
  grid%tail_statevars%Units = 'W/m2'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%irbxy
  grid%tail_statevars%streams(1) = 167772161
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%irbxy(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",5664,&
    'frame/module_domain.f: Failed to allocate grid%irbxy(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'trxy'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%trxy(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",5673,&
    'frame/module_domain.f: Failed to allocate grid%trxy(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%trxy=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'trxy'
  grid%tail_statevars%DataName = 'TR'
  grid%tail_statevars%Description = 'transpiration'
  grid%tail_statevars%Units = 'W/m2'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%trxy
  grid%tail_statevars%streams(1) = 167772161
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%trxy(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",5723,&
    'frame/module_domain.f: Failed to allocate grid%trxy(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'evcxy'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%evcxy(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",5732,&
    'frame/module_domain.f: Failed to allocate grid%evcxy(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%evcxy=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'evcxy'
  grid%tail_statevars%DataName = 'EVC'
  grid%tail_statevars%Description = 'canopy evaporation'
  grid%tail_statevars%Units = 'W/m2'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%evcxy
  grid%tail_statevars%streams(1) = 167772161
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%evcxy(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",5782,&
    'frame/module_domain.f: Failed to allocate grid%evcxy(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'chleafxy'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%chleafxy(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",5791,&
    'frame/module_domain.f: Failed to allocate grid%chleafxy(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%chleafxy=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'chleafxy'
  grid%tail_statevars%DataName = 'CHLEAF'
  grid%tail_statevars%Description = 'leaf exchange coefficient'
  grid%tail_statevars%Units = 'm/s'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%chleafxy
  grid%tail_statevars%streams(1) = 167772161
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%chleafxy(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",5841,&
    'frame/module_domain.f: Failed to allocate grid%chleafxy(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'chucxy'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%chucxy(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",5850,&
    'frame/module_domain.f: Failed to allocate grid%chucxy(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%chucxy=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'chucxy'
  grid%tail_statevars%DataName = 'CHUC'
  grid%tail_statevars%Description = 'under canopy exchange coefficient'
  grid%tail_statevars%Units = 'm/s'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%chucxy
  grid%tail_statevars%streams(1) = 167772161
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%chucxy(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",5900,&
    'frame/module_domain.f: Failed to allocate grid%chucxy(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'chv2xy'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%chv2xy(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",5909,&
    'frame/module_domain.f: Failed to allocate grid%chv2xy(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%chv2xy=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'chv2xy'
  grid%tail_statevars%DataName = 'CHV2'
  grid%tail_statevars%Description = 'leaf exchange coefficient'
  grid%tail_statevars%Units = 'm/s'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%chv2xy
  grid%tail_statevars%streams(1) = 167772161
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%chv2xy(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",5959,&
    'frame/module_domain.f: Failed to allocate grid%chv2xy(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'chb2xy'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%chb2xy(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",5968,&
    'frame/module_domain.f: Failed to allocate grid%chb2xy(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%chb2xy=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'chb2xy'
  grid%tail_statevars%DataName = 'CHB2'
  grid%tail_statevars%Description = 'under canopy exchange coefficient'
  grid%tail_statevars%Units = 'm/s'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%chb2xy
  grid%tail_statevars%streams(1) = 167772161
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%chb2xy(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",6018,&
    'frame/module_domain.f: Failed to allocate grid%chb2xy(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'chstarxy'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%chstarxy(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",6027,&
    'frame/module_domain.f: Failed to allocate grid%chstarxy(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%chstarxy=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'chstarxy'
  grid%tail_statevars%DataName = 'CHSTAR'
  grid%tail_statevars%Description = 'dummy exchange coefficient'
  grid%tail_statevars%Units = 'm/s'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%chstarxy
  grid%tail_statevars%streams(1) = 167772161
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%chstarxy(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",6077,&
    'frame/module_domain.f: Failed to allocate grid%chstarxy(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'mp_restart_state').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((7501)-(1)+1))) * 4
  ALLOCATE(grid%mp_restart_state(1:7501),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",6086,&
    'frame/module_domain.f: Failed to allocate grid%mp_restart_state(1:7501). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%mp_restart_state=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'mp_restart_state'
  grid%tail_statevars%DataName = 'MP_RESTART_STATE'
  grid%tail_statevars%Description = 'STATE VECTOR FOR MICROPHYSICS RESTARTS'
  grid%tail_statevars%Units = '-'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'C'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 1
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_1d => grid%mp_restart_state
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = 1
  grid%tail_statevars%ed1 = 7501
  grid%tail_statevars%sd2 = 1
  grid%tail_statevars%ed2 = 1
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = 1
  grid%tail_statevars%em1 = 7501
  grid%tail_statevars%sm2 = 1
  grid%tail_statevars%em2 = 1
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = 1
  grid%tail_statevars%ep1 = 7501
  grid%tail_statevars%sp2 = 1
  grid%tail_statevars%ep2 = 1
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%dimname1 = ''
  grid%tail_statevars%dimname2 = ''
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%mp_restart_state(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",6134,&
    'frame/module_domain.f: Failed to allocate grid%mp_restart_state(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'tbpvs_state').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((7501)-(1)+1))) * 4
  ALLOCATE(grid%tbpvs_state(1:7501),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",6143,&
    'frame/module_domain.f: Failed to allocate grid%tbpvs_state(1:7501). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tbpvs_state=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'tbpvs_state'
  grid%tail_statevars%DataName = 'TBPVS_STATE'
  grid%tail_statevars%Description = 'STATE FOR ETAMPNEW MICROPHYSICS'
  grid%tail_statevars%Units = '-'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'C'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 1
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_1d => grid%tbpvs_state
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = 1
  grid%tail_statevars%ed1 = 7501
  grid%tail_statevars%sd2 = 1
  grid%tail_statevars%ed2 = 1
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = 1
  grid%tail_statevars%em1 = 7501
  grid%tail_statevars%sm2 = 1
  grid%tail_statevars%em2 = 1
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = 1
  grid%tail_statevars%ep1 = 7501
  grid%tail_statevars%sp2 = 1
  grid%tail_statevars%ep2 = 1
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%dimname1 = ''
  grid%tail_statevars%dimname2 = ''
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%tbpvs_state(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",6191,&
    'frame/module_domain.f: Failed to allocate grid%tbpvs_state(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'tbpvs0_state').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((7501)-(1)+1))) * 4
  ALLOCATE(grid%tbpvs0_state(1:7501),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",6200,&
    'frame/module_domain.f: Failed to allocate grid%tbpvs0_state(1:7501). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tbpvs0_state=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'tbpvs0_state'
  grid%tail_statevars%DataName = 'TBPVS0_STATE'
  grid%tail_statevars%Description = 'STATE FOR ETAMPNEW MICROPHYSICS'
  grid%tail_statevars%Units = '-'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'C'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 1
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_1d => grid%tbpvs0_state
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = 1
  grid%tail_statevars%ed1 = 7501
  grid%tail_statevars%sd2 = 1
  grid%tail_statevars%ed2 = 1
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = 1
  grid%tail_statevars%em1 = 7501
  grid%tail_statevars%sm2 = 1
  grid%tail_statevars%em2 = 1
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = 1
  grid%tail_statevars%ep1 = 7501
  grid%tail_statevars%sp2 = 1
  grid%tail_statevars%ep2 = 1
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%dimname1 = ''
  grid%tail_statevars%dimname2 = ''
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%tbpvs0_state(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",6248,&
    'frame/module_domain.f: Failed to allocate grid%tbpvs0_state(1).  ')
  endif
ENDIF
  IF (.NOT.grid%is_intermediate) THEN
   ALLOCATE( grid%tail_statevars%next )
   grid%tail_statevars => grid%tail_statevars%next
   NULLIFY( grid%tail_statevars%next )
   grid%tail_statevars%ProcOrient = '  '
   grid%tail_statevars%VarName = 'landuse_isice'
   grid%tail_statevars%DataName = 'LANDUSE_ISICE'
   grid%tail_statevars%Description = '-'
   grid%tail_statevars%Units = '-'
   grid%tail_statevars%Type = 'i'
   grid%tail_statevars%Ntl = 0
   grid%tail_statevars%Restart = .TRUE.
   grid%tail_statevars%Ndim = 0
   grid%tail_statevars%scalar_array = .FALSE.
   grid%tail_statevars%ifield_0d => grid%landuse_isice
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097152
  ENDIF
IF ( setinitval .EQ. 3 ) grid%landuse_isice=0
  IF (.NOT.grid%is_intermediate) THEN
   ALLOCATE( grid%tail_statevars%next )
   grid%tail_statevars => grid%tail_statevars%next
   NULLIFY( grid%tail_statevars%next )
   grid%tail_statevars%ProcOrient = '  '
   grid%tail_statevars%VarName = 'landuse_lucats'
   grid%tail_statevars%DataName = 'LANDUSE_LUCATS'
   grid%tail_statevars%Description = '-'
   grid%tail_statevars%Units = '-'
   grid%tail_statevars%Type = 'i'
   grid%tail_statevars%Ntl = 0
   grid%tail_statevars%Restart = .TRUE.
   grid%tail_statevars%Ndim = 0
   grid%tail_statevars%scalar_array = .FALSE.
   grid%tail_statevars%ifield_0d => grid%landuse_lucats
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097152
  ENDIF
IF ( setinitval .EQ. 3 ) grid%landuse_lucats=0
  IF (.NOT.grid%is_intermediate) THEN
   ALLOCATE( grid%tail_statevars%next )
   grid%tail_statevars => grid%tail_statevars%next
   NULLIFY( grid%tail_statevars%next )
   grid%tail_statevars%ProcOrient = '  '
   grid%tail_statevars%VarName = 'landuse_luseas'
   grid%tail_statevars%DataName = 'LANDUSE_LUSEAS'
   grid%tail_statevars%Description = '-'
   grid%tail_statevars%Units = '-'
   grid%tail_statevars%Type = 'i'
   grid%tail_statevars%Ntl = 0
   grid%tail_statevars%Restart = .TRUE.
   grid%tail_statevars%Ndim = 0
   grid%tail_statevars%scalar_array = .FALSE.
   grid%tail_statevars%ifield_0d => grid%landuse_luseas
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097152
  ENDIF
IF ( setinitval .EQ. 3 ) grid%landuse_luseas=0
  IF (.NOT.grid%is_intermediate) THEN
   ALLOCATE( grid%tail_statevars%next )
   grid%tail_statevars => grid%tail_statevars%next
   NULLIFY( grid%tail_statevars%next )
   grid%tail_statevars%ProcOrient = '  '
   grid%tail_statevars%VarName = 'landuse_isn'
   grid%tail_statevars%DataName = 'LANDUSE_ISN'
   grid%tail_statevars%Description = '-'
   grid%tail_statevars%Units = '-'
   grid%tail_statevars%Type = 'i'
   grid%tail_statevars%Ntl = 0
   grid%tail_statevars%Restart = .TRUE.
   grid%tail_statevars%Ndim = 0
   grid%tail_statevars%scalar_array = .FALSE.
   grid%tail_statevars%ifield_0d => grid%landuse_isn
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097152
  ENDIF
IF ( setinitval .EQ. 3 ) grid%landuse_isn=0
IF(in_use_for_config(id,'lu_state').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((7501)-(1)+1))) * 4
  ALLOCATE(grid%lu_state(1:7501),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",6333,&
    'frame/module_domain.f: Failed to allocate grid%lu_state(1:7501). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%lu_state=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'lu_state'
  grid%tail_statevars%DataName = 'LU_STATE'
  grid%tail_statevars%Description = '-'
  grid%tail_statevars%Units = '-'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'C'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 1
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_1d => grid%lu_state
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = 1
  grid%tail_statevars%ed1 = 7501
  grid%tail_statevars%sd2 = 1
  grid%tail_statevars%ed2 = 1
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = 1
  grid%tail_statevars%em1 = 7501
  grid%tail_statevars%sm2 = 1
  grid%tail_statevars%em2 = 1
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = 1
  grid%tail_statevars%ep1 = 7501
  grid%tail_statevars%sp2 = 1
  grid%tail_statevars%ep2 = 1
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%dimname1 = ''
  grid%tail_statevars%dimname2 = ''
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%lu_state(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",6381,&
    'frame/module_domain.f: Failed to allocate grid%lu_state(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'t_phy').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%t_phy(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",6390,&
    'frame/module_domain.f: Failed to allocate grid%t_phy(sm31:em31,sm32:em32,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%t_phy=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 't_phy'
  grid%tail_statevars%DataName = 'T_PHY'
  grid%tail_statevars%Description = 'Temperature'
  grid%tail_statevars%Units = 'K'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XZY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 3
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_3d => grid%t_phy
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = kds
  grid%tail_statevars%ed2 = (kde-1)
  grid%tail_statevars%sd3 = jds
  grid%tail_statevars%ed3 = (jde-1)
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = kms
  grid%tail_statevars%em2 = kme
  grid%tail_statevars%sm3 = jms
  grid%tail_statevars%em3 = jme
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = kps
  grid%tail_statevars%ep2 = MIN( (kde-1), kpe )
  grid%tail_statevars%sp3 = jps
  grid%tail_statevars%ep3 = MIN( (jde-1), jpe )
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'bottom_top'
  grid%tail_statevars%dimname3 = 'south_north'
  ENDIF
ELSE
  ALLOCATE(grid%t_phy(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",6440,&
    'frame/module_domain.f: Failed to allocate grid%t_phy(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'tmn'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%tmn(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",6449,&
    'frame/module_domain.f: Failed to allocate grid%tmn(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tmn=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'tmn'
  grid%tail_statevars%DataName = 'TMN'
  grid%tail_statevars%Description = 'SOIL TEMPERATURE AT LOWER BOUNDARY'
  grid%tail_statevars%Units = 'K'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%tmn
  grid%tail_statevars%streams(1) = 234881025
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%tmn(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",6499,&
    'frame/module_domain.f: Failed to allocate grid%tmn(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'tyr'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%tyr(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",6508,&
    'frame/module_domain.f: Failed to allocate grid%tyr(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tyr=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'tyr'
  grid%tail_statevars%DataName = 'TYR'
  grid%tail_statevars%Description = 'ANNUAL MEAN SFC TEMPERATURE'
  grid%tail_statevars%Units = 'K'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%tyr
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%tyr(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",6558,&
    'frame/module_domain.f: Failed to allocate grid%tyr(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'tyra'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%tyra(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",6567,&
    'frame/module_domain.f: Failed to allocate grid%tyra(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tyra=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'tyra'
  grid%tail_statevars%DataName = 'TYRA'
  grid%tail_statevars%Description = 'ACCUMULATED YEARLY SFC TEMPERATURE FOR CURRENT YEAR'
  grid%tail_statevars%Units = 'K'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%tyra
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%tyra(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",6617,&
    'frame/module_domain.f: Failed to allocate grid%tyra(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'tdly'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%tdly(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",6626,&
    'frame/module_domain.f: Failed to allocate grid%tdly(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tdly=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'tdly'
  grid%tail_statevars%DataName = 'TDLY'
  grid%tail_statevars%Description = 'ACCUMULATED DAILY SFC TEMPERATURE FOR CURRENT DAY'
  grid%tail_statevars%Units = 'K'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%tdly
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%tdly(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",6676,&
    'frame/module_domain.f: Failed to allocate grid%tdly(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'tlag'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((model_config_rec%lagday)-(1)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%tlag(sm31:em31,1:model_config_rec%lagday,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",6685,&
    'frame/module_domain.f: Failed to allocate grid%tlag(sm31:em31,1:model_config_rec%lagday,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tlag=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'tlag'
  grid%tail_statevars%DataName = 'TLAG'
  grid%tail_statevars%Description = 'DAILY MEAN SFC TEMPERATURE OF PRIOR DAYS'
  grid%tail_statevars%Units = 'K'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XZY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 3
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_3d => grid%tlag
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = 1
  grid%tail_statevars%ed2 = config_flags%lagday
  grid%tail_statevars%sd3 = jds
  grid%tail_statevars%ed3 = (jde-1)
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = 1
  grid%tail_statevars%em2 = config_flags%lagday
  grid%tail_statevars%sm3 = jms
  grid%tail_statevars%em3 = jme
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = 1
  grid%tail_statevars%ep2 = config_flags%lagday
  grid%tail_statevars%sp3 = jps
  grid%tail_statevars%ep3 = MIN( (jde-1), jpe )
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'lagday'
  grid%tail_statevars%dimname3 = 'south_north'
  ENDIF
ELSE
  ALLOCATE(grid%tlag(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",6735,&
    'frame/module_domain.f: Failed to allocate grid%tlag(1,1,1).  ')
  endif
ENDIF
  IF (.NOT.grid%is_intermediate) THEN
   ALLOCATE( grid%tail_statevars%next )
   grid%tail_statevars => grid%tail_statevars%next
   NULLIFY( grid%tail_statevars%next )
   grid%tail_statevars%ProcOrient = '  '
   grid%tail_statevars%VarName = 'nyear'
   grid%tail_statevars%DataName = 'NYEAR'
   grid%tail_statevars%Description = 'ACCUM DAYS IN A YEAR'
   grid%tail_statevars%Units = ''
   grid%tail_statevars%Type = 'i'
   grid%tail_statevars%Ntl = 0
   grid%tail_statevars%Restart = .TRUE.
   grid%tail_statevars%Ndim = 0
   grid%tail_statevars%scalar_array = .FALSE.
   grid%tail_statevars%ifield_0d => grid%nyear
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097152
  ENDIF
IF ( setinitval .EQ. 3 ) grid%nyear=0
  IF (.NOT.grid%is_intermediate) THEN
   ALLOCATE( grid%tail_statevars%next )
   grid%tail_statevars => grid%tail_statevars%next
   NULLIFY( grid%tail_statevars%next )
   grid%tail_statevars%ProcOrient = '  '
   grid%tail_statevars%VarName = 'nday'
   grid%tail_statevars%DataName = 'NDAY'
   grid%tail_statevars%Description = 'ACCUM TIMESTEPS IN A DAY'
   grid%tail_statevars%Units = ''
   grid%tail_statevars%Type = 'r'
   grid%tail_statevars%Ntl = 0
   grid%tail_statevars%Restart = .TRUE.
   grid%tail_statevars%Ndim = 0
   grid%tail_statevars%scalar_array = .FALSE.
   grid%tail_statevars%rfield_0d => grid%nday
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097152
  ENDIF
IF ( setinitval .EQ. 3 ) grid%nday=initial_data_value
IF(in_use_for_config(id,'xland'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%xland(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",6782,&
    'frame/module_domain.f: Failed to allocate grid%xland(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%xland=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'xland'
  grid%tail_statevars%DataName = 'XLAND'
  grid%tail_statevars%Description = 'LAND MASK (1 FOR LAND, 2 FOR WATER)'
  grid%tail_statevars%Units = ''
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%xland
  grid%tail_statevars%streams(1) = 167772161
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%xland(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",6832,&
    'frame/module_domain.f: Failed to allocate grid%xland(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'znt').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%znt(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",6841,&
    'frame/module_domain.f: Failed to allocate grid%znt(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%znt=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'znt'
  grid%tail_statevars%DataName = 'ZNT'
  grid%tail_statevars%Description = 'TIME-VARYING ROUGHNESS LENGTH'
  grid%tail_statevars%Units = 'm'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%znt
  grid%tail_statevars%streams(1) = 268435456
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%znt(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",6891,&
    'frame/module_domain.f: Failed to allocate grid%znt(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'ck').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%ck(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",6900,&
    'frame/module_domain.f: Failed to allocate grid%ck(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ck=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'ck'
  grid%tail_statevars%DataName = 'CK'
  grid%tail_statevars%Description = 'ENTHALPY EXCHANGE COEFF AT 10 m'
  grid%tail_statevars%Units = ''
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%ck
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%ck(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",6950,&
    'frame/module_domain.f: Failed to allocate grid%ck(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'cka').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%cka(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",6959,&
    'frame/module_domain.f: Failed to allocate grid%cka(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%cka=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'cka'
  grid%tail_statevars%DataName = 'CKA'
  grid%tail_statevars%Description = 'ENTHALPY EXCHANGE COEFF AT LOWEST MODEL LVL'
  grid%tail_statevars%Units = ''
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%cka
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%cka(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",7009,&
    'frame/module_domain.f: Failed to allocate grid%cka(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'cd').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%cd(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",7018,&
    'frame/module_domain.f: Failed to allocate grid%cd(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%cd=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'cd'
  grid%tail_statevars%DataName = 'CD'
  grid%tail_statevars%Description = 'DRAG COEFF AT 10m'
  grid%tail_statevars%Units = ''
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%cd
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%cd(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",7068,&
    'frame/module_domain.f: Failed to allocate grid%cd(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'cda').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%cda(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",7077,&
    'frame/module_domain.f: Failed to allocate grid%cda(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%cda=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'cda'
  grid%tail_statevars%DataName = 'CDA'
  grid%tail_statevars%Description = 'DRAG COEFF AT LOWEST MODEL LVL'
  grid%tail_statevars%Units = ''
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%cda
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%cda(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",7127,&
    'frame/module_domain.f: Failed to allocate grid%cda(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'ust').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%ust(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",7136,&
    'frame/module_domain.f: Failed to allocate grid%ust(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ust=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'ust'
  grid%tail_statevars%DataName = 'UST'
  grid%tail_statevars%Description = 'U* IN SIMILARITY THEORY'
  grid%tail_statevars%Units = 'm s-1'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%ust
  grid%tail_statevars%streams(1) = 1
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%ust(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",7186,&
    'frame/module_domain.f: Failed to allocate grid%ust(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'ustm').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%ustm(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",7195,&
    'frame/module_domain.f: Failed to allocate grid%ustm(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ustm=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'ustm'
  grid%tail_statevars%DataName = 'USTM'
  grid%tail_statevars%Description = 'U* IN SIMILARITY THEORY WITHOUT VCONV'
  grid%tail_statevars%Units = 'm s-1'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%ustm
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%ustm(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",7245,&
    'frame/module_domain.f: Failed to allocate grid%ustm(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'rmol').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%rmol(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",7254,&
    'frame/module_domain.f: Failed to allocate grid%rmol(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rmol=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'rmol'
  grid%tail_statevars%DataName = 'RMOL'
  grid%tail_statevars%Description = '1./Monin Ob. Length'
  grid%tail_statevars%Units = ''
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%rmol
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%rmol(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",7304,&
    'frame/module_domain.f: Failed to allocate grid%rmol(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'mol').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%mol(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",7313,&
    'frame/module_domain.f: Failed to allocate grid%mol(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%mol=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'mol'
  grid%tail_statevars%DataName = 'MOL'
  grid%tail_statevars%Description = 'T* IN SIMILARITY THEORY'
  grid%tail_statevars%Units = 'K'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%mol
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%mol(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",7363,&
    'frame/module_domain.f: Failed to allocate grid%mol(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'pblh').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%pblh(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",7372,&
    'frame/module_domain.f: Failed to allocate grid%pblh(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%pblh=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'pblh'
  grid%tail_statevars%DataName = 'PBLH'
  grid%tail_statevars%Description = 'PBL HEIGHT'
  grid%tail_statevars%Units = 'm'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%pblh
  grid%tail_statevars%streams(1) = 1
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%pblh(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",7422,&
    'frame/module_domain.f: Failed to allocate grid%pblh(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'capg').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%capg(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",7431,&
    'frame/module_domain.f: Failed to allocate grid%capg(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%capg=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'capg'
  grid%tail_statevars%DataName = 'CAPG'
  grid%tail_statevars%Description = 'HEAT CAPACITY FOR SOIL'
  grid%tail_statevars%Units = 'J K-1 m-3'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%capg
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%capg(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",7481,&
    'frame/module_domain.f: Failed to allocate grid%capg(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'thc').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%thc(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",7490,&
    'frame/module_domain.f: Failed to allocate grid%thc(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%thc=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'thc'
  grid%tail_statevars%DataName = 'THC'
  grid%tail_statevars%Description = 'THERMAL INERTIA'
  grid%tail_statevars%Units = 'Cal cm-2 K-1 s-0.5'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%thc
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%thc(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",7540,&
    'frame/module_domain.f: Failed to allocate grid%thc(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'hfx').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%hfx(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",7549,&
    'frame/module_domain.f: Failed to allocate grid%hfx(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%hfx=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'hfx'
  grid%tail_statevars%DataName = 'HFX'
  grid%tail_statevars%Description = 'UPWARD HEAT FLUX AT THE SURFACE'
  grid%tail_statevars%Units = 'W m-2'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%hfx
  grid%tail_statevars%streams(1) = 1
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%hfx(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",7599,&
    'frame/module_domain.f: Failed to allocate grid%hfx(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'qfx').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%qfx(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",7608,&
    'frame/module_domain.f: Failed to allocate grid%qfx(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%qfx=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'qfx'
  grid%tail_statevars%DataName = 'QFX'
  grid%tail_statevars%Description = 'UPWARD MOISTURE FLUX AT THE SURFACE'
  grid%tail_statevars%Units = 'kg m-2 s-1'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%qfx
  grid%tail_statevars%streams(1) = 1
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%qfx(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",7658,&
    'frame/module_domain.f: Failed to allocate grid%qfx(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'lh').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%lh(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",7667,&
    'frame/module_domain.f: Failed to allocate grid%lh(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%lh=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'lh'
  grid%tail_statevars%DataName = 'LH'
  grid%tail_statevars%Description = 'LATENT HEAT FLUX AT THE SURFACE'
  grid%tail_statevars%Units = 'W m-2'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%lh
  grid%tail_statevars%streams(1) = 1
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%lh(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",7717,&
    'frame/module_domain.f: Failed to allocate grid%lh(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'achfx'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%achfx(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",7726,&
    'frame/module_domain.f: Failed to allocate grid%achfx(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%achfx=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'achfx'
  grid%tail_statevars%DataName = 'ACHFX'
  grid%tail_statevars%Description = 'ACCUMULATED UPWARD HEAT FLUX AT THE SURFACE'
  grid%tail_statevars%Units = 'J m-2'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%achfx
  grid%tail_statevars%streams(1) = 1
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%achfx(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",7776,&
    'frame/module_domain.f: Failed to allocate grid%achfx(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'aclhf'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%aclhf(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",7785,&
    'frame/module_domain.f: Failed to allocate grid%aclhf(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%aclhf=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'aclhf'
  grid%tail_statevars%DataName = 'ACLHF'
  grid%tail_statevars%Description = 'ACCUMULATED UPWARD LATENT HEAT FLUX AT THE SURFACE'
  grid%tail_statevars%Units = 'J m-2'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%aclhf
  grid%tail_statevars%streams(1) = 1
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%aclhf(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",7835,&
    'frame/module_domain.f: Failed to allocate grid%aclhf(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'flhc').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%flhc(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",7844,&
    'frame/module_domain.f: Failed to allocate grid%flhc(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%flhc=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'flhc'
  grid%tail_statevars%DataName = 'FLHC'
  grid%tail_statevars%Description = 'SURFACE EXCHANGE COEFFICIENT FOR HEAT'
  grid%tail_statevars%Units = ''
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%flhc
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%flhc(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",7894,&
    'frame/module_domain.f: Failed to allocate grid%flhc(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'flqc').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%flqc(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",7903,&
    'frame/module_domain.f: Failed to allocate grid%flqc(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%flqc=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'flqc'
  grid%tail_statevars%DataName = 'FLQC'
  grid%tail_statevars%Description = 'SURFACE EXCHANGE COEFFICIENT FOR MOISTURE'
  grid%tail_statevars%Units = ''
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%flqc
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%flqc(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",7953,&
    'frame/module_domain.f: Failed to allocate grid%flqc(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'qsg').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%qsg(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",7962,&
    'frame/module_domain.f: Failed to allocate grid%qsg(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%qsg=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'qsg'
  grid%tail_statevars%DataName = 'QSG'
  grid%tail_statevars%Description = 'SURFACE SATURATION WATER VAPOR MIXING RATIO'
  grid%tail_statevars%Units = 'kg kg-1'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%qsg
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%qsg(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",8012,&
    'frame/module_domain.f: Failed to allocate grid%qsg(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'qvg').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%qvg(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",8021,&
    'frame/module_domain.f: Failed to allocate grid%qvg(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%qvg=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'qvg'
  grid%tail_statevars%DataName = 'QVG'
  grid%tail_statevars%Description = 'WATER VAPOR MIXING RATIO AT THE SURFACE'
  grid%tail_statevars%Units = 'kg kg-1'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%qvg
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%qvg(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",8071,&
    'frame/module_domain.f: Failed to allocate grid%qvg(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'dfi_qvg').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%dfi_qvg(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",8080,&
    'frame/module_domain.f: Failed to allocate grid%dfi_qvg(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%dfi_qvg=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'dfi_qvg'
  grid%tail_statevars%DataName = 'QVG_DFI'
  grid%tail_statevars%Description = 'WATER VAPOR MIXING RATIO AT THE SURFACE'
  grid%tail_statevars%Units = 'kg kg-1'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%dfi_qvg
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%dfi_qvg(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",8130,&
    'frame/module_domain.f: Failed to allocate grid%dfi_qvg(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'qcg').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%qcg(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",8139,&
    'frame/module_domain.f: Failed to allocate grid%qcg(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%qcg=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'qcg'
  grid%tail_statevars%DataName = 'QCG'
  grid%tail_statevars%Description = 'CLOUD WATER MIXING RATIO AT THE GROUND SURFACE'
  grid%tail_statevars%Units = 'kg kg-1'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%qcg
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%qcg(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",8189,&
    'frame/module_domain.f: Failed to allocate grid%qcg(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'dew').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%dew(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",8198,&
    'frame/module_domain.f: Failed to allocate grid%dew(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%dew=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'dew'
  grid%tail_statevars%DataName = 'DEW'
  grid%tail_statevars%Description = 'DEW MIXING RATIO AT THE SURFACE'
  grid%tail_statevars%Units = 'kg kg-1'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%dew
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%dew(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",8248,&
    'frame/module_domain.f: Failed to allocate grid%dew(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'soilt1').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%soilt1(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",8257,&
    'frame/module_domain.f: Failed to allocate grid%soilt1(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soilt1=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'soilt1'
  grid%tail_statevars%DataName = 'SOILT1'
  grid%tail_statevars%Description = 'TEMPERATURE INSIDE SNOW '
  grid%tail_statevars%Units = 'K'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%soilt1
  grid%tail_statevars%streams(1) = 234881025
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%soilt1(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",8307,&
    'frame/module_domain.f: Failed to allocate grid%soilt1(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'dfi_soilt1').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%dfi_soilt1(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",8316,&
    'frame/module_domain.f: Failed to allocate grid%dfi_soilt1(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%dfi_soilt1=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'dfi_soilt1'
  grid%tail_statevars%DataName = 'SOILT1_DFI'
  grid%tail_statevars%Description = 'TEMPERATURE INSIDE SNOW '
  grid%tail_statevars%Units = 'K'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%dfi_soilt1
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%dfi_soilt1(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",8366,&
    'frame/module_domain.f: Failed to allocate grid%dfi_soilt1(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'tsnav').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%tsnav(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",8375,&
    'frame/module_domain.f: Failed to allocate grid%tsnav(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tsnav=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'tsnav'
  grid%tail_statevars%DataName = 'TSNAV'
  grid%tail_statevars%Description = 'AVERAGE SNOW TEMPERATURE '
  grid%tail_statevars%Units = 'C'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%tsnav
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%tsnav(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",8425,&
    'frame/module_domain.f: Failed to allocate grid%tsnav(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'dfi_tsnav').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%dfi_tsnav(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",8434,&
    'frame/module_domain.f: Failed to allocate grid%dfi_tsnav(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%dfi_tsnav=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'dfi_tsnav'
  grid%tail_statevars%DataName = 'TSNAV_DFI'
  grid%tail_statevars%Description = 'AVERAGE SNOW TEMPERATURE '
  grid%tail_statevars%Units = 'C'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%dfi_tsnav
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%dfi_tsnav(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",8484,&
    'frame/module_domain.f: Failed to allocate grid%dfi_tsnav(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'regime').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%regime(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",8493,&
    'frame/module_domain.f: Failed to allocate grid%regime(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%regime=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'regime'
  grid%tail_statevars%DataName = 'REGIME'
  grid%tail_statevars%Description = 'FLAGS: 1=Night/Stable, 2=Mechanical Turbulent, 3=Forced Conv, 4=Free Conv'
  grid%tail_statevars%Units = ''
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%regime
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%regime(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",8543,&
    'frame/module_domain.f: Failed to allocate grid%regime(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'snowc'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%snowc(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",8552,&
    'frame/module_domain.f: Failed to allocate grid%snowc(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%snowc=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'snowc'
  grid%tail_statevars%DataName = 'SNOWC'
  grid%tail_statevars%Description = 'FLAG INDICATING SNOW COVERAGE (1 FOR SNOW COVER)'
  grid%tail_statevars%Units = ''
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%snowc
  grid%tail_statevars%streams(1) = 33554433
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%snowc(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",8602,&
    'frame/module_domain.f: Failed to allocate grid%snowc(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'dfi_snowc').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%dfi_snowc(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",8611,&
    'frame/module_domain.f: Failed to allocate grid%dfi_snowc(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%dfi_snowc=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'dfi_snowc'
  grid%tail_statevars%DataName = 'SNOWC_DFI'
  grid%tail_statevars%Description = 'FLAG INDICATING SNOW COVERAGE (1 FOR SNOW COVER)'
  grid%tail_statevars%Units = ''
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%dfi_snowc
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%dfi_snowc(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",8661,&
    'frame/module_domain.f: Failed to allocate grid%dfi_snowc(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'mavail').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%mavail(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",8670,&
    'frame/module_domain.f: Failed to allocate grid%mavail(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%mavail=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'mavail'
  grid%tail_statevars%DataName = 'MAVAIL'
  grid%tail_statevars%Description = 'SURFACE MOISTURE AVAILABILITY'
  grid%tail_statevars%Units = ''
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%mavail
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%mavail(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",8720,&
    'frame/module_domain.f: Failed to allocate grid%mavail(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'tkesfcf').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%tkesfcf(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",8729,&
    'frame/module_domain.f: Failed to allocate grid%tkesfcf(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tkesfcf=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'tkesfcf'
  grid%tail_statevars%DataName = 'TKESFCF'
  grid%tail_statevars%Description = 'TKE AT THE SURFACE'
  grid%tail_statevars%Units = 'm2 s-2'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%tkesfcf
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%tkesfcf(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",8779,&
    'frame/module_domain.f: Failed to allocate grid%tkesfcf(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'sr').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%sr(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",8788,&
    'frame/module_domain.f: Failed to allocate grid%sr(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sr=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'sr'
  grid%tail_statevars%DataName = 'SR'
  grid%tail_statevars%Description = 'fraction of frozen precipitation'
  grid%tail_statevars%Units = '-'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%sr
  grid%tail_statevars%streams(1) = 33554433
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%sr(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",8838,&
    'frame/module_domain.f: Failed to allocate grid%sr(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'potevp').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%potevp(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",8847,&
    'frame/module_domain.f: Failed to allocate grid%potevp(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%potevp=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'potevp'
  grid%tail_statevars%DataName = 'POTEVP'
  grid%tail_statevars%Description = 'accumulated potential evaporation'
  grid%tail_statevars%Units = 'm'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%potevp
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%potevp(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",8897,&
    'frame/module_domain.f: Failed to allocate grid%potevp(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'snopcx').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%snopcx(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",8906,&
    'frame/module_domain.f: Failed to allocate grid%snopcx(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%snopcx=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'snopcx'
  grid%tail_statevars%DataName = 'SNOPCX'
  grid%tail_statevars%Description = 'snow phase change heat flux'
  grid%tail_statevars%Units = 'W m-2'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%snopcx
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%snopcx(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",8956,&
    'frame/module_domain.f: Failed to allocate grid%snopcx(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'soiltb').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%soiltb(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",8965,&
    'frame/module_domain.f: Failed to allocate grid%soiltb(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%soiltb=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'soiltb'
  grid%tail_statevars%DataName = 'SOILTB'
  grid%tail_statevars%Description = 'bottom soil temperature'
  grid%tail_statevars%Units = 'K'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%soiltb
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%soiltb(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",9015,&
    'frame/module_domain.f: Failed to allocate grid%soiltb(1,1).  ')
  endif
ENDIF
  IF (.NOT.grid%is_intermediate) THEN
   ALLOCATE( grid%tail_statevars%next )
   grid%tail_statevars => grid%tail_statevars%next
   NULLIFY( grid%tail_statevars%next )
   grid%tail_statevars%ProcOrient = '  '
   grid%tail_statevars%VarName = 'stepbl'
   grid%tail_statevars%DataName = 'STEPBL'
   grid%tail_statevars%Description = 'NUMBER OF FUNDAMENTAL TIMESTEPS BETWEEN PBL CALLS'
   grid%tail_statevars%Units = ''
   grid%tail_statevars%Type = 'i'
   grid%tail_statevars%Ntl = 0
   grid%tail_statevars%Restart = .TRUE.
   grid%tail_statevars%Ndim = 0
   grid%tail_statevars%scalar_array = .FALSE.
   grid%tail_statevars%ifield_0d => grid%stepbl
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097152
  ENDIF
IF ( setinitval .EQ. 3 ) grid%stepbl=0
IF(in_use_for_config(id,'taucldi').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%taucldi(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",9043,&
    'frame/module_domain.f: Failed to allocate grid%taucldi(sm31:em31,sm32:em32,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%taucldi=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'taucldi'
  grid%tail_statevars%DataName = 'TAUCLDI'
  grid%tail_statevars%Description = 'CLOUD OPTICAL THICKNESS FOR ICE'
  grid%tail_statevars%Units = ''
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XZY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 3
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_3d => grid%taucldi
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = kds
  grid%tail_statevars%ed2 = (kde-1)
  grid%tail_statevars%sd3 = jds
  grid%tail_statevars%ed3 = (jde-1)
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = kms
  grid%tail_statevars%em2 = kme
  grid%tail_statevars%sm3 = jms
  grid%tail_statevars%em3 = jme
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = kps
  grid%tail_statevars%ep2 = MIN( (kde-1), kpe )
  grid%tail_statevars%sp3 = jps
  grid%tail_statevars%ep3 = MIN( (jde-1), jpe )
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'bottom_top'
  grid%tail_statevars%dimname3 = 'south_north'
  ENDIF
ELSE
  ALLOCATE(grid%taucldi(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",9093,&
    'frame/module_domain.f: Failed to allocate grid%taucldi(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'taucldc').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%taucldc(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",9102,&
    'frame/module_domain.f: Failed to allocate grid%taucldc(sm31:em31,sm32:em32,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%taucldc=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'taucldc'
  grid%tail_statevars%DataName = 'TAUCLDC'
  grid%tail_statevars%Description = 'CLOUD OPTICAL THICKNESS FOR WATER'
  grid%tail_statevars%Units = ''
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XZY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 3
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_3d => grid%taucldc
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = kds
  grid%tail_statevars%ed2 = (kde-1)
  grid%tail_statevars%sd3 = jds
  grid%tail_statevars%ed3 = (jde-1)
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = kms
  grid%tail_statevars%em2 = kme
  grid%tail_statevars%sm3 = jms
  grid%tail_statevars%em3 = jme
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = kps
  grid%tail_statevars%ep2 = MIN( (kde-1), kpe )
  grid%tail_statevars%sp3 = jps
  grid%tail_statevars%ep3 = MIN( (jde-1), jpe )
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'bottom_top'
  grid%tail_statevars%dimname3 = 'south_north'
  ENDIF
ELSE
  ALLOCATE(grid%taucldc(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",9152,&
    'frame/module_domain.f: Failed to allocate grid%taucldc(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'defor11').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%defor11(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",9161,&
    'frame/module_domain.f: Failed to allocate grid%defor11(sm31:em31,sm32:em32,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%defor11=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'defor11'
  grid%tail_statevars%DataName = 'DEFOR11'
  grid%tail_statevars%Description = 'DEFORMATION 11'
  grid%tail_statevars%Units = 's-1'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XZY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 3
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_3d => grid%defor11
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = kds
  grid%tail_statevars%ed2 = (kde-1)
  grid%tail_statevars%sd3 = jds
  grid%tail_statevars%ed3 = (jde-1)
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = kms
  grid%tail_statevars%em2 = kme
  grid%tail_statevars%sm3 = jms
  grid%tail_statevars%em3 = jme
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = kps
  grid%tail_statevars%ep2 = MIN( (kde-1), kpe )
  grid%tail_statevars%sp3 = jps
  grid%tail_statevars%ep3 = MIN( (jde-1), jpe )
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'bottom_top'
  grid%tail_statevars%dimname3 = 'south_north'
  ENDIF
ELSE
  ALLOCATE(grid%defor11(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",9211,&
    'frame/module_domain.f: Failed to allocate grid%defor11(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'defor22').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%defor22(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",9220,&
    'frame/module_domain.f: Failed to allocate grid%defor22(sm31:em31,sm32:em32,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%defor22=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'defor22'
  grid%tail_statevars%DataName = 'DEFOR22'
  grid%tail_statevars%Description = 'DEFORMATION 22'
  grid%tail_statevars%Units = 's-1'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XZY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 3
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_3d => grid%defor22
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = kds
  grid%tail_statevars%ed2 = (kde-1)
  grid%tail_statevars%sd3 = jds
  grid%tail_statevars%ed3 = (jde-1)
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = kms
  grid%tail_statevars%em2 = kme
  grid%tail_statevars%sm3 = jms
  grid%tail_statevars%em3 = jme
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = kps
  grid%tail_statevars%ep2 = MIN( (kde-1), kpe )
  grid%tail_statevars%sp3 = jps
  grid%tail_statevars%ep3 = MIN( (jde-1), jpe )
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'bottom_top'
  grid%tail_statevars%dimname3 = 'south_north'
  ENDIF
ELSE
  ALLOCATE(grid%defor22(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",9270,&
    'frame/module_domain.f: Failed to allocate grid%defor22(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'defor12').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%defor12(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",9279,&
    'frame/module_domain.f: Failed to allocate grid%defor12(sm31:em31,sm32:em32,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%defor12=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'defor12'
  grid%tail_statevars%DataName = 'DEFOR12'
  grid%tail_statevars%Description = 'DEFORMATION 12'
  grid%tail_statevars%Units = 's-1'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XZY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 3
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_3d => grid%defor12
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = kds
  grid%tail_statevars%ed2 = (kde-1)
  grid%tail_statevars%sd3 = jds
  grid%tail_statevars%ed3 = (jde-1)
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = kms
  grid%tail_statevars%em2 = kme
  grid%tail_statevars%sm3 = jms
  grid%tail_statevars%em3 = jme
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = kps
  grid%tail_statevars%ep2 = MIN( (kde-1), kpe )
  grid%tail_statevars%sp3 = jps
  grid%tail_statevars%ep3 = MIN( (jde-1), jpe )
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'bottom_top'
  grid%tail_statevars%dimname3 = 'south_north'
  ENDIF
ELSE
  ALLOCATE(grid%defor12(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",9329,&
    'frame/module_domain.f: Failed to allocate grid%defor12(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'defor33').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%defor33(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",9338,&
    'frame/module_domain.f: Failed to allocate grid%defor33(sm31:em31,sm32:em32,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%defor33=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'defor33'
  grid%tail_statevars%DataName = 'DEFOR33'
  grid%tail_statevars%Description = 'DEFORMATION 33'
  grid%tail_statevars%Units = 's-1'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XZY'
  grid%tail_statevars%Stagger = 'Z'
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 3
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_3d => grid%defor33
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = kds
  grid%tail_statevars%ed2 = kde
  grid%tail_statevars%sd3 = jds
  grid%tail_statevars%ed3 = (jde-1)
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = kms
  grid%tail_statevars%em2 = kme
  grid%tail_statevars%sm3 = jms
  grid%tail_statevars%em3 = jme
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = kps
  grid%tail_statevars%ep2 = MIN( kde, kpe )
  grid%tail_statevars%sp3 = jps
  grid%tail_statevars%ep3 = MIN( (jde-1), jpe )
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'bottom_top_stag'
  grid%tail_statevars%dimname3 = 'south_north'
  ENDIF
ELSE
  ALLOCATE(grid%defor33(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",9388,&
    'frame/module_domain.f: Failed to allocate grid%defor33(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'defor13').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%defor13(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",9397,&
    'frame/module_domain.f: Failed to allocate grid%defor13(sm31:em31,sm32:em32,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%defor13=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'defor13'
  grid%tail_statevars%DataName = 'DEFOR13'
  grid%tail_statevars%Description = 'DEFORMATION 13'
  grid%tail_statevars%Units = 's-1'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XZY'
  grid%tail_statevars%Stagger = 'Z'
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 3
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_3d => grid%defor13
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = kds
  grid%tail_statevars%ed2 = kde
  grid%tail_statevars%sd3 = jds
  grid%tail_statevars%ed3 = (jde-1)
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = kms
  grid%tail_statevars%em2 = kme
  grid%tail_statevars%sm3 = jms
  grid%tail_statevars%em3 = jme
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = kps
  grid%tail_statevars%ep2 = MIN( kde, kpe )
  grid%tail_statevars%sp3 = jps
  grid%tail_statevars%ep3 = MIN( (jde-1), jpe )
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'bottom_top_stag'
  grid%tail_statevars%dimname3 = 'south_north'
  ENDIF
ELSE
  ALLOCATE(grid%defor13(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",9447,&
    'frame/module_domain.f: Failed to allocate grid%defor13(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'defor23').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%defor23(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",9456,&
    'frame/module_domain.f: Failed to allocate grid%defor23(sm31:em31,sm32:em32,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%defor23=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'defor23'
  grid%tail_statevars%DataName = 'DEFOR23'
  grid%tail_statevars%Description = 'DEFORMATION 23'
  grid%tail_statevars%Units = 's-1'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XZY'
  grid%tail_statevars%Stagger = 'Z'
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 3
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_3d => grid%defor23
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = kds
  grid%tail_statevars%ed2 = kde
  grid%tail_statevars%sd3 = jds
  grid%tail_statevars%ed3 = (jde-1)
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = kms
  grid%tail_statevars%em2 = kme
  grid%tail_statevars%sm3 = jms
  grid%tail_statevars%em3 = jme
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = kps
  grid%tail_statevars%ep2 = MIN( kde, kpe )
  grid%tail_statevars%sp3 = jps
  grid%tail_statevars%ep3 = MIN( (jde-1), jpe )
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'bottom_top_stag'
  grid%tail_statevars%dimname3 = 'south_north'
  ENDIF
ELSE
  ALLOCATE(grid%defor23(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",9506,&
    'frame/module_domain.f: Failed to allocate grid%defor23(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'xkmv').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%xkmv(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",9515,&
    'frame/module_domain.f: Failed to allocate grid%xkmv(sm31:em31,sm32:em32,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%xkmv=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'xkmv'
  grid%tail_statevars%DataName = 'XKMV'
  grid%tail_statevars%Description = 'VERTICAL EDDY VISCOSITY'
  grid%tail_statevars%Units = 'm2 s-1'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XZY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 3
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_3d => grid%xkmv
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = kds
  grid%tail_statevars%ed2 = (kde-1)
  grid%tail_statevars%sd3 = jds
  grid%tail_statevars%ed3 = (jde-1)
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = kms
  grid%tail_statevars%em2 = kme
  grid%tail_statevars%sm3 = jms
  grid%tail_statevars%em3 = jme
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = kps
  grid%tail_statevars%ep2 = MIN( (kde-1), kpe )
  grid%tail_statevars%sp3 = jps
  grid%tail_statevars%ep3 = MIN( (jde-1), jpe )
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'bottom_top'
  grid%tail_statevars%dimname3 = 'south_north'
  ENDIF
ELSE
  ALLOCATE(grid%xkmv(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",9565,&
    'frame/module_domain.f: Failed to allocate grid%xkmv(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'xkmh').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%xkmh(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",9574,&
    'frame/module_domain.f: Failed to allocate grid%xkmh(sm31:em31,sm32:em32,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%xkmh=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'xkmh'
  grid%tail_statevars%DataName = 'XKMH'
  grid%tail_statevars%Description = 'HORIZONTAL EDDY VISCOSITY'
  grid%tail_statevars%Units = 'm2 s-1'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XZY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 3
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_3d => grid%xkmh
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = kds
  grid%tail_statevars%ed2 = (kde-1)
  grid%tail_statevars%sd3 = jds
  grid%tail_statevars%ed3 = (jde-1)
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = kms
  grid%tail_statevars%em2 = kme
  grid%tail_statevars%sm3 = jms
  grid%tail_statevars%em3 = jme
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = kps
  grid%tail_statevars%ep2 = MIN( (kde-1), kpe )
  grid%tail_statevars%sp3 = jps
  grid%tail_statevars%ep3 = MIN( (jde-1), jpe )
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'bottom_top'
  grid%tail_statevars%dimname3 = 'south_north'
  ENDIF
ELSE
  ALLOCATE(grid%xkmh(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",9624,&
    'frame/module_domain.f: Failed to allocate grid%xkmh(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'xkhv').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%xkhv(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",9633,&
    'frame/module_domain.f: Failed to allocate grid%xkhv(sm31:em31,sm32:em32,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%xkhv=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'xkhv'
  grid%tail_statevars%DataName = 'XKHV'
  grid%tail_statevars%Description = 'VERTICAL EDDY DIFFUSIVITY OF HEAT'
  grid%tail_statevars%Units = 'm2 s-1'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XZY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 3
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_3d => grid%xkhv
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = kds
  grid%tail_statevars%ed2 = (kde-1)
  grid%tail_statevars%sd3 = jds
  grid%tail_statevars%ed3 = (jde-1)
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = kms
  grid%tail_statevars%em2 = kme
  grid%tail_statevars%sm3 = jms
  grid%tail_statevars%em3 = jme
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = kps
  grid%tail_statevars%ep2 = MIN( (kde-1), kpe )
  grid%tail_statevars%sp3 = jps
  grid%tail_statevars%ep3 = MIN( (jde-1), jpe )
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'bottom_top'
  grid%tail_statevars%dimname3 = 'south_north'
  ENDIF
ELSE
  ALLOCATE(grid%xkhv(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",9683,&
    'frame/module_domain.f: Failed to allocate grid%xkhv(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'xkhh').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%xkhh(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",9692,&
    'frame/module_domain.f: Failed to allocate grid%xkhh(sm31:em31,sm32:em32,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%xkhh=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'xkhh'
  grid%tail_statevars%DataName = 'XKHH'
  grid%tail_statevars%Description = 'HORIZONTAL EDDY DIFFUSIVITY OF HEAT'
  grid%tail_statevars%Units = 'm2 s-1'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XZY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 3
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_3d => grid%xkhh
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = kds
  grid%tail_statevars%ed2 = (kde-1)
  grid%tail_statevars%sd3 = jds
  grid%tail_statevars%ed3 = (jde-1)
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = kms
  grid%tail_statevars%em2 = kme
  grid%tail_statevars%sm3 = jms
  grid%tail_statevars%em3 = jme
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = kps
  grid%tail_statevars%ep2 = MIN( (kde-1), kpe )
  grid%tail_statevars%sp3 = jps
  grid%tail_statevars%ep3 = MIN( (jde-1), jpe )
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'bottom_top'
  grid%tail_statevars%dimname3 = 'south_north'
  ENDIF
ELSE
  ALLOCATE(grid%xkhh(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",9742,&
    'frame/module_domain.f: Failed to allocate grid%xkhh(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'div').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%div(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",9751,&
    'frame/module_domain.f: Failed to allocate grid%div(sm31:em31,sm32:em32,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%div=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'div'
  grid%tail_statevars%DataName = 'DIV'
  grid%tail_statevars%Description = 'DIVERGENCE'
  grid%tail_statevars%Units = 's-1'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XZY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 3
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_3d => grid%div
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = kds
  grid%tail_statevars%ed2 = (kde-1)
  grid%tail_statevars%sd3 = jds
  grid%tail_statevars%ed3 = (jde-1)
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = kms
  grid%tail_statevars%em2 = kme
  grid%tail_statevars%sm3 = jms
  grid%tail_statevars%em3 = jme
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = kps
  grid%tail_statevars%ep2 = MIN( (kde-1), kpe )
  grid%tail_statevars%sp3 = jps
  grid%tail_statevars%ep3 = MIN( (jde-1), jpe )
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'bottom_top'
  grid%tail_statevars%dimname3 = 'south_north'
  ENDIF
ELSE
  ALLOCATE(grid%div(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",9801,&
    'frame/module_domain.f: Failed to allocate grid%div(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'bn2').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%bn2(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",9810,&
    'frame/module_domain.f: Failed to allocate grid%bn2(sm31:em31,sm32:em32,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%bn2=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'bn2'
  grid%tail_statevars%DataName = 'BN2'
  grid%tail_statevars%Description = 'BRUNT-VAISALA FREQUENCY'
  grid%tail_statevars%Units = 's-2'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XZY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 3
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_3d => grid%bn2
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = kds
  grid%tail_statevars%ed2 = (kde-1)
  grid%tail_statevars%sd3 = jds
  grid%tail_statevars%ed3 = (jde-1)
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = kms
  grid%tail_statevars%em2 = kme
  grid%tail_statevars%sm3 = jms
  grid%tail_statevars%em3 = jme
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = kps
  grid%tail_statevars%ep2 = MIN( (kde-1), kpe )
  grid%tail_statevars%sp3 = jps
  grid%tail_statevars%ep3 = MIN( (jde-1), jpe )
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'bottom_top'
  grid%tail_statevars%dimname3 = 'south_north'
  ENDIF
ELSE
  ALLOCATE(grid%bn2(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",9860,&
    'frame/module_domain.f: Failed to allocate grid%bn2(1,1,1).  ')
  endif
ENDIF
  IF (.NOT.grid%is_intermediate) THEN
   ALLOCATE( grid%tail_statevars%next )
   grid%tail_statevars => grid%tail_statevars%next
   NULLIFY( grid%tail_statevars%next )
   grid%tail_statevars%ProcOrient = '  '
   grid%tail_statevars%VarName = 'warm_rain'
   grid%tail_statevars%DataName = 'WARM_RAIN'
   grid%tail_statevars%Description = 'WARM_RAIN_LOGICAL'
   grid%tail_statevars%Units = '-'
   grid%tail_statevars%Type = 'l'
   grid%tail_statevars%Ntl = 0
   grid%tail_statevars%Restart = .FALSE.
   grid%tail_statevars%Ndim = 0
   grid%tail_statevars%scalar_array = .FALSE.
   grid%tail_statevars%lfield_0d => grid%warm_rain
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 0
  ENDIF
IF ( setinitval .EQ. 3 ) grid%warm_rain=.FALSE.
  IF (.NOT.grid%is_intermediate) THEN
   ALLOCATE( grid%tail_statevars%next )
   grid%tail_statevars => grid%tail_statevars%next
   NULLIFY( grid%tail_statevars%next )
   grid%tail_statevars%ProcOrient = '  '
   grid%tail_statevars%VarName = 'adv_moist_cond'
   grid%tail_statevars%DataName = 'ADV_MOIST_COND'
   grid%tail_statevars%Description = 'ADVECT MOIST CONDENSATES LOGICAL'
   grid%tail_statevars%Units = '-'
   grid%tail_statevars%Type = 'l'
   grid%tail_statevars%Ntl = 0
   grid%tail_statevars%Restart = .FALSE.
   grid%tail_statevars%Ndim = 0
   grid%tail_statevars%scalar_array = .FALSE.
   grid%tail_statevars%lfield_0d => grid%adv_moist_cond
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 0
  ENDIF
IF ( setinitval .EQ. 3 ) grid%adv_moist_cond=.FALSE.
  IF (.NOT.grid%is_intermediate) THEN
   ALLOCATE( grid%tail_statevars%next )
   grid%tail_statevars => grid%tail_statevars%next
   NULLIFY( grid%tail_statevars%next )
   grid%tail_statevars%ProcOrient = '  '
   grid%tail_statevars%VarName = 'save_topo_from_real'
   grid%tail_statevars%DataName = 'SAVE_TOPO_FROM_REAL'
   grid%tail_statevars%Description = '1=original topo from real/0=topo modified by WRF'
   grid%tail_statevars%Units = 'flag'
   grid%tail_statevars%Type = 'i'
   grid%tail_statevars%Ntl = 0
   grid%tail_statevars%Restart = .TRUE.
   grid%tail_statevars%Ndim = 0
   grid%tail_statevars%scalar_array = .FALSE.
   grid%tail_statevars%ifield_0d => grid%save_topo_from_real
  grid%tail_statevars%streams(1) = 33554433
  grid%tail_statevars%streams(2) = 2097152
  ENDIF
IF ( setinitval .EQ. 3 ) grid%save_topo_from_real=0
  IF (.NOT.grid%is_intermediate) THEN
   ALLOCATE( grid%tail_statevars%next )
   grid%tail_statevars => grid%tail_statevars%next
   NULLIFY( grid%tail_statevars%next )
   grid%tail_statevars%ProcOrient = '  '
   grid%tail_statevars%VarName = 'stepfg'
   grid%tail_statevars%DataName = 'STEPFG'
   grid%tail_statevars%Description = 'NUMBER OF FUNDAMENTAL TIMESTEPS BETWEEN FDDA GRID CALLS'
   grid%tail_statevars%Units = ''
   grid%tail_statevars%Type = 'i'
   grid%tail_statevars%Ntl = 0
   grid%tail_statevars%Restart = .TRUE.
   grid%tail_statevars%Ndim = 0
   grid%tail_statevars%scalar_array = .FALSE.
   grid%tail_statevars%ifield_0d => grid%stepfg
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097152
  ENDIF
IF ( setinitval .EQ. 3 ) grid%stepfg=0
IF(in_use_for_config(id,'rundgdten').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%rundgdten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",9945,&
    'frame/module_domain.f: Failed to allocate grid%rundgdten(sm31:em31,sm32:em32,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rundgdten=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'rundgdten'
  grid%tail_statevars%DataName = 'RUNDGDTEN'
  grid%tail_statevars%Description = 'COUPLED X WIND TENDENCY DUE TO FDDA GRID NUDGING'
  grid%tail_statevars%Units = 'Pa m s-2'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XZY'
  grid%tail_statevars%Stagger = 'X'
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 3
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_3d => grid%rundgdten
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = ide
  grid%tail_statevars%sd2 = kds
  grid%tail_statevars%ed2 = (kde-1)
  grid%tail_statevars%sd3 = jds
  grid%tail_statevars%ed3 = (jde-1)
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = kms
  grid%tail_statevars%em2 = kme
  grid%tail_statevars%sm3 = jms
  grid%tail_statevars%em3 = jme
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( ide, ipe )
  grid%tail_statevars%sp2 = kps
  grid%tail_statevars%ep2 = MIN( (kde-1), kpe )
  grid%tail_statevars%sp3 = jps
  grid%tail_statevars%ep3 = MIN( (jde-1), jpe )
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east_stag'
  grid%tail_statevars%dimname2 = 'bottom_top'
  grid%tail_statevars%dimname3 = 'south_north'
  ENDIF
ELSE
  ALLOCATE(grid%rundgdten(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",9995,&
    'frame/module_domain.f: Failed to allocate grid%rundgdten(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'rvndgdten').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%rvndgdten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",10004,&
    'frame/module_domain.f: Failed to allocate grid%rvndgdten(sm31:em31,sm32:em32,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rvndgdten=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'rvndgdten'
  grid%tail_statevars%DataName = 'RVNDGDTEN'
  grid%tail_statevars%Description = 'COUPLED Y WIND TENDENCY DUE TO FDDA GRID NUDGING'
  grid%tail_statevars%Units = 'Pa m s-2'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XZY'
  grid%tail_statevars%Stagger = 'Y'
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 3
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_3d => grid%rvndgdten
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = kds
  grid%tail_statevars%ed2 = (kde-1)
  grid%tail_statevars%sd3 = jds
  grid%tail_statevars%ed3 = jde
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = kms
  grid%tail_statevars%em2 = kme
  grid%tail_statevars%sm3 = jms
  grid%tail_statevars%em3 = jme
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = kps
  grid%tail_statevars%ep2 = MIN( (kde-1), kpe )
  grid%tail_statevars%sp3 = jps
  grid%tail_statevars%ep3 = MIN( jde, jpe )
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'bottom_top'
  grid%tail_statevars%dimname3 = 'south_north_stag'
  ENDIF
ELSE
  ALLOCATE(grid%rvndgdten(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",10054,&
    'frame/module_domain.f: Failed to allocate grid%rvndgdten(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'rthndgdten').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%rthndgdten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",10063,&
    'frame/module_domain.f: Failed to allocate grid%rthndgdten(sm31:em31,sm32:em32,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rthndgdten=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'rthndgdten'
  grid%tail_statevars%DataName = 'RTHNDGDTEN'
  grid%tail_statevars%Description = 'COUPLED THETA TENDENCY DUE TO FDDA GRID NUDGING'
  grid%tail_statevars%Units = 'Pa K s-1'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XZY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 3
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_3d => grid%rthndgdten
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = kds
  grid%tail_statevars%ed2 = (kde-1)
  grid%tail_statevars%sd3 = jds
  grid%tail_statevars%ed3 = (jde-1)
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = kms
  grid%tail_statevars%em2 = kme
  grid%tail_statevars%sm3 = jms
  grid%tail_statevars%em3 = jme
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = kps
  grid%tail_statevars%ep2 = MIN( (kde-1), kpe )
  grid%tail_statevars%sp3 = jps
  grid%tail_statevars%ep3 = MIN( (jde-1), jpe )
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'bottom_top'
  grid%tail_statevars%dimname3 = 'south_north'
  ENDIF
ELSE
  ALLOCATE(grid%rthndgdten(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",10113,&
    'frame/module_domain.f: Failed to allocate grid%rthndgdten(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'rphndgdten').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%rphndgdten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",10122,&
    'frame/module_domain.f: Failed to allocate grid%rphndgdten(sm31:em31,sm32:em32,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rphndgdten=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'rphndgdten'
  grid%tail_statevars%DataName = 'RPHNDGDTEN'
  grid%tail_statevars%Description = 'COUPLED GEOPOTENTIAL TENDENCY DUE TO FDDA GRID NUDGING'
  grid%tail_statevars%Units = 'Pa m2 s-3'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XZY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 3
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_3d => grid%rphndgdten
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = kds
  grid%tail_statevars%ed2 = (kde-1)
  grid%tail_statevars%sd3 = jds
  grid%tail_statevars%ed3 = (jde-1)
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = kms
  grid%tail_statevars%em2 = kme
  grid%tail_statevars%sm3 = jms
  grid%tail_statevars%em3 = jme
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = kps
  grid%tail_statevars%ep2 = MIN( (kde-1), kpe )
  grid%tail_statevars%sp3 = jps
  grid%tail_statevars%ep3 = MIN( (jde-1), jpe )
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'bottom_top'
  grid%tail_statevars%dimname3 = 'south_north'
  ENDIF
ELSE
  ALLOCATE(grid%rphndgdten(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",10172,&
    'frame/module_domain.f: Failed to allocate grid%rphndgdten(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'rqvndgdten').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%rqvndgdten(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",10181,&
    'frame/module_domain.f: Failed to allocate grid%rqvndgdten(sm31:em31,sm32:em32,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rqvndgdten=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'rqvndgdten'
  grid%tail_statevars%DataName = 'RQVNDGDTEN'
  grid%tail_statevars%Description = 'COUPLED Q_V TENDENCY DUE TO FDDA GRID NUDGING'
  grid%tail_statevars%Units = 'Pa kg kg-1 s-1'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XZY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 3
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_3d => grid%rqvndgdten
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = kds
  grid%tail_statevars%ed2 = (kde-1)
  grid%tail_statevars%sd3 = jds
  grid%tail_statevars%ed3 = (jde-1)
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = kms
  grid%tail_statevars%em2 = kme
  grid%tail_statevars%sm3 = jms
  grid%tail_statevars%em3 = jme
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = kps
  grid%tail_statevars%ep2 = MIN( (kde-1), kpe )
  grid%tail_statevars%sp3 = jps
  grid%tail_statevars%ep3 = MIN( (jde-1), jpe )
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'bottom_top'
  grid%tail_statevars%dimname3 = 'south_north'
  ENDIF
ELSE
  ALLOCATE(grid%rqvndgdten(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",10231,&
    'frame/module_domain.f: Failed to allocate grid%rqvndgdten(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'rmundgdten').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%rmundgdten(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",10240,&
    'frame/module_domain.f: Failed to allocate grid%rmundgdten(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rmundgdten=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'rmundgdten'
  grid%tail_statevars%DataName = 'RMUNDGDTEN'
  grid%tail_statevars%Description = 'MU TENDENCY DUE TO FDDA GRID NUDGING'
  grid%tail_statevars%Units = 'Pa s-1'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%rmundgdten
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%rmundgdten(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",10290,&
    'frame/module_domain.f: Failed to allocate grid%rmundgdten(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'fdda3d'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1)*num_fdda3d)) * 4
  ALLOCATE(grid%fdda3d(sm31:em31,sm32:em32,sm33:em33,num_fdda3d),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",10299,&
    'frame/module_domain.f: Failed to allocate grid%fdda3d(sm31:em31,sm32:em32,sm33:em33,num_fdda3d). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%fdda3d=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'fdda3d'
  grid%tail_statevars%DataName = 'FDDA3D'
  grid%tail_statevars%Description = '-'
  grid%tail_statevars%Units = '-'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XZY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 4
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .TRUE.
  grid%tail_statevars%rfield_4d => grid%fdda3d
  grid%tail_statevars%num_table => fdda3d_num_table
  grid%tail_statevars%index_table => fdda3d_index_table
  grid%tail_statevars%boundary_table => fdda3d_boundary_table
  grid%tail_statevars%dname_table => fdda3d_dname_table
  grid%tail_statevars%desc_table => fdda3d_desc_table
  grid%tail_statevars%units_table => fdda3d_units_table
  grid%tail_statevars%streams_table => fdda3d_streams_table
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097168
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = kds
  grid%tail_statevars%ed2 = (kde-1)
  grid%tail_statevars%sd3 = jds
  grid%tail_statevars%ed3 = (jde-1)
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = kms
  grid%tail_statevars%em2 = kme
  grid%tail_statevars%sm3 = jms
  grid%tail_statevars%em3 = jme
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = kps
  grid%tail_statevars%ep2 = MIN( (kde-1), kpe )
  grid%tail_statevars%sp3 = jps
  grid%tail_statevars%ep3 = MIN( (jde-1), jpe )
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'bottom_top'
  grid%tail_statevars%dimname3 = 'south_north'
  ENDIF
ELSE
  ALLOCATE(grid%fdda3d(1,1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",10356,&
    'frame/module_domain.f: Failed to allocate grid%fdda3d(1,1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'fdda2d'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((1)-(1)+1))*(((em33)-(sm33)+1)*num_fdda2d)) * 4
  ALLOCATE(grid%fdda2d(sm31:em31,1:1,sm33:em33,num_fdda2d),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",10365,&
    'frame/module_domain.f: Failed to allocate grid%fdda2d(sm31:em31,1:1,sm33:em33,num_fdda2d). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%fdda2d=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'fdda2d'
  grid%tail_statevars%DataName = 'FDDA2D'
  grid%tail_statevars%Description = '-'
  grid%tail_statevars%Units = '-'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XZY'
  grid%tail_statevars%Stagger = 'Z'
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 4
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .TRUE.
  grid%tail_statevars%rfield_4d => grid%fdda2d
  grid%tail_statevars%num_table => fdda2d_num_table
  grid%tail_statevars%index_table => fdda2d_index_table
  grid%tail_statevars%boundary_table => fdda2d_boundary_table
  grid%tail_statevars%dname_table => fdda2d_dname_table
  grid%tail_statevars%desc_table => fdda2d_desc_table
  grid%tail_statevars%units_table => fdda2d_units_table
  grid%tail_statevars%streams_table => fdda2d_streams_table
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097168
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = 1
  grid%tail_statevars%ed2 = 1
  grid%tail_statevars%sd3 = jds
  grid%tail_statevars%ed3 = (jde-1)
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = 1
  grid%tail_statevars%em2 = 1
  grid%tail_statevars%sm3 = jms
  grid%tail_statevars%em3 = jme
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = 1
  grid%tail_statevars%ep2 = 1
  grid%tail_statevars%sp3 = jps
  grid%tail_statevars%ep3 = MIN( (jde-1), jpe )
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'one_stag'
  grid%tail_statevars%dimname3 = 'south_north'
  ENDIF
ELSE
  ALLOCATE(grid%fdda2d(1,1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",10422,&
    'frame/module_domain.f: Failed to allocate grid%fdda2d(1,1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'u10_ndg_old').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%u10_ndg_old(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",10431,&
    'frame/module_domain.f: Failed to allocate grid%u10_ndg_old(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%u10_ndg_old=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'u10_ndg_old'
  grid%tail_statevars%DataName = 'U10_NDG_OLD'
  grid%tail_statevars%Description = 'OLD U10 FOR SURFACE FDDA GRID NUDGING'
  grid%tail_statevars%Units = 'm s-1'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = 'X'
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%u10_ndg_old
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097160
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = ide
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( ide, ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east_stag'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%u10_ndg_old(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",10481,&
    'frame/module_domain.f: Failed to allocate grid%u10_ndg_old(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'u10_ndg_new').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%u10_ndg_new(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",10490,&
    'frame/module_domain.f: Failed to allocate grid%u10_ndg_new(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%u10_ndg_new=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'u10_ndg_new'
  grid%tail_statevars%DataName = 'U10_NDG_NEW'
  grid%tail_statevars%Description = 'NEW U10 FOR SURFACE FDDA GRID NUDGING'
  grid%tail_statevars%Units = 'm s-1'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = 'X'
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%u10_ndg_new
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097160
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = ide
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( ide, ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east_stag'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%u10_ndg_new(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",10540,&
    'frame/module_domain.f: Failed to allocate grid%u10_ndg_new(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'v10_ndg_old').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%v10_ndg_old(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",10549,&
    'frame/module_domain.f: Failed to allocate grid%v10_ndg_old(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%v10_ndg_old=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'v10_ndg_old'
  grid%tail_statevars%DataName = 'V10_NDG_OLD'
  grid%tail_statevars%Description = 'OLD V10 FOR SURFACE FDDA GRID NUDGING'
  grid%tail_statevars%Units = 'm s-1'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = 'Y'
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%v10_ndg_old
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097160
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = jde
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( jde, jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north_stag'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%v10_ndg_old(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",10599,&
    'frame/module_domain.f: Failed to allocate grid%v10_ndg_old(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'v10_ndg_new').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%v10_ndg_new(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",10608,&
    'frame/module_domain.f: Failed to allocate grid%v10_ndg_new(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%v10_ndg_new=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'v10_ndg_new'
  grid%tail_statevars%DataName = 'V10_NDG_NEW'
  grid%tail_statevars%Description = 'NEW V10 FOR SURFACE FDDA GRID NUDGING'
  grid%tail_statevars%Units = 'm s-1'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = 'Y'
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%v10_ndg_new
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097160
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = jde
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( jde, jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north_stag'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%v10_ndg_new(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",10658,&
    'frame/module_domain.f: Failed to allocate grid%v10_ndg_new(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'t2_ndg_old').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%t2_ndg_old(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",10667,&
    'frame/module_domain.f: Failed to allocate grid%t2_ndg_old(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%t2_ndg_old=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 't2_ndg_old'
  grid%tail_statevars%DataName = 'T2_NDG_OLD'
  grid%tail_statevars%Description = 'OLD T2 FOR SURFACE FDDA GRID NUDGING'
  grid%tail_statevars%Units = 'K'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%t2_ndg_old
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097160
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%t2_ndg_old(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",10717,&
    'frame/module_domain.f: Failed to allocate grid%t2_ndg_old(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'t2_ndg_new').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%t2_ndg_new(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",10726,&
    'frame/module_domain.f: Failed to allocate grid%t2_ndg_new(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%t2_ndg_new=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 't2_ndg_new'
  grid%tail_statevars%DataName = 'T2_NDG_NEW'
  grid%tail_statevars%Description = 'NEW T2 FOR SURFACE FDDA GRID NUDGING'
  grid%tail_statevars%Units = 'K'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%t2_ndg_new
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097160
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%t2_ndg_new(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",10776,&
    'frame/module_domain.f: Failed to allocate grid%t2_ndg_new(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'th2_ndg_old').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%th2_ndg_old(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",10785,&
    'frame/module_domain.f: Failed to allocate grid%th2_ndg_old(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%th2_ndg_old=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'th2_ndg_old'
  grid%tail_statevars%DataName = 'TH2_NDG_OLD'
  grid%tail_statevars%Description = 'OLD TH2 FOR SURFACE FDDA GRID NUDGING'
  grid%tail_statevars%Units = 'K'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%th2_ndg_old
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097160
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%th2_ndg_old(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",10835,&
    'frame/module_domain.f: Failed to allocate grid%th2_ndg_old(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'th2_ndg_new').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%th2_ndg_new(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",10844,&
    'frame/module_domain.f: Failed to allocate grid%th2_ndg_new(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%th2_ndg_new=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'th2_ndg_new'
  grid%tail_statevars%DataName = 'TH2_NDG_NEW'
  grid%tail_statevars%Description = 'NEW TH2 FOR SURFACE FDDA GRID NUDGING'
  grid%tail_statevars%Units = 'K'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%th2_ndg_new
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097160
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%th2_ndg_new(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",10894,&
    'frame/module_domain.f: Failed to allocate grid%th2_ndg_new(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'q2_ndg_old').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%q2_ndg_old(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",10903,&
    'frame/module_domain.f: Failed to allocate grid%q2_ndg_old(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%q2_ndg_old=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'q2_ndg_old'
  grid%tail_statevars%DataName = 'Q2_NDG_OLD'
  grid%tail_statevars%Description = 'OLD Q2 FOR SURFACE FDDA GRID NUDGING'
  grid%tail_statevars%Units = 'kg kg-1'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%q2_ndg_old
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097160
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%q2_ndg_old(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",10953,&
    'frame/module_domain.f: Failed to allocate grid%q2_ndg_old(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'q2_ndg_new').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%q2_ndg_new(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",10962,&
    'frame/module_domain.f: Failed to allocate grid%q2_ndg_new(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%q2_ndg_new=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'q2_ndg_new'
  grid%tail_statevars%DataName = 'Q2_NDG_NEW'
  grid%tail_statevars%Description = 'NEW Q2 FOR SURFACE FDDA GRID NUDGING'
  grid%tail_statevars%Units = 'kg kg-1'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%q2_ndg_new
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097160
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%q2_ndg_new(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",11012,&
    'frame/module_domain.f: Failed to allocate grid%q2_ndg_new(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'rh_ndg_old').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%rh_ndg_old(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",11021,&
    'frame/module_domain.f: Failed to allocate grid%rh_ndg_old(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rh_ndg_old=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'rh_ndg_old'
  grid%tail_statevars%DataName = 'RH_NDG_OLD'
  grid%tail_statevars%Description = 'OLD RH FOR SURFACE FDDA GRID NUDGING'
  grid%tail_statevars%Units = '%'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%rh_ndg_old
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097160
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%rh_ndg_old(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",11071,&
    'frame/module_domain.f: Failed to allocate grid%rh_ndg_old(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'rh_ndg_new').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%rh_ndg_new(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",11080,&
    'frame/module_domain.f: Failed to allocate grid%rh_ndg_new(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%rh_ndg_new=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'rh_ndg_new'
  grid%tail_statevars%DataName = 'RH_NDG_NEW'
  grid%tail_statevars%Description = 'NEW RH FOR SURFACE FDDA GRID NUDGING'
  grid%tail_statevars%Units = '%'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%rh_ndg_new
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097160
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%rh_ndg_new(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",11130,&
    'frame/module_domain.f: Failed to allocate grid%rh_ndg_new(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'psl_ndg_old').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%psl_ndg_old(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",11139,&
    'frame/module_domain.f: Failed to allocate grid%psl_ndg_old(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%psl_ndg_old=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'psl_ndg_old'
  grid%tail_statevars%DataName = 'PSL_NDG_OLD'
  grid%tail_statevars%Description = 'OLD PSL FOR SURFACE FDDA GRID NUDGING'
  grid%tail_statevars%Units = 'Pa'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%psl_ndg_old
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097160
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%psl_ndg_old(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",11189,&
    'frame/module_domain.f: Failed to allocate grid%psl_ndg_old(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'psl_ndg_new').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%psl_ndg_new(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",11198,&
    'frame/module_domain.f: Failed to allocate grid%psl_ndg_new(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%psl_ndg_new=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'psl_ndg_new'
  grid%tail_statevars%DataName = 'PSL_NDG_NEW'
  grid%tail_statevars%Description = 'NEW PSL FOR SURFACE FDDA GRID NUDGING'
  grid%tail_statevars%Units = 'Pa'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%psl_ndg_new
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097160
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%psl_ndg_new(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",11248,&
    'frame/module_domain.f: Failed to allocate grid%psl_ndg_new(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'ps_ndg_old').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%ps_ndg_old(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",11257,&
    'frame/module_domain.f: Failed to allocate grid%ps_ndg_old(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ps_ndg_old=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'ps_ndg_old'
  grid%tail_statevars%DataName = 'PS_NDG_OLD'
  grid%tail_statevars%Description = 'OLD PS FOR SURFACE FDDA GRID NUDGING'
  grid%tail_statevars%Units = 'Pa'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%ps_ndg_old
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097160
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%ps_ndg_old(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",11307,&
    'frame/module_domain.f: Failed to allocate grid%ps_ndg_old(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'ps_ndg_new').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%ps_ndg_new(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",11316,&
    'frame/module_domain.f: Failed to allocate grid%ps_ndg_new(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%ps_ndg_new=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'ps_ndg_new'
  grid%tail_statevars%DataName = 'PS_NDG_NEW'
  grid%tail_statevars%Description = 'NEW PS FOR SURFACE FDDA GRID NUDGING'
  grid%tail_statevars%Units = 'Pa'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%ps_ndg_new
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097160
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%ps_ndg_new(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",11366,&
    'frame/module_domain.f: Failed to allocate grid%ps_ndg_new(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'tob_ndg_old').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%tob_ndg_old(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",11375,&
    'frame/module_domain.f: Failed to allocate grid%tob_ndg_old(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tob_ndg_old=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'tob_ndg_old'
  grid%tail_statevars%DataName = 'TOB_NDG_OLD'
  grid%tail_statevars%Description = 'OLD TOB FOR SURFACE FDDA GRID NUDGING'
  grid%tail_statevars%Units = ''
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%tob_ndg_old
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097160
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%tob_ndg_old(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",11425,&
    'frame/module_domain.f: Failed to allocate grid%tob_ndg_old(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'odis_ndg_old').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%odis_ndg_old(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",11434,&
    'frame/module_domain.f: Failed to allocate grid%odis_ndg_old(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%odis_ndg_old=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'odis_ndg_old'
  grid%tail_statevars%DataName = 'ODIS_NDG_OLD'
  grid%tail_statevars%Description = 'OLD ODIS FOR SURFACE FDDA GRID NUDGING'
  grid%tail_statevars%Units = 'km'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%odis_ndg_old
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097160
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%odis_ndg_old(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",11484,&
    'frame/module_domain.f: Failed to allocate grid%odis_ndg_old(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'tob_ndg_new').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%tob_ndg_new(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",11493,&
    'frame/module_domain.f: Failed to allocate grid%tob_ndg_new(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tob_ndg_new=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'tob_ndg_new'
  grid%tail_statevars%DataName = 'TOB_NDG_NEW'
  grid%tail_statevars%Description = 'NEW TOB FOR SURFACE FDDA GRID NUDGING'
  grid%tail_statevars%Units = ''
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%tob_ndg_new
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097160
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%tob_ndg_new(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",11543,&
    'frame/module_domain.f: Failed to allocate grid%tob_ndg_new(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'odis_ndg_new').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%odis_ndg_new(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",11552,&
    'frame/module_domain.f: Failed to allocate grid%odis_ndg_new(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%odis_ndg_new=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'odis_ndg_new'
  grid%tail_statevars%DataName = 'ODIS_NDG_NEW'
  grid%tail_statevars%Description = 'NEW ODIS FOR SURFACE FDDA GRID NUDGING'
  grid%tail_statevars%Units = 'km'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%odis_ndg_new
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097160
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%odis_ndg_new(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",11602,&
    'frame/module_domain.f: Failed to allocate grid%odis_ndg_new(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'sn_ndg_new').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%sn_ndg_new(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",11611,&
    'frame/module_domain.f: Failed to allocate grid%sn_ndg_new(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sn_ndg_new=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'sn_ndg_new'
  grid%tail_statevars%DataName = 'SN_NDG_NEW'
  grid%tail_statevars%Description = 'NEW Snow Water Equivalent'
  grid%tail_statevars%Units = 'mm'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%sn_ndg_new
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097160
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%sn_ndg_new(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",11661,&
    'frame/module_domain.f: Failed to allocate grid%sn_ndg_new(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'sn_ndg_old').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%sn_ndg_old(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",11670,&
    'frame/module_domain.f: Failed to allocate grid%sn_ndg_old(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%sn_ndg_old=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'sn_ndg_old'
  grid%tail_statevars%DataName = 'SN_NDG_OLD'
  grid%tail_statevars%Description = 'OLD Snow Water Equivalent'
  grid%tail_statevars%Units = 'mm'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%sn_ndg_old
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097160
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%sn_ndg_old(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",11720,&
    'frame/module_domain.f: Failed to allocate grid%sn_ndg_old(1,1).  ')
  endif
ENDIF
  IF (.NOT.grid%is_intermediate) THEN
   ALLOCATE( grid%tail_statevars%next )
   grid%tail_statevars => grid%tail_statevars%next
   NULLIFY( grid%tail_statevars%next )
   grid%tail_statevars%ProcOrient = '  '
   grid%tail_statevars%VarName = 'moved'
   grid%tail_statevars%DataName = 'MOVED'
   grid%tail_statevars%Description = '-'
   grid%tail_statevars%Units = '-'
   grid%tail_statevars%Type = 'l'
   grid%tail_statevars%Ntl = 0
   grid%tail_statevars%Restart = .FALSE.
   grid%tail_statevars%Ndim = 0
   grid%tail_statevars%scalar_array = .FALSE.
   grid%tail_statevars%lfield_0d => grid%moved
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 0
  ENDIF
IF ( setinitval .EQ. 3 ) grid%moved=.FALSE.
IF(in_use_for_config(id,'abstot').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((model_config_rec%cam_abs_dim2)-(1)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%abstot(sm31:em31,sm32:em32,1:model_config_rec%cam_abs_dim2,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",11748,&
    'frame/module_domain.f: Failed to allocate grid%abstot(sm31:em31,sm32:em32,1:model_config_rec%cam_abs_dim2,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%abstot=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'abstot'
  grid%tail_statevars%DataName = 'ABSTOT'
  grid%tail_statevars%Description = ''
  grid%tail_statevars%Units = ' '
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XZZ'
  grid%tail_statevars%Stagger = 'Z'
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 4
  grid%tail_statevars%Restart = .FALSE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_4d => grid%abstot
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 0
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = kds
  grid%tail_statevars%ed2 = kde
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = config_flags%cam_abs_dim2
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = kms
  grid%tail_statevars%em2 = kme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = config_flags%cam_abs_dim2
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = kps
  grid%tail_statevars%ep2 = MIN( kde, kpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = config_flags%cam_abs_dim2
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'bottom_top_stag'
  grid%tail_statevars%dimname3 = 'cam_abs_dim2_stag'
  ENDIF
ELSE
  ALLOCATE(grid%abstot(1,1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",11797,&
    'frame/module_domain.f: Failed to allocate grid%abstot(1,1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'absnxt').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((model_config_rec%cam_abs_dim1)-(1)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%absnxt(sm31:em31,sm32:em32,1:model_config_rec%cam_abs_dim1,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",11806,&
    'frame/module_domain.f: Failed to allocate grid%absnxt(sm31:em31,sm32:em32,1:model_config_rec%cam_abs_dim1,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%absnxt=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'absnxt'
  grid%tail_statevars%DataName = 'ABSNXT'
  grid%tail_statevars%Description = ''
  grid%tail_statevars%Units = ' '
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XZC'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 4
  grid%tail_statevars%Restart = .FALSE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_4d => grid%absnxt
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 0
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = kds
  grid%tail_statevars%ed2 = (kde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = config_flags%cam_abs_dim1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = kms
  grid%tail_statevars%em2 = kme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = config_flags%cam_abs_dim1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = kps
  grid%tail_statevars%ep2 = MIN( (kde-1), kpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = config_flags%cam_abs_dim1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'bottom_top'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%absnxt(1,1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",11855,&
    'frame/module_domain.f: Failed to allocate grid%absnxt(1,1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'emstot').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%emstot(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",11864,&
    'frame/module_domain.f: Failed to allocate grid%emstot(sm31:em31,sm32:em32,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%emstot=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'emstot'
  grid%tail_statevars%DataName = 'EMSTOT'
  grid%tail_statevars%Description = ''
  grid%tail_statevars%Units = ' '
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XZY'
  grid%tail_statevars%Stagger = 'Z'
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 3
  grid%tail_statevars%Restart = .FALSE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_3d => grid%emstot
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 0
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = kds
  grid%tail_statevars%ed2 = kde
  grid%tail_statevars%sd3 = jds
  grid%tail_statevars%ed3 = (jde-1)
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = kms
  grid%tail_statevars%em2 = kme
  grid%tail_statevars%sm3 = jms
  grid%tail_statevars%em3 = jme
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = kps
  grid%tail_statevars%ep2 = MIN( kde, kpe )
  grid%tail_statevars%sp3 = jps
  grid%tail_statevars%ep3 = MIN( (jde-1), jpe )
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'bottom_top_stag'
  grid%tail_statevars%dimname3 = 'south_north'
  ENDIF
ELSE
  ALLOCATE(grid%emstot(1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",11914,&
    'frame/module_domain.f: Failed to allocate grid%emstot(1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'dpsdt').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%dpsdt(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",11923,&
    'frame/module_domain.f: Failed to allocate grid%dpsdt(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%dpsdt=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'dpsdt'
  grid%tail_statevars%DataName = 'DPSDT'
  grid%tail_statevars%Description = 'surface pressure tendency'
  grid%tail_statevars%Units = 'Pa/sec'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .FALSE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%dpsdt
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 0
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%dpsdt(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",11973,&
    'frame/module_domain.f: Failed to allocate grid%dpsdt(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'dmudt').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%dmudt(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",11982,&
    'frame/module_domain.f: Failed to allocate grid%dmudt(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%dmudt=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'dmudt'
  grid%tail_statevars%DataName = 'DMUDT'
  grid%tail_statevars%Description = 'mu tendency'
  grid%tail_statevars%Units = 'Pa/sec'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .FALSE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%dmudt
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 0
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%dmudt(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",12032,&
    'frame/module_domain.f: Failed to allocate grid%dmudt(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'pk1m').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%pk1m(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",12041,&
    'frame/module_domain.f: Failed to allocate grid%pk1m(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%pk1m=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'pk1m'
  grid%tail_statevars%DataName = 'PK1M'
  grid%tail_statevars%Description = 'surface pressure at previous step'
  grid%tail_statevars%Units = 'Pa'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .FALSE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%pk1m
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 0
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%pk1m(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",12091,&
    'frame/module_domain.f: Failed to allocate grid%pk1m(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'mu_2m').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%mu_2m(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",12100,&
    'frame/module_domain.f: Failed to allocate grid%mu_2m(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%mu_2m=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'mu_2m'
  grid%tail_statevars%DataName = 'MU_2M'
  grid%tail_statevars%Description = 'mu_2 at previous step'
  grid%tail_statevars%Units = 'Pa'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .FALSE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%mu_2m
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 0
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%mu_2m(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",12150,&
    'frame/module_domain.f: Failed to allocate grid%mu_2m(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'wspd10max').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%wspd10max(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",12159,&
    'frame/module_domain.f: Failed to allocate grid%wspd10max(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%wspd10max=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'wspd10max'
  grid%tail_statevars%DataName = 'WSPD10MAX'
  grid%tail_statevars%Description = 'WIND SPD MAX 10 M'
  grid%tail_statevars%Units = 'm s-1'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%wspd10max
  grid%tail_statevars%streams(1) = 1
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%wspd10max(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",12209,&
    'frame/module_domain.f: Failed to allocate grid%wspd10max(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'w_up_max').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%w_up_max(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",12218,&
    'frame/module_domain.f: Failed to allocate grid%w_up_max(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%w_up_max=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'w_up_max'
  grid%tail_statevars%DataName = 'W_UP_MAX'
  grid%tail_statevars%Description = 'MAX Z-WIND UPDRAFT'
  grid%tail_statevars%Units = 'm s-1'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%w_up_max
  grid%tail_statevars%streams(1) = 1
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%w_up_max(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",12268,&
    'frame/module_domain.f: Failed to allocate grid%w_up_max(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'w_dn_max').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%w_dn_max(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",12277,&
    'frame/module_domain.f: Failed to allocate grid%w_dn_max(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%w_dn_max=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'w_dn_max'
  grid%tail_statevars%DataName = 'W_DN_MAX'
  grid%tail_statevars%Description = 'MAX Z-WIND DOWNDRAFT'
  grid%tail_statevars%Units = 'm s-1'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%w_dn_max
  grid%tail_statevars%streams(1) = 1
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%w_dn_max(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",12327,&
    'frame/module_domain.f: Failed to allocate grid%w_dn_max(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'refd_max').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%refd_max(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",12336,&
    'frame/module_domain.f: Failed to allocate grid%refd_max(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%refd_max=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'refd_max'
  grid%tail_statevars%DataName = 'REFD_MAX'
  grid%tail_statevars%Description = 'MAX DERIVED RADAR REFL'
  grid%tail_statevars%Units = 'dbZ'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%refd_max
  grid%tail_statevars%streams(1) = 1
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%refd_max(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",12386,&
    'frame/module_domain.f: Failed to allocate grid%refd_max(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'up_heli_max').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%up_heli_max(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",12395,&
    'frame/module_domain.f: Failed to allocate grid%up_heli_max(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%up_heli_max=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'up_heli_max'
  grid%tail_statevars%DataName = 'UP_HELI_MAX'
  grid%tail_statevars%Description = 'MAX UPDRAFT HELICITY'
  grid%tail_statevars%Units = 'm2 s-2'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%up_heli_max
  grid%tail_statevars%streams(1) = 1
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%up_heli_max(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",12445,&
    'frame/module_domain.f: Failed to allocate grid%up_heli_max(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'w_mean').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%w_mean(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",12454,&
    'frame/module_domain.f: Failed to allocate grid%w_mean(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%w_mean=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'w_mean'
  grid%tail_statevars%DataName = 'W_MEAN'
  grid%tail_statevars%Description = 'HOURLY MEAN Z-WIND'
  grid%tail_statevars%Units = 'm s-1'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%w_mean
  grid%tail_statevars%streams(1) = 1
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%w_mean(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",12504,&
    'frame/module_domain.f: Failed to allocate grid%w_mean(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'grpl_max').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%grpl_max(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",12513,&
    'frame/module_domain.f: Failed to allocate grid%grpl_max(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%grpl_max=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'grpl_max'
  grid%tail_statevars%DataName = 'GRPL_MAX'
  grid%tail_statevars%Description = 'MAX COL INT GRAUPEL'
  grid%tail_statevars%Units = 'kg m-2'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%grpl_max
  grid%tail_statevars%streams(1) = 1
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%grpl_max(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",12563,&
    'frame/module_domain.f: Failed to allocate grid%grpl_max(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'uh').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%uh(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",12572,&
    'frame/module_domain.f: Failed to allocate grid%uh(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%uh=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'uh'
  grid%tail_statevars%DataName = 'UH'
  grid%tail_statevars%Description = 'UPDRAFT HELICITY'
  grid%tail_statevars%Units = 'm2 s-2'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%uh
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%uh(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",12622,&
    'frame/module_domain.f: Failed to allocate grid%uh(1,1).  ')
  endif
ENDIF
  IF (.NOT.grid%is_intermediate) THEN
   ALLOCATE( grid%tail_statevars%next )
   grid%tail_statevars => grid%tail_statevars%next
   NULLIFY( grid%tail_statevars%next )
   grid%tail_statevars%ProcOrient = '  '
   grid%tail_statevars%VarName = 'max_cfl'
   grid%tail_statevars%DataName = 'MAX_CFL'
   grid%tail_statevars%Description = 'maximum CFL value in grid at a time'
   grid%tail_statevars%Units = '-'
   grid%tail_statevars%Type = 'r'
   grid%tail_statevars%Ntl = 0
   grid%tail_statevars%Restart = .FALSE.
   grid%tail_statevars%Ndim = 0
   grid%tail_statevars%scalar_array = .FALSE.
   grid%tail_statevars%rfield_0d => grid%max_cfl
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 0
  ENDIF
IF ( setinitval .EQ. 3 ) grid%max_cfl=initial_data_value
IF(in_use_for_config(id,'prec_acc_c').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%prec_acc_c(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",12650,&
    'frame/module_domain.f: Failed to allocate grid%prec_acc_c(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%prec_acc_c=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'prec_acc_c'
  grid%tail_statevars%DataName = 'PREC_ACC_C'
  grid%tail_statevars%Description = 'ACCUMULATED CUMULUS PRECIPITATION OVER prec_acc_dt PERIODS OF TIME'
  grid%tail_statevars%Units = 'mm'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%prec_acc_c
  grid%tail_statevars%streams(1) = 1
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%prec_acc_c(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",12700,&
    'frame/module_domain.f: Failed to allocate grid%prec_acc_c(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'prec_acc_nc').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%prec_acc_nc(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",12709,&
    'frame/module_domain.f: Failed to allocate grid%prec_acc_nc(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%prec_acc_nc=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'prec_acc_nc'
  grid%tail_statevars%DataName = 'PREC_ACC_NC'
  grid%tail_statevars%Description = 'ACCUMULATED GRID SCALE  PRECIPITATION OVER prec_acc_dt PERIODS OF TIME'
  grid%tail_statevars%Units = 'mm'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%prec_acc_nc
  grid%tail_statevars%streams(1) = 1
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%prec_acc_nc(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",12759,&
    'frame/module_domain.f: Failed to allocate grid%prec_acc_nc(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'snow_acc_nc').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%snow_acc_nc(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",12768,&
    'frame/module_domain.f: Failed to allocate grid%snow_acc_nc(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%snow_acc_nc=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'snow_acc_nc'
  grid%tail_statevars%DataName = 'SNOW_ACC_NC'
  grid%tail_statevars%Description = 'ACCUMULATED SNOW WATER EQUIVALENT OVER prec_acc_dt PERIODS OF TIME'
  grid%tail_statevars%Units = 'mm'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%snow_acc_nc
  grid%tail_statevars%streams(1) = 1
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%snow_acc_nc(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",12818,&
    'frame/module_domain.f: Failed to allocate grid%snow_acc_nc(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'advh_t'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1)*num_advh_t)) * 4
  ALLOCATE(grid%advh_t(sm31:em31,sm32:em32,sm33:em33,num_advh_t),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",12827,&
    'frame/module_domain.f: Failed to allocate grid%advh_t(sm31:em31,sm32:em32,sm33:em33,num_advh_t). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%advh_t=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'advh_t'
  grid%tail_statevars%DataName = 'ADVH_T'
  grid%tail_statevars%Description = '-'
  grid%tail_statevars%Units = '-'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XZY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 4
  grid%tail_statevars%Restart = .FALSE.
  grid%tail_statevars%scalar_array = .TRUE.
  grid%tail_statevars%rfield_4d => grid%advh_t
  grid%tail_statevars%num_table => advh_t_num_table
  grid%tail_statevars%index_table => advh_t_index_table
  grid%tail_statevars%boundary_table => advh_t_boundary_table
  grid%tail_statevars%dname_table => advh_t_dname_table
  grid%tail_statevars%desc_table => advh_t_desc_table
  grid%tail_statevars%units_table => advh_t_units_table
  grid%tail_statevars%streams_table => advh_t_streams_table
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 0
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = kds
  grid%tail_statevars%ed2 = (kde-1)
  grid%tail_statevars%sd3 = jds
  grid%tail_statevars%ed3 = (jde-1)
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = kms
  grid%tail_statevars%em2 = kme
  grid%tail_statevars%sm3 = jms
  grid%tail_statevars%em3 = jme
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = kps
  grid%tail_statevars%ep2 = MIN( (kde-1), kpe )
  grid%tail_statevars%sp3 = jps
  grid%tail_statevars%ep3 = MIN( (jde-1), jpe )
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'bottom_top'
  grid%tail_statevars%dimname3 = 'south_north'
  ENDIF
ELSE
  ALLOCATE(grid%advh_t(1,1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",12884,&
    'frame/module_domain.f: Failed to allocate grid%advh_t(1,1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'advz_t'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em32)-(sm32)+1))*(((em33)-(sm33)+1)*num_advz_t)) * 4
  ALLOCATE(grid%advz_t(sm31:em31,sm32:em32,sm33:em33,num_advz_t),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",12893,&
    'frame/module_domain.f: Failed to allocate grid%advz_t(sm31:em31,sm32:em32,sm33:em33,num_advz_t). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%advz_t=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'advz_t'
  grid%tail_statevars%DataName = 'ADVZ_T'
  grid%tail_statevars%Description = '-'
  grid%tail_statevars%Units = '-'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XZY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 4
  grid%tail_statevars%Restart = .FALSE.
  grid%tail_statevars%scalar_array = .TRUE.
  grid%tail_statevars%rfield_4d => grid%advz_t
  grid%tail_statevars%num_table => advz_t_num_table
  grid%tail_statevars%index_table => advz_t_index_table
  grid%tail_statevars%boundary_table => advz_t_boundary_table
  grid%tail_statevars%dname_table => advz_t_dname_table
  grid%tail_statevars%desc_table => advz_t_desc_table
  grid%tail_statevars%units_table => advz_t_units_table
  grid%tail_statevars%streams_table => advz_t_streams_table
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 0
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = kds
  grid%tail_statevars%ed2 = (kde-1)
  grid%tail_statevars%sd3 = jds
  grid%tail_statevars%ed3 = (jde-1)
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = kms
  grid%tail_statevars%em2 = kme
  grid%tail_statevars%sm3 = jms
  grid%tail_statevars%em3 = jme
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = kps
  grid%tail_statevars%ep2 = MIN( (kde-1), kpe )
  grid%tail_statevars%sp3 = jps
  grid%tail_statevars%ep3 = MIN( (jde-1), jpe )
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'bottom_top'
  grid%tail_statevars%dimname3 = 'south_north'
  ENDIF
ELSE
  ALLOCATE(grid%advz_t(1,1,1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",12950,&
    'frame/module_domain.f: Failed to allocate grid%advz_t(1,1,1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'tml'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%tml(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",12959,&
    'frame/module_domain.f: Failed to allocate grid%tml(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tml=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'tml'
  grid%tail_statevars%DataName = 'TML'
  grid%tail_statevars%Description = 'OCEAN MIXED-LAYER TEMPERATURE'
  grid%tail_statevars%Units = 'K'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%tml
  grid%tail_statevars%streams(1) = 1
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%tml(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",13009,&
    'frame/module_domain.f: Failed to allocate grid%tml(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'t0ml'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%t0ml(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",13018,&
    'frame/module_domain.f: Failed to allocate grid%t0ml(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%t0ml=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 't0ml'
  grid%tail_statevars%DataName = 'T0ML'
  grid%tail_statevars%Description = 'INITIAL OCEAN MIXED-LAYER TEMPERATURE'
  grid%tail_statevars%Units = 'K'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%t0ml
  grid%tail_statevars%streams(1) = 1
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%t0ml(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",13068,&
    'frame/module_domain.f: Failed to allocate grid%t0ml(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'hml'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%hml(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",13077,&
    'frame/module_domain.f: Failed to allocate grid%hml(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%hml=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'hml'
  grid%tail_statevars%DataName = 'HML'
  grid%tail_statevars%Description = 'OCEAN MIXED-LAYER DEPTH'
  grid%tail_statevars%Units = 'm'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%hml
  grid%tail_statevars%streams(1) = 1
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%hml(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",13127,&
    'frame/module_domain.f: Failed to allocate grid%hml(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'h0ml'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%h0ml(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",13136,&
    'frame/module_domain.f: Failed to allocate grid%h0ml(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%h0ml=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'h0ml'
  grid%tail_statevars%DataName = 'H0ML'
  grid%tail_statevars%Description = 'INITIAL OCEAN MIXED-LAYER DEPTH'
  grid%tail_statevars%Units = 'm'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%h0ml
  grid%tail_statevars%streams(1) = 234881025
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%h0ml(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",13186,&
    'frame/module_domain.f: Failed to allocate grid%h0ml(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'huml'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%huml(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",13195,&
    'frame/module_domain.f: Failed to allocate grid%huml(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%huml=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'huml'
  grid%tail_statevars%DataName = 'HUML'
  grid%tail_statevars%Description = 'OCEAN MIXED-LAYER DEPTH * U-CURRENT'
  grid%tail_statevars%Units = ' m2s-1 '
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%huml
  grid%tail_statevars%streams(1) = 1
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%huml(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",13245,&
    'frame/module_domain.f: Failed to allocate grid%huml(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'hvml'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%hvml(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",13254,&
    'frame/module_domain.f: Failed to allocate grid%hvml(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%hvml=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'hvml'
  grid%tail_statevars%DataName = 'HVML'
  grid%tail_statevars%Description = 'OCEAN MIXED-LAYER DEPTH * V-CURRENT'
  grid%tail_statevars%Units = ' m2s-1 '
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%hvml
  grid%tail_statevars%streams(1) = 1
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%hvml(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",13304,&
    'frame/module_domain.f: Failed to allocate grid%hvml(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'tmoml'))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((em31)-(sm31)+1))*(((em33)-(sm33)+1))) * 4
  ALLOCATE(grid%tmoml(sm31:em31,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",13313,&
    'frame/module_domain.f: Failed to allocate grid%tmoml(sm31:em31,sm33:em33). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%tmoml=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'tmoml'
  grid%tail_statevars%DataName = 'TMOML'
  grid%tail_statevars%Description = 'OCEAN LAYER MEAN TEMPERATURE   '
  grid%tail_statevars%Units = 'K'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'XY'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .TRUE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%tmoml
  grid%tail_statevars%streams(1) = 234881025
  grid%tail_statevars%streams(2) = 2097152
  grid%tail_statevars%sd1 = ids
  grid%tail_statevars%ed1 = (ide-1)
  grid%tail_statevars%sd2 = jds
  grid%tail_statevars%ed2 = (jde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = ims
  grid%tail_statevars%em1 = ime
  grid%tail_statevars%sm2 = jms
  grid%tail_statevars%em2 = jme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = ips
  grid%tail_statevars%ep1 = MIN( (ide-1), ipe )
  grid%tail_statevars%sp2 = jps
  grid%tail_statevars%ep2 = MIN( (jde-1), jpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%subgrid_x = .FALSE.
  grid%tail_statevars%subgrid_y = .FALSE.
  grid%tail_statevars%dimname1 = 'west_east'
  grid%tail_statevars%dimname2 = 'south_north'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%tmoml(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",13363,&
    'frame/module_domain.f: Failed to allocate grid%tmoml(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'track_z').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((model_config_rec%track_loc_in)-(1)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%track_z(1:model_config_rec%track_loc_in,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",13372,&
    'frame/module_domain.f: Failed to allocate grid%track_z(1:model_config_rec%track_loc_in,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%track_z=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'track_z'
  grid%tail_statevars%DataName = 'TRACK_Z'
  grid%tail_statevars%Description = 'mid-level Height'
  grid%tail_statevars%Units = 'm'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'CZ'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .FALSE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%track_z
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 0
  grid%tail_statevars%sd1 = 1
  grid%tail_statevars%ed1 = config_flags%track_loc_in
  grid%tail_statevars%sd2 = kds
  grid%tail_statevars%ed2 = (kde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = 1
  grid%tail_statevars%em1 = config_flags%track_loc_in
  grid%tail_statevars%sm2 = kms
  grid%tail_statevars%em2 = kme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = 1
  grid%tail_statevars%ep1 = config_flags%track_loc_in
  grid%tail_statevars%sp2 = kps
  grid%tail_statevars%ep2 = MIN( (kde-1), kpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%dimname1 = ''
  grid%tail_statevars%dimname2 = 'bottom_top'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%track_z(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",13420,&
    'frame/module_domain.f: Failed to allocate grid%track_z(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'track_t').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((model_config_rec%track_loc_in)-(1)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%track_t(1:model_config_rec%track_loc_in,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",13429,&
    'frame/module_domain.f: Failed to allocate grid%track_t(1:model_config_rec%track_loc_in,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%track_t=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'track_t'
  grid%tail_statevars%DataName = 'TRACK_T'
  grid%tail_statevars%Description = 'mid-level temperature'
  grid%tail_statevars%Units = 'K'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'CZ'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .FALSE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%track_t
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 0
  grid%tail_statevars%sd1 = 1
  grid%tail_statevars%ed1 = config_flags%track_loc_in
  grid%tail_statevars%sd2 = kds
  grid%tail_statevars%ed2 = (kde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = 1
  grid%tail_statevars%em1 = config_flags%track_loc_in
  grid%tail_statevars%sm2 = kms
  grid%tail_statevars%em2 = kme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = 1
  grid%tail_statevars%ep1 = config_flags%track_loc_in
  grid%tail_statevars%sp2 = kps
  grid%tail_statevars%ep2 = MIN( (kde-1), kpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%dimname1 = ''
  grid%tail_statevars%dimname2 = 'bottom_top'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%track_t(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",13477,&
    'frame/module_domain.f: Failed to allocate grid%track_t(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'track_p').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((model_config_rec%track_loc_in)-(1)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%track_p(1:model_config_rec%track_loc_in,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",13486,&
    'frame/module_domain.f: Failed to allocate grid%track_p(1:model_config_rec%track_loc_in,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%track_p=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'track_p'
  grid%tail_statevars%DataName = 'TRACK_P'
  grid%tail_statevars%Description = 'mid-level pressure'
  grid%tail_statevars%Units = 'Pa'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'CZ'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .FALSE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%track_p
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 0
  grid%tail_statevars%sd1 = 1
  grid%tail_statevars%ed1 = config_flags%track_loc_in
  grid%tail_statevars%sd2 = kds
  grid%tail_statevars%ed2 = (kde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = 1
  grid%tail_statevars%em1 = config_flags%track_loc_in
  grid%tail_statevars%sm2 = kms
  grid%tail_statevars%em2 = kme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = 1
  grid%tail_statevars%ep1 = config_flags%track_loc_in
  grid%tail_statevars%sp2 = kps
  grid%tail_statevars%ep2 = MIN( (kde-1), kpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%dimname1 = ''
  grid%tail_statevars%dimname2 = 'bottom_top'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%track_p(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",13534,&
    'frame/module_domain.f: Failed to allocate grid%track_p(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'track_u').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((model_config_rec%track_loc_in)-(1)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%track_u(1:model_config_rec%track_loc_in,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",13543,&
    'frame/module_domain.f: Failed to allocate grid%track_u(1:model_config_rec%track_loc_in,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%track_u=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'track_u'
  grid%tail_statevars%DataName = 'TRACK_U'
  grid%tail_statevars%Description = 'x-wind component'
  grid%tail_statevars%Units = 'm s-1'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'CZ'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .FALSE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%track_u
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 0
  grid%tail_statevars%sd1 = 1
  grid%tail_statevars%ed1 = config_flags%track_loc_in
  grid%tail_statevars%sd2 = kds
  grid%tail_statevars%ed2 = (kde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = 1
  grid%tail_statevars%em1 = config_flags%track_loc_in
  grid%tail_statevars%sm2 = kms
  grid%tail_statevars%em2 = kme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = 1
  grid%tail_statevars%ep1 = config_flags%track_loc_in
  grid%tail_statevars%sp2 = kps
  grid%tail_statevars%ep2 = MIN( (kde-1), kpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%dimname1 = ''
  grid%tail_statevars%dimname2 = 'bottom_top'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%track_u(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",13591,&
    'frame/module_domain.f: Failed to allocate grid%track_u(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'track_v').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((model_config_rec%track_loc_in)-(1)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%track_v(1:model_config_rec%track_loc_in,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",13600,&
    'frame/module_domain.f: Failed to allocate grid%track_v(1:model_config_rec%track_loc_in,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%track_v=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'track_v'
  grid%tail_statevars%DataName = 'TRACK_V'
  grid%tail_statevars%Description = 'y-wind component'
  grid%tail_statevars%Units = 'm s-1'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'CZ'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .FALSE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%track_v
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 0
  grid%tail_statevars%sd1 = 1
  grid%tail_statevars%ed1 = config_flags%track_loc_in
  grid%tail_statevars%sd2 = kds
  grid%tail_statevars%ed2 = (kde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = 1
  grid%tail_statevars%em1 = config_flags%track_loc_in
  grid%tail_statevars%sm2 = kms
  grid%tail_statevars%em2 = kme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = 1
  grid%tail_statevars%ep1 = config_flags%track_loc_in
  grid%tail_statevars%sp2 = kps
  grid%tail_statevars%ep2 = MIN( (kde-1), kpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%dimname1 = ''
  grid%tail_statevars%dimname2 = 'bottom_top'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%track_v(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",13648,&
    'frame/module_domain.f: Failed to allocate grid%track_v(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'track_w').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((model_config_rec%track_loc_in)-(1)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%track_w(1:model_config_rec%track_loc_in,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",13657,&
    'frame/module_domain.f: Failed to allocate grid%track_w(1:model_config_rec%track_loc_in,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%track_w=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'track_w'
  grid%tail_statevars%DataName = 'TRACK_W'
  grid%tail_statevars%Description = 'z-wind component'
  grid%tail_statevars%Units = 'm s-1'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'CZ'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .FALSE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%track_w
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 0
  grid%tail_statevars%sd1 = 1
  grid%tail_statevars%ed1 = config_flags%track_loc_in
  grid%tail_statevars%sd2 = kds
  grid%tail_statevars%ed2 = (kde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = 1
  grid%tail_statevars%em1 = config_flags%track_loc_in
  grid%tail_statevars%sm2 = kms
  grid%tail_statevars%em2 = kme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = 1
  grid%tail_statevars%ep1 = config_flags%track_loc_in
  grid%tail_statevars%sp2 = kps
  grid%tail_statevars%ep2 = MIN( (kde-1), kpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%dimname1 = ''
  grid%tail_statevars%dimname2 = 'bottom_top'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%track_w(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",13705,&
    'frame/module_domain.f: Failed to allocate grid%track_w(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'track_rh').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((model_config_rec%track_loc_in)-(1)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%track_rh(1:model_config_rec%track_loc_in,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",13714,&
    'frame/module_domain.f: Failed to allocate grid%track_rh(1:model_config_rec%track_loc_in,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%track_rh=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'track_rh'
  grid%tail_statevars%DataName = 'TRACK_RH'
  grid%tail_statevars%Description = 'relative humidity'
  grid%tail_statevars%Units = '0 - 1 fraction'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'CZ'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .FALSE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%track_rh
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 0
  grid%tail_statevars%sd1 = 1
  grid%tail_statevars%ed1 = config_flags%track_loc_in
  grid%tail_statevars%sd2 = kds
  grid%tail_statevars%ed2 = (kde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = 1
  grid%tail_statevars%em1 = config_flags%track_loc_in
  grid%tail_statevars%sm2 = kms
  grid%tail_statevars%em2 = kme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = 1
  grid%tail_statevars%ep1 = config_flags%track_loc_in
  grid%tail_statevars%sp2 = kps
  grid%tail_statevars%ep2 = MIN( (kde-1), kpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%dimname1 = ''
  grid%tail_statevars%dimname2 = 'bottom_top'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%track_rh(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",13762,&
    'frame/module_domain.f: Failed to allocate grid%track_rh(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'track_alt').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((model_config_rec%track_loc_in)-(1)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%track_alt(1:model_config_rec%track_loc_in,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",13771,&
    'frame/module_domain.f: Failed to allocate grid%track_alt(1:model_config_rec%track_loc_in,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%track_alt=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'track_alt'
  grid%tail_statevars%DataName = 'TRACK_ALT'
  grid%tail_statevars%Description = 'inverse density'
  grid%tail_statevars%Units = 'm3 kg-1'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'CZ'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .FALSE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%track_alt
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 0
  grid%tail_statevars%sd1 = 1
  grid%tail_statevars%ed1 = config_flags%track_loc_in
  grid%tail_statevars%sd2 = kds
  grid%tail_statevars%ed2 = (kde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = 1
  grid%tail_statevars%em1 = config_flags%track_loc_in
  grid%tail_statevars%sm2 = kms
  grid%tail_statevars%em2 = kme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = 1
  grid%tail_statevars%ep1 = config_flags%track_loc_in
  grid%tail_statevars%sp2 = kps
  grid%tail_statevars%ep2 = MIN( (kde-1), kpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%dimname1 = ''
  grid%tail_statevars%dimname2 = 'bottom_top'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%track_alt(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",13819,&
    'frame/module_domain.f: Failed to allocate grid%track_alt(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'track_ele').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((model_config_rec%track_loc_in)-(1)+1))) * 4
  ALLOCATE(grid%track_ele(1:model_config_rec%track_loc_in),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",13828,&
    'frame/module_domain.f: Failed to allocate grid%track_ele(1:model_config_rec%track_loc_in). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%track_ele=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'track_ele'
  grid%tail_statevars%DataName = 'TRACK_ELE'
  grid%tail_statevars%Description = 'elevation'
  grid%tail_statevars%Units = 'm'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'C'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 1
  grid%tail_statevars%Restart = .FALSE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_1d => grid%track_ele
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 0
  grid%tail_statevars%sd1 = 1
  grid%tail_statevars%ed1 = config_flags%track_loc_in
  grid%tail_statevars%sd2 = 1
  grid%tail_statevars%ed2 = 1
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = 1
  grid%tail_statevars%em1 = config_flags%track_loc_in
  grid%tail_statevars%sm2 = 1
  grid%tail_statevars%em2 = 1
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = 1
  grid%tail_statevars%ep1 = config_flags%track_loc_in
  grid%tail_statevars%sp2 = 1
  grid%tail_statevars%ep2 = 1
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%dimname1 = ''
  grid%tail_statevars%dimname2 = ''
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%track_ele(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",13876,&
    'frame/module_domain.f: Failed to allocate grid%track_ele(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'track_aircraft').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((model_config_rec%track_loc_in)-(1)+1))) * 4
  ALLOCATE(grid%track_aircraft(1:model_config_rec%track_loc_in),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",13885,&
    'frame/module_domain.f: Failed to allocate grid%track_aircraft(1:model_config_rec%track_loc_in). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%track_aircraft=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'track_aircraft'
  grid%tail_statevars%DataName = 'TRACK_AIRCRAFT'
  grid%tail_statevars%Description = 'aircraft altitude'
  grid%tail_statevars%Units = 'm'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'C'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 1
  grid%tail_statevars%Restart = .FALSE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_1d => grid%track_aircraft
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 0
  grid%tail_statevars%sd1 = 1
  grid%tail_statevars%ed1 = config_flags%track_loc_in
  grid%tail_statevars%sd2 = 1
  grid%tail_statevars%ed2 = 1
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = 1
  grid%tail_statevars%em1 = config_flags%track_loc_in
  grid%tail_statevars%sm2 = 1
  grid%tail_statevars%em2 = 1
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = 1
  grid%tail_statevars%ep1 = config_flags%track_loc_in
  grid%tail_statevars%sp2 = 1
  grid%tail_statevars%ep2 = 1
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%dimname1 = ''
  grid%tail_statevars%dimname2 = ''
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%track_aircraft(1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",13933,&
    'frame/module_domain.f: Failed to allocate grid%track_aircraft(1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'track_qcloud').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((model_config_rec%track_loc_in)-(1)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%track_qcloud(1:model_config_rec%track_loc_in,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",13942,&
    'frame/module_domain.f: Failed to allocate grid%track_qcloud(1:model_config_rec%track_loc_in,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%track_qcloud=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'track_qcloud'
  grid%tail_statevars%DataName = 'TRACK_QCLOUD'
  grid%tail_statevars%Description = 'cloud water mixing ratio'
  grid%tail_statevars%Units = 'kg kg-1'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'CZ'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .FALSE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%track_qcloud
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 0
  grid%tail_statevars%sd1 = 1
  grid%tail_statevars%ed1 = config_flags%track_loc_in
  grid%tail_statevars%sd2 = kds
  grid%tail_statevars%ed2 = (kde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = 1
  grid%tail_statevars%em1 = config_flags%track_loc_in
  grid%tail_statevars%sm2 = kms
  grid%tail_statevars%em2 = kme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = 1
  grid%tail_statevars%ep1 = config_flags%track_loc_in
  grid%tail_statevars%sp2 = kps
  grid%tail_statevars%ep2 = MIN( (kde-1), kpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%dimname1 = ''
  grid%tail_statevars%dimname2 = 'bottom_top'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%track_qcloud(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",13990,&
    'frame/module_domain.f: Failed to allocate grid%track_qcloud(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'track_qrain').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((model_config_rec%track_loc_in)-(1)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%track_qrain(1:model_config_rec%track_loc_in,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",13999,&
    'frame/module_domain.f: Failed to allocate grid%track_qrain(1:model_config_rec%track_loc_in,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%track_qrain=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'track_qrain'
  grid%tail_statevars%DataName = 'TRACK_QRAIN'
  grid%tail_statevars%Description = 'rain water mixing ratio'
  grid%tail_statevars%Units = 'kg kg-1'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'CZ'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .FALSE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%track_qrain
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 0
  grid%tail_statevars%sd1 = 1
  grid%tail_statevars%ed1 = config_flags%track_loc_in
  grid%tail_statevars%sd2 = kds
  grid%tail_statevars%ed2 = (kde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = 1
  grid%tail_statevars%em1 = config_flags%track_loc_in
  grid%tail_statevars%sm2 = kms
  grid%tail_statevars%em2 = kme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = 1
  grid%tail_statevars%ep1 = config_flags%track_loc_in
  grid%tail_statevars%sp2 = kps
  grid%tail_statevars%ep2 = MIN( (kde-1), kpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%dimname1 = ''
  grid%tail_statevars%dimname2 = 'bottom_top'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%track_qrain(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",14047,&
    'frame/module_domain.f: Failed to allocate grid%track_qrain(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'track_qice').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((model_config_rec%track_loc_in)-(1)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%track_qice(1:model_config_rec%track_loc_in,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",14056,&
    'frame/module_domain.f: Failed to allocate grid%track_qice(1:model_config_rec%track_loc_in,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%track_qice=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'track_qice'
  grid%tail_statevars%DataName = 'TRACK_QICE'
  grid%tail_statevars%Description = 'ice mixing ratio'
  grid%tail_statevars%Units = 'kg kg-1'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'CZ'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .FALSE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%track_qice
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 0
  grid%tail_statevars%sd1 = 1
  grid%tail_statevars%ed1 = config_flags%track_loc_in
  grid%tail_statevars%sd2 = kds
  grid%tail_statevars%ed2 = (kde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = 1
  grid%tail_statevars%em1 = config_flags%track_loc_in
  grid%tail_statevars%sm2 = kms
  grid%tail_statevars%em2 = kme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = 1
  grid%tail_statevars%ep1 = config_flags%track_loc_in
  grid%tail_statevars%sp2 = kps
  grid%tail_statevars%ep2 = MIN( (kde-1), kpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%dimname1 = ''
  grid%tail_statevars%dimname2 = 'bottom_top'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%track_qice(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",14104,&
    'frame/module_domain.f: Failed to allocate grid%track_qice(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'track_qsnow').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((model_config_rec%track_loc_in)-(1)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%track_qsnow(1:model_config_rec%track_loc_in,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",14113,&
    'frame/module_domain.f: Failed to allocate grid%track_qsnow(1:model_config_rec%track_loc_in,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%track_qsnow=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'track_qsnow'
  grid%tail_statevars%DataName = 'TRACK_QSNOW'
  grid%tail_statevars%Description = 'snow mixing ratio'
  grid%tail_statevars%Units = 'kg kg-1'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'CZ'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .FALSE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%track_qsnow
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 0
  grid%tail_statevars%sd1 = 1
  grid%tail_statevars%ed1 = config_flags%track_loc_in
  grid%tail_statevars%sd2 = kds
  grid%tail_statevars%ed2 = (kde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = 1
  grid%tail_statevars%em1 = config_flags%track_loc_in
  grid%tail_statevars%sm2 = kms
  grid%tail_statevars%em2 = kme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = 1
  grid%tail_statevars%ep1 = config_flags%track_loc_in
  grid%tail_statevars%sp2 = kps
  grid%tail_statevars%ep2 = MIN( (kde-1), kpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%dimname1 = ''
  grid%tail_statevars%dimname2 = 'bottom_top'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%track_qsnow(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",14161,&
    'frame/module_domain.f: Failed to allocate grid%track_qsnow(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'track_qgraup').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((model_config_rec%track_loc_in)-(1)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%track_qgraup(1:model_config_rec%track_loc_in,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",14170,&
    'frame/module_domain.f: Failed to allocate grid%track_qgraup(1:model_config_rec%track_loc_in,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%track_qgraup=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'track_qgraup'
  grid%tail_statevars%DataName = 'TRACK_QGRAUP'
  grid%tail_statevars%Description = 'graupel mixing ratio'
  grid%tail_statevars%Units = 'kg kg-1'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'CZ'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .FALSE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%track_qgraup
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 0
  grid%tail_statevars%sd1 = 1
  grid%tail_statevars%ed1 = config_flags%track_loc_in
  grid%tail_statevars%sd2 = kds
  grid%tail_statevars%ed2 = (kde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = 1
  grid%tail_statevars%em1 = config_flags%track_loc_in
  grid%tail_statevars%sm2 = kms
  grid%tail_statevars%em2 = kme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = 1
  grid%tail_statevars%ep1 = config_flags%track_loc_in
  grid%tail_statevars%sp2 = kps
  grid%tail_statevars%ep2 = MIN( (kde-1), kpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%dimname1 = ''
  grid%tail_statevars%dimname2 = 'bottom_top'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%track_qgraup(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",14218,&
    'frame/module_domain.f: Failed to allocate grid%track_qgraup(1,1).  ')
  endif
ENDIF
IF(in_use_for_config(id,'track_qvapor').AND.(.NOT.grid%is_intermediate))THEN
  num_bytes_allocated = num_bytes_allocated + &
((((model_config_rec%track_loc_in)-(1)+1))*(((em32)-(sm32)+1))) * 4
  ALLOCATE(grid%track_qvapor(1:model_config_rec%track_loc_in,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",14227,&
    'frame/module_domain.f: Failed to allocate grid%track_qvapor(1:model_config_rec%track_loc_in,sm32:em32). ')
  endif
  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) grid%track_qvapor=initial_data_value
  IF (.NOT.grid%is_intermediate) THEN
  ALLOCATE( grid%tail_statevars%next )
  grid%tail_statevars => grid%tail_statevars%next
  NULLIFY( grid%tail_statevars%next )
  grid%tail_statevars%VarName = 'track_qvapor'
  grid%tail_statevars%DataName = 'TRACK_QVAPOR'
  grid%tail_statevars%Description = 'water vapor mixing ratio'
  grid%tail_statevars%Units = 'kg kg-1'
  grid%tail_statevars%Type = 'r'
  grid%tail_statevars%ProcOrient = ' '
  grid%tail_statevars%MemoryOrder = 'CZ'
  grid%tail_statevars%Stagger = ''
  grid%tail_statevars%Ntl = 0
  grid%tail_statevars%Ndim = 2
  grid%tail_statevars%Restart = .FALSE.
  grid%tail_statevars%scalar_array = .FALSE.
  grid%tail_statevars%rfield_2d => grid%track_qvapor
  grid%tail_statevars%streams(1) = 0
  grid%tail_statevars%streams(2) = 0
  grid%tail_statevars%sd1 = 1
  grid%tail_statevars%ed1 = config_flags%track_loc_in
  grid%tail_statevars%sd2 = kds
  grid%tail_statevars%ed2 = (kde-1)
  grid%tail_statevars%sd3 = 1
  grid%tail_statevars%ed3 = 1
  grid%tail_statevars%sm1 = 1
  grid%tail_statevars%em1 = config_flags%track_loc_in
  grid%tail_statevars%sm2 = kms
  grid%tail_statevars%em2 = kme
  grid%tail_statevars%sm3 = 1
  grid%tail_statevars%em3 = 1
  grid%tail_statevars%sp1 = 1
  grid%tail_statevars%ep1 = config_flags%track_loc_in
  grid%tail_statevars%sp2 = kps
  grid%tail_statevars%ep2 = MIN( (kde-1), kpe )
  grid%tail_statevars%sp3 = 1
  grid%tail_statevars%ep3 = 1
  grid%tail_statevars%dimname1 = ''
  grid%tail_statevars%dimname2 = 'bottom_top'
  grid%tail_statevars%dimname3 = ''
  ENDIF
ELSE
  ALLOCATE(grid%track_qvapor(1,1),STAT=ierr)
  if (ierr.ne.0) then
    CALL wrf_error_fatal3("<stdin>",14275,&
    'frame/module_domain.f: Failed to allocate grid%track_qvapor(1,1).  ')
  endif
ENDIF
IF ( setinitval .EQ. 3 ) grid%run_days=0
IF ( setinitval .EQ. 3 ) grid%run_hours=0
IF ( setinitval .EQ. 3 ) grid%run_minutes=0
IF ( setinitval .EQ. 3 ) grid%run_seconds=0
IF ( setinitval .EQ. 3 ) grid%start_year=0
IF ( setinitval .EQ. 3 ) grid%start_month=0
IF ( setinitval .EQ. 3 ) grid%start_day=0
IF ( setinitval .EQ. 3 ) grid%start_hour=0
IF ( setinitval .EQ. 3 ) grid%start_minute=0
IF ( setinitval .EQ. 3 ) grid%start_second=0
IF ( setinitval .EQ. 3 ) grid%end_year=0
IF ( setinitval .EQ. 3 ) grid%end_month=0
IF ( setinitval .EQ. 3 ) grid%end_day=0
IF ( setinitval .EQ. 3 ) grid%end_hour=0
IF ( setinitval .EQ. 3 ) grid%end_minute=0
IF ( setinitval .EQ. 3 ) grid%end_second=0
IF ( setinitval .EQ. 3 ) grid%interval_seconds=0
IF ( setinitval .EQ. 3 ) grid%input_from_file=.FALSE.
IF ( setinitval .EQ. 3 ) grid%fine_input_stream=0
IF ( setinitval .EQ. 3 ) grid%input_from_hires=.FALSE.
IF ( setinitval .EQ. 3 ) grid%all_ic_times=.FALSE.
IF ( setinitval .EQ. 3 ) grid%julyr=0
IF ( setinitval .EQ. 3 ) grid%julday=0
IF ( setinitval .EQ. 3 ) grid%gmt=initial_data_value
IF ( setinitval .EQ. 3 ) grid%write_input=.FALSE.
IF ( setinitval .EQ. 3 ) grid%write_restart_at_0h=.FALSE.
IF ( setinitval .EQ. 3 ) grid%write_hist_at_0h_rst=.FALSE.
IF ( setinitval .EQ. 3 ) grid%adjust_output_times=.FALSE.
IF ( setinitval .EQ. 3 ) grid%adjust_input_times=.FALSE.
IF ( setinitval .EQ. 3 ) grid%diag_print=0
IF ( setinitval .EQ. 3 ) grid%nocolons=.FALSE.
IF ( setinitval .EQ. 3 ) grid%cycling=.FALSE.
IF ( setinitval .EQ. 3 ) grid%output_diagnostics=0
IF ( setinitval .EQ. 3 ) grid%nwp_diagnostics=0
IF ( setinitval .EQ. 3 ) grid%dfi_opt=0
IF ( setinitval .EQ. 3 ) grid%dfi_savehydmeteors=0
IF ( setinitval .EQ. 3 ) grid%dfi_nfilter=0
IF ( setinitval .EQ. 3 ) grid%dfi_write_filtered_input=.FALSE.
IF ( setinitval .EQ. 3 ) grid%dfi_write_dfi_history=.FALSE.
IF ( setinitval .EQ. 3 ) grid%dfi_cutoff_seconds=0
IF ( setinitval .EQ. 3 ) grid%dfi_time_dim=0
IF ( setinitval .EQ. 3 ) grid%dfi_fwdstop_year=0
IF ( setinitval .EQ. 3 ) grid%dfi_fwdstop_month=0
IF ( setinitval .EQ. 3 ) grid%dfi_fwdstop_day=0
IF ( setinitval .EQ. 3 ) grid%dfi_fwdstop_hour=0
IF ( setinitval .EQ. 3 ) grid%dfi_fwdstop_minute=0
IF ( setinitval .EQ. 3 ) grid%dfi_fwdstop_second=0
IF ( setinitval .EQ. 3 ) grid%dfi_bckstop_year=0
IF ( setinitval .EQ. 3 ) grid%dfi_bckstop_month=0
IF ( setinitval .EQ. 3 ) grid%dfi_bckstop_day=0
IF ( setinitval .EQ. 3 ) grid%dfi_bckstop_hour=0
IF ( setinitval .EQ. 3 ) grid%dfi_bckstop_minute=0
IF ( setinitval .EQ. 3 ) grid%dfi_bckstop_second=0
IF ( setinitval .EQ. 3 ) grid%time_step=0
IF ( setinitval .EQ. 3 ) grid%time_step_fract_num=0
IF ( setinitval .EQ. 3 ) grid%time_step_fract_den=0
IF ( setinitval .EQ. 3 ) grid%time_step_dfi=0
IF ( setinitval .EQ. 3 ) grid%min_time_step=0
IF ( setinitval .EQ. 3 ) grid%max_time_step=0
IF ( setinitval .EQ. 3 ) grid%target_cfl=initial_data_value
IF ( setinitval .EQ. 3 ) grid%target_hcfl=initial_data_value
IF ( setinitval .EQ. 3 ) grid%max_step_increase_pct=0
IF ( setinitval .EQ. 3 ) grid%starting_time_step=0
IF ( setinitval .EQ. 3 ) grid%step_to_output_time=.FALSE.
IF ( setinitval .EQ. 3 ) grid%adaptation_domain=0
IF ( setinitval .EQ. 3 ) grid%use_adaptive_time_step=.FALSE.
IF ( setinitval .EQ. 3 ) grid%use_adaptive_time_step_dfi=.FALSE.
IF ( setinitval .EQ. 3 ) grid%max_dom=0
IF ( setinitval .EQ. 3 ) grid%lats_to_mic=0
IF ( setinitval .EQ. 3 ) grid%s_we=0
IF ( setinitval .EQ. 3 ) grid%e_we=0
IF ( setinitval .EQ. 3 ) grid%s_sn=0
IF ( setinitval .EQ. 3 ) grid%e_sn=0
IF ( setinitval .EQ. 3 ) grid%s_vert=0
IF ( setinitval .EQ. 3 ) grid%e_vert=0
IF ( setinitval .EQ. 3 ) grid%num_metgrid_levels=0
IF ( setinitval .EQ. 3 ) grid%num_metgrid_soil_levels=0
IF ( setinitval .EQ. 3 ) grid%p_top_requested=initial_data_value
IF ( setinitval .EQ. 3 ) grid%interp_theta=.FALSE.
IF ( setinitval .EQ. 3 ) grid%interp_type=0
IF ( setinitval .EQ. 3 ) grid%vert_refine_fact=0
IF ( setinitval .EQ. 3 ) grid%extrap_type=0
IF ( setinitval .EQ. 3 ) grid%t_extrap_type=0
IF ( setinitval .EQ. 3 ) grid%hypsometric_opt=0
IF ( setinitval .EQ. 3 ) grid%lowest_lev_from_sfc=.FALSE.
IF ( setinitval .EQ. 3 ) grid%use_levels_below_ground=.FALSE.
IF ( setinitval .EQ. 3 ) grid%use_tavg_for_tsk=.FALSE.
IF ( setinitval .EQ. 3 ) grid%use_surface=.FALSE.
IF ( setinitval .EQ. 3 ) grid%lagrange_order=0
IF ( setinitval .EQ. 3 ) grid%force_sfc_in_vinterp=0
IF ( setinitval .EQ. 3 ) grid%zap_close_levels=initial_data_value
IF ( setinitval .EQ. 3 ) grid%sfcp_to_sfcp=.FALSE.
IF ( setinitval .EQ. 3 ) grid%adjust_heights=.FALSE.
IF ( setinitval .EQ. 3 ) grid%smooth_cg_topo=.FALSE.
IF ( setinitval .EQ. 3 ) grid%nest_interp_coord=0
IF ( setinitval .EQ. 3 ) grid%aggregate_lu=.FALSE.
IF ( setinitval .EQ. 3 ) grid%rh2qv_wrt_liquid=.FALSE.
IF ( setinitval .EQ. 3 ) grid%rh2qv_method=0
IF ( setinitval .EQ. 3 ) grid%qv_max_p_safe=initial_data_value
IF ( setinitval .EQ. 3 ) grid%qv_max_flag=initial_data_value
IF ( setinitval .EQ. 3 ) grid%qv_max_value=initial_data_value
IF ( setinitval .EQ. 3 ) grid%qv_min_p_safe=initial_data_value
IF ( setinitval .EQ. 3 ) grid%qv_min_flag=initial_data_value
IF ( setinitval .EQ. 3 ) grid%qv_min_value=initial_data_value
IF ( setinitval .EQ. 3 ) grid%dx=initial_data_value
IF ( setinitval .EQ. 3 ) grid%dy=initial_data_value
IF ( setinitval .EQ. 3 ) grid%grid_id=0
IF ( setinitval .EQ. 3 ) grid%grid_allowed=.FALSE.
IF ( setinitval .EQ. 3 ) grid%parent_id=0
IF ( setinitval .EQ. 3 ) grid%i_parent_start=0
IF ( setinitval .EQ. 3 ) grid%j_parent_start=0
IF ( setinitval .EQ. 3 ) grid%parent_grid_ratio=0
IF ( setinitval .EQ. 3 ) grid%parent_time_step_ratio=0
IF ( setinitval .EQ. 3 ) grid%feedback=0
IF ( setinitval .EQ. 3 ) grid%smooth_option=0
IF ( setinitval .EQ. 3 ) grid%blend_width=0
IF ( setinitval .EQ. 3 ) grid%ztop=initial_data_value
IF ( setinitval .EQ. 3 ) grid%moad_grid_ratio=0
IF ( setinitval .EQ. 3 ) grid%moad_time_step_ratio=0
IF ( setinitval .EQ. 3 ) grid%shw=0
IF ( setinitval .EQ. 3 ) grid%tile_sz_x=0
IF ( setinitval .EQ. 3 ) grid%tile_sz_y=0
IF ( setinitval .EQ. 3 ) grid%numtiles=0
IF ( setinitval .EQ. 3 ) grid%numtiles_inc=0
IF ( setinitval .EQ. 3 ) grid%numtiles_x=0
IF ( setinitval .EQ. 3 ) grid%numtiles_y=0
IF ( setinitval .EQ. 3 ) grid%tile_strategy=0
IF ( setinitval .EQ. 3 ) grid%nproc_x=0
IF ( setinitval .EQ. 3 ) grid%nproc_y=0
IF ( setinitval .EQ. 3 ) grid%irand=0
IF ( setinitval .EQ. 3 ) grid%dt=initial_data_value
IF ( setinitval .EQ. 3 ) grid%num_moves=0
IF ( setinitval .EQ. 3 ) grid%ts_buf_size=0
IF ( setinitval .EQ. 3 ) grid%max_ts_locs=0
IF ( setinitval .EQ. 3 ) grid%vortex_interval=0
IF ( setinitval .EQ. 3 ) grid%max_vortex_speed=0
IF ( setinitval .EQ. 3 ) grid%corral_dist=0
IF ( setinitval .EQ. 3 ) grid%track_level=0
IF ( setinitval .EQ. 3 ) grid%time_to_move=initial_data_value
IF ( setinitval .EQ. 3 ) grid%move_id=0
IF ( setinitval .EQ. 3 ) grid%move_interval=0
IF ( setinitval .EQ. 3 ) grid%move_cd_x=0
IF ( setinitval .EQ. 3 ) grid%move_cd_y=0
IF ( setinitval .EQ. 3 ) grid%swap_x=.FALSE.
IF ( setinitval .EQ. 3 ) grid%swap_y=.FALSE.
IF ( setinitval .EQ. 3 ) grid%cycle_x=.FALSE.
IF ( setinitval .EQ. 3 ) grid%cycle_y=.FALSE.
IF ( setinitval .EQ. 3 ) grid%reorder_mesh=.FALSE.
IF ( setinitval .EQ. 3 ) grid%perturb_input=.FALSE.
IF ( setinitval .EQ. 3 ) grid%eta_levels=initial_data_value
IF ( setinitval .EQ. 3 ) grid%max_dz=initial_data_value
IF ( setinitval .EQ. 3 ) grid%ocean_levels=0
IF ( setinitval .EQ. 3 ) grid%ocean_z=initial_data_value
IF ( setinitval .EQ. 3 ) grid%ocean_t=initial_data_value
IF ( setinitval .EQ. 3 ) grid%ocean_s=initial_data_value
IF ( setinitval .EQ. 3 ) grid%num_traj=0
IF ( setinitval .EQ. 3 ) grid%max_ts_level=0
IF ( setinitval .EQ. 3 ) grid%track_loc_in=0
IF ( setinitval .EQ. 3 ) grid%insert_bogus_storm=.FALSE.
IF ( setinitval .EQ. 3 ) grid%remove_storm=.FALSE.
IF ( setinitval .EQ. 3 ) grid%num_storm=0
IF ( setinitval .EQ. 3 ) grid%latc_loc=initial_data_value
IF ( setinitval .EQ. 3 ) grid%lonc_loc=initial_data_value
IF ( setinitval .EQ. 3 ) grid%vmax_meters_per_second=initial_data_value
IF ( setinitval .EQ. 3 ) grid%rmax=initial_data_value
IF ( setinitval .EQ. 3 ) grid%vmax_ratio=initial_data_value
IF ( setinitval .EQ. 3 ) grid%rankine_lid=initial_data_value
IF ( setinitval .EQ. 3 ) grid%mp_physics=0
IF ( setinitval .EQ. 3 ) grid%nssl_cccn=initial_data_value
IF ( setinitval .EQ. 3 ) grid%nssl_alphah=initial_data_value
IF ( setinitval .EQ. 3 ) grid%nssl_alphahl=initial_data_value
IF ( setinitval .EQ. 3 ) grid%nssl_cnoh=initial_data_value
IF ( setinitval .EQ. 3 ) grid%nssl_cnohl=initial_data_value
IF ( setinitval .EQ. 3 ) grid%nssl_cnor=initial_data_value
IF ( setinitval .EQ. 3 ) grid%nssl_cnos=initial_data_value
IF ( setinitval .EQ. 3 ) grid%nssl_rho_qh=initial_data_value
IF ( setinitval .EQ. 3 ) grid%nssl_rho_qhl=initial_data_value
IF ( setinitval .EQ. 3 ) grid%nssl_rho_qs=initial_data_value
IF ( setinitval .EQ. 3 ) grid%gsfcgce_hail=0
   END SUBROUTINE alloc_space_field_core_2
END MODULE module_alloc_space_2
