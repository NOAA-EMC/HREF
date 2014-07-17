

MODULE module_wind_fitch
  USE module_wind_generic
  USE module_driver_constants, ONLY : max_domains
  USE module_model_constants, ONLY : piconst
  USE module_model_constants, ONLY : g
  IMPLICIT NONE
  LOGICAL, DIMENSION(max_domains) :: inited
  PUBLIC turbine_drag
  PRIVATE dragcof, turbine_area, inited
CONTAINS
  SUBROUTINE turbine_drag( &
       & id &
       &,phb,u,v,xlat_u,xlong_u &
       &,xlat_v,xlong_v &
       &,dx,dz,dt,qke &
       &,qke_adv,bl_mynn_tkeadvect &
       &,du,dv &
       &,ids,ide,jds,jde,kds,kde &
       &,ims,ime,jms,jme,kms,kme &
       &,its,ite,jts,jte,kts,kte &
       &)
  INTEGER, INTENT(IN) :: id
  INTEGER, INTENT(IN) :: its,ite,jts,jte,kts,kte
  INTEGER, INTENT(IN) :: ims,ime,jms,jme,kms,kme
  INTEGER, INTENT(IN) :: ids,ide,jds,jde,kds,kde
  LOGICAL, INTENT(IN) :: bl_mynn_tkeadvect
  REAL, INTENT(IN) :: dx,dt
  REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(IN) :: dz,u,v,phb
  REAL, DIMENSION(ims:ime,jms:jme), INTENT(IN) :: xlat_u, xlong_u
  REAL, DIMENSION(ims:ime,jms:jme), INTENT(IN) :: xlat_v, xlong_v
  REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: du,dv,qke,qke_adv
  TYPE(windturbine_specs), POINTER :: p
  INTEGER turbgridid
  REAL hubheight,diameter,power,cutinspeed,cutoutspeed,stdthrcoef,turbpercell
  INTEGER ewfx,ewfy,pwfx,pwfy
  REAL blade_l_point,blade_u_point,zheightl,zheightu,z1,z2,tarea
  REAL speed,tkecof,powcof,thrcof,wfdensity
  INTEGER itf,jtf,ktf
  INTEGER i,j,k,swfindx,ewfindx,swfindy,ewfindy,n,n1,n2,iturbine
  INTEGER k_turbine_bot, k_turbine_top
  LOGICAL :: kfound
  INTEGER :: allzero
  itf=MIN0(ite,ide-1)
  jtf=MIN0(jte,jde-1)
  ktf=MIN0(kte,kde-1)
  CALL nl_get_td_turbpercell(1,turbpercell)
  CALL nl_get_td_turbgridid(1,turbgridid)
  IF ( .NOT. inited(id) ) THEN
    IF ( windspec .EQ. WIND_TURBINES_FROMLIST ) THEN
      allzero=1
      DO j=jts,jtf
        DO i=its,itf
          IF (xlat_u(i,j).NE.0. .OR. xlong_u(i,j).NE.0.)allzero=0
        ENDDO
      ENDDO
      CALL wrf_dm_bcast_integer(allzero,1)
      IF ( allzero .NE. 1 ) THEN
        DO iturbine = 1,nwindturbines
          p => windturbines(iturbine)
          IF ( id .EQ. p%id ) THEN
            DO j=jts,jtf
              DO i=its,itf
                IF (xlat_v(i,j) .LE. p%lat .AND. p%lat .LT. xlat_v(i,j+1) .AND. &
                    xlong_u(i,j).LE. p%lon .AND. p%lon .LT. xlong_u(i+1,j)) THEN
                  p%i=i
                  p%j=j
                ENDIF
              ENDDO
            ENDDO
          ENDIF
        ENDDO
      ENDIF
    ELSE IF ( windspec .EQ. WIND_TURBINES_IDEAL .AND. id .EQ. turbgridid ) THEN
      CALL nl_get_td_ewfx(1,ewfx)
      CALL nl_get_td_ewfy(1,ewfy)
      CALL nl_get_td_pwfx(1,pwfx)
      CALL nl_get_td_pwfy(1,pwfy)
      CALL nl_get_td_hubheight(1,hubheight)
      CALL nl_get_td_diameter(1,diameter)
      CALL nl_get_td_power(1,power)
      CALL nl_get_td_cutinspeed(1,cutinspeed)
      CALL nl_get_td_cutoutspeed(1,cutoutspeed)
      CALL nl_get_td_stdthrcoef(1,stdthrcoef)
      n = 0
      DO j = jts,jtf
        IF ( pwfy .LE. j .AND. j .LE. (pwfy+ewfy-1) ) THEN
          DO i = its,itf
            IF ( pwfx .LE. i .AND. i .LE. (pwfx+ewfx-1) ) THEN
              n = n + 1
            ENDIF
          ENDDO
        ENDIF
      ENDDO
      nwindturbines = n
      ALLOCATE(windturbines(nwindturbines))
      n = 0
      DO j = jts,jtf
        IF ( pwfy .LE. j .AND. j .LE. (pwfy+ewfy-1) ) THEN
          DO i = its,itf
            IF ( pwfx .LE. i .AND. i .LE. (pwfx+ewfx-1) ) THEN
              n = n + 1
              IF ( n .GT. nwindturbines ) THEN
                CALL wrf_error_fatal3("<stdin>",165,&
'would overrun windturbines array')
              ENDIF
              windturbines(n)%id = id
              windturbines(n)%lat = 0.0
              windturbines(n)%lon = 0.0
              windturbines(n)%i = i
              windturbines(n)%j = j
              windturbines(n)%hubheight = hubheight
              windturbines(n)%diameter = diameter
              windturbines(n)%stdthrcoef = stdthrcoef
              windturbines(n)%power = power
              windturbines(n)%cutinspeed = cutinspeed
              windturbines(n)%cutoutspeed = cutoutspeed
            ENDIF
          ENDDO
        ENDIF
      ENDDO
    ENDIF
    inited(id) = .TRUE.
  ENDIF
  IF ( windspec .EQ. WIND_TURBINES_FROMLIST ) THEN
    wfdensity = 1.0/(dx*dx)
  ELSE
    wfdensity = turbpercell/(dx*dx)
  ENDIF
  IF (inited(id) .AND. &
      ((windspec .EQ. WIND_TURBINES_FROMLIST) .OR. &
       (windspec .EQ. WIND_TURBINES_IDEAL .AND. id .EQ. turbgridid ))) THEN
    DO iturbine = 1,nwindturbines
      p => windturbines(iturbine)
      IF ( id .EQ. p%id ) THEN
        k_turbine_bot=0
        k_turbine_top=-1
        i = p%i
        j = p%j
        IF (( its .LE. i .AND. i .LE. itf ) .AND. &
            ( jts .LE. j .AND. j .LE. jtf ) ) THEN
          blade_l_point=p%hubheight-p%diameter/2.
          blade_u_point=p%hubheight+p%diameter/2.
          kfound = .false.
          zheightl=0.0
          DO k=kts,ktf
            IF(.NOT. kfound) THEN
              zheightu = zheightl + dz(i,k,j)
              IF(blade_l_point .GE. zheightl .AND. blade_l_point .LE. zheightu) THEN
                k_turbine_bot=k
              ENDIF
              IF(blade_u_point .GE. zheightl .AND. blade_u_point .LE. zheightu) THEN
                k_turbine_top=k
                kfound = .TRUE.
              ENDIF
              zheightl = zheightu
            ENDIF
          ENDDO
          IF ( kfound ) THEN
            DO k=k_turbine_bot,k_turbine_top
              z1=phb(i,k,j)/g-blade_l_point-phb(i,1,j)/g
              z2=phb(i,k+1,j)/g-blade_l_point-phb(i,1,j)/g
              IF(z1 .LT. 0.) z1=0.0
              IF(z2 .GT. p%diameter) z2=p%diameter
              speed=sqrt(u(i,k,j)**2.+v(i,k,j)**2.)
              CALL dragcof(tkecof,powcof,thrcof, &
                           speed,p%cutinspeed,p%cutoutspeed, &
                           p%power,p%diameter,p%stdthrcoef)
              CALL turbine_area(z1,z2,p%diameter,wfdensity,tarea)
              qke(i,k,j) = qke(i,k,j)+speed**3.*tarea*tkecof*dt/dz(i,k,j)
              du(i,k,j) = du(i,k,j)-.5*u(i,k,j)*thrcof*speed*tarea/dz(i,k,j)
              dv(i,k,j) = dv(i,k,j)-.5*v(i,k,j)*thrcof*speed*tarea/dz(i,k,j)
            ENDDO
          ENDIF
        ENDIF
      ENDIF
    ENDDO
  ENDIF
   IF (BL_MYNN_TKEADVECT) THEN
      qke_adv=qke
   ENDIF
  END SUBROUTINE turbine_drag
  SUBROUTINE turbine_area(z1,z2,tdiameter,wfdensity,tarea)
  REAL, INTENT(IN) ::tdiameter,wfdensity
  REAL, INTENT(INOUT) ::z1,z2
  REAL, INTENT(OUT):: tarea
  REAL r,zc1,zc2
  r=tdiameter/2.
  z1=r-z1
  z2=r-z2
  zc1=abs(z1)
  zc2=abs(z2)
  IF(z1 .GT. 0. .AND. z2 .GT. 0.) THEN
     tarea=zc1*sqrt(r*r-zc1*zc1)+r*r*asin(zc1/r)- &
     (zc2*sqrt(r*r-zc2*zc2)+r*r*asin(zc2/r))
  ELSE IF(z1 .LT. 0. .AND. z2 .LT. 0.) THEN
     tarea=zc2*sqrt(r*r-zc2*zc2)+r*r*asin(zc2/r)- &
     (zc1*sqrt(r*r-zc1*zc1)+r*r*asin(zc1/r))
  ELSE
     tarea=zc2*sqrt(r*r-zc2*zc2)+r*r*asin(zc2/r)+ &
     zc1*sqrt(r*r-zc1*zc1)+r*r*asin(zc1/r)
  ENDIF
  tarea=tarea*wfdensity
  END SUBROUTINE turbine_area
  SUBROUTINE dragcof(tkecof,powcof,thrcof,speed,cispeed,cospeed, &
                     tpower,tdiameter,stdthrcoef)
  REAL, INTENT(IN):: speed, cispeed, cospeed, tpower,tdiameter,stdthrcoef
  REAL, INTENT(OUT):: tkecof,powcof,thrcof
  REAL :: power,area,mspeed,hspeed
  area=piconst/4.*tdiameter**2.
  mspeed=0.5*(cospeed+cispeed)
  hspeed=0.5*(cospeed-mspeed)
  power =tpower*(.5*tanh((speed - (mspeed-hspeed))/(hspeed*0.60)) + .5)*.8
  IF(speed .LE. cispeed .OR. speed .GE. cospeed) THEN
     power=0.
     powcof=0.
  ELSE
     powcof = power * 2.e+6 / (speed**3.*area)
     IF (speed .LT. cispeed*2.) THEN
        powcof = powcof * exp(-((speed-cispeed*2.)**2./(cispeed*2.)))
     end if
     powcof = MIN(powcof,.55)
  ENDIF
  IF (speed .LE. cispeed .OR. speed .GE. cospeed) THEN
     thrcof = stdthrcoef
  ELSE
     thrcof = powcof + powcof*0.75
     thrcof = MIN(thrcof,.9)
     thrcof = MAX(thrcof,stdthrcoef)
  ENDIF
  tkecof=thrcof-powcof
  IF(tkecof .LT. 0.) tkecof=0.
  END SUBROUTINE dragcof
  SUBROUTINE init_module_wind_fitch
    inited = .FALSE.
  END SUBROUTINE init_module_wind_fitch
END MODULE module_wind_fitch
