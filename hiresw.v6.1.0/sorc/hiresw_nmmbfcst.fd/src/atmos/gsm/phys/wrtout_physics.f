
      subroutine wrtout_physics(phour,fhour,zhour,idate,
     &                  sl,si,
     &                  sfc_fld, flx_fld, nst_fld, g2d_fld,
     &                  fluxr,
     &                  lats_nodes_r,global_lats_r,lonsperlar,nblck,
     &                  colat1,cfhour1,pl_coeff,sfcf,flxf,d3df)
!!
!
! May 2009 Jun Wang, modified to use write grid component
! Jan 2010 Sarah Lu, AOD added to flx files
! Feb 2010 Jun Wang, write out restart file
! Jul 2010 S. Moorthi - added nst and other modifications
! Jul 2010 S. Moorthi - added  hchuang  Add flx files output to wrtflx_a
! Jul 2010 Sarah Lu, write out aerosol diag files (for g2d_fld)
! Aug 2010 Sarah Lu, scale the 2d_aer_diag by 1.e6
!                    output time-avg 2d_aer_diag
! Oct 2010 Sarah Lu, add g2d_fld%met
! Oct 2010 Sarah Lu, g2d_fld%met changed from instant to accumulated
! Dec 2010 Sarah Lu, g2d_fld%met contains both instant and time-avg;
!                    wrtaer is called only when gocart is on
! Dec 2010 Jun Wang, change to nemsio library
! Nov 2012 Jun Wang, removing quilting, which is not used
! Nov 2012 Jun Wang, add sfcio opt
! Nov 2012 Jun Wang, add d3d opt
! Nov 2012 Jun Wang, removing quilting, which is not used
! Jan 2013 S. Moorthi, adding sfcf,flxf,d3df to call and related changes
! Feb 2013 Jun Wang, using gribit_gsm from gsm (gridit_gsm)
! May 2013 S. Moorthi, using gribit_gsm from gsm (gridit_gsm)
!

      use resol_def,               ONLY: latr, levs, levp1, lonr, nfxr,
     &                                   ngrids_aer
      use layout1,                 ONLY: me, nodes, lats_node_r, 
     &                                   nodes_comp
      use namelist_physics_def,    ONLY: gen_coord_hybrid, ldiag3d, 
     &                                   hybrid, fhlwr, fhswr, ens_nam,
     &                                   nst_fcst, lggfs3d, sfcio_out
      use mpi_def,                 ONLY: liope, info, mpi_comm_all, 
     &                                   mc_comp, mpi_comm_null
      use gfs_physics_sfc_flx_mod, ONLY: Sfc_Var_Data, Flx_Var_Data
      use gfs_physics_nst_var_mod, ONLY: Nst_Var_Data
      use gfs_physics_g2d_mod,     ONLY: G2D_Var_Data
      USE machine,                 ONLY: kind_evod, kind_io8
      implicit none
!!
      TYPE(Sfc_Var_Data)        :: sfc_fld
      TYPE(Flx_Var_Data)        :: flx_fld
      TYPE(Nst_Var_Data)        :: nst_fld
      TYPE(G2D_Var_Data)        :: g2d_fld
      CHARACTER(16)             :: CFHOUR1    ! for ESMF Export State Creation
      character*5 sfcf,flxf,d3df
      integer ixgr, pl_coeff
      real(kind=kind_evod) phour,fhour,zhour
!     real(kind=kind_evod) phour,fhour,zhour, xgf
!!
      integer              idate(4),nblck,km,iostat,no3d,ks
      logical lfnhr
      real colat1
      real(kind=8) t1,t2,t3,t4,t5,ta,tb,tc,td,te,tf,rtc,tx,ty
      real timesum
!!
      real(kind=kind_evod) sl(levs), si(levp1)
!!
      integer              lats_nodes_r(nodes)
!!
      integer              ierr,j,k,l,lenrec,locl,n,node
      integer nosfc,noflx,nonst,noaer,nfill
      character*16 cosfc,const
      data timesum/0./
!!
!!
      character CFHOUR*40,CFORM*40
      integer jdate(4),ndigyr,ndig,kh,IOPROC
!!
      REAL (KIND=KIND_IO8) GESHEM(LONR,LATS_NODE_R)
      INTEGER              GLOBAL_LATS_R(LATR),   lonsperlar(LATR)
!
      REAL (KIND=kind_io8) fluxr(nfxr,LONR,LATS_NODE_R)
      real(kind=kind_io8) slmsk(lonr,lats_node_r)
!!
      real(kind=kind_evod) secphy,secswr,seclwr
      real(kind=8) tba,tbb,tbc,tbd
      integer iret
!
!-------------------------------------------------------------------------
!     print *,' in wrtout_phyiscs me=',me
      t3=rtc()
      call mpi_barrier(mpi_comm_all,ierr)
      t4=rtc()
      tba=t4-t3
      if(nodes_comp .lt. 1 .or. nodes_comp .gt. nodes) then
        print *, '  NODES_COMP UNDEFINED, CANNOT DO I.O '
        call mpi_finalize()
         stop 333
      endif
!
      ioproc=nodes_comp-1
       
      t1=rtc()
!!
!!
!     CREATE CFHOUR
      JDATE=IDATE
      ndigyr=4
      IF(NDIGYR.EQ.2) THEN
        JDATE(4)=MOD(IDATE(4)-1,100)+1
      ENDIF

!sela set lfnhr to false for writing one step output etc.
      lfnhr=.true.    ! no output
      lfnhr=3600*abs(fhour-nint(fhour)).le.1.or.phour.eq.0
      IF(LFNHR) THEN
        KH=NINT(FHOUR)
        NDIG=MAX(LOG10(KH+0.5)+1.,2.)
        WRITE(CFORM,'("(I",I1,".",I1,")")') NDIG,NDIG
        WRITE(CFHOUR,CFORM) KH
      ELSE
        KS=NINT(FHOUR*3600)
        KH=KS/3600
        KM=(KS-KH*3600)/60
        KS=KS-KH*3600-KM*60
        NDIG=MAX(LOG10(KH+0.5)+1.,2.)
        WRITE(CFORM,
     &      '("(I",I1,".",I1,",A1,I2.2,A1,I2.2)")') NDIG,NDIG
        WRITE(CFHOUR,CFORM) KH,':',KM,':',KS
      ENDIF
      IF(nfill(ens_nam) == 0) THEN
      CFHOUR = CFHOUR(1:nfill(CFHOUR))
      ELSE
      CFHOUR = CFHOUR(1:nfill(CFHOUR)) // ens_nam(1:nfill(ens_nam))
      END IF
!jfe
      nosfc = 62
      noflx = 63
      nonst = 65
      noaer = 66
!!
      t3=rtc()
      call MPI_BARRIER(mpi_comm_all,ierr)
      t4=rtc()
      tbd=t4-t3
      t3=rtc()
      SECPHY=(FHOUR-ZHOUR)*3600.
      SECSWR=MAX(SECPHY,FHSWR*3600.)
      SECLWR=MAX(SECPHY,FHLWR*3600.)
!
!*** BUILD STATE ON EACH NODE ********
! build state on each node.   COMP tasks only
! assemble spectral state first then sfc state,
! then (only if liope)  flux state.
! finally (only if gocart is turned on) aer_diag state
! 
!      print *,'---- start sfc collection section -----'
      t3=rtc()
      if(mc_comp .ne. MPI_COMM_NULL) then
        CALL sfc_collect(sfc_fld,global_lats_r,lonsperlar)

       if ( nst_fcst > 0 ) then
         call nst_collect(nst_fld,global_lats_r,lonsperlar)
       endif
!
! collect flux grids as was done with sfc grids above.
! but only if liope is true.  If liope is false,
! the fluxes are handled by the original wrtsfc
! predating the I/O task updates.
!
            call   wrtflx_a
     &             (IOPROC,noflx,ZHOUR,FHOUR,IDATE,colat1,SECSWR,SECLWR,
     &              sfc_fld, flx_fld, fluxr, global_lats_r,lonsperlar,
     &              slmsk)

	    if ( ngrids_aer .gt. 0) then
               call   wrtaer
     &             (IOPROC,noaer,ZHOUR,FHOUR,IDATE,
     &              sfc_fld, g2d_fld, global_lats_r, lonsperlar)
            endif

      endif                 ! comp node
      t4=rtc()
      td=t4-t3
!
!  done with state build
!  NOW STATE IS ASSEMBLED ON EACH NODE.  GET EVERYTHING OFF THE COMPUTE
!  NODES (currently done with a send to the I/O task_
!  send state to I/O task.  All tasks
!
      if(sfcio_out) then
          if(me==0)print *,'---- start sfc.f section -----'
          call sfc_only_move(ioproc)
          cosfc = sfcf//CFHOUR
          call sfc_wrt(ioproc,nosfc,cosfc,fhour,jdate
     &,                global_lats_r,lonsperlar)
          call flx_only_move(ioproc)
          cosfc = flxf//CFHOUR
!         print *,'wrtout_physics call wrtsfc to write out ',
!    &    'flx, noflx=',noflx,'cosfc=',trim(cosfc),'ZHOUR=',ZHOUR,
!    &    'FHOUR=',FHOUR,'IDATE=',IDATE,'ioproc=',ioproc
          if(me  == ioproc) then
            call baopenwt(noflx,cosfc,iostat)
!           print *,'after open flx file,',trim(cosfc)
          endif
          call  wrtflx_w(IOPROC,noflx,ZHOUR,FHOUR,IDATE,colat1,SECSWR,
     &                  SECLWR,slmsk, global_lats_r,lonsperlar)
      endif          !  sfcio _out
!
      t4=rtc()
      te=t4-t3
!
!      print *,'---- start diag3d.f section -----'
      if (ldiag3d) then
        if(me==0) print *,' wrtout_physics ldiag3d on so wrt3d '
        no3d=64
        if(me.eq.IOPROC)
     &  call BAOPENWT(NO3D,d3df//CFHOUR,iostat)
        if (hybrid .or. gen_coord_hybrid) then
          call WRT3D_hyb(IOPROC,no3d,ZHOUR,FHOUR,IDATE,colat1,
     &                   global_lats_r,lonsperlar,pl_coeff,
     &                   SECSWR,SECLWR,sfc_fld%slmsk,flx_fld%psurf)
        else
          call WRT3D(IOPROC,no3d,ZHOUR,FHOUR,IDATE,colat1,
     &               global_lats_r,lonsperlar,pl_coeff,
     &               SECSWR,SECLWR,sl,si,sfc_fld%slmsk,flx_fld%psurf)
        endif
      endif
!
!      if(me .eq. ioproc)  call wrtlog_physics(phour,fhour,idate)

      tb = rtc()
      tf = tb-t1
!     tf = tb-ta
      t2 = rtc()

       if (me == ioproc) write(0,*)' WRTOUT_PHYSICS TIME=',tf

!     print 1011,tf
!1011 format(' WRTOUT_PHYSICS TIME ',f10.4)
      timesum = timesum+(t2-t1)
!     print 1012,timesum,t2-t1,td,te,tf,t4-t3,tba,tbb,tbc,tbd
 1012 format(
     1 ' WRTOUT_PHYSICS TIME ALL TASKS  ',f10.4,f10.4,
     1 ' state, send, io  iobarr, (beginbarr),
     1 spectbarr,open, openbarr )  ' ,
     1  8f9.4)
!
      return
      end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      SUBROUTINE wrtout_restart_physics(
     &        sfc_fld, nst_fld, fhour,idate,
     &        lats_nodes_r,global_lats_r,lonsperlar,
     &        phy_f3d, phy_f2d, ngptc, nblck, ens_nam)
!!
! Feb 2010 Jun Wang, write out restart file
! Mar 2013 Jun Wang, add idea fields to restart file

      use resol_def,               ONLY: latr, levp1, levs, lonr,
     &                                   num_p2d, num_p3d
      use  namelist_physics_def,   ONLY: nst_fcst, lsidea
      use layout1,                 ONLY: me, nodes, lats_node_r
      use gfs_physics_sfc_flx_mod, ONLY: Sfc_Var_Data, Flx_Var_Data
      use gfs_physics_nst_var_mod, ONLY: Nst_Var_Data
      USE machine,                 ONLY: kind_evod, kind_phys
      use idea_composition,        ONLY: pr_idea,gg,prsilvl,amgms
      implicit none
!!
      TYPE(Sfc_Var_Data)        :: sfc_fld
      TYPE(Nst_Var_Data)        :: nst_fld

      real(kind=kind_evod) fhour
      character (len=*)  :: ens_nam
!!
      integer              idate(4), ixgr
!
      integer              ngptc, nblck
      REAL (KIND=KIND_phys)
     &            phy_f3d(ngptc,levs,nblck,LATS_NODE_R,num_p3d)
     &,           phy_f2d(LONR,LATS_NODE_R,num_p2d)
!!
      real(kind=kind_evod) sl(levs)
      real(kind=kind_evod) si(levp1)
!!
      integer igen
!!
      INTEGER              lats_nodes_r(nodes)
      INTEGER              GLOBAL_LATS_R(LATR)
      INTEGER              lonsperlar(LATR)
      integer IOPROC, IPRINT
      integer needoro, iret, nfill
!
!!
      integer n3,n4,nflop
      character*20 cfile
      integer nn
!!
      IPRINT = 0
!
      cfile='SFCR'
!      print *,' cfile=',cfile,'ens_nam=',ens_nam(1:nfill(ens_nam))
!
!      print *,' in rest write fhour=',fhour,
!     &  'idate=',idate,' before para_fixio_w'
!
      IOPROC=nodes-1
      CALL para_fixio_w(ioproc,sfc_fld,cfile,fhour,idate,
     &  lats_nodes_r,global_lats_r,lonsperlar,
     &  phy_f3d, phy_f2d, ngptc, nblck, ens_nam,
     &  lsidea, pr_idea, gg, prsilvl, amgms)
!
      if(nst_fcst>0) then
        cfile='NSTR'
        CALL para_nst_w(ioproc,nst_fld,cfile,fhour,idate,
     &   lats_nodes_r,global_lats_r,lonsperlar,
     &   ens_nam)
      endif 
!
      return
      end
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      SUBROUTINE wrtlog_physics(phour,fhour,idate)
      use namelist_physics_def, ONLY: ens_nam
      implicit none

      integer idate(4),ndigyr,nolog
      integer ks,kh,km,ndig,nfill
      character CFHOUR*40,CFORM*40
      logical lfnhr
      real phour,fhour
!
!     CREATE CFHOUR

!sela set lfnhr to false for writing one step output etc.
      lfnhr=.true.    ! no output
!!mr  lfnhr=.false.   !    output
      lfnhr=3600*abs(fhour-nint(fhour)).le.1.or.phour.eq.0
      IF(LFNHR) THEN
        KH=NINT(FHOUR)
        NDIG=MAX(LOG10(KH+0.5)+1.,2.)
        WRITE(CFORM,'("(I",I1,".",I1,")")') NDIG,NDIG
        WRITE(CFHOUR,CFORM) KH
        WRITE(CFORM,'("(I",I1,".",I1,")")') NDIG,NDIG
        WRITE(CFHOUR,CFORM) KH
      ELSE
        KS=NINT(FHOUR*3600)
        KH=KS/3600
        KM=(KS-KH*3600)/60
        KS=KS-KH*3600-KM*60
        NDIG=MAX(LOG10(KH+0.5)+1.,2.)
        WRITE(CFORM,
     &      '("(I",I1,".",I1,",A1,I2.2,A1,I2.2)")') NDIG,NDIG
        WRITE(CFHOUR,CFORM) KH,':',KM,':',KS
      ENDIF
      IF(nfill(ens_nam) == 0) THEN
      CFHOUR = CFHOUR(1:nfill(CFHOUR))
      ELSE
      CFHOUR = CFHOUR(1:nfill(CFHOUR)) // ens_nam(1:nfill(ens_nam))
      END IF

      nolog=99
      OPEN(NOlog,FILE='LOG.F'//CFHOUR,FORM='FORMATTED')
      write(nolog,100)fhour,idate
100   format(' completed mrf fhour=',f10.3,2x,4(i4,2x))
      CLOSE(NOlog)

      RETURN
      END



      SUBROUTINE sfc_collect (sfc_fld,global_lats_r,lonsperlar)
!!
      use resol_def,               ONLY: latr, lonr, ngrids_sfcc, 
     &                                   ngrids_sfcc2d,ngrids_sfcc3d,
     &                                   ngrids_flx, lsoil
      use mod_state,               ONLY:
     &                                   buff_mult_piecea2d,ngrid2d,
     &                                   buff_mult_piecea3d,ngrid3d
      use layout1,                 ONLY: lats_node_r,lats_node_r_max
      use gfs_physics_sfc_flx_mod, ONLY: Sfc_Var_Data
      USE machine,                 ONLY: kind_io8, kind_io4
      implicit none
!!
      TYPE(Sfc_Var_Data)        :: sfc_fld
!
      INTEGER              GLOBAL_LATS_R(latr)
      INTEGER              lonsperlar(latr)
!!
!!!   real(kind=kind_io4) buff4(lonr,latr,4),bufsave(lonr,lats_node_r)
      real(kind=kind_io8) buffo(lonr,lats_node_r)
      real(kind=kind_io8) buffi(lonr,lats_node_r_max)
      integer kmsk(lonr,lats_node_r_max)
!     integer kmsk(lonr,lats_node_r_max),kmskcv(lonr,lats_node_r_max)
      integer k,il
       integer ubound
       integer icount
        integer  ierr
!!
      CHARACTER*8 labfix(4)
      real(kind=kind_io4) yhour
      integer,save:: version
      data version/200004/
      data  icount/0/
      integer maxlats_comp
!
      ngrid2d=1
      ngrid3d=1
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
!!
      if(allocated(buff_mult_piecea2d)) then
         continue
      else
         allocate
     1 (buff_mult_piecea2d(lonr,lats_node_r_max,1:ngrids_sfcc2d+1),
     1  buff_mult_piecea3d(lonr,lats_node_r_max,1:ngrids_sfcc3d+1))
      endif
!
      kmsk (:,1:lats_node_r) = nint(sfc_fld%slmsk(:,1:lats_node_r))
!
      ngrid2d=1
      CALL uninterprez(1,kmsk,buffo,sfc_fld%tsea,
     &       global_lats_r,lonsperlar,buff_mult_piecea2d(1,1,ngrid2d))
!
! ngrid=2 here
                                                                                                        
!
      ngrid3d=0
      DO k=1,LSOIL
        buffi(:,:) = sfc_fld%SMC(k,:,:)
        ngrid3d=ngrid3d+1
        CALL uninterprez(1,kmsk,buffo,buffi,global_lats_r,lonsperlar,
     &        buff_mult_piecea3d(1,1,ngrid3d))
      ENDDO
!
      ngrid2d=ngrid2d+1
      CALL uninterprez(1,kmsk,buffo,sfc_fld%SHELEG,
     &   global_lats_r,lonsperlar,buff_mult_piecea2d(1,1,ngrid2d))
!
      DO k=1,LSOIL
        buffi(:,:) = sfc_fld%STC(k,:,:)
!
        ngrid3d=ngrid3d+1
        CALL uninterprez(1,kmsk,buffo,buffi,global_lats_r,lonsperlar,
     &         buff_mult_piecea3d(1,1,ngrid3d))
      ENDDO
!
      ngrid2d=ngrid2d+1
      CALL uninterprez(1,kmsk,buffo,sfc_fld%TG3,
     &       global_lats_r,lonsperlar,buff_mult_piecea2d(1,1,ngrid2d))
!
      ngrid2d=ngrid2d+1
      CALL uninterprez(1,kmsk,buffo,sfc_fld%ZORL,
     &       global_lats_r,lonsperlar,buff_mult_piecea2d(1,1,ngrid2d))
!!
!     where(CV.gt.0.)
!         kmskcv=1
!     elsewhere
!         kmskcv=0
!     endwhere
!
!*********************************************************************
!   Not in version 200501
!     CALL uninterprez(1,kmskcv,buffo,CV,global_lats_r,lonsperlar)
!     CALL uninterprez(1,kmskcv,buffo,CVB,global_lats_r,lonsperlar)
!     CALL uninterprez(1,kmskcv,buffo,CVT,global_lats_r,lonsperlar)
!*********************************************************************
!jws
      ngrid2d=ngrid2d+1
      CALL uninterprez(1,kmsk,buffo,sfc_fld%ALVSF,
     &       global_lats_r,lonsperlar,buff_mult_piecea2d(1,1,ngrid2d))
!
      ngrid2d=ngrid2d+1
      CALL uninterprez(1,kmsk,buffo,sfc_fld%ALVWF,
     &       global_lats_r,lonsperlar,buff_mult_piecea2d(1,1,ngrid2d))
!
      ngrid2d=ngrid2d+1
      CALL uninterprez(1,kmsk,buffo,sfc_fld%ALNSF,
     &       global_lats_r,lonsperlar,buff_mult_piecea2d(1,1,ngrid2d))
!
      ngrid2d=ngrid2d+1
      CALL uninterprez(1,kmsk,buffo,sfc_fld%ALNWF,
     &       global_lats_r,lonsperlar,buff_mult_piecea2d(1,1,ngrid2d))
!
      ngrid2d=ngrid2d+1
      CALL uninterprez(1,kmsk,buffo,sfc_fld%SLMSK,
     &       global_lats_r,lonsperlar,buff_mult_piecea2d(1,1,ngrid2d))
!
      ngrid2d=ngrid2d+1
      CALL uninterprez(1,kmsk,buffo,sfc_fld%VFRAC,
     &       global_lats_r,lonsperlar,buff_mult_piecea2d(1,1,ngrid2d))
!
      ngrid2d=ngrid2d+1
      CALL uninterprez(1,kmsk,buffo,sfc_fld%CANOPY,
     &       global_lats_r,lonsperlar,buff_mult_piecea2d(1,1,ngrid2d))
!
      ngrid2d=ngrid2d+1
      CALL uninterprez(1,kmsk,buffo,sfc_fld%F10M,
     &       global_lats_r,lonsperlar,buff_mult_piecea2d(1,1,ngrid2d))
! T2M
      ngrid2d=ngrid2d+1
      CALL uninterprez(1,kmsk,buffo,sfc_fld%T2M,
     &       global_lats_r,lonsperlar,buff_mult_piecea2d(1,1,ngrid2d))
! Q2M
      ngrid2d=ngrid2d+1
      CALL uninterprez(1,kmsk,buffo,sfc_fld%Q2M,
     &       global_lats_r,lonsperlar,buff_mult_piecea2d(1,1,ngrid2d))
!
      ngrid2d=ngrid2d+1
      CALL uninterprez(1,kmsk,buffo,sfc_fld%VTYPE,
     &       global_lats_r,lonsperlar,buff_mult_piecea2d(1,1,ngrid2d))
!
      ngrid2d=ngrid2d+1
      CALL uninterprez(1,kmsk,buffo,sfc_fld%STYPE,
     &       global_lats_r,lonsperlar,buff_mult_piecea2d(1,1,ngrid2d))

!
      ngrid2d=ngrid2d+1
      CALL uninterprez(1,kmsk,buffo,sfc_fld%FACSF,
     &       global_lats_r,lonsperlar,buff_mult_piecea2d(1,1,ngrid2d))
!
      ngrid2d=ngrid2d+1
      CALL uninterprez(1,kmsk,buffo,sfc_fld%FACWF,
     &       global_lats_r,lonsperlar,buff_mult_piecea2d(1,1,ngrid2d))
!
      ngrid2d=ngrid2d+1
      CALL uninterprez(1,kmsk,buffo,sfc_fld%UUSTAR,
     &       global_lats_r,lonsperlar,buff_mult_piecea2d(1,1,ngrid2d))
!
      ngrid2d=ngrid2d+1
      CALL uninterprez(1,kmsk,buffo,sfc_fld%FFMM,
     &       global_lats_r,lonsperlar,buff_mult_piecea2d(1,1,ngrid2d))
!
      ngrid2d=ngrid2d+1
      CALL uninterprez(1,kmsk,buffo,sfc_fld%FFHH,
     &       global_lats_r,lonsperlar,buff_mult_piecea2d(1,1,ngrid2d))
!
!c-- XW: FOR SEA-ICE Nov04
      ngrid2d=ngrid2d+1
      CALL uninterprez(1,kmsk,buffo,sfc_fld%HICE,
     &       global_lats_r,lonsperlar,buff_mult_piecea2d(1,1,ngrid2d))
      ngrid2d=ngrid2d+1
      CALL uninterprez(1,kmsk,buffo,sfc_fld%FICE,
     &       global_lats_r,lonsperlar,buff_mult_piecea2d(1,1,ngrid2d))
      ngrid2d=ngrid2d+1
      CALL uninterprez(1,kmsk,buffo,sfc_fld%TISFC,
     &       global_lats_r,lonsperlar,buff_mult_piecea2d(1,1,ngrid2d))

!c-- XW: END SEA-ICE Nov04
!
!lu: the addition of 8 Noah-related records starts here ........................
!tprcp
      ngrid2d=ngrid2d+1
      CALL uninterprez(1,kmsk,buffo,sfc_fld%TPRCP,
     &       global_lats_r,lonsperlar,buff_mult_piecea2d(1,1,ngrid2d))
!srflag
      ngrid2d=ngrid2d+1
      CALL uninterprez(1,kmsk,buffo,sfc_fld%SRFLAG,
     &       global_lats_r,lonsperlar,buff_mult_piecea2d(1,1,ngrid2d))
!snwdph
      ngrid2d=ngrid2d+1
      CALL uninterprez(1,kmsk,buffo,sfc_fld%SNWDPH,
     &       global_lats_r,lonsperlar,buff_mult_piecea2d(1,1,ngrid2d))
!slc
!      write(0,*)'in wrt phy, before stc,ngrid2d=',ngrid2d,'ngrid3d=',
!     &   ngrid3d,'lsoil=',lsoil
      DO k=1,LSOIL
        buffi(:,:) = sfc_fld%SLC(k,:,:)
        ngrid3d=ngrid3d+1
        CALL uninterprez(1,kmsk,buffo,buffi,global_lats_r,lonsperlar,
     &         buff_mult_piecea3d(1,1,ngrid3d))
      ENDDO
!shdmin
      ngrid2d=ngrid2d+1
      CALL uninterprez(1,kmsk,buffo,sfc_fld%SHDMIN,
     &       global_lats_r,lonsperlar,buff_mult_piecea2d(1,1,ngrid2d))
!shdmax
      ngrid2d=ngrid2d+1
      CALL uninterprez(1,kmsk,buffo,sfc_fld%SHDMAX,
     &       global_lats_r,lonsperlar,buff_mult_piecea2d(1,1,ngrid2d))
!slope
      ngrid2d=ngrid2d+1
      CALL uninterprez(1,kmsk,buffo,sfc_fld%SLOPE,
     &       global_lats_r,lonsperlar,buff_mult_piecea2d(1,1,ngrid2d))
!snoalb
      ngrid2d=ngrid2d+1
      CALL uninterprez(1,kmsk,buffo,sfc_fld%SNOALB,
     &       global_lats_r,lonsperlar,buff_mult_piecea2d(1,1,ngrid2d))
!lu: the addition of 8 Noah records ends here .........................
!
! Oro
      ngrid2d=ngrid2d+1
      CALL uninterprez(1,kmsk,buffo,sfc_fld%ORO,
     &       global_lats_r,lonsperlar,buff_mult_piecea2d(1,1,ngrid2d))
!
  999 continue
      return
      end
       subroutine sfc_only_move(ioproc)
!
!***********************************************************************
!
      use resol_def, ONLY: ngrids_flx, ngrids_sfcc, lonr,latr
     &                    ,ngrids_sfcc2d,ngrids_sfcc3d,ngrids_sfcc
      use mod_state, ONLY: buff_mult_pieces,buff_mult_piece,
     &                     buff_mult_piecea2d,
     &                     buff_mult_piecea3d, 
     &                     ivar_global_a, ivar_global
      use layout1,   ONLY: nodes, ipt_lats_node_r, lats_node_r, 
     &                     lats_node_r_max, me, nodes_comp
      use mpi_def,   ONLY: mpi_comm_null, mpi_r_io, mc_comp, 
     &                     mpi_integer, mpi_comm_all, liope, 
     &                     info, stat
      implicit none
!
      integer ipt_lats_node_rl,nodesr
      integer lats_nodes_rl
!     integer lats_nodes_r(nodes),ipt,maxfld,ioproc,nproct
      integer ioproc
      integer proc,j,lat,msgtag,nproc,i,msgtag1,buff,startlat,ierr
      integer illen,ubound,nd1
      integer icount
      data icount/0/
      save icount
      integer maxlats_comp
!  allocate the data structures
!
      if(icount .eq. 0) then
         allocate(ivar_global(10))
         allocate(ivar_global_a(10,nodes))
         ivar_global(1)=ipt_lats_node_r
         ivar_global(2)= lats_node_r
         ivar_global(3)=lats_node_r_max
         call mpi_gather(ivar_global,10,MPI_INTEGER,
     1       ivar_global_a,10,MPI_INTEGER,ioproc,mc_comp,ierr)
         if(me==ioproc) write(0,*)'in sfc_only_move, ivar_global_a=',
     &     ivar_global_a(1:3,1:nodes)
         icount=icount+1
      endif
!!
      if(allocated(buff_mult_pieces)) then
          deallocate(buff_mult_pieces)
      else
          maxlats_comp=lats_node_r_max
          if(me .eq. ioproc) then
            maxlats_comp=ivar_global_a(3,1)
           endif
      endif
      if(me .eq. ioproc) then
!gwv watch this!!
          allocate
     1  (buff_mult_pieces(lonr*latr*ngrids_sfcc))
         buff_mult_pieces=0.
      endif

      if(allocated(buff_mult_piece)) then
         continue
      else
         allocate(buff_mult_piece(lonr*lats_node_r*ngrids_sfcc))
      endif                                                   
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!   SENDLOOP of grids from comp processors to I/O task.  The
!   I/O task may or may not be a comp task also.  The
!   send logic on that task is different for these two cases
!
!  big send
!     if(me .gt. -1) return
!
       buff_mult_piece=0.
       buff_mult_piece(1:lonr*lats_node_r*ngrids_sfcc2d)=
     1 reshape(buff_mult_piecea2d(1:lonr,1:lats_node_r,1:ngrids_sfcc2d),
     1   (/lonr*lats_node_r*ngrids_sfcc2d/)) 
       buff_mult_piece(lonr*lats_node_r*ngrids_sfcc2d+1:
     1    lonr*lats_node_r*ngrids_sfcc)=
     1 reshape(buff_mult_piecea3d(1:lonr,1:lats_node_r,1:ngrids_sfcc3d),
     1   (/lonr*lats_node_r*ngrids_sfcc3d/) )
!
      IF (ME .ne. ioproc) THEN    !   Sending the data
         msgtag=me
         illen=lats_node_r
         CALL mpi_send            !  send the local grid domain
     &(buff_mult_piece,illen*lonr*ngrids_sfcc,MPI_R_IO,ioproc,
     &                  msgtag,MPI_COMM_ALL,info)
      ELSE
        if( MC_COMP .ne. MPI_COMM_NULL) then
!
c iotask is also a compute task.  send is replaced with direct
c  array copy
!
         if(nodes_comp==1) then
           buff_mult_pieces(1:lonr*lats_node_r*ngrids_sfcc)=
     1     buff_mult_piece(1:lonr*lats_node_r*ngrids_sfcc)
!                              END COMPUTE TASKS PORTION OF LOGIC
         else
!
!  END COMPUTE TASKS PORTION OF LOGIC
!  receiving part of I/O task, ioproc is the last fcst pe
!
!!
!!      for pes ioproc
!jw        nd1=lonr*lats_node_r*ngrids_sfcc
        nd1=0
        DO proc=1,nodes_comp
          illen=ivar_global_a(2,proc)
          if (proc.ne.ioproc+1) then
            msgtag=proc-1
            CALL mpi_recv(buff_mult_pieces(nd1+1),
     1        illen*lonr*ngrids_sfcc
     1        ,MPI_R_IO,proc-1,
     &                msgtag,MPI_COMM_ALL,stat,info)
          else
           buff_mult_pieces(nd1+1:nd1+lonr*illen*ngrids_sfcc)=
     1       buff_mult_piece(1:lonr*illen*ngrids_sfcc)
          endif
          nd1=nd1+illen*lonr*ngrids_sfcc
        enddo
        endif

       Endif
!end ioproc
      ENDIF
!!
      return
      end
!----------------------------------------------------------------------
      SUBROUTINE sfc_wrt(IOPROC,nw,cfile,xhour,idate
     &,                  global_lats_r,lonsperlar)
!!
      use sfcio_module
      use resol_def,    ONLY: lonr, latr, levs,ngrids_sfcc,
     &   ngrids_sfcc2d,lsoil,ivssfc
      use layout1,      ONLY: me
      USE machine,      ONLY: kind_io8, kind_io4
      implicit none
!!
      integer,intent(in) ::  IOPROC,nw
      character*16,intent(in) ::  cfile
      real(kind=kind_io8),intent(in) ::  xhour
      integer,intent(in) :: idate(4)
      integer,intent(in) :: GLOBAL_LATS_R(latr),lonsperlar(latr)
!
!-- local vars
      integer k,il, ngridss,ngrid,ngrid3
      CHARACTER*8 labfix(4)
      real(kind=kind_io4) yhour
      integer,save:: version
      data version/200501/
!
      type(sfcio_head) head
      type(sfcio_data) data
      integer iret
      logical first
      save head, first
      data first /.true./
      real(kind=kind_io4), target,allocatable :: buff_mult(:,:,:)
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
!    Build surface fields in to buff_mult
!
      if(.not.allocated(buff_mult)) allocate(buff_mult(lonr,latr,
     &    ngrids_sfcc))
!
      ngrid=1
      do ngridss=1,ngrids_sfcc
!     print *,' inside sfc_wrt calling unsp ngridss=',ngridss
        call unsplit2z(ioproc,ngridss,ngrids_sfcc,
     &       buff_mult(1,1,ngridss),global_lats_r)
      enddo
!    Building surface field is done
!
      if (me.eq.ioproc) then
!
        if (first) then
          head%clabsfc = CHAR(0)//CHAR(0)//CHAR(0)//CHAR(0)//
     &                   CHAR(0)//CHAR(0)//CHAR(0)//CHAR(0)
          head%latb    = latr
          head%lonb    = lonr
          head%ivs     = ivssfc
!         head%irealf  = 1
          head%lsoil   = lsoil
          call sfcio_alhead(head,iret)
          head%lpl     = lonsperlar(1:latr/2)
          if (lsoil .eq. 4) then
            head%zsoil   = (/-0.1,-0.4,-1.0,-2.0/)
          elseif (lsoil .eq. 2) then
            head%zsoil   = (/-0.1,-2.0/)
          endif
          first = .false.
        endif
        head%fhour   = xhour
        head%idate   = idate
!
        PRINT 99,nw,xhour,IDATE
99      FORMAT(1H ,'in fixio nw=',i7,2x,'HOUR=',f8.2,3x,'IDATE=',
     &  4(1X,I4))
!
        ngrid = 1
        ngrid3 = ngrids_sfcc2d+1
!
        data%tsea=>buff_mult(:,:,ngrid)
        ngrid=ngrid+1
        data%smc=>buff_mult(:,:,ngrid3:ngrid3+lsoil-1)
        ngrid3=ngrid3+lsoil
        data%sheleg=>buff_mult(:,:,ngrid)
        ngrid=ngrid+1
        data%stc=>buff_mult(:,:,ngrid3:ngrid3+lsoil-1)
        ngrid3=ngrid3+lsoil
        data%tg3=>buff_mult(:,:,ngrid)
        ngrid=ngrid+1
        data%zorl=>buff_mult(:,:,ngrid)
        ngrid=ngrid+1
        data%alvsf=>buff_mult(:,:,ngrid)
        ngrid=ngrid+1
        data%alvwf=>buff_mult(:,:,ngrid)
        ngrid=ngrid+1
        data%alnsf=>buff_mult(:,:,ngrid)
        ngrid=ngrid+1
        data%alnwf=>buff_mult(:,:,ngrid)
        ngrid=ngrid+1
        data%slmsk=>buff_mult(:,:,ngrid)
        ngrid=ngrid+1
        data%vfrac=>buff_mult(:,:,ngrid)
        ngrid=ngrid+1
        data%canopy=>buff_mult(:,:,ngrid)
        ngrid=ngrid+1
        data%f10m=>buff_mult(:,:,ngrid)
        ngrid=ngrid+1
        data%t2m=>buff_mult(:,:,ngrid)
        ngrid=ngrid+1
        data%q2m=>buff_mult(:,:,ngrid)
        ngrid=ngrid+1
        data%vtype=>buff_mult(:,:,ngrid)
        ngrid=ngrid+1
        data%stype=>buff_mult(:,:,ngrid)
        ngrid=ngrid+1
        data%facsf=>buff_mult(:,:,ngrid)
        ngrid=ngrid+1
        data%facwf=>buff_mult(:,:,ngrid)
        ngrid=ngrid+1
        data%uustar=>buff_mult(:,:,ngrid)
        ngrid=ngrid+1
        data%ffmm=>buff_mult(:,:,ngrid)
        ngrid=ngrid+1
        data%ffhh=>buff_mult(:,:,ngrid)
        ngrid=ngrid+1
!c-- XW: FOR SEA-ICE Nov04
        data%hice=>buff_mult(:,:,ngrid)
        ngrid=ngrid+1
        data%fice=>buff_mult(:,:,ngrid)
        ngrid=ngrid+1
        data%tisfc=>buff_mult(:,:,ngrid)
        ngrid=ngrid+1
!c-- XW: END SEA-ICE Nov04
!
!lu: the addition of 8 Noah-related records starts here ...............
        data%tprcp=>buff_mult(:,:,ngrid)
        ngrid=ngrid+1
        data%srflag=>buff_mult(:,:,ngrid)
        ngrid=ngrid+1
        data%snwdph=>buff_mult(:,:,ngrid)
        ngrid=ngrid+1
        data%slc=>buff_mult(:,:,ngrid3:ngrid3+lsoil-1)
        ngrid3=ngrid3+lsoil
        data%shdmin=>buff_mult(:,:,ngrid)
        ngrid=ngrid+1
        data%shdmax=>buff_mult(:,:,ngrid)
        ngrid=ngrid+1
        data%slope=>buff_mult(:,:,ngrid)
        ngrid=ngrid+1
        data%snoalb=>buff_mult(:,:,ngrid)
        ngrid=ngrid+1
!lu: the addition of 8 Noah records ends here .........................
!
        data%orog=>buff_mult(:,:,ngrid)      ! Orography
!
        call sfcio_swohdc(nw,cfile,head,data,iret)
!
!
      endif
      if(allocated(buff_mult)) deallocate(buff_mult)
!

      return
      end subroutine sfc_wrt

!------------------------------------------------------------------------------
      SUBROUTINE sfc_wrt_nemsio(ioproc,cfile,xhour,idate
     &,                  global_lats_r,lonsperlar)
!!
      use nemsio_module,only: nemsio_gfile,nemsio_init,nemsio_open,
     &    nemsio_writerec,nemsio_close
      use resol_def,    ONLY: lonr, latr, levs,ngrids_sfcc,
     &    ncld,ntrac,ntcw,ntoz,lsoil, ivssfc,thermodyn_id,sfcpress_id
      use layout1,      ONLY: me,idrt
      USE machine,      ONLY: kind_io8, kind_io4
!jw
      use gfs_physics_output, only : PHY_INT_STATE_ISCALAR,
     &    PHY_INT_STATE_RSCALAR,
     &    PHY_INT_STATE_1D_I,PHY_INT_STATE_1D_R,
     &    PHY_INT_STATE_2D_R_SFC,PHY_INT_STATE_3D_R
      implicit none
!!
      integer ioproc
      character*16 cfile
      real(kind=kind_io8) xhour
      integer idate(4),k,il, ngridss
!
      integer i,j,ndim3,N2DR,idate7(7),nrec,kount
      integer nfhour,nfminute,nfsecondd,nfsecondn
      logical  :: outtest
      integer  :: nmetavari,nmetavarr,nmetavarl,nmetaaryi,nmetaaryr
      character(16),allocatable :: recname(:),reclevtyp(:)
      integer,allocatable :: reclev(:)
      character(16),allocatable :: variname(:),varrname(:),
     &    aryiname(:),aryrname(:)
      integer,allocatable :: varival(:),aryilen(:),aryival(:,:)
      real(kind_io4),allocatable    :: varrval(:),aryrval(:,:)
      real(kind_io4),allocatable :: buff_mult(:,:,:),tmp(:)
      type(nemsio_gfile) gfileout
!
!!
      CHARACTER*8 labfix(4)
      real(kind=kind_io4) yhour
      integer,save:: version
      data version/200501/
      INTEGER              GLOBAL_LATS_R(latr), lonsperlar(latr)
!
      integer iret
      logical first
      save first
      save  recname, reclevtyp, reclev
      save nrec,nmetavari,nmetavarr,nmetaaryi,nmetaaryr,
     &     variname,varrname,aryiname,
     &     varival,varrval,aryilen,aryival
      data first /.true./
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
!    Build surface fields in to buff_mult
!
      if (me.eq.ioproc) then
!
!
         allocate(buff_mult(lonr,latr,ngrids_sfcc))
         do ngridss=1,ngrids_sfcc
           call unsplit2z(ioproc,ngridss,ngrids_sfcc,
     &       buff_mult(1,1,ngridss),global_lats_r)
         enddo
!
!    Building surface field is done
!
         if (first) then
!write out nemsio sfc file:
          nrec=ngrids_sfcc
          kount=size(PHY_INT_STATE_ISCALAR,2)
          do i=1,kount
           if(trim(PHY_INT_STATE_ISCALAR(2,i)).eq.'OGFS_PHY'
     &     .or.trim(PHY_INT_STATE_ISCALAR(2,i)).eq.'OGFS_SFC')
     &        nmetavari=nmetavari+1
          enddo
          allocate(variname(nmetavari),varival(nmetavari))
          j=0
          do i=1,kount
           if(trim(PHY_INT_STATE_ISCALAR(2,i)).eq.'OGFS_PHY' .or.
     &      trim(PHY_INT_STATE_ISCALAR(2,i)).eq.'OGFS_SFC' )then
            j=j+1
            variname(j)=trim(PHY_INT_STATE_ISCALAR(1,i))
            if(trim(variname(j))=='latr') varival(j)=latr
            if(trim(variname(j))=='lonr') varival(j)=lonr
            if(trim(variname(j))=='levs') varival(j)=levs
            if(trim(variname(j))=='ntoz') varival(j)=ntoz
            if(trim(variname(j))=='ntcw') varival(j)=ntcw
            if(trim(variname(j))=='ncld') varival(j)=ncld
            if(trim(variname(j))=='ntrac') varival(j)=ntrac
            if(trim(variname(j))=='thermodyn_id')varival(j)=thermodyn_id
            if(trim(variname(j))=='sfcpress_id') varival(j)=sfcpress_id
            if(trim(variname(j))=='lsoil') varival(j)=lsoil
            if(trim(variname(j))=='idrt') varival(j)=idrt
            if(trim(variname(j))=='ivssfc') varival(j)=ivssfc
           endif
          enddo
!!for real var::
          nmetavarr=0
          do i=1,kount
           if(trim(PHY_INT_STATE_RSCALAR(2,i)).eq.'OGFS_PHY'
     &     .or.trim(PHY_INT_STATE_RSCALAR(2,i)).eq.'OGFS_SFC')
     &     nmetavarr=nmetavarr+1
          enddo
          allocate(varrname(nmetavarr),varrval(nmetavarr))
          j=0
          do i=1,kount
           if(trim(PHY_INT_STATE_RSCALAR(2,i)).eq.'OGFS_PHY'
     &     .or.trim(PHY_INT_STATE_RSCALAR(2,i)).eq.'OGFS_SFC')then
             j=j+1
             varrname(j)=trim(PHY_INT_STATE_RSCALAR(1,i))
             if(trim(varrname(j))=='fhour') varrval(j)=xhour
           endif
          enddo
!!for 1D ary::
          nmetaaryi=0
          do i=1,kount
           if(trim(PHY_INT_STATE_1D_I(2,i)).eq.'OGFS_PHY'
     &     .or.trim(PHY_INT_STATE_1D_I(2,i)).eq.'OGFS_SFC')
     &     nmetaaryi=nmetaaryi+1
          enddo
          allocate(aryiname(nmetaaryi),aryilen(nmetaaryi))
          j=0
          do i=1,kount
           if(trim(PHY_INT_STATE_1D_I(2,i)).eq.'OGFS_PHY'
     &     .or.trim(PHY_INT_STATE_1D_I(2,i)).eq.'OGFS_SFC')then
             j=j+1
             aryiname(j)=trim(PHY_INT_STATE_1D_I(1,i))
             if(aryiname(j)=='IDATE') aryilen(j)=size(idate)
           endif
          enddo
          allocate(aryival(maxval(aryilen),nmetaaryi) )
          aryival(1:aryilen(1),1)=idate(1:aryilen(1))
!
!!for record name, levtyp and lev
          allocate (recname(nrec),reclevtyp(nrec),reclev(nrec))
          N2DR=0
          do i=1,kount
           if(trim(PHY_INT_STATE_2D_R_SFC(2,i)).eq.'OGFS_SFC')then
            N2DR=N2DR+1
            recname(N2DR)=trim(PHY_INT_STATE_2D_R_SFC(1,i))
            reclevtyp(N2DR)=trim(trim(PHY_INT_STATE_2D_R_SFC(3,i)))
            reclev(N2DR)=1
           endif
          enddo
!
          do i=1,kount
           if(trim(PHY_INT_STATE_3D_R(2,i)).eq.'OGFS_SFC')then
            ndim3=0
            if(trim(PHY_INT_STATE_3D_R(4,i)).eq.'lsoil') then
             ndim3=lsoil
            endif
            if(ndim3>0) then
             do j=1,ndim3
              N2DR=N2DR+1
              recname(N2DR)=trim(PHY_INT_STATE_3D_R(1,i))
              reclevtyp(N2DR)=trim(trim(PHY_INT_STATE_3D_R(3,i)) )
              if(trim(PHY_INT_STATE_3D_R(4,i)).eq.'lsoil') then
                reclev(N2DR)=j
              endif
             enddo
            endif
!
           endif
          enddo
!end first
          first=.false.
         endif
     
        idate7=0
        idate7(1)=idate(4)
        idate7(2)=idate(2)
        idate7(3)=idate(3)
        idate7(4)=idate(1)
        idate7(7)=100           !denominator for second
!
        nfhour=int(xhour)
        nfminute=int((xhour-nfhour)*60)
        nfsecondn=int(((xhour-nfhour)*3600-nfminute*60)*100)
        nfsecondd=100
!
        call nemsio_init()
!
        call nemsio_open(gfileout,trim(cfile),'write',
     &    iret = iret,
     &    modelname='GFS',gdatatype='bin4',
     &    idate=idate7,nrec=nrec,
     &    dimx=lonr,dimy=latr,dimz=levs,ncldt=ncld,nmeta=5,
     &    nfhour=nfhour,nfminute=nfminute,nfsecondn=nfsecondn,
     &    nfsecondd=nfsecondd,
     &    extrameta=.true.,nmetavari=nmetavari,
     &    nmetavarr=nmetavarr,
     &    nmetaaryi=nmetaaryi,nmetaaryr=nmetaaryr,
     &    variname=variname,varival=varival,varrname=varrname,
     &    varrval=varrval,
     &    aryiname=aryiname,aryilen=aryilen,aryival=aryival,
     &    ntrac=ntrac,nsoil=lsoil,idrt=idrt,
     &    recname=recname,reclevtyp=reclevtyp,reclev=reclev)
!
        allocate(tmp(lonr*latr))
        do i=1,nrec
         tmp(:)=reshape(buff_mult(:,:,i),(/lonr*latr/) )
         call nemsio_writerec(gfileout,i,tmp,iret=iret)
        enddo
        deallocate(tmp)
        deallocate(buff_mult)
!
        call nemsio_close(gfileout)
!end write pe
      endif
!
      return
      end subroutine sfc_wrt_nemsio
!------------------------------------------------------------------------------
      SUBROUTINE wrtflx_a(IOPROC,noflx,ZHOUR,FHOUR,IDATE,colat1,
     &                  SECSWR,SECLWR, sfc_fld, flx_fld, fluxr,
     &                  global_lats_r,lonsperlar,slmskful)
!!
!--  revision history
!  May 2013 S. Moorthi: fix sheleg, iceth,sndpth,gflxu in flx file
!
!
      use resol_def,               ONLY: lonr, latr, levp1, lsoil, nfxr,
     *                                   ngrids_sfcc
      use namelist_physics_def,    ONLY: lggfs3d
      use mod_state,               ONLY: buff_mult_piecef
      use layout1,                 ONLY: me, lats_node_r,lats_node_r_max
      use gfs_physics_sfc_flx_mod, ONLY: Sfc_Var_Data, Flx_Var_Data
      USE machine,             ONLY: kind_io8, kind_io4,grib_undef
      implicit none
!!
      TYPE(Sfc_Var_Data)        :: sfc_fld
      TYPE(Flx_Var_Data)        :: flx_fld
      INTEGER              GLOBAL_LATS_R(LATR)
      INTEGER              lonsperlar(LATR)
      integer   IOPROC
      real(kind=kind_io8) slmskful(lonr,lats_node_r)
      real(kind=kind_io8) slmskloc(LONR,LATS_NODE_R_max)
!!
      integer LEN,NFLD
      integer j,i,k,itop,ibot,k4,l,noflx,nundef,ngrid2d
!*    PARAMETER(NFLD=18)
!     PARAMETER(NFLD=18+6)      ! 550nm AOD added
      PARAMETER(NFLD=25+6)      ! 550nm AOD added
       integer ilpds,iyr,imo,ida,ihr,ifhr,ithr,lg,ierr
       real (kind=kind_io8) RTIMER(NFLD),rtime,rtimsw,rtimlw
       real (kind=kind_io8) colat1
       real (kind=kind_io8) cl1,secswr,zhour,fhour,seclwr
C

      real(kind=kind_io4) wrkga(lonr*latr),wrkgb(lonr*latr)
!
      INTEGER     IDATE(4), IDS(255),IENS(5)
!     INTEGER     IDATE(4)
      real (kind=kind_io8) SI(LEVP1)
!
!sela..................................................................
!* change rflux 3rd dimension from 27 to nfxr (Sarah Lu)
!*    real (kind=kind_io8)   rflux(lonr,LATS_NODE_R,27)     
      real (kind=kind_io8)   rflux(lonr,LATS_NODE_R_max,nfxr)
      real (kind=kind_io8)   glolal(lonr,LATS_NODE_R_max)
      real (kind=kind_io8)   buffo(lonr,LATS_NODE_R_max)
      real (kind=kind_io4)   buff1(lonr,latr)
      real (kind=kind_io4)   buff1l(lonr*latr)
!sela..................................................................
      real (kind=kind_io8)  FLUXR(nfxr,LONR,LATS_NODE_R)
!sela..................................................................
      integer kmsk(lonr,lats_node_r_max),kmsk0(lonr,lats_node_r_max)
      integer kmskcv(lonr,LATS_NODE_R_max)
!jws
      integer kmskgrib(lonr,lats_node_r_max)
      real(kind=kind_io4) buff_max
!jwe
!
!!
      kmsk     = nint(sfc_fld%slmsk)
      kmsk0    = 0
!
      kmskgrib = 0
      ngrid2d  = 1
!
      CALL uninterprez(1,kmsk,glolal,sfc_fld%slmsk,
     &       global_lats_r,lonsperlar,buff_mult_piecef(1,1,ngrid2d))
      slmskloc = glolal
      slmskful = buff_mult_piecef(1:lonr,1:lats_node_r,ngrid2d)
!
      do k=1,nfxr
        do j=1,LATS_NODE_R
          do i=1,lonr
           rflux(i,j,k) = fluxr(k,i,j)
          enddo
        enddo
      enddo
!!
!
      IF(FHOUR > ZHOUR) THEN
        RTIME   = 1./(3600.*(FHOUR-ZHOUR))
      ELSE
        RTIME   = 0.
      ENDIF
      IF(SECSWR > 0.) THEN
        RTIMSW   = 1./SECSWR
      ELSE
        RTIMSW   = 1.
      ENDIF
      IF(SECLWR > 0.) THEN
        RTIMLW   = 1./SECLWR
      ELSE
        RTIMLW   = 1.
      ENDIF
      RTIMER     = RTIMSW
      RTIMER(1)  = RTIMLW
!*RADFLX*
      RTIMER(20) = RTIMLW       ! CSULF_TOA
      RTIMER(22) = RTIMLW       ! CSDLF_SFC
      RTIMER(25) = RTIMLW       ! CSULF_SFC
!*RADFLX*
      CL1        = colat1
!!
!..........................................................
      glolal  = flx_fld%DUSFC*RTIME
!jw
      ngrid2d = 1
      CALL uninterprez(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar,
     &    buff_mult_piecef(1,1,ngrid2d))
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '01)Zonal compt of momentum flux (N/m**2) land and sea surface '

!..........................................................
      glolal  = flx_fld%DVSFC*RTIME
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar,
     &     buff_mult_piecef(1,1,ngrid2d))
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '02)Merid compt of momentum flux (N/m**2) land and sea surface '
!..........................................................
      glolal  = flx_fld%DTSFC*RTIME
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar,
     &     buff_mult_piecef(1,1,ngrid2d))
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '03)Sensible heat flux (W/m**2) land and sea surface           '
!..........................................................
      glolal  = flx_fld%DQSFC*RTIME
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar,
     &     buff_mult_piecef(1,1,ngrid2d))
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '04)Latent heat flux (W/m**2) land and sea surface             '
!..........................................................
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmsk0,buffo,sfc_fld%tsea,
     &       global_lats_r,lonsperlar,buff_mult_piecef(1,1,ngrid2d))
!     if(ierr.ne.0)print*,'wrtsfc gribsn ierr=',ierr,'  ',
!    x '05)Temperature (K) land and sea surface                       '
!..........................................................
      glolal(:,:) = sfc_fld%SMC(1,:,:)
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar,
     &     buff_mult_piecef(1,1,ngrid2d))
      where(nint(slmskful)/=1)
     &     buff_mult_piecef(:,:,ngrid2d)=grib_undef
!     if(ierr.ne.0)print*,'wrtsfc gribsn ierr=',ierr,'  ',
!    x '06)Volumetric soil moist content (frac) layer 10cm and 0cm    '
!..........................................................
      glolal(:,:) = sfc_fld%SMC(2,:,:)
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar,
     &     buff_mult_piecef(1,1,ngrid2d))
      where(nint(slmskful)/=1)
     &     buff_mult_piecef(:,:,ngrid2d)=grib_undef
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!lu  x '07)Volumetric soil moist content (frac) layer 200cm and 10cm  '
!    + '07)Volumetric soil moist content (frac) layer 40cm and 10cm  '
!..........................................................
      glolal(:,:) = sfc_fld%STC(1,:,:)
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar,
     &     buff_mult_piecef(1,1,ngrid2d))
      where(nint(slmskful) /= 1)
     &     buff_mult_piecef(:,:,ngrid2d) = grib_undef
      nundef   = 0
      buff_max = 0.
      do j=1,lats_node_r
        do i=1,lonr
          if(buff_mult_piecef(i,j,ngrid2d)/=grib_undef) then
            if(buff_mult_piecef(i,j,ngrid2d) >buff_max)
     &                      buff_max = buff_mult_piecef(i,j,ngrid2d)
            nundef = nundef+1
          endif
        enddo
      enddo
!      write(0,*)'in wrtsfc_a, max stc=',buff_max,' grib_undef=',
!     &   grib_undef,'nundef=',nundef
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '08)Temp (K) layer betw two depth below land sfc 10cm and 0cm  '
!..........................................................
      glolal(:,:) = sfc_fld%STC(2,:,:)
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar,
     &     buff_mult_piecef(1,1,ngrid2d))
      where(slmskful /= 1._kind_io8)
     &     buff_mult_piecef(:,:,ngrid2d) = grib_undef
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!lu  x '09)Temp (K) layer betw two depth below land sfc 200cm and 10cm'
!    + '09)Temp (K) layer betw two depth below land sfc 40cm and 10cm'
!..........................................................
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmsk,buffo,sfc_fld%sheleg,
     &       global_lats_r,lonsperlar,buff_mult_piecef(1,1,ngrid2d))
      where(nint(slmskful) == 0)
     &     buff_mult_piecef(:,:,ngrid2d) = 0.0
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '10)Water equiv of accum snow depth (kg/m**2) land sea surface '
c..........................................................
!      write(0,*)'before DLWSFC'
      glolal  = flx_fld%DLWSFC*RTIME
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar,
     &     buff_mult_piecef(1,1,ngrid2d))
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '11)Downward long wave radiation flux (W/m**2) land sea surface'
!..........................................................
      glolal  = flx_fld%ULWSFC*RTIME
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar,
     &     buff_mult_piecef(1,1,ngrid2d))
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '12)Upward long wave radiation flux (W/m**2) land sea surface  '
!..........................................................
!.......  FIX FLUXES FOR APPROX DIURNAL CYCLE
      DO 113 K=1,4
       do j=1,LATS_NODE_R
        do i=1,lonr
         glolal(i,j) = rflux(i,j,k)*RTIMER(k)
        enddo
       enddo
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar,
     &     buff_mult_piecef(1,1,ngrid2d))
!     if(ierr.ne.0.and.k.eq.1)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '13)Upward long wave radiation flux (W/m**2) top of atmosphere '
!     if(ierr.ne.0.and.k.eq.2)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '14)Upward solar radiation flux (W/m**2) top of atmosphere     '
!     if(ierr.ne.0.and.k.eq.3)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '15)Upward solar radiation flux (W/m**2) land and sea surface  '
!     if(ierr.ne.0.and.k.eq.4)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '16)Downward solar radiation flux (W/m**2) land and sea surface'
  113 CONTINUE
!..........................................................
!
!     For UV-B fluxes
!
      do j=1,LATS_NODE_R
        do i=1,lonr
          glolal(i,j) = rflux(i,j,21)*rtimsw
        enddo
      enddo
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar,
     &     buff_mult_piecef(1,1,ngrid2d))
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '17)UV-B Downward solar flux (W/m**2) land sea surface'
      do j=1,LATS_NODE_R
        do i=1,lonr
          glolal(i,j) = rflux(i,j,22)*rtimsw
        enddo
      enddo
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar,
     &     buff_mult_piecef(1,1,ngrid2d))
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '18)clear sky UV-B Downward solar flux (W/m**2) land sea surface'
!
!     End UV-B fluxes
!
!..........................................................
!..........................................................
      DO 813 K=5,7
!
       do j=1,LATS_NODE_R
        do i=1,lonr
         glolal(i,j) = rflux(i,j,k)*100.*rtimsw
        enddo
       enddo
      where(glolal.ge.0.5)
        kmskcv = 1
      elsewhere
        kmskcv = 0
      endwhere
!!
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmskcv,buffo,glolal,global_lats_r,lonsperlar,
     &     buff_mult_piecef(1,1,ngrid2d))
!      where(buff_mult_piecef(:,:,ngrid2d)<=0.5_kind_io4)
!     &    buff_mult_piecef(:,:,ngrid2d) = grib_undef
      kmskgrib = 0
      where(buff_mult_piecef(:,:,ngrid2d)<=0.5_kind_io4)
     &    kmskgrib = 1
!     if(ierr.ne.0.and.k.eq.5)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '19)Total cloud cover (percent) high cloud layer               '
!     if(ierr.ne.0.and.k.eq.6)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '23)Total cloud cover (percent) middle cloud layer             '
!     if(ierr.ne.0.and.k.eq.7)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '27)Total cloud cover (percent) low cloud layer                '
!
        K4 = 4  + (K-5)*4
        L  = K4 + 1
!
       do j=1,LATS_NODE_R
        do i=1,lonr
         if(rflux(i,j,k) > 0.) then
          glolal(i,j) = rflux(i,j,k+3)/rflux(i,j,k)
         else
          glolal(i,j) = 0.
         endif
        enddo
       enddo
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmskcv,buffo,glolal,global_lats_r,lonsperlar,
     &     buff_mult_piecef(1,1,ngrid2d))
      where(kmskgrib==1) buff_mult_piecef(:,:,ngrid2d)=grib_undef
!     if(ierr.ne.0.and.k.eq.5)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '20)Pressure (Pa) high cloud top level                         '
!     if(ierr.ne.0.and.k.eq.6)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '24)Pressure (Pa) middle cloud top level                       '
!     if(ierr.ne.0.and.k.eq.7)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '28)Pressure (Pa) low cloud top level                          '
        L = K4 + 2
!
       do j=1,LATS_NODE_R
        do i=1,lonr
         if(rflux(i,j,k) > 0.)then
          glolal(i,j) = rflux(i,j,k+6)/rflux(i,j,k)
         else
          glolal(i,j) = 0.
         endif
        enddo
       enddo
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmskcv,buffo,glolal,global_lats_r,lonsperlar,
     &     buff_mult_piecef(1,1,ngrid2d))
      where(kmskgrib==1) buff_mult_piecef(:,:,ngrid2d)=grib_undef
!     if(ierr.ne.0.and.k.eq.5)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '21)Pressure (Pa) high cloud bottom level                      '
!     if(ierr.ne.0.and.k.eq.6)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '25)Pressure (Pa) middle cloud bottom level                    '
!     if(ierr.ne.0.and.k.eq.7)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '29)Pressure (Pa) low cloud bottom level                       '
        L = K4 + 3
!
       do j=1,LATS_NODE_R
        do i=1,lonr
         if(rflux(i,j,k) > 0.)then
          glolal(i,j) = rflux(i,j,k+9)/rflux(i,j,k)
         else
          glolal(i,j) = 0.
         endif
        enddo
       enddo
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmskcv,buffo,glolal,global_lats_r,lonsperlar,
     &     buff_mult_piecef(1,1,ngrid2d))
      where(kmskgrib==1) buff_mult_piecef(:,:,ngrid2d)=grib_undef
!     if(ierr.ne.0.and.k.eq.5)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '22)Temperature (K) high cloud top level                       '
!     if(ierr.ne.0.and.k.eq.6)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '26)Temperature (K) middle cloud top level                     '
!     if(ierr.ne.0.and.k.eq.7)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '30)Temperature (K) low cloud top level                        '
        L = K4 + 4
!
  813 CONTINUE
!!
!...................................................................
      glolal = flx_fld%GESHEM*1.E3*RTIME
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar,
     &     buff_mult_piecef(1,1,ngrid2d))
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '31)Precipitation rate (kg/m**2/s) land and sea surface        '
c...................................................................
      glolal = flx_fld%BENGSH*1.E3*RTIME
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar,
     &     buff_mult_piecef(1,1,ngrid2d))
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '32)Convective precipitation rate (kg/m**2/s) land sea surface '
!...................................................................
      glolal  = flx_fld%GFLUX*RTIME
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar,
     &     buff_mult_piecef(1,1,ngrid2d))
      where(slmskful==0._kind_io8)
     &     buff_mult_piecef(:,:,ngrid2d) = grib_undef
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '33)Ground heat flux (W/m**2) land and sea surface             '
!...................................................................
!     buffo=MOD(slmskloc,2._kind_io8)
!gwv   add something here
!     do j=1,lats_node_r
!       do i=1,lonr
!         buff_mult_piecea(i,ngrid,j)=buffo(i,j)
!       end do
!     end do
!     ngrid=ngrid+1
!...................................................................
!     Add land/sea mask here
      ngrid2d=ngrid2d+1
      buffo=MOD(slmskloc,2._kind_io8)
      do j=1,lats_node_r
        do i=1,lonr
!jw          buff_mult_piecea(i,ngrid,j) = buffo(i,j)
          buff_mult_piecef(i,j,ngrid2d) = buffo(i,j)
        end do
      end do
!jw        ngrid=ngrid+1
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '34)Land-sea mask (1=land; 0=sea) (integer) land sea surface   '
!gwv   add something here
!
!c-- XW: FOR SEA-ICE Nov04
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmsk0,buffo,sfc_fld%fice,
     &       global_lats_r,lonsperlar,buff_mult_piecef(1,1,ngrid2d))
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '35)Ice concentration (ice>0; no ice=0) (1/0) land sea surface '
!c-- XW: END SEA-ICE
!...................................................................
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmsk0,buffo,flx_fld%u10m,
     &       global_lats_r,lonsperlar,buff_mult_piecef(1,1,ngrid2d))
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '36)u wind (m/s) height above ground                           '
!...................................................................
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmsk0,buffo,flx_fld%v10m,
     &       global_lats_r,lonsperlar,buff_mult_piecef(1,1,ngrid2d))
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '37)v wind (m/s) height above ground                           '
!...................................................................
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmsk0,buffo,sfc_fld%t2m,
     &       global_lats_r,lonsperlar,buff_mult_piecef(1,1,ngrid2d))
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '38)Temperature (K) height above ground                        '
!...................................................................
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmsk0,buffo,sfc_fld%q2m,
     &       global_lats_r,lonsperlar,buff_mult_piecef(1,1,ngrid2d))
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '39)Specific humidity (kg/kg) height above ground              '
!...................................................................
      glolal  = flx_fld%PSURF
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar,
     &     buff_mult_piecef(1,1,ngrid2d))
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '40)Pressure (Pa) land and sea surface                         '
!...................................................................
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmsk0,buffo,flx_fld%tmpmax,
     &       global_lats_r,lonsperlar,buff_mult_piecef(1,1,ngrid2d))
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '41)Maximum temperature (K) height above ground                '
!...................................................................
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmsk0,buffo,flx_fld%tmpmin,
     &       global_lats_r,lonsperlar,buff_mult_piecef(1,1,ngrid2d))
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '42)Minimum temperature (K) height above ground                '
!...................................................................
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmsk0,buffo,flx_fld%spfhmax,
     &       global_lats_r,lonsperlar,buff_mult_piecef(1,1,ngrid2d))
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '41a)Maximum specific humidity (kg/kg) height above ground      '
!...................................................................
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmsk0,buffo,flx_fld%spfhmin,
     &       global_lats_r,lonsperlar,buff_mult_piecef(1,1,ngrid2d))
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '42a)Minimum specific humidity (kg/kg) height above ground      '
!...................................................................
      glolal = flx_fld%RUNOFF * 1.E3
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar,
     &     buff_mult_piecef(1,1,ngrid2d))
      where(slmskful == 0._kind_io8)
     &     buff_mult_piecef(:,:,ngrid2d) = grib_undef
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '43)Runoff (kg/m**2) land and sea surface                      '
!...................................................................
      glolal  = flx_fld%EP * RTIME
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar,
     &     buff_mult_piecef(1,1,ngrid2d))
      where(slmskful == 0._kind_io8)
     &     buff_mult_piecef(:,:,ngrid2d) = grib_undef
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '44)Potential evaporation rate (w/m**/) land and sea surface   '
!...................................................................
      glolal  = flx_fld%CLDWRK * RTIME
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar,
     &     buff_mult_piecef(1,1,ngrid2d))
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '45)Cloud work function (J/Kg) total atmospheric column        '
!...................................................................
      glolal  = flx_fld%DUGWD*RTIME
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar,
     &     buff_mult_piecef(1,1,ngrid2d))
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '46)Zonal gravity wave stress (N/m**2) land and sea surface    '
!...................................................................
      glolal  = flx_fld%DVGWD*RTIME
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar,
     &     buff_mult_piecef(1,1,ngrid2d))
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '47)Meridional gravity wave stress (N/m**2) land sea surface   '
!...................................................................
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmsk0,buffo,flx_fld%hpbl,
     &       global_lats_r,lonsperlar,buff_mult_piecef(1,1,ngrid2d))
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '48)Boundary layer height '
!...................................................................
!hmhj CALL uninterprez(2,kmsk0,buffo,flx_fld%pwat,
!hmhj&                 global_lats_r,lonsperlar)
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmsk0,buffo,flx_fld%pwat,
     &       global_lats_r,lonsperlar,buff_mult_piecef(1,1,ngrid2d))
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '49)Precipitable water (kg/m**2) total atmospheric column      '
!...................................................................
!
       do j=1,LATS_NODE_R
        do i=1,lonr
         if (rflux(i,j,4) > 0.) then
          glolal(i,j) = rflux(i,j,3)/rflux(i,j,4) * 100.
          if (glolal(i,j) > 100.) glolal(i,j) = 100.
         else
          glolal(i,j) = 0.
         endif
        enddo
       enddo
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar,
     &     buff_mult_piecef(1,1,ngrid2d))
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '50)Albedo (percent) land and sea surface                      '
!
       do j=1,LATS_NODE_R
        do i=1,lonr
         glolal(i,j) = rflux(i,j,26)*100.*rtimsw
        enddo
       enddo
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar,
     &     buff_mult_piecef(1,1,ngrid2d))
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '51)Total cloud cover (percent) total atmospheric column       '
!
! CONVECTIVE CLOUDS
! LABELED INSTANTANEOUS BUT ACTUALLY AVERAGED OVER FHSWR HOURS
!
      glolal = sfc_fld%CV*1.E2
      where(glolal >= 0.5)
        kmskcv = 1
      elsewhere
        kmskcv = 0
      endwhere
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmskcv,buffo,glolal,global_lats_r,lonsperlar,
     &     buff_mult_piecef(1,1,ngrid2d))
      kmskgrib = 0
      where(buff_mult_piecef(:,:,ngrid2d)<0.5_kind_io8)
     &     kmskgrib = 1
!      where(buff_mult_piecef(:,:,ngrid2d)<0.5_kind_io8)
!     &     buff_mult_piecef(:,:,ngrid2d)=grib_undef

!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '52)Total cloud cover (percent) convective cloud layer         '
!.................................................
       do j=1,LATS_NODE_R
        do i=1,lonr
        glolal(i,j) = 0.
        IF(sfc_fld%CV(i,j) > 0.) THEN
!        ITOP=NINT(CVT(i,j))
!        IF(ITOP.GE.1.AND.ITOP.LE.LEVS)
!    &   glolal(i,j)=SI(ITOP+1)*PSURF(i,j)*1.E3
!...      cvt already a pressure (cb)...convert to Pa
!        glolal(i,j) = sfc_fld%CVT(i,j)*1.E3
         glolal(i,j) = sfc_fld%CVT(i,j)     ! already Pa
        END IF
       ENDDO
      ENDDO
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmskcv,buffo,glolal,global_lats_r,lonsperlar,
     &     buff_mult_piecef(1,1,ngrid2d))
      where(kmskgrib == 1)
     &     buff_mult_piecef(:,:,ngrid2d)=grib_undef
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '53)Pressure (Pa) convective cloud top level                   '
!.................................................
       do j=1,LATS_NODE_R
        do i=1,lonr
        glolal(i,j) = 0.
        IF(sfc_fld%CV(i,j) > 0.) THEN
!        Ibot=NINT(CVB(i,j))
!        IF(Ibot.GE.1.AND.Ibot.LE.LEVS)
!    &   glolal(i,j)=SI(IBOT)*PSURF(i,j)*1.E3
!...      cvb already a pressure (cb)...convert to Pa
!        glolal(i,j) = sfc_fld%CVB(i,j)*1.E3
         glolal(i,j) = sfc_fld%CVB(i,j)      ! already Pa
        END IF
       ENDDO
      ENDDO
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmskcv,buffo,glolal,global_lats_r,lonsperlar,
     &     buff_mult_piecef(1,1,ngrid2d))
      where(kmskgrib == 1)
     &     buff_mult_piecef(:,:,ngrid2d) = grib_undef
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '54)Pressure (Pa) convective cloud bottom level                '
!.................................................
!...   SAVE B.L. CLOUD AMOUNT
!
       do j=1,LATS_NODE_R
        do i=1,lonr
         glolal(i,j) = rflux(i,j,27)*100.*rtimsw
        enddo
       enddo
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar,
     &     buff_mult_piecef(1,1,ngrid2d))
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '55)Total cloud cover (percent) boundary layer cloud layer     '
!c-- XW: FOR SEA-ICE Nov04
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmsk0,buffo,sfc_fld%hice,
     &       global_lats_r,lonsperlar,buff_mult_piecef(1,1,ngrid2d))
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
      where(nint(slmskful) == 1)
     &     buff_mult_piecef(:,:,ngrid2d)=grib_undef
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '56)Sea ice thickness (m) category 1'
!c-- XW: END SEA-ICE
!.................................................
!lu: add smc(3:4), stc(3:4), slc(1:4), snwdph, canopy
!lu: addition of 10 records starts here -------------------------------
      if(lsoil > 2)then
        glolal(:,:) = sfc_fld%SMC(3,:,:)
        ngrid2d = ngrid2d+1
        CALL uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar,
     &     buff_mult_piecef(1,1,ngrid2d))
      where(nint(slmskful) /= 1)
     &     buff_mult_piecef(:,:,ngrid2d) = grib_undef
!       if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    &   '57)Volumetric soil moist content (frac) layer 100cm and 40cm '
!..........................................................
        glolal(:,:) = sfc_fld%SMC(4,:,:)
        ngrid2d = ngrid2d+1
        CALL uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar,
     &     buff_mult_piecef(1,1,ngrid2d))
      where(nint(slmskful) /= 1)
     &     buff_mult_piecef(:,:,ngrid2d) = grib_undef
!       if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    &   '58)Volumetric soil moist content (frac) layer 200cm and 100cm '
!..........................................................
        glolal(:,:) = sfc_fld%STC(3,:,:)
        ngrid2d = ngrid2d+1
        CALL uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar,
     &     buff_mult_piecef(1,1,ngrid2d))
      where(nint(slmskful) /= 1)
     &     buff_mult_piecef(:,:,ngrid2d) = grib_undef
!       if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    &   '59)Temp (K) layer betw two depth below land sfc 100cm and 40cm'
!..........................................................
        glolal(:,:) = sfc_fld%STC(4,:,:)
        ngrid2d = ngrid2d+1
        CALL uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar,
     &     buff_mult_piecef(1,1,ngrid2d))
      where(nint(slmskful) /= 1)
     &     buff_mult_piecef(:,:,ngrid2d) = grib_undef
!       if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    &   '60)Temp (K) layer betw two depth below land sfc 200cm and 100cm'
      endif
!..........................................................
      glolal(:,:) = sfc_fld%SLC(1,:,:)
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar,
     &     buff_mult_piecef(1,1,ngrid2d))
      where(nint(slmskful) /= 1)
     &     buff_mult_piecef(:,:,ngrid2d)=grib_undef
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '61)Liquid soil moist content (frac) layer 10cm and 0cm  '
!..........................................................
      glolal(:,:) = sfc_fld%SLC(2,:,:)
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar,
     &     buff_mult_piecef(1,1,ngrid2d))
      where(nint(slmskful) /=1 )
     &     buff_mult_piecef(:,:,ngrid2d) = grib_undef
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '62)Liquid soil moist content (frac) layer 40cm and 10cm '
!..........................................................
      if(lsoil.gt.2)then
        glolal(:,:) = sfc_fld%SLC(3,:,:)
      ngrid2d = ngrid2d+1
        CALL uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar,
     &     buff_mult_piecef(1,1,ngrid2d))
      where(nint(slmskful) /= 1)
     &     buff_mult_piecef(:,:,ngrid2d) = grib_undef
!       if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    &   '63)Liquid soil moist content (frac) layer 100cm and 40cm'
!..........................................................
        glolal(:,:) = sfc_fld%SLC(4,:,:)
      ngrid2d = ngrid2d+1
        CALL uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar,
     &     buff_mult_piecef(1,1,ngrid2d))
      where(nint(slmskful) /= 1)
     &     buff_mult_piecef(:,:,ngrid2d) = grib_undef
!       if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    &   '64)Liquid soil moist content (frac) layer 200cm and 100cm'
      endif
!..........................................................
      glolal = sfc_fld%SNWDPH / 1.E3       !! convert from mm to m
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar,
     &     buff_mult_piecef(1,1,ngrid2d))
      where(nint(slmskful) == 0)
     &     buff_mult_piecef(:,:,ngrid2d) = grib_undef
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    & '65)Snow depth (m) land surface 
c..........................................................
!     LBM=slmskful.EQ.1._kind_io8
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmsk,buffo,sfc_fld%canopy,
     &       global_lats_r,lonsperlar,
     &       buff_mult_piecef(1,1,ngrid2d))
      where(nint(slmskful) /= 1)
     &     buff_mult_piecef(:,:,ngrid2d) = grib_undef
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    & '66)Canopy water content (kg/m^2) land surface      '
!lu: addition of 10 records ends here -------------------------------
!
!wei: addition of 30 records starts here -------------------------------
      glolal  = sfc_fld%ZORL / 1.E2       !! convert from cm to m
      ngrid2d = ngrid2d+1
      CALL uninterprez(1,kmsk0,buffo,glolal,global_lats_r,lonsperlar,
     &     buff_mult_piecef(1,1,ngrid2d))
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    & '67)Surface roughness (m)       '
!..........................................................
      glolal  = sfc_fld%vfrac*100.
      ngrid2d = ngrid2d+1
      CALL uninterprez(1,kmsk,buffo,glolal,global_lats_r,lonsperlar,
     &     buff_mult_piecef(1,1,ngrid2d))
      where(nint(slmskful) /= 1)
     &     buff_mult_piecef(:,:,ngrid2d)=grib_undef
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    & '68)Vegetation fraction (fractional) land surface      '
!..........................................................
      ngrid2d = ngrid2d+1
      CALL uninterprez(1,kmsk,glolal,sfc_fld%vtype,
     &       global_lats_r,lonsperlar,buff_mult_piecef(1,1,ngrid2d))
      buffo = MOD(glolal,2._kind_io8)
      where(nint(slmskful) /= 1)
     &     buff_mult_piecef(:,:,ngrid2d) = grib_undef
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    & '69)Vegetation type land surface      '
!..........................................................
      ngrid2d = ngrid2d+1
      CALL uninterprez(1,kmsk,glolal,sfc_fld%stype,
     &       global_lats_r,lonsperlar,buff_mult_piecef(1,1,ngrid2d))
      buffo=MOD(glolal,2._kind_io8)
      where(nint(slmskful) /= 1)
     &     buff_mult_piecef(:,:,ngrid2d)=grib_undef
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    & '70)Soil type land surface      '
!..........................................................
      ngrid2d = ngrid2d+1
      CALL uninterprez(1,kmsk,glolal,sfc_fld%slope,
     &       global_lats_r,lonsperlar,buff_mult_piecef(1,1,ngrid2d))
      buffo = MOD(glolal,2._kind_io8)
      where(nint(slmskful) /= 1)
     &     buff_mult_piecef(:,:,ngrid2d) = grib_undef
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    & '71)Slope type land surface      '
!..........................................................
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmsk0,buffo,sfc_fld%uustar,
     &       global_lats_r,lonsperlar,buff_mult_piecef(1,1,ngrid2d))
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    & '72)Frictional velocity (m/s)     '
!..........................................................
      ngrid2d = ngrid2d+1
      CALL uninterprez(1,kmsk,buffo,sfc_fld%oro,
     &       global_lats_r,lonsperlar,buff_mult_piecef(1,1,ngrid2d))
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    & '73)Surface height (m)       '
!..........................................................
      ngrid2d = ngrid2d+1
      CALL uninterprez(1,kmsk,buffo,sfc_fld%srflag,
     &       global_lats_r,lonsperlar,buff_mult_piecef(1,1,ngrid2d))
      where(nint(slmskful) /= 1)
     &     buff_mult_piecef(:,:,ngrid2d)=grib_undef
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    & '74)Freezing precip flag land surface      '
!..........................................................
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmsk0,buffo,flx_fld%chh,
     &       global_lats_r,lonsperlar,buff_mult_piecef(1,1,ngrid2d))
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    & '75)Exchange coefficient CH(m/s)       '
!..........................................................
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmsk0,buffo,flx_fld%cmm,
     &       global_lats_r,lonsperlar,buff_mult_piecef(1,1,ngrid2d))
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    & '76)Exchange coefficient CM(m/s)       '
!..........................................................
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmsk,buffo,flx_fld%EPI,
     &       global_lats_r,lonsperlar,buff_mult_piecef(1,1,ngrid2d))
      where(nint(slmskful) /= 1)
     &     buff_mult_piecef(:,:,ngrid2d) = grib_undef
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    & '77)Potential evaporation rate (w/m**2) land and sea surface   '
!..........................................................
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmsk0,buffo,flx_fld%DLWSFCI,
     &       global_lats_r,lonsperlar,buff_mult_piecef(1,1,ngrid2d))
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    & '78)Downward long wave radiation flux (W/m**2) '
!..........................................................
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmsk0,buffo,flx_fld%ULWSFCI,
     &       global_lats_r,lonsperlar,buff_mult_piecef(1,1,ngrid2d))
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    & '79)Upward long wave radiation flux (W/m**2)  '
!..........................................................
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmsk0,buffo,flx_fld%USWSFCI,
     &       global_lats_r,lonsperlar,buff_mult_piecef(1,1,ngrid2d))
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    & '80)Upward short wave radiation flux (W/m**2)  '
!..........................................................
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmsk0,buffo,flx_fld%DSWSFCI,
     &       global_lats_r,lonsperlar,buff_mult_piecef(1,1,ngrid2d))
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    & '81)Downward short wave radiation flux (W/m**2)   '
!..........................................................
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmsk0,buffo,flx_fld%DTSFCI,
     &       global_lats_r,lonsperlar,buff_mult_piecef(1,1,ngrid2d))
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    & '82)Sensible heat flux (W/m**2) land and sea surface       '
!..........................................................
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmsk0,buffo,flx_fld%DQSFCI,
     &       global_lats_r,lonsperlar,buff_mult_piecef(1,1,ngrid2d))
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    & '83)Latent heat flux (W/m**2) land and sea surface         '
!..........................................................
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmsk,buffo,flx_fld%GFLUXI,
     &       global_lats_r,lonsperlar,buff_mult_piecef(1,1,ngrid2d))
      where(nint(slmskful) == 0)
     &     buff_mult_piecef(:,:,ngrid2d) = grib_undef
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    & '84)Ground heat flux (W/m**2) land and sea surface         '
!..........................................................
      glolal  = flx_fld%SRUNOFF * 1.E3
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar,
     &    buff_mult_piecef(1,1,ngrid2d))
      where(nint(slmskful) /= 1)
     &     buff_mult_piecef(:,:,ngrid2d) = grib_undef
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    & '85)Surface runoff (kg/m^2) land surface      '
!..........................................................
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmsk0,buffo,flx_fld%t1,
     &       global_lats_r,lonsperlar,buff_mult_piecef(1,1,ngrid2d))
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '86)Lowest model level Temp (K)      '
!..........................................................
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmsk0,buffo,flx_fld%q1,
     &       global_lats_r,lonsperlar,buff_mult_piecef(1,1,ngrid2d))
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    & '87)Lowest model specific humidity (kg/kg)    '
!..........................................................
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmsk0,buffo,flx_fld%u1,
     &       global_lats_r,lonsperlar,buff_mult_piecef(1,1,ngrid2d))
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '88)Lowest model u wind (m/s)      '
!..........................................................
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmsk0,buffo,flx_fld%v1,
     &       global_lats_r,lonsperlar,buff_mult_piecef(1,1,ngrid2d))
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    & '89)Lowest model v wind (m/s)       '
!..........................................................
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmsk,buffo,flx_fld%zlvl,
     &       global_lats_r,lonsperlar,buff_mult_piecef(1,1,ngrid2d))
      where(nint(slmskful) /= 1)
     &     buff_mult_piecef(:,:,ngrid2d) = grib_undef
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '90)Lowest model level height (m) land surface      '
!..........................................................
      glolal  = flx_fld%EVBSA*RTIME
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar,
     &     buff_mult_piecef(1,1,ngrid2d))
      where(nint(slmskful) /= 1)
     &     buff_mult_piecef(:,:,ngrid2d) = grib_undef
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    & '91)Direct evaporation from bare soil(W/m^2) land surface      '
!..........................................................
      glolal  = flx_fld%EVCWA*RTIME
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar,
     &     buff_mult_piecef(1,1,ngrid2d))
      where(nint(slmskful) /=1 )
     &     buff_mult_piecef(:,:,ngrid2d) = grib_undef
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    & '92)Canopy water evaporation(W/m^2) land surface      '
!..........................................................
      glolal  = flx_fld%TRANSA*RTIME
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar,
     &     buff_mult_piecef(1,1,ngrid2d))
      where(nint(slmskful) /= 1)
     &     buff_mult_piecef(:,:,ngrid2d) = grib_undef
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    & '93)Transpiration (W/m^2) land surface      '
!..........................................................
      glolal  = flx_fld%SBSNOA*RTIME
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar,
     &     buff_mult_piecef(1,1,ngrid2d))
      where(nint(slmskful) /= 1)
     &     buff_mult_piecef(:,:,ngrid2d) = grib_undef
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    & '94)Snow Sublimation (W/m^2) land surface      '
!..........................................................
      glolal  = flx_fld%SNOWCA*RTIME*100.
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar,
     &     buff_mult_piecef(1,1,ngrid2d))
      where(nint(slmskful) /= 1)
     &     buff_mult_piecef(:,:,ngrid2d) = grib_undef
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    & '95)Snow Cover (fraction) land surface      '
!..........................................................
      glolal  = flx_fld%soilm*1.E3       !! convert from m to (mm)kg/m^2
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar,
     &     buff_mult_piecef(1,1,ngrid2d))
      where(nint(slmskful) /= 1)
     &     buff_mult_piecef(:,:,ngrid2d) = grib_undef
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    & '96)Total column soil moisture (Kg/m^2) land surface      '

Cwei: addition of 30 records ends here -------------------------------

!*RADFLX*
!Clu: addition of 7 records starts here -------------------------------
!dswrf_toa, csulf_toa, csusf_toa, csdlf_sfc,csusf_sfc, csdsf_sfc, csulf_sfc
!
      DO 115 K=19, 25
       if(K .eq. 19)  then
          L = 18
        else
          L = K + 8
       endif
       do j=1,LATS_NODE_R
        do i=1,lonr
         glolal(i,j)=rflux(i,j,L)*RTIMER(K)
        enddo
       enddo
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar,
     &     buff_mult_piecef(1,1,ngrid2d))
!     if(ierr.ne.0.and.k.eq.19)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '97)Downward solar radiation flux (W/m**2) TOA '
!     if(ierr.ne.0.and.k.eq.20)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',

!    x '98)CS upward long wave radiation flux (W/m**2) TOA '
!     if(ierr.ne.0.and.k.eq.21)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '99)CS upward solar radiation flux (W/m**2) TOA     '
!     if(ierr.ne.0.and.k.eq.22)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '100)CS downward long radiation flux (W/m**2) SFC  '
!     if(ierr.ne.0.and.k.eq.23)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '101)CS upward solar radiation flux (W/m**2)  SFC '
!     if(ierr.ne.0.and.k.eq.24)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '102)CS downward solar radiation flux (W/m**2) SFC'
!     if(ierr.ne.0.and.k.eq.25)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '103)CS upward long wave radiation flux (W/m**2) SFC '

  115 CONTINUE
!..........................................................
      glolal  = flx_fld%snohfa*RTIME
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar,
     &     buff_mult_piecef(1,1,ngrid2d))
      where(nint(slmskful) /= 1)
     &     buff_mult_piecef(:,:,ngrid2d) = grib_undef
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '104)Snow phase-change heat flux [W/m^2] land surface      '
!..........................................................
      glolal  = flx_fld%smcwlt2
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar,
     &     buff_mult_piecef(1,1,ngrid2d))
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '105)Wilting point [fraction] land surface      '
!..........................................................
      glolal  = flx_fld%smcref2
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar,
     &     buff_mult_piecef(1,1,ngrid2d))
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '106)Field capacity [fraction] land surface      '
!..........................................................

!lu: addition of 7 records ends here ---------------------------------
!..........................................................
!
!    accumulated sunshine time
!
      glolal  = flx_fld%suntim
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar,
     &     buff_mult_piecef(1,1,ngrid2d))
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '107)Accumulated sunshine duration (sec)'
!
!    end sunshine time
!
Clu: addition of 6 aod fields starts here ---------------------------
       do k = nfxr-5, nfxr
         do j=1,LATS_NODE_R
           do i=1,lonr
             glolal(i,j) = rflux(i,j,k)*RTIMER(k-15)
           enddo
         enddo
       ngrid2d = ngrid2d+1
       CALL uninterprez(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar,
     &     buff_mult_piecef(1,1,ngrid2d))
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '108)Total Aerosol optical depth at 550nm land sea surface'
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '109)DU Aerosol optical depth at 550nm land sea surface'
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '110)BC Aerosol optical depth at 550nm land sea surface'
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '111)OC Aerosol optical depth at 550nm land sea surface'
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '112)SU Aerosol optical depth at 550nm land sea surface'
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '113)SS Aerosol optical depth at 550nm land sea surface'
       enddo
Clu: addition of 6 aod fields ends here -----------------------------
!
!...................................................................
! Output additional variable (averaged quantity) for GOCART
! If LGGFS3D = .TRUE.
!
      IF ( LGGFS3D ) THEN
!
!hchuang code change [+16L] 11/12/2007 :
!..........................................................
      glolal  = flx_fld%gsoil*rtime !! gsoil already in mm (kg/m^2)
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar,
     &     buff_mult_piecef(1,1,ngrid2d))
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '114)Average VOL soil moist content(frac) layer 10cm -> 0cm'
!..........................................................
      glolal  = flx_fld%gtmp2m*rtime
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar,
     &     buff_mult_piecef(1,1,ngrid2d))
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '115)Average temperature at 2 meter (K)                    '
!..........................................................
      glolal  = flx_fld%gustar*rtime
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar,
     &     buff_mult_piecef(1,1,ngrid2d))
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '116)Average Frictional Velocity (m/s)                     '
!..........................................................
      glolal  = flx_fld%gpblh*rtime
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar,
     &     buff_mult_piecef(1,1,ngrid2d))
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '117)Average Boundary layer height                        '
!..........................................................
      glolal  = flx_fld%gu10m*rtime
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar,
     &     buff_mult_piecef(1,1,ngrid2d))
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '118)Average u wind (m/s) height 10m above ground         '
!..........................................................
      glolal  = flx_fld%gv10m*rtime
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar,
     &     buff_mult_piecef(1,1,ngrid2d))
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '119)Average v wind (m/s) height 10m above ground         '
!..........................................................
!hchuang confirmed by Helin, correct bug, zorl unit in cm not mm
! BUG      glolal=gzorl*1.0E-3*rtime  !! convert from mm to m
      glolal  = flx_fld%gzorl*1.0E-2*rtime  !! convert from cm to m
      ngrid2d = ngrid2d+1
      CALL uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar,
     &     buff_mult_piecef(1,1,ngrid2d))
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '120)Average Surface roughness (m)
!
!..........................................................
!
!hchuang when callng sub  uninterprez, array glolal is assign to
!        buff_mult_piecea at the ngrid location, then ngrid advanced
!        by 1.  Before assign the modified value (buffo) to
!        buff_mult_piecea again dial ngrid back by 1 for the correct
!        ngrid index otherwise, you risk the chance that ngrid might
!        > ngrids_flx+1  which cause the address error or arry over-run
!
!hchuang code change [+2]  when callng sub  uninterprez, array glolal is assign to
        glolal  = flx_fld%goro*rtime
        ngrid2d = ngrid2d+1
        CALL uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar,
     &     buff_mult_piecef(1,1,ngrid2d))
!     if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '121)Average Land Sea surface (fraction)                  '
!
!..........................................................
      ENDIF


      if(me.eq.ioproc)
     &   PRINT *,'(wrtflx_a) GRIB FLUX FILE WRITTEN ',FHOUR,IDATE,noflx
!!
      RETURN
      END

!!*********************************************************************
!! This routine is added to output 2d aerosol diag fields (Sarah Lu)

      SUBROUTINE wrtaer(IOPROC,noaer,ZHOUR,FHOUR,IDATE,
     &             sfc_fld, g2d_fld,global_lats_r, lonsperlar)
!!
      use resol_def,               ONLY: lonr, latr, ngrids_aer
      use mod_state,               ONLY: buff_mult_pieceg
      use layout1,                 ONLY: me, lats_node_r,lats_node_r_max
      use gfs_physics_sfc_flx_mod, ONLY: Sfc_Var_Data
      use gfs_physics_g2d_mod,     ONLY: G2D_Var_Data
      USE machine,                 ONLY: kind_io8, kind_io4
      implicit none
!!
      TYPE(Sfc_Var_Data)        :: sfc_fld
      TYPE(G2D_Var_Data)        :: g2d_fld
      INTEGER                   GLOBAL_LATS_R(LATR)
      INTEGER                   lonsperlar(LATR)
      integer                   IOPROC
!!
      integer                   i,j,k,l,noaer,ngrid2d,ierr
      real (kind=kind_io8)      rtime
      real (kind=kind_io8)      zhour,fhour

!     real(kind=kind_io8) slmskful(lonr,lats_node_r)
!     real(kind=kind_io8) slmskloc(LONR,LATS_NODE_R)
!
      INTEGER     IDATE(4), IDS(255),IENS(5)
!
      real (kind=kind_io8)   glolal(lonr,LATS_NODE_R_max)
      real (kind=kind_io8)   buffo(lonr,LATS_NODE_R_max)
      integer kmsk  (lonr,lats_node_r_max),kmsk0(lonr,lats_node_r_max)
!
      kmsk=nint(sfc_fld%slmsk)
      kmsk0=0
!
!     ngrid2d=1
!
      IF(FHOUR.GT.ZHOUR) THEN
        RTIME=1./(3600.*(FHOUR-ZHOUR))
      ELSE
        RTIME=0.
      ENDIF
!
!..........................................................
!
      ngrid2d = 0
      if ( g2d_fld%du%nfld > 0 ) then
        do  k = 1, g2d_fld%du%nfld
          glolal = RTIME*1.e6*g2d_fld%du%diag(k)%flds
          ngrid2d=ngrid2d+1
          CALL uninterprez(2,kmsk0,buffo,glolal,global_lats_r,
     &                   lonsperlar,buff_mult_pieceg(1,1,ngrid2d))
        enddo
      endif
!
!..........................................................
!
      if ( g2d_fld%su%nfld > 0 ) then
        do  k = 1, g2d_fld%su%nfld
          glolal = RTIME*1.e6*g2d_fld%su%diag(k)%flds
          ngrid2d=ngrid2d+1
          CALL uninterprez(2,kmsk0,buffo,glolal,global_lats_r,
     &                   lonsperlar,buff_mult_pieceg(1,1,ngrid2d))
        enddo
      endif
!
!..........................................................
!
      if ( g2d_fld%ss%nfld > 0 ) then
        do  k = 1, g2d_fld%ss%nfld
          glolal = RTIME*1.e6*g2d_fld%ss%diag(k)%flds
          ngrid2d=ngrid2d+1
          CALL uninterprez(2,kmsk0,buffo,glolal,global_lats_r,
     &                   lonsperlar,buff_mult_pieceg(1,1,ngrid2d))
        enddo
      endif
!
!..........................................................
!
      if ( g2d_fld%oc%nfld > 0 ) then
        do  k = 1, g2d_fld%oc%nfld
          glolal=RTIME*1.e6*g2d_fld%oc%diag(k)%flds
          ngrid2d=ngrid2d+1
          CALL uninterprez(2,kmsk0,buffo,glolal,global_lats_r,
     &                   lonsperlar,buff_mult_pieceg(1,1,ngrid2d))
        enddo
      endif
!
!..........................................................
!
      if ( g2d_fld%bc%nfld > 0 ) then
        do  k = 1, g2d_fld%bc%nfld
          glolal = RTIME*1.e6*g2d_fld%bc%diag(k)%flds
          ngrid2d=ngrid2d+1
          CALL uninterprez(2,kmsk0,buffo,glolal,global_lats_r,
     &                   lonsperlar,buff_mult_pieceg(1,1,ngrid2d))
        enddo
      endif
!
!..........................................................
! 2d met fields (k=01-10) are time-avg;
! 3d met fields (k=11-24) are instant
! this change makes comparison easier (flx for 2d, sig for 3d)
!
      if ( g2d_fld%met%nfld > 0 ) then
        do  k = 1, g2d_fld%met%nfld
          if (k .le. 10 ) then                      ! time-avg
             glolal=RTIME*g2d_fld%met%diag(k)%flds
          else                                      ! instant
             glolal=g2d_fld%met%diag(k)%flds
          endif
          ngrid2d=ngrid2d+1
          CALL uninterprez(2,kmsk0,buffo,glolal,global_lats_r,
     &                   lonsperlar,buff_mult_pieceg(1,1,ngrid2d))
        enddo
      endif

!!

      if(me.eq.ioproc)
     &   PRINT *,'(wrtaer) GRIB AER FILE WRITTEN ',FHOUR,IDATE,noaer
!!
      RETURN
      END
!!****

       subroutine flx_only_move(ioproc)
!
!***********************************************************************
!
      use resol_def, ONLY: ngrids_flx, ngrids_sfcc, lonr,latr
      use mod_state, ONLY: buff_mult_pieces, buff_mult_piecef,
     &                     ivar_global_a, ivar_global
      use layout1,   ONLY: me, nodes, ipt_lats_node_r, lats_node_r,
     &                     lats_node_r_max, nodes_comp
      use mpi_def,   ONLY: mpi_r_io, stat, mpi_comm_null, info, 
     &                     mc_comp, mpi_integer, mpi_comm_all, liope
      implicit none
!
      integer ipt_lats_node_rl,nodesr
      integer lats_nodes_rl
!      integer lats_nodes_r(nodes),ipt,maxfld,ioproc,nproct
      integer ioproc
      integer proc,j,lat,msgtag,nproc,i,msgtag1,buff,startlat,ierr
      integer illen,ubound,nd1
      integer icount
      data icount/0/
      integer maxlats_comp
      save maxlats_comp,icount
      integer kllen
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      if(icount .eq. 0) then
        if(.not.allocated(ivar_global)) allocate(ivar_global(10))
        if(.not.allocated(ivar_global_a)) 
     &      allocate(ivar_global_a(10,nodes))
        ivar_global(1)=ipt_lats_node_r
        ivar_global(2)= lats_node_r
        ivar_global(3)=lats_node_r_max
        call mpi_gather(ivar_global,10,MPI_INTEGER,
     1    ivar_global_a,10,MPI_INTEGER,ioproc,MPI_COMM_ALL,ierr)
        icount=icount+1
      endif
!!
      if(allocated(buff_mult_pieces)) then
        deallocate(buff_mult_pieces)
      else
        maxlats_comp=lats_node_r_max
        if(me .eq. ioproc) then
          maxlats_comp=ivar_global_a(3,1)
        endif
      endif
      if(me .eq. ioproc) then
!gwv watch this!!
          allocate
     1  (buff_mult_pieces(lonr*latr*ngrids_flx))
         buff_mult_pieces=0.
       endif
!
!  big send
       IF (me.ne.ioproc) THEN
!
!         Sending the data
         msgtag=me
         illen=lats_node_r
         kllen=illen*lonr*ngrids_flx
! send the local grid domain
         CALL mpi_send
     &     (buff_mult_piecef(1:lonr,1:lats_node_r,1:ngrids_flx),
     &      kllen,MPI_R_IO,ioproc,msgtag,mc_comp,info)
      ELSE
        if( MC_COMP .ne. MPI_COMM_NULL) then
! iotask is also a compute task.  send is replaced with direct
!  array copy
 
        if(nodes_comp==1) then
          buff_mult_pieces(1:lonr*lats_node_r*ngrids_flx)=
     1   reshape(buff_mult_piecef(1:lonr,1:lats_node_r,1:ngrids_flx),
     1     (/lonr*lats_node_r*ngrids_flx/) )
        else

!  END COMPUTE TASKS PORTION OF LOGIC
!  receiving part of I/O task
 
!!
!!     for pes ioproc
        nd1=0
        DO proc=1,nodes_comp
         illen=ivar_global_a(2,proc)
         if (proc.ne.ioproc+1) then
           msgtag=proc-1
           kllen=illen*lonr*ngrids_flx
           CALL mpi_recv
     1       (buff_mult_pieces(nd1+1),kllen,MPI_R_IO,proc-1,
     &                msgtag,mc_comp,stat,info)
!     &                msgtag,MPI_COMM_ALL,stat,info)
         else
           buff_mult_pieces(nd1+1:nd1+lonr*illen*ngrids_flx)=
     1      reshape(buff_mult_piecef(1:lonr,1:illen,1:ngrids_flx),
     1       (/lonr*illen*ngrids_flx/) )
         endif
         nd1=nd1+illen*lonr*ngrids_flx
        enddo
       endif

      endif
!end ioproc
      ENDIF
!
      return
      end
!------------------------------------------------------------------------ 
      SUBROUTINE flx_wrt_nemsio(IOPROC,cfile,ZHOUR,FHOUR,idate
     &,                  global_lats_r,lonsperlar)
!!
      use nemsio_module, only: nemsio_open,nemsio_writerec,nemsio_close
     &  ,nemsio_gfile, nemsio_init,nemsio_finalize
      use resol_def,    ONLY: lonr, latr, levs,ngrids_flx,
     & ncld,ntrac,ntcw,ntoz,lsoil, ivssfc,thermodyn_id,sfcpress_id
      use layout1,      ONLY: me,idrt
      USE machine,      ONLY: kind_io8, kind_io4
!
      use gfs_physics_output, only : PHY_INT_STATE_ISCALAR,
     &    PHY_INT_STATE_RSCALAR,
     &    PHY_INT_STATE_1D_I,PHY_INT_STATE_1D_R,
     &    PHY_INT_STATE_2D_R_FLX
      implicit none
!!
      integer nw,IOPROC
      character*16 cfile,NAME2D
      real(kind=kind_io8) zhour,fhour
      integer idate(4),k,il, ngridss
!
      integer i,j,ndim3,N2DR,INDX,idate7(7),kount,nrec
      integer nfhour,nfminute,nfsecondn,nfsecondd
      logical  :: outtest
      integer ::nmetavari,nmetavarr,nmetavarl,nmetaaryi,nmetaaryr
      character(16),allocatable :: recname(:),reclevtyp(:)
      integer,allocatable :: reclev(:),itr(:)
      character(16),allocatable :: variname(:),varrname(:),
     &    aryiname(:),aryrname(:)
      integer,allocatable :: varival(:),aryilen(:),
     &    aryival(:,:)
      real(kind=kind_io4),allocatable    :: varrval(:)
      real(kind=kind_io4),allocatable    :: buff_mult(:,:,:),tmp(:)
      type(nemsio_gfile) gfileout
!

!!
      CHARACTER*8 labfix(4)
      real(kind=kind_io4) yhour
      integer,save:: version
      data version/200501/
      INTEGER              GLOBAL_LATS_R(latr), lonsperlar(latr)
!
      integer iret
      logical first
      save first
      save  recname, reclevtyp, reclev
      save nrec,nmetavari,nmetavarr,nmetaaryi,nmetaaryr,
     &     variname,varrname,aryiname,
     &     varival,varrval,aryilen,aryival
!jw     &     variname,varrname,aryiname,aryrname,
!jw     &     varival,aryilen,aryrlen,aryival,aryrval,varrval
      data first /.true./
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
!    Build surface fields in to buff_mult
!
      if (me.eq.ioproc) then
!
      print *,' begin of flx_wrt '

        allocate(buff_mult(lonr,latr,ngrids_flx))
        buff_mult=0.
        do ngridss=1,ngrids_flx
          print *,' inside flx_wrt calling unsp ngridss=',ngridss
          call unsplit2z(ioproc,ngridss,ngrids_flx,
     &      buff_mult(1,1,ngridss),global_lats_r)
        enddo
!    Building surface field is done
!
        if (first) then
!write out nemsio sfc file:
          nrec=ngrids_flx
          kount=size(PHY_INT_STATE_ISCALAR,2)
          do i=1,kount
           if(trim(PHY_INT_STATE_ISCALAR(2,i)).eq.'OGFS_PHY'
     &     .or.trim(PHY_INT_STATE_ISCALAR(2,i)).eq.'OGFS_FLX')
     &        nmetavari=nmetavari+1
          enddo
          allocate(variname(nmetavari),varival(nmetavari))
          j=0
          do i=1,kount
           if(trim(PHY_INT_STATE_ISCALAR(2,i)).eq.'OGFS_PHY' .or.
     &      trim(PHY_INT_STATE_ISCALAR(2,i)).eq.'OGFS_FLX' )then
            j=j+1
            variname(j)=trim(PHY_INT_STATE_ISCALAR(1,i))
            if(trim(variname(j))=='latr') varival(j)=latr
            if(trim(variname(j))=='lonr') varival(j)=lonr
            if(trim(variname(j))=='levs') varival(j)=levs
            if(trim(variname(j))=='ntoz') varival(j)=ntoz
            if(trim(variname(j))=='ntcw') varival(j)=ntcw
            if(trim(variname(j))=='ncld') varival(j)=ncld
            if(trim(variname(j))=='ntrac') varival(j)=ntrac
            if(trim(variname(j))=='thermodyn_id')varival(j)=thermodyn_id
            if(trim(variname(j))=='sfcpress_id') varival(j)=sfcpress_id
            if(trim(variname(j))=='lsoil') varival(j)=lsoil
            if(trim(variname(j))=='idrt') varival(j)=idrt
           endif
          enddo
!!for real var::
          nmetavarr=0
          do i=1,kount
           if(trim(PHY_INT_STATE_RSCALAR(2,i)).eq.'OGFS_PHY'
     &     .or.trim(PHY_INT_STATE_RSCALAR(2,i)).eq.'OGFS_FLX')
     &     nmetavarr=nmetavarr+1
          enddo
          if(nmetavarr>0) then
            allocate(varrname(nmetavarr),varrval(nmetavarr))
            j=0
            do i=1,kount
             if(trim(PHY_INT_STATE_RSCALAR(2,i)).eq.'OGFS_PHY'
     &       .or.trim(PHY_INT_STATE_RSCALAR(2,i)).eq.'OGFS_FLX')then
               j=j+1
               varrname(j)=trim(PHY_INT_STATE_RSCALAR(1,i))
               if(trim(varrname(j))=='fhour') varrval(j)=fhour
               if(trim(varrname(j))=='zhour') varrval(j)=zhour
             endif
            enddo
          endif
!!for 1D ary::
          nmetaaryi=0
          do i=1,kount
           if(trim(PHY_INT_STATE_1D_I(2,i)).eq.'OGFS_PHY'
     &     .or.trim(PHY_INT_STATE_1D_I(2,i)).eq.'OGFS_FLX')
     &     nmetaaryi=nmetaaryi+1
          enddo
          allocate(aryiname(nmetaaryi),aryilen(nmetaaryi))
          j=0
          do i=1,kount
           if(trim(PHY_INT_STATE_1D_I(2,i)).eq.'OGFS_PHY'
     &     .or.trim(PHY_INT_STATE_1D_I(2,i)).eq.'OGFS_FLX')then
             j=j+1
             aryiname(j)=trim(PHY_INT_STATE_1D_I(1,i))
             if(trim(aryiname(j))=='IDATE') aryilen(j)=size(idate)
           endif
          enddo
          allocate(aryival(maxval(aryilen),nmetaaryi) )
          aryival(1:aryilen(1),1)=idate(:)
!!!for 1D real ary::
!          nmetaaryr=0
!          do i=1,kount
!           if(trim(PHY_INT_STATE_1D_R(2,i)).eq.'OGFS_PHY'
!     &     .or.trim(PHY_INT_STATE_1D_R(2,i)).eq.'OGFS_FLX')
!     &     nmetaaryr=nmetaaryr+1
!          enddo
!          allocate(aryrname(nmetaaryr),aryrlen(nmetaaryr))
!          do i=1,kount
!           if(trim(PHY_INT_STATE_1D_R(2,i)).eq.'OGFS_PHY')
!     &     .or.trim(PHY_INT_STATE_1D_R(2,i)).eq.'OGFS_FLX')then
!             aryrname(i)=trim(PHY_INT_STATE_1D_R(1,i))
!             if(i==1) aryrlen(i)=size(ak5)
!             if(i==2) aryrlen(i)=size(bk5)
!             if(i==3) aryrlen(i)=size(ck5)
!           endif
!          enddo
!          allocate(aryrval(maxval(aryrlen),nmetaaryr)
!          aryrval(1:aryrlen(1),1)=ak5(:)
!          aryrval(1:aryrlen(2),2)=bk5(:)
!          aryrval(1:aryrlen(3),2)=ck5(:)
!
!!for record name, levtyp and lev
          allocate (recname(nrec),reclevtyp(nrec),reclev(nrec))
          allocate (itr(nrec))
          N2DR=0
          itr=-99
          do i=1,kount
           if(trim(PHY_INT_STATE_2D_R_FLX(2,i)).eq.'OGFS_FLX')then
            N2DR=N2DR+1
            NAME2D=trim(PHY_INT_STATE_2D_R_FLX(1,i))
            INDX=INDEX(NAME2D,"_")
            if(indx>0) then
              recname(N2DR)=NAME2D(1:INDX-1)
            else
              recname(N2DR)=NAME2D
            endif
!
            reclevtyp(N2DR)=trim(trim(PHY_INT_STATE_2D_R_FLX(3,i)))
            reclev(N2DR)=1
!
!check time average
           if(INDEX(NAME2D,"_ave") >0) then
               itr(N2DR)=3
            elseif(INDEX(NAME2D,"_acc") >0) then
               itr(N2DR)=4
            elseif(INDEX(NAME2D,"_win") >0) then
               itr(N2DR)=2
            endif

           endif
          enddo
!
!end first
          first=.false.
         endif
!
        idate7=0
        idate7(1)=idate(4)
        idate7(2)=idate(2)
        idate7(3)=idate(3)
        idate7(4)=idate(1)
        idate7(7)=100           !denominator for second
!
        nfhour=int(fhour)     
        nfminute=int((fhour-nfhour)*60)
        nfsecondn=int(((fhour-nfhour)*3600-nfminute*60)*100)
        nfsecondd=100
!
        call nemsio_init()
!
        call nemsio_open(gfileout,trim(cfile),'write',
     &    iret = iret,
     &    modelname='GFS',gdatatype='grib',
     &    idate=idate7,nrec=nrec,
     &    dimx=lonr,dimy=latr,dimz=levs,ncldt=ncld,nmeta=5,
     &    nfhour=nfhour,nfminute=nfminute,nfsecondn=nfsecondn,
     &    nfsecondd=nfsecondd,
     &    extrameta=.true.,nmetavari=nmetavari,
     &    nmetavarr=nmetavarr,
     &    nmetaaryi=nmetaaryi,
     &    variname=variname,varival=varival,varrname=varrname,
     &    varrval=varrval,
     &    aryiname=aryiname,aryilen=aryilen,aryival=aryival,
     &    ntrac=ntrac,nsoil=lsoil,idrt=idrt,
     &    recname=recname,reclevtyp=reclevtyp,reclev=reclev)
!
        allocate(tmp(lonr*latr))
        yhour=zhour
        do i=1,nrec
          tmp(:)=reshape(buff_mult(:,:,i),(/lonr*latr/) )
          if(itr(i)==-99) then
            call nemsio_writerec(gfileout,i,tmp,iret=iret)
          else
            call nemsio_writerec(gfileout,i,tmp,iret=iret,itr=itr(i),
     &        zhour=yhour)
          endif
        enddo
        deallocate(tmp)
        deallocate(buff_mult)
!
        call nemsio_close(gfileout)
!end write pe
        call nemsio_finalize()
      endif
!
      print *,' end of flx_wrt '
      return
      end subroutine flx_wrt_nemsio
!-------------------------------------------------------------------------
      SUBROUTINE wrtflx_w(IOPROC,noflx,ZHOUR,FHOUR,IDATE,colat1,SECSWR,
     &                  SECLWR,slmsk, global_lats_r,lonsperlar)
!
      use resol_def
      use mod_state
      use layout1
!      use sig_io
      use namelist_physics_def
      USE machine, ONLY: kind_evod, kind_io4,kind_io8
      implicit none
!!
      INTEGER              GLOBAL_LATS_R(LATR)
      INTEGER              lonsperlar(LATR)
      integer   IOPROC
      REAL (KIND=KIND_IO8) slmsk (LONR,LATS_NODE_R)
!!
      integer   IPRS,ITEMP,IZNLW,IMERW,ISPHUM,IPWAT,
     $          IPCPR,ISNOWD,ICLDF,ICCLDF,
     $          ISLMSK,IZORL,IALBDO,ISOILM,ISNOHF,ISMCWLT,
     $          ISMCREF,ICEMSK,
     $          ILHFLX,ISHFLX,IZWS,IMWS,IGHFLX,
     $          IUSWFC,IDSWFC,IULWFC,IDLWFC,
     $          INSWFC,INLWFC,
     $          IDSWVB,IDSWVD,IDSWNB,IDSWND,
     $          ITMX,ITMN,IRNOF,IEP,
!jwang add iqmx, iqmn
     $          IQMX,IQMN,
     &          ICLDWK,IZGW,IMGW,IHPBL,
     $          IDSWF,IDLWF,IUSWF,IULWF,ICPCPR,
     $          ISFC,ITOA,IELEV,
     $          ISGLEV,IDBLS,I2DBLS,ICOLMN,
     $          IBLBL,IBLTL,IBLLYR,
     $          ILCBL,ILCTL,ILCLYR,
     $          IMCBL,IMCTL,IMCLYR,
     $          IHCBL,IHCTL,IHCLYR,
     $          ICVBL,ICVTL,ICVLYR,
     $          INST,IWIN,IAVG,IACC,
     $          IFHOUR,IFDAY,
!    $          LEN,NFLD,
     $          NFLD,
     $          IUVBF,IUVBFC,
!yth add ISUNTM for sunshine time sep/08
     &          ISUNTM,
     $   j,i,k,itop,ibot,k4,l,noflx
     &,  isik, islc, isnod, icnp
     &,  iveg, ivtp, istp, islo,iust,ihgt,irst,ichh
     &,  icmm,isrf,ievbs,ievcw,itran,isbs,isnc,istc
!*RADFLX*
     +,  ICSUSW,ICSDSW,ICSULW,ICSDLW

!*RADFLX*
!     PARAMETER(NFLD=16)
!*    PARAMETER(NFLD=18)
      PARAMETER(NFLD=25)
       integer ilpds,iyr,imo,ida,ihr,ifhr,ithr,lg,ierr
       real (kind=kind_io8) RTIMER(NFLD),rtime,rtimsw,rtimlw
       real (kind=kind_io8) colat1
       real (kind=kind_io8) cl1,secswr,zhour,fhour,seclwr
!
      PARAMETER(IPRS=1,ITEMP=11,IZNLW=33,IMERW=34,ISPHUM=51,IPWAT=54,
     $          IPCPR=59,ISNOWD=65,ICLDF=71,ICCLDF=72,
     $          ISLMSK=81,IZORL=83,IALBDO=84,ISOILM=144,ICEMSK=91,
     $          ISIK=92,                                ! FOR SEA-ICE - XW Nov04
     $          ILHFLX=121,ISHFLX=122,IZWS=124,IMWS=125,IGHFLX=155,
     $          IUSWFC=160,IDSWFC=161,IULWFC=162,IDLWFC=163,
     $          INSWFC=164,INLWFC=165,
     $          IDSWVB=166,IDSWVD=167,IDSWNB=168,IDSWND=169,
     $          ITMX=15,ITMN=16,IRNOF=90,IEP=145,
!jwang add iqmx, iqmn
     $          IQMX=204,IQMN=205,
     &          ICLDWK=146,IZGW=147,IMGW=148,IHPBL=221,
     $          IDSWF=204,IDLWF=205,IUSWF=211,IULWF=212,ICPCPR=214,
!*RADFLX
!*   &          IUVBF=200,IUVBFC=201)
!    +          IUVBF=200,IUVBFC=201,
!yth  add ISUNTM for sunshine time  sep/08
     &          IUVBF=200,IUVBFC=201,ISUNTM=191,
     +          ICSUSW=160,ICSDSW=161,ICSULW=162,ICSDLW=163)
      PARAMETER(ISFC=1,ITOA=8,IELEV=105,
     $          ISGLEV=109,IDBLS=111,I2DBLS=112,ICOLMN=200,
!Cwei    $          ISGLEV=107,IDBLS=111,I2DBLS=112,ICOLMN=200,
     $          IBLBL=209,IBLTL=210,IBLLYR=211,
     $          ILCBL=212,ILCTL=213,ILCLYR=214,
     $          IMCBL=222,IMCTL=223,IMCLYR=224,
     $          IHCBL=232,IHCTL=233,IHCLYR=234,
     $          ICVBL=242,ICVTL=243,ICVLYR=244)

!Clu [+1L]: define parameter index, using Table 130
      PARAMETER(ISLC=160,ISNOD=66)
!Cwei
      PARAMETER(ISLO=222,ISBS=198,ISNC=238,ICMM=179)
      PARAMETER(ISNOHF=229,ISMCWLT=219,ISMCREF=220)
!Clu [+1L]: define parameter index, using Table 2
      PARAMETER(ICNP=223)
!Cwei
      PARAMETER(IVEG=87,IVTP=225,ISTP=224,IUST=253,IHGT=7,
     $          IRST=140,ICHH=208,ISRF=235,IEVBS=199,
     $          IEVCW=200,ITRAN=210,ISTC=86)

      PARAMETER(INST=10,IWIN=2,IAVG=3,IACC=4)
      PARAMETER(IFHOUR=1,IFDAY=2)
!     PARAMETER(LEN=lonr*latr)
      real(kind=kind_io4) wrkga(lonr*latr),wrkgb(lonr*latr)
      real(kind=kind_io8) slmskful(lonr*latr)
!jw      real(kind=kind_io8) slmskloc(LONR,LATS_NODE_R)
c
      LOGICAL(1) LBM(lonr*latr)
      CHARACTER G(200+lonr*latr*(16+1)/8)
      INTEGER   IPUR(NFLD),ITLR(NFLD)
      DATA      IPUR/IULWF , IUSWF , IUSWF , IDSWF ,  ICLDF,   IPRS,
     $                 IPRS, ITEMP ,  ICLDF,   IPRS,   IPRS, ITEMP ,
!*RADFLX*
!*   $                ICLDF,   IPRS,   IPRS, ITEMP ,  IUVBF, IUVBFC /
     +                ICLDF,   IPRS,   IPRS, ITEMP ,  IUVBF, IUVBFC,
     +                IDSWF, ICSULW, ICSUSW, ICSDLW, ICSUSW, ICSDSW,
     +                ICSULW/
!    $                ICLDF,   IPRS,   IPRS, ITEMP /
      DATA      ITLR/ITOA  , ITOA  , ISFC  , ISFC  , IHCLYR, IHCTL ,
     $               IHCBL , IHCTL , IMCLYR, IMCTL , IMCBL , IMCTL ,
!*RADFLX*
!*   $               ILCLYR, ILCTL , ILCBL , ILCTL , ISFC  , ISFC /
     +               ILCLYR, ILCTL , ILCBL , ILCTL , ISFC  , ISFC ,
     +               ITOA  ,  ITOA ,  ITOA ,  ISFC , ISFC  , ISFC,
     +               ISFC /
!    $               ILCLYR, ILCTL , ILCBL , ILCTL /
      INTEGER     IDATE(4), IDS(255)
!     INTEGER     IDATE(4), IDS(255),IENS(5)
      real (kind=kind_io8) SI(LEVP1)
C
csela..................................................................
!     real (kind=kind_io8)   rflux(lonr,LATS_NODE_R,27)
!     real (kind=kind_io8)   glolal(lonr,LATS_NODE_R)
!     real (kind=kind_io8)   buffo(lonr,LATS_NODE_R)
!     real (kind=kind_io4)   buff1(lonr,latr)
      real (kind=kind_io4)   buff1l(lonr*latr)
csela..................................................................
!     real (kind=kind_io8)  FLUXR(nfxr,LONR,LATS_NODE_R)
csela..................................................................
      integer kmsk(lonr,lats_node_r),kmsk0(lonr,lats_node_r)
      integer kmskcv(lonr,LATS_NODE_R),il
      integer iens(5)
      integer icen,icen2,ienst,iensi
      integer ngridss
!!
!
      print *,'in wrtflx_w'

      icen=7;icen2=0;igen=82;ienst=0;iensi=0

!       ngrid=0
!       ngrid=0+ngrids_sfcc+1
        ngrid=0+ngrids_sfcc+1+ngrids_nst
cjfe
      IDS=0
      G=' '
cjfe
!!
!jw      kmsk=nint(slmsk)
!jw      kmsk0=0
!jw      call unsplit2z(ioproc,buff1l,global_lats_r)
      call unsplit2d_phys_r(ioproc,slmskful,slmsk,global_lats_r)
!jw      slmskful=buff1l
!
!     do k=1,nfxr
!      do j=1,LATS_NODE_R
!       do i=1,lonr
      CALL IDSDEF(1,IDS)
!jwang add spfhmax/spfhmin
      ids(IQMX)   = 5
      ids(IQMN)   = 5
! UV-B scaling factor, if set up already, comment the next 2 lines out
      ids(IUVBF)  = 2
      ids(IUVBFC) = 2
! Ice conentration and thickness scaling factor
      ids(icemsk) = 3      ! ICE CONCENTRATION ()
      ids(isik)   = 2      ! ICE THICKNESS (M)
!
!wei added 10/24/2006
      ids(IZORL)  = 4
      ids(IHGT)   = 3
      ids(IVEG)   = 2
      ids(IUST)   = 3
      ids(ICHH)   = 4
      ids(ICMM)   = 4
      ids(ISRF)   = 5
      ids(ITEMP)  = 3
      ids(ISPHUM) = 6
      ids(IZNLW)  = 2
      ids(IMERW)  = 2
      ids(ISNC)   = 3
      ids(ISTC)   = 4
      ids(ISOILM) = 4
      ids(ISNOD)  = 6
      ids(ISNOWD) = 5
      ids(ICNP)   = 5
      ids(IPCPR)  = 6
      ids(ICPCPR) = 6
      ids(IRNOF)  = 5
      ids(ISMCWLT)  = 4
      ids(ISMCREF)  = 4
!
      ILPDS = 28
      IF(ICEN2.EQ.2) ILPDS = 45
      IENS(1) = 1
      IENS(2) = IENST
      IENS(3) = IENSI
      IENS(4) = 1
      IENS(5) = 255
      IYR     = IDATE(4)
      IMO     = IDATE(2)
      IDA     = IDATE(3)
      IHR     = IDATE(1)
      IFHR    = NINT(ZHOUR)
      ITHR    = NINT(FHOUR)
      IF(FHOUR.GT.ZHOUR) THEN
        RTIME = 1./(3600.*(FHOUR-ZHOUR))
      ELSE
        RTIME = 0.
      ENDIF
      IF(SECSWR.GT.0.) THEN
        RTIMSW = 1./SECSWR
      ELSE
        RTIMSW=1.
      ENDIF
      IF(SECLWR.GT.0.) THEN
        RTIMLW=1./SECLWR
      ELSE
        RTIMLW=1.
      ENDIF
      RTIMER=RTIMSW
      RTIMER(1)=RTIMLW
!*RADFLX*
      RTIMER(20)=RTIMLW       ! CSULF_TOA
      RTIMER(22)=RTIMLW       ! CSDLF_SFC
      RTIMER(25)=RTIMLW       ! CSULF_SFC
!*RADFLX*
      CL1=colat1
!!
!..........................................................
!
      print *,' begin of flx_wrt '
      ierr = 0

      ngridss=1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
!
      if(me.eq.ioproc) then
        call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,IZWS,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(IZWS),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
!         print *, ' called wryte unit noflx' ,noflx,ngrid
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '01)Zonal compt of momentum flux (N/m**2) land and sea surface '
        endif
      endif

!..........................................................
!     glolal=DVSFC*RTIME
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
        call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,IMWS,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(IMWS),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '02)Merid compt of momentum flux (N/m**2) land and sea surface '
        endif
      endif
!..........................................................
!     glolal=DTSFC*RTIME
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
        call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,ISHFLX,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(ISHFLX),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '03)Sensible heat flux (W/m**2) land and sea surface           '
        endif
      endif
!..........................................................
!     glolal=DQSFC*RTIME
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
        print *,' ngrid for ltflx=',maxval(wrkga),minval(wrkga),
     &   minloc(wrkga)
        call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,ILHFLX,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(ILHFLX),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '04)Latent heat flux (W/m**2) land and sea surface             '
        endif
      endif
!..........................................................
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
        print *,' ngrid for tmpsfc=',maxval(wrkga),minval(wrkga),
     &   minloc(wrkga),'ngridss=',ngridss,'ngrids_flx=',ngrids_flx
        call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,ITEMP,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ITEMP),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '05)Temperature (K) land and sea surface                       '
          stop
        endif
      endif
!..........................................................
!     glolal(:,:)=SMC(1,:,:)
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)

      if(me.eq.ioproc) then
        LBM=slmskful.EQ.1._kind_io8
        call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,ISOILM,I2DBLS,0,10,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ISOILM),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '06)Volumetric soil moist content (frac) layer 10cm and 0cm    '
        endif
      endif
!..........................................................
!     glolal(:,:)=SMC(2,:,:)
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
        LBM=slmskful.EQ.1._kind_io8
        call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
!lu  &            1,ISOILM,I2DBLS,10,200,IYR,IMO,IDA,IHR,
     +            1,ISOILM,I2DBLS,10,40,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ISOILM),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!lu  x '07)Volumetric soil moist content (frac) layer 200cm and 10cm  '
     + '07)Volumetric soil moist content (frac) layer 40cm and 10cm  '
        endif
      endif
!..........................................................
!     glolal(:,:)=STC(1,:,:)
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
        LBM=slmskful.EQ.1._kind_io8
        call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,ITEMP,I2DBLS,0,10,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ITEMP),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '08)Temp (K) layer betw two depth below land sfc 10cm and 0cm  '
        endif
      endif
!..........................................................
!     glolal(:,:)=STC(2,:,:)
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
        LBM=slmskful.EQ.1._kind_io8
        call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
!lu  &            1,ITEMP,I2DBLS,10,200,IYR,IMO,IDA,IHR,
     +            1,ITEMP,I2DBLS,10,40,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ITEMP),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!lu  x '09)Temp (K) layer betw two depth below land sfc 200cm and 10cm'
     + '09)Temp (K) layer betw two depth below land sfc 40cm and 10cm'
        endif
      endif
!..........................................................
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
        call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,ISNOWD,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ISNOWD),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '10)Water equiv of accum snow depth (kg/m**2) land sea surface '
        endif
      endif
!..........................................................
!     glolal=DLWSFC*RTIME
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
        call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,IDLWF,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(IDLWF),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '11)Downward long wave radiation flux (W/m**2) land sea surface'
        endif
      endif
!..........................................................
!     glolal=ULWSFC*RTIME
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
        call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,IULWF,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(IULWF),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '12)Upward long wave radiation flux (W/m**2) land sea surface  '
        endif
      endif
!..........................................................
!.......  FIX FLUXES FOR APPROX DIURNAL CYCLE
      DO K=1,4
!       do j=1,LATS_NODE_R
!         do i=1,lonr
!            glolal(i,j)=rflux(i,j,k)*RTIMER(k)
!         enddo
!       enddo
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
        call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &              0,IPUR(K),ITLR(K),0,0,IYR,IMO,IDA,IHR,
     &              IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(IPUR(K)),IENS,
     &              0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          if(k.eq.1)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '13)Upward long wave radiation flux (W/m**2) top of atmosphere '
          if(k.eq.2)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '14)Upward solar radiation flux (W/m**2) top of atmosphere     '
          if(k.eq.3)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '15)Upward solar radiation flux (W/m**2) land and sea surface  '
          if(k.eq.4)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '16)Downward solar radiation flux (W/m**2) land and sea surface'
        endif
      endif
      ENDDO
!..........................................................
!
!     For UV-B fluxes
!
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
       call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,129,ICEN,IGEN,
     &            0,IUVBF,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(IUVBF),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '17)UV-B Downward solar flux (W/m**2) land sea surface'
        endif
      endif

      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
       call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,129,ICEN,IGEN,
     &            0,IUVBFC,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(IUVBFC),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '18)clear sky UV-B Downward solar flux (W/m**2) land sea surface'
        endif
      endif
!
!     End UV-B fluxes
!
!..........................................................
!..........................................................
      DO K=5,7
!
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
!
        K4=4+(K-5)*4
        L=K4+1
        LBM=wrkga.Ge.0.5_kind_io8
        call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &              0,IPUR(L),ITLR(L),0,0,IYR,IMO,IDA,IHR,
     &              IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(IPUR(L)),IENS,
     &              0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          if(k.eq.5)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '19)Total cloud cover (percent) high cloud layer               '
          if(k.eq.6)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '23)Total cloud cover (percent) middle cloud layer             '
          if(k.eq.7)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '27)Total cloud cover (percent) low cloud layer                '
        endif
      endif
!
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
        L=K4+2
        call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &              1,IPUR(L),ITLR(L),0,0,IYR,IMO,IDA,IHR,
     &              IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(IPUR(L)),IENS,
     &              0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          if(k.eq.5)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '20)Pressure (Pa) high cloud top level                         '
          if(k.eq.6)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '24)Pressure (Pa) middle cloud top level                       '
          if(k.eq.7)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '28)Pressure (Pa) low cloud top level                          '
        endif
      endif
!
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
!
        L=K4+3
        call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &              1,IPUR(L),ITLR(L),0,0,IYR,IMO,IDA,IHR,
     &              IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(IPUR(L)),IENS,
     &              0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          if(k.eq.5)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '21)Pressure (Pa) high cloud bottom level                      '
          if(k.eq.6)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '25)Pressure (Pa) middle cloud bottom level                    '
          if(k.eq.7)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '29)Pressure (Pa) low cloud bottom level                       '
        endif
      endif
!
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
        L=K4+4
        call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &              1,IPUR(L),ITLR(L),0,0,IYR,IMO,IDA,IHR,
     &              IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(IPUR(L)),IENS,
     &              0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          if(k.eq.5)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '22)Temperature (K) high cloud top level                       '
          if(k.eq.6)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '26)Temperature (K) middle cloud top level                     '
          if(k.eq.7)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '30)Temperature (K) low cloud top level                        '
        endif
      endif
!
      ENDDO
!!
!...................................................................
!     glolal=GESHEM*1.E3*RTIME
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
        call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,IPCPR,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(IPCPR),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '31)Precipitation rate (kg/m**2/s) land and sea surface        '
        endif
      endif
!...................................................................
!     glolal=BENGSH*1.E3*RTIME
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
        call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,ICPCPR,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(ICPCPR),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '32)Convective precipitation rate (kg/m**2/s) land sea surface '
        endif
      endif
!...................................................................
!     glolal=GFLUX*RTIME
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
        LBM=slmskful.NE.0._kind_io8
        call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,IGHFLX,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(IGHFLX),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '33)Ground heat flux (W/m**2) land and sea surface             '
        endif
      endif
!...................................................................
!     buffo=MOD(slmskloc,2._kind_io8)
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
        call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,ISLMSK,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ISLMSK),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '34)Land-sea mask (1=land; 0=sea) (integer) land sea surface   '
        endif
      endif
!...................................................................
!c-- XW: FOR SEA-ICE Nov04
!     buffo=MAX(slmskloc-1._kind_io8,0._kind_io8)
!     call unsplit2z(ioproc,wrkga,global_lats_r)
!     if(me.eq.ioproc) then
!       call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
!    &            0,ICEMSK,ISFC,0,0,IYR,IMO,IDA,IHR,
!    &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ICEMSK),IENS,
!    &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
!       if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
!    x '35)Ice concentration (ice=1; no ice=0) (1/0) land sea surface '
!     endif
!     IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
        call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,ICEMSK,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ICEMSK),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '35)Ice concentration (ice>0; no ice=0) (1/0) land sea surface '
        endif
      endif

!...................................................................
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
        call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,IZNLW,IELEV,0,10,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(IZNLW),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '36)u wind (m/s) height above ground                           '
        endif
      endif
!...................................................................
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
        call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,IMERW,IELEV,0,10,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(IMERW),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '37)v wind (m/s) height above ground                           '
        endif
      endif
!...................................................................
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
        call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,ITEMP,IELEV,0,2,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ITEMP),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '38)Temperature (K) height above ground                        '
        endif
      endif
!...................................................................
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
        call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,ISPHUM,IELEV,0,2,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ISPHUM),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '39)Specific humidity (kg/kg) height above ground              '
        endif
      endif
!...................................................................
!     glolal=PSURF
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
        call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,IPRS,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(IPRS),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '40)Pressure (Pa) land and sea surface                         '
        endif
      endif
!...................................................................
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
        call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,ITMX,IELEV,0,2,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IWIN,0,0,ICEN2,IDS(ITMX),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '41)Maximum temperature (K) height above ground                '
        endif
      endif
!...................................................................
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
        call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,ITMN,IELEV,0,2,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IWIN,0,0,ICEN2,IDS(ITMN),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '42)Minimum temperature (K) height above ground                '
        endif
      endif
!...................................................................
!jwang add spfhmax,spfhmin
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
       call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,133,ICEN,IGEN,
     &            0,IQMX,IELEV,0,2,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IWIN,0,0,ICEN2,IDS(IQMX),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '41a)Maximum specific humidity (kg/kg) height above ground     '
        endif
      endif
!...................................................................
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
       call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,133,ICEN,IGEN,
     &            0,IQMN,IELEV,0,2,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IWIN,0,0,ICEN2,IDS(IQMN),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '42a)Minimum specific humidity (kg/kg) height above ground      '
        endif
      endif
!...................................................................
!  The output unit of runoff is kg/m2 (accumulative value)
!     glolal=RUNOFF * 1.E3
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
        LBM=slmskful.NE.0._kind_io8
        call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,IRNOF,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IACC,0,0,ICEN2,IDS(IRNOF),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '43)Runoff (kg/m**2) land and sea surface                      '
        endif
      endif
!...................................................................
!     glolal=EP * RTIME
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.NE.0._kind_io8
        call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,IEP,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(IEP),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '44)Potential evaporation rate (w/m**/) land and sea surface   '
        endif
      endif
!...................................................................
!     glolal=CLDWRK * RTIME
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
        call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,ICLDWK,ICOLMN,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(ICLDWK),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '45)Cloud work function (J/Kg) total atmospheric column        '
        endif
      endif
!...................................................................
!     glolal=DUGWD*RTIME
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
        call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,IZGW,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(IZGW),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '46)Zonal gravity wave stress (N/m**2) land and sea surface    '
        endif
      endif
!...................................................................
!     glolal=DVGWD*RTIME
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
        call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,IMGW,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(IMGW),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '47)Meridional gravity wave stress (N/m**2) land sea surface   '
        endif
      endif
!...................................................................
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
        call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,IHPBL,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(IHPBL),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '48)Boundary layer height '
        endif
      endif
!...................................................................
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
        call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,IPWAT,ICOLMN,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(IPWAT),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '49)Precipitable water (kg/m**2) total atmospheric column      '
        endif
      endif
!...................................................................
!
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
!
        call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,IALBDO,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(IALBDO),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '50)Albedo (percent) land and sea surface                      '
        endif
      endif
!
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
!
        call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &              0,ICLDF,ICOLMN,0,0,IYR,IMO,IDA,IHR,
     &              IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(ICLDF),IENS,
     &              0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '51)Total cloud cover (percent) total atmospheric column       '
        endif
      endif
!
! CONVECTIVE CLOUDS
! LABELED INSTANTANEOUS BUT ACTUALLY AVERAGED OVER FHSWR HOURS
!
!     glolal=CV*1.E2
!     where(glolal.ge.0.5)
!       kmskcv=1
!     elsewhere
!       kmskcv=0
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      LBM=wrkga.Ge.0.5_kind_io8
        call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,ICLDF,ICVLYR,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ICLDF),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '52)Total cloud cover (percent) convective cloud layer         '
        endif
      endif
!.................................................
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
        call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,IPRS,ICVTL,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(IPRS),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '53)Pressure (Pa) convective cloud top level                   '
        endif
      endif
!.................................................
!      do j=1,LATS_NODE_R
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
        call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,IPRS,ICVBL,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(IPRS),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '54)Pressure (Pa) convective cloud bottom level                '
        endif
      endif
!.................................................
!...   SAVE B.L. CLOUD AMOUNT
!
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
!
        call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &              0,ICLDF,IBLLYR,0,0,IYR,IMO,IDA,IHR,
     &              IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(ICLDF),IENS,
     &              0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
           print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '55)Total cloud cover (percent) boundary layer cloud layer     '
        endif
      endif
!
!c-- XW: FOR SEA-ICE Nov04
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.NE.1._kind_io8
!     LBM=slmskful.EQ.2._kind_io8
      call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,ISIK,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ISIK),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '56)Sea ice thickness (m) category 1'
        endif
      endif
!c-- XW: END SEA-ICE
!.................................................
!lu: add smc(3:4), stc(3:4), slc(1:4), snwdph, canopy
!lu: addition of 10 records starts here -------------------------------
      if(lsoil.gt.2)then

!     glolal(:,:)=SMC(3,:,:)
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.EQ.1._kind_io8
      call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,ISOILM,I2DBLS,40,100,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ISOILM),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '57)Volumetric soil moist content (frac) layer 100cm and 40cm '
        endif
      endif
!..........................................................
!     glolal(:,:)=SMC(4,:,:)
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.EQ.1._kind_io8
      call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,ISOILM,I2DBLS,100,200,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ISOILM),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '58)Volumetric soil moist content (frac) layer 200cm and 100cm '
        endif
      endif
!..........................................................
!     glolal(:,:)=STC(3,:,:)
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.EQ.1._kind_io8
      call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,ITEMP,I2DBLS,40,100,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ITEMP),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '59)Temp (K) layer betw two depth below land sfc 100cm and 40cm'
        endif
      endif
!..........................................................
!     glolal(:,:)=STC(4,:,:)
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.EQ.1._kind_io8
      call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,ITEMP,I2DBLS,100,200,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ITEMP),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '60)Temp (K) layer betw two depth below land sfc 200cm and 100cm'
        endif
      endif
!
      endif
!..........................................................
!     glolal(:,:)=SLC(1,:,:)
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.EQ.1._kind_io8
      call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,130,ICEN,IGEN,
     &            1,ISLC,I2DBLS,0,10,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ISOILM),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
         print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '61)Liquid soil moist content (frac) layer 10cm and 0cm  '
        endif
      endif
!..........................................................
!     glolal(:,:)=SLC(2,:,:)
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.EQ.1._kind_io8
      call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,130,ICEN,IGEN,
     &            1,ISLC,I2DBLS,10,40,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ISOILM),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '62)Liquid soil moist content (frac) layer 40cm and 10cm '
        endif
      endif
!..........................................................
      if(lsoil.gt.2)then
!     glolal(:,:)=SLC(3,:,:)
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.EQ.1._kind_io8
      call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,130,ICEN,IGEN,
     &            1,ISLC,I2DBLS,40,100,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ISOILM),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '63)Liquid soil moist content (frac) layer 100cm and 40cm'
        endif
      endif
!..........................................................
!     glolal(:,:)=SLC(4,:,:)
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.EQ.1._kind_io8
      call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,130,ICEN,IGEN,
     &            1,ISLC,I2DBLS,100,200,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ISOILM),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '64)Liquid soil moist content (frac) layer 200cm and 100cm'
        endif
      endif
      endif
!..........................................................
!     glolal=SNWDPH / 1.E3       !! convert from mm to m
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
!     LBM=slmskful.EQ.1._kind_io8
      LBM=slmskful.EQ.1._kind_io8 .or. slmskful.EQ.2._kind_io8
      call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,ISNOD,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ISNOD),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
         print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '65)Snow depth (m) land surface                  '
        endif
      endif
!..........................................................
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.EQ.1._kind_io8
      call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,ICNP,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ICNP),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '66)Canopy water content (kg/m^2) land surface      '
        endif
      endif
!lu: addition of 10 records ends here -------------------------------
!
!wei: addition of 30 records starts here -------------------------------
!..........................................................
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,IZORL,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(IZORL),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '67)Surface roughness (m)       '
        endif
      endif
!..........................................................
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.EQ.1._kind_io8
      call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,IVEG,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(IVEG),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '68)Vegetation fraction (fractional) land surface      '
        endif
      endif
!..........................................................
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.EQ.1._kind_io8
      call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,IVTP,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(IVTP),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '69)Vegetation type land surface      '
        endif
      endif
!..........................................................
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.EQ.1._kind_io8
      call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,ISTP,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ISTP),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '70)Soil type land surface      '
        endif
      endif
!..........................................................
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.EQ.1._kind_io8
      call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,130,ICEN,IGEN,
     &            1,ISLO,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ISLO),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '71)Slope type land surface      '
        endif
      endif
!..........................................................
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,IUST,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(IUST),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '72)Frictional velocity (m/s)      '
        endif
      endif
!..........................................................
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,IHGT,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(IHGT),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '73)Surface height (m)      '
        endif
      endif
!..........................................................
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.EQ.1._kind_io8
      call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,IRST,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(IRST),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '74)Freezing precip flag land surface      '
        endif
      endif
!..........................................................
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,ICHH,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ICHH),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '75)Exchange coefficient CH(m/s)      '
        endif
      endif
!..........................................................
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,130,ICEN,IGEN,
     &            0,ICMM,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ICMM),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '76)Exchange coefficient CM(m/s)      '
        endif
      endif
!..........................................................
!jw      if (.not. climate) then
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
        if(me.eq.ioproc) then
        LBM=slmskful.EQ.1._kind_io8
        call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &              1,IEP,ISFC,0,0,IYR,IMO,IDA,IHR,
     &              IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(IEP),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
          IF(IERR.EQ.0) then
            CALL WRYTE(noflx,LG,G)
          else
            print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x   '77)Potential evaporation rate (w/m**2) land and sea surface  '
          endif
        endif
!..........................................................
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
        if(me.eq.ioproc) then
        call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &              0,IDLWF,ISFC,0,0,IYR,IMO,IDA,IHR,
     &              IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(IDLWF),IENS,
     &              0.,0.,0.,0.,0.,0.,G,LG,IERR)
          IF(IERR.EQ.0) then
            CALL WRYTE(noflx,LG,G)
          else
            print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x   '78)Downward long wave radiation flux (W/m**2)'
          endif
        endif
!..........................................................
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
        if(me.eq.ioproc) then
        call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &              0,IULWF,ISFC,0,0,IYR,IMO,IDA,IHR,
     &              IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(IULWF),IENS,
     &              0.,0.,0.,0.,0.,0.,G,LG,IERR)
          IF(IERR.EQ.0) then
            CALL WRYTE(noflx,LG,G)
          else
            print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x   '79)Upward long wave radiation flux (W/m**2)  '
          endif
        endif
!..........................................................
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
        if(me.eq.ioproc) then
        call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &              0,IUSWF,ISFC,0,0,IYR,IMO,IDA,IHR,
     &              IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(IUSWF),IENS,
     &              0.,0.,0.,0.,0.,0.,G,LG,IERR)
          IF(IERR.EQ.0) then
            CALL WRYTE(noflx,LG,G)
          else
            print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x   '80)Upward short wave radiation flux (W/m**2)  '
          endif
        endif
!..........................................................
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
        if(me.eq.ioproc) then
        call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &              0,IDSWF,ISFC,0,0,IYR,IMO,IDA,IHR,
     &              IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(IDSWF),IENS,
     &              0.,0.,0.,0.,0.,0.,G,LG,IERR)
          IF(IERR.EQ.0) then
            CALL WRYTE(noflx,LG,G)
          else
            print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x   '81)Downward short wave radiation flux (W/m**2)   '
          endif
        endif
!..........................................................
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
        if(me.eq.ioproc) then
         call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &              0,ISHFLX,ISFC,0,0,IYR,IMO,IDA,IHR,
     &              IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ISHFLX),IENS,
     &              0.,0.,0.,0.,0.,0.,G,LG,IERR)
          IF(IERR.EQ.0) then
            CALL WRYTE(noflx,LG,G)
          else
            print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x   '82)Sensible heat flux (W/m**2) land and sea surface          '
          endif
        endif
!..........................................................
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
        if(me.eq.ioproc) then
         call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &              0,ILHFLX,ISFC,0,0,IYR,IMO,IDA,IHR,
     &              IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ILHFLX),IENS,
     &              0.,0.,0.,0.,0.,0.,G,LG,IERR)
          IF(IERR.EQ.0) then
            CALL WRYTE(noflx,LG,G)
          else
            print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x   '83)Latent heat flux (W/m**2) land and sea surface            '
          endif
        endif
!..........................................................
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
        if(me.eq.ioproc) then
         LBM=slmskful.NE.0._kind_io8
         call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &              1,IGHFLX,ISFC,0,0,IYR,IMO,IDA,IHR,
     &              IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(IGHFLX),IENS,
     &              0.,0.,0.,0.,0.,0.,G,LG,IERR)
          IF(IERR.EQ.0) then
            CALL WRYTE(noflx,LG,G)
          else
            print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x   '84)Ground heat flux (W/m**2) land and sea surface            '
          endif
        endif
!jw      endif             ! end of if(.not climate)
!..........................................................
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.EQ.1._kind_io8
      call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,ISRF,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IACC,0,0,ICEN2,IDS(ISRF),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '85)Surface runoff (kg/m^2) land surface      '
        endif
      endif
!..........................................................
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,ITEMP,isglev,1,1,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ITEMP),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '86)Lowest model level Temp (K)       '
        endif
      endif
!..........................................................
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,ISPHUM,isglev,1,1,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ISPHUM),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '87)Lowest model specific humidity (kg/kg)       '
        endif
      endif
!..........................................................
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,IZNLW,isglev,1,1,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(IZNLW),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '88)Lowest model u wind (m/s)      '
        endif
      endif
!..........................................................
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,IMERW,isglev,1,1,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(IMERW),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '89)Lowest model v wind (m/s)       '
        endif
      endif
!..........................................................
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.EQ.1._kind_io8
      call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,IHGT,isglev,1,1,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(IHGT),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '90)Lowest model level height (m) land surface      '
        endif
      endif
!..........................................................
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.EQ.1._kind_io8
      call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,IEVBS,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(IEVBS),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '91)Direct evaporation from bare soil(W/m^2) land surface      '
        endif
      endif
!..........................................................
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.EQ.1._kind_io8
      call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,IEVCW,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(IEVBS),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '92)Canopy water evaporation(W/m^2) land surface      '
        endif
      endif
!..........................................................
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.EQ.1._kind_io8
      call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,ITRAN,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(ITRAN),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '93)Transpiration (W/m^2) land surface      '
        endif
      endif
!..........................................................
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.EQ.1._kind_io8
      call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,130,ICEN,IGEN,
     &            1,ISBS,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(ISBS),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '94)Snow Sublimation (W/m^2) land surface      '
        endif
      endif
!..........................................................
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.EQ.1._kind_io8
      call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,ISNC,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(ISNC),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '95)Snow Cover (fraction) land surface      '
        endif
      endif
!..........................................................
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.EQ.1._kind_io8
      call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,ISTC,I2DBLS,0,200,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ISTC),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '96)Total column soil moisture (Kg/m^2) land surface      '
        endif
      endif

!wei: addition of 30 records ends here -------------------------------

!!
!*RADFLX*
!Clu: Addition of 7 records starts here -------------------------------
!dswrf_toa, csulf_toa, csusf_toa, csdlf_sfc, csusf_sfc, csdsf_sfc, csulf_sfc

      DO K=19, 25
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
        call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &              0,IPUR(K),ITLR(K),0,0,IYR,IMO,IDA,IHR,
     &              IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(IPUR(K)),IENS,
     &              0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          if(k.eq.19)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '97)Downward solar radiation flux (W/m**2) TOA  '
          if(k.eq.20)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '98)CS upward long wave radiation flux (W/m**2) TOA     '
          if(k.eq.21)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '99)CS upward solar radiation flux (W/m**2) TOA  '
          if(k.eq.22)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '100)CS downward long wave radiation flux (W/m**2) SFC '
          if(k.eq.23)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '101)CS upward solar radiation flux (W/m**2)  SFC'
          if(k.eq.24)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '102)CS downward solar radiation flux (W/m**2) SFC'
          if(k.eq.25)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '103)CS upward long wave radiation flux (W/m**2) SFC'
        endif
      endif
      ENDDO
!..........................................................
!     glolal=SNOHFA*RTIME
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
       LBM=slmskful.EQ.1._kind_io8
       call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,130,ICEN,IGEN,
     &            1,ISNOHF,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(ISNOHF),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '104)Snow phase-change heat flux [W/m^2] land surface   '
        endif
      endif
!..........................................................
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
       LBM=slmskful.EQ.1._kind_io8
       call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,130,ICEN,IGEN,
     &            1,ISMCWLT,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ISMCWLT),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '105)Wilting point [fraction] land surface   '
        endif
      endif
!..........................................................
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
       LBM=slmskful.EQ.1._kind_io8
       call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,130,ICEN,IGEN,
     &            1,ISMCREF,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ISMCREF),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '106)Field capacity [fraction] land surface   '
        endif
      endif
!..........................................................
Clu: Addition of 7 records ends here ---------------------------------
!..........................................................
!
!     Sunshine duration time
!
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,133,ICEN,IGEN,
     &            0,ISUNTM,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IACC,0,0,ICEN2,IDS(ISUNTM),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        if(ierr.ne.0)print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '107)Accumulated sunshine duration time (sec) '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!
!     end sunshine duration time
!
!hchuang code change add additional 2D output start here  -------------------------------
!...................................................................
! Output additional variable (averaged quantity) for GOCART
! If LGGFS3D = .TRUE.
!
      IF ( LGGFS3D ) THEN
!!      PRINT *, '***** DUMMY gribit gsm ***** ', DUMMY(96,6)
!=======================================================================
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.EQ.1._kind_io8
      call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,ISOILM,I2DBLS,0,10,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(ISOILM),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '108)Average VOL soil moist content(frac) layer 10cm -> 0cm'
        endif
      endif
!=======================================================================
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      LBM=.TRUE.
      call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,itemp,IELEV,0,2,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(itemp),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '109)Average temperature at 2 meter (K)                    '
        endif
      endif
!=======================================================================
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,iust,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(iust),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '110)Average Frictional Velocity (m/s)                     '
        endif
      endif
!=======================================================================
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,ihpbl,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(ihpbl),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '111)Average Boundary layer height                        '
        endif
      endif
!=======================================================================
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,IZNLW,IELEV,0,10,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(IZNLW),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '112)Average u wind (m/s) height 10m above ground         '
        endif
      endif
!=======================================================================
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,IMERW,IELEV,0,10,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(IMERW),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '113)Average v wind (m/s) height 10m above ground         '
        endif
      endif
!=======================================================================
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,izorl,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(izorl),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '114)Average Surface roughness (m)                        '
        endif
      endif
!=======================================================================
      ngridss=ngridss+1
      call unsplit2z(ioproc,ngridss,ngrids_flx,wrkga,global_lats_r)
      if(me.eq.ioproc) then
      call gribit_gsm(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,ISLMSK,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(ISLMSK),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit_gsm ierr=',ierr,'  ',
     x '115)Average Land-sea surface (fraction)                  '
        endif
      endif
!
      END IF   ! LGGFS3d Switch
!hchuang code change add dummy 2D output end here  ----------------------------
!!
      if(me.eq.ioproc)
     &   PRINT *,'GRIB FLUX FILE WRITTEN ',FHOUR,IDATE,noflx
!!
      RETURN

      end subroutine wrtflx_w

!
!-------------------------------------------------------------------------
!
!      INTEGER FUNCTION nfill(C)
!      implicit none
!      integer j
!      CHARACTER*(*) C
!      NFILL=LEN(C)
!      DO J=1,NFILL
!        IF(C(J:J).EQ.' ') THEN
!          NFILL=J-1
!          RETURN
!        ENDIF
!      ENDDO
!      RETURN
!      END
 
 
      SUBROUTINE nst_collect (nst_fld,global_lats_r,lonsperlar)
!!
      use resol_def,               ONLY: latr, lonr,ngrids_nst
      use mod_state,               ONLY:
     &                                   buff_mult_piecenst,ngridnst
      use layout1,                 ONLY: lats_node_r,lats_node_r_max
      use gfs_physics_nst_var_mod, ONLY: Nst_Var_Data
      USE machine,                 ONLY: kind_io8, kind_io4
      implicit none
!!
      TYPE(Nst_Var_Data)        :: nst_fld
!
      INTEGER              GLOBAL_LATS_R(latr)
      INTEGER              lonsperlar(latr)
!!
      real(kind=kind_io8) buffo(lonr,lats_node_r)
      integer kmsk(lonr,lats_node_r_max)
      integer k,il
       integer ubound
       integer icount
        integer  ierr
!!
!
      if(allocated(buff_mult_piecenst)) then
         continue
      else
         allocate
     1 (buff_mult_piecenst(lonr,lats_node_r_max,1:ngrids_nst+1))
      endif
!
      kmsk= nint(nst_fld%slmsk)
!
!-- slmsk
      ngridnst=1
      CALL uninterprez(1,kmsk,buffo,nst_fld%slmsk,
     &       global_lats_r,lonsperlar,buff_mult_piecenst(1,1,ngridnst))
!
!-- xt
      ngridnst=ngridnst+1
      CALL uninterprez(1,kmsk,buffo,nst_fld%xt,
     &       global_lats_r,lonsperlar,buff_mult_piecenst(1,1,ngridnst))
!
!-- xs
      ngridnst=ngridnst+1
      CALL uninterprez(1,kmsk,buffo,nst_fld%xs,
     &       global_lats_r,lonsperlar,buff_mult_piecenst(1,1,ngridnst))
!
!-- xu
      ngridnst=ngridnst+1
      CALL uninterprez(1,kmsk,buffo,nst_fld%xu,
     &       global_lats_r,lonsperlar,buff_mult_piecenst(1,1,ngridnst))
!
!-- xv
      ngridnst=ngridnst+1
      CALL uninterprez(1,kmsk,buffo,nst_fld%xv,
     &       global_lats_r,lonsperlar,buff_mult_piecenst(1,1,ngridnst))
!
!-- 6 xz
      ngridnst=ngridnst+1
      CALL uninterprez(1,kmsk,buffo,nst_fld%xz,
     &       global_lats_r,lonsperlar,buff_mult_piecenst(1,1,ngridnst))
!
!-- zm
      ngridnst=ngridnst+1
      CALL uninterprez(1,kmsk,buffo,nst_fld%zm,
     &       global_lats_r,lonsperlar,buff_mult_piecenst(1,1,ngridnst))
!
!-- xtts
      ngridnst=ngridnst+1
      CALL uninterprez(1,kmsk,buffo,nst_fld%xtts,
     &       global_lats_r,lonsperlar,buff_mult_piecenst(1,1,ngridnst))
!
!-- xzts
      ngridnst=ngridnst+1
      CALL uninterprez(1,kmsk,buffo,nst_fld%xzts,
     &       global_lats_r,lonsperlar,buff_mult_piecenst(1,1,ngridnst))
!
!-- 10 dt_cool
      ngridnst=ngridnst+1
      CALL uninterprez(1,kmsk,buffo,nst_fld%dt_cool,
     &       global_lats_r,lonsperlar,buff_mult_piecenst(1,1,ngridnst))
!
!-- z_c
      ngridnst=ngridnst+1
      CALL uninterprez(1,kmsk,buffo,nst_fld%z_c,
     &       global_lats_r,lonsperlar,buff_mult_piecenst(1,1,ngridnst))
!
!-- c_0
      ngridnst=ngridnst+1
      CALL uninterprez(1,kmsk,buffo,nst_fld%c_0,
     &       global_lats_r,lonsperlar,buff_mult_piecenst(1,1,ngridnst))
!
!-- c_d
      ngridnst=ngridnst+1
      CALL uninterprez(1,kmsk,buffo,nst_fld%c_d,
     &       global_lats_r,lonsperlar,buff_mult_piecenst(1,1,ngridnst))
!
!-- w_0
      ngridnst=ngridnst+1
      CALL uninterprez(1,kmsk,buffo,nst_fld%w_0,
     &       global_lats_r,lonsperlar,buff_mult_piecenst(1,1,ngridnst))
!
!-- w_d
      ngridnst=ngridnst+1
      CALL uninterprez(1,kmsk,buffo,nst_fld%w_d,
     &       global_lats_r,lonsperlar,buff_mult_piecenst(1,1,ngridnst))
!
!-- d_conv
      ngridnst=ngridnst+1
      CALL uninterprez(1,kmsk,buffo,nst_fld%d_conv,
     &       global_lats_r,lonsperlar,buff_mult_piecenst(1,1,ngridnst))
!
!-- ifd
      ngridnst=ngridnst+1
      CALL uninterprez(1,kmsk,buffo,nst_fld%ifd,
     &       global_lats_r,lonsperlar,buff_mult_piecenst(1,1,ngridnst))
!
!-- tref
      ngridnst=ngridnst+1
      CALL uninterprez(1,kmsk,buffo,nst_fld%tref,
     &       global_lats_r,lonsperlar,buff_mult_piecenst(1,1,ngridnst))
!
!-- qrain
      ngridnst=ngridnst+1
      CALL uninterprez(1,kmsk,buffo,nst_fld%qrain,
     &       global_lats_r,lonsperlar,buff_mult_piecenst(1,1,ngridnst))
!

      return
      end subroutine nst_collect
