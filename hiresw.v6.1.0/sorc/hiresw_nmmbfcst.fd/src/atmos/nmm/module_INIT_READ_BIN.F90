!-----------------------------------------------------------------------
                        module module_INIT_READ_BIN
!-----------------------------------------------------------------------
use module_include
!xxxuse module_dm_parallel,only : ids,ide,jds,jde &
!xxx                             ,ims,ime,jms,jme &
!xxx                             ,its,ite,jts,jte &
!xxx                             ,its_h2,ite_h2,jts_h2,jte_h2 &
!xxx                             ,lm &
!xxx                             ,mype_share,npes,num_pts_max &
!xxx                             ,mpi_comm_comp &
!xxx                             ,dstrb,idstrb
use module_dm_parallel,only : dstrb,idstrb
use module_exchange
use module_constants
use module_solver_internal_state,only: solver_internal_state
use module_microphysics_nmm
!
!-----------------------------------------------------------------------
!
      implicit none
!
      private
!
      public :: read_binary,physics_read_gwd
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!-----------------------------------------------------------------------
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
      contains
!
!-----------------------------------------------------------------------
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!-----------------------------------------------------------------------
!
                        subroutine read_binary &
      (INT_STATE &
      ,my_domain_id &
      ,mpi_comm_comp &
      ,mype &
      ,its,ite,jts,jte &
      ,ims,ime,jms,jme &
      ,ids,ide,jds,jde &
      ,its_h2,ite_h2,jts_h2,jte_h2 &
      ,lm &
      ,rc)
!
!-----------------------------------------------------------------------
!
implicit none
!
!------------------------
!***  Argument variables
!------------------------
!
type(solver_internal_state),pointer :: int_state

integer(kind=kint),intent(in) :: &
 its,ite &
,ims,ime &
,ids,ide &
,its_h2,ite_h2 &
,jts,jte &
,jms,jme &
,jds,jde &
,jts_h2,jte_h2 &
,lm &
,mpi_comm_comp &
,my_domain_id &
,mype 

integer(kind=kint),intent(out) :: &
 rc
!
!---------------------
!***  Local variables
!---------------------
!
integer(kind=kint) :: &
 i &                         ! index in x direction
,iend &
,irtn &
,j &                         ! index in y direction
,jend &
,k &                         ! index
,kount &
,ks &                        ! tracer index
,l &                         ! index in p direction
,length &
,n

integer(kind=kint) :: &      ! dimensions from input file
 im &
,jm &
,lmm &
,lnsh

integer(kind=kint) :: &
 iyear_fcst           &
,imonth_fcst          &
,iday_fcst            &
,ihour_fcst

real(kind=kfpt):: &
 tend,tend_max

real(kind=kfpt),dimension(int_state%NSOIL) :: &
 soil1din

real(kind=kfpt),dimension(:),allocatable :: &
 all_bc_data

real(kind=kfpt),allocatable,dimension(:,:) :: &
 temp1

integer(kind=kfpt),allocatable,dimension(:,:) :: &
 itemp

real(kind=kfpt),allocatable,dimension(:,:,:) :: &
 tempsoil

logical(kind=klog) :: opened

integer(kind=kint) :: &
 ierr 

character(64):: &
 infile

integer(kind=kint):: &
 ntsd &
,ntstm_max &
,nfcst

!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
      allocate(temp1(ids:ide,jds:jde),stat=i)
!
!-----------------------------------------------------------------------
!
      select_unit: do n=51,59
        inquire(n,opened=opened)
        if(.not.opened)then
          nfcst=n
          exit select_unit
        endif
      enddo select_unit
!
!-----------------------------------------------------------------------
!
      read_blocks: if(.not.int_state%RESTART) then                     ! cold start
!
!-----------------------------------------------------------------------
!
        write(infile,'(a,i2.2)')'input_domain_',my_domain_id
        open(unit=nfcst,file=infile,status='old',form='unformatted'     &
            ,iostat=ierr)
        if(ierr/=0)then
          write(0,*)' Unable to open ',trim(infile),' in READ_BINARY'
          rc = ierr
          return
        endif
!
!-----------------------------------------------------------------------
!
        read (nfcst) int_state%RUN, &
                     int_state%IDAT, &
                     int_state%IHRST, &
                     int_state%IHREND, &
                     NTSD
        read (nfcst) int_state%PT, &
                     int_state%PDTOP, &
                     int_state%LPT2, &
                     int_state%SGM, &
                     int_state%SG1, &
                     int_state%DSG1, &
                     int_state%SGML1, &
                     int_state%SG2, &
                     int_state%DSG2, &
                     int_state%SGML2
        read (nfcst) int_state%I_PAR_STA, &
                     int_state%J_PAR_STA
        read (nfcst) int_state%DLMD, &
                     int_state%DPHD, &
                     int_state%WBD, &
                     int_state%SBD, &
                     int_state%TLM0D, &
                     int_state%TPH0D
        read (nfcst) im,jm,lmm,lnsh
!
!-----------------------------------------------------------------------
!***  Print the time & domain information.
!-----------------------------------------------------------------------
!
        if(mype==0)then
          write(0,*) 'run, idat,ntsd: ', int_state%RUN, int_state%IDAT, NTSD
          write(0,*)' Start year =',int_state%IDAT(3)
          write(0,*)' Start month=',int_state%IDAT(2)
          write(0,*)' Start day  =',int_state%IDAT(1)
          write(0,*)' Start hour =',int_state%IHRST
          write(0,*)' Timestep   =',int_state%DT
          write(0,*)' Steps/hour =',3600./int_state%DT
          if(.not.int_state%GLOBAL)write(0,*)' Max fcst hours=',int_state%IHREND
          write(0,*) 'nmm_dyn reads of PT, PDTOP: ',int_state%PT,int_state%PDTOP
          write(0,*) 'nmm_dyn reads of I_PAR_STA, J_PAR_STA: ',int_state%I_PAR_STA,int_state%J_PAR_STA
          write(0,*) 'nmm_dyn reads of TLM0D, TPH0D: ',int_state%TLM0D,int_state%TPH0D
          write(0,*) 'nmm_dyn reads of DLMD, DPHD: ',int_state%DLMD,int_state%DPHD
          write(0,*) 'nmm_dyn reads of WBD, SBD: ',int_state%WBD,int_state%SBD
          write(0,*) 'nmm_dyn reads of IM, JM, LM, LNSH: ',im,jm,lmm,lnsh
        endif
!
!-----------------------------------------------------------------------
!
      DO L=1,LM+1
        int_state%PSG1(L)=int_state%SG1(L)*int_state%PDTOP+int_state%PT
      ENDDO
      DO L=1,LM
        int_state%PDSG1(L)=int_state%DSG1(L)*int_state%PDTOP
        int_state%PSGML1(L)=int_state%SGML1(L)*int_state%PDTOP+int_state%PT
      ENDDO
!
!-----------------------------------------------------------------------
!***  Proceed with getting fields from input file.
!***  NOTE: Five records were already read at the top of this routine.
!-----------------------------------------------------------------------
!
        if(mype==0)then
          read(nfcst)temp1
        endif
        do j=jms,jme
        do i=ims,ime
          int_state%fis(i,j)=0.
        enddo
        enddo
        call dstrb(temp1,int_state%fis,1,1,1,1,1,mype,mpi_comm_comp)
        call halo_exch(int_state%fis,1,2,2)
!-----------------------------------------------------------------------
!
        if(mype==0)then
          read(nfcst)temp1
        endif
        do j=jms,jme
        do i=ims,ime
          int_state%stdh(i,j)=0.
        enddo
        enddo
        call dstrb(temp1,int_state%stdh,1,1,1,1,1,mype,mpi_comm_comp)
        call halo_exch(int_state%stdh,1,2,2)
!-----------------------------------------------------------------------
!
        if(mype==0)then
          read(nfcst)temp1
        endif
        do j=jms,jme
        do i=ims,ime
          int_state%sm(i,j)=0.
        enddo
        enddo
        call dstrb(temp1,int_state%sm,1,1,1,1,1,mype,mpi_comm_comp)
        call halo_exch(int_state%sm,1,2,2)
!-----------------------------------------------------------------------
!
        if(mype==0)then
          read(nfcst)temp1
        endif
        do j=jms,jme
        do i=ims,ime
          int_state%pd(i,j)=0.
        enddo
        enddo
        call dstrb(temp1,int_state%pd,1,1,1,1,1,mype,mpi_comm_comp)
        call halo_exch(int_state%pd,1,2,2)
!-----------------------------------------------------------------------
!
        call mpi_barrier(mpi_comm_comp,irtn)
        do l=1,lm
          if(mype==0)then
            read(nfcst)temp1
          endif
          do j=jms,jme
          do i=ims,ime
            int_state%u(i,j,l)=0.
          enddo
          enddo
          call dstrb(temp1,int_state%u,1,1,1,lm,l,mype,mpi_comm_comp)
        enddo
        call halo_exch(int_state%u,lm,2,2)
!-----------------------------------------------------------------------
!
        do l=1,lm
          if(mype==0)then
            read(nfcst)temp1
          endif
          do j=jms,jme
          do i=ims,ime
            int_state%v(i,j,l)=0.
          enddo
          enddo
          call dstrb(temp1,int_state%v,1,1,1,lm,l,mype,mpi_comm_comp)
        enddo
        call halo_exch(int_state%v,lm,2,2)
!-----------------------------------------------------------------------
!
        do l=1,lm
          if(mype==0)then
            read(nfcst)temp1
            write(0,*) 'L, T extremes: ', L, minval(temp1),maxval(temp1)
          endif
          do j=jms,jme
          do i=ims,ime
            int_state%t(i,j,l)=0.
          enddo
          enddo
          call dstrb(temp1,int_state%t,1,1,1,lm,l,mype,mpi_comm_comp)
        enddo
        call halo_exch(int_state%t,lm,2,2)
!-----------------------------------------------------------------------
!
        do l=1,lm
          if(mype==0)then
            read(nfcst)temp1
          endif
          do j=jms,jme
          do i=ims,ime
            int_state%q(i,j,l)=0.
          enddo
          enddo
          call dstrb(temp1,int_state%q,1,1,1,lm,l,mype,mpi_comm_comp)
        enddo
        call halo_exch(int_state%q,lm,2,2)
!
        do l=1,lm
        do j=jms,jme
        do i=ims,ime
          int_state%water(i,j,l,int_state%p_qv)=int_state%q(i,j,l)/(1.-int_state%q(i,j,l))    ! WRF water array uses mixing ratio for vapor
        enddo
        enddo
        enddo
!
!-----------------------------------------------------------------------
!
        do l=1,lm
          if(mype==0)then
            read(nfcst)temp1
          endif
          do j=jms,jme
          do i=ims,ime
            int_state%cw(i,j,l)=0.
          enddo
          enddo
          call dstrb(temp1,int_state%cw,1,1,1,lm,l,mype,mpi_comm_comp)
        enddo
        call halo_exch(int_state%cw,lm,2,2)
!
!-----------------------------------------------------------------------
!
        do l=1,lm
          if(mype==0)then
            read(nfcst)temp1   ! O3
          endif
          do j=jms,jme
          do i=ims,ime
            int_state%o3(i,j,l)=0.
          enddo
          enddo
          call dstrb(temp1,int_state%o3,1,1,1,lm,l,mype,mpi_comm_comp)
        enddo
        call halo_exch(int_state%o3,lm,2,2)
!
!-----------------------------------------------------------------------
!
        int_state%NTSTI=ntsd+1
!
        tend_max=real(int_state%IHREND)
        ntstm_max=nint(tend_max*3600./int_state%DT)+1
        tend=real(int_state%nhours_fcst)
        int_state%NTSTM=nint(tend*3600./int_state%DT)+1
        if(.not.int_state%global)then
          if(mype==0)then
            write(0,*)' Max runtime is ',tend_max,' hours'
          endif
        endif
        if(mype==0)then
          write(0,*)' Requested runtime is ',tend,' hours'
          write(0,*)' NTSTM=',int_state%NTSTM
        endif
        if(int_state%NTSTM>ntstm_max.and..not.int_state%global)then
          if(mype==0)then
            write(0,*)' Requested fcst length exceeds maximum'
            write(0,*)' Resetting to maximum'
          endif
          int_state%NTSTM=min(int_state%NTSTM,ntstm_max)
        endif
!
        int_state%ihr=nint(ntsd*int_state%DT/3600.)
!
!-----------------------------------------------------------------------
        do l=1,lm
          int_state%pdsg1(l)=int_state%dsg1(l)*int_state%pdtop
          int_state%psgml1(l)=int_state%sgml1(l)*int_state%pdtop+int_state%pt
        enddo
!
        do l=1,lm+1
          int_state%psg1(l)=int_state%sg1(l)*int_state%pdtop+int_state%pt
        enddo
!-----------------------------------------------------------------------
        do j=jts,jte
          do i=its,ite
            int_state%pdo(i,j)=int_state%pd(i,j)
          enddo
        enddo
        call halo_exch(int_state%pdo,1,2,2)
!
        do l=1,lm
          do j=jts,jte
            do i=its,ite
              int_state%up(i,j,l)=int_state%u(i,j,l)
              int_state%vp(i,j,l)=int_state%v(i,j,l)
              int_state%tp(i,j,l)=int_state%t(i,j,l)
            enddo
          enddo
        enddo
        call halo_exch(int_state%tp,lm,int_state%up,lm,int_state%vp,lm,2,2)
!-----------------------------------------------------------------------
        do l=1,lm
          do j=jms,jme
            do i=ims,ime
              int_state%q2(i,j,l)=0.02
              int_state%o3(i,j,l)=0.
              if(i.ge.ide  /2+1- 6.and.i.le.ide  /2+1+ 6.and. &
                 j.ge.jde*3/4+1- 6.and.j.le.jde*3/4+1+ 6.) then !global
!                 j.ge.jde  /2+1- 6.and.j.le.jde  /2+1+ 6.) then !regional
                int_state%o3(i,j,l)=10.
              endif
              int_state%dwdt(i,j,l)=1.
              int_state%w(i,j,l)=0.
            enddo
          enddo
        enddo
        call halo_exch(int_state%dwdt,lm,2,2)
!
        do j=jts,jte
          do i=its,ite
            int_state%pint(i,j,1)=int_state%pt
          enddo
        enddo
!
        do l=1,lm
          do j=jts,jte
            do i=its,ite
              int_state%pint(i,j,l+1)=int_state%PINT(i,j,l)+int_state%DSG2(l)*int_state%PD(i,j)+int_state%PDSG1(l)
            enddo
          enddo
        enddo
        call halo_exch(int_state%pint,lm+1,2,2)
!
        call halo_exch(int_state%q2,lm,int_state%o3,lm,2,2)
        do l=1,lm
          do j=jms,jme
            do i=ims,ime
              int_state%tracers_prev(i,j,l,int_state%indx_q )=sqrt(max(int_state%q (i,j,l),0.))
              int_state%tracers_prev(i,j,l,int_state%indx_cw)=sqrt(max(int_state%cw(i,j,l),0.))
              int_state%tracers_prev(i,j,l,int_state%indx_o3)=sqrt(max(int_state%o3(i,j,l),0.))
              int_state%tracers_prev(i,j,l,int_state%indx_q2)=sqrt(max(int_state%q2(i,j,l),0.))
            enddo
          enddo
        enddo
!
!-----------------------------------------------------------------------
!---reading surface data------------------------------------------------
!-----------------------------------------------------------------------
!
        if(mype==0)then
          read(nfcst)temp1  ! ALBEDO
        endif
      CALL DSTRB(TEMP1,int_state%ALBEDO,1,1,1,1,1,mype,mpi_comm_comp)
!
        if(mype==0)then
          read(nfcst)temp1  ! ALBASE
        endif
      CALL DSTRB(TEMP1,int_state%ALBASE,1,1,1,1,1,mype,mpi_comm_comp)
!
        if(mype==0)then
          read(nfcst)temp1  ! EPSR
        endif
      CALL DSTRB(TEMP1,int_state%EPSR,1,1,1,1,1,mype,mpi_comm_comp)
!
        if(mype==0)then
          read(nfcst)temp1  ! MXSNAL
        endif
      CALL DSTRB(TEMP1,int_state%MXSNAL,1,1,1,1,1,mype,mpi_comm_comp)
!
        if(mype==0)then
          read(nfcst)temp1  ! TSKIN
        endif
      CALL DSTRB(TEMP1,int_state%TSKIN,1,1,1,1,1,mype,mpi_comm_comp)
!
        if(mype==0)then
          read(nfcst)temp1  ! SST
        endif
      CALL DSTRB(TEMP1,int_state%SST,1,1,1,1,1,mype,mpi_comm_comp)
!
        if(mype==0)then
          read(nfcst)temp1  ! SNO
        endif
      CALL DSTRB(TEMP1,int_state%SNO,1,1,1,1,1,mype,mpi_comm_comp)
!
        if(mype==0)then
          read(nfcst)temp1  ! SI
        endif
      CALL DSTRB(TEMP1,int_state%SI,1,1,1,1,1,mype,mpi_comm_comp)
!
        if(mype==0)then
          read(nfcst)temp1  ! SICE
        endif
      CALL DSTRB(TEMP1,int_state%SICE,1,1,1,1,1,mype,mpi_comm_comp)
!
        if(mype==0)then
          read(nfcst) temp1  ! TG
        endif
      CALL DSTRB(TEMP1,int_state%TG,1,1,1,1,1,mype,mpi_comm_comp)
!
        if(mype==0)then
          read(nfcst) temp1  ! CMC
        endif
      CALL DSTRB(TEMP1,int_state%CMC,1,1,1,1,1,mype,mpi_comm_comp)
!
        if(mype==0)then
          read(nfcst) temp1  ! SR
        endif
      CALL DSTRB(TEMP1,int_state%SR,1,1,1,1,1,mype,mpi_comm_comp)
!
        if(mype==0)then
          read(nfcst) temp1  ! USTAR
        endif
      CALL DSTRB(TEMP1,int_state%USTAR,1,1,1,1,1,mype,mpi_comm_comp)
!
        if(mype==0)then
          read(nfcst) temp1  ! Z0
        endif
      CALL DSTRB(TEMP1,int_state%Z0,1,1,1,1,1,mype,mpi_comm_comp)
      CALL HALO_EXCH(int_state%Z0,1,3,3)
!
        if(mype==0)then
          read(nfcst) temp1  ! Z0BASE
        endif
      CALL DSTRB(TEMP1,int_state%Z0BASE,1,1,1,1,1,mype,mpi_comm_comp)
      CALL HALO_EXCH(int_state%Z0BASE,1,3,3)
!
      ALLOCATE(TEMPSOIL(1:int_state%NSOIL,IDS:IDE,JDS:JDE),STAT=I)
!
        if(mype==0)then
          read(nfcst) TEMPSOIL  ! STC
        endif
      CALL DSTRB(TEMPSOIL(1,IDS:IDE,JDS:JDE),int_state%STC(IMS:IME,JMS:JME,1),1,1,1,1,1,mype,mpi_comm_comp)
      CALL DSTRB(TEMPSOIL(2,IDS:IDE,JDS:JDE),int_state%STC(IMS:IME,JMS:JME,2),1,1,1,1,1,mype,mpi_comm_comp)
      CALL DSTRB(TEMPSOIL(3,IDS:IDE,JDS:JDE),int_state%STC(IMS:IME,JMS:JME,3),1,1,1,1,1,mype,mpi_comm_comp)
      CALL DSTRB(TEMPSOIL(4,IDS:IDE,JDS:JDE),int_state%STC(IMS:IME,JMS:JME,4),1,1,1,1,1,mype,mpi_comm_comp)
!
        if(mype==0)then
          read(nfcst) TEMPSOIL  ! SMC
        endif
      CALL DSTRB(TEMPSOIL(1,:,:),int_state%SMC(:,:,1),1,1,1,1,1,mype,mpi_comm_comp)
      CALL DSTRB(TEMPSOIL(2,:,:),int_state%SMC(:,:,2),1,1,1,1,1,mype,mpi_comm_comp)
      CALL DSTRB(TEMPSOIL(3,:,:),int_state%SMC(:,:,3),1,1,1,1,1,mype,mpi_comm_comp)
      CALL DSTRB(TEMPSOIL(4,:,:),int_state%SMC(:,:,4),1,1,1,1,1,mype,mpi_comm_comp)
!
        if(mype==0)then
          read(nfcst) TEMPSOIL  ! SH2O
        endif
      CALL DSTRB(TEMPSOIL(1,:,:),int_state%SH2O(:,:,1),1,1,1,1,1,mype,mpi_comm_comp)
      CALL DSTRB(TEMPSOIL(2,:,:),int_state%SH2O(:,:,2),1,1,1,1,1,mype,mpi_comm_comp)
      CALL DSTRB(TEMPSOIL(3,:,:),int_state%SH2O(:,:,3),1,1,1,1,1,mype,mpi_comm_comp)
      CALL DSTRB(TEMPSOIL(4,:,:),int_state%SH2O(:,:,4),1,1,1,1,1,mype,mpi_comm_comp)
!
      DEALLOCATE(TEMPSOIL)
      ALLOCATE(ITEMP(IDS:IDE,JDS:JDE),STAT=I)
!
        if(mype==0)then
          read(nfcst) ITEMP  ! ISLTYP
        endif
      CALL IDSTRB(ITEMP,int_state%ISLTYP,mype,mpi_comm_comp)
!
        if(mype==0)then
          read(nfcst) ITEMP  ! IVGTYP
        endif
      CALL IDSTRB(ITEMP,int_state%IVGTYP,mype,mpi_comm_comp)
!
      DEALLOCATE(ITEMP)
!
        if(mype==0)then
          read(nfcst) temp1  ! VEGFRC
        endif
      CALL DSTRB(TEMP1,int_state%VEGFRC,1,1,1,1,1,mype,mpi_comm_comp)
!
        if(mype==0)then
          read(nfcst) SOIL1DIN  ! DZSOIL
        endif
!
        if(mype==0)then
          read(nfcst) SOIL1DIN  ! SLDPTH
        endif
!
!       if(mype==0)then               ! here will be 14 orography fields for GWD
!         do n=1,14
!           read(nfcst) temp1
!         enddo
!       endif
!
!-----------------------------------------------------------------------
!
        close(nfcst)
!
!-----------------------------------------------------------------------
      else  read_blocks                         ! restart
!-----------------------------------------------------------------------
!
        write(infile,'(a,i2.2)')'restart_file_',my_domain_id
        open(unit=nfcst,file=infile,status='old',form='unformatted'     &
            ,iostat=ierr)
        if(ierr/=0)then
          write(0,*)' Unable to open ',trim(infile),' in READ_BINARY'
          rc = ierr
          return
        endif
!
!-----------------------------------------------------------------------
!***  Read from restart file: Integer scalars
!-----------------------------------------------------------------------
        read(nfcst) iyear_fcst
        read(nfcst) imonth_fcst
        read(nfcst) iday_fcst
        read(nfcst) ihour_fcst
        read(nfcst) !iminute_fcst
        read(nfcst) ! second_fcst
        read(nfcst) ! ntsd
        read(nfcst) ! im
        read(nfcst) ! jm
        read(nfcst) ! lm
        read(nfcst) int_state%IHRST
        read(nfcst) int_state%I_PAR_STA
        read(nfcst) int_state%J_PAR_STA
        read(nfcst) int_state%LAST_STEP_MOVED
        read(nfcst) int_state%LPT2
        read(nfcst) ! nsoil
        read(nfcst) ! nphs
        read(nfcst) ! nclod
        read(nfcst) ! nheat
        read(nfcst) ! nmts
        read(nfcst) ! nprec
        read(nfcst) ! nrdlw
        read(nfcst) ! nrdsw
        read(nfcst) ! nsrfc
!
!-----------------------------------------------------------------------
!***  Read from restart file: Integer 1D arrays
!-----------------------------------------------------------------------
        read(nfcst) ! int_state%NTSCM
        read(nfcst) int_state%IDAT
!
        if(mype==0)then
          write(0,*)'**** read in core *******************'
          write(0,*)' Restart year =',iyear_fcst
          write(0,*)' Restart month=',imonth_fcst
          write(0,*)' Restart day  =',iday_fcst
          write(0,*)' Restart hour =',ihour_fcst
          write(0,*)' Original start year =',int_state%IDAT(3)
          write(0,*)' Original start month=',int_state%IDAT(2)
          write(0,*)' Original start day  =',int_state%IDAT(1)
          write(0,*)' Original start hour =',int_state%IHRST
          write(0,*)' Timestep   =',int_state%DT
          write(0,*)' Steps/hour =',3600./int_state%DT
          write(0,*)'*************************************'
        endif
!
!-----------------------------------------------------------------------
!***  Read from restart file: Integer scalars
!-----------------------------------------------------------------------
        read(nfcst) ! mp_physics
        read(nfcst) ! sf_surface_physics
!-----------------------------------------------------------------------
!***  Read from restart file: Real scalars
!-----------------------------------------------------------------------
        read(nfcst) ! dt
        read(nfcst) ! dyh
        read(nfcst) int_state%PDTOP
        read(nfcst) int_state%PT
        read(nfcst) int_state%TLM0D
        read(nfcst) int_state%TPH0D
        read(nfcst) ! tstart
        read(nfcst) int_state%DPHD
        read(nfcst) int_state%DLMD
        read(nfcst) int_state%SBD
        read(nfcst) int_state%WBD
!-----------------------------------------------------------------------
!***  Read from restart file: Real 1D arrays
!-----------------------------------------------------------------------
        read(nfcst) ! dxh
        read(nfcst) int_state%SG1
        read(nfcst) int_state%SG2
        read(nfcst) int_state%DSG1
        read(nfcst) int_state%DSG2
        read(nfcst) int_state%SGML1
        read(nfcst) int_state%SGML2
        read(nfcst) int_state%SGM
        read(nfcst) int_state%SLDPTH
        read(nfcst) int_state%MP_RESTART_STATE
        read(nfcst) int_state%TBPVS_STATE
        read(nfcst) int_state%TBPVS0_STATE
!
        DO L=1,LM
          int_state%PDSG1(L)=int_state%DSG1(L)*int_state%PDTOP
          int_state%PSGML1(L)=int_state%SGML1(L)*int_state%PDTOP+int_state%PT
        ENDDO
!
        DO L=1,LM+1
          int_state%PSG1(L)=int_state%SG1(L)*int_state%PDTOP+int_state%PT
        ENDDO
!
!-----------------------------------------------------------------------
!***  Read in the full-domain 1-D datastring of boundary winds.
!***  Each task isolates its own piece of that data.
!-----------------------------------------------------------------------
!
        length=(5*lm+1)*2*2*int_state%lnsv*((ide-ids+1)+(jde-jds+1))
        allocate(all_bc_data(1:length))
!
        read(nfcst) all_bc_data
!
!-----------------------------------------------------------------------
!
        kount=0
!
!-----------------------------------------------------------------------
!
        iend=min(ite_h2,ide)
        do n=1,2
        do j=1,int_state%lnsv
        do i=ids,ide
          if(jts==jds.and.i>=its_h2.and.i<=iend)then                         !<-- South boundary tasks extract their BC winds
            int_state%pdbs(i,j,n)=all_bc_data(kount+1)
          endif
          kount=kount+1
        enddo
        enddo
        do l=1,lm
        do j=1,int_state%lnsv
        do i=ids,ide
          if(jts==jds.and.i>=its_h2.and.i<=iend)then                         !<-- South boundary tasks extract their BC winds
            int_state%tbs(i,j,l,n)=all_bc_data(kount+1)
            int_state%qbs(i,j,l,n)=all_bc_data(kount+2)
            int_state%wbs(i,j,l,n)=all_bc_data(kount+3)
            int_state%ubs(i,j,l,n)=all_bc_data(kount+4)
            int_state%vbs(i,j,l,n)=all_bc_data(kount+5)
          endif
          kount=kount+5
        enddo
        enddo
        enddo
        enddo
!
        do n=1,2
        do j=1,int_state%lnsv
        do i=ids,ide
          if(jte==jde.and.i>=its_h2.and.i<=iend)then                         !<-- North boundary tasks extract their BC winds
            int_state%pdbn(i,j,n)=all_bc_data(kount+1)
          endif
          kount=kount+1
        enddo
        enddo
        do l=1,lm
        do j=1,int_state%lnsv
        do i=ids,ide
          if(jte==jde.and.i>=its_h2.and.i<=iend)then                         !<-- North boundary tasks extract their BC winds
            int_state%tbn(i,j,l,n)=all_bc_data(kount+1)
            int_state%qbn(i,j,l,n)=all_bc_data(kount+2)
            int_state%wbn(i,j,l,n)=all_bc_data(kount+3)
            int_state%ubn(i,j,l,n)=all_bc_data(kount+4)
            int_state%vbn(i,j,l,n)=all_bc_data(kount+5)
          endif
          kount=kount+5
        enddo
        enddo
        enddo
        enddo
!
        jend=min(jte_h2,jde)
        do n=1,2
        do j=jds,jde
        do i=1,int_state%lnsv
          if(its==ids.and.j>=jts_h2.and.j<=jend)then                         !<-- West boundary tasks extract their BC winds
            int_state%pdbw(i,j,n)=all_bc_data(kount+1)
          endif
          kount=kount+1
        enddo
        enddo
        do l=1,lm
        do j=jds,jde
        do i=1,int_state%lnsv
          if(its==ids.and.j>=jts_h2.and.j<=jend)then                         !<-- West boundary tasks extract their BC winds
            int_state%tbw(i,j,l,n)=all_bc_data(kount+1)
            int_state%qbw(i,j,l,n)=all_bc_data(kount+2)
            int_state%wbw(i,j,l,n)=all_bc_data(kount+3)
            int_state%ubw(i,j,l,n)=all_bc_data(kount+4)
            int_state%vbw(i,j,l,n)=all_bc_data(kount+5)
          endif
          kount=kount+5
        enddo
        enddo
        enddo
        enddo
!
        do n=1,2
        do j=jds,jde
        do i=1,int_state%lnsv
          if(ite==ide.and.j>=jts_h2.and.j<=jend)then                         !<-- West boundary tasks extract their BC winds
            int_state%pdbe(i,j,n)=all_bc_data(kount+1)
          endif
          kount=kount+1
        enddo
        enddo
        do l=1,lm
        do j=jds,jde
        do i=1,int_state%lnsv
          if(ite==ide.and.j>=jts_h2.and.j<=jend)then                         !<-- West boundary tasks extract their BC winds
            int_state%tbe(i,j,l,n)=all_bc_data(kount+1)
            int_state%qbe(i,j,l,n)=all_bc_data(kount+2)
            int_state%wbe(i,j,l,n)=all_bc_data(kount+3)
            int_state%ube(i,j,l,n)=all_bc_data(kount+4)
            int_state%vbe(i,j,l,n)=all_bc_data(kount+5)
          endif
          kount=kount+5
        enddo
        enddo
        enddo
        enddo
!
        deallocate(all_bc_data)
!
!-----------------------------------------------------------------------
!***  Read from restart file: Logical
!-----------------------------------------------------------------------
        read(nfcst) ! global
        read(nfcst) int_state%RUN
        read(nfcst) ! adiabatic
!
!-----------------------------------------------------------------------
!***  Read from restart file: Integer 2D arrays
!-----------------------------------------------------------------------
!
      ALLOCATE(ITEMP(IDS:IDE,JDS:JDE),STAT=I)
!
      IF(MYPE==0)THEN
        READ(NFCST) ITEMP
      ENDIF
      CALL IDSTRB(ITEMP,int_state%ISLTYP,MYPE,MPI_COMM_COMP)
!
      IF(MYPE==0)THEN
        READ(NFCST) ITEMP
      ENDIF
      CALL IDSTRB(ITEMP,int_state%IVGTYP,MYPE,MPI_COMM_COMP)
!
      IF(MYPE==0)THEN
        READ(NFCST) ITEMP
      ENDIF
      CALL IDSTRB(ITEMP,int_state%NCFRCV,MYPE,MPI_COMM_COMP)
!
      IF(MYPE==0)THEN
        READ(NFCST) ITEMP
      ENDIF
      CALL IDSTRB(ITEMP,int_state%NCFRST,MYPE,MPI_COMM_COMP)
!
      DEALLOCATE(ITEMP)
!
!-----------------------------------------------------------------------
!***  Read from restart file: Real 2D arrays
!-----------------------------------------------------------------------
!-- fis
        if(mype==0)then
          read(nfcst)temp1
        endif
        do j=jms,jme
        do i=ims,ime
          int_state%fis(i,j)=0.
        enddo
        enddo
        call dstrb(temp1,int_state%fis,1,1,1,1,1,mype,mpi_comm_comp)
        call halo_exch(int_state%fis,1,2,2)
!-----------------------------------------------------------------------
!-- glat
        if(mype==0)then
          read(nfcst)temp1
        endif
        do j=jms,jme
        do i=ims,ime
          int_state%glat(i,j)=0.
        enddo
        enddo
        call dstrb(temp1,int_state%glat,1,1,1,1,1,mype,mpi_comm_comp)
        call halo_exch(int_state%glat,1,2,2)
!-----------------------------------------------------------------------
!-- glon
        if(mype==0)then
          read(nfcst)temp1
        endif
        do j=jms,jme
        do i=ims,ime
          int_state%glon(i,j)=0.
        enddo
        enddo
        call dstrb(temp1,int_state%glon,1,1,1,1,1,mype,mpi_comm_comp)
        call halo_exch(int_state%glon,1,2,2)
!-----------------------------------------------------------------------
!-- hdacx
        if(mype==0)then
          read(nfcst)temp1
        endif
        do j=jms,jme
        do i=ims,ime
          int_state%hdacx(i,j)=0.
        enddo
        enddo
        call dstrb(temp1,int_state%hdacx,1,1,1,1,1,mype,mpi_comm_comp)
        call halo_exch(int_state%hdacx,1,2,2)
!-----------------------------------------------------------------------
!-- hdacy
        if(mype==0)then
          read(nfcst)temp1
        endif
        do j=jms,jme
        do i=ims,ime
          int_state%hdacy(i,j)=0.
        enddo
        enddo
        call dstrb(temp1,int_state%hdacy,1,1,1,1,1,mype,mpi_comm_comp)
        call halo_exch(int_state%hdacy,1,2,2)
!-----------------------------------------------------------------------
!--pd
        if(mype==0)then
          read(nfcst)temp1
        endif
        do j=jms,jme
        do i=ims,ime
          int_state%pd(i,j)=0.
        enddo
        enddo
        call dstrb(temp1,int_state%pd,1,1,1,1,1,mype,mpi_comm_comp)
        call halo_exch(int_state%pd,1,2,2)
!-----------------------------------------------------------------------
!-- f
        if(mype==0)then
          read(nfcst)temp1
        endif
        do j=jms,jme
        do i=ims,ime
          int_state%f(i,j)=0.
        enddo
        enddo
        call dstrb(temp1,int_state%f,1,1,1,1,1,mype,mpi_comm_comp)
        call halo_exch(int_state%f,1,2,2)
!-----------------------------------------------------------------------
!-- vlat
        if(mype==0)then
          read(nfcst)temp1
        endif
        do j=jms,jme
        do i=ims,ime
          int_state%vlat(i,j)=0.
        enddo
        enddo
        call dstrb(temp1,int_state%vlat,1,1,1,1,1,mype,mpi_comm_comp)
        call halo_exch(int_state%vlat,1,2,2)
!-----------------------------------------------------------------------
!-- vlon
        if(mype==0)then
          read(nfcst)temp1
        endif
        do j=jms,jme
        do i=ims,ime
          int_state%vlon(i,j)=0.
        enddo
        enddo
        call dstrb(temp1,int_state%vlon,1,1,1,1,1,mype,mpi_comm_comp)
        call halo_exch(int_state%vlon,1,2,2)
!-----------------------------------------------------------------------
!-- hdacvx
        if(mype==0)then
          read(nfcst)temp1
        endif
        do j=jms,jme
        do i=ims,ime
          int_state%hdacvx(i,j)=0.
        enddo
        enddo
        call dstrb(temp1,int_state%hdacvx,1,1,1,1,1,mype,mpi_comm_comp)
        call halo_exch(int_state%hdacvx,1,2,2)
!-----------------------------------------------------------------------
!-- hdacvy
        if(mype==0)then
          read(nfcst)temp1
        endif
        do j=jms,jme
        do i=ims,ime
          int_state%hdacvy(i,j)=0.
        enddo
        enddo
        call dstrb(temp1,int_state%hdacvy,1,1,1,1,1,mype,mpi_comm_comp)
        call halo_exch(int_state%hdacvy,1,2,2)
!-----------------------------------------------------------------------
!-- pdo
        if(mype==0)then
          read(nfcst)temp1
        endif
        do j=jms,jme
        do i=ims,ime
          int_state%pdo(i,j)=0.
        enddo
        enddo
        call dstrb(temp1,int_state%pdo,1,1,1,1,1,mype,mpi_comm_comp)
        call halo_exch(int_state%pdo,1,2,2)
!
!-----------------------------------------------------------------------
!***  ACFRCV
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
!
!d      DO J=JMS,JME
!d      DO I=IMS,IME
!d        int_state%ACFRCV(I,J)=0.
!d      ENDDO
!d      ENDDO
      CALL DSTRB(TEMP1,int_state%ACFRCV,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  ACFRST
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
!
!d      DO J=JMS,JME
!d      DO I=IMS,IME
!d        int_state%ACFRST(I,J)=0.
!d      ENDDO
!d      ENDDO
      CALL DSTRB(TEMP1,int_state%ACFRST,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  ACPREC
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
!
!d      DO J=JMS,JME
!d      DO I=IMS,IME
!d        int_state%ACPREC(I,J)=0.
!d      ENDDO
!d      ENDDO
      CALL DSTRB(TEMP1,int_state%ACPREC,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  ACSNOM
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
!
!d      DO J=JMS,JME
!d      DO I=IMS,IME
!d        int_state%ACSNOM(I,J)=0.
!!dd      ENDDO
!d      ENDDO
      CALL DSTRB(TEMP1,int_state%ACSNOM,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  ACSNOW
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
!
!d      DO J=JMS,JME
!d      DO I=IMS,IME
!d        int_state%ACSNOW(I,J)=0.
!d      ENDDO
!d      ENDDO
      CALL DSTRB(TEMP1,int_state%ACSNOW,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  AKHS_OUT
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
!
!d      DO J=JMS,JME
!d      DO I=IMS,IME
!d        int_state%AKHS_OUT(I,J)=0.
!d      ENDDO
!d      ENDDO
      CALL DSTRB(TEMP1,int_state%AKHS_OUT,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  AKMS_OUT
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
!
!d      DO J=JMS,JME
!d      DO I=IMS,IME
!d        int_state%AKMS_OUT(I,J)=0.
!d      ENDDO
!d      ENDDO
      CALL DSTRB(TEMP1,int_state%AKMS_OUT,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  ALBASE
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
!
!d      DO J=JMS,JME
!d      DO I=IMS,IME
!d        int_state%ALBASE(I,J)=0.
!d      ENDDO
!d      ENDDO
      CALL DSTRB(TEMP1,int_state%ALBASE,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  ALBEDO
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%ALBEDO,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  ALWIN
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%ALWIN,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  ALWOUT
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%ALWOUT,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  ALWTOA
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%ALWTOA,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  ASWIN
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%ASWIN,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  ASWOUT
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%ASWOUT,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  ASWTOA
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%ASWTOA,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  BGROFF
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%BGROFF,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  CFRACH
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%CFRACH,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  CFRACL
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%CFRACL,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  CFRACM
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%CFRACM,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  CLDEFI
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%CLDEFI,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  CMC
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%CMC,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  CNVBOT
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%CNVBOT,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  CNVTOP
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%CNVTOP,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  CPRATE
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%CPRATE,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  CUPPT
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%CUPPT,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  CUPREC
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%CUPREC,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  CZEN
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%CZEN,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  CZMEAN
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%CZMEAN,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  EPSR
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%EPSR,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  GRNFLX
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%GRNFLX,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  HBOTD
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%HBOTD,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  HBOTS
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%HBOTS,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  HTOPD
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%HTOPD,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  HTOPS
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%HTOPS,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  SNOW ALBEDO
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%MXSNAL,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  PBLH
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%PBLH,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  POTEVP
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%POTEVP,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  PREC
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%PREC,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  PSHLTR
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%PSHLTR,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  Q10
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%Q10,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  QSH
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%QSH,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  QSHLTR
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%QSHLTR,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  QWBS
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%QWBS,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  QZ0
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%QZ0,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  RADOT
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%RADOT,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  RLWIN
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%RLWIN,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  RLWTOA
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%RLWTOA,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  RSWIN
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%RSWIN,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  RSWINC
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%RSWINC,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  RSWOUT
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%RSWOUT,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  SFCEVP
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%SFCEVP,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  SFCEXC
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%SFCEXC,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  SFCLHX
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%SFCLHX,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  SFCSHX
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%SFCSHX,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  SI
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%SI,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  SICE
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%SICE,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  SIGT4
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%SIGT4,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  SM (Seamask)
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
!
      DO J=JMS,JME
      DO I=IMS,IME
        int_state%SM(I,J)=0.
      ENDDO
      ENDDO
      CALL DSTRB(TEMP1,int_state%SM,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  SMSTAV
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%SMSTAV,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  SMSTOT
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%SMSTOT,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  SNO
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%SNO,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  SNOPCX
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%SNOPCX,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  SOILTB
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%SOILTB,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  SR
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%SR,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  SSROFF
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%SSROFF,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  SST
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%SST,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  SUBSHX
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%SUBSHX,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  TG
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%TG,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  TH10
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%TH10,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  THS
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%THS,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  THZ0
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%THZ0,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  TSHLTR
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%TSHLTR,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  TWBS
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%TWBS,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  U10
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%U10,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  USTAR
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%USTAR,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  UZ0
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%UZ0,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  V10
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%V10,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  VEGFRC
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST) TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%VEGFRC,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  VZ0
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST) TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%VZ0,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  Z0
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%Z0,1,1,1,1,1,MYPE,MPI_COMM_COMP)
      CALL HALO_EXCH(int_state%Z0,1,3,3)
!-----------------------------------------------------------------------
!***  TSKIN
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%TSKIN,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  AKHS
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%AKHS,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  AKMS
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%AKMS,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  HBOT
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%HBOT,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  HTOP
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%HTOP,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  RSWTOA
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%RSWTOA,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!
!-----------------------------------------------------------------------
!***  POTFLX
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%POTFLX,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!
!-----------------------------------------------------------------------
!***  RMOL
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%RMOL,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!
!-----------------------------------------------------------------------
!***  T2
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%T2,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!
!-----------------------------------------------------------------------
!***  Z0BASE
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%Z0BASE,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!
!-----------------------------------------------------------------------
!***  TLMIN
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%TLMIN,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!
!-----------------------------------------------------------------------
!***  TLMAX
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%TLMAX,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!
!-----------------------------------------------------------------------
!***  ACUTIM
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%ACUTIM,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  APHTIM
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%APHTIM,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  ARDLW
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%ARDLW,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  ARDSW
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%ARDSW,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  ASRFC
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%ASRFC,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  AVRAIN
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%AVRAIN,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!***  AVCNVC
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NFCST)TEMP1
      ENDIF
      CALL DSTRB(TEMP1,int_state%AVCNVC,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!
!-----------------------------------------------------------------------
!***  Read from restart file: Real 3D arrays (only DYN)
!-----------------------------------------------------------------------
!
        do l=1,lm
          if(mype==0)then
            read(nfcst)temp1
          endif
!d          do j=jms,jme
!d          do i=ims,ime
!d            int_state%w(i,j,l)=0.
!d          enddo
!d          enddo
          call dstrb(temp1,int_state%w,1,1,1,lm,l,mype,mpi_comm_comp)
        enddo
        call halo_exch(int_state%w,lm,2,2)
!-----------------------------------------------------------------------
        do l=1,lm
          if(mype==0)then
            read(nfcst)temp1
          endif
!d          do j=jms,jme
!d          do i=ims,ime
!d            int_state%omgalf(i,j,l)=0.
!d          enddo
!d          enddo
          call dstrb(temp1,int_state%omgalf,1,1,1,lm,l,mype,mpi_comm_comp)
        enddo
        call halo_exch(int_state%omgalf,lm,2,2)
!-----------------------------------------------------------------------
        do l=1,lm
          if(mype==0)then
            read(nfcst)temp1
          endif
!d          do j=jms,jme
!d          do i=ims,ime
!d            int_state%o3(i,j,l)=0.
!d          enddo
!d          enddo
          call dstrb(temp1,int_state%o3,1,1,1,lm,l,mype,mpi_comm_comp)
        enddo
        call halo_exch(int_state%o3,lm,2,2)
!-----------------------------------------------------------------------
        do l=1,lm
          if(mype==0)then
            read(nfcst)temp1
          endif
!d          do j=jms,jme
!d          do i=ims,ime
!d            int_state%div(i,j,l)=0.
!d          enddo
!d          enddo
          call dstrb(temp1,int_state%div,1,1,1,lm,l,mype,mpi_comm_comp)
        enddo
        call halo_exch(int_state%div,lm,2,2)
!-----------------------------------------------------------------------
        do l=1,lm
          if(mype==0)then
            read(nfcst)temp1
          endif
!d          do j=jms,jme
!d          do i=ims,ime
!d            int_state%rtop(i,j,l)=0.
!d          enddo
!d          enddo
          call dstrb(temp1,int_state%rtop,1,1,1,lm,l,mype,mpi_comm_comp)
        enddo
        call halo_exch(int_state%rtop,lm,2,2)
!-----------------------------------------------------------------------
        do l=1,lm
          if(mype==0)then
            read(nfcst)temp1
          endif
!d          do j=jms,jme
!d          do i=ims,ime
!d            int_state%tcu(i,j,l)=0.
!d          enddo
!d          enddo
          call dstrb(temp1,int_state%tcu,1,1,1,lm,l,mype,mpi_comm_comp)
        enddo
        call halo_exch(int_state%tcu,lm,2,2)
!-----------------------------------------------------------------------
        do l=1,lm
          if(mype==0)then
            read(nfcst)temp1
          endif
!d          do j=jms,jme
!d          do i=ims,ime
!d            int_state%tcv(i,j,l)=0.
!d          enddo
!d          enddo
          call dstrb(temp1,int_state%tcv,1,1,1,lm,l,mype,mpi_comm_comp)
        enddo
        call halo_exch(int_state%tcv,lm,2,2)
!-----------------------------------------------------------------------
        do l=1,lm
          if(mype==0)then
            read(nfcst)temp1
          endif
!d          do j=jms,jme
!d          do i=ims,ime
!d            int_state%tct(i,j,l)=0.
!d          enddo
!d          enddo
          call dstrb(temp1,int_state%tct,1,1,1,lm,l,mype,mpi_comm_comp)
        enddo
        call halo_exch(int_state%tct,lm,2,2)
!-----------------------------------------------------------------------
        do l=1,lm
          if(mype==0)then
            read(nfcst)temp1
          endif
!d          do j=jms,jme
!d          do i=ims,ime
!d            int_state%tp(i,j,l)=0.
!d          enddo
!d          enddo
          call dstrb(temp1,int_state%tp,1,1,1,lm,l,mype,mpi_comm_comp)
        enddo
        call halo_exch(int_state%tp,lm,2,2)
!-----------------------------------------------------------------------
        do l=1,lm
          if(mype==0)then
            read(nfcst)temp1
          endif
!d          do j=jms,jme
!d          do i=ims,ime
!d            int_state%up(i,j,l)=0.
!d          enddo
!d          enddo
          call dstrb(temp1,int_state%up,1,1,1,lm,l,mype,mpi_comm_comp)
        enddo
        call halo_exch(int_state%up,lm,2,2)
!-----------------------------------------------------------------------
        do l=1,lm
          if(mype==0)then
            read(nfcst)temp1
          endif
!d          do j=jms,jme
!d          do i=ims,ime
!d            int_state%vp(i,j,l)=0.
!d          enddo
!d          enddo
          call dstrb(temp1,int_state%vp,1,1,1,lm,l,mype,mpi_comm_comp)
        enddo
        call halo_exch(int_state%vp,lm,2,2)
!-----------------------------------------------------------------------
        do l=1,lm
          if(mype==0)then
            read(nfcst)temp1
          endif
!d          do j=jms,jme
!d          do i=ims,ime
!d            int_state%e2(i,j,l)=0.
!d          enddo
!d          enddo
          call dstrb(temp1,int_state%e2,1,1,1,lm,l,mype,mpi_comm_comp)
        enddo
        call halo_exch(int_state%e2,lm,2,2)
!----------- psgdt -----------------------------------------------------
        do l=1,lm-1
          if(mype==0)then
            read(nfcst)temp1
          endif
        enddo
!-----------------------------------------------------------------------
        do l=1,lm
          if(mype==0)then
            read(nfcst)temp1
          endif
!d          do j=jms,jme
!d          do i=ims,ime
!d            int_state%z(i,j,l)=0.
!d          enddo
!d          enddo
          call dstrb(temp1,int_state%z,1,1,1,lm,l,mype,mpi_comm_comp)
        enddo
        call halo_exch(int_state%z,lm,2,2)
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!***  Read from restart file: Real 3D arrays
!-----------------------------------------------------------------------
        call mpi_barrier(mpi_comm_comp,irtn)
!-----------------------------------------------------------------------
        do l=1,lm
          if(mype==0)then
            read(nfcst)temp1
          endif
          do j=jms,jme
          do i=ims,ime
            int_state%Told(i,j,l)=0.
          enddo
          enddo
          call dstrb(temp1,int_state%Told,1,1,1,lm,l,mype,mpi_comm_comp)
        enddo
        call halo_exch(int_state%Told,lm,2,2)
!-----------------------------------------------------------------------
        do l=1,lm
          if(mype==0)then
            read(nfcst)temp1
          endif
          do j=jms,jme
          do i=ims,ime
            int_state%Tadj(i,j,l)=0.
          enddo
          enddo
          call dstrb(temp1,int_state%Tadj,1,1,1,lm,l,mype,mpi_comm_comp)
        enddo
        call halo_exch(int_state%Tadj,lm,2,2)
!-----------------------------------------------------------------------
        do l=1,lm
          if(mype==0)then
            read(nfcst)temp1 ! cldfra
          endif
          call dstrb(temp1,int_state%CLDFRA,1,1,1,lm,l,mype,mpi_comm_comp)
        enddo
!-----------------------------------------------------------------------
        do l=1,lm
          if(mype==0)then
            read(nfcst)temp1
          endif
          do j=jms,jme
          do i=ims,ime
            int_state%cw(i,j,l)=0.
          enddo
          enddo
          call dstrb(temp1,int_state%cw,1,1,1,lm,l,mype,mpi_comm_comp)
        enddo
        call halo_exch(int_state%cw,lm,2,2)
!-----------------------------------------------------------------------
        do l=1,lm
          if(mype==0)then
            read(nfcst)temp1
          endif
          do j=jms,jme
          do i=ims,ime
            int_state%q(i,j,l)=0.
          enddo
          enddo
          call dstrb(temp1,int_state%q,1,1,1,lm,l,mype,mpi_comm_comp)
        enddo
        call halo_exch(int_state%q,lm,2,2)
!-----------------------------------------------------------------------
        do l=1,lm
          if(mype==0)then
            read(nfcst)temp1
          endif
          do j=jms,jme
          do i=ims,ime
            int_state%q2(i,j,l)=0.
          enddo
          enddo
          call dstrb(temp1,int_state%q2,1,1,1,lm,l,mype,mpi_comm_comp)
        enddo
        call halo_exch(int_state%q2,lm,2,2)
!-----------------------------------------------------------------------
        do l=1,lm
          if(mype==0)then
            read(nfcst)temp1 ! rlwtt
          endif
          do j=jms,jme
          do i=ims,ime
            int_state%RLWTT(i,j,l)=0.
          enddo
          enddo
          call dstrb(temp1,int_state%RLWTT,1,1,1,lm,l,mype,mpi_comm_comp)
        enddo
!-----------------------------------------------------------------------
        do l=1,lm
          if(mype==0)then
            read(nfcst)temp1 ! rswtt
          endif
          do j=jms,jme
          do i=ims,ime
            int_state%RSWTT(i,j,l)=0.
          enddo
          enddo
          call dstrb(temp1,int_state%RSWTT,1,1,1,lm,l,mype,mpi_comm_comp)
        enddo
!-----------------------------------------------------------------------
        do l=1,lm+1
          if(mype==0)then
            read(nfcst)temp1
          endif
          do j=jms,jme
          do i=ims,ime
            int_state%pint(i,j,l)=0.
          enddo
          enddo
          call dstrb(temp1,int_state%pint,1,1,1,lm+1,l,mype,mpi_comm_comp)
        enddo
        call halo_exch(int_state%pint,lm+1,2,2)
!-----------------------------------------------------------------------
        do l=1,lm
          if(mype==0)then
            read(nfcst)temp1
          endif
          do j=jms,jme
          do i=ims,ime
            int_state%dwdt(i,j,l)=0.
          enddo
          enddo
          call dstrb(temp1,int_state%dwdt,1,1,1,lm,l,mype,mpi_comm_comp)
        enddo
        call halo_exch(int_state%dwdt,lm,2,2)
!-----------------------------------------------------------------------
        do l=1,lm
          if(mype==0)then
            read(nfcst)temp1
            write(0,*) 'L, T extremes: ', L, minval(temp1),maxval(temp1)
          endif
          do j=jms,jme
          do i=ims,ime
            int_state%t(i,j,l)=0.
          enddo
          enddo
          call dstrb(temp1,int_state%t,1,1,1,lm,l,mype,mpi_comm_comp)
        enddo
        call halo_exch(int_state%t,lm,2,2)
!-----------------------------------------------------------------------
        do l=1,lm
          if(mype==0)then
            read(nfcst)temp1 ! tcucn
          endif
          do j=jms,jme
          do i=ims,ime
            int_state%TCUCN(i,j,l)=0.
          enddo
          enddo
          call dstrb(temp1,int_state%TCUCN,1,1,1,lm,l,mype,mpi_comm_comp)
        enddo
!-----------------------------------------------------------------------
        do l=1,lm
          if(mype==0)then
            read(nfcst)temp1 ! train
          endif
          do j=jms,jme
          do i=ims,ime
            int_state%TRAIN(i,j,l)=0.
          enddo
          enddo
          call dstrb(temp1,int_state%TRAIN,1,1,1,lm,l,mype,mpi_comm_comp)
        enddo
!-----------------------------------------------------------------------
        do l=1,lm
          if(mype==0)then
            read(nfcst)temp1
          endif
          do j=jms,jme
          do i=ims,ime
            int_state%u(i,j,l)=0.
          enddo
          enddo
          call dstrb(temp1,int_state%U,1,1,1,lm,l,mype,mpi_comm_comp)
        enddo
        call halo_exch(int_state%u,lm,2,2)
!-----------------------------------------------------------------------
        do l=1,lm
          if(mype==0)then
            read(nfcst)temp1
          endif
          do j=jms,jme
          do i=ims,ime
            int_state%v(i,j,l)=0.
          enddo
          enddo
          call dstrb(temp1,int_state%V,1,1,1,lm,l,mype,mpi_comm_comp)
        enddo
        call halo_exch(int_state%v,lm,2,2)
!-----------------------------------------------------------------------
!
      DO K=1,LM
        IF(MYPE==0)THEN
          READ(NFCST)TEMP1   ! XLEN_MIX
        ENDIF
!
        DO J=JMS,JME
        DO I=IMS,IME
          int_state%XLEN_MIX(I,J,K)=0.
        ENDDO
        ENDDO
!
        CALL DSTRB(TEMP1,int_state%XLEN_MIX,1,1,1,LM,K,MYPE,MPI_COMM_COMP)
      ENDDO
!-----------------------------------------------------------------------
!
      DO K=1,LM
        IF(MYPE==0)THEN
          READ(NFCST)TEMP1   ! F_ICE
        ENDIF
!
        DO J=JMS,JME
        DO I=IMS,IME
          int_state%F_ICE(I,J,K)=0.
        ENDDO
        ENDDO
!
        CALL DSTRB(TEMP1,int_state%F_ICE,1,1,1,LM,K,MYPE,MPI_COMM_COMP)
      ENDDO
!-----------------------------------------------------------------------
!
      DO K=1,LM
        IF(MYPE==0)THEN
          READ(NFCST)TEMP1   ! F_RIMEF
        ENDIF
!
        DO J=JMS,JME
        DO I=IMS,IME
          int_state%F_RIMEF(I,J,K)=0.
        ENDDO
        ENDDO
!
        CALL DSTRB(TEMP1,int_state%F_RIMEF,1,1,1,LM,K,MYPE,MPI_COMM_COMP)
      ENDDO
!-----------------------------------------------------------------------
!
      DO K=1,LM
        IF(MYPE==0)THEN
          READ(NFCST)TEMP1   ! F_RAIN
        ENDIF
!
        DO J=JMS,JME
        DO I=IMS,IME
          int_state%F_RAIN(I,J,K)=0.
        ENDDO
        ENDDO
!
        CALL DSTRB(TEMP1,int_state%F_RAIN,1,1,1,LM,K,MYPE,MPI_COMM_COMP)
      ENDDO
!-----------------------------------------------------------------------
!***  SH2O, SMC, STC
!-----------------------------------------------------------------------
!
      DO K=1,int_state%NSOIL
!
        IF(MYPE==0)THEN
          READ(NFCST)TEMP1
!          write(0,*) 'lev, min, max for SH2O: ', k,minval(TEMP1),maxval(TEMP1)
        ENDIF
!
        CALL DSTRB(TEMP1,int_state%SH2O,1,1,1,int_state%NSOIL,K        &
                  ,MYPE,MPI_COMM_COMP)
!
      ENDDO
!
      DO K=1,int_state%NSOIL
!
        IF(MYPE==0)THEN
          READ(NFCST)TEMP1
!          write(0,*) 'lev, min, max for SMC: ', k,minval(TEMP1),maxval(TEMP1)
        ENDIF
!
        CALL DSTRB(TEMP1,int_state%SMC,1,1,1,int_state%NSOIL,K        &
                  ,MYPE,MPI_COMM_COMP)
!
      ENDDO
!
      DO K=1,int_state%NSOIL
!
        IF(MYPE==0)THEN
          READ(NFCST)TEMP1
!          write(0,*) 'lev, min, max for STC: ', k,minval(TEMP1),maxval(TEMP1)
        ENDIF
!
        CALL DSTRB(TEMP1,int_state%STC,1,1,1,int_state%NSOIL,K          &
                  ,MYPE,MPI_COMM_COMP)
!
      ENDDO
!
!-----------------------------------------------------------------------
        do n=1,int_state%indx_o3
          do l=1,lm
            if(mype==0)then
              read(nfcst)temp1
            endif
!d            do j=jms,jme
!d            do i=ims,ime
!d              int_state%TRACERS_PREV(i,j,l,n)=0.
!d            enddo
!d            enddo
            call dstrb(temp1,int_state%TRACERS_PREV(:,:,:,n),1,1,1,lm,l &
                      ,mype,mpi_comm_comp)
          enddo
        enddo
        call halo_exch(int_state%TRACERS_PREV,lm,int_state%indx_o3,1,2,2)
!

        do n=int_state%INDX_O3+1,int_state%NUM_TRACERS_TOTAL                     !<-- The first 'indx_o3' arrays are unallocated pointers
          do l=1,lm
            if(mype==0)then
              read(nfcst)temp1
            endif
            do j=jms,jme
            do i=ims,ime
              int_state%TRACERS(i,j,l,n)=0.
            enddo
            enddo
            call dstrb(temp1,int_state%TRACERS(:,:,:,n),1,1,1,lm,l      &
                      ,mype,mpi_comm_comp)
          enddo
        enddo
!
        call halo_exch(int_state%TRACERS,lm,int_state%NUM_TRACERS_TOTAL,1,2,2)
!
        do n=1,int_state%NUM_WATER
        do l=1,lm
          do j=jms,jme
          do i=ims,ime
            int_state%WATER(i,j,l,n)=int_state%TRACERS(i,j,l,n+int_state%NUM_TRACERS_TOTAL-int_state%NUM_WATER)
          enddo
          enddo
        enddo
        enddo
!
!-----------------------------------------------------------------------
!
        close(nfcst)
!
!-----------------------------------------------------------------------
        int_state%NTSTI=ntsd+1
!
        tend_max=real(int_state%IHREND)
        ntstm_max=nint(tend_max*3600./int_state%DT)+1
        tend=real(int_state%nhours_fcst)
        int_state%NTSTM=nint(tend*3600./int_state%DT)+1
        if(.not.int_state%GLOBAL)then
          if(mype==0)then
            write(0,*)' Max runtime is ',tend_max,' hours'
          endif
        endif
        if(mype==0)then
          write(0,*)' Requested runtime is ',tend,' hours'
          write(0,*)' NTSTM=',int_state%NTSTM
        endif
        if(int_state%NTSTM>ntstm_max.and..not.int_state%GLOBAL)then
          if(mype==0)then
            write(0,*)' Requested fcst length exceeds maximum'
            write(0,*)' Resetting to maximum'
          endif
          int_state%NTSTM=min(int_state%NTSTM,ntstm_max)
        endif
!
        int_state%IHR=nint(ntsd*int_state%DT/3600.)
!-----------------------------------------------------------------------
        do l=1,lm
          int_state%PDSG1(l)=int_state%DSG1(l)*int_state%PDTOP
          int_state%PSGML1(l)=int_state%SGML1(l)*int_state%PDTOP+int_state%PT
        enddo
!
        do l=1,lm+1
          int_state%PSG1(l)=int_state%SG1(l)*int_state%PDTOP+int_state%PT
        enddo
!-----------------------------------------------------------------------
      endif  read_blocks                        ! cold start /restart
!-----------------------------------------------------------------------
!
      deallocate(temp1)
!
!-----------------------------------------------------------------------
!
      end subroutine read_binary
!
!-----------------------------------------------------------------------
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!-----------------------------------------------------------------------
!
      SUBROUTINE PHYSICS_READ_GWD(INFILE,NGWD,INT_STATE                &
                                 ,MYPE,MPI_COMM_COMP                   &
                                 ,IDS,IDE,JDS,JDE,RC)
!----------------------------------------------------------------------
!
!------------------------
!***  Argument variables
!------------------------
!
      INTEGER,INTENT(IN) :: NGWD,MYPE,MPI_COMM_COMP
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE
!
      CHARACTER(LEN=*),INTENT(IN) :: INFILE
!
      TYPE(SOLVER_INTERNAL_STATE),POINTER,INTENT(INOUT) :: INT_STATE     !<-- The physics internal state
!
      INTEGER,INTENT(OUT) :: RC
!
!---------------------
!***  Local variables
!---------------------
!
      INTEGER :: IERR
!
      REAL,DIMENSION(:,:),ALLOCATABLE :: TEMP_GWD
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
      RC = 0
!
      ALLOCATE(TEMP_GWD(IDS:IDE,JDS:JDE))      
!
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        OPEN(unit=NGWD,file=INFILE,status='old',form='unformatted'      &
            ,iostat=IERR)
        IF(IERR/=0)THEN
          WRITE(0,*)' Unable to open file ',TRIM(INFILE)                &
                   ,' in PHYSICS_READ_GWD'
          RC = IERR
          RETURN
        ENDIF
      ENDIF
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NGWD)TEMP_GWD
      ENDIF
!
      CALL DSTRB(TEMP_GWD,int_state%HSTDV,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NGWD)TEMP_GWD
      ENDIF
!
      CALL DSTRB(TEMP_GWD,int_state%HCNVX,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NGWD)TEMP_GWD
      ENDIF
!
      CALL DSTRB(TEMP_GWD,int_state%HASYW,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NGWD)TEMP_GWD
      ENDIF
!
      CALL DSTRB(TEMP_GWD,int_state%HASYS,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NGWD)TEMP_GWD
      ENDIF
!
      CALL DSTRB(TEMP_GWD,int_state%HASYSW,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NGWD)TEMP_GWD
      ENDIF
!
      CALL DSTRB(TEMP_GWD,int_state%HASYNW,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NGWD)TEMP_GWD
      ENDIF
!
      CALL DSTRB(TEMP_GWD,int_state%HLENW,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NGWD)TEMP_GWD
      ENDIF
!
      CALL DSTRB(TEMP_GWD,int_state%HLENS,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NGWD)TEMP_GWD
      ENDIF
!
      CALL DSTRB(TEMP_GWD,int_state%HLENSW,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NGWD)TEMP_GWD
      ENDIF
!
      CALL DSTRB(TEMP_GWD,int_state%HLENNW,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NGWD)TEMP_GWD
      ENDIF
!
      CALL DSTRB(TEMP_GWD,int_state%HANGL,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NGWD)TEMP_GWD
      ENDIF
!
      CALL DSTRB(TEMP_GWD,int_state%HANIS,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NGWD)TEMP_GWD
      ENDIF
!
      CALL DSTRB(TEMP_GWD,int_state%HSLOP,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
      IF(MYPE==0)THEN
        READ(NGWD)TEMP_GWD
      ENDIF
!
      CALL DSTRB(TEMP_GWD,int_state%HZMAX,1,1,1,1,1,MYPE,MPI_COMM_COMP)
!-----------------------------------------------------------------------
!
      IF(MYPE==0)THEN
        CLOSE(NGWD)
      ENDIF
!
      DEALLOCATE(TEMP_GWD)
!-----------------------------------------------------------------------
!
      END SUBROUTINE PHYSICS_READ_GWD 
!
!-----------------------------------------------------------------------
!#######################################################################
!-----------------------------------------------------------------------
!
      end module module_INIT_READ_BIN
!
!-----------------------------------------------------------------------
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!-----------------------------------------------------------------------

