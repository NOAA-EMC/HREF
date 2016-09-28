MODULE output_nemsinit

  USE parallel_module
  USE module_data
  USE nemsio_module

CONTAINS

  SUBROUTINE write_nemsinit(grid, gridin, ndom, do_gwd, max_dom, lnsh)

       IMPLICIT NONE

#ifdef _MPI
       INCLUDE "mpif.h"
#else
       INTEGER mpi_comm_world
#endif

       TYPE(output_vars):: grid
       TYPE(input_vars):: gridin
       TYPE(nemsio_gfile) :: iunit_nemsio

       REAL, PARAMETER:: DEGRAD=1./57.2957795
       REAL, PARAMETER:: a=6376000.   ! radius of earth; matches value used in model

       INTEGER, INTENT (IN) :: ndom
       INTEGER, INTENT (IN) :: max_dom
       INTEGER:: L,IUNIT,IDS,IDE,KDS,KDE,JDS,JDE,IUNIT2, II, nx,n_children,iunit3, iunit4
       INTEGER:: ITS,ITE,JTS,JTE,ITARG,JTARG,kts,kte, MPI_COMM_COMP, NPES,MYPROC
       INTEGER:: IMS,IME,JMS,JME,KMS,KME
       INTEGER:: IM,JM,LM,I,J,NSOIL,IDAY,IMONTH,IYEAR,N, idx_glat, idx_glon
       INTEGER:: IDS_str, IDE_end, IEND, JEND, IERR, MYPE
       INTEGER:: JDS_str, JDE_end, lnsh
       CHARACTER(LEN=19) :: cdate
       CHARACTER(LEN=2) :: nchar
       CHARACTER(LEN=11) :: flname
       CHARACTER(LEN=4) :: nchar4
       CHARACTER(LEN=9) :: nchar8,nchar8_2

       REAL             :: wbd, sbd, SB, TPH, DLM, DPH
       REAL             :: swlat, swlon, nelat, nelon
       REAL, ALLOCATABLE:: sgm(:), DX(:), DY(:)
       REAL, ALLOCATABLE:: sg1(:), dsg1(:), sgml1(:)
       REAL, ALLOCATABLE:: sg2(:), dsg2(:), sgml2(:)
       REAL, ALLOCATABLE:: TEMP1(:,:),TEMPSOIL(:,:,:),TEMP2(:,:),TEMPSOIL2(:,:,:)
       REAL, ALLOCATABLE:: PSFC(:,:), lat_hold(:),lon_hold(:),TEMP1D(:)

       INTEGER, ALLOCATABLE :: ITEMP(:,:),ITEMP2(:,:)

       INTEGER :: idat(3),ihrst,ihrend,ntsd,ncount,iret
       INTEGER :: nemsio_count,nc
       INTEGER :: idate(7)
       real, SAVE ::  tmp_dlmd(21), tmp_dphd(21)

       CHARACTER(LEN=100) :: esmf_input, esmf_input_nemsio, nest_config, dom_config
       CHARACTER(len=60):: line
       CHARACTER(LEN=8) :: fname

       LOGICAL :: opened,run, u_var, v_var, DOFILL, e_w_average, GLOBAL
       LOGICAL :: do_gwd, print_diag, polavg

       INTEGER :: nrec,nmetavari,nmetavarr,nmetavarl,nmetaaryi,nmetaaryr
       INTEGER :: nframe
       INTEGER :: ls, jrec
       CHARACTER(8),ALLOCATABLE :: recname(:),metaaryrname(:)
       CHARACTER(8) :: gwdname
       CHARACTER(16),ALLOCATABLE :: reclevtyp(:)
       INTEGER,ALLOCATABLE :: reclev(:)
       CHARACTER(8),ALLOCATABLE :: variname(:),varrname(:),varlname(:),aryiname(:),aryrname(:)
       INTEGER,ALLOCATABLE :: varival(:),aryilen(:),aryrlen(:),aryival(:,:)
       REAL,ALLOCATABLE :: varrval(:),aryrval(:,:)
       LOGICAL,ALLOCATABLE :: varlval(:)
!
!-------------------------------------------------------------------------------
!
	if (ndom .eq. 1) then
       GLOBAL=grid%global
        else
       GLOBAL=.false.
        endif

       IEND=gridin%IDE-1 !?
       JEND=gridin%JDE-1 !?

       IMS =gridin%IMS
       IME =gridin%IME
       JMS =gridin%JMS
       JME =gridin%JME

       IDS =gridin%IDS
       IDE =gridin%IDE
       JDS =gridin%JDS
       JDE =gridin%JDE

       ITS =gridin%ITS
       ITE =gridin%ITE
       JTS =gridin%JTS
       JTE =gridin%JTE

       MPI_COMM_COMP=comm
       NPES=nprocs
       MYPROC=my_proc_id

       if (MYPROC .eq. 0) then
         print_diag=.true.
       else
         print_diag=.false.
       endif

#ifdef _MPI
       CALL MPI_BARRIER(MPI_COMM_COMP, IERR)
#else
       mpi_comm_world=1
#endif

       nsoil = gridin%numsoil
       LM=gridin%KDE-1
!prevlat      nrec = 33 + 6*lm + lm+1 + 3*nsoil
!prenewalb       nrec = 35 + 6*lm + lm+1 + 3*nsoil

       nrec = 37 + 6*lm + lm+1 + 3*nsoil
       IF (do_gwd) nrec=nrec+14
        write(0,*) 'nrec is: ', nrec

       if ( MYPROC .eq. 0 ) then
          allocate(recname(nrec),reclevtyp(nrec),reclev(nrec))
       nc=0

        nc=nc+1
         recname(nc)='fis';reclevtyp(nc)='sfc';reclev(nc)=1
        nc=nc+1
         recname(nc)='hgt';reclevtyp(nc)='sfc';reclev(nc)=1
        nc=nc+1
         recname(nc)='stdh';reclevtyp(nc)='sfc';reclev(nc)=1
        nc=nc+1
         recname(nc)='sm';reclevtyp(nc)='sfc';reclev(nc)=1
        nc=nc+1
         recname(nc)='dpres';reclevtyp(nc)='hybrid sig lev';reclev(nc)=1
        nc=nc+1

         do L=1,LM
         recname(nc)='ugrd';reclevtyp(nc)='mid layer';reclev(nc)=L
         nc=nc+1
         ENDDO

         recname(nc)='u10';reclevtyp(nc)='10 m above gnd';reclev(nc)=1
         nc=nc+1

         do L=1,LM
         recname(nc)='vgrd';reclevtyp(nc)='mid layer';reclev(nc)=L
         nc=nc+1
         ENDDO

         recname(nc)='v10';reclevtyp(nc)='10 m above gnd';reclev(nc)=1
         nc=nc+1

         do L=1,LM
         recname(nc)='tmp';reclevtyp(nc)='mid layer';reclev(nc)=L
         nc=nc+1
         ENDDO

         do L=1,LM
         recname(nc)='spfh';reclevtyp(nc)='mid layer';reclev(nc)=L
         nc=nc+1
         ENDDO

         do L=1,LM
         recname(nc)='clwmr';reclevtyp(nc)='mid layer';reclev(nc)=L
         nc=nc+1
         ENDDO

         do L=1,LM
         recname(nc)='o3mr';reclevtyp(nc)='mid layer';reclev(nc)=L
         nc=nc+1
         ENDDO

         do L=1,LM+1
         recname(nc)='pres';reclevtyp(nc)='layer';reclev(nc)=L
         nc=nc+1
         ENDDO

         recname(nc)='albedo';reclevtyp(nc)='sfc';reclev(nc)=1
        nc=nc+1
         recname(nc)='albase';reclevtyp(nc)='sfc';reclev(nc)=1
        nc=nc+1
         recname(nc)='epsr';reclevtyp(nc)='sfc';reclev(nc)=1
        nc=nc+1
         recname(nc)='mxsnal';reclevtyp(nc)='sfc';reclev(nc)=1
        nc=nc+1
         recname(nc)='tskin';reclevtyp(nc)='sfc';reclev(nc)=1
        nc=nc+1
         recname(nc)='ths';reclevtyp(nc)='sfc';reclev(nc)=1
        nc=nc+1
         recname(nc)='tsea';reclevtyp(nc)='sfc';reclev(nc)=1
        nc=nc+1
         recname(nc)='sno';reclevtyp(nc)='sfc';reclev(nc)=1
        nc=nc+1
         recname(nc)='si';reclevtyp(nc)='sfc';reclev(nc)=1
        nc=nc+1
         recname(nc)='sice';reclevtyp(nc)='sfc';reclev(nc)=1
        nc=nc+1
         recname(nc)='tg';reclevtyp(nc)='sfc';reclev(nc)=1
        nc=nc+1
         recname(nc)='cmc';reclevtyp(nc)='sfc';reclev(nc)=1
        nc=nc+1
         recname(nc)='sr';reclevtyp(nc)='sfc';reclev(nc)=1
        nc=nc+1
         recname(nc)='ustar';reclevtyp(nc)='sfc';reclev(nc)=1
        nc=nc+1
         recname(nc)='zorl';reclevtyp(nc)='sfc';reclev(nc)=1
        nc=nc+1
         recname(nc)='z0base';reclevtyp(nc)='sfc';reclev(nc)=1
        nc=nc+1
         recname(nc)='glat';reclevtyp(nc)='sfc';reclev(nc)=1
        nc=nc+1
         recname(nc)='glon';reclevtyp(nc)='sfc';reclev(nc)=1
        nc=nc+1
         recname(nc)='vlat';reclevtyp(nc)='sfc';reclev(nc)=1
        nc=nc+1
         recname(nc)='vlon';reclevtyp(nc)='sfc';reclev(nc)=1
        nc=nc+1

         do L=1,NSOIL
         recname(nc)='stc';reclevtyp(nc)='soil layer';reclev(nc)=L
         nc=nc+1
         enddo

         do L=1,NSOIL
         recname(nc)='smc';reclevtyp(nc)='soil layer';reclev(nc)=L
         nc=nc+1
         enddo

         do L=1,NSOIL
         recname(nc)='sh2o';reclevtyp(nc)='soil layer';reclev(nc)=L
         nc=nc+1
         enddo

         recname(nc)='sltyp';reclevtyp(nc)='sfc';reclev(nc)=1
        nc=nc+1
         recname(nc)='vgtyp';reclevtyp(nc)='sfc';reclev(nc)=1
        nc=nc+1
         recname(nc)='vegfrc';reclevtyp(nc)='sfc';reclev(nc)=1
        nc=nc+1
         recname(nc)='dirvssfa';reclevtyp(nc)='sfc';reclev(nc)=1
        nc=nc+1
         recname(nc)='difvssfa';reclevtyp(nc)='sfc';reclev(nc)=1
        nc=nc+1
         recname(nc)='dirnisfa';reclevtyp(nc)='sfc';reclev(nc)=1
        nc=nc+1
         recname(nc)='difnisfa';reclevtyp(nc)='sfc';reclev(nc)=1
        nc=nc+1
         recname(nc)='dirbbsfa';reclevtyp(nc)='sfc';reclev(nc)=1
        nc=nc+1
         recname(nc)='difbbsfa';reclevtyp(nc)='sfc';reclev(nc)=1
        nc=nc+1
         recname(nc)='mxsnalnw';reclevtyp(nc)='sfc';reclev(nc)=1
        nc=nc+1

       IF (do_gwd) THEN
       do L=1,14

	if (L .eq. 1) gwdname='HSTDV'
	if (L .eq. 2) gwdname='HCNVX'
!
	if (L .eq. 3) gwdname='HASYW'
	if (L .eq. 4) gwdname='HASYS'
	if (L .eq. 5) gwdname='HASYSW'
	if (L .eq. 6) gwdname='HASYNW'
!
	if (L .eq. 7) gwdname='HLENW'
	if (L .eq. 8) gwdname='HLENS'
	if (L .eq. 9) gwdname='HLENSW'
	if (L .eq. 10) gwdname='HLENNW'
!
	if (L .eq. 11) gwdname='HANGL'
	if (L .eq. 12) gwdname='HANIS'
	if (L .eq. 13) gwdname='HSLOP'
	if (L .eq. 14) gwdname='HZMAX'

         recname(nc)=gwdname;reclevtyp(nc)='sfc';reclev(nc)=1
        nc=nc+1

       enddo
       ENDIF

        do L=1,nrec
        write(0,*) 'nc, recname, reclevtyp, reclev: ',L, recname(L), &
                   reclevtyp(L),reclev(L)
        enddo

       end if

       if (print_diag) write(0,*) 'ARE WE GLOBAL?'
       IF (GLOBAL) THEN

         if (print_diag) write(0,*) 'Adding extra rows and columns to globalize it'

!         ITS=2
!         ITE=IDE
!         JTS=2
!         JTE=JDE

         ITARG=0
         JTARG=0

!!! not sure about the local array dimensions here (June 7, 2007)

         if (print_diag) then
         write(0,*) 'TEMP1 I-dim limits: ', ITS, min(ITE,IDE)
         write(0,*) 'TEMP1 J-dim limits: ', JTS, min(JTE,JDE)
         write(0,*) 'TEMP2 I-dim limits: ', IDS-1, IDE
         write(0,*) 'TEMP2 J-dim limits: ', JDS-1, JDE
	endif

         IF (.NOT. ALLOCATED(TEMP1)) ALLOCATE(TEMP1(ITS:min(ITE,IDE),JTS:min(JTE,JDE)))
         IF (.NOT. ALLOCATED(TEMP2)) ALLOCATE(TEMP2(IDS-1:IDE,JDS-1:JDE))
         IF (.NOT. ALLOCATED(lat_hold)) ALLOCATE(lat_hold((IDE-IDS+2)*(JDE-JDS+2)))
         IF (.NOT. ALLOCATED(lon_hold)) ALLOCATE(lon_hold((IDE-IDS+2)*(JDE-JDS+2)))
         IF (.NOT. ALLOCATED(TEMP1D)) ALLOCATE(TEMP1D((IDE-IDS+2)*(JDE-JDS+2)))

         IF (.NOT. ALLOCATED(ITEMP)) ALLOCATE(ITEMP(ITS:min(ITE,IDE),JTS:min(JTE,JDE)))
         IF (.NOT. ALLOCATED(ITEMP2)) ALLOCATE(ITEMP2(IDS-1:IDE,JDS-1:JDE))

         IF (.NOT. ALLOCATED(TEMPSOIL)) ALLOCATE(TEMPSOIL(NSOIL,ITS:min(ITE,IDE),JTS:min(JTE,JDE)))
         IF (.NOT. ALLOCATED(TEMPSOIL2)) ALLOCATE(TEMPSOIL2(NSOIL,IDS-1:IDE,JDS-1:JDE))

         esmf_input='input_domain_01'
         esmf_input_nemsio='input_domain_01_nemsio'
         dom_config='domain_details'//'_'//nchar
         DOFILL=.true.
         JDS_str=JDS-1
         IDS_str=IDS-1
         JDE_end=JDE
         IDE_end=IDE

         nframe=0 ! taken care of by physical dimensions??
         write (nchar,633) ndom

       ELSE

         if (print_diag) write(0,*) 'not global, write out true dimensions'

         ITARG=0
         JTARG=0

         IF (.NOT. ALLOCATED(TEMP1)) ALLOCATE(TEMP1(ITS:min(ITE,IDE-1),JTS:min(JTE,JDE-1)))
         IF (.NOT. ALLOCATED(TEMP2)) ALLOCATE(TEMP2(IDS:IDE-1,JDS:JDE-1))
         IF (.NOT. ALLOCATED(lat_hold)) ALLOCATE(lat_hold((IDE-IDS)*(JDE-JDS)))
         IF (.NOT. ALLOCATED(lon_hold)) ALLOCATE(lon_hold((IDE-IDS)*(JDE-JDS)))
         IF (.NOT. ALLOCATED(TEMP1D)) ALLOCATE(TEMP1D((IDE-IDS)*(JDE-JDS)))
         IF (.NOT. ALLOCATED(ITEMP)) ALLOCATE(ITEMP(ITS:min(ITE,IDE-1),JTS:min(JTE,JDE-1)))
         IF (.NOT. ALLOCATED(ITEMP2)) ALLOCATE(ITEMP2(IDS:IDE-1,JDS:JDE-1))
         IF (.NOT. ALLOCATED(TEMPSOIL)) ALLOCATE(TEMPSOIL(NSOIL,ITS:min(ITE,IDE-1),JTS:min(JTE,JDE-1)))
         IF (.NOT. ALLOCATED(TEMPSOIL2)) ALLOCATE(TEMPSOIL2(NSOIL,IDS:IDE-1,JDS:JDE-1))

         if (print_diag) then
         write(0,*) 'IDS, IDE, JDS, JDE: ', IDS, IDE, JDS, JDE
         write(0,*) 'TEMP1 size: ', size(temp1,dim=1),size(temp1,dim=2)
         write(0,*) 'TEMP2 size: ', size(temp2,dim=1),size(temp2,dim=2)
         endif

         write (nchar,633) ndom
 633     format(I2.2)

         esmf_input='input_domain'//'_'//nchar
         esmf_input_nemsio='input_domain'//'_'//nchar//'_nemsio'
         nest_config='configure_nest_details'//'_'//nchar
         dom_config='domain_details'//'_'//nchar
         DOFILL=.false.
         JDS_str=JDS
         IDS_str=IDS
         JDE_end=JDE-1
         IDE_end=IDE-1

         nframe=0

       ENDIF

       run=.true.
       ntsd=0

       cdate(1:19)=gridin%current_date(1:19)

       READ(cdate,FMT='(    I4)') iyear
       READ(cdate,FMT='( 5X,I2)') imonth
       READ(cdate,FMT='( 8X,I2)') iday
       READ(cdate,FMT='(11X,I2)') ihrst

       write(0,*) 'iyear, imonth, iday: ', iyear, imonth, iday
       write(0,*) 'IHRST: ', IHRST

       ihrend=gridin%fcstlength

       if (print_diag) write(0,*) 'IHREND: ', IHREND

       IDAT(1)=iday
       IDAT(2)=imonth
       IDAT(3)=iyear

       ALLOCATE(sgm(LM+1))
       ALLOCATE(sg1(LM+1))
       ALLOCATE(dsg1(LM))
       ALLOCATE(sgml1(LM))
       ALLOCATE(sg2(LM+1))
       ALLOCATE(dsg2(LM))
       ALLOCATE(sgml2(LM))

       if ( MYPROC .eq. 0 ) then
        open_unit: do l=51,99
          inquire(l,opened=opened)

          if(.not.opened)then
            iunit=l
            open(unit=iunit,file=esmf_input,form='unformatted')
            write(0,*) 'opening with iunit: ', iunit
            exit open_unit
          endif

        end do open_unit
       end if


       NCOUNT=0
       nc=0
       if ( MYPROC .eq. 0 ) then
         write(iunit) run,idat,ihrst,ihrend,ntsd
	 write(0,*) 'run,idat,ihrst,ihrend,ntsd: ', run,idat,ihrst,ihrend,ntsd
         NCOUNT=NCOUNT+1
       endif

       if (print_diag) then
       write(0,*) 'to nl_etalevs usage, LM+1', LM+1
       write(0,*) 'size(grid%nl_etalevs): ', size(grid%nl_etalevs)
       write(0,*) 'size(grid%eta1), size(grid%eta2): ', size(grid%eta1), size(grid%eta2)
       endif

       do L=1,LM+1
         sgm(l)=grid%nl_etalevs(L)
         sg1(L)=grid%eta1(L)
         sg2(L)=grid%eta2(L)
       enddo

       do L=1,LM
         dsg1(L)=grid%deta1(L)
         sgml1(L)=grid%aeta1(L)
         dsg2(L)=grid%deta2(L)
         sgml2(L)=grid%aeta2(L)
       enddo



!!! assemble lat/lon arrays for nemsio metadata

       do J=JTS,min(JTE,JDE_end)
       do I=ITS,min(ITE,IDE_end)
         TEMP1(I,J)=gridin%xlat_m(I+ITARG,J+JTARG)*DEGRAD
       end do
       end do

       call make_global(TEMP1,TEMP2,MYPROC,npes,mpi_comm_world                &
                       ,IDS_str,IDE_end,JDS_str,JDE_end,1,1             &
                       ,IMS,IME,JMS,JME,KMS,KME                           &
                       ,ITS,min(ITE,IDE_end),JTS,min(JTE,JDE_end),KTS,KTE)

       if ( MYPROC .eq. 0 ) then
         call make_nemsio_array(TEMP2,lat_hold, IDS_str, IDE_end, JDS_str, JDE_end)
       endif

       do J=JTS,min(JTE,JDE_end)
       do I=ITS,min(ITE,IDE_end)
         TEMP1(I,J)=gridin%xlong_m(I+ITARG,J+JTARG)*DEGRAD
       end do
       end do

       call make_global(TEMP1,TEMP2,MYPROC,npes,mpi_comm_world                &
                       ,IDS_str,IDE_end,JDS_str,JDE_end,1,1             &
                       ,IMS,IME,JMS,JME,KMS,KME                           &
                       ,ITS,min(ITE,IDE_end),JTS,min(JTE,JDE_end),KTS,KTE)

       if ( MYPROC .eq. 0 ) then
         call make_nemsio_array(TEMP2,lon_hold, IDS_str, IDE_end, JDS_str, JDE_end)
       endif

       if (MYPROC .eq. 0) then
         tmp_dlmd(1)=grid%dlmd
         tmp_dphd(1)=grid%dphd
         if (ndom .gt. 1) then
           tmp_dlmd(ndom)=tmp_dlmd(grid%parent_id_out(ndom))/grid%parent_grid_ratio_out(ndom)
           tmp_dphd(ndom)=tmp_dphd(grid%parent_id_out(ndom))/grid%parent_grid_ratio_out(ndom)
      	   write(0,*) 'tmp_dlmd(1:ndom): ', tmp_dlmd(1:ndom)
          endif

        call tll(gridin%xlong_m(1+ITARG,1+JTARG),gridin%xlat_m(1+ITARG,1+JTARG),wbd,sbd,grid%tph0d,grid%tlm0d)

	allocate(DX((IDE_end-IDS_str+1)*(JDE_end-JDS_str+1)))
	allocate(DY((IDE_end-IDS_str+1)*(JDE_end-JDS_str+1)))

          SB=sbd*DEGRAD
	  DLM=tmp_dlmd(ndom)*DEGRAD
	  DPH=tmp_dphd(ndom)*DEGRAD

          nx=1
          DO J=JDS_str,JDE_end
          DO I=IDS_str,IDE_end
            TPH=SB+(J-JDS_STR)*DPH
            DX(nx)=A*DLM*COS(TPH)
            nx=nx+1
          ENDDO
          ENDDO

          nx=1
          DO J=JDS_str,JDE_end
          DO I=IDS_str,IDE_end
            DY(nx)=A*DPH
            nx=nx+1
        if (nx .eq. 1000) then
        write(0,*) 'DX,DY(999): ', dx(999),dy(999)
        endif

          ENDDO
          ENDDO
        endif

!! end assemble lat/lon arrays

       if ( MYPROC .eq. 0 ) then

         write(0,*) 'grid%eta1 dimension: ', size(grid%eta1,dim=1)
         write(0,*) 'grid%deta1 dimension: ', size(grid%deta1,dim=1)
         write(0,*) 'grid%aeta1 dimension: ', size(grid%aeta1,dim=1)

         write(0,*) 'pt: ', grid%pt
         write(0,*) 'pdtop: ', grid%pdtop
         write(0,*) 'LPT2: ', grid%LPT2
         write(0,*) 'sgm: ', size(sgm), sgm
         write(0,*) 'sg1: ', size(sg1),sg1
         write(0,*) 'dsg1: ', size(dsg1),dsg1
         write(0,*) 'sgml1: ', size(sgml1),sgml1
         write(0,*) 'sg2: ', size(sg2),sg2
         write(0,*) 'dsg2: ', size(dsg2),dsg2
         write(0,*) 'sgml2: ', size(sgml2),sgml2

         write(iunit) grid%pt,grid%pdtop,grid%LPT2,sgm,sg1,dsg1,sgml1,sg2,dsg2,sgml2
         NCOUNT=NCOUNT+1
         write(iunit) grid%i_parent_start_loc, grid%j_parent_start_loc
	write(0,*) 'grid%i_parent_start_loc, grid%j_parent_start_loc: ', grid%i_parent_start_loc, grid%j_parent_start_loc
         NCOUNT=NCOUNT+1

         tmp_dlmd(1)=grid%dlmd
         tmp_dphd(1)=grid%dphd
         if (ndom .gt. 1) then
           tmp_dlmd(ndom)=tmp_dlmd(grid%parent_id_out(ndom))/grid%parent_grid_ratio_out(ndom)
           tmp_dphd(ndom)=tmp_dphd(grid%parent_id_out(ndom))/grid%parent_grid_ratio_out(ndom)
      	   write(0,*) 'tmp_dlmd(1:ndom): ', tmp_dlmd(1:ndom)
          endif

	print*, ' ndom, xlong, xlat into tll: ', ndom, gridin%xlong_m(1+ITARG,1+JTARG),gridin%xlat_m(1+ITARG,1+JTARG)
        call tll(gridin%xlong_m(1+ITARG,1+JTARG),gridin%xlat_m(1+ITARG,1+JTARG),wbd,sbd,grid%tph0d,grid%tlm0d)
	print*, 'ndom, wbd, sbd: ', ndom, wbd, sbd

	write(iunit) tmp_dlmd(ndom), tmp_dphd(ndom), wbd, sbd, grid%tlm0d, grid%tph0d
	write(0,*) 'tmp_dlmd(ndom), tmp_dphd(ndom), wbd, sbd, grid%tlm0d, grid%tph0d: ', tmp_dlmd(ndom), tmp_dphd(ndom), wbd, sbd, grid%tlm0d, grid%tph0d
        NCOUNT=NCOUNT+1
        write(iunit) IDE-1, JDE-1, LM, lnsh
	write(0,*) 'IDE-1, JDE-1, LM, lnsh: ', IDE-1, JDE-1, LM, lnsh
        NCOUNT=NCOUNT+1

!!! nemsio open

!  set up metadata for nemsio file

         idate(1:6)=0;idate(7)=100.
         idate(1)=idat(3)
         idate(2)=idat(2)
         idate(3)=idat(1)
         idate(4)=ihrst

!--
         nmetavari=6
         allocate(variname(nmetavari),varival(nmetavari))

         variname(1:nmetavari)=(/'ihrst   ','ihrend  ','ntsd    ','lpt2    ','iparstrt','jparstrt'/)
         varival(1:nmetavari)=(/ihrst,ihrend,ntsd,grid%LPT2,grid%i_parent_start_loc,grid%j_parent_start_loc/)

	
	write(0,*) 'writing ndom, dlmd, dphd, wbd, sbd: ', ndom, tmp_dlmd(ndom),tmp_dphd(ndom), wbd, sbd
	write(0,*) 'writing tph0d, tlm0d: ', grid%tph0d,grid%tlm0d

         nmetavarr=10
         allocate(varrname(nmetavarr),varrval(nmetavarr))
         varrname(1:nmetavarr)=(/'pdtop   ','pt      ','dlmd    ','dphd    ','wbd     ',&
                                 'sbd     ','tph0d   ','tlm0d   ','cen_lat ','cen_lon '/)
         varrval(1:nmetavarr)=(/grid%pdtop,grid%pt,tmp_dlmd(ndom),tmp_dphd(ndom),wbd,sbd,grid%tph0d, &
                              grid%tlm0d,grid%dom_cen_lat(ndom),grid%dom_cen_lon(ndom)/)


         nmetavarl=2
         allocate(varlname(nmetavarl),varlval(nmetavarl))
         varlname(1:2)=(/'run   ','global'/)
         varlval(1:2)=(/run,global/)
!--
         nmetaaryi=1
         allocate(aryiname(nmetaaryi),aryilen(nmetaaryi),aryival(3,nmetaaryi))
         aryiname(1:1)=(/'idat'/)
         aryilen(1:1)=3
         aryival(1:3,1)=idat(1:3)

         nmetaaryr=9
         allocate(aryrname(nmetaaryr),aryrlen(nmetaaryr),aryrval(lm+1,nmetaaryr))
         aryrname(1:nmetaaryr)=(/'sgm    ','sg1    ','sg2    ','dsg1   ','dsg2   ', &
                                 'sgml1  ','sgml2  ','dzsoil ','sldpth '/)
         aryrlen(1:9)=(/lm+1,lm+1,lm+1,lm,lm,lm,lm,nsoil,nsoil/)
         aryrval(1:lm+1,1)=sgm(1:lm+1)
         aryrval(1:lm+1,2)=sg1(1:lm+1)
         aryrval(1:lm+1,3)=sg2(1:lm+1)
         aryrval(1:lm,4)=dsg1(1:lm)
         aryrval(1:lm,5)=dsg2(1:lm)
         aryrval(1:lm,6)=sgml1(1:lm)
         aryrval(1:lm,7)=sgml2(1:lm)
         aryrval(1:nsoil,8)=grid%dzsoil(1:nsoil)
         aryrval(1:nsoil,9)=grid%sldpth(1:nsoil)
!
         call nemsio_init(iret=iret)
         if (iret/=0) then
           write(0,*)' nemsio_init error ', iret
#ifdef _MPI
           CALL mpi_abort(MPI_COMM_WORLD,1,ierr)
#else
           STOP
#endif
         endif

         call nemsio_open(iunit_nemsio,trim(esmf_input_nemsio),                            &
                          gaction='WRITE',iret=iret,                                       &
                          modelname="NMMB",gdatatype="bin4",idate=IDATE,                   &
                          dimx=IDE_end-IDS_str+1,dimy=JDE_end-JDS_str+1,                   &    
                          dimz=LM,nframe=NFRAME,                                           &
                          nsoil=NSOIL,nrec=nrec,                                           &
                          lat=lat_hold, lon=lon_hold,        &
                          dx=dx,dy=dy,                                                     &
                          extrameta=.true.,                                                &
                          nmetavari=nmetavari,                                             &
                          nmetavarr=nmetavarr,                                             &
                          nmetavarl=nmetavarl,                                             &
                          nmetaaryi=nmetaaryi,                                             &
                          nmetaaryr=nmetaaryr,                                             &
                          variname=VARINAME,varival=VARIVAL,                               &
                          varrname=VARRNAME,varrval=VARRVAL,                               &
                          varlname=VARLNAME,varlval=VARLVAL,                               &
                          aryiname=ARYINAME,aryilen=ARYILEN,aryival=ARYIVAL,               &
                          aryrname=ARYRNAME,aryrlen=ARYRLEN,aryrval=ARYRVAL,               &
                          recname=RECNAME,reclevtyp=RECLEVTYP,reclev=RECLEV)
         if (iret/=0) then
           write(0,*)' nemsio_open error ', iret
#ifdef _MPI
           CALL mpi_abort(MPI_COMM_WORLD,1,ierr)
#else
           STOP
#endif

         endif



       endif

       if (print_diag) then
       write(0,*) 'filled TEMP1 for FIS over J vals: ', JTS,min(JTE,JDE)
       write(0,*) 'filled TEMP1 for FIS over I vals: ', ITS,min(ITE,IDE)
       endif

!--- FIS ----------------------------------------------------------------

       do J=JTS,min(JTE,JDE_end)
       do I=ITS,min(ITE,IDE_end)
         TEMP1(I,J)=grid%fis(I+ITARG,J+JTARG)
       end do
       end do

       u_var=.false.
       v_var=.false.
       e_w_average=.true.
       polavg=.true.

       call make_global(TEMP1,TEMP2,MYPROC,npes,mpi_comm_world                &
                       ,IDS_str,IDE_end,JDS_str,JDE_end,1,1             &
                       ,IMS,IME,JMS,JME,KMS,KME                           &
                       ,ITS,min(ITE,IDE_end),JTS,min(JTE,JDE_end),KTS,KTE)

       if (DOFILL .and. MYPROC .eq. 0) then
         CALL global_fill(temp2,IDS_str,IDE_end,JDS_str,JDE_end,& 
                          u_var,v_var,e_w_average,polavg)
       endif

       if (MYPROC .eq. 0 ) then
         write(iunit) TEMP2  ! FIS
         NCOUNT=NCOUNT+1
         write(0,*) 'FIS ', NCOUNT
         write(0,*) 'min, max of nmm_fis: ', minval(TEMP2),maxval(TEMP2)

         nc = nc + 1

         call make_nemsio_array(TEMP2,TEMP1D, IDS_str, IDE_end, JDS_str, JDE_end)

!!! do all recname, reclevtype, reclev defs prior to opening file?

!         recname(nc)='fis';reclevtyp(nc)='sfc';reclev(nc)=1
             call nemsio_writerec(iunit_nemsio,nc,TEMP1D,iret=iret)

             if (iret/=0) then
               write(0,*)' nemsio_writerec error ', iret
#ifdef _MPI
               CALL mpi_abort(MPI_COMM_WORLD,1,ierr)
#else
               STOP
#endif
             endif

! ---- Terrain hgt (nemsio only) ----------------------------------------

        DO J=JDS_str,JDE_end
        DO I=IDS_str,IDE_end
	 TEMP2(I,J)=TEMP2(I,J)/9.81
        ENDDO
        ENDDO

         nc = nc + 1
         call make_nemsio_array(TEMP2,TEMP1D, IDS_str, IDE_end, JDS_str, JDE_end)
!         recname(nc)='hgt';reclevtyp(nc)='sfc';reclev(nc)=1
             call nemsio_writerec(iunit_nemsio,nc,TEMP1D,iret=iret)
       endif

!--- STDH ---------------------------------------------------------------

       do J=JTS,min(JTE,JDE_end)
       do I=ITS,min(ITE,IDE_end)
         TEMP1(I,J)=grid%stdvtopo(I+ITARG,J+JTARG)
       enddo
       enddo

       if (print_diag) write(0,*) 'min, max of stdvtopo: ', minval(temp1), maxval(temp1)

       u_var=.false.
       v_var=.false.
       e_w_average=.true.

       call make_global(TEMP1,TEMP2,MYPROC,npes,mpi_comm_world                &
                       ,IDS_str,IDE_end,JDS_str,JDE_end,1,1             &
                       ,IMS,IME,JMS,JME,KMS,KME                           &
                       ,ITS,min(ITE,IDE_end),JTS,min(JTE,JDE_end),KTS,KTE)

       if (DOFILL .and. MYPROC .eq. 0) then
         CALL global_fill(temp2,IDS_str,IDE_end,JDS_str,JDE_end,& 
                          u_var,v_var,e_w_average,polavg)
       endif

       if (MYPROC .eq. 0 ) then
         write(iunit) TEMP2   ! STDH field.
         NCOUNT=NCOUNT+1
         write(0,*) 'STDH ', NCOUNT
         write(0,*) 'min, max of STDH: ', minval(TEMP2),maxval(TEMP2)

         nc = nc + 1
         call make_nemsio_array(TEMP2,TEMP1D, IDS_str, IDE_end, JDS_str, JDE_end)
         call nemsio_writerec(iunit_nemsio,nc,TEMP1D,iret=iret)
       endif

!--- SM -----------------------------------------------------------------

       do J=JTS,min(JTE,JDE_end)
       do I=ITS,min(ITE,IDE_end)
         TEMP1(I,J)=grid%sm(I+ITARG,J+JTARG)
       end do
       end do

       call make_global(TEMP1,TEMP2,MYPROC,npes,mpi_comm_world                &
                       ,IDS_str,IDE_end,JDS_str,JDE_end,1,1             &
                       ,IMS,IME,JMS,JME,KMS,KME                           &
                       ,ITS,min(ITE,IDE_end),JTS,min(JTE,JDE_end),KTS,KTE)

       if ( DOFILL .and. MYPROC .eq. 0 ) then
         e_w_average=.false.
         polavg=.true.
         CALL global_fill(temp2,IDS_str,IDE_end,JDS_str,JDE_end,& 
                          u_var,v_var,e_w_average,polavg)
       endif

       if (MYPROC .eq. 0 ) then
         write(iunit) TEMP2   ! SM
         NCOUNT=NCOUNT+1
         write(0,*) 'SM ', NCOUNT

         nc = nc + 1

         call make_nemsio_array(TEMP2,TEMP1D, IDS_str, IDE_end, JDS_str, JDE_end)
         call nemsio_writerec(iunit_nemsio,nc,TEMP1D,iret=iret)
       endif

!--- PD -----------------------------------------------------------------

       do J=JTS,min(JTE,JDE_end)
       do I=ITS,min(ITE,IDE_end)
         TEMP1(I,J)=grid%pd(I+ITARG,J+JTARG)
       end do
       end do

       call make_global(TEMP1,TEMP2,MYPROC,npes,mpi_comm_world                &
                       ,IDS_str,IDE_end,JDS_str,JDE_end,1,1             &
                       ,IMS,IME,JMS,JME,KMS,KME                           &
                       ,ITS,min(ITE,IDE_end),JTS,min(JTE,JDE_end),KTS,KTE)

       if ( DOFILL .and. MYPROC .eq. 0 ) then
         e_w_average=.true.
         polavg=.true.
         CALL global_fill(temp2,IDS_str,IDE_end,JDS_str,JDE_end,& 
                          u_var,v_var,e_w_average,polavg)
       endif

       if (MYPROC .eq. 0 ) then
         write(0,*) 'minval(TEMP2),maxval(TEMP2) as written for PD: ', minval(TEMP2),maxval(TEMP2)
         write(iunit) TEMP2   ! PD
         NCOUNT=NCOUNT+1
         write(0,*) 'PD ', NCOUNT

         nc = nc + 1
         call make_nemsio_array(TEMP2,TEMP1D, IDS_str, IDE_end, JDS_str, JDE_end)
         call nemsio_writerec(iunit_nemsio,nc,TEMP1D,iret=iret)

         allocate(PSFC(IDS_str:IDE_end,JDS_str:JDE_end))

         do J=JDS_str,JDE_end
         do I=IDS_str,IDE_end
           PSFC(I,J)=TEMP2(I,J)+grid%pt
         enddo
         enddo
         write(0,*) 'extremes for PSFC: ', minval(PSFC),maxval(PSFC)

       endif

!--- U ------------------------------------------------------------------

       do L=1,LM

       do J=JTS,min(JTE,JDE_end)
       do I=ITS,min(ITE,IDE_end)
         TEMP1(I,J)=grid%u(I+ITARG,J+JTARG,L)
       end do
       end do

       call make_global(TEMP1,TEMP2,MYPROC,npes,mpi_comm_world                &
                       ,IDS_str,IDE_end,JDS_str,JDE_end,1,1             &
                       ,IMS,IME,JMS,JME,KMS,KME                           &
                       ,ITS,min(ITE,IDE_end),JTS,min(JTE,JDE_end),KTS,KTE)

       u_var=.true.
       v_var=.false.

       if ( DOFILL .and. MYPROC .eq. 0 ) then
         CALL global_fill(temp2,IDS_str,IDE_end,JDS_str,JDE_end,& 
                          u_var,v_var,e_w_average,polavg)
       endif

       if (MYPROC .eq. 0 ) then
         write(iunit) TEMP2   ! U
         NCOUNT=NCOUNT+1
         write(0,*) NCOUNT, ' U '

         nc = nc + 1
         call make_nemsio_array(TEMP2,TEMP1D, IDS_str, IDE_end, JDS_str, JDE_end)
         call nemsio_writerec(iunit_nemsio,nc,TEMP1D,iret=iret)
       endif

       end do

!--- U10 (nemsio only, slightly bogus) ----------------------------------

! for now a simple placeholder defined as 90% of the lowest model level

       if (MYPROC .eq. 0 ) then
         TEMP2=0.90*TEMP2
         nc = nc + 1
         call make_nemsio_array(TEMP2,TEMP1D, IDS_str, IDE_end, JDS_str, JDE_end)
         call nemsio_writerec(iunit_nemsio,nc,TEMP1D,iret=iret)
       endif

!--- V ------------------------------------------------------------------

       do L=1,LM
       do J=JTS,min(JTE,JDE_end)
       do I=ITS,min(ITE,IDE_end)
         TEMP1(I,J)=grid%v(I+ITARG,J+JTARG,L)
       end do
       end do

       call make_global(TEMP1,TEMP2,MYPROC,npes,mpi_comm_world                &
                       ,IDS_str,IDE_end,JDS_str,JDE_end,1,1             &
                       ,IMS,IME,JMS,JME,KMS,KME                           &
                       ,ITS,min(ITE,IDE_end),JTS,min(JTE,JDE_end),KTS,KTE)

       u_var=.false.
       v_var=.true.

       if ( DOFILL .and. MYPROC .eq. 0 ) then
         CALL global_fill(temp2,IDS_str,IDE_end,JDS_str,JDE_end,& 
                          u_var,v_var,e_w_average,polavg)
       endif

       if (MYPROC .eq. 0 ) then
         write(iunit) TEMP2   ! V
         NCOUNT=NCOUNT+1
         write(0,*) NCOUNT, ' V '

         nc = nc + 1
         call make_nemsio_array(TEMP2,TEMP1D, IDS_str, IDE_end, JDS_str, JDE_end)
         call nemsio_writerec(iunit_nemsio,nc,TEMP1D,iret=iret)
       endif

       end do

!--- V10 (nemsio only, slightly bogus) ----------------------------------

! for now a simple placeholder defined as 90% of the lowest model level

       if (MYPROC .eq. 0 ) then
         TEMP2=0.90*TEMP2
         nc = nc + 1
         call make_nemsio_array(TEMP2,TEMP1D, IDS_str, IDE_end, JDS_str, JDE_end)
         call nemsio_writerec(iunit_nemsio,nc,TEMP1D,iret=iret)
       endif


!--- T ------------------------------------------------------------------

       do L=1,LM

       do J=JTS,min(JTE,JDE_end)
       do I=ITS,min(ITE,IDE_end)
         TEMP1(I,J)=grid%t(I+ITARG,J+JTARG,L)
       end do
       end do
       if (print_diag) write(0,*) 'min, max of T : ', L, minval(temp1),maxval(temp1)

       call make_global(TEMP1,TEMP2,MYPROC,npes,mpi_comm_world                &
                       ,IDS_str,IDE_end,JDS_str,JDE_end,1,1             &
                       ,IMS,IME,JMS,JME,KMS,KME                           &
                       ,ITS,min(ITE,IDE_end),JTS,min(JTE,JDE_end),KTS,KTE)

       u_var=.false.
       v_var=.false.

       if ( DOFILL .and. MYPROC .eq. 0 ) then
         CALL global_fill(temp2,IDS_str,IDE_end,JDS_str,JDE_end,& 
                          u_var,v_var,e_w_average,polavg)
       endif

       if (MYPROC .eq. 0 ) then
         write(iunit) TEMP2   ! T
         NCOUNT=NCOUNT+1
         write(0,*) NCOUNT, ' T '

         nc = nc + 1
         call make_nemsio_array(TEMP2,TEMP1D, IDS_str, IDE_end, JDS_str, JDE_end)
         call nemsio_writerec(iunit_nemsio,nc,TEMP1D,iret=iret)
       endif

       end do

!--- Q ------------------------------------------------------------------

       do L=1,LM

       do J=JTS,min(JTE,JDE_end)
       do I=ITS,min(ITE,IDE_end)
         TEMP1(I,J)=grid%q(I+ITARG,J+JTARG,L)
       end do
       end do

       if (print_diag) write(0,*) 'min,max of TEMP1 for Q: ', L, minval(TEMP1), maxval(TEMP1)

       call make_global(TEMP1,TEMP2,MYPROC,npes,mpi_comm_world                &
                       ,IDS_str,IDE_end,JDS_str,JDE_end,1,1             &
                       ,IMS,IME,JMS,JME,KMS,KME                           &
                       ,ITS,min(ITE,IDE_end),JTS,min(JTE,JDE_end),KTS,KTE)


       if ( DOFILL .and. MYPROC .eq. 0 ) then
         CALL global_fill(temp2,IDS_str,IDE_end,JDS_str,JDE_end,& 
                          u_var,v_var,e_w_average,polavg)
       endif

       if (MYPROC .eq. 0 ) then
         write(0,*) 'min,max of TEMP2 for Q: ', L, minval(TEMP2), maxval(TEMP2)
         write(iunit) TEMP2   ! Q
         NCOUNT=NCOUNT+1
         write(0,*) NCOUNT, ' Q '

         nc = nc + 1
         call make_nemsio_array(TEMP2,TEMP1D, IDS_str, IDE_end, JDS_str, JDE_end)
         call nemsio_writerec(iunit_nemsio,nc,TEMP1D,iret=iret)
       endif

       end do

!--- CWM ----------------------------------------------------------------


        if (print_diag) then
	write(0,*) 'size(cwm) when placed into TEMP1: ', size(grid%cwm,dim=1), size(grid%cwm,dim=2), size(grid%cwm,dim=3)
	write(0,*) 'size(TEMP1): ', size(TEMP1,dim=1),size(TEMP1,dim=2)
        endif

       do L=1,LM

	TEMP1=0.

       do J=JTS,min(JTE,JDE_end)
       do I=ITS,min(ITE,IDE_end)
         TEMP1(I,J)=grid%cwm(I+ITARG,J+JTARG,L)

	if (TEMP1(I,J) .lt. 0. .or. TEMP1(I,J) .gt. 50.e-2) then
	write(0,*) 'I,J,L, grid%CWM: ', I,J,L, grid%CWM(I+ITARG,J+JTARG,L)
	endif

       end do
       end do

	if (print_diag) write(0,*) 'L, maxval CWM in TEMP1: ', L, maxval(TEMP1)

       call make_global(TEMP1,TEMP2,MYPROC,npes,mpi_comm_world                &
                       ,IDS_str,IDE_end,JDS_str,JDE_end,1,1             &
                       ,IMS,IME,JMS,JME,KMS,KME                           &
                       ,ITS,min(ITE,IDE_end),JTS,min(JTE,JDE_end),KTS,KTE)

       if ( DOFILL .and. MYPROC .eq. 0 ) then
         CALL global_fill(temp2,IDS_str,IDE_end,JDS_str,JDE_end,& 
                          u_var,v_var,e_w_average,polavg)
       endif

       if (MYPROC .eq. 0 ) then
	write(0,*) 'min,maxval of CWM as written: ', minval(TEMP2),maxval(TEMP2)
         write(iunit) TEMP2   ! CWM
         NCOUNT=NCOUNT+1
         write(0,*) NCOUNT, ' CWM '

         nc = nc + 1
         call make_nemsio_array(TEMP2,TEMP1D, IDS_str, IDE_end, JDS_str, JDE_end)
         call nemsio_writerec(iunit_nemsio,nc,TEMP1D,iret=iret)
       endif

       end do

!--- O3 (placeholder for now ) ----------------------------------------------------------------

       if (MYPROC .eq. 0 ) then

       TEMP2=0.

       do L=1,LM
         write(iunit) TEMP2 ! O3
         NCOUNT=NCOUNT+1
         write(0,*) NCOUNT, ' O3MR'

! what will nemsio name be? (o3mr or something else?)
!
         nc = nc + 1

         call make_nemsio_array(TEMP2,TEMP1D, IDS_str, IDE_end, JDS_str, JDE_end)
         call nemsio_writerec(iunit_nemsio,nc,TEMP1D,iret=iret)

       enddo
       endif

!--- PINT (nemsio only) -----------------------------------------------

       do L=1,LM+1

       do J=JTS,min(JTE,JDE_end)
       do I=ITS,min(ITE,IDE_end)
         TEMP1(I,J)=grid%pint_out(I+ITARG,L,J+JTARG)
       end do
       end do

!       write(0,*) 'min,max of TEMP1 for PINT: ', L, minval(TEMP1), maxval(TEMP1)

       call make_global(TEMP1,TEMP2,MYPROC,npes,mpi_comm_world                &
                       ,IDS_str,IDE_end,JDS_str,JDE_end,1,1             &
                       ,IMS,IME,JMS,JME,KMS,KME                           &
                       ,ITS,min(ITE,IDE_end),JTS,min(JTE,JDE_end),KTS,KTE)

       if ( DOFILL .and. MYPROC .eq. 0 ) then
         CALL global_fill(temp2,IDS_str,IDE_end,JDS_str,JDE_end,& 
                          u_var,v_var,e_w_average,polavg)
       endif

       if (MYPROC .eq. 0 ) then
       write(0,*) 'min,max of TEMP2 for PINT: ', L, minval(TEMP2), maxval(TEMP2)
         nc = nc + 1
         call make_nemsio_array(TEMP2,TEMP1D, IDS_str, IDE_end, JDS_str, JDE_end)
         call nemsio_writerec(iunit_nemsio,nc,TEMP1D,iret=iret)
       endif

       end do

!--- ALBEDO -------------------------------------------------------------

       do J=JTS,min(JTE,JDE_end)
       do I=ITS,min(ITE,IDE_end)
         TEMP1(I,J)=grid%albedo(I+ITARG,J+JTARG)
       end do
       end do

       call make_global(TEMP1,TEMP2,MYPROC,npes,mpi_comm_world                &
                       ,IDS_str,IDE_end,JDS_str,JDE_end,1,1             &
                       ,IMS,IME,JMS,JME,KMS,KME                           &
                       ,ITS,min(ITE,IDE_end),JTS,min(JTE,JDE_end),KTS,KTE)

       if ( DOFILL .and. MYPROC .eq. 0 ) then
         e_w_average=.false.
         polavg=.true.
         CALL global_fill(temp2,IDS_str,IDE_end,JDS_str,JDE_end,& 
                          u_var,v_var,e_w_average,polavg)
       endif

       if (MYPROC .eq. 0 ) then
         write(0,*) 'min, max of temp2 for ALBEDO: ', minval(temp2),maxval(temp2)
         write(iunit) TEMP2   ! ALBEDO
         NCOUNT=NCOUNT+1
         write(0,*) NCOUNT, ' ALBEDO '

         nc = nc + 1
         call make_nemsio_array(TEMP2,TEMP1D, IDS_str, IDE_end, JDS_str, JDE_end)
         call nemsio_writerec(iunit_nemsio,nc,TEMP1D,iret=iret)
       endif

!--- ALBASE -----------------------------------------------

       do J=JTS,min(JTE,JDE_end)
       do I=ITS,min(ITE,IDE_end)
         TEMP1(I,J)=grid%albase(I+ITARG,J+JTARG)
       end do
       end do

       call make_global(TEMP1,TEMP2,MYPROC,npes,mpi_comm_world                &
                       ,IDS_str,IDE_end,JDS_str,JDE_end,1,1             &
                       ,IMS,IME,JMS,JME,KMS,KME                           &
                       ,ITS,min(ITE,IDE_end),JTS,min(JTE,JDE_end),KTS,KTE)

       if ( DOFILL .and. MYPROC .eq. 0 ) then
         e_w_average=.false.
         polavg=.true.
         CALL global_fill(temp2,IDS_str,IDE_end,JDS_str,JDE_end,& 
                          u_var,v_var,e_w_average,polavg)
       endif

       if (MYPROC .eq. 0 ) then
         write(0,*) 'min, max of temp2 for ALBASE: ', minval(temp2),maxval(temp2)
         write(iunit) TEMP2   ! ALBASE
         NCOUNT=NCOUNT+1
         write(0,*) NCOUNT, ' ALBASE '

         nc = nc + 1
         call make_nemsio_array(TEMP2,TEMP1D, IDS_str, IDE_end, JDS_str, JDE_end)
         call nemsio_writerec(iunit_nemsio,nc,TEMP1D,iret=iret)
       endif

!--- EPSR ---------------------------------------------------------------

       do J=JTS,min(JTE,JDE_end)
       do I=ITS,min(ITE,IDE_end)
         TEMP1(I,J)=grid%epsr(I+ITARG,J+JTARG)
       end do
       end do

       call make_global(TEMP1,TEMP2,MYPROC,npes,mpi_comm_world                &
                       ,IDS_str,IDE_end,JDS_str,JDE_end,1,1             &
                       ,IMS,IME,JMS,JME,KMS,KME                           &
                       ,ITS,min(ITE,IDE_end),JTS,min(JTE,JDE_end),KTS,KTE)

       if ( DOFILL .and. MYPROC .eq. 0 ) then
         e_w_average=.false.
         polavg=.true.
         CALL global_fill(temp2,IDS_str,IDE_end,JDS_str,JDE_end,& 
                          u_var,v_var,e_w_average,polavg)
       endif

       if (MYPROC .eq. 0 ) then
         write(iunit) TEMP2   ! EPSR
         write(0,*) 'min, max of EPSR: ', minval(TEMP2),maxval(TEMP2)
         NCOUNT=NCOUNT+1
         write(0,*) NCOUNT, ' EPSR '

         nc = nc + 1
         call make_nemsio_array(TEMP2,TEMP1D, IDS_str, IDE_end, JDS_str, JDE_end)
         call nemsio_writerec(iunit_nemsio,nc,TEMP1D,iret=iret)
       endif

!--- MXSNAL -------------------------------------------------------------

       do J=JTS,min(JTE,JDE_end)
       do I=ITS,min(ITE,IDE_end)
         TEMP1(I,J)=grid%MXSNAL(I+ITARG,J+JTARG)
       end do
       end do

       call make_global(TEMP1,TEMP2,MYPROC,npes,mpi_comm_world                &
                       ,IDS_str,IDE_end,JDS_str,JDE_end,1,1             &
                       ,IMS,IME,JMS,JME,KMS,KME                           &
                       ,ITS,min(ITE,IDE_end),JTS,min(JTE,JDE_end),KTS,KTE)

       if ( DOFILL .and. MYPROC .eq. 0 ) then
         polavg=.true.
         CALL global_fill(temp2,IDS_str,IDE_end,JDS_str,JDE_end,& 
                          u_var,v_var,e_w_average,polavg)
       endif

       if (MYPROC .eq. 0 ) then
         write(iunit) TEMP2   ! SNOW ALBEDO
         write(0,*) 'min, max of SNO ALBEDO: ', minval(TEMP2),maxval(TEMP2)
         NCOUNT=NCOUNT+1
         write(0,*) NCOUNT, ' SNOW ALBEDO '

         nc = nc + 1
         call make_nemsio_array(TEMP2,TEMP1D, IDS_str, IDE_end, JDS_str, JDE_end)
         call nemsio_writerec(iunit_nemsio,nc,TEMP1D,iret=iret)
       endif

!--- TSK ----------------------------------------------------------------

       do J=JTS,min(JTE,JDE_end)
       do I=ITS,min(ITE,IDE_end)
         TEMP1(I,J)=grid%tsk(I+ITARG,J+JTARG)
       end do
       end do

       call make_global(TEMP1,TEMP2,MYPROC,npes,mpi_comm_world                &
                       ,IDS_str,IDE_end,JDS_str,JDE_end,1,1             &
                       ,IMS,IME,JMS,JME,KMS,KME                           &
                       ,ITS,min(ITE,IDE_end),JTS,min(JTE,JDE_end),KTS,KTE)

       if ( DOFILL .and. MYPROC .eq. 0 ) then
         polavg=.true.
         CALL global_fill(temp2,IDS_str,IDE_end,JDS_str,JDE_end,& 
                          u_var,v_var,e_w_average,polavg)
       endif

       if (MYPROC .eq. 0 ) then
         write(0,*) 'min,max for TSK ', minval(TEMP2),maxval(TEMP2)
         write(iunit) TEMP2   ! TSK
         NCOUNT=NCOUNT+1
         write(0,*) NCOUNT, ' TSK '

         nc = nc + 1
         call make_nemsio_array(TEMP2,TEMP1D, IDS_str, IDE_end, JDS_str, JDE_end)
         call nemsio_writerec(iunit_nemsio,nc,TEMP1D,iret=iret)
       endif

!--- THS (nemsio only) --------------------------------------------------

       if (MYPROC .eq. 0 ) then

       do J=JDS_str,JDE_end
       do I=IDS_str,IDE_end
       TEMP2(I,J)=TEMP2(I,J)*(100000./(PSFC(I,J)))**(0.28589641)
       enddo
       enddo

         write(0,*) 'min,max for THS ', minval(TEMP2),maxval(TEMP2)

         nc = nc + 1
         call make_nemsio_array(TEMP2,TEMP1D, IDS_str, IDE_end, JDS_str, JDE_end)
         call nemsio_writerec(iunit_nemsio,nc,TEMP1D,iret=iret)
       endif

!--- SST ----------------------------------------------------------------

       do J=JTS,min(JTE,JDE_end)
       do I=ITS,min(ITE,IDE_end)
         TEMP1(I,J)=grid%SST(I+ITARG,J+JTARG)
       end do
       end do

       call make_global(TEMP1,TEMP2,MYPROC,npes,mpi_comm_world                &
                       ,IDS_str,IDE_end,JDS_str,JDE_end,1,1             &
                       ,IMS,IME,JMS,JME,KMS,KME                           &
                       ,ITS,min(ITE,IDE_end),JTS,min(JTE,JDE_end),KTS,KTE)

       if ( DOFILL .and. MYPROC .eq. 0 ) then
         CALL global_fill(temp2,IDS_str,IDE_end,JDS_str,JDE_end,& 
                          u_var,v_var,e_w_average,polavg)
       endif

       if (MYPROC .eq. 0 ) then
         write(iunit) TEMP2   ! SST
         write(0,*) 'min,max for SST', minval(TEMP2),maxval(TEMP2)
         NCOUNT=NCOUNT+1
         write(0,*) NCOUNT, ' SST '

         nc = nc + 1
         call make_nemsio_array(TEMP2,TEMP1D, IDS_str, IDE_end, JDS_str, JDE_end)
         call nemsio_writerec(iunit_nemsio,nc,TEMP1D,iret=iret)
       endif

!--- SNO ----------------------------------------------------------------

       do J=JTS,min(JTE,JDE_end)
       do I=ITS,min(ITE,IDE_end)
         TEMP1(I,J)=grid%sno(I+ITARG,J+JTARG)
       end do
       end do

       call make_global(TEMP1,TEMP2,MYPROC,npes,mpi_comm_world                &
                       ,IDS_str,IDE_end,JDS_str,JDE_end,1,1             &
                       ,IMS,IME,JMS,JME,KMS,KME                           &
                       ,ITS,min(ITE,IDE_end),JTS,min(JTE,JDE_end),KTS,KTE)

       if ( DOFILL .and. MYPROC .eq. 0 ) then
         polavg=.true.
         CALL global_fill(temp2,IDS_str,IDE_end,JDS_str,JDE_end,& 
                          u_var,v_var,e_w_average,polavg)
       endif

       if (MYPROC .eq. 0 ) then
         write(iunit) TEMP2   ! SNO
         write(0,*) 'min, max of SNO: ', minval(TEMP2),maxval(TEMP2)
         NCOUNT=NCOUNT+1
         write(0,*) NCOUNT, ' SNO '

         nc = nc + 1
         call make_nemsio_array(TEMP2,TEMP1D, IDS_str, IDE_end, JDS_str, JDE_end)
         call nemsio_writerec(iunit_nemsio,nc,TEMP1D,iret=iret)
       endif

!--- SI -----------------------------------------------------------------

       do J=JTS,min(JTE,JDE_end)
       do I=ITS,min(ITE,IDE_end)
         TEMP1(I,J)=grid%si(I+ITARG,J+JTARG)
       end do
       end do

       call make_global(TEMP1,TEMP2,MYPROC,npes,mpi_comm_world                &
                       ,IDS_str,IDE_end,JDS_str,JDE_end,1,1             &
                       ,IMS,IME,JMS,JME,KMS,KME                           &
                       ,ITS,min(ITE,IDE_end),JTS,min(JTE,JDE_end),KTS,KTE)

       if ( DOFILL .and. MYPROC .eq. 0 ) then
         polavg=.true.
         CALL global_fill(temp2,IDS_str,IDE_end,JDS_str,JDE_end,& 
                          u_var,v_var,e_w_average,polavg)
       endif

       if (MYPROC .eq. 0 ) then
         write(iunit) TEMP2   ! SI (for snow depth)
         write(0,*) 'min, max of SI: ', minval(TEMP2),maxval(TEMP2)
         NCOUNT=NCOUNT+1
         write(0,*) NCOUNT, ' SI '

         nc = nc + 1
         call make_nemsio_array(TEMP2,TEMP1D, IDS_str, IDE_end, JDS_str, JDE_end)
         call nemsio_writerec(iunit_nemsio,nc,TEMP1D,iret=iret)
       endif

!--- SICE ---------------------------------------------------------------

       do J=JTS,min(JTE,JDE_end)
       do I=ITS,min(ITE,IDE_end)
         TEMP1(I,J)=grid%sice(I+ITARG,J+JTARG)
       end do
       end do

       call make_global(TEMP1,TEMP2,MYPROC,npes,mpi_comm_world                &
                       ,IDS_str,IDE_end,JDS_str,JDE_end,1,1             &
                       ,IMS,IME,JMS,JME,KMS,KME                           &
                       ,ITS,min(ITE,IDE_end),JTS,min(JTE,JDE_end),KTS,KTE)

       if ( DOFILL .and. MYPROC .eq. 0 ) then
         polavg=.true.
         CALL global_fill(temp2,IDS_str,IDE_end,JDS_str,JDE_end,& 
                          u_var,v_var,e_w_average,polavg)
       endif

       if (MYPROC .eq. 0 ) then
         write(iunit) TEMP2   ! SICE
         write(0,*) 'min, max of SICE: ', minval(TEMP2),maxval(TEMP2)
         NCOUNT=NCOUNT+1
         write(0,*) NCOUNT, ' SICE '

         nc = nc + 1
         call make_nemsio_array(TEMP2,TEMP1D, IDS_str, IDE_end, JDS_str, JDE_end)
         call nemsio_writerec(iunit_nemsio,nc,TEMP1D,iret=iret)
       endif

!--- TG -----------------------------------------------------------------

       do J=JTS,min(JTE,JDE_end)
       do I=ITS,min(ITE,IDE_end)
         TEMP1(I,J)=grid%tg(I+ITARG,J+JTARG)
       end do
       end do

       call make_global(TEMP1,TEMP2,MYPROC,npes,mpi_comm_world                &
                       ,IDS_str,IDE_end,JDS_str,JDE_end,1,1             &
                       ,IMS,IME,JMS,JME,KMS,KME                           &
                       ,ITS,min(ITE,IDE_end),JTS,min(JTE,JDE_end),KTS,KTE)

       if ( DOFILL .and. MYPROC .eq. 0 ) then
         polavg=.true.
         CALL global_fill(temp2,IDS_str,IDE_end,JDS_str,JDE_end,& 
                          u_var,v_var,e_w_average,polavg)
       endif

       if (MYPROC .eq. 0 ) then
         write(iunit) TEMP2   ! TG
         write(0,*) 'min, max of TG: ', minval(TEMP2),maxval(TEMP2)
         NCOUNT=NCOUNT+1
         write(0,*) NCOUNT, ' TG '

         nc = nc + 1
         call make_nemsio_array(TEMP2,TEMP1D, IDS_str, IDE_end, JDS_str, JDE_end)
         call nemsio_writerec(iunit_nemsio,nc,TEMP1D,iret=iret)
       endif

!--- CMC ----------------------------------------------------------------

       do J=JTS,min(JTE,JDE_end)
       do I=ITS,min(ITE,IDE_end)
!         TEMP1(I,J)=max(grid%cmc(I+ITARG,J+JTARG),0.001)
         TEMP1(I,J)=grid%cmc(I+ITARG,J+JTARG)
       end do
       end do

       call make_global(TEMP1,TEMP2,MYPROC,npes,mpi_comm_world                &
                       ,IDS_str,IDE_end,JDS_str,JDE_end,1,1             &
                       ,IMS,IME,JMS,JME,KMS,KME                           &
                       ,ITS,min(ITE,IDE_end),JTS,min(JTE,JDE_end),KTS,KTE)

       if ( DOFILL .and. MYPROC .eq. 0 ) then
         polavg=.true.
         CALL global_fill(temp2,IDS_str,IDE_end,JDS_str,JDE_end,& 
                          u_var,v_var,e_w_average,polavg)
       endif


       if (MYPROC .eq. 0 ) then
         write(iunit) TEMP2   ! CMC
         write(0,*) 'min, max of CMC: ', minval(TEMP2),maxval(TEMP2)
         NCOUNT=NCOUNT+1
         write(0,*) NCOUNT, ' CMC '

         nc = nc + 1
         call make_nemsio_array(TEMP2,TEMP1D, IDS_str, IDE_end, JDS_str, JDE_end)
         call nemsio_writerec(iunit_nemsio,nc,TEMP1D,iret=iret)
       endif

!--- SR -----------------------------------------------------------------

       do J=JTS,min(JTE,JDE_end)
       do I=ITS,min(ITE,IDE_end)
         TEMP1(I,J)=grid%sr(I+ITARG,J+JTARG)
       end do
       end do

       call make_global(TEMP1,TEMP2,MYPROC,npes,mpi_comm_world                &
                       ,IDS_str,IDE_end,JDS_str,JDE_end,1,1             &
                       ,IMS,IME,JMS,JME,KMS,KME                           &
                       ,ITS,min(ITE,IDE_end),JTS,min(JTE,JDE_end),KTS,KTE)

       if ( DOFILL .and. MYPROC .eq. 0 ) then
         polavg=.true.
         CALL global_fill(temp2,IDS_str,IDE_end,JDS_str,JDE_end,& 
                          u_var,v_var,e_w_average,polavg)
       endif

       if (MYPROC .eq. 0 ) then
         write(iunit) TEMP2   ! SR
         write(0,*) 'min, max of SR: ', minval(TEMP2),maxval(TEMP2)
         NCOUNT=NCOUNT+1
         write(0,*) NCOUNT, ' SR '

         nc = nc + 1
         call make_nemsio_array(TEMP2,TEMP1D, IDS_str, IDE_end, JDS_str, JDE_end)
         call nemsio_writerec(iunit_nemsio,nc,TEMP1D,iret=iret)
       endif

!--- USTAR --------------------------------------------------------------

       do J=JTS,min(JTE,JDE_end)
       do I=ITS,min(ITE,IDE_end)
         TEMP1(I,J)=grid%ustar(I+ITARG,J+JTARG)
       end do
       end do

       call make_global(TEMP1,TEMP2,MYPROC,npes,mpi_comm_world                &
                       ,IDS_str,IDE_end,JDS_str,JDE_end,1,1             &
                       ,IMS,IME,JMS,JME,KMS,KME                           &
                       ,ITS,min(ITE,IDE_end),JTS,min(JTE,JDE_end),KTS,KTE)

       if ( DOFILL .and. MYPROC .eq. 0 ) then
         polavg=.true.
         CALL global_fill(temp2,IDS_str,IDE_end,JDS_str,JDE_end,& 
                          u_var,v_var,e_w_average,polavg)
       endif

       if (MYPROC .eq. 0 ) then
         write(iunit) TEMP2   ! USTAR
         write(0,*) 'min, max of USTAR: ', minval(TEMP2),maxval(TEMP2)
         NCOUNT=NCOUNT+1
         write(0,*) NCOUNT, ' USTAR  '

         nc = nc + 1
         call make_nemsio_array(TEMP2,TEMP1D, IDS_str, IDE_end, JDS_str, JDE_end)
         call nemsio_writerec(iunit_nemsio,nc,TEMP1D,iret=iret)
       endif

!--- Z0 -----------------------------------------------------------------

       do J=JTS,min(JTE,JDE_end)
       do I=ITS,min(ITE,IDE_end)
         TEMP1(I,J)=grid%z0(I+ITARG,J+JTARG)
       end do
       end do

       call make_global(TEMP1,TEMP2,MYPROC,npes,mpi_comm_world                &
                       ,IDS_str,IDE_end,JDS_str,JDE_end,1,1             &
                       ,IMS,IME,JMS,JME,KMS,KME                           &
                       ,ITS,min(ITE,IDE_end),JTS,min(JTE,JDE_end),KTS,KTE)

       if ( DOFILL .and. MYPROC .eq. 0 ) then
         polavg=.true.
         CALL global_fill(temp2,IDS_str,IDE_end,JDS_str,JDE_end,& 
                          u_var,v_var,e_w_average,polavg)
       endif

       if (MYPROC .eq. 0 ) then
         write(0,*) 'min, max of Z0: ', minval(TEMP2),maxval(TEMP2)
         write(iunit) TEMP2   ! Z0
         NCOUNT=NCOUNT+1
         write(0,*) NCOUNT, ' Z0  '
         write(iunit) TEMP2   ! Z0BASE (currently same as Z0)
         NCOUNT=NCOUNT+1
         write(0,*) NCOUNT, ' Z0BASE  '

         nc = nc + 1
         call make_nemsio_array(TEMP2,TEMP1D, IDS_str, IDE_end, JDS_str, JDE_end)
         call nemsio_writerec(iunit_nemsio,nc,TEMP1D,iret=iret)
         nc = nc + 1
         call make_nemsio_array(TEMP2,TEMP1D, IDS_str, IDE_end, JDS_str, JDE_end)
         call nemsio_writerec(iunit_nemsio,nc,TEMP1D,iret=iret)

       endif

!--- GLAT -(nemsio only) ---------------------------------------------------------------------

       do J=JTS,min(JTE,JDE_end)
       do I=ITS,min(ITE,IDE_end)
         TEMP1(I,J)=gridin%xlat_m(I+ITARG,J+JTARG)*DEGRAD
       end do
       end do

       call make_global(TEMP1,TEMP2,MYPROC,npes,mpi_comm_world                &
                       ,IDS_str,IDE_end,JDS_str,JDE_end,1,1             &
                       ,IMS,IME,JMS,JME,KMS,KME                           &
                       ,ITS,min(ITE,IDE_end),JTS,min(JTE,JDE_end),KTS,KTE)

	swlat=TEMP2(1,1)/DEGRAD
	nelat=TEMP2(ide-1,jde-1)/DEGRAD
	

       if ( DOFILL .and. MYPROC .eq. 0 ) then
         e_w_average=.false.
         polavg=.false.
         CALL global_fill(temp2,IDS_str,IDE_end,JDS_str,JDE_end,& 
                          u_var,v_var,e_w_average,polavg)
       endif


       if ( MYPROC .eq. 0) then
        write(0,*) 'glat(IDE_end,JDE_end): ', TEMP2(IDE_end,JDE_end)/DEGRAD
        write(0,*) 'glat(IDE_end-1,JDE_end-1): ', TEMP2(IDE_end-1,JDE_end-1)/DEGRAD
        write(0,*) 'glat(IDE_end-2,JDE_end-2): ', TEMP2(IDE_end-2,JDE_end-2)/DEGRAD
        write(0,*) ' '
        write(0,*) 'glat(IDS_str+2,JDS_str+2): ', TEMP2(IDS_str+2,JDS_str+2)/DEGRAD
        write(0,*) 'glat(IDS_str+1,JDS_str+1): ', TEMP2(IDS_str+1,JDS_str+1)/DEGRAD
        write(0,*) 'glat(IDS_str,JDS_str): ', TEMP2(IDS_str,JDS_str)/DEGRAD
         nc = nc + 1
	 idx_glat=nc
         call make_nemsio_array(TEMP2,TEMP1D, IDS_str, IDE_end, JDS_str, JDE_end)
         call nemsio_writerec(iunit_nemsio,nc,TEMP1D,iret=iret)
       endif

!--- GLON -(nemsio only) ---------------------------------------------------------------------

       do J=JTS,min(JTE,JDE_end)
       do I=ITS,min(ITE,IDE_end)
         TEMP1(I,J)=gridin%xlong_m(I+ITARG,J+JTARG)*DEGRAD
       end do
       end do

!        TEMP2=0.

       call make_global(TEMP1,TEMP2,MYPROC,npes,mpi_comm_world                &
                       ,IDS_str,IDE_end,JDS_str,JDE_end,1,1             &
                       ,IMS,IME,JMS,JME,KMS,KME                           &
                       ,ITS,min(ITE,IDE_end),JTS,min(JTE,JDE_end),KTS,KTE)

	swlon=TEMP2(1,1)/DEGRAD
	nelon=TEMP2(ide-1,jde-1)/DEGRAD

       if ( DOFILL .and. MYPROC .eq. 0 ) then
         e_w_average=.false.
         polavg=.false.
         CALL global_fill(temp2,IDS_str,IDE_end,JDS_str,JDE_end,& 
                          u_var,v_var,e_w_average,polavg)
       endif

       if ( MYPROC .eq. 0) then
        write(0,*) ' '
        write(0,*) 'glon(IDE_end,JDE_end): ', TEMP2(IDE_end,JDE_end)/DEGRAD
        write(0,*) 'glon(IDE_end-1,JDE_end-1): ', TEMP2(IDE_end-1,JDE_end-1)/DEGRAD
        write(0,*) 'glon(IDE_end-2,JDE_end-2): ', TEMP2(IDE_end-2,JDE_end-2)/DEGRAD
        write(0,*) ' '
        write(0,*) 'glon(IDS_str+2,JDS_str+2): ', TEMP2(IDS_str+2,JDS_str+2)/DEGRAD
        write(0,*) 'glon(IDS_str+1,JDS_str+1): ', TEMP2(IDS_str+1,JDS_str+1)/DEGRAD
        write(0,*) 'glon(IDS_str,JDS_str): ', TEMP2(IDS_str,JDS_str)/DEGRAD
         nc = nc + 1
	 idx_glon=nc
         call make_nemsio_array(TEMP2,TEMP1D, IDS_str, IDE_end, JDS_str, JDE_end)
         call nemsio_writerec(iunit_nemsio,nc,TEMP1D,iret=iret)
       endif

!--- VLAT -(nemsio only) ---------------------------------------------------------------------

       do J=JTS,min(JTE,JDE_end)
       do I=ITS,min(ITE,IDE_end)
         TEMP1(I,J)=gridin%xlat_v(I+ITARG,J+JTARG)*DEGRAD
       end do
       end do

       call make_global(TEMP1,TEMP2,MYPROC,npes,mpi_comm_world                &
                       ,IDS_str,IDE_end,JDS_str,JDE_end,1,1             &
                       ,IMS,IME,JMS,JME,KMS,KME                           &
                       ,ITS,min(ITE,IDE_end),JTS,min(JTE,JDE_end),KTS,KTE)


       if ( DOFILL .and. MYPROC .eq. 0 ) then
         polavg=.false.
         CALL global_fill(temp2,IDS_str,IDE_end,JDS_str,JDE_end,& 
                          u_var,v_var,e_w_average,polavg)
       endif

       if ( MYPROC .eq. 0) then
         nc = nc + 1
         call make_nemsio_array(TEMP2,TEMP1D, IDS_str, IDE_end, JDS_str, JDE_end)
         call nemsio_writerec(iunit_nemsio,nc,TEMP1D,iret=iret)
       endif

!--- VLON -(nemsio only) ---------------------------------------------------------------------

       do J=JTS,min(JTE,JDE_end)
       do I=ITS,min(ITE,IDE_end)
         TEMP1(I,J)=gridin%xlong_v(I+ITARG,J+JTARG)*DEGRAD
       end do
       end do

       call make_global(TEMP1,TEMP2,MYPROC,npes,mpi_comm_world                &
                       ,IDS_str,IDE_end,JDS_str,JDE_end,1,1             &
                       ,IMS,IME,JMS,JME,KMS,KME                           &
                       ,ITS,min(ITE,IDE_end),JTS,min(JTE,JDE_end),KTS,KTE)

       if ( DOFILL .and. MYPROC .eq. 0 ) then
         polavg=.false.
         CALL global_fill(temp2,IDS_str,IDE_end,JDS_str,JDE_end,& 
                          u_var,v_var,e_w_average,polavg)
       endif

       if ( MYPROC .eq. 0) then
         nc = nc + 1
         call make_nemsio_array(TEMP2,TEMP1D, IDS_str, IDE_end, JDS_str, JDE_end)
         call nemsio_writerec(iunit_nemsio,nc,TEMP1D,iret=iret)
       endif

!--- STC ----------------------------------------------------------------------------------------

       do L=1,NSOIL

       do J=JTS,min(JTE,JDE_end)
       do I=ITS,min(ITE,IDE_end)
         TEMP1(I,J)=grid%stc(I+ITARG,L,J+JTARG)
       enddo
       enddo

       call make_global(TEMP1,TEMP2,MYPROC,npes,mpi_comm_world                &
                       ,IDS_str,IDE_end,JDS_str,JDE_end,1,1             &
                       ,IMS,IME,JMS,JME,KMS,KME                           &
                       ,ITS,min(ITE,IDE_end),JTS,min(JTE,JDE_end),KTS,KTE)

       if ( DOFILL .and. MYPROC .eq. 0 ) then
         polavg=.true.
         CALL global_fill(temp2,IDS_str,IDE_end,JDS_str,JDE_end,& 
                          u_var,v_var,e_w_average,polavg)
       endif

       IF (MYPROC .eq. 0 ) then
         DO J=JDS_str,JDE_end
         DO I=IDS_str,IDE_end
         TEMPSOIL2(L,I,J)=TEMP2(I,J)
         ENDDO
         ENDDO

         nc = nc + 1
         call make_nemsio_array(TEMP2,TEMP1D, IDS_str, IDE_end, JDS_str, JDE_end)
         call nemsio_writerec(iunit_nemsio,nc,TEMP1D,iret=iret)
       endif

       ENDDO  ! on L

       if (MYPROC .eq. 0 ) then
         write(iunit) TEMPSOIL2   ! STC
         write(0,*) 'min, max of STC: ', minval(TEMPSOIL2),maxval(TEMPSOIL2)
         NCOUNT=NCOUNT+1
         write(0,*) NCOUNT, ' STC  '

       endif

!--- SMC ----------------------------------------------------------------

       do L=1,NSOIL

       do J=JTS,min(JTE,JDE_end)
       do I=ITS,min(ITE,IDE_end)
         TEMP1(I,J)=grid%smc(I+ITARG,L,J+JTARG)
       enddo
       enddo

       call make_global(TEMP1,TEMP2,MYPROC,npes,mpi_comm_world                &
                       ,IDS_str,IDE_end,JDS_str,JDE_end,1,1             &
                       ,IMS,IME,JMS,JME,KMS,KME                           &
                       ,ITS,min(ITE,IDE_end),JTS,min(JTE,JDE_end),KTS,KTE)

       if ( DOFILL .and. MYPROC .eq. 0 ) then
         polavg=.true.
         CALL global_fill(temp2,IDS_str,IDE_end,JDS_str,JDE_end,& 
                          u_var,v_var,e_w_average,polavg)
       endif

       IF (MYPROC .eq. 0 ) then
         DO J=JDS_str,JDE_end
         DO I=IDS_str,IDE_end
         TEMPSOIL2(L,I,J)=TEMP2(I,J)
         ENDDO
         ENDDO

         nc = nc + 1
         call make_nemsio_array(TEMP2,TEMP1D, IDS_str, IDE_end, JDS_str, JDE_end)
         call nemsio_writerec(iunit_nemsio,nc,TEMP1D,iret=iret)
       endif

       ENDDO  ! on L

       if (MYPROC .eq. 0 ) then
         write(iunit) TEMPSOIL2   ! SMC
         write(0,*) 'min, max of SMC: ', minval(TEMPSOIL2),maxval(TEMPSOIL2)
         NCOUNT=NCOUNT+1
         write(0,*) NCOUNT, ' SMC  '
       endif

!--- SH2O SMC ----------------------------------------------------------

       do L=1,NSOIL

       do J=JTS,min(JTE,JDE_end)
       do I=ITS,min(ITE,IDE_end)
         TEMP1(I,J)=grid%smc(I+ITARG,L,J+JTARG)
       enddo
       enddo

       call make_global(TEMP1,TEMP2,MYPROC,npes,mpi_comm_world                &
                       ,IDS_str,IDE_end,JDS_str,JDE_end,1,1             &
                       ,IMS,IME,JMS,JME,KMS,KME                           &
                       ,ITS,min(ITE,IDE_end),JTS,min(JTE,JDE_end),KTS,KTE)

       if ( DOFILL .and. MYPROC .eq. 0 ) then
         polavg=.true.
         CALL global_fill(temp2,IDS_str,IDE_end,JDS_str,JDE_end,& 
                          u_var,v_var,e_w_average,polavg)
       endif

       IF (MYPROC .eq. 0 ) then
         DO J=JDS_str,JDE_end
         DO I=IDS_str,IDE_end
         TEMPSOIL2(L,I,J)=TEMP2(I,J)
         ENDDO
         ENDDO

         nc = nc + 1
         call make_nemsio_array(TEMP2,TEMP1D, IDS_str, IDE_end, JDS_str, JDE_end)
         call nemsio_writerec(iunit_nemsio,nc,TEMP1D,iret=iret)
       endif

       ENDDO  ! on L

       if (MYPROC .eq. 0 ) then
         write(iunit) TEMPSOIL2   ! SH2O
         NCOUNT=NCOUNT+1
         write(0,*) NCOUNT, ' SH2O  '
       endif

!--- ISLTYP -------------------------------------------------------------

       do J=JTS,min(JTE,JDE_end)
       do I=ITS,min(ITE,IDE_end)
         ITEMP(I,J)=grid%isltyp(I+ITARG,J+JTARG)
       end do
       end do

       u_var=.false.
       v_var=.false.

       call make_global(float(ITEMP),TEMP2,MYPROC,npes,mpi_comm_world                &
                       ,IDS_str,IDE_end,JDS_str,JDE_end,1,1             &
                       ,IMS,IME,JMS,JME,KMS,KME                           &
                       ,ITS,min(ITE,IDE_end),JTS,min(JTE,JDE_end),KTS,KTE)

       if (DOFILL) then
         polavg=.false.
         CALL global_fill(temp2,IDS_str,IDE_end,JDS_str,JDE_end,& 
                          u_var,v_var,e_w_average,polavg)
       endif

       if (MYPROC .eq. 0 ) then
         write(iunit) int(TEMP2)   ! soil type
         NCOUNT=NCOUNT+1
         write(0,*) NCOUNT, ' ISLTYP  '

         nc = nc + 1
         call make_nemsio_array(TEMP2,TEMP1D, IDS_str, IDE_end, JDS_str, JDE_end)
         call nemsio_writerec(iunit_nemsio,nc,TEMP1D,iret=iret)
       endif

!--- IVGTYP -------------------------------------------------------------

       do J=JTS,min(JTE,JDE_end)
       do I=ITS,min(ITE,IDE_end)
         ITEMP(I,J)=grid%ivgtyp(I+ITARG,J+JTARG)
       end do
       end do

       call make_global(float(ITEMP),TEMP2,MYPROC,npes,mpi_comm_world                &
                       ,IDS_str,IDE_end,JDS_str,JDE_end,1,1             &
                       ,IMS,IME,JMS,JME,KMS,KME                           &
                       ,ITS,min(ITE,IDE_end),JTS,min(JTE,JDE_end),KTS,KTE)

       if (DOFILL) then
         polavg=.false.
         CALL global_fill(temp2,IDS_str,IDE_end,JDS_str,JDE_end,& 
                          u_var,v_var,e_w_average,polavg)
       endif

       if (MYPROC .eq. 0 ) then
         write(iunit) int(TEMP2)   ! veg type
         NCOUNT=NCOUNT+1
         write(0,*) NCOUNT, ' IVGTYP  '

         nc = nc + 1
         call make_nemsio_array(TEMP2,TEMP1D, IDS_str, IDE_end, JDS_str, JDE_end)
         call nemsio_writerec(iunit_nemsio,nc,TEMP1D,iret=iret)
       endif

!--- VEGFRA -------------------------------------------------------------

       do J=JTS,min(JTE,JDE_end)
       do I=ITS,min(ITE,IDE_end)
         TEMP1(I,J)=grid%vegfra(I+ITARG,J+JTARG)
       end do
       end do

       call make_global(TEMP1,TEMP2,MYPROC,npes,mpi_comm_world                &
                       ,IDS_str,IDE_end,JDS_str,JDE_end,1,1             &
                       ,IMS,IME,JMS,JME,KMS,KME                           &
                       ,ITS,min(ITE,IDE_end),JTS,min(JTE,JDE_end),KTS,KTE)

       if (DOFILL) then
         polavg=.true.
         CALL global_fill(temp2,IDS_str,IDE_end,JDS_str,JDE_end,& 
                          u_var,v_var,e_w_average,polavg)
       endif

       if (MYPROC .eq. 0 ) then
         write(iunit) TEMP2   ! veg fraction
         NCOUNT=NCOUNT+1
         write(0,*) NCOUNT, ' VEGFRA  '
         write(0,*) 'min, max veg fraction: ', minval(TEMP2),maxval(TEMP2)

         nc = nc + 1
         call make_nemsio_array(TEMP2,TEMP1D, IDS_str, IDE_end, JDS_str, JDE_end)
         call nemsio_writerec(iunit_nemsio,nc,TEMP1D,iret=iret)
       endif

!--- SNOWFREEALB1 (direct visible) - (nemsio only) -------------------------------------------------

       do J=JTS,min(JTE,JDE_end)
       do I=ITS,min(ITE,IDE_end)
         TEMP1(I,J)=0.01
       end do
       end do

       call make_global(TEMP1,TEMP2,MYPROC,npes,mpi_comm_world                &
                       ,IDS_str,IDE_end,JDS_str,JDE_end,1,1             &
                       ,IMS,IME,JMS,JME,KMS,KME                           &
                       ,ITS,min(ITE,IDE_end),JTS,min(JTE,JDE_end),KTS,KTE)

       if ( DOFILL .and. MYPROC .eq. 0 ) then
         polavg=.true.
         CALL global_fill(temp2,IDS_str,IDE_end,JDS_str,JDE_end,& 
                          u_var,v_var,e_w_average,polavg)
       endif

       if ( MYPROC .eq. 0) then
         nc = nc + 1
         call make_nemsio_array(TEMP2,TEMP1D, IDS_str, IDE_end, JDS_str, JDE_end)
         call nemsio_writerec(iunit_nemsio,nc,TEMP1D,iret=iret)
       endif

!--- SNOWFREEALB2 (diffuse visible) - (nemsio only) -------------------------------------------------

       do J=JTS,min(JTE,JDE_end)
       do I=ITS,min(ITE,IDE_end)
         TEMP1(I,J)=0.02
       end do
       end do

       call make_global(TEMP1,TEMP2,MYPROC,npes,mpi_comm_world                &
                       ,IDS_str,IDE_end,JDS_str,JDE_end,1,1             &
                       ,IMS,IME,JMS,JME,KMS,KME                           &
                       ,ITS,min(ITE,IDE_end),JTS,min(JTE,JDE_end),KTS,KTE)

       if ( DOFILL .and. MYPROC .eq. 0 ) then
         polavg=.true.
         CALL global_fill(temp2,IDS_str,IDE_end,JDS_str,JDE_end,& 
                          u_var,v_var,e_w_average,polavg)
       endif

       if ( MYPROC .eq. 0) then
         nc = nc + 1
         call make_nemsio_array(TEMP2,TEMP1D, IDS_str, IDE_end, JDS_str, JDE_end)
         call nemsio_writerec(iunit_nemsio,nc,TEMP1D,iret=iret)
       endif

!--- SNOWFREEALB3 (direct near-IR) - (nemsio only) -------------------------------------------------

       do J=JTS,min(JTE,JDE_end)
       do I=ITS,min(ITE,IDE_end)
         TEMP1(I,J)=0.03
       end do
       end do

       call make_global(TEMP1,TEMP2,MYPROC,npes,mpi_comm_world                &
                       ,IDS_str,IDE_end,JDS_str,JDE_end,1,1             &
                       ,IMS,IME,JMS,JME,KMS,KME                           &
                       ,ITS,min(ITE,IDE_end),JTS,min(JTE,JDE_end),KTS,KTE)

       if ( DOFILL .and. MYPROC .eq. 0 ) then
         polavg=.true.
         CALL global_fill(temp2,IDS_str,IDE_end,JDS_str,JDE_end,& 
                          u_var,v_var,e_w_average,polavg)
       endif

       if ( MYPROC .eq. 0) then
         nc = nc + 1
         call make_nemsio_array(TEMP2,TEMP1D, IDS_str, IDE_end, JDS_str, JDE_end)
         call nemsio_writerec(iunit_nemsio,nc,TEMP1D,iret=iret)
       endif

!--- SNOWFREEALB4 (diffuse near-IR) - (nemsio only) -------------------------------------------------

       do J=JTS,min(JTE,JDE_end)
       do I=ITS,min(ITE,IDE_end)
         TEMP1(I,J)=0.04
       end do
       end do

       call make_global(TEMP1,TEMP2,MYPROC,npes,mpi_comm_world                &
                       ,IDS_str,IDE_end,JDS_str,JDE_end,1,1             &
                       ,IMS,IME,JMS,JME,KMS,KME                           &
                       ,ITS,min(ITE,IDE_end),JTS,min(JTE,JDE_end),KTS,KTE)

       if ( DOFILL .and. MYPROC .eq. 0 ) then
         polavg=.true.
         CALL global_fill(temp2,IDS_str,IDE_end,JDS_str,JDE_end,& 
                          u_var,v_var,e_w_average,polavg)
       endif

       if ( MYPROC .eq. 0) then
         nc = nc + 1
         call make_nemsio_array(TEMP2,TEMP1D, IDS_str, IDE_end, JDS_str, JDE_end)
         call nemsio_writerec(iunit_nemsio,nc,TEMP1D,iret=iret)
       endif

!--- SNOWFREEALB5 (direct broadband) - (nemsio only) -------------------------------------------------

       do J=JTS,min(JTE,JDE_end)
       do I=ITS,min(ITE,IDE_end)
         TEMP1(I,J)=0.05
       end do
       end do

       call make_global(TEMP1,TEMP2,MYPROC,npes,mpi_comm_world                &
                       ,IDS_str,IDE_end,JDS_str,JDE_end,1,1             &
                       ,IMS,IME,JMS,JME,KMS,KME                           &
                       ,ITS,min(ITE,IDE_end),JTS,min(JTE,JDE_end),KTS,KTE)

       if ( DOFILL .and. MYPROC .eq. 0 ) then
         polavg=.true.
         CALL global_fill(temp2,IDS_str,IDE_end,JDS_str,JDE_end,& 
                          u_var,v_var,e_w_average,polavg)
       endif

       if ( MYPROC .eq. 0) then
         nc = nc + 1
         call make_nemsio_array(TEMP2,TEMP1D, IDS_str, IDE_end, JDS_str, JDE_end)
         call nemsio_writerec(iunit_nemsio,nc,TEMP1D,iret=iret)
       endif

!--- SNOWFREEALB6 (diffuse broadband) - (nemsio only) -------------------------------------------------

       do J=JTS,min(JTE,JDE_end)
       do I=ITS,min(ITE,IDE_end)
         TEMP1(I,J)=0.06
       end do
       end do

       call make_global(TEMP1,TEMP2,MYPROC,npes,mpi_comm_world                &
                       ,IDS_str,IDE_end,JDS_str,JDE_end,1,1             &
                       ,IMS,IME,JMS,JME,KMS,KME                           &
                       ,ITS,min(ITE,IDE_end),JTS,min(JTE,JDE_end),KTS,KTE)

       if ( DOFILL .and. MYPROC .eq. 0 ) then
         polavg=.true.
         CALL global_fill(temp2,IDS_str,IDE_end,JDS_str,JDE_end,& 
                          u_var,v_var,e_w_average,polavg)
       endif

       if ( MYPROC .eq. 0) then
         nc = nc + 1
         call make_nemsio_array(TEMP2,TEMP1D, IDS_str, IDE_end, JDS_str, JDE_end)
         call nemsio_writerec(iunit_nemsio,nc,TEMP1D,iret=iret)
       endif

!--- MAXSNOWALB (nemsio only) -------------------------------------------------

       do J=JTS,min(JTE,JDE_end)
       do I=ITS,min(ITE,IDE_end)
         TEMP1(I,J)=0.01
       end do
       end do

       call make_global(TEMP1,TEMP2,MYPROC,npes,mpi_comm_world                &
                       ,IDS_str,IDE_end,JDS_str,JDE_end,1,1             &
                       ,IMS,IME,JMS,JME,KMS,KME                           &
                       ,ITS,min(ITE,IDE_end),JTS,min(JTE,JDE_end),KTS,KTE)

       if ( DOFILL .and. MYPROC .eq. 0 ) then
         polavg=.true.
         CALL global_fill(temp2,IDS_str,IDE_end,JDS_str,JDE_end,& 
                          u_var,v_var,e_w_average,polavg)
       endif

       if ( MYPROC .eq. 0) then
         nc = nc + 1
         call make_nemsio_array(TEMP2,TEMP1D, IDS_str, IDE_end, JDS_str, JDE_end)
         call nemsio_writerec(iunit_nemsio,nc,TEMP1D,iret=iret)
       endif

!--- DZSOIL, SLDPTH, PT -------------------------------------------------

      if (MYPROC .eq. 0 ) then

         write(iunit) grid%dzsoil   ! dzsoil
         NCOUNT=NCOUNT+1
         write(0,*) NCOUNT, ' DZSOIL  '

         write(iunit) grid%sldpth   ! sldpth
         NCOUNT=NCOUNT+1
         write(0,*) NCOUNT, ' SLDPTH  '

!         write(iunit) grid%pt
!         NCOUNT=NCOUNT+1
!         write(0,*) 'pt written as NCOUNT: ', NCOUNT

!	write(iunit) grid%i_parent_start_loc, grid%j_parent_start_loc
!         NCOUNT=NCOUNT+1

!        tmp_dlmd(1)=grid%dlmd
!        tmp_dphd(1)=grid%dphd
!        if (ndom .gt. 1) then
!        tmp_dlmd(ndom)=tmp_dlmd(grid%parent_id_out(ndom))/grid%parent_grid_ratio_out(ndom)
!        tmp_dphd(ndom)=tmp_dphd(grid%parent_id_out(ndom))/grid%parent_grid_ratio_out(ndom)
!	write(0,*) 'tmp_dlmd(1:ndom): ', tmp_dlmd(1:ndom)
!        endif

!	print*, ' ndom, xlong, xlat into tll: ', ndom, gridin%xlong_m(1+ITARG,1+JTARG),gridin%xlat_m(1+ITARG,1+JTARG)
!        call tll(gridin%xlong_m(1+ITARG,1+JTARG),gridin%xlat_m(1+ITARG,1+JTARG),wbd,sbd,grid%tph0d,grid%tlm0d)
!	print*, 'ndom, wbd, sbd: ', ndom, wbd, sbd

!	write(iunit) tmp_dlmd(ndom), tmp_dphd(ndom), wbd, sbd, grid%tlm0d, grid%tph0d
!         NCOUNT=NCOUNT+1

	write(0,*) 'writing parent_start fields: ', grid%i_parent_start_loc, grid%j_parent_start_loc
	write(0,*) 'child count'

	n_children=0

        if (max_dom .gt. 1) then
	do I=1,21
	if ( I .le. max_dom .and. grid%parent_id_out(I) .eq. ndom ) then
	n_children=n_children+1
	endif
	enddo
        endif

	write(0,*) 'n_children: ', n_children

       nest_config='configure_nest_details'//'_'//nchar
       dom_config='domain_details'//'_'//nchar

       if ( MYPROC .eq. 0 ) then
        open_unit3: do l=51,99
          inquire(l,opened=opened)
          if(.not.opened)then
            iunit3=l
            open(unit=iunit3,file=nest_config,form='formatted')
            exit open_unit3
          endif
        end do open_unit3

        open_unit4: do l=51,99
          inquire(l,opened=opened)
          if(.not.opened)then
            iunit4=l
            open(unit=iunit4,file=dom_config,form='formatted')
            exit open_unit4
          endif
        end do open_unit4

       endif

	line='my_domain_id: '//nchar

	write(0,*) 'line: ', line
        write(iunit3,636) line


	write(0,*) 'IDE-1: ', ide-1
        write (nchar4,635) (IDE-1)
	line='nx: ' //nchar4
        write(iunit4,636) line

	write(0,*) 'JDE-1: ', jde-1
        write (nchar4,635) JDE-1
	line='ny: ' //nchar4
        write(iunit4,636) line

	write(0,*) 'tmp_dlmd(ndom): ', tmp_dlmd(ndom)
        write (nchar8,637) tmp_dlmd(ndom)
	line='dlmd: ' //nchar8
        write(iunit4,636) line

	write(0,*) 'tmp_dphd(ndom): ', tmp_dphd(ndom)
        write (nchar8,637) tmp_dphd(ndom)
	line='dphd: ' //nchar8
        write(iunit4,636) line

        write (nchar8,638) grid%tph0d
	line='tph0d: ' //nchar8
        write(iunit4,636) line

        write (nchar8,638) grid%tlm0d
	line='tlm0d: ' //nchar8
        write(iunit4,636) line

	if (ndom .eq. 1) then

        write (nchar8,638) wbd
	line='wbd: ' //nchar8
        write(iunit4,636) line

        write (nchar8,638) sbd
	line='sbd: ' //nchar8
        write(iunit4,636) line

	else

        write (nchar8,638) wbd
	line='wbd(NEST): ' //nchar8
        write(iunit4,636) line

        write (nchar8,638) sbd
	line='sbd(NEST): ' //nchar8
        write(iunit4,636) line

	endif

	write(nchar8,638) swlat
	write(nchar8_2,638) swlon
	write(0,*) 'SW lat,lon: ', nchar8, nchar8_2
	line='SW lat,lon: ' //nchar8//' '//nchar8_2
        write(iunit4,636) line

	write(nchar8,638) nelat
	write(nchar8_2,638) nelon
	write(0,*) 'NE lat,lon: ', nchar8, nchar8_2
	line='NE lat,lon: ' //nchar8//' '//nchar8_2
        write(iunit4,636) line

	


        if (ndom .gt. 1) then
         write (nchar,633) grid%parent_id_out(ndom)
         line='my_parent_id: '//nchar
        else
         line='my_parent_id: -999'
        endif
        write(iunit3,636) line

	write(nchar,633) n_children
	line='n_children: '//nchar
        write(iunit3,636) line

	if (ndom .gt. 1) then

	write(nchar4,635)  grid%i_parent_start_loc
        line='i_parent_start: '//nchar4
        write(iunit3,636) line

	write(nchar4,635)  grid%j_parent_start_loc
        line='j_parent_start: '//nchar4
        write(iunit3,636) line

        write(nchar4, 635) grid%parent_grid_ratio_out(ndom)
!        line='child_parent_space_ratio: '//nchar4
        line='parent_child_space_ratio: '//nchar4
        write(iunit3,636) line

	line='input_ready: .TRUE.'
        write(iunit3,636) line

	endif

  636	format(A60)
	close(iunit3)
	close(iunit4)

  635	format(I4)
  637	format(f9.6)
  638	format(f8.3)

       endif

!--- GWD_OROG -----------------------------------------------------------

       IF (do_gwd) THEN

       if ( MYPROC .eq. 0 ) then
        open_unit2: do l=51,99
          inquire(l,opened=opened)

          if(.not.opened)then
            iunit2=l
            write (nchar,633) ndom
            flname='GWD_bin_'//nchar
            open(unit=iunit2,file=flname,form='unformatted')
            write(0,*) 'opening with iunit2: ', iunit2
            exit open_unit2
          endif

        end do open_unit2
       endif

       do L=1,14

       do J=JTS,min(JTE,JDE_end)
       do I=ITS,min(ITE,IDE_end)
         TEMP1(I,J)=gridin%GWD_OROG(I+ITARG,J+JTARG,L)
       end do
       end do

       call make_global(TEMP1,TEMP2,MYPROC,npes,mpi_comm_world                &
                       ,IDS_str,IDE_end,JDS_str,JDE_end,1,1             &
                       ,IMS,IME,JMS,JME,KMS,KME                           &
                       ,ITS,min(ITE,IDE_end),JTS,min(JTE,JDE_end),KTS,KTE)

       if (print_diag) write(0,*) 'min,max, GWD_OROG(L): ', L, minval(TEMP2),maxval(TEMP2)

       u_var=.false.
       v_var=.false.

       if ( DOFILL .and. MYPROC .eq. 0 ) then
         polavg=.true.
         CALL global_fill(temp2,IDS_str,IDE_end,JDS_str,JDE_end,& 
                          u_var,v_var,e_w_average,polavg)
       endif

       if (MYPROC .eq. 0 ) then
          write(iunit) TEMP2   ! GWD_OROG
          write(iunit2) TEMP2   ! GWD_OROG (GWD.bin file)
          NCOUNT=NCOUNT+1
          write(0,*) NCOUNT, ' GWD_OROG '

         nc = nc + 1
         call make_nemsio_array(TEMP2,TEMP1D, IDS_str, IDE_end, JDS_str, JDE_end)
         call nemsio_writerec(iunit_nemsio,nc,TEMP1D,iret=iret)
       endif

       end do

       if (MYPROC .eq. 0 ) then
          close(iunit2)
       endif

       ENDIF

!---------------------------------------------------------------------------

       if ( MYPROC .eq. 0 ) then
         close(iunit)

         call nemsio_close(iunit_nemsio,iret=iret)
         if (iret/=0) then
           write(0,*)' nemsio_close error ', iret
#ifdef _MPI
           CALL mpi_abort(MPI_COMM_WORLD,1,ierr)
#else
           STOP
#endif
         endif
         call nemsio_finalize()
       endif


!---------------------------------------------------------------------------

        DEALLOCATE(sgm,sg1,dsg1,sgml1,sg2,dsg2,sgml2)
        DEALLOCATE(TEMP1,ITEMP,TEMPSOIL)
        DEALLOCATE(TEMP2,ITEMP2,TEMPSOIL2)
	if (MYPROC .eq. 0) DEALLOCATE(PSFC)

        RETURN

  END SUBROUTINE write_nemsinit

  SUBROUTINE global_fill(array,iglb_s,iglb_e,jglb_s,jglb_e,u_var,v_var,e_w_average,polavg)
    IMPLICIT NONE
    INTEGER:: iglb_s,iglb_e,jglb_s,jglb_e, I,J, IND, IADD
    REAL:: array(iglb_s:iglb_e,jglb_s:jglb_e), avg, AS,AN,rcycle
    LOGICAL:: u_var,v_var,e_w_average,polavg

!   Assumed that currently everything is defined over iglb_s+1 --> iglb_e-1, and same in J direction

!   WEST/EAST boundaries

    DO J=jglb_s+1,jglb_e-1
      array(iglb_s,J)=array(iglb_e-2,J)
      array(iglb_e,J)=array(iglb_s+2,J) 

! believe this would be problematic with fields such as SM, but useful for mass type variables
        if (e_w_average) then
      avg=0.5*(array(iglb_e-1,J)+array(2,J))
      array(iglb_s+1,J)=avg         ! average the duplicated column
      array(iglb_e-1,J)=avg  ! average the duplicated column
        endif ! e-w average

    END DO

!   SOUTH boundary
!

!!
!!  New logic added 13 August 2007 to average latitudinal circles
!!

        if (polavg) then

    rcycle=1./(iglb_e-3)
    AS=0.
    AN=0.

    DO I=iglb_s+1,iglb_e-1
      AS = AS + array(i, 2)
      AN = AN + array(i, jglb_e-1)
    ENDDO

    AS=AS*rcycle
    AN=AN*rcycle

        write(0,*) 'resulting S bound average:: ', AS
        write(0,*) 'resulting N bound average:: ', AN

    DO I=iglb_s,iglb_e
      array(i, 2       ) = AS
      array(i, jglb_e-1) = AN
    ENDDO

       endif

!!!
!!! End 13 August 2007 addition
!!!



!!!
!!! Bug fix:  u_var added a negative sign to the definition (2007/08/13)
!!! Bug fix:  I index modified, addition of IND index (2007/08/13)
!!!
!!! Bug fix:  IND dimension was going way out of bounds (2009/05/05)
!!!
    IADD=(IGLB_E-3)/2

    DO I=iglb_s,iglb_e
    IND=I+IADD
    if (IND>IGLB_E) IND=IND-IGLB_E+3
      if (u_var) then
         array(i,jglb_s) = -array(IND,jglb_s+1)
      elseif (v_var) then
         array(i,jglb_s) = -array(IND,jglb_s+1)
      else
        if (polavg) then
         array(i,jglb_s) =  array(IND,jglb_s+2)
        else
         array(i,jglb_s) =  array(I,jglb_s+2)
        endif
      endif
    END DO

!   NORTH boundary
!
    DO I=iglb_s,iglb_e
    IND=I+IADD
    if (IND>IGLB_E) IND=IND-IGLB_E+3

      if (u_var) then
         array(i,jglb_e-1) =  -array(IND,jglb_e-2)
         array(i,jglb_e)   =  -array(IND,jglb_e-2)
      elseif (v_var) then
         array(i,jglb_e-1) = -array(IND,jglb_e-2)
         array(i,jglb_e)   = -array(IND,jglb_e-2)
      else
        if (polavg) then
         array(i,jglb_e)=array(IND,jglb_e-2)
        else
         array(i,jglb_e)=array(I,jglb_e-2)
        endif
      endif
    END DO

  END SUBROUTINE global_fill

! ---------------------------------------------------------------------


!-------------------------------------------------------------------------------

  SUBROUTINE make_nemsio_array(TEMP2,TEMP2_NEMSIO, IDS_str, IDE_end, JDS_str, JDE_end)

    INTEGER :: IJ, IDS_str,IDE_end, JDS_str,JDE_end

    REAL, INTENT(IN) :: TEMP2(IDS_str:IDE_end,JDS_str:JDE_end)
    REAL, INTENT(OUT) :: TEMP2_NEMSIO( (IDE_end-IDS_str+1)*(JDE_end-JDS_str+1) )

    IJ=0
    do J=JDS_str,JDE_end
    do I=IDS_str,IDE_end
        IJ=IJ+1
        TEMP2_NEMSIO(IJ)=TEMP2(I,J)
    ENDDO
    ENDDO

  END SUBROUTINE make_nemsio_array

!-------------------------------------------------------------------------------

   subroutine tll(almd,aphd,tlmd,tphd,tph0d,tlm0d)
!-------------------------------------------------------------------------------
      real, intent(in) :: almd, aphd
      real, intent(out) :: tlmd, tphd
      real, intent(in) :: tph0d, tlm0d
!-------------------------------------------------------------------------------
      real, parameter :: pi=3.141592654
      real, parameter :: dtr=pi/180.0
!
      real :: tph0, ctph0, stph0, relm, srlm, crlm
      real :: aph, sph, cph, cc, anum, denom
!-------------------------------------------------------------------------------
!
      if (tlm0d==0.0.and.tph0d==0.0) then
      tlmd=almd
      tphd=aphd
      else

      tph0=tph0d*dtr
      ctph0=cos(tph0)
      stph0=sin(tph0)
!
      relm=(almd-tlm0d)*dtr
      srlm=sin(relm)
      crlm=cos(relm)
      aph=aphd*dtr
      sph=sin(aph)
      cph=cos(aph)
      cc=cph*crlm
      anum=cph*srlm
      denom=ctph0*cc+stph0*sph
!
      tlmd=atan2(anum,denom)/dtr
      tphd=asin(ctph0*sph-stph0*cc)/dtr

      end if
!
      return
!
   end subroutine tll
END MODULE output_nemsinit
