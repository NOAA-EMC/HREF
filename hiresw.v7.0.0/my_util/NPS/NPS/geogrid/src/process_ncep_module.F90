!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Module: process_ncep_module
!
! Description:
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module process_ncep_module

   use module_debug

   contains

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: process_ncep
   !
   ! Purpose: Replaces geogrid innards by simply reading the contents of
   !          gridgen_sfc GRIB file output, and writing to slightly different
   !          geogrid output file (SOILCAT and VEGCAT as dominant types rather
   !          than fractions in each category)
   !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   subroutine process_ncep(which_domain,grid_type,domain_name,end_dom_i,end_dom_j, &
                           end_file_i,end_file_j,top_i_parent_start,top_j_parent_start)

!      use llxy_module
      use misc_definitions_module
      use output_module
      use hash_module
!      use smooth_module
      use source_data_module
      use grib_mod
      use gridinfo_module, only: use_igbp

      implicit none

      ! Arguments
      integer, intent(in) :: which_domain
      character (len=1), intent(in) :: grid_type
      character (len=*), intent(in) :: domain_name
      integer, intent(in) :: end_dom_i, end_dom_j
      integer, intent(in) :: end_file_i, end_file_j
      integer, intent(in) :: top_i_parent_start, top_j_parent_start

      ! Local variables
      integer :: i, j, k, istatus, ifieldstatus, idomcatstatus, field_count
      integer :: ierr, N, landusef_dim

      integer :: start_dom_i, start_dom_j, end_dom_stag_i, end_dom_stag_j
      integer :: start_patch_i, end_patch_i, start_patch_j, end_patch_j, end_patch_stag_i, end_patch_stag_j
      integer :: start_mem_i, end_mem_i, start_mem_j, end_mem_j, end_mem_stag_i, end_mem_stag_j
      integer :: sm1, em1, sm2, em2
      integer :: dim1size,dim2size,dim3size

      real, allocatable :: dum2d(:,:,:), dum3d(:,:,:), xlat_array(:,:), xlon_array(:,:)
      real, allocatable :: xlat_array_v(:,:), xlon_array_v(:,:)

      real(KIND=8), allocatable :: dum2d_8(:,:), dum3d_8(:,:,:)

      real :: sum, dominant, msg_fill_val, topo_flag_val, mass_flag
      real, dimension(16) :: corner_lats, corner_lons
      logical :: extra_col, extra_row

      character (len=19) :: datestr
      character (len=128) :: fieldname, gradname, domname, landmask_name,filename

!!! GRIB1 RELATED
      integer:: JPDS(200),JGDS(200),IRET1,KNUM,NWORDS
      integer:: KPDS1(200),KGDS(200),KPDS2(200),KPDS(200),KGDS2(200)
      logical, allocatable:: BITMAP(:)

!!! GRIB2 RELATED
      
      type(gribfield) :: gfld
      integer, dimension(200) :: JIDS,JPDT,JGDT


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      write(0,*)' in process_ncep : ',which_domain,grid_type,domain_name
      write(0,*)' in process_ncep : ',end_dom_i,end_dom_j,end_file_i,end_file_j,top_i_parent_start,top_j_parent_start

      fieldname = '                                                                                                                               '
      datestr = '0000-00-00_00:00:00'
      field_count = 0
      mass_flag=1.0

      extra_row=.false.
      extra_col=.false.

!        print*, 'end_dom_i: ', end_dom_i
!        print*, 'end_dom_j: ', end_dom_j

      allocate(dum2d(end_dom_i,end_dom_j,1))

      start_dom_i=1
      start_dom_j=1

      start_patch_i=start_dom_i
      start_patch_j=start_dom_j
      end_patch_i=end_dom_i
      end_patch_j=end_dom_j

      start_mem_i=start_dom_i
      start_mem_j=start_dom_j
      end_mem_i=end_dom_i
      end_mem_j=end_dom_j

!        print*, 'start_mem_i, end_mem_i: ', start_mem_i, end_mem_i

      write(0,*) 'in process_ncep_module with ncep_proc_grib2: ',ncep_proc_grib2

      dyn_opt=4

      ALLOCATE(BITMAP(end_file_i*end_file_j))

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!  READ/WRITE HLAT
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      filename=trim(domain_name)//'_hpnt_latitudes.grb'
      if (ncep_proc_grib2) filename=trim(filename)//'2'

      fieldname='XLAT_M'

      call baopenr(23,trim(filename),ierr)
      if (ierr .ne. 0) then
        write(0,*)' ierr .ne. 0 for baopenr ',trim(filename), ierr
         stop
      end if

      allocate(xlat_array(end_mem_i, end_mem_j))
      allocate(dum2d_8(end_file_i, end_file_j))
      xlat_array=0.

      if (.not.ncep_proc_grib2) then
        jpds=-1
        jgds=-1
        call getgb(23,0,end_file_i*end_file_j,0,JPDS,JGDS,nwords,KNUM,KPDS,KGDS, &
                   BITMAP,dum2d_8,IRET1)
      else
        JIDS=-9999
        JPDT=-9999
        JGDT=-9999
        call getgb2(23,23,0,-1,JIDS,-1,JPDT,-1,JGDT,.TRUE.,K,GFLD,IRET1)
        if (IRET1.eq.0) then
          if (end_file_i*end_file_j .ne. GFLD%ndpts ) then
             write(0,*)' end_file_i*end_file_j .ne. GFLD%ndpts ',trim(filename)
             write(0,*)  end_file_i,end_file_j,     GFLD%ndpts
             stop
          end if
          dum2d_8 = reshape(GFLD%fld, (/ end_file_i, end_file_j /))
        end if
        call g2_free(gfld)
      end if

      if (IRET1 .eq. 0) then
        print*,  'XLAT_M extremes (dum2d_8): ', minval(dum2d_8),maxval(dum2d_8)
        do j=1,end_mem_j
        do i=1,end_mem_i
          xlat_array(I,J)=dum2d_8(I+top_i_parent_start-1,J+top_j_parent_start-1)
          if (xlat_array(I,J) .ge. 90.0) then
          write(0,*) 'set XLAT_M to 90 at I,J from: ', I,J, xlat_array(I,J)
          xlat_array(I,J)=90.0
          endif
        enddo
        enddo
        print*,  'XLAT_M extremes (xlat_array): ', minval(xlat_array),maxval(xlat_array)
      else
        write(0,*)' IRET1 .ne. 0 for ',trim(filename), IRET1
        stop
      endif

      if (abs(maxval(xlat_array)) .gt. 90.) then
        write(0,*) 'quit due to weird xlat value 32/64 bit problem?'
        call mpi_abort(mpi_comm_world, 1, ierr)
      endif

      call baclose(23,ierr)
      if (ierr .ne. 0) then
        write(0,*)' ierr .ne. 0 for baclose ',trim(filename), ierr
         stop
      end if

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!  READ/WRITE HLON
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      filename=trim(domain_name)//'_hpnt_longitudes.grb'
      if (ncep_proc_grib2) filename=trim(filename)//'2'

      fieldname='XLON_M'

      call baopenr(24,trim(filename),ierr)
      if (ierr .ne. 0) then
         write(0,*)' ierr .ne. 0 for baopenr ',trim(filename), ierr
         stop
      end if

      allocate(xlon_array(end_mem_i, end_mem_j))
      xlon_array=0.

      if (.not.ncep_proc_grib2) then
        jpds=-1
        jgds=-1
        call getgb(24,0,end_file_i*end_dom_j,0,JPDS,JGDS,nwords,KNUM,KPDS,KGDS, &
                   BITMAP,dum2d_8,IRET1)
      else
        JIDS=-9999
        JPDT=-9999
        JGDT=-9999
        call getgb2(24,24,0,-1,JIDS,-1,JPDT,-1,JGDT,.TRUE.,K,GFLD,IRET1)
        if (IRET1.eq.0) then
          if (end_file_i*end_file_j .ne. GFLD%ndpts ) then
             write(0,*)' end_file_i*end_file_j .ne. GFLD%ndpts ',trim(filename)
             write(0,*)  end_file_i,end_file_j,     GFLD%ndpts
             stop
          end if
          dum2d_8 = reshape(GFLD%fld, (/ end_file_i, end_file_j /))

          do j=1,end_file_j
          do i=1,end_file_i
            dum2d_8(I,J)=-(360.-dum2d_8(I,J))
            if (dum2d_8(I,J) .lt. -180.) dum2d_8(I,J)=dum2d_8(I,J)+360.
            if (dum2d_8(I,J) .gt. 180.) dum2d_8(I,J)=dum2d_8(I,J)-360.
          enddo
          enddo

        end if
        call g2_free(gfld)
      end if

      if (IRET1 .eq. 0) then
        do j=1,end_mem_j
        do i=1,end_mem_i
          xlon_array(I,J)=dum2d_8(I+top_i_parent_start-1,J+top_j_parent_start-1)
        enddo
        enddo
        print*, 'XLON_M extremes: ', minval(xlon_array),maxval(xlon_array)
      else
        write(0,*)' IRET1 .ne. 0 for ',trim(filename), IRET1
        stop
      endif

      call baclose(24,ierr)
      if (ierr .ne. 0) then
        write(0,*)' ierr .ne. 0 for baclose ',trim(filename), ierr
         stop
      end if

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!  READ/WRITE VLAT
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      filename=trim(domain_name)//'_vpnt_latitudes.grb'
      if (ncep_proc_grib2) filename=trim(filename)//'2'

      fieldname='XLAT_V'

      call baopenr(25,trim(filename),ierr)
      if (ierr .ne. 0) then
         write(0,*)' ierr .ne. 0 for baopenr ',trim(filename), ierr
         stop
      end if

      allocate(xlat_array_v(end_mem_i, end_mem_j))
      xlat_array_v=0.

      if (.not.ncep_proc_grib2) then
        jpds=-1
        jgds=-1
        call getgb(25,0,end_file_i*end_dom_j,0,JPDS,JGDS,nwords,KNUM,KPDS,KGDS, &
                   BITMAP,dum2d_8,IRET1)
      else
        JIDS=-9999
        JPDT=-9999
        JGDT=-9999
        call getgb2(25,25,0,-1,JIDS,-1,JPDT,-1,JGDT,.TRUE.,K,GFLD,IRET1)
        if (IRET1.eq.0) then
          if (end_file_i*end_file_j .ne. GFLD%ndpts ) then
             write(0,*)' end_file_i*end_file_j .ne. GFLD%ndpts ',trim(filename)
             write(0,*)  end_file_i,end_file_j,     GFLD%ndpts
             stop
          end if
          dum2d_8 = reshape(GFLD%fld, (/ end_file_i, end_file_j /))
        end if
        call g2_free(gfld)
      end if

      if (IRET1 .eq. 0) then
        do j=1,end_mem_j
        do i=1,end_mem_i
          xlat_array_v(I,J)=dum2d_8(I+top_i_parent_start-1,J+top_j_parent_start-1)

          if (xlat_array_v(I,J) .ge. 90.0) then
          write(0,*) 'set XLAT_V to 90 at I,J from: ', I,J, xlat_array_v(I,J)
          xlat_array_v(I,J)=90.0
          endif

        enddo
        enddo
        print*, 'XLAT_V extremes: ', minval(xlat_array_v),maxval(xlat_array_v)
      else
        write(0,*)' IRET1 .ne. 0 for ',trim(filename), IRET1
        stop
      endif

      call baclose(25,ierr)
      if (ierr .ne. 0) then
        write(0,*)' ierr .ne. 0 for baclose ',trim(filename), ierr
         stop
      end if

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!  READ/WRITE VLON
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      filename=trim(domain_name)//'_vpnt_longitudes.grb'
      if (ncep_proc_grib2) filename=trim(filename)//'2'

      fieldname='XLON_V'

      call baopenr(26,trim(filename),ierr)
      if (ierr .ne. 0) then
         write(0,*)' ierr .ne. 0 for baopenr ',trim(filename), ierr
         stop
      end if

      allocate(xlon_array_v(end_mem_i, end_mem_j))
      xlon_array_v=0.

      if (.not.ncep_proc_grib2) then
        jpds=-1
        jgds=-1
        call getgb(26,0,end_file_i*end_dom_j,0,JPDS,JGDS,nwords,KNUM,KPDS,KGDS, &
                   BITMAP,dum2d_8,IRET1)
      else
        JIDS=-9999
        JPDT=-9999
        JGDT=-9999
        call getgb2(26,26,0,-1,JIDS,-1,JPDT,-1,JGDT,.TRUE.,K,GFLD,IRET1)
        if (IRET1.eq.0) then
          if (end_file_i*end_file_j .ne. GFLD%ndpts ) then
             write(0,*)' end_file_i*end_file_j .ne. GFLD%ndpts ',trim(filename)
             write(0,*)  end_file_i,end_file_j,     GFLD%ndpts
             stop
          end if
          dum2d_8 = reshape(GFLD%fld, (/ end_file_i, end_file_j /))

          do j=1,end_file_j
          do i=1,end_file_i
            dum2d_8(I,J)=-(360.-dum2d_8(I,J))
            if (dum2d_8(I,J) .lt. -180.) dum2d_8(I,J)=dum2d_8(I,J)+360.
            if (dum2d_8(I,J) .gt. 180.) dum2d_8(I,J)=dum2d_8(I,J)-360.
          enddo
          enddo
        end if
        call g2_free(gfld)
      end if

      if (IRET1 .eq. 0) then
        do j=1,end_mem_j
        do i=1,end_mem_i
          xlon_array_v(I,J)=dum2d_8(I+top_i_parent_start-1,J+top_j_parent_start-1)
        enddo
        enddo
        write(0,*) 'XLON_V extremes: ', minval(xlon_array_v),maxval(xlon_array_v)
      else
        write(0,*)' IRET1 .ne. 0 for ',trim(filename), IRET1
        stop
      endif

      call baclose(26,ierr)
      if (ierr .ne. 0) then
        write(0,*)' ierr .ne. 0 for baclose ',trim(filename), ierr
         stop
      end if

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      corner_lats(1) = xlat_array(start_patch_i,start_patch_j)
      corner_lats(2) = xlat_array(start_patch_i,end_patch_j)
      corner_lats(3) = xlat_array(end_patch_i,end_patch_j)
      corner_lats(4) = xlat_array(end_patch_i,start_patch_j)

      corner_lats(5) = xlat_array_v(start_patch_i,start_patch_j)
      corner_lats(6) = xlat_array_v(start_patch_i,end_patch_j)
      corner_lats(7) = xlat_array_v(end_patch_i,end_patch_j)
      corner_lats(8) = xlat_array_v(end_patch_i,start_patch_j)

      corner_lats(9)  = 0.0
      corner_lats(10) = 0.0
      corner_lats(11) = 0.0
      corner_lats(12) = 0.0

      corner_lats(13) = 0.0
      corner_lats(14) = 0.0
      corner_lats(15) = 0.0
      corner_lats(16) = 0.0

      corner_lons(1) = xlon_array(start_patch_i,start_patch_j)
      corner_lons(2) = xlon_array(start_patch_i,end_patch_j)
      corner_lons(3) = xlon_array(end_patch_i,end_patch_j)
      corner_lons(4) = xlon_array(end_patch_i,start_patch_j)

      corner_lons(5) = xlon_array_v(start_patch_i,start_patch_j)
      corner_lons(6) = xlon_array_v(start_patch_i,end_patch_j)
      corner_lons(7) = xlon_array_v(end_patch_i,end_patch_j)
      corner_lons(8) = xlon_array_v(end_patch_i,start_patch_j)

      corner_lons(9)  = 0.0
      corner_lons(10) = 0.0
      corner_lons(11) = 0.0
      corner_lons(12) = 0.0

      corner_lons(13) = 0.0
      corner_lons(14) = 0.0
      corner_lons(15) = 0.0
      corner_lons(16) = 0.0

      print*, 'SW corner point: ', corner_lats(1),corner_lons(1)
      print*, 'NE corner point: ', corner_lats(3),corner_lons(3)


! Initialize the output module now that we have the corner point lats/lons


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



      call output_init(which_domain, 'OUTPUT FROM GRIDGEN', '0000-00-00_00:00:00', grid_type, dyn_opt, &
                       corner_lats, corner_lons, &
                       start_dom_i,   end_dom_i,   start_dom_j,   end_dom_j, &
                       start_patch_i, end_patch_i, start_patch_j, end_patch_j, &
                       start_mem_i,   end_mem_i,   start_mem_j,   end_mem_j, &
                       extra_col, extra_row)

      print*, ' '
      print*, ' '
      print*, ' '
      print*, ' '

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      call write_field(start_mem_i, end_mem_i, start_mem_j, end_mem_j, 1, 1, &
                       'XLAT_M', datestr, real_array = xlat_array)
      call write_field(start_mem_i, end_mem_i, start_mem_j, end_mem_j, 1, 1, &
                       'XLONG_M', datestr, real_array = xlon_array)
      call write_field(start_mem_i, end_mem_i, start_mem_j, end_mem_j, 1, 1, &
                       'XLAT_V', datestr, real_array = xlat_array_v)
      call write_field(start_mem_i, end_mem_i, start_mem_j, end_mem_j, 1, 1, &
                       'XLONG_V', datestr, real_array = xlon_array_v)

      deallocate(xlat_array, xlon_array)
      deallocate(xlat_array_v, xlon_array_v)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!  READ/WRITE HGT_M
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      filename=trim(domain_name)//'_elevtiles.grb'
      if (ncep_proc_grib2) then
        filename=trim(filename)//'2'
      end if

      fieldname='HGT_M'

      call baopenr(27,trim(filename),ierr)
      if (ierr .ne. 0) then
         write(0,*)' ierr .ne. 0 for baopenr ',trim(filename), ierr
         stop
      end if

      dum2d=0.

      if (.not.ncep_proc_grib2) then
        jpds=-1
        jgds=-1

        jpds(5)=8
        jpds(6)=1
        jpds(7)=0

        call getgb(27,0,end_file_i*end_file_j,0,JPDS,JGDS,nwords,KNUM,KPDS,KGDS, &
                   BITMAP,dum2d_8,IRET1)
      else
        JIDS=-9999
        JPDT=-9999
        JGDT=-9999
        JPDT(2)=7
        call getgb2(27,27,0,-1,JIDS,0,JPDT,-1,JGDT,.TRUE.,K,GFLD,IRET1)
        if (IRET1.eq.0) then
          dum2d_8 = reshape(GFLD%fld, (/ end_file_i, end_file_j /))
        end if
        call g2_free(gfld)
      end if
      print*, 'IRET1 from getgb for HGT_M: ', IRET1

      if (IRET1 .eq. 0) then

        do j=1,end_mem_j
        do i=1,end_mem_i
          dum2d(I,J,1)=dum2d_8(I+top_i_parent_start-1,J+top_j_parent_start-1)
        enddo
        enddo

        print*, ' to hgt_m print'

        do J=end_mem_j,1,-end_mem_j/25
        print 909, (dum2d(I,J,1),I=1,end_mem_i,end_mem_i/20)
        enddo
  909   format(25(f5.0,1x))

        print*, 'minval, maxval(HGT_M): ', minval(dum2d),maxval(dum2d)
        call write_field(start_mem_i, end_mem_i, start_mem_j, end_mem_j, 1, 1, &
                         fieldname, datestr, dum2d)
      endif

! ----------------------------------------------------------------

  GWD  :  if (do_gwd) then

! - 1 ---------------------------------------------------------------

      fieldname='HGTSTDV'

      if (.not.ncep_proc_grib2) then
        jpds=-1
        jgds=-1
        jpds(5)=9
        call getgb(27,0,end_file_i*end_file_j,0,JPDS,JGDS,nwords,KNUM,KPDS,KGDS, &
                   BITMAP,dum2d_8,IRET1)
      else
        JIDS=-9999
        JPDT=-9999
        JGDT=-9999
        JPDT(2)=9
        call getgb2(27,27,0,-1,JIDS,0,JPDT,-1,JGDT,.TRUE.,K,GFLD,IRET1)
        if (IRET1.eq.0) then
          dum2d_8 = reshape(GFLD%fld, (/ end_file_i, end_file_j /))
        end if
        call g2_free(gfld)
      end if

      print*, 'IRET1 for HGTSTDV : ', IRET1
      if (IRET1 .eq. 0) then
        do j=1,end_mem_j
        do i=1,end_mem_i
          dum2d(I,J,1)=dum2d_8(I+top_i_parent_start-1,J+top_j_parent_start-1)
        enddo
        enddo
        print*, 'minval, maxval(HGTSTDV): ', minval(dum2d),maxval(dum2d)
        call write_field(start_mem_i, end_mem_i, start_mem_j, end_mem_j, 1, 1, &
                         fieldname, datestr, dum2d)
      endif

! - 2 ---------------------------------------------------------------

      fieldname='HGTCNVX'

      if (.not.ncep_proc_grib2) then
        jpds=-1
        jgds=-1
        jpds(5)=187
        call getgb(27,0,end_file_i*end_file_j,0,JPDS,JGDS,nwords,KNUM,KPDS,KGDS, &
                   BITMAP,dum2d_8,IRET1)
      else
        JIDS=-9999
        JPDT=-9999
        JGDT=-9999
        JPDT(2)=187
        call getgb2(27,27,0,-1,JIDS,0,JPDT,-1,JGDT,.TRUE.,K,GFLD,IRET1)
        if (IRET1.eq.0) then
          dum2d_8 = reshape(GFLD%fld, (/ end_file_i, end_file_j /))
        end if
        call g2_free(gfld)
      end if

      if (IRET1 .eq. 0) then
        do j=1,end_mem_j
        do i=1,end_mem_i
          dum2d(I,J,1)=dum2d_8(I+top_i_parent_start-1,J+top_j_parent_start-1)
        enddo
        enddo
        print*, 'minval, maxval(HGTCNVX): ', minval(dum2d),maxval(dum2d)
        call write_field(start_mem_i, end_mem_i, start_mem_j, end_mem_j, 1, 1, &
                         fieldname, datestr, dum2d)
       endif

! - 3 ---------------------------------------------------------------

      fieldname='HGTOA1'

      if (.not.ncep_proc_grib2) then
        jpds=-1
        jgds=-1
        jpds(5)=166
        call getgb(27,0,end_file_i*end_file_j,0,JPDS,JGDS,nwords,KNUM,KPDS,KGDS, &
                   BITMAP,dum2d_8,IRET1)
      else
        JIDS=-9999
        JPDT=-9999
        JGDT=-9999
        JPDT(2)=166
        call getgb2(27,27,0,-1,JIDS,0,JPDT,-1,JGDT,.TRUE.,K,GFLD,IRET1)
        if (IRET1.eq.0) then
          dum2d_8 = reshape(GFLD%fld, (/ end_file_i, end_file_j /))
        end if
        call g2_free(gfld)
      end if

      if (IRET1 .eq. 0) then
        do j=1,end_mem_j
        do i=1,end_mem_i
          dum2d(I,J,1)=dum2d_8(I+top_i_parent_start-1,J+top_j_parent_start-1)
        enddo
        enddo
        print*, 'minval, maxval(HGTOA1): ', minval(dum2d),maxval(dum2d)
        call write_field(start_mem_i, end_mem_i, start_mem_j, end_mem_j, 1, 1, &
                       fieldname, datestr, dum2d)
      endif

! - 4 ---------------------------------------------------------------

      fieldname='HGTOA2'

      if (.not.ncep_proc_grib2) then
        jpds=-1
        jgds=-1
        jpds(5)=167
        call getgb(27,0,end_file_i*end_file_j,0,JPDS,JGDS,nwords,KNUM,KPDS,KGDS, &
                   BITMAP,dum2d_8,IRET1)
      else
        JIDS=-9999
        JPDT=-9999
        JGDT=-9999
        JPDT(2)=167
        call getgb2(27,27,0,-1,JIDS,0,JPDT,-1,JGDT,.TRUE.,K,GFLD,IRET1)
        if (IRET1.eq.0) then
          dum2d_8 = reshape(GFLD%fld, (/ end_file_i, end_file_j /))
        end if
        call g2_free(gfld)
      end if

      if (IRET1 .eq. 0) then
        do j=1,end_mem_j
        do i=1,end_mem_i
          dum2d(I,J,1)=dum2d_8(I+top_i_parent_start-1,J+top_j_parent_start-1)
        enddo
        enddo
        print*, 'minval, maxval(HGTOA2): ', minval(dum2d),maxval(dum2d)
        call write_field(start_mem_i, end_mem_i, start_mem_j, end_mem_j, 1, 1, &
                       fieldname, datestr, dum2d)
      endif

! - 5 ---------------------------------------------------------------

      fieldname='HGTOA3'
      if (.not.ncep_proc_grib2) then
        jpds=-1
        jgds=-1
        jpds(5)=168
        call getgb(27,0,end_file_i*end_file_j,0,JPDS,JGDS,nwords,KNUM,KPDS,KGDS, &
                   BITMAP,dum2d_8,IRET1)
      else
        JIDS=-9999
        JPDT=-9999
        JGDT=-9999
        JPDT(2)=168
        call getgb2(27,27,0,-1,JIDS,0,JPDT,-1,JGDT,.TRUE.,K,GFLD,IRET1)
        if (IRET1.eq.0) then
          dum2d_8 = reshape(GFLD%fld, (/ end_file_i, end_file_j /))
        end if
        call g2_free(gfld)
      end if

      if (IRET1 .eq. 0) then
        do j=1,end_mem_j
        do i=1,end_mem_i
          dum2d(I,J,1)=dum2d_8(I+top_i_parent_start-1,J+top_j_parent_start-1)
        enddo
        enddo
        print*, 'minval, maxval(HGTOA3): ', minval(dum2d),maxval(dum2d)
        call write_field(start_mem_i, end_mem_i, start_mem_j, end_mem_j, 1, 1, &
                       fieldname, datestr, dum2d)
      endif

! - 6 ---------------------------------------------------------------

      fieldname='HGTOA4'
      if (.not.ncep_proc_grib2) then
        jpds=-1
        jgds=-1
        jpds(5)=169
        call getgb(27,0,end_file_i*end_file_j,0,JPDS,JGDS,nwords,KNUM,KPDS,KGDS, &
                   BITMAP,dum2d_8,IRET1)
      else
        JIDS=-9999
        JPDT=-9999
        JGDT=-9999
        JPDT(2)=169
        call getgb2(27,27,0,-1,JIDS,0,JPDT,-1,JGDT,.TRUE.,K,GFLD,IRET1)
        if (IRET1.eq.0) then
          dum2d_8 = reshape(GFLD%fld, (/ end_file_i, end_file_j /))
        end if
        call g2_free(gfld)
      end if

      if (IRET1 .eq. 0) then
        do j=1,end_mem_j
        do i=1,end_mem_i
          dum2d(I,J,1)=dum2d_8(I+top_i_parent_start-1,J+top_j_parent_start-1)
        enddo
        enddo
        print*, 'minval, maxval(HGTOA4): ', minval(dum2d),maxval(dum2d)
        call write_field(start_mem_i, end_mem_i, start_mem_j, end_mem_j, 1, 1, &
                       fieldname, datestr, dum2d)
      endif

! - 7 ---------------------------------------------------------------

      fieldname='HGTOL1'
      if (.not.ncep_proc_grib2) then
        jpds=-1
        jgds=-1
        jpds(5)=151
        call getgb(27,0,end_file_i*end_file_j,0,JPDS,JGDS,nwords,KNUM,KPDS,KGDS, &
                   BITMAP,dum2d_8,IRET1)
      else
        JIDS=-9999
        JPDT=-9999
        JGDT=-9999
        JPDT(2)=151
        call getgb2(27,27,0,-1,JIDS,0,JPDT,-1,JGDT,.TRUE.,K,GFLD,IRET1)
        if (IRET1.eq.0) then
          dum2d_8 = reshape(GFLD%fld, (/ end_file_i, end_file_j /))
        end if
        call g2_free(gfld)
      end if

      if (IRET1 .eq. 0) then
        do j=1,end_mem_j
        do i=1,end_mem_i
          dum2d(I,J,1)=dum2d_8(I+top_i_parent_start-1,J+top_j_parent_start-1)
        enddo
        enddo
        print*, 'minval, maxval(HGTOL1): ', minval(dum2d),maxval(dum2d)
        call write_field(start_mem_i, end_mem_i, start_mem_j, end_mem_j, 1, 1, &
                       fieldname, datestr, dum2d)
      endif

! - 8 ---------------------------------------------------------------

      fieldname='HGTOL2'
      if (.not.ncep_proc_grib2) then
        jpds=-1
        jgds=-1
        jpds(5)=152
        call getgb(27,0,end_file_i*end_file_j,0,JPDS,JGDS,nwords,KNUM,KPDS,KGDS, &
                   BITMAP,dum2d_8,IRET1)
      else
        JIDS=-9999
        JPDT=-9999
        JGDT=-9999
        JPDT(2)=152
        call getgb2(27,27,0,-1,JIDS,0,JPDT,-1,JGDT,.TRUE.,K,GFLD,IRET1)
        if (IRET1.eq.0) then
          dum2d_8 = reshape(GFLD%fld, (/ end_file_i, end_file_j /))
        end if
        call g2_free(gfld)
      end if

      if (IRET1 .eq. 0) then
        do j=1,end_mem_j
        do i=1,end_mem_i
          dum2d(I,J,1)=dum2d_8(I+top_i_parent_start-1,J+top_j_parent_start-1)
        enddo
        enddo
        print*, 'minval, maxval(HGTOL2): ', minval(dum2d),maxval(dum2d)
        call write_field(start_mem_i, end_mem_i, start_mem_j, end_mem_j, 1, 1, &
                       fieldname, datestr, dum2d)
      endif

! - 9 ---------------------------------------------------------------

      fieldname='HGTOL3'
      if (.not.ncep_proc_grib2) then
        jpds=-1
        jgds=-1
        jpds(5)=153
        call getgb(27,0,end_file_i*end_file_j,0,JPDS,JGDS,nwords,KNUM,KPDS,KGDS, &
                   BITMAP,dum2d_8,IRET1)
      else
        JIDS=-9999
        JPDT=-9999
        JGDT=-9999
        JPDT(2)=152
        call getgb2(27,27,0,-1,JIDS,0,JPDT,-1,JGDT,.TRUE.,K,GFLD,IRET1)
        if (IRET1.eq.0) then
          dum2d_8 = reshape(GFLD%fld, (/ end_file_i, end_file_j /))
        end if
        call g2_free(gfld)
      end if

      if (IRET1 .eq. 0) then
        do j=1,end_mem_j
        do i=1,end_mem_i
          dum2d(I,J,1)=dum2d_8(I+top_i_parent_start-1,J+top_j_parent_start-1)
        enddo
        enddo
        print*, 'minval, maxval(HGTOL3): ', minval(dum2d),maxval(dum2d)
        call write_field(start_mem_i, end_mem_i, start_mem_j, end_mem_j, 1, 1, &
                       fieldname, datestr, dum2d)
      endif

! - 10 ---------------------------------------------------------------

      fieldname='HGTOL4'
      if (.not.ncep_proc_grib2) then
        jpds=-1
        jgds=-1
        jpds(5)=154
        call getgb(27,0,end_file_i*end_file_j,0,JPDS,JGDS,nwords,KNUM,KPDS,KGDS, &
                   BITMAP,dum2d_8,IRET1)
      else
        JIDS=-9999
        JPDT=-9999
        JGDT=-9999
        JPDT(2)=154
        call getgb2(27,27,0,-1,JIDS,0,JPDT,-1,JGDT,.TRUE.,K,GFLD,IRET1)
        if (IRET1.eq.0) then
          dum2d_8 = reshape(GFLD%fld, (/ end_file_i, end_file_j /))
        end if
        call g2_free(gfld)
      end if

      if (IRET1 .eq. 0) then
        do j=1,end_mem_j
        do i=1,end_mem_i
          dum2d(I,J,1)=dum2d_8(I+top_i_parent_start-1,J+top_j_parent_start-1)
        enddo
        enddo
        print*, 'minval, maxval(HGTOL4): ', minval(dum2d),maxval(dum2d)
        call write_field(start_mem_i, end_mem_i, start_mem_j, end_mem_j, 1, 1, &
                       fieldname, datestr, dum2d)
      endif

! - 11 --------------------------------------------------------------

      fieldname='HGTTHTA'
      if (.not.ncep_proc_grib2) then
        jpds=-1
        jgds=-1
        jpds(5)=101
        call getgb(27,0,end_file_i*end_file_j,0,JPDS,JGDS,nwords,KNUM,KPDS,KGDS, &
                   BITMAP,dum2d_8,IRET1)
      else
        JIDS=-9999
        JPDT=-9999
        JGDT=-9999
        JPDT(2)=101
        call getgb2(27,27,0,-1,JIDS,0,JPDT,-1,JGDT,.TRUE.,K,GFLD,IRET1)
        if (IRET1.eq.0) then
          dum2d_8 = reshape(GFLD%fld, (/ end_file_i, end_file_j /))
        end if
        call g2_free(gfld)
      end if

      if (IRET1 .eq. 0) then
        do j=1,end_mem_j
        do i=1,end_mem_i
          dum2d(I,J,1)=dum2d_8(I+top_i_parent_start-1,J+top_j_parent_start-1)
        enddo
        enddo
        print*, 'minval, maxval(HGTTHTA): ', minval(dum2d),maxval(dum2d)
        call write_field(start_mem_i, end_mem_i, start_mem_j, end_mem_j, 1, 1, &
                       fieldname, datestr, dum2d)
      endif

! - 12 ---------------------------------------------------------------
      fieldname='HGTGMMA'
      if (.not.ncep_proc_grib2) then
        jpds=-1
        jgds=-1
        jpds(5)=103
        call getgb(27,0,end_file_i*end_file_j,0,JPDS,JGDS,nwords,KNUM,KPDS,KGDS, &
                   BITMAP,dum2d_8,IRET1)
      else
        JIDS=-9999
        JPDT=-9999
        JGDT=-9999
        JPDT(2)=103
        call getgb2(27,27,0,-1,JIDS,0,JPDT,-1,JGDT,.TRUE.,K,GFLD,IRET1)
        if (IRET1.eq.0) then
          dum2d_8 = reshape(GFLD%fld, (/ end_file_i, end_file_j /))
        end if
        call g2_free(gfld)
      end if

      if (IRET1 .eq. 0) then
        do j=1,end_mem_j
        do i=1,end_mem_i
          dum2d(I,J,1)=dum2d_8(I+top_i_parent_start-1,J+top_j_parent_start-1)
        enddo
        enddo
        print*, 'minval, maxval(HGTGMMA): ', minval(dum2d),maxval(dum2d)
        call write_field(start_mem_i, end_mem_i, start_mem_j, end_mem_j, 1, 1, &
                       fieldname, datestr, dum2d)
      endif

! - 13 ---------------------------------------------------------------

      fieldname='HGTSGMA'
      if (.not.ncep_proc_grib2) then
        jpds=-1
        jgds=-1
        jpds(5)=102
        call getgb(27,0,end_file_i*end_file_j,0,JPDS,JGDS,nwords,KNUM,KPDS,KGDS, &
                   BITMAP,dum2d_8,IRET1)
      else
        JIDS=-9999
        JPDT=-9999
        JGDT=-9999
        JPDT(2)=102
        call getgb2(27,27,0,-1,JIDS,0,JPDT,-1,JGDT,.TRUE.,K,GFLD,IRET1)
        if (IRET1.eq.0) then
          dum2d_8 = reshape(GFLD%fld, (/ end_file_i, end_file_j /))
        end if
        call g2_free(gfld)
      end if

      if (IRET1 .eq. 0) then
        do j=1,end_mem_j
        do i=1,end_mem_i
          dum2d(I,J,1)=dum2d_8(I+top_i_parent_start-1,J+top_j_parent_start-1)
        enddo
        enddo
        print*, 'minval, maxval(HGTSGMA): ', minval(dum2d),maxval(dum2d)
        call write_field(start_mem_i, end_mem_i, start_mem_j, end_mem_j, 1, 1, &
                       fieldname, datestr, dum2d)
      endif

! - 14 ---------------------------------------------------------------

      fieldname='HGTMAX'
      if (.not.ncep_proc_grib2) then
        jpds=-1
        jgds=-1
        jpds(5)=221
        call getgb(27,0,end_file_i*end_file_j,0,JPDS,JGDS,nwords,KNUM,KPDS,KGDS, &
                   BITMAP,dum2d_8,IRET1)
      else
        JIDS=-9999
        JPDT=-9999
        JGDT=-9999
        JPDT(2)=221
        call getgb2(27,27,0,-1,JIDS,0,JPDT,-1,JGDT,.TRUE.,K,GFLD,IRET1)
        if (IRET1.eq.0) then
          dum2d_8 = reshape(GFLD%fld, (/ end_file_i, end_file_j /))
        end if
        call g2_free(gfld)
      end if

      if (IRET1 .eq. 0) then
        do j=1,end_mem_j
        do i=1,end_mem_i
          dum2d(I,J,1)=dum2d_8(I+top_i_parent_start-1,J+top_j_parent_start-1)
        enddo
        enddo
        print*, 'minval, maxval(HGTMAX): ', minval(dum2d),maxval(dum2d)
        call write_field(start_mem_i, end_mem_i, start_mem_j, end_mem_j, 1, 1, &
                       fieldname, datestr, dum2d)
      endif

! ----------------------------------------------------------------

      endif GWD


! ----------------------------------------------------------------
! ----------------------------------------------------------------

      call baclose(27,IERR)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!  READ/WRITE VEGCAT
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      filename=trim(domain_name)//'_vegtiles.grb'
      if (ncep_proc_grib2) then
        filename=trim(filename)//'2'
      end if

      call baopenr(28,trim(filename),ierr)
      if (ierr .ne. 0) then
         write(0,*)' ierr .ne. 0 for baopenr ',trim(filename), ierr
         stop
      end if

      dum2d=0.

      if (.not.ncep_proc_grib2) then
        jpds=-1
        jgds=-1
        call getgb(28,0,end_file_i*end_file_j,0,JPDS,JGDS,nwords,KNUM,KPDS,KGDS, &
                   BITMAP,dum2d_8,IRET1)
      else
        JIDS=-9999
        JPDT=-9999
        JGDT=-9999
        call getgb2(28,28,0,-1,JIDS,-1,JPDT,-1,JGDT,.TRUE.,K,GFLD,IRET1)
        if (IRET1.eq.0) then
          dum2d_8 = reshape(GFLD%fld, (/ end_file_i, end_file_j /))
        end if
        call g2_free(gfld)
      end if

      if (use_igbp) then
        landusef_dim=20
      else
        landusef_dim=24
      endif

      if (IRET1 .eq. 0) then
        print*, 'minval, maxval(VEGCAT): ', minval(dum2d_8),maxval(dum2d_8)

        allocate(dum3d(end_dom_i,end_dom_j,landusef_dim))

        do N=1,landusef_dim
        do J=1,end_mem_j
        do I=1,end_mem_i
          dum3d(I,J,N)=dum2d_8(I+top_i_parent_start-1,J+top_j_parent_start-1)
        enddo
        enddo
        enddo

        call write_field(start_mem_i, end_mem_i, start_mem_j, end_mem_j, 1, landusef_dim,  &
                         'LANDUSEF', datestr, real_array = dum3d)
        call write_field(start_mem_i, end_mem_i, start_mem_j, end_mem_j, 1, 1, &
                         'LU_INDEX', datestr, real_array = dum3d)

        deallocate(dum3d)
      else
        write(0,*)' IRET1 .ne. 0 for ',trim(filename), IRET1
        stop
      endif

      call baclose(28,IERR)
      if (ierr .ne. 0) then
        write(0,*)' ierr .ne. 0 for baclose ',trim(filename), ierr
         stop
      end if


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!  READ/WRITE SOILTEMP
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      filename=trim(domain_name)//'_tbot.grb'
      if (ncep_proc_grib2) then
        filename=trim(filename)//'2'
      end if

      call baopenr(29,trim(filename),ierr)
      if (ierr .ne. 0) then
         write(0,*)' ierr .ne. 0 for baopenr ',trim(filename), ierr
         stop
      end if

      if (.not.ncep_proc_grib2) then
        jpds=-1
        jgds=-1
        call getgb(29,0,end_file_i*end_file_j,0,JPDS,JGDS,nwords,KNUM,KPDS,KGDS, &
                   BITMAP,dum2d_8,IRET1)
      else
        JIDS=-9999
        JPDT=-9999
        JGDT=-9999
        call getgb2(29,29,0,-1,JIDS,-1,JPDT,-1,JGDT,.TRUE.,K,GFLD,IRET1)
        if (IRET1.eq.0) then
          dum2d_8 = reshape(GFLD%fld, (/ end_file_i, end_file_j /))
        end if
        call g2_free(gfld)
      end if

      if (IRET1 .eq. 0) then
        do j=1,end_mem_j
        do i=1,end_mem_i
          dum2d(I,J,1)=dum2d_8(I+top_i_parent_start-1,J+top_j_parent_start-1)
        enddo
        enddo
        print*, 'minval, maxval(SOILTEMP): ', minval(dum2d),maxval(dum2d)
        call write_field(start_mem_i, end_mem_i, start_mem_j, end_mem_j, 1, 1, &
                         'SOILTEMP', datestr, real_array = dum2d)
      else
        write(0,*)' IRET1 .ne. 0 for ',trim(filename), IRET1
        stop
      endif

      call baclose(29,ierr)
      if (ierr .ne. 0) then
        write(0,*)' ierr .ne. 0 for baclose ',trim(filename), ierr
         stop
      end if

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!  READ/WRITE SOILCTOP/CBOT
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      filename=trim(domain_name)//'_soiltiles.grb'
      if (ncep_proc_grib2) then
        filename=trim(filename)//'2'
      end if

      call baopenr(30,trim(filename),ierr)
      if (ierr .ne. 0) then
         write(0,*)' ierr .ne. 0 for baopenr ',trim(filename), ierr
         stop
      end if

      allocate(dum3d(end_dom_i,end_dom_j,16))
      dum3d=0.

      if (.not.ncep_proc_grib2) then
        jpds=-1
        jgds=-1
        call getgb(30,0,end_file_i*end_file_j,0,JPDS,JGDS,nwords,KNUM,KPDS,KGDS, &
                   BITMAP,dum2d_8,IRET1)
      else
        JIDS=-9999
        JPDT=-9999
        JGDT=-9999
        call getgb2(30,30,0,-1,JIDS,-1,JPDT,-1,JGDT,.TRUE.,K,GFLD,IRET1)
        if (IRET1.eq.0) then
          dum2d_8 = reshape(GFLD%fld, (/ end_file_i, end_file_j /))
        end if
        call g2_free(gfld)
      end if

      if (IRET1 .eq. 0) then
        do j=1,end_mem_j
        do i=1,end_mem_i
          dum3d(I,J,1)=dum2d_8(I+top_i_parent_start-1,J+top_j_parent_start-1)
        enddo
        enddo

        do N=2,16
        do J=1,end_dom_j
        do I=1,end_dom_i
          dum3d(I,J,N)=dum3d(I,J,1)
        enddo
        enddo
        enddo

        call write_field(start_mem_i, end_mem_i, start_mem_j, end_mem_j, 1, 16, &
                         'SOILCTOP', datestr, real_array = dum3d)
        call write_field(start_mem_i, end_mem_i, start_mem_j, end_mem_j, 1, 16, &
                         'SOILCBOT', datestr, real_array = dum3d)

      endif

      call baclose(30,ierr)
      if (ierr .ne. 0) then
        write(0,*)' ierr .ne. 0 for baclose ',trim(filename), ierr
        stop
      end if
      deallocate(dum3d)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!  READ/WRITE ALBEDO12M
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      filename=trim(domain_name)//'_snowfree_albedo.grb'
      if (ncep_proc_grib2) then
        filename=trim(filename)//'2'
      end if

      call baopenr(31,trim(filename),ierr)
      if (ierr .ne. 0) then
         write(0,*)' ierr .ne. 0 for baopenr ',trim(filename), ierr
         stop
      end if

      allocate(dum3d(end_dom_i,end_dom_j,12))

      dum3d=0.

      if (.not.ncep_proc_grib2) then
        jpds=-1
        jgds=-1

!!!  How to pull out the multiple records?....use skip # records option in getgb?
!!
        call getgb(31,0,end_file_i*end_file_j,0,JPDS,JGDS,nwords,KNUM,KPDS,KGDS, &
                   BITMAP,dum2d_8,IRET1)
        do j=1,end_mem_j
        do i=1,end_mem_i
          dum3d(I,J,1)=dum2d_8(I+top_i_parent_start-1,J+top_j_parent_start-1)
          dum3d(I,J,5)=dum2d_8(I+top_i_parent_start-1,J+top_j_parent_start-1)
          dum3d(I,J,9)=dum2d_8(I+top_i_parent_start-1,J+top_j_parent_start-1)
        enddo
        enddo

        call getgb(31,0,end_file_i*end_file_j,1,JPDS,JGDS,nwords,KNUM,KPDS,KGDS, &
                   BITMAP,dum2d_8,IRET1)
        do j=1,end_mem_j
        do i=1,end_mem_i
          dum3d(I,J,2)=dum2d_8(I+top_i_parent_start-1,J+top_j_parent_start-1)
          dum3d(I,J,6)=dum2d_8(I+top_i_parent_start-1,J+top_j_parent_start-1)
          dum3d(I,J,10)=dum2d_8(I+top_i_parent_start-1,J+top_j_parent_start-1)
        enddo
        enddo

        call getgb(31,0,end_file_i*end_file_j,2,JPDS,JGDS,nwords,KNUM,KPDS,KGDS, &
                   BITMAP,dum2d_8,IRET1)
        do j=1,end_mem_j
        do i=1,end_mem_i
          dum3d(I,J,3)=dum2d_8(I+top_i_parent_start-1,J+top_j_parent_start-1)
          dum3d(I,J,7)=dum2d_8(I+top_i_parent_start-1,J+top_j_parent_start-1)
          dum3d(I,J,11)=dum2d_8(I+top_i_parent_start-1,J+top_j_parent_start-1)
        enddo
        enddo

        call getgb(31,0,end_file_i*end_file_j,3,JPDS,JGDS,nwords,KNUM,KPDS,KGDS, &
                   BITMAP,dum2d_8,IRET1)
        do j=1,end_mem_j
        do i=1,end_mem_i
          dum3d(I,J,4)=dum2d_8(I+top_i_parent_start-1,J+top_j_parent_start-1)
          dum3d(I,J,8)=dum2d_8(I+top_i_parent_start-1,J+top_j_parent_start-1)
          dum3d(I,J,12)=dum2d_8(I+top_i_parent_start-1,J+top_j_parent_start-1)
        enddo
        enddo

      else
        JIDS=-9999
        JPDT=-9999
        JGDT=-9999
        call getgb2(31,31,0,-1,JIDS,-1,JPDT,-1,JGDT,.TRUE.,K,GFLD,IRET1)
        if (IRET1.eq.0) then
          dum2d_8 = reshape(GFLD%fld, (/ end_file_i, end_file_j /))
        end if
        call g2_free(gfld)
        do j=1,end_mem_j
        do i=1,end_mem_i
          dum3d(I,J,1)=dum2d_8(I+top_i_parent_start-1,J+top_j_parent_start-1)
          dum3d(I,J,5)=dum2d_8(I+top_i_parent_start-1,J+top_j_parent_start-1)
          dum3d(I,J,9)=dum2d_8(I+top_i_parent_start-1,J+top_j_parent_start-1)
        enddo
        enddo

        call getgb2(31,31,1,-1,JIDS,-1,JPDT,-1,JGDT,.TRUE.,K,GFLD,IRET1)
        if (IRET1.eq.0) then
          dum2d_8 = reshape(GFLD%fld, (/ end_file_i, end_file_j /))
        end if
        call g2_free(gfld)
        do j=1,end_mem_j
        do i=1,end_mem_i
          dum3d(I,J,2)=dum2d_8(I+top_i_parent_start-1,J+top_j_parent_start-1)
          dum3d(I,J,6)=dum2d_8(I+top_i_parent_start-1,J+top_j_parent_start-1)
          dum3d(I,J,10)=dum2d_8(I+top_i_parent_start-1,J+top_j_parent_start-1)
        enddo
        enddo

        call getgb2(31,31,2,-1,JIDS,-1,JPDT,-1,JGDT,.TRUE.,K,GFLD,IRET1)
        if (IRET1.eq.0) then
          dum2d_8 = reshape(GFLD%fld, (/ end_file_i, end_file_j /))
        end if
        call g2_free(gfld)
        do j=1,end_mem_j
        do i=1,end_mem_i
          dum3d(I,J,3)=dum2d_8(I+top_i_parent_start-1,J+top_j_parent_start-1)
          dum3d(I,J,7)=dum2d_8(I+top_i_parent_start-1,J+top_j_parent_start-1)
          dum3d(I,J,11)=dum2d_8(I+top_i_parent_start-1,J+top_j_parent_start-1)
        enddo
        enddo

        call getgb2(31,31,3,-1,JIDS,-1,JPDT,-1,JGDT,.TRUE.,K,GFLD,IRET1)
        if (IRET1.eq.0) then
          dum2d_8 = reshape(GFLD%fld, (/ end_file_i, end_file_j /))
        end if
        call g2_free(gfld)
        do j=1,end_mem_j
        do i=1,end_mem_i
          dum3d(I,J,4)=dum2d_8(I+top_i_parent_start-1,J+top_j_parent_start-1)
          dum3d(I,J,8)=dum2d_8(I+top_i_parent_start-1,J+top_j_parent_start-1)
          dum3d(I,J,12)=dum2d_8(I+top_i_parent_start-1,J+top_j_parent_start-1)
        enddo
        enddo

      end if


      if (IRET1 .eq. 0) then
        do N=1,4
        print*, 'minval, maxval(ALBEDO): ', minval(dum3d(:,:,N)),maxval(dum3d(:,:,N))
        enddo

        call write_field(start_mem_i, end_mem_i, start_mem_j, end_mem_j, 1, 12, &
                         'ALBEDO12M', datestr, real_array = dum3d)
      endif

      deallocate(dum3d)

      call baclose(31,IERR)
      if (ierr .ne. 0) then
        write(0,*)' ierr .ne. 0 for baclose ',trim(filename), ierr
        stop
      end if


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!  READ/WRITE GREENFRAC
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      filename=trim(domain_name)//'_vegfrac.grb'
      if (ncep_proc_grib2) then
        filename=trim(filename)//'2'
      end if

      call baopenr(32,trim(filename),ierr)
      if (ierr .ne. 0) then
         write(0,*)' ierr .ne. 0 for baopenr ',trim(filename), ierr
         stop
      end if

      allocate(dum3d(end_dom_i,end_dom_j,12))
      allocate(dum3d_8(end_file_i,end_file_j,12))
      dum3d=0.
      dum3d_8=0.

      if (.not.ncep_proc_grib2) then
        jpds=-1
        jgds=-1
        do N=1,12
          call getgb(32,0,end_file_i*end_file_j,N-1,JPDS,JGDS,nwords,KNUM,KPDS,KGDS, &
                     BITMAP,dum3d_8(:,:,N),IRET1)

          if (IRET1 .eq. 0) then
            do j=1,end_mem_j
            do i=1,end_mem_i
              dum3d(I,J,N)=dum3d_8(I+top_i_parent_start-1,J+top_j_parent_start-1,N)
            enddo
            enddo
            print*, 'minval, maxval(VEGFRC): ', minval(dum3d(:,:,N)),maxval(dum3d(:,:,N))
          endif
        enddo
      else
        JIDS=-9999
        JPDT=-9999
        JGDT=-9999
        do N=1,12
          call getgb2(32,32,N-1,-1,JIDS,-1,JPDT,-1,JGDT,.TRUE.,K,GFLD,IRET1)
          if (IRET1.eq.0) then
            dum2d_8 = reshape(GFLD%fld, (/ end_file_i, end_file_j /))
            do j=1,end_mem_j
            do i=1,end_mem_i
              dum3d(I,J,N)=dum2d_8(I+top_i_parent_start-1,J+top_j_parent_start-1)
            enddo
            enddo
          end if
          call g2_free(gfld)
        enddo
      endif

      if (IRET1 .eq. 0) then
        call write_field(start_mem_i, end_mem_i, start_mem_j, end_mem_j, 1, 12, &
                         'GREENFRAC', datestr, real_array = dum3d)
      end if

      deallocate(dum3d)
      deallocate(dum3d_8)

      call baclose(32,ierr)
      if (ierr .ne. 0) then
        write(0,*)' ierr .ne. 0 for baclose ',trim(filename), ierr
        stop
      end if

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!  READ/WRITE SNOALB
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      filename=trim(domain_name)//'_mxsnoalb.grb'
      if (ncep_proc_grib2) then
        filename=trim(filename)//'2'
      end if

      call baopenr(33,trim(filename),ierr)
      if (ierr .ne. 0) then
         write(0,*)' ierr .ne. 0 for baopenr ',trim(filename), ierr
         stop
      end if

      dum2d=0.
      if (.not.ncep_proc_grib2) then
        jpds=-1
        jgds=-1

        call getgb(33,0,end_file_i*end_file_j,0,JPDS,JGDS,nwords,KNUM,KPDS,KGDS, &
                    BITMAP,dum2d_8,IRET1)
      else
        JIDS=-9999
        JPDT=-9999
        JGDT=-9999
        call getgb2(33,33,0,-1,JIDS,-1,JPDT,-1,JGDT,.TRUE.,K,GFLD,IRET1)
        if (IRET1.eq.0) then
          dum2d_8 = reshape(GFLD%fld, (/ end_file_i, end_file_j /))
        end if
        call g2_free(gfld)
      end if

      if (IRET1 .eq. 0) then
        do j=1,end_mem_j
        do i=1,end_mem_i
          dum2d(I,J,1)=dum2d_8(I+top_i_parent_start-1,J+top_j_parent_start-1)
        enddo
        enddo
        print*, 'minval, maxval(SNOALB): ', minval(dum2d),maxval(dum2d)
        call write_field(start_mem_i, end_mem_i, start_mem_j, end_mem_j, 1, 1, &
                         'SNOALB', datestr, real_array = dum2d)
      else
        write(0,*)' IRET1 .ne. 0 for ',trim(filename), IRET1
        stop
      endif

      call baclose(33,ierr)
      if (ierr .ne. 0) then
        write(0,*)' ierr .ne. 0 for baclose ',trim(filename), ierr
        stop
      end if

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!  READ/WRITE SLOPECAT
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      filename=trim(domain_name)//'_slopeidx.grb'
      if (ncep_proc_grib2) then
        filename=trim(filename)//'2'
      end if

      call baopenr(34,trim(filename),ierr)
      if (ierr .ne. 0) then
         write(0,*)' ierr .ne. 0 for baopenr ',trim(filename), ierr
         stop
      end if

      dum2d=0.
      if (.not.ncep_proc_grib2) then
        jpds=-1
        jgds=-1
        call getgb(34,0,end_file_i*end_file_j,0,JPDS,JGDS,nwords,KNUM,KPDS,KGDS, &
                   BITMAP,dum2d_8,IRET1)
      else
        JIDS=-9999
        JPDT=-9999
        JGDT=-9999
        call getgb2(34,34,0,-1,JIDS,-1,JPDT,-1,JGDT,.TRUE.,K,GFLD,IRET1)
        if (IRET1.eq.0) then
          dum2d_8 = reshape(GFLD%fld, (/ end_file_i, end_file_j /))
        end if
        call g2_free(gfld)
      end if

      if (IRET1 .eq. 0) then
        do j=1,end_mem_j
        do i=1,end_mem_i
          dum2d(I,J,1)=dum2d_8(I+top_i_parent_start-1,J+top_j_parent_start-1)
        enddo
        enddo
        print*, 'minval, maxval(SLOPCAT): ', minval(dum2d),maxval(dum2d)
        call write_field(start_mem_i, end_mem_i, start_mem_j, end_mem_j, 1, 1, &
                         'SLOPECAT', datestr, dum2d)
      else
        write(0,*)' IRET1 for _slopeidx.grb ',IRET1
        stop
      endif

      call baclose(34,ierr)
      if (ierr .ne. 0) then
        write(0,*)' ierr .ne. 0 for baclose ',trim(filename), ierr
        stop
      end if

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!  READ/WRITE LANDMASK
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      filename=trim(domain_name)//'_slmask.grb'
      if (ncep_proc_grib2) then
        filename=trim(filename)//'2'
      end if

      call baopenr(35,trim(filename),ierr)
      if (ierr .ne. 0) then
         write(0,*)' ierr .ne. 0 for baopenr ',trim(filename), ierr
         stop
      end if

      dum2d=0.
      if (.not.ncep_proc_grib2) then
        jpds=-1
        jgds=-1
        call getgb(35,0,end_file_i*end_file_j,0,JPDS,JGDS,nwords,KNUM,KPDS,KGDS, &
                   BITMAP,dum2d_8,IRET1)
      else
        JIDS=-9999
        JPDT=-9999
        JGDT=-9999
        call getgb2(35,35,0,-1,JIDS,-1,JPDT,-1,JGDT,.TRUE.,K,GFLD,IRET1)
        if (IRET1.eq.0) then
          dum2d_8 = reshape(GFLD%fld, (/ end_file_i, end_file_j /))
        end if
        call g2_free(gfld)
      end if

      if (IRET1 .eq. 0) then
        do j=1,end_mem_j
        do i=1,end_mem_i
        dum2d(I,J,1)=dum2d_8(I+top_i_parent_start-1,J+top_j_parent_start-1)
        enddo
        enddo
        print*, 'minval, maxval(LANDMASK): ', minval(dum2d),maxval(dum2d)
        call write_field(start_mem_i, end_mem_i, start_mem_j, end_mem_j, 1, 1, &
                         'LANDMASK', datestr, dum2d)
      else
        write(0,*)' IRET1 for _slmask.grb ',IRET1
        stop
      endif

      call baclose(35,ierr)
      if (ierr .ne. 0) then
        write(0,*)' ierr .ne. 0 for baclose ',trim(filename), ierr
        stop
      end if

      call output_close()

   end subroutine process_ncep

   subroutine g2_free(gfld)

      use grib_mod

      implicit none

      type(gribfield)  :: gfld

      if(associated(gfld%idsect)) deallocate(gfld%idsect)
      if(associated(gfld%local)) deallocate(gfld%local)
      if(associated(gfld%list_opt)) deallocate(gfld%list_opt)
      if(associated(gfld%igdtmpl)) deallocate(gfld%igdtmpl)
      if(associated(gfld%ipdtmpl)) deallocate(gfld%ipdtmpl)
      if(associated(gfld%coord_list)) deallocate(gfld%coord_list)
      if(associated(gfld%idrtmpl)) deallocate(gfld%idrtmpl)
      if(associated(gfld%bmap)) deallocate(gfld%bmap)
      if(associated(gfld%fld)) deallocate(gfld%fld)

      nullify(gfld%idsect)
      nullify(gfld%local)
      nullify(gfld%list_opt)
      nullify(gfld%igdtmpl)
      nullify(gfld%ipdtmpl)
      nullify(gfld%coord_list)
      nullify(gfld%idrtmpl)
      nullify(gfld%bmap)
      nullify(gfld%fld)

      return

   end subroutine g2_free

end module process_ncep_module
