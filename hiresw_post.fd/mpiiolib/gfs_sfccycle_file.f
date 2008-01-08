 module gfs_sfccycle_file


 contains

!-----------------------------------------------------------------------
! calculates the byte position of the beginning of each record
! of a sfccycle file.  the file is sequential, so calculations
! include the 4 byte header and trailer between each record.
!
! works for the noah era version of the surface cycle file but
! WITHOUT the new sea ice model fields.
!-----------------------------------------------------------------------

 subroutine calc_offset_200412(imdl, jmdl, offset)

! works for version 200412

 implicit none

 integer, intent(in)            :: imdl, jmdl

 integer*8, intent(out)   ::  offset(50)

 offset(1) = 4                                        ! start of record 1
 offset(2) = offset(1) + 32 + 4 + 4                   ! start of record 2
 offset(3) = offset(2) + 8 * 4 + jmdl/2 * 4 + 4 + 4   ! start of record 3  skint
 offset(4) = offset(3) + 4 * imdl * jmdl  + 4 + 4     ! start of record 4  soilm
 offset(5) = offset(4) + 4 * imdl * jmdl * 4 + 4 + 4  ! start of record 5  sheleg
 offset(6) = offset(5) + 4 * imdl * jmdl + 4 + 4      ! start of record 6  soilt
 offset(7) = offset(6) + 4 * imdl * jmdl *4 + 4 + 4   ! start of record 7  tg3
 offset(8) = offset(7) + 4 * imdl * jmdl + 4 + 4      ! start of record 8  z0
 offset(9) = offset(8) + 4 * imdl * jmdl + 4 + 4      ! start of record 9  cv
 offset(10)= offset(9) + 4 * imdl * jmdl + 4 + 4      ! start of record 10 cvb
 offset(11)= offset(10) + 4 * imdl * jmdl + 4 + 4     ! start of record 11 cbt
 offset(12)= offset(11) + 4 * imdl * jmdl + 4 + 4     ! start of record 12 4 albs
 offset(13)= offset(12) + 4 * imdl * jmdl *4 + 4 + 4  ! start of record 13 slmsk
 offset(14)= offset(13) + 4 * imdl * jmdl + 4 + 4     ! start of record 14 green
 offset(15)= offset(14) + 4 * imdl * jmdl + 4 + 4     ! start of record 15 cmc
 offset(16)= offset(15) + 4 * imdl * jmdl + 4 + 4     ! start of record 16 f10m
 offset(17)= offset(16) + 4 * imdl * jmdl + 4 + 4     ! start of record 17 vegtyp
 offset(18)= offset(17) + 4 * imdl * jmdl + 4 + 4     ! start of record 18 sltyp
 offset(19)= offset(18) + 4 * imdl * jmdl + 4 + 4     ! start of record 19 facs/wf
 offset(20)= offset(19) + 4 * imdl * jmdl *2 + 4 + 4  ! start of record 20 ustar
 offset(21)= offset(20) + 4 * imdl * jmdl + 4 + 4     ! start of record 21 ffmm
 offset(22)= offset(21) + 4 * imdl * jmdl + 4 + 4     ! start of record 22 ffhh
 offset(23)= offset(22) + 4 * imdl * jmdl + 4 + 4     ! start of record 23 tprcp
 offset(24)= offset(23) + 4 * imdl * jmdl + 4 + 4     ! start of record 24 srflag
 offset(25)= offset(24) + 4 * imdl * jmdl + 4 + 4     ! start of record 25 snowd
 offset(26)= offset(25) + 4 * imdl * jmdl + 4 + 4     ! start of record 26 slc
 offset(27)= offset(26) + 4 * imdl * jmdl*4 + 4 + 4   ! start of record 27 ming
 offset(28)= offset(27) + 4 * imdl * jmdl + 4 + 4     ! start of record 28 maxg
 offset(29)= offset(28) + 4 * imdl * jmdl + 4 + 4     ! start of record 29 slope
 offset(30)= offset(29) + 4 * imdl * jmdl + 4 + 4     ! start of record 29 mxsna

 return

 end subroutine calc_offset_200412

!-----------------------------------------------------------------------
! this works for the noah era surface file WITHOUT the new sea ice
! model fields.
!-----------------------------------------------------------------------

 subroutine get_index_200412(variable, offset_index)

 implicit none

 character*8, intent(in)      :: variable

 integer, intent(out)         :: offset_index

 select case (trim(variable))
 case('skint')         ! skin temperature
   offset_index = 3
 case('soilm')         ! total soil moisture
   offset_index = 4
 case('sheleg')        ! snow water equivalent
   offset_index = 5
 case('soilt')         ! soil temperature
   offset_index = 6
 case('tg3')           ! soil substrate temperature
   offset_index = 7
 case('z0')            ! roughness length
   offset_index = 8
 case('cv')            ! convective cloud
   offset_index = 9
 case('cvb')           ! conv cloud bottom
   offset_index = 10
 case('cvt')           ! conv cloud top
   offset_index = 11
 case('alb')           ! snow free albedo (4 types)
   offset_index = 12
 case('slmsk')         ! land mask
   offset_index = 13
 case('green')         ! greenness fraction
   offset_index = 14
 case('cmc')           ! canopy moisture content
   offset_index = 15
 case('f10m')          ! f10m
   offset_index = 16
 case('vegtyp')        ! vegetation type
   offset_index = 17
 case('sltyp')         ! soil type
   offset_index = 18
 case('fac')           ! facsf/facwf
   offset_index = 19
 case('ustar')         ! u*
   offset_index = 20
 case('ffmm')          ! ffmm
   offset_index = 21
 case('ffhh')          ! ffhh
   offset_index = 22
 case('tprcp')         ! precip
   offset_index = 23
 case('srflag')        ! precip flag
   offset_index = 24
 case('snod')          ! snow depth
   offset_index = 25
 case('slc')           ! soilm liq
   offset_index = 26
 case('ming')          ! minimum greenness
   offset_index = 27
 case('maxg')          ! maximum greenness
   offset_index = 28
 case('slope')         ! slope category
   offset_index = 29
 case('mxsna')         ! max snow albedo
   offset_index = 30
 case default
   print*,'bad variable input'
   stop
 end select

 return

 end subroutine get_index_200412

 end module gfs_sfccycle_file