  program ut_diurnal_bb

     use Chem_UtilMod
     implicit NONE

     integer, parameter :: im = 720, jm=361
     integer :: nhms, i, j
     real :: dlon, dlat, cdt
     real :: bb(im,jm), bb_(im,jm), lons(im), lats(jm)

     bb_ = 1.
     dlon = 360. / im
     dlat = 180. / (jm-1)
     do i = 1, im
        lons(i) = -180. + (i-1) * dlon
     end do
     do j = 1, jm
        lats(j) = -90. + (j-1) * dlat
     end do
     cdt = 60 * 60. ! 1 hour in secs

     open(10,file='diurnal.bin',form='unformatted')
     do nhms = 0, 230000, 10000
        call Chem_BiomassDiurnal ( bb, bb_, lons, lats, nhms, cdt)
        write(10) bb
!!!!        print *, 'nhms = ', nhms, ' --- bb = ', bb(:,jm/2)
     end do
     close(10)

   end program ut_diurnal_bb
