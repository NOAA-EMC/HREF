  openr, lun, 'coagtest.txt', /get_lun
  readf, lun, t0, nbin, nelem
;  ibin_ = intarr(nbin)
  r_    = fltarr(nbin,nelem)      ; radius (m)
  rmass_= fltarr(nbin,nelem)      ; radius (m)
  pc_   = fltarr(nbin,nelem)      ; # conc (m^-3) in a bin
  psd_    = fltarr(nbin,nelem)    ; dN/dlogr = dN/dlogD (m^-3)
  data = fltarr(5)
  for ielem = 0, nelem-1 do begin
   for ibin = 0, nbin-1 do begin
    readf, lun, data
;    ibin_[ibin] = fix( data[0] )
    r_[ibin,ielem]   = data[1]
    rmass_[ibin,ielem]   = data[2]
    pc_[ibin,ielem]    = data[3]
    psd_[ibin,ielem] = data[4]
   endfor
  endfor
  time = t0
;  ibin = ibin_
  r = r_
  rmass = rmass_
  pc = pc_
  psd = psd_

  nt = 1
  while(not(eof(lun))) do begin
   readf, lun, t1
   for ielem = 0, nelem-1 do begin
    for ibin = 0, nbin-1 do begin
     readf, lun, data
;    ibin_[ibin] = fix( data[0] )
     r_[ibin,ielem]   = data[1]
     rmass_[ibin,ielem] = data[2]
     pc_[ibin,ielem]    = data[3]
     psd_[ibin,ielem] = data[4]
    endfor
   endfor
   time = [time,t1]
;   ibin = [ibin,ibin_]
   r = [r,r_]
   rmass = [rmass,rmass_]
   pc = [pc,pc_]
   psd = [psd,psd_]
   nt = nt+1
  endwhile
  free_lun, lun

;  ibin = reform(ibin,nbin,nt)
  r = reform(r,nbin,nt,nelem)
  rmass = reform(rmass,nbin,nt,nelem)
  pc = reform(pc,nbin,nt,nelem)
  psd = reform(psd,nbin,nt,nelem)

; The data has been manipulated and converted to match Figure 2 from
; Jacobson et al., Atmospheric Environment 28, 1327-1338, 1994
; Initial monodisperse distribution is slightly different -- probably because
; I don't know what bin edges were used to calculate dr
;
; - JAS, August 15, 2007

  plot, [ 0.005, 0.1 ], [ 1., 1.e8 ], /nodata $
      , xtitle = 'Diameter (um)' $
      , /xlog, xstyle = 1 $
      , ytitle = 'dN / d log D (cm^-3)' $
      , /ylog, ystyle = 1 $
      , charsize = 2
  plots, 2.*r*1.e6, psd[*,0,0]*1.e-6, psym=2, symsize=2 
  oplot, 2.*r*1.e6, psd[*,nt-1,0]*1.e-6, lin=2

end
