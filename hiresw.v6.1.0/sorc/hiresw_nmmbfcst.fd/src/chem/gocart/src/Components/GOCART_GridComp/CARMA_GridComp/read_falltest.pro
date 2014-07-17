  openr, lun, 'falltest.txt', /get_lun
  readf, lun, t0, nz
  rhoa_ = fltarr(nz)
  z_    = fltarr(nz)
  dz_   = fltarr(nz)
  q_    = fltarr(nz)
  data = fltarr(5)
  for iz = 0, nz-1 do begin
   readf, lun, data
   rhoa_[iz] = data[3]
   q_[iz]    = data[4]
   z_[iz]    = data[1]
   dz_[iz]   = data[2]
  endfor
  time = t0
  rhoa = rhoa_
  dz = dz_
  z = z_
  q = q_

  nt = 1
  while(not(eof(lun))) do begin
   readf, lun, t1
   for iz = 0, nz-1 do begin
    readf, lun, data
    rhoa_[iz] = data[3]
    q_[iz]    = data[4]
    z_[iz]    = data[1]
    dz_[iz]   = data[2]
   endfor
   time = [time,t1]
   rhoa = [rhoa,rhoa_]
   z = [z,z_]
   dz = [dz,dz_]
   q = [q,q_]
   nt = nt+1
  endwhile
  free_lun, lun

  dz = reform(dz,nz,nt)
  z = reform(z,nz,nt)
  q = reform(q,nz,nt)
  rhoa = reform(rhoa,nz,nt)

  for it = 0, nt-1 do begin
   plot, q[*,0]*rhoa[*,0], z[*,0], yrange=[0,10000], xrange=[0,2.5e-10], $
         title = 'time = '+string(time[it])+' seconds', ytitle = 'Altitude [m]'
   oplot, q[*,it]*rhoa[*,it], z[*,nt-1], lin=2
   wait, 0.1
  endfor

  it = 48
  plot, q[*,0]*rhoa[*,0], z[*,0], yrange=[0,10000], xrange=[0,2.5e-10], $
        title = 'time = 1 day', ytitle = 'Altitude [m]'
  oplot, q[*,it]*rhoa[*,it], z[*,nt-1], lin=2

end
