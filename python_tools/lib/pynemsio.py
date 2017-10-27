import numpy as np
class nemsfile(object):
    # Intialize and read a nemsio file.  
    # Provide some attritbutes and methods to the Class object

    def __init__(self,filename):        
        from nemsio_interface import open_init_nemsio,latlons,read_field,read_winds
        im,jm,lm,nsoil,nframe,tph0d,tlm0d,dlat,dlon=open_init_nemsio(filename)
        self.im = im; self.jm = jm ; self.lm = lm ; self.nsoil=nsoil
        self.tph0d = tph0d; self.tlm0d = tlm0d
        self.dlat = dlat; self.dlon=dlon
        self.nframe = nframe
        self.filename = filename
        lats,lons = latlons(self.filename,self.im,self.jm,self.nframe)
        self.lats = lats; self.lons=lons
        # Load up available methods from nemsio_interface.so
        self._read_field = read_field
        self._read_winds = read_winds

    def getfield(self,varname,levtype,lm=None):
        if lm is None:
          lm=self._nverts(levtype)
        var=self._read_field(self.filename,varname,levtype,self.im,self.jm,lm,self.nframe)
        if len(np.shape(var)) == 3 and lm == 1:
          return var[:,:,0]
        else:
          return var        

    def getuv(self,uname,vname,levtype,lm=None):
        if lm is None:
          lm=self._nverts(levtype)
        # Would have preferred to pass in self.lats and self.lons
        #  but f2py was really messing up the interpretation of the subroutine
        #  arguments in nemsio_interface.f90 (setting lm=im and so on).
        uout,vout=self._read_winds(self.filename,uname,vname,levtype,self.im,\
                         self.jm,lm,self.nframe,self.tph0d,self.tlm0d)
        # If the right-most dimension is 1, just return a 2d array       
        if len(np.shape(uout)) == 3 and lm == 1:
          return uout[:,:,0],vout[:,:,0]
        else:
          return uout,vout

    def _nverts(self,levtype):
        if levtype.lower()=='layer':
          lm=self.lm+1
        elif levtype.lower()=='soil layer':
          lm=self.nsoil
        elif levtype.lower() in ['sfc','10 m above gnd']:
          lm=1
        elif levtype.lower()=='layerm1':
          lm=self.lm-1
        elif levtype.lower()=='mid layer':
          lm = self.lm          
        return lm

    def ll_to_ij(self,lat,lon):
        #      Input :: lat, lon (input earth lon and lat in degrees)
        # Note - lon must be negative west!

        tlat,tlon=self._tll(lat,lon)
	tsw_lat,tsw_lon=self._tll(self.lats[0,0],self.lons[0,0])
	i=1. + (tlon - tsw_lon)/self.dlon
	j=1. + (tlat - tsw_lat)/self.dlat
        # Do not forget to subtract one here since python arrays begin at 0
  	return int(round(i-1.)),int(round(j-1.))
    
    def _tll(self,aphd,almd):
        # Convert from earth lat and lon to rotated lat and lon
	#      Input :: almd, aphd (input earth lon and lat in degrees)
        # Note - almd must be negative west!

        pi=3.141592653589793
        dtr=pi/180.0
  	if self.tlm0d==0.0 and self.tph0d==0.0:
  	  tlmd=almd
  	  tphd=aphd
  	else:
  	  tph0=self.tph0d*dtr
  	  ctph0=np.cos(tph0)
  	  stph0=np.sin(tph0)
  	  relm=(almd-self.tlm0d)*dtr
  	  srlm=np.sin(relm)
  	  crlm=np.cos(relm)
  	  aph=aphd*dtr
  	  sph=np.sin(aph)
  	  cph=np.cos(aph)
  	  cc=cph*crlm
  	  anum=cph*srlm
      	  denom=ctph0*cc+stph0*sph
  	  tlmd=np.arctan2(anum,denom)/dtr
  	  tphd=np.arcsin(ctph0*sph-stph0*cc)/dtr
	return tphd,tlmd

    def ij_to_ll(self,i,j):
  	# Convert i,j point to earth lat and earth lon
  	return self.lats[i,j],self.lons[i,j]
