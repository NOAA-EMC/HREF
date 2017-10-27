
import pygrib
from ncepgrib2 import Grib2Decode, Grib2Encode
import ConfigParser
import sys


def write_gb2(gribfilehandle,msg,field,frange,yyyy,mm,dd,hh,startfhr):
  # convert grib message to a ncepgrib2.Grib2Message instance.
  grbmsg_out = Grib2Decode(msg.tostring(), gribmsg=True)

  grbo = Grib2Encode(grbmsg_out.discipline_code,grbmsg_out.identification_section)
  grbo.addgrid(grbmsg_out.grid_definition_info,grbmsg_out.grid_definition_template)
  # add product definition template, data representation template
  # and data (including bitmap which is read from data mask).

  print('Product def template prior to updating: ')
  print(grbmsg_out.product_definition_template)

  #Set the correct forecast range
  grbmsg_out.product_definition_template[26]=int(frange)
  #Set the correct valid time (end of the range window
  grbmsg_out.product_definition_template[15]=int(yyyy) #year
  grbmsg_out.product_definition_template[16]=int(mm)   #month 
  grbmsg_out.product_definition_template[17]=int(dd)   #day
  grbmsg_out.product_definition_template[18]=int(hh)   #hour
  
  #Set the correct starting fhr
  grbmsg_out.product_definition_template[8]=int(startfhr)
  
  print
  print('Product def template after updating: ')
  print(grbmsg_out.product_definition_template)


  grbo.addfield(grbmsg_out.product_definition_template_number,
                grbmsg_out.product_definition_template,
                grbmsg_out.data_representation_template_number,
                grbmsg_out.data_representation_template,
                field)
  # - finalize the grib message.
  grbo.end()
  # - write it to the file.
  gribfilehandle.write(grbo.msg)


if __name__ == '__main__':

  
  print('Starting...')
  if len(sys.argv)!=2:
    sys.exit('Not enough arguments! Usage:python %s %s' %(sys.argv[0],'full_path_to_config_file'))

  '''  
  config.in example:
   
  combine_or_subtract (1 or -1, respectively. subtracting is f2-f1) \n\
  				full/path/to/input/file_1 \n \
  				full/path/to/input/file_2 \n \
  				full/path/to/OUTPUT/filename\n \
  				output grib string name   \n \
  				starting fhr \n \
  				10 digit forecast valid date \n \
  				range in integer hours covering accum. period'
  print(('Usage:python %s) % sys.argv[0])
  
  '''
  config=ConfigParser.ConfigParser()  
  config.read(sys.argv[1])

  gb1=config.get("myvars", "gb1")
  gb2=config.get("myvars", "gb2")
  gb3=config.get("myvars", "gb3")
  gb4=config.get("myvars", "gb4")
  gb5=config.get("myvars", "gb5")
  gb6=config.get("myvars", "gb6")
  gbout=config.get("myvars", "gbout")
  idate=config.get("myvars", "idate")
  frange=config.get("myvars", "frange")
  startfhr=0

  # Decompose the dates and times
  # Python goes to n-1
  hh=idate[8:10] 
  yyyy=idate[0:4]
  mm=idate[4:6]
  dd=idate[6:8]
  fhr=int(startfhr)+int(frange)

  # Open the grib files
  f1=pygrib.open(gb1)
  f2=pygrib.open(gb2)
  f3=pygrib.open(gb3)
  f4=pygrib.open(gb4)
  f5=pygrib.open(gb5)
  f6=pygrib.open(gb6)
   
  msg1=f1.select(name='Total Precipitation',lengthOfTimeRange=6)[0]
  precip1 = msg1.values

  msg2=f2.select(name='Total Precipitation',lengthOfTimeRange=6)[0] #store grib msg 
  precip2 = msg2.values 

  msg3=f3.select(name='Total Precipitation',lengthOfTimeRange=6)[0] #store grib msg 
  precip3 = msg3.values 

  msg4=f4.select(name='Total Precipitation',lengthOfTimeRange=6)[0] #store grib msg 
  precip4 = msg4.values 

  msg5=f5.select(name='Total Precipitation',lengthOfTimeRange=6)[0] #store grib msg 
  precip5 = msg5.values 

  msg6=f6.select(name='Total Precipitation',lengthOfTimeRange=6)[0] #store grib msg 
  precip6 = msg6.values 

  print('Adding!')
  precip_out = precip1+precip2+precip3+precip4+precip5+precip6
  print('Min precip value: '+repr(precip_out.min())+' Max precip value: '+repr(precip_out.max()))
  
  if precip_out.min() < 0.0:
    print 'Whoa - you have negative precip! Check dates/times on file1 and file2. \n \
              When subtracting, program does file2-file1.  Masking this to zero!'
    neg_indices = precip_out < 0.0  # Where values are < 0.0
    precip_out[neg_indices] = 0.0  # All negative values set to 0.0

  # Now write the grib2 file
  # Open up new grib2 output file
  gribfilehandle = open(gbout,'wb')
  write_gb2(gribfilehandle,msg2,precip_out,frange,yyyy,mm,dd,hh,startfhr)
  # - close the output file
  gribfilehandle.close()

