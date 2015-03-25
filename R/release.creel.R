'release.creel'<-function(foo.inc){
  line.type<-substring(foo.inc,1,1)   # need to implement an error check in the future
  spp.release<-substring(foo.inc,2,4)
  release.num<-as.numeric(substring(foo.inc,5,7))
  r.lt.125<-as.numeric(substring(foo.inc,8,9))
  r.125<-as.numeric(substring(foo.inc,10,11))
  r.150<-as.numeric(substring(foo.inc,12,13))
  r.175<-as.numeric(substring(foo.inc,14,15))
  r.200<-as.numeric(substring(foo.inc,16,17))
  r.230<-as.numeric(substring(foo.inc,18,19))
  r.255<-as.numeric(substring(foo.inc,20,21))
  r.280<-as.numeric(substring(foo.inc,22,23))
  r.305<-as.numeric(substring(foo.inc,24,25))
  r.330<-as.numeric(substring(foo.inc,26,27))
  r.355<-as.numeric(substring(foo.inc,28,29))
  r.380<-as.numeric(substring(foo.inc,30,31))
  r.405<-as.numeric(substring(foo.inc,32,33))
  r.430<-as.numeric(substring(foo.inc,34,35))
  r.455<-as.numeric(substring(foo.inc,36,37))
  r.480<-as.numeric(substring(foo.inc,38,39))
  r.510<-as.numeric(substring(foo.inc,40,41))
  r.530<-as.numeric(substring(foo.inc,42,43))
  r.560<-as.numeric(substring(foo.inc,44,45))
  r.585<-as.numeric(substring(foo.inc,46,47))
  r.610<-as.numeric(substring(foo.inc,48,49))
  r.635<-as.numeric(substring(foo.inc,50,51))
  r.660<-as.numeric(substring(foo.inc,52,53))
  r.685<-as.numeric(substring(foo.inc,54,55))
  r.710<-as.numeric(substring(foo.inc,56,57))
  r.735_815<-as.numeric(substring(foo.inc,58,59))
  r.840_915<-as.numeric(substring(foo.inc,60,61))
  r.gt.915<-as.numeric(substring(foo.inc,62,63))

    release.block<-cbind(spp.release,release.num,r.lt.125,r.125,r.150,
                 r.175,r.200,r.230,r.255,r.280,r.305,r.330,
                 r.355,r.380,r.405,r.430,r.455,r.480,r.510,
                 r.530,r.560,r.585,r.610,r.635,r.660,r.685,
                 r.710,r.735_815,r.840_915,r.gt.915)
                          


  release.block[is.na(release.block)]<-0  #replace NAs with zeros for counts
  return(release.block)
}
