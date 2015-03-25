'harvest.creel'<-function(foo.inc){
  line.type<-substring(foo.inc,1,1)  # need to implement an error check in the future
  spp.harvest<-substring(foo.inc,2,4)
  harvest.num<-as.numeric(substring(foo.inc,5,7))
  harvest.mass<-as.numeric(substring(foo.inc,8,12))
  h.1<-as.numeric(substring(foo.inc,13,16))
  h.2<-as.numeric(substring(foo.inc,17,20))
  h.3<-as.numeric(substring(foo.inc,21,24))
  h.4<-as.numeric(substring(foo.inc,25,28))
  h.5<-as.numeric(substring(foo.inc,29,32))
  h.6<-as.numeric(substring(foo.inc,33,36))
  h.7<-as.numeric(substring(foo.inc,37,40))
  h.8<-as.numeric(substring(foo.inc,41,44))
  h.9<-as.numeric(substring(foo.inc,45,48))
  h.10<-as.numeric(substring(foo.inc,49,52))
  h.11<-as.numeric(substring(foo.inc,53,56))
  h.12<-as.numeric(substring(foo.inc,57,60))
  h.13<-as.numeric(substring(foo.inc,61,64))
  h.14<-as.numeric(substring(foo.inc,65,68))
  h.15<-as.numeric(substring(foo.inc,69,72))

  harvest.block<-cbind(spp.harvest,harvest.num,harvest.mass,h.1,h.2,h.3,h.4,h.5,
                       h.6,h.7,h.8,h.9,h.10,h.11,h.12,h.13,h.14,h.15)

  harvest.block[is.na(harvest.block)]<-0  #replace NAs with zeros for counts
  return(harvest.block)
}