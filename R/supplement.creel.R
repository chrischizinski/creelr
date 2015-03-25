'supplement.creel'<-function(foo.inc){
  line.type<-substring(foo.inc,1,1)   # need to implement an error check in the future
  s1<-trim(substring(foo.inc,2,6))
  s2<-trim(substring(foo.inc,7,11))
  s3<-trim(substring(foo.inc,12,16))
  s4<-trim(substring(foo.inc,17,21))
  s5<-trim(substring(foo.inc,22,26))
  s6<-trim(substring(foo.inc,27,31))
  s7<-trim(substring(foo.inc,32,36))
  s8<-trim(substring(foo.inc,37,41))
  s9<-trim(substring(foo.inc,42,46))
  s10<-trim(substring(foo.inc,47,51))
  s11<-trim(substring(foo.inc,52,56))
  s12<-trim(substring(foo.inc,57,61))
  s13<-trim(substring(foo.inc,62,66))

  
  supplement.block<-cbind(s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13)
  
  return(supplement.block)
}