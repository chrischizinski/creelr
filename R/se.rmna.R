'se.rmna'<-function(x){
  se.x<-sd(x,na.rm=TRUE)/sqrt(length(x[!is.na(x)]))
  return(se.x)
}