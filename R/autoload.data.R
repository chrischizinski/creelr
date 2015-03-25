"autoload.data"<-function(wd){
  filez<-list.files(wd)
  callz<-strsplit(paste(filez),".",fixed=TRUE)
  
  cd.id<-which(sapply(callz, "[", 2)=="cd")
  cp.id<-which(sapply(callz, "[", 2)=="cp")
  
  return(c(file.cd=filez[cd.id],file.cp=filez[cp.id]))
  
}